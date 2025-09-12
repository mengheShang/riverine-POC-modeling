# 加载包
library(sf)
library(dplyr)
library(igraph)

# 加载POC测站数据
poc_stations <- read.csv("D:\\POC research\\data\\Sites position.csv")
coordinates(poc_stations) <- ~Longitude + Latitude
proj4string(poc_stations) <- CRS("+proj=longlat +datum=WGS84")
poc_stations <- st_as_sf(poc_stations, coords = c("Longitude", "Latitude"), crs = 4326)

# 加载河流干流和支流数据
main_rivers <- st_read("D:\\POC research\\data\\river\\stem.shp")
tributaries <- st_read("D:\\POC research\\data\\river\\tributaries.shp")

# 加载水库和大坝数据
reservoirs_dams <- st_read("D:\\POC research\\data\\Reservoirs and Dams\\China_Dams.shp")

# 输出列名以便调试
main_rivers_cols <- names(main_rivers)
tributaries_cols <- names(tributaries)
print("Main Rivers Columns:")
print(main_rivers_cols)
print("Tributaries Columns:")
print(tributaries_cols)
# 确保数据类型一致
for (col in intersect(main_rivers_cols, tributaries_cols)) {
  if (class(main_rivers[[col]])[1] != class(tributaries[[col]])[1]) {
    print(paste("Converting column:", col))
    main_rivers[[col]] <- as.character(main_rivers[[col]])
    tributaries[[col]] <- as.character(tributaries[[col]])
  }
}

# 合并所有河流
all_rivers <- bind_rows(main_rivers, tributaries)
# 确保 CRS 一致
all_rivers <- st_transform(all_rivers, st_crs(poc_stations))
reservoirs_dams <- st_transform(reservoirs_dams, st_crs(poc_stations))

# 空间连接POC测站和河流
poc_stations_with_rivers <- st_join(poc_stations, all_rivers, join = st_intersects)

# 提取河流的起点和终点
get_coords <- function(line) {
  coords <- st_coordinates(line)
  list(start = coords[1, ], end = coords[nrow(coords), ])
}

# 构建河流网络
river_network <- graph.empty(directed = TRUE)
vertex_names <- c()

for (i in 1:nrow(all_rivers)) {
  coords <- get_coords(all_rivers$geometry[i])
  start_node <- paste0(coords$start, collapse = ",")
  end_node <- paste0(coords$end, collapse = ",")
  
  if (!start_node %in% vertex_names) {
    river_network <- add_vertices(river_network, n = 1, name = start_node)
    vertex_names <- c(vertex_names, start_node)
  }
  if (!end_node %in% vertex_names) {
    river_network <- add_vertices(river_network, n = 1, name = end_node)
    vertex_names <- c(vertex_names, end_node)
  }
  river_network <- add_edges(river_network, c(start_node, end_node))
}

# 将水库和大坝添加为节点
for (i in 1:nrow(reservoirs_dams)) {
  dam_location <- st_coordinates(reservoirs_dams$geometry[i])
  dam_node <- paste0(dam_location, collapse = ",")
  
  if (!dam_node %in% vertex_names) {
    river_network <- add_vertices(river_network, n = 1, name = dam_node)
    vertex_names <- c(vertex_names, dam_node)
  }
}

# 查找每个测站上游的水库和大坝
upstream_dams_list <- list()
for (i in 1:nrow(poc_stations_with_rivers)) {
  station_location <- st_coordinates(poc_stations_with_rivers$geometry[i])
  station_node <- paste0(station_location, collapse = ",")
  
  # 查找上游的水库和大坝
  if (station_node %in% V(river_network)$name) {
    upstream_nodes <- subcomponent(river_network, station_node, mode = "in")
    upstream_dams <- V(river_network)[upstream_nodes]$name
    upstream_dams_list[[i]] <- upstream_dams
  } else {
    upstream_dams_list[[i]] <- NA
  }
}

# 添加结果到POC测站数据
poc_stations_with_rivers$upstream_dams <- upstream_dams_list

# 查看结果
print(poc_stations_with_rivers)