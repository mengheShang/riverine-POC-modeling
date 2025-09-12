r_to_rmd <- function(r_file, rmd_file = NULL) {
  lines <- readLines(r_file)
  
  if (is.null(rmd_file)) {
    rmd_file <- sub("\\.R$", ".Rmd", r_file)
  }
  
  output <- c()
  chunk_open <- FALSE
  chunk_label <- NULL
  
  for (line in lines) {
    if (grepl("^# ?####", line)) {
      # Close previous chunk if open
      if (chunk_open) {
        output <- c(output, "```", "")
        chunk_open <- FALSE
      }
      # Extract chunk label between ####
      chunk_label <- gsub("^# ?#### *", "", line)
      chunk_label <- gsub(" *####$", "", chunk_label)   # remove trailing ####
      chunk_label <- gsub("[^A-Za-z0-9_]", "_", chunk_label)  # safe name
      output <- c(output, paste0("```{r ", chunk_label, "}"))
      chunk_open <- TRUE
    } else {
      output <- c(output, line)
    }
  }
  
  # Close last chunk
  if (chunk_open) {
    output <- c(output, "```")
  }
  
  writeLines(output, rmd_file)
  message("Rmd written to: ", rmd_file)
}

r_to_rmd("D:/POC/code/model_XGboost_Avg_10cv.R")