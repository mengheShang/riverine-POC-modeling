import os
import glob
import rasterio
from rasterio.merge import merge
from rasterio.plot import show
from rasterio.shutil import copy as rio_copy

# 1. 设置路径（所有 .tif 文件所在文件夹）
input_folder = "D:/POC/data/SRTM"  # 
output_path = 'D:/POC/data/SRTM/SRTM_90m.tif'

# 2. 查找所有图像文件
tif_files = glob.glob(os.path.join(input_folder, '*.tif'))

# 3. 读取所有图像对象
src_files_to_mosaic = [rasterio.open(fp) for fp in tif_files]

# 4. 拼接图像
mosaic, out_trans = merge(src_files_to_mosaic)

# 5. 使用第一个图像的元数据作为基础
out_meta = src_files_to_mosaic[0].meta.copy()
out_meta.update({
    "driver": "GTiff",
    "height": mosaic.shape[1],
    "width": mosaic.shape[2],
    "transform": out_trans,
    "compress": "lzw"  # 可选压缩方式
})

# 6. 写入输出文件
with rasterio.open(output_path, "w", **out_meta) as dest:
    dest.write(mosaic)

print(f"✅ 拼接完成，输出文件保存在：{output_path}")
