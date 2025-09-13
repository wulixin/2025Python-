

new_lib <- .libPaths()[1]  # 通常是用户库
# 定义路径（请根据实际情况修改）
old_lib <- "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library"
new_lib <- .libPaths()[1]

# 获取旧库中的所有包名
pkgs <- dir(old_lib)

# 批量复制
for (pkg in pkgs) {
  file.copy(
    from = file.path(old_lib, pkg),
    to = new_lib,
    recursive = TRUE,
    overwrite = FALSE  # 避免覆盖已安装的同名包
  )
}


# 添加旧 R 的库路径（替换为你的实际路径）

# 将其加入库路径
.libPaths(c(old_lib, .libPaths()))

# 检查是否加载成功
library(Tushare)  # 测试一个你以前安装过的包
