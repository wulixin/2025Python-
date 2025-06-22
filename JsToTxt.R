
library(jsonlite)

# Step 1: 定义输入 JSON 数据（或从文件读取）
json_data <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)


# 解析 JSON 数据
data <- fromJSON(json_data)

# Step 2: 创建写入函数
write_stock_info <- function(stock_name, info, file_conn) {
  cat("====================\n", file = file_conn)
  cat(paste0("股票名称：", info$name, "\n"), file = file_conn)
  cat(paste0("股票代码：", info$代码, "\n"), file = file_conn)
  cat("--------------------\n", file = file_conn)
  
  cat("【股价信息】\n")
  cat(paste0(info$股价信息, "\n\n"), file = file_conn)
  
  cat("【上市信息】\n")
  cat(paste0(info$上市信息, "\n\n"), file = file_conn)
  
  cat("【行业与股东】\n")
  items <- unlist(strsplit(info$行业股东, " "))
  for (item in items) {
    cat(paste0(item, " "))
  }
  cat("\n\n", file = file_conn)
  
  cat("【概念与产业链】\n")
  concepts <- strsplit(gsub("-\\s*", "", info$概念产业链), " ")[[1]]
  for (concept in concepts) {
    if (nchar(concept) > 0) cat(paste0("- ", concept, "\n"), file = file_conn)
  }
  cat("\n", file = file_conn)
  
  cat("【政策与产品属性】\n")
  policies <- unlist(strsplit(info$政策产品, " "))
  cat(paste(policies, collapse = " | "), "\n\n", file = file_conn)
  
  cat("====================\n\n", file = file_conn)
}

# Step 3: 打开输出文件连接
output_file <- "//Users//wulixin//Desktop//stock_knowledge_base.txt"
con <- file(output_file, encoding = "UTF-8")
open(con, "w")

# Step 4: 遍历所有股票条目并写入
for (stock in names(data)) {
  write_stock_info(stock, data[[stock]], con)
}

# Step 5: 关闭文件连接
close(con)

cat("✅ 已成功生成 '", output_file, "' 文件！\n")
