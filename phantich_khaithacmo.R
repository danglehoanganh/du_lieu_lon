data <- read.csv(file_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)  # Thử với dấu phẩy
# Nếu vẫn lỗi, thử với dấu chấm phẩy:
# data <- read.csv(file_path, header = TRUE, sep = ";", stringsAsFactors = FALSE)
print(sapply(data, class))  # Kiểm tra kiểu dữ liệu
numeric_cols <- setdiff(names(data), "date")  # Loại trừ cột `date`

data[numeric_cols] <- lapply(data[numeric_cols], function(x) {
  x <- gsub(",", ".", x)  # Thay dấu `,` thành `.`
  x <- gsub("[^0-9.]", "", x)  # Loại bỏ các ký tự không phải số hoặc dấu `.`
  as.numeric(x)  # Chuyển thành số
})
if ("date" %in% names(data)) {
  data$date <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  cat("✅ Cột `date` đã chuyển đổi thành kiểu thời gian.\n")
}
print(sapply(data, class))  # Kiểm tra kiểu dữ liệu sau khi chuyển đổi
cat("✅ Tổng số giá trị NA trước khi xử lý:", sum(is.na(data)), "\n")

# Nếu có quá nhiều NA, thay thế thay vì xóa toàn bộ:
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

cat("✅ Tổng số giá trị NA sau khi xử lý:", sum(is.na(data)), "\n")
numeric_cols <- names(data)[sapply(data, is.numeric)]
print(numeric_cols)
cat("✅ Tổng số cột số sau khi chuyển đổi:", length(numeric_cols), "\n")

# Nếu không có đủ cột số:
if (length(numeric_cols) < 2) {
  stop("❌ Không có đủ biến số để phân tích! Hãy kiểm tra lại kiểu dữ liệu.")
}
