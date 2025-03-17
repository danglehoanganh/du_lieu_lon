
gc()
rm(list = ls())
gc()

# Load các thư viện cần thiết
library(data.table)    # Đọc và xử lý dữ liệu
library(dplyr)         # Xử lý dữ liệu
library(ggplot2)       # Vẽ biểu đồ
library(caret)         # Chia dữ liệu train/test & xây dựng mô hình
library(randomForest)  # Xây dựng mô hình Random Forest
library(corrplot)      # Vẽ biểu đồ ma trận tương quan

# -------------------------
# 2. ĐỌC VÀ TIỀN XỬ LÝ DỮ LIỆU
# -------------------------
file_path <- "MiningProcess_Flotation_Plant_Database.csv"

# Đọc dữ liệu (với separator là dấu phẩy)
data <- tryCatch({
  fread(file_path, sep = ",", stringsAsFactors = FALSE)
}, error = function(e) {
  fread(file_path, sep = ";", stringsAsFactors = FALSE)
})

# Chuyển đổi tên cột thành tên hợp lệ (sử dụng make.names)
setnames(data, make.names(names(data)))

# Kiểm tra cấu trúc và tóm tắt dữ liệu
str(data)
summary(data)

# Nếu có cột 'date', chuyển sang kiểu thời gian
if("date" %in% names(data)) {
  data$date <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  cat("✅ Cột 'date' đã được chuyển đổi sang kiểu thời gian.\n")
}

# Xử lý các cột số: nếu dữ liệu số được định dạng dạng ký tự (có dấu phẩy, ký tự không cần thiết)
numeric_cols <- setdiff(names(data), "date")
data[ , (numeric_cols) := lapply(.SD, function(x) {
  if(is.character(x)) {
    x <- gsub(",", ".", x)         # Thay dấu phẩy thành dấu chấm
    x <- gsub("[^0-9.]", "", x)      # Loại bỏ ký tự không phải số
    return(as.numeric(x))
  } else {
    return(x)
  }
}), .SDcols = numeric_cols]

# Thay thế giá trị NA cho các biến số (bằng trung bình của cột)
num_vars <- names(data)[sapply(data, is.numeric)]
data[ , (num_vars) := lapply(.SD, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}), .SDcols = num_vars]

cat("✅ Tổng số giá trị NA sau khi xử lý:", sum(is.na(data)), "\n")

# -------------------------
# 3. PHÂN TÍCH KHÁI QUÁT (EDA)
# -------------------------
# Vẽ histogram cho các biến số
for(col in num_vars) {
  p <- ggplot(data, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram của", col), x = col, y = "Tần số") +
    theme_minimal()
  print(p)
}

# Vẽ biểu đồ ma trận tương quan cho các biến số
cor_matrix <- cor(data[, ..num_vars], use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# Chọn một số biến quan trọng để phân tích (theo gợi ý ban đầu)
# Lưu ý: tên biến đã được chuyển đổi thành tên hợp lệ (ví dụ: "% Iron Feed" thành "X..Iron.Feed")
important_features <- c("X..Iron.Feed", "X..Silica.Feed", "Starch.Flow", 
                        "Amina.Flow", "Ore.Pulp.Flow", "Ore.Pulp.pH", "X..Iron.Concentrate")
important_features <- important_features[important_features %in% names(data)]
if(length(important_features) >= 2) {
  pairs(data[, ..important_features], main = "Scatterplot Matrix của các biến quan trọng")
}

# -------------------------
# 4. XÂY DỰNG MÔ HÌNH DỰ ĐOÁN
# -------------------------
# Giả sử biến mục tiêu là "X..Iron.Concentrate" (sau khi chuyển đổi tên cột)
target <- "X..Iron.Concentrate"
if(!(target %in% names(data))) {
  stop("❌ Biến mục tiêu ", target, " không có trong dữ liệu.")
}

# Sử dụng mẫu dữ liệu nếu dataset quá lớn
set.seed(123)
if(nrow(data) > 100000) {
  data_sample <- data[sample(nrow(data), 100000), ]
} else {
  data_sample <- data
}

# Chọn các biến mô hình: sử dụng các biến quan trọng đã chọn, loại trừ biến mục tiêu
model_features <- setdiff(important_features, target)
model_data <- data_sample[, c(model_features, target), with = FALSE]

# Chia dữ liệu train và test (80%-20%)
set.seed(123)
trainIndex <- createDataPartition(model_data[[target]], p = 0.8, list = FALSE)
trainData <- model_data[trainIndex, ]
testData <- model_data[-trainIndex, ]

# Để xây dựng công thức mô hình với các tên biến có ký tự đặc biệt, 
# chúng ta sẽ đặt tên biến trong backticks.
form_str <- paste("`", target, "` ~ .", sep = "")

# --- MÔ HÌNH 1: RANDOM FOREST ---
rf_model <- randomForest(as.formula(form_str), data = trainData, ntree = 50)
rf_pred <- predict(rf_model, testData)

# Tính RMSE cho mô hình Random Forest
rmse_rf <- sqrt(mean((testData[[target]] - rf_pred)^2))
cat("📌 Random Forest - RMSE:", round(rmse_rf, 3), "\n")

# Vẽ biểu đồ so sánh giá trị thực và dự đoán của mô hình Random Forest
ggplot(data.frame(Actual = testData[[target]], Predicted = rf_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "So sánh Dự đoán (Random Forest)", x = "Giá trị Thực", y = "Giá trị Dự đoán") +
  theme_minimal()

# --- MÔ HÌNH 2: HỒI QUY TUYẾN TÍNH (Linear Regression) ---
lm_model <- lm(as.formula(form_str), data = trainData)
lm_pred <- predict(lm_model, testData)
rmse_lm <- sqrt(mean((testData[[target]] - lm_pred)^2))
cat("📌 Linear Regression - RMSE:", round(rmse_lm, 3), "\n")

# Vẽ biểu đồ so sánh giá trị thực và dự đoán của mô hình Linear Regression
ggplot(data.frame(Actual = testData[[target]], Predicted = lm_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "So sánh Dự đoán (Linear Regression)", x = "Giá trị Thực", y = "Giá trị Dự đoán") +
  theme_minimal()
