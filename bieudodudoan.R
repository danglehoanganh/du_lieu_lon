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
# 1. ĐỌC VÀ TIỀN XỬ LÝ DỮ LIỆU
# -------------------------
file_path <- "MiningProcess_Flotation_Plant_Database.csv"

# Đọc dữ liệu (thử với dấu phẩy, nếu lỗi thì thử lại với dấu chấm phẩy)
data <- tryCatch({
  fread(file_path, sep = ",")
}, error = function(e) {
  fread(file_path, sep = ";")
})

# Chuyển đổi tên cột thành tên hợp lệ (sử dụng make.names)
setnames(data, make.names(names(data)))

# Kiểm tra cấu trúc và tóm tắt dữ liệu
str(data)
summary(data)

# Nếu có cột 'date', chuyển sang kiểu thời gian
if ("date" %in% names(data)) {
  data$date <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  cat("✅ Cột 'date' đã được chuyển đổi sang kiểu thời gian.\n")
}

# Xử lý các cột số: nếu dữ liệu số được định dạng dạng ký tự (có dấu phẩy, ký tự không cần thiết)
numeric_cols <- setdiff(names(data), "date")
data[ , (numeric_cols) := lapply(.SD, function(x) {
  if (is.character(x)) {
    x <- gsub(",", ".", x)         # Thay dấu phẩy thành dấu chấm
    x <- gsub("[^0-9.]", "", x)    # Loại bỏ ký tự không phải số
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
# 2. PHÂN TÍCH KHÁI QUÁT (EDA) VỚI DỮ LIỆU RÚT GỌN
# -------------------------
# Để tránh R bị treo khi vẽ nhiều biểu đồ, ta lấy mẫu nhỏ hơn để phân tích EDA
sample_size_eda <- 1000
if (nrow(data) > sample_size_eda) {
  data_eda <- data[sample(.N, sample_size_eda)]
} else {
  data_eda <- data
}

# Lấy danh sách biến số trong data_eda
num_vars_eda <- names(data_eda)[sapply(data_eda, is.numeric)]

# 2.1. Vẽ histogram cho các biến số (với dữ liệu rút gọn)
for (col in num_vars_eda) {
  p <- ggplot(data_eda, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram của", col), x = col, y = "Tần số") +
    theme_minimal()
  print(p)
}

# 2.2. Vẽ biểu đồ ma trận tương quan (dùng dữ liệu rút gọn)
if (length(num_vars_eda) > 1) {
  cor_matrix <- cor(data_eda[, ..num_vars_eda], use = "complete.obs")
  corrplot(cor_matrix, method = "color", tl.cex = 0.7)
}

# 2.3. Vẽ pairs cho các biến quan trọng (cũng dùng dữ liệu rút gọn)
important_features <- c("X..Iron.Feed", "X..Silica.Feed", "Starch.Flow", 
                        "Amina.Flow", "Ore.Pulp.Flow", "Ore.Pulp.pH", "X..Iron.Concentrate")
important_features <- important_features[important_features %in% names(data_eda)]

if (length(important_features) >= 2) {
  pairs(data_eda[, ..important_features], main = "Scatterplot Matrix (Sample EDA)")
}

# -------------------------
# 3. XÂY DỰNG MÔ HÌNH DỰ ĐOÁN (DÙNG DỮ LIỆU ĐẦY ĐỦ HOẶC RÚT GỌN 100K)
# -------------------------
target <- "X..Iron.Concentrate"
if (!(target %in% names(data))) {
  stop("❌ Biến mục tiêu ", target, " không có trong dữ liệu.")
}

# Nếu dữ liệu quá lớn, rút gọn còn 100.000 dòng để huấn luyện
set.seed(123)
if (nrow(data) > 100000) {
  data_sample <- data[sample(.N, 100000)]
} else {
  data_sample <- data
}

# Chọn các biến mô hình: sử dụng các biến quan trọng đã chọn, loại trừ biến mục tiêu
model_features <- setdiff(important_features, target)
# Nếu vì lý do nào đó model_features trống, ta có thể chọn tạm 1 vài biến numeric
if (length(model_features) == 0) {
  model_features <- head(num_vars, 5)  # Lấy tạm 5 biến số đầu tiên
}

model_data <- data_sample[, c(model_features, target), with = FALSE]

# Chia dữ liệu train và test (80%-20%)
set.seed(123)
trainIndex <- createDataPartition(model_data[[target]], p = 0.8, list = FALSE)
trainData <- model_data[trainIndex, ]
testData  <- model_data[-trainIndex, ]

# Xây dựng công thức mô hình (backticks cho tên biến có ký tự đặc biệt)
form_str <- paste("`", target, "` ~ .", sep = "")

# --- MÔ HÌNH 1: RANDOM FOREST ---
rf_model <- randomForest(as.formula(form_str), data = trainData, ntree = 50)
rf_pred  <- predict(rf_model, testData)

# Tính RMSE cho mô hình Random Forest
rmse_rf <- sqrt(mean((testData[[target]] - rf_pred)^2))
cat("📌 Random Forest - RMSE:", round(rmse_rf, 3), "\n")

# Vẽ biểu đồ so sánh giá trị thực và dự đoán của mô hình Random Forest
p_rf <- ggplot(data.frame(Actual = testData[[target]], Predicted = rf_pred),
               aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "So sánh Dự đoán (Random Forest)", x = "Giá trị Thực", y = "Giá trị Dự đoán") +
  theme_minimal()
print(p_rf)

# --- MÔ HÌNH 2: HỒI QUY TUYẾN TÍNH (Linear Regression) ---
lm_model <- lm(as.formula(form_str), data = trainData)
lm_pred  <- predict(lm_model, testData)
rmse_lm  <- sqrt(mean((testData[[target]] - lm_pred)^2))
cat("📌 Linear Regression - RMSE:   ", round(rmse_lm, 3), "\n")

# Vẽ biểu đồ so sánh giá trị thực và dự đoán của mô hình Linear Regression
p_lm <- ggplot(data.frame(Actual = testData[[target]], Predicted = lm_pred),
               aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "green") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "So sánh Dự đoán (Linear Regression)", x = "Giá trị Thực", y = "Giá trị Dự đoán") +
  theme_minimal()
print(p_lm)
