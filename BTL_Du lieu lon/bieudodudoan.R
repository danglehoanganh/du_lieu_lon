# 📌 Giải phóng bộ nhớ
gc()
rm(list = ls())
gc()

# 📌 Đọc dữ liệu
library(data.table)
file_path <- "MiningProcess_Flotation_Plant_Database.csv"
data <- fread(file_path, sep = ",")

# 📌 Lấy mẫu dữ liệu nhỏ hơn
set.seed(123)
data_sample <- data[sample(nrow(data), size = 100000), ]

# 📌 Chọn các biến quan trọng
important_features <- c("X..Iron.Feed", "X..Silica.Feed", "Starch.Flow", "Amina.Flow", "Ore.Pulp.Flow", "Ore.Pulp.pH", "X..Iron.Concentrate")
data_reduced <- data_sample[ , important_features, with = FALSE]

# 📌 Chia train/test
set.seed(123)
trainIndex <- createDataPartition(data_reduced$X..Iron.Concentrate, p = 0.8, list = FALSE)
trainData <- data_reduced[trainIndex, ]
testData <- data_reduced[-trainIndex, ]

# 📌 Train mô hình với ít cây hơn
rf_model <- randomForest(X..Iron.Concentrate ~ ., data = trainData, ntree = 50)  # Giảm số cây
rf_pred <- predict(rf_model, testData)

# 📌 Kiểm tra kết quả
rmse_value <- sqrt(mean((testData$X..Iron.Concentrate - rf_pred)^2))
cat("📌 Random Forest - RMSE:", round(rmse_value, 3), "\n")

# 📌 Vẽ biểu đồ dự đoán
library(ggplot2)
ggplot(data.frame(Actual = testData$X..Iron.Concentrate, Predicted = rf_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "So Sánh Kết Quả Dự Đoán Random Forest", x = "Giá Trị Thực", y = "Giá Trị Dự Đoán") +
  theme_minimal()
