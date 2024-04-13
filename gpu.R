# Đặt đường dẫn của tệp CSV vào biến 'path'
path <- "./All_GPUs.csv"

# path <- "./newDF.csv" # load dữ liệu đã lưu sau khi fill

# Sử dụng hàm 'read.csv' để đọc tệp
cpu_data <- read.csv(file = path, header = TRUE, sep = ",")
# Tổng quan về dữ liệu
# str(cpu_data)

# Tạo một dataframe mới chỉ chứa các cột quan trọng
new_DF <- cpu_data[, c( 
    "Architecture", "Core_Speed", "Boost_Clock", "Memory", 
    "Memory_Speed", "Memory_Bus", "Max_Power", "Process", "TMUs", "Shader"
)]
# new_DF <- cpu_data
# In ra 6 dòng đầu tiên của dữ liệu mới

str(new_DF)

# Loại bỏ ký tự ' MHz' và '-' và chuyển đổi dữ liệu sang định dạng số trong cột 'Core_Speed'
new_DF$Core_Speed <- as.numeric(gsub(" MHz|-", "", new_DF$Core_Speed))
# Loại bỏ ký tự ' MHz' và chuyển đổi dữ liệu sang định dạng số trong cột 'Boost_Clock'
new_DF$Boost_Clock <- as.numeric(gsub(" MHz", "", new_DF$Boost_Clock))
# Loại bỏ ký tự ' MB' và chuyển đổi dữ liệu sang định dạng số trong cột 'Memory'
new_DF$Memory <- as.numeric(gsub(" MB", "", new_DF$Memory))
# Loại bỏ ký tự ' MHz' và chuyển đổi dữ liệu sang định dạng số trong cột 'Memory_Speed'
new_DF$Memory_Speed <- as.numeric(gsub(" MHz", "", new_DF$Memory_Speed))
# Loại bỏ ký tự ' Bit' và chuyển đổi dữ liệu sang định dạng số trong cột 'Memory_Bus'
new_DF$Memory_Bus <- as.numeric(gsub(" Bit", "", new_DF$Memory_Bus))
# Loại bỏ ký tự ' MHz' và chuyển đổi dữ liệu sang định dạng số trong cột 'Memory_Speed'
new_DF$Memory_Speed <- as.numeric(gsub(" MHz", "", new_DF$Memory_Speed))
# Loại bỏ ký tự ' Watts' và chuyển đổi dữ liệu sang định dạng số trong cột 'Max_Power'
new_DF$Max_Power <- as.numeric(gsub(" Watts", "", new_DF$Max_Power))
# Loại bỏ ký tự 'nm' và chuyển đổi dữ liệu sang định dạng số trong cột 'Process'
new_DF$Process <- as.numeric(gsub("nm", "", new_DF$Process))

# In ra 6 dòng đầu tiên của dữ liệu mới

head(new_DF)


# Tính số lượng dữ liệu thiếu trong các biến
apply(is.na(new_DF), 2, sum)
# Tính tỷ lệ phần trăm dữ liệu thiếu trong các biến
apply(is.na(new_DF), 2, mean)


# # Lặp qua các cột khác để thực hiện quy trình tương tự
vars <- names(new_DF)
vars <- vars[vars != "Architecture"]
# Đơn vị của các biến 
units <- c("MHz", "MHz", "MB", "MHz", "Bit", "Watt", "nm", "unit", "core")

# Thay thế dữ liệu bằng kỳ vọng
for (var in vars) {
    if (var != "Core_Speed" && var != "Boost_Clock" && var != "Max_Power") {
        median_value <- median(new_DF[[var]], na.rm = TRUE)
        new_DF[[var]][is.na(new_DF[[var]])] <- median_value
    }
}

str(new_DF)
apply(is.na(new_DF), 2, sum)


temp_DF <- new_DF

library(randomForest)
set.seed(2) # đồng nhất các mẫu

rf_model <- randomForest(Boost_Clock ~ ., 
	data = temp_DF[, -which(names(new_DF) %in% c("Core_Speed", "Max_Power"))], 
	ntree = 500, mtry = 3, na.action=na.exclude
)
# "rfBoostclock"
new_DF$Boost_Clock <- as.integer(predict(rf_model, new_DF) / 10) * 10

rf_model <- randomForest(Max_Power ~ ., 
	data = temp_DF[, -which(names(new_DF) %in% c("Core_Speed", "Boost_Clock"))], 
	ntree = 500, mtry = 3, na.action=na.exclude
	)
# "rfmaxpower"
new_DF$Max_Power <- as.integer(predict(rf_model, new_DF) / 10) * 10

rf_model <- randomForest(Core_Speed ~ ., 
	data = temp_DF[, -which(names(new_DF) %in% c("Boost_Clock", "Max_Power"))], 
	ntree = 500, mtry = 3, na.action=na.exclude
	)
# "rfcorespeed"
new_DF$Core_Speed <- as.integer(predict(rf_model, new_DF) / 10) * 10

write.csv(new_DF, "newDF.csv") 
temp_DF <- new_DF

str(temp_DF)
apply(is.na(temp_DF), 2, sum)


Vector_Average  <- c()
Vector_Median <- c()
Vector_SD <- c()
Vector_Max <- c()
Vector_Min <- c()

for (var in vars) {
	mean_Val <- mean(new_DF[[var]])
	median_value <- median(new_DF[[var]])
	sd_value <- sd(new_DF[[var]])
	max_value <- max(new_DF[[var]])
	min_value <- min(new_DF[[var]])
	Vector_Average  <- c(Vector_Average, mean_Val)
	Vector_Median <- c(Vector_Median, median_value)
	Vector_SD <- c(Vector_SD, sd_value)
	Vector_Max <- c(Vector_Max, max_value)
	Vector_Min <- c(Vector_Min, min_value)
}

Features <- c(
    "Core_Speed (MHz)", "Boost_Clock (MHz)", "Memory (MB)", "Memory_Speed (MHz)",
    "Memory_Bus (bit)", "Max_Power (Watts)", "Process (nm)", "TMU (units)", "Shader (cores)"
)
# Bảng thống kê biến
TableX <- data.frame(Features,
    "Average" = Vector_Average, "Median" = Vector_Median,
    "StdDeviation" = Vector_SD, "Max" = Vector_Max, "Min" = Vector_Min
)

Xem bảng thống kê
View(TableX, "Bảng Thống Kê Mô Tả")

for (i in seq_along(vars)) {
    var <- vars[i]
    unit <- units[i]
    color <- rgb(runif(1), runif(1), runif(1))
    hist.default(temp_DF[[var]],
        main = paste("Distribution of", var), col = color,
        xlab = paste(var, sprintf("(%s)", unit)),
		labels = TRUE
    )
}


boxplot(Memory_Speed ~ Memory, new_DF,col = "blue", 
	outline=FALSE, xlab=Features[3], ylab=Features[4], 
	main=paste("Between", Features[3], "and", Features[4])
)
boxplot(Memory_Bus ~ Memory, new_DF,col = "blue", 
	outline=FALSE, xlab=Features[3], ylab=Features[5], 
	main=paste("Between", Features[3], "and", Features[5])
)
boxplot(Boost_Clock ~ Max_Power, new_DF, col = "red",
	outline=FALSE, xlab=Features[6], ylab=Features[2], 
	main=paste("Between", Features[6], "and", Features[2])
)
pairs(new_DF[, which(names(new_DF) %in% c("Memory_Speed", "Process", "Core_Speed"))], 
	col="purple", main="Correlate between Process and Memory & Core Speed "
) 

