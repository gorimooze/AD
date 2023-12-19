# Установка необходимых пакетов
install.packages("tidyverse")
library("tidyverse")

# Загрузка данных
file_path <- "C:\\Users\\rogal\\OneDrive\\Рабочий стол\\rogali\\autos.csv"
data <- read.csv(file_path)
summary(data)
head(data)
data <- data %>% select(price, vehicleType, yearOfRegistration, gearbox, powerPS, kilometer, monthOfRegistration, fuelType, notRepairedDamage)
# Заменяем пустые строки на NA
data[data == ""] <- NA
# Удаляем строки с NA
data <- na.omit(data)
head(data)
#переобразование категоориальных переменых
columns_to_factorize <- c("notRepairedDamage", "fuelType", "gearbox", "vehicleType")
data[columns_to_factorize] <- lapply(data[columns_to_factorize], factor)

#EDA

ggplot(data, aes(x = notRepairedDamage, y = price)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Состояние", x = "Состояние", y = "Цена")

ggplot(data, aes(x = fuelType, y = price)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Тип топлива", x = "Тип топлива", y = "Цена")

ggplot(data, aes(x = kilometer, y = price)) +
  geom_bar(stat = "identity", fill = "yellow", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Пробег", x = "Пробег", y = "Цена")

ggplot(data, aes(x = powerPS, y = price)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Мощность", x = "Мощность", y = "Цена")

ggplot(data, aes(x = gearbox, y = price)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-КПП", x = "КПП", y = "Цена")

ggplot(data, aes(x = yearOfRegistration, y = price)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Гистограмма: Цена-Год", x = "Год", y = "Цена")

#создание данных для предсказания
set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train_data  <- data[sample, ]
test_data   <- data[!sample, ]
head(train_data)
head(test_data)

# Построение модели линейной регрессии
model <- lm(price ~ ., data = train_data)
summary(model)

# Расчет R-квадрат
rsquared <- summary(model)$r.squared
cat("R-квадрат:", rsquared, "\n")

#Предсказываение цены
predictions <- predict(model, newdata = test_data)
print(predictions)

# Создаем график с использованием ggplot2
ggplot(data = test_data, aes(x = price, y = predictions)) +
  geom_point(colour = "blue", shape = 10) +  # Точки для предсказаний
  geom_point(aes(y = price), colour = "red", shape = 10) +  # Точки для фактических значений
  geom_abline(intercept = 0, slope = 1, colour = "green", linewidth = 1) +  # Линия идеального соответствия
  ggtitle("Predictions vs. Actual Values") +  # Заголовок графика
  xlab("Actual Values") +  # Подпись оси X
  ylab("Predicted Values") +  # Подпись оси Y
  theme_minimal() +  # Применяем минималистичный стиль оформления
  # Добавляем легенду
  scale_colour_manual(
    name = "Legend",
    values = c("blue", "red", "green"),
    labels = c("Predicted", "Actual", "Perfect Fit")
  )

