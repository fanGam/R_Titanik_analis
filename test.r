# Устанавливаем необходимый пакет
# install.packages("readxl")

# Загружаем пакет
library(readxl)
library(mice)
library(VIM)
library(DescTools)

# Считываем данные из файла xls
dataset <- read_excel("ДанныеТитаник.xls")

# 1 Часть
# 1 Задание
# Считаем количество наблюдений
num_observations <- nrow(dataset)

# Считаем количество переменных
num_variables <- ncol(dataset)

# Получаем информацию о переменных и их типах
variables_info <- sapply(dataset, typeof)

# Печатаем результаты
cat("Количество наблюдений:", num_observations, "\n")
cat("Количество переменных:", num_variables, "\n")
cat("Информация о переменных и их типах:\n")
print(variables_info)

# 2, 3, 4 Задания
# Исключение пропущенных значений
data <- na.omit(dataset)
# View(data)

# Считаем количество наблюдений
num_observations_new <- nrow(data)
cat("Количество наблюдений без пропущенных значений:", num_observations_new, "\n")

# Считаем количество наблюдений без пропущенных значений
null_counts <- colSums(is.na(dataset))
cat("Количество Null значений по каждой переменной:\n")
print(null_counts)

# 2 Часть
# 1 Задание
# Добавление новой переменной
data$female <- ifelse(data$sex == 'female', 1, 0)

# 2 Задание
# Отделение кортежей с условием
df2 <- subset(data, (age > 25 & age <= 45) & (pclass == 2 | pclass == 3))
# View(df2)

# 3 Задание
# Подсчёт количество кортежей с условием
mans <- sum(data$female == 0)
cat("Количество мужчин на корабле:", mans, "\n")
women <- sum(data$female == 1)
cat("Количество женщин на корабле:", women, "\n")

# 4 Задание
# Нахождение самых старых и молодых пассажиров, а также среднего
yangest <- subset(data, age == min(data$age))
# View(yangest)
oldest <- subset(data, age == max(data$age))
# View(oldest)
cat("Средний возраст пассажиров:", mean(data$age), "\n")


# 3 Часть
# 1 Задание
# Описательная статистика для возраста
age_mean <- mean(data$age)
age_variance <- var(data$age)
age_sd <- sd(data$age)
age_mode <- mode(data$age)
age_median <- median(data$age)
age_skewness <- moments::skewness(data$age)
age_kurtosis <- moments::kurtosis(data$age)
age_cv <- (age_sd / age_mean) * 100

# Вывод результатов
cat("Среднее:", age_mean, "\n")
cat("Дисперсия:", age_variance, "\n")
cat("Среднеквадратичное отклонение:", age_sd, "\n")
cat("Мода:", age_mode, "\n")
cat("Медиана:", age_median, "\n")
cat("Коэффициент асимметрии:", age_skewness, "\n")
cat("Эксцесс:", age_kurtosis, "\n")
cat("Коэффициент вариации:", age_cv, "%", "\n")
# Среднее, мода и медиана: Значения среднего, моды и медианы очень близки друг к другу (все около 36). 
# Это указывает на симметричность распределения.

# Коэффициент асимметрии: Значение -0.009216347 почти равно нулю, 
# что также свидетельствует о симметричности распределения.

# Эксцесс: Значение эксцесса 2.668815 показывает, что распределение имеет более 
# выраженные пики и более широкие хвосты по сравнению с нормальным распределением 
# (эксцесс для нормального распределения равен 0).

# Коэффициент вариации: Составляет 42.28035%, что указывает на значительную 
# изменчивость данных относительно среднего значения.

# В целом, можно сделать вывод, что распределение возраста пассажиров близко к симметричному, 
# но с некоторыми отклонениями от нормальности (в виде более выраженных пиков и широких хвостов). 
# Это подтверждается значением эксцесса, который показывает, что распределение 
# более "высокое" и "широкое", чем нормальное.

# Описательная статистика для пола
gender_counts <- as.character(names(which.max(table(data$sex))))
cat("На корабле больше:",gender_counts,"\n")
# На основе этих данных можно сказать, что количество мужчин на корабле было больше, чем
# количество женщин

# 2 Задание
# Построение гистограммы
hist(data$age, , breaks=10, col="skyblue", main="Распределение возраста", xlab="Возраст", ylab="Частота", probability=TRUE, ylim=c(0,0.03))

# Добавление плотности нормального распределения
curve(dnorm(x, mean=mean(data$age), sd=sd(data$age)), from=0, to=80, add=TRUE, col="red", lwd=2)

# Анализ возраста
age_table <- table(cut(data$age, breaks=seq(min(data$age), max(data$age), by=5)))
cat("Людей какого возраста больше всего:", names(which.max(age_table)), "\n")
cat("Людей какого возраста меньше всего:", names(which.min(age_table)), "\n")

# 4 Часть
# 1 Задание
# 95% доверительный интервал
conf_interval_mean <- MeanCI(data$age, conf.level = 0.95, na.rm = TRUE)
cat('95% доверительный интервал для среднего возраста: [', conf_interval_mean[1], ',', conf_interval_mean[2], ']\n')

# Дисперсия и 95% доверительный интервал для дисперсии возраста
conf_interval_var <- VarCI(data$age, conf.level = 0.95, na.rm = TRUE)
cat('95% доверительный интервал для дисперсии возраста: [', conf_interval_var[1], ',', conf_interval_var[2], ']\n')

# Среднее и 95% доверительный интервал для среднего возраста при известном стандартном отклонении
conf_interval_known_sd <- age_mean + c(-1, 1) * qnorm(1 - 0.05/2) * (8 / sqrt(sum(!is.na(data$age))))
cat('95% доверительный интервал для среднего возраста при ср.квадратичном отклонении 8: [', conf_interval_known_sd[1], ',', conf_interval_known_sd[2], ']\n')

# Пропорция и 95% доверительный интервал для доли выживших
conf_interval_survived <- BinomCI(sum(data$survived == 1, na.rm = TRUE), sum(!is.na(data$survived)), conf.level = 0.95)
cat('95% доверительный интервал для доли людей, которые выжили: [', conf_interval_survived[1], ',', conf_interval_survived[2], ']\n')
