library(tidyverse)
install.packages("tidyverse")
library(modelr)
install.packages("modelr")
library(scatterplot3d)
library(readr)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
install.packages("rsample")
library(rsample)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("caret")
library(caret)
install.packages("rlang")
library(magrittr)

Ford <- read_csv("ford.csv")

Ford %>%
  ggplot(aes(model, price)) + geom_point(color="darkgreen") + geom_smooth()

Ford %>%
  ggplot(aes(year, price)) + geom_point() + geom_smooth(color="darkgreen")

Ford %>%
  ggplot(aes(transmission, price)) + geom_point(color="darkgreen") + geom_smooth()

Ford %>%
  ggplot(aes(mileage, price)) + geom_point() + geom_smooth(color="darkgreen")

Ford %>%
  ggplot(aes(fuelType, price)) + geom_point(color="darkgreen") + geom_smooth()

Ford %>%
  ggplot(aes(tax, price)) + geom_point() + geom_smooth(color="darkgreen")

Ford %>%
  ggplot(aes(mpg, price)) + geom_point() + geom_smooth(color="darkgreen")

Ford %>%
  ggplot(aes(engineSize, price)) + geom_point() + geom_smooth(color="darkgreen")


pret_Year <- lm(data = Ford, price ~ year)
summary(pret_Year)


grid_year <- Ford %>%
  data_grid(year = seq_range(year, 100)) %>%
  add_predictions(pret_Year, "price")


ggplot(Ford, aes(year, price)) +
  geom_point() +
  geom_line(data = grid_year, color="blue", size=1)


confint(pret_Year)

df <- read.csv("Ford.csv")
Ford <- Ford[Ford$year <= 2022, ]
Ford <- Ford[Ford$year >= 2005, ]


ggplot(df, aes(x = year, y = price)) + 
  geom_point() +
  labs(title = "price vs year")


str(Ford)


model_levels <- levels(Ford$model)
numere_model <- as.integer(model_levels)


Ford <- Ford %>%
  mutate(model = as.factor(model),
         transmission = as.factor(transmission),
         fuelType = as.factor(fuelType))

model <- lm(price ~ model, data = Ford)
summary(model)

model <- lm(price ~ model, data = Ford)
summary(model)

pret_model <- lm(data = Ford, price ~ model)
summary(pret_model)

confint(pret_model) 

Ford <- select(Ford, -tax)
View(Ford)





#transmission
pret_Transmission <- lm(data = Ford, price ~ transmission)
summary(pret_Transmission)


confint(pret_Transmission)

#mileage
pret_Mileage <- lm(data = Ford, price ~ mileage)
summary(pret_Mileage)

grid_mileage <- Ford %>%
  data_grid(mileage = seq_range(mileage, 100)) %>%
  add_predictions(pret_Mileage, "price")

ggplot(Ford, aes(mileage, price)) +
  geom_point() +
  geom_line(data = grid_mileage, color="purple", size=1)

confint(pret_Mileage)

#fuelType
pret_FuelType <- lm(data = Ford, price ~ fuelType)
summary(pret_FuelType)

confint(pret_FuelType)


#mpg
pret_Mpg <- lm(data = Ford , price ~ mpg)
summary(pret_Mpg)

grid_mpg <- Ford %>%
  data_grid(mpg = seq_range(mpg, 100)) %>%
  add_predictions(pret_Mpg, "price")

ggplot(Ford, aes(mpg, price)) +
  geom_point() +
  geom_line(data = grid_mpg, color="purple", size=1)

confint(pret_Mpg)

#engineSize
pret_EngineSize <- lm(data = Ford, price ~ engineSize)
summary(pret_EngineSize)

grid_engineSize <- Ford %>%
  data_grid(engineSize = seq_range(engineSize, 100)) %>%
  add_predictions(pret_EngineSize, "price")

ggplot(Ford, aes(engineSize, price)) +
  geom_point() +
  geom_line(data = grid_engineSize, color="purple", size=1)

confint(pret_EngineSize)




pret_All <- lm(data = Ford, price ~ model+mpg+year+engineSize+mileage+transmission+fuelType)
summary(pret_All)

pret_without_Transmission_FuelType <- lm(data = Ford, price ~ model+mpg+year+engineSize+mileage)
summary(pret_without_Transmission_FuelType)


selected_columns <- c("model", "price")


ford_cars <- Ford



max_price_model <- ford_cars %>%
  arrange(desc(price)) %>%
  slice(1) %>%
  select(model, price)


min_price_model <- ford_cars %>%
  arrange(price) %>%
  slice(1) %>%
  select(model, price)


print("Modelul cu cel mai mare preț:")
print(max_price_model)

print("Modelul cu cel mai mic preț:")
print(min_price_model)





priceCar <- tibble(
  year=2020,
  model="Fiesta",
  mpg=32.4,
  engineSize=1,
  mileage=30000,
  transmission="Manual",
  fuelType="Diesel"
)

predict(pret_All, newdata = priceCar, interval="confidence")
predict(pret_All, newdata = priceCar, interval="prediction")


str(Ford)



#arbori de decizie
model_levels <- levels(Ford$model)
numere_model <- as.integer(model_levels)





head(Ford)
str(Ford)



Ford %>% 
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")


set.seed(123)
Ford_split <- initial_split(Ford, prop = 0.7)
Ford_train <- training(Ford_split)
Ford_test <- testing(Ford_split)


Ford %>%
  ggplot(aes(price))+
  geom_density()


arb <- rpart(
  formula = price ~ .,
  data = Ford_train,
  method = "anova"
)

arb
rpart.plot(arb)
plotcp(arb)
arb$cptable  


arb2 <- rpart (
  formula = price ~ .,
  data=Ford_train,
  method="anova",
  control=list(cp=0, xval = 10)
)
arb2
plotcp(arb2)






optim_tree <- rpart(
  formula = price ~. ,
  data = Ford_train,
  method = "anova",
  control = list(minsplit = 11, maxdepth = 13, cp = 0.01)
)


optim_tree
rpart.plot(optim_tree)



pred_price_Ford <- predict(optim_tree, newdata = Ford_test)
RMSE(pred = pred , obs = Ford_test$price)



new_car <- data.frame(model = "Fiesta",
                      year = 2020,
                      transmission = "Manual",
                      mpg = 32.4,
                      engineSize = 1,
                      mileage = 3000,
                      fuelType = "Diesel")


prediction <- predict(arb, newdata = new_car)


actual_prices <- Ford_train$price
error <- prediction - actual_prices
rmse <- sqrt(mean(error^2))


cat("Prețul estimat al mașinii este:", prediction, "\n")
cat("RMSE:", rmse, "\n")




predictions <- predict(arb, newdata = Ford_test)


actual_prices <- Ford_test$price
errors <- predictions - actual_prices


r_squared <- 1 - sum(errors^2) / sum((actual_prices - mean(actual_prices))^2)


mae <- mean(abs(errors))


mse <- mean(errors^2)


root_node_error <- sqrt(mean(errors^2))


#cat("R^2:", r_squared, "\n")
#cat("MAE:", mae, "\n")
#cat("MSE:", mse, "\n")
#cat("Root Node Error:", root_node_error, "\n")


