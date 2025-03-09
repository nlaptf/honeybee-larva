
library("lme4")
library("lmerTest")
library("dplyr")
library("nlme")
library("lsmeans")
library("PMCMRplus")
library("car")
library("multcomp")
library("dunn.test")
library("MASS")
library("tidyr")
library("purrr")


dataColcombin<- read.csv("D:/Postdoct/2023.06.26 OR11 expression/Larva Behavior/Larvae_data_three_colony.csv")
attach(dataColcombin)
shapiro.test(sqrt(Number_Consumed))

dataColcombin$Colony <- factor(dataColcombin$Colony)
model1.1 <- lme(sqrt(Number_Consumed)~Food_Volume, random = ~1|Colony, data=dataColcombin)
residuals_model <- residuals(model1.1)
fitted_values <- fitted(model1.1)
plot(fitted_values, residuals_model, main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")
qqnorm(residuals_model)
qqline(residuals_model)
anova(model1.1)
res <- wilcox.test(Number_Consumed ~ Food_Volume, data = dataColcombin, var.equal = TRUE)
res



datacolthree_Accumlated<- read.csv("D:/Postdoct/2023.06.26 OR11 expression/Larva Behavior/Larvae_data_accumlated_drop_Colonythree.csv")
attach(datacolthree_Accumlated)
datacolthree_Accumlated <- pivot_longer(datacolthree_Accumlated, cols = starts_with("Sample"), names_to = "Sample", values_to = "Value")
datacolthree_Accumlated <- datacolthree_Accumlated %>% filter(!is.na(Value))

shapiro_results <- datacolthree_Accumlated %>%
  group_by(Hour, Food_Volume) %>%
  summarise(
    sample_size = n(),
    variance = var(Value),
    shapiro_p_value = ifelse(sample_size >= 3 & variance != 0, shapiro.test(Value)$p.value, NA)
  )
print(n=40,shapiro_results)
results <- datacolthree_Accumlated %>%
  group_by(Hour, Food_Volume) %>%
  summarise(Value_List = list(Value), .groups = 'drop') %>%
  pivot_wider(names_from = Food_Volume, values_from = Value_List) %>%
  mutate(
    Wilcox_Test = pmap(list(`2ul`, `5ul`), ~wilcox.test(x = .x, y = .y, exact = FALSE)),
    W = map_dbl(Wilcox_Test, 'statistic'),
    p_value = map_dbl(Wilcox_Test, 'p.value')
  ) %>%
  select(Hour, W, p_value)
print(results)



dataLarvae_data_three_Weight<- read.csv("D:/Postdoct/2023.06.26 OR11 expression/Larva Behavior/Larvae_data_three_Weight.csv")
attach(dataLarvae_data_three_Weight)

subdata2ul <- subset(dataLarvae_data_three_Weight, Food_Volume == "2ul")
shapiro.test(subdata2ul$Number_Consumed)
shapiro.test(subdata2ul$Weight_Before)

test_result1 <- cor.test(subdata2ul$Number_Consumed, subdata2ul$Weight_Before, method = "spearman")
print(test_result1)
summary(lm(subdata2ul$Number_Consumed ~ subdata2ul$Weight_Before, data = subdata2ul))

subdata5ul <- subset(dataLarvae_data_three_Weight, Food_Volume == "5ul")
test_result5 <- cor.test(subdata5ul$Number_Consumed, subdata5ul$Weight_Before, method = "spearman")
print(test_result5)
summary(lm(subdata5ul$Number_Consumed ~ subdata5ul$Weight_Before, data = subdata5ul))





