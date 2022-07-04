library(ggplot2)
library(ggthemes)
library(showtext)
library(dplyr)
library(randomForest)

set.seed(42)
font_add_google("Schoolbell", "bell")
showtext_auto()

df = read.csv("../datasets/pokemon.csv")
head(df)

#drop useless variables
columns_to_drop <- c("abilities", "japanese_name")
df <- df[ , !(names(df) %in% columns_to_drop)]

#change to factor
df$classfication <- as.factor(df$classfication)
df$type1 <- as.factor(df$type1)
df$type2 <- as.factor(df$type2)
df$generation <- as.factor(df$generation)
df$is_legendary <- as.factor(df$is_legendary)

#visualize data

#barchart for type1 counts
ggplot(df, aes(type1, fill= type1)) +
  geom_bar()+
  labs(title="Total Number per type",
       subtitle = "Number of Pokemons per Type 1",
       y = "Count",
       x = "Type 1")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15), axis.text.x = element_text(angle = 90))

#scatterplot attack vs total
ggplot(df, aes(x = attack, y = base_total, color=attack))+
  geom_point()+
  scale_color_gradient2() + 
  labs(title="Attack points vs Total points",
       subtitle = "Scatter plot of how Total Points change based on Attack Points",
       y = "Total Points",
       x = "Attack points")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

#defense of water type vs bug type
bug_type <- filter(df, type1 == "bug")
water_type <- filter(df, type1 == "water")
ggplot(bug_type, aes(defense))+
  geom_histogram(fill= "pink")+
  labs(title="Defense points",
       subtitle = "Defense points of Pokemons of Type Bug",
       y = "Number of Pokemons",
       x = "Defense Value")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

ggplot(water_type, aes(defense))+
  geom_histogram(fill= "lightblue")+
  scale_color_brewer(palette = "set1") + 
  labs(title="Defense points",
       subtitle = "Defense points of Pokemons of Type Water",
       y = "Number of Pokemons",
       x = "Defense Value")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))


# CLT
df_20 <- df[1:20, ]
ggplot(df_20, aes(attack))+
  geom_histogram(fill = "blue")+ 
  labs(title="Attack Points",
       subtitle = "Attack Points of 20 Pokemons",
       y = "Number of Pokemons",
       x = "Attack Value")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

df_200 <- df[1:200, ]
ggplot(df_200, aes(attack))+
  geom_histogram(fill = "red")+ 
  labs(title="Attack Points",
       subtitle = "Attack Points of 200 Pokemons",
       y = "Number of Pokemons",
       x = "Attack Value")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

ggplot(df, aes(attack))+
  geom_histogram(fill = "green")+ 
  labs(title="Attack Points",
       subtitle = "Attack Points of 801 Pokemons",
       y = "Number of Pokemons",
       x = "Attack Value")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))


#confidence interval with 95% confidence
attack.mean <- mean(df$attack)
attack.sd <- sd(df$attack)
error <- qnorm(0.975)* attack.sd / sqrt(801)
confidence_interval <- c(attack.mean - error , attack.mean + error)

ggplot(df, aes(height_m))+
  geom_histogram(fill = "lightgreen")+
  labs(title="Height of Pokemons",
       subtitle = "Height of Pokemons in types of meter",
       y = "Number of Pokemons",
       x = "Height Value in Meters")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))

ggplot(df, aes(height_m))+
  geom_histogram(fill = "lightblue")+
  scale_x_log10()+
  labs(title="Height of Pokemons",
       subtitle = "Height of Pokemons in Log10 scale",
       y = "Number of Pokemons",
       x = "Height Value in Log Scale")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text(), text = element_text(family = "bell", size = 15))


#Wilcoxon rank-sum test

df$height_m[is.na(df$height_m)] <- mean(df$height_m, na.rm = T)
wilcox.test(df$height_m , mu = 1.1, alternative = "two.sided")


#paired T-Test

attack_values <- df$attack
attack_values_rework <- c()
for (i in 1:801) {
  attack_values_rework[i] <- attack_values[i] + runif(1, -5.0 , +5.0)
}

t.test(attack_values, attack_values_rework, paired = T, alternative = "two.sided")

#Fisher's exact test for count data
fisher.test(df$type1, df$is_legendary, simulate.p.value=TRUE)


#ANOVA
normal_type <- filter(df, type1 == "normal")

water_df <- data.frame(water_type$attack, water_type$type1)
colnames(water_df) <- c("attack", "Type1")
normal_df <- data.frame(normal_type$attack, normal_type$type1)
colnames(normal_df) <- c("attack", "Type1")
bug_df <- data.frame(bug_type$attack, bug_type$type1)
colnames(bug_df) <- c("attack", "Type1")

new_df <- rbind(water_df, normal_df, bug_df)

new_df$Type1 <- ordered(new_df$Type1, levels = c("bug", "normal", "water"))

res.anova <- aov(attack ~ Type1, data = new_df)
summary(res.anova)

TukeyHSD(res.anova, conf.level=.95)

#Multiple Linear Regression
columns_to_keep <- c("against_bug", "against_dark", "attack", "hp", "is_legendary", "base_total")
reg_df <- df[ , (names(df) %in% columns_to_keep)]

reg_df_train <- reg_df[1:600, ]
reg_df_test <- reg_df[601:801, ]
reg_df_test_Y <- subset(reg_df_test, select = c(base_total))
reg_df_test_X <- subset(reg_df_test, select = -c(base_total))

reg_df.rf <- randomForest(base_total ~ . , data = reg_df_train)
predictions <- predict(reg_df.rf, reg_df_test)

predicted_vs_true <- data.frame(reg_df_test_Y, predictions)
sqrt(mean((predicted_vs_true$base_total - predicted_vs_true$predictions)^2))
