library(dplyr)
library(ggplot2)

cat('\014') 
graphics.off()

# LOAD IN DATA
deaths.raw <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
deaths <- read.csv(deaths.raw)
head(deaths)

# DATA SUBSETTING AND PREPROCESSING
deaths <- deaths %>% 
  select(-Province.State, -Lat, -Long)

subset <- deaths %>% 
  filter(Country.Region == 'US')
subset <- subset %>% 
  select(-Country.Region)
subset

subset <- t(subset)
subset <- as.data.frame(subset)
colnames(subset) <- c('usa_deaths')
subset$usa_deaths <- log(subset$usa_deaths)

subset <- subset %>% 
  filter(usa_deaths >= 0)

subset$days <- seq(length(subset$usa_deaths)) # add days column
subset$date <- seq(as.Date('2020/02/29'), by = 'days', length.out = length(subset$days)) # add date column

subset <- subset %>% 
  filter(days <= 57)
subset

# LOG PLOT
ggplot(NULL, aes(x = seq(length(subset$usa_deaths)))) +
  geom_point(aes(y = subset$usa_deaths), 
             size = 5) +
  labs(x = 'Days', 
       y = 'Number of Deaths (log)', 
       title = 'COVID-19 Deaths in the USA') + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 25)) + 
  scale_x_continuous(breaks = seq(0, max(subset$days)+5, by = 5)) +
  scale_y_continuous(breaks = seq(0, max(subset$usa_deaths)+1, by = 1)) 
#ggsave('/Users/Grant/Desktop/plot1.jpeg')

# BREAK DATA INTO SUBSETS, ADD GROUPING VARIABLE, CONCAT, AND VISUALIZE SUBSETS
subset1 <- subset %>% 
  filter(days >= 15 & days <= 30)

subset2 <- subset %>% 
  filter(days >= 40 & days <= 57)

subset1$group <- replicate(length(subset1$days), 0)

subset2$group <- replicate(length(subset2$days), 1)

glm_df <- rbind(subset1, subset2)
glm_df

ggplot(NULL, aes(x = glm_df$days)) +
  geom_point(aes(y = glm_df$usa_deaths), 
             size = 5, 
             color = 'red') +
  labs(x = 'Days', 
       y = 'Number of Deaths (log)', 
       title = 'USA COVID-19 Deaths (Linear Subsets)') + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 25)) + 
  scale_x_continuous(breaks = seq(min(glm_df$days), max(glm_df$days), by = 5)) 
#ggsave('/Users/Grant/Desktop/plot2.jpeg')

# CREATE GLM
days <- glm_df$days
deaths <- glm_df$usa_deaths
group <- glm_df$group

model <- lm(deaths ~ days + as.factor(group) + as.factor(group)*days)
summary(model)
confint(model)

# SELECT DATA FROM DAY 57 ONWARDS
deaths <- read.csv(deaths.raw)

deaths <- deaths %>% 
  select(-Province.State, -Lat, -Long)

new_data <- deaths %>% 
  filter(Country.Region == 'US')
new_data <- new_data %>% 
  select(-Country.Region)

new_data <- t(new_data)
new_data <- as.data.frame(new_data)
colnames(new_data) <- c('usa_deaths')
new_data$usa_deaths <- log(new_data$usa_deaths)
new_data <- new_data %>% 
  filter(usa_deaths >= 0)

new_data$days <- seq(length(new_data$usa_deaths)) # add days column
new_data$date <- seq(as.Date('2020/02/29'), by = 'days', length.out = length(new_data$days)) # add date column
new_data$group <- replicate(length(new_data$days), 1)

new_data <- new_data %>% 
  filter(days > 57)
new_data

new_df <- rbind(glm_df, new_data)

# NEW DATA PLOT
ggplot(NULL) +
  geom_point(aes(x = glm_df$days, 
                 y = glm_df$usa_deaths, 
                 color = 'days 15 to 30 and 40 to 57'), 
             size = 4) +
  geom_point(aes(x = new_data$days, 
                 y = new_data$usa_deaths, 
                 color = 'days 58 onward'),
             size = 4) + 
  labs(x = 'Days', 
     y = 'Number of Deaths (log)', 
     title = 'USA COVID-19 Deaths With Days 57 Onward') + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 25), 
        legend.position = c(0.8, 0.15),
        legend.text = element_text(size=20),
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = seq(min(new_df$days), max(new_df$days), by = 5))
#ggsave('/Users/Grant/Desktop/plot3.jpeg')

# CREATE GLM WITH NEW DATA
days <- new_df$days
deaths <- new_df$usa_deaths
group <- new_df$group

new_model <- lm(deaths ~ days + as.factor(group) + as.factor(group)*days)
summary(new_model)
confint(new_model)
