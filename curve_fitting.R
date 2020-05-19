library(dplyr)
library(ggplot2)

cat('\014') 
graphics.off()

deaths.raw <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'

deaths <- read.csv(deaths.raw)
head(deaths)

# DATA SUBSETTING
deaths <- deaths %>% 
  select(-Province.State, -Lat, -Long)
print(deaths)

subset <- deaths %>% 
  filter(Country.Region == 'US' |
         Country.Region == 'Korea, South' |
         Country.Region == 'Spain' |
         Country.Region == 'Italy')
print(subset)

subset <- subset %>% 
  select(-Country.Region)
print(subset)

subset <- t(subset)
colnames(subset) <- c('italy', 'south.korea', 'spain', 'usa')
subset <- as.data.frame(subset)
print(subset)
colnames(subset)

# USA
usa <- subset['usa']
usa <- usa %>% 
  filter(usa > 1)
usa <- log(as.numeric(unlist(usa)))
print(usa)

x <- seq(length(usa))
y <- usa

model2 <- lm(y ~ poly(x, 3, raw=TRUE)) 
model3 <- lm(y ~ poly(x, 4, raw=TRUE)) 
model4 <- lm(y ~ poly(x, 5, raw=TRUE))

summary(model2)
summary(model3)
summary(model4)

ggplot(NULL, aes(x = x)) +
  geom_point(aes(y = y), size = 1.7) +
  geom_line(aes(y = predict(model2, data.frame(x=x)), 
                color = '3rd degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model3, data.frame(x=x)), 
                color = '4th degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model4, data.frame(x=x)), 
                color = '5th degree'), 
            size = 0.6) +
  labs(x = 'Days', 
       y = 'Number of Deaths (log)', 
       title = 'US COVID-19 Deaths') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size=12),
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = seq(0, max(x), 5)) + 
  scale_y_continuous(breaks = seq(0, max(y)+1, 1))

# ITALY
italy <- subset['italy']
italy <- italy %>% 
  filter(italy > 0)
italy <- log(as.numeric(unlist(italy)))
print(italy)

x <- seq(length(italy))
y <- italy

model1 <- lm(y ~ poly(x, 2, raw=TRUE)) 
model2 <- lm(y ~ poly(x, 3, raw=TRUE)) 
model3 <- lm(y ~ poly(x, 4, raw=TRUE)) 

summary(model1)
summary(model2)
summary(model3)

ggplot(NULL, aes(x = x)) +
  geom_point(aes(y = y), size = 1.7) +
  geom_line(aes(y = predict(model1, data.frame(x=x)), 
                color = '2nd degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model2, data.frame(x=x)), 
                color = '3rd degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model3, data.frame(x=x)), 
                color = '4th degree'), 
            size = 0.6) +
  labs(x = 'Days', 
       y = 'Number of Deaths (log)', 
       title = 'Italy COVID-19 Deaths') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size=12),
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = seq(0, max(x), 5)) + 
  scale_y_continuous(breaks = seq(0, max(y)+1, 1))

# SOUTH KOREA
sk <- subset['south.korea']
sk <- sk %>% 
  filter(sk > 0)
sk <- log(as.numeric(unlist(sk)))
print(sk)

x <- seq(length(sk))
y <- sk

model1 <- lm(y ~ poly(x, 2, raw=TRUE)) 
model2 <- lm(y ~ poly(x, 3, raw=TRUE)) 
model3 <- lm(y ~ poly(x, 4, raw=TRUE)) 
model4 <- lm(y ~ poly(x, 5, raw=TRUE)) 
model5 <- lm(y ~ poly(x, 6, raw=TRUE)) 

summary(model2)
summary(model3)
summary(model4)
summary(model5)

ggplot(NULL, aes(x = x)) +
  geom_point(aes(y = y), size = 1.7) +
  geom_line(aes(y = predict(model2, data.frame(x=x)), 
                color = '3rd degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model3, data.frame(x=x)), 
                color = '4th degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model4, data.frame(x=x)), 
                color = '5th degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model5, data.frame(x=x)), 
                color = '6th degree'), 
            size = 0.6) +
  labs(x = 'Days', 
       y = 'Number of Deaths (log)', 
       title = 'South Korea COVID-19 Deaths') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size=12),
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = seq(0, max(x), 5)) + 
  scale_y_continuous(breaks = seq(0, max(y)+1, 1))

# SPAIN
spain <- subset['spain']
spain <- spain %>% 
  filter(spain > 0)
spain <- log(as.numeric(unlist(spain)))
print(spain)

x <- seq(length(spain))
y <- spain

model1 <- lm(y ~ poly(x, 2, raw=TRUE)) 
model2 <- lm(y ~ poly(x, 3, raw=TRUE)) 
model3 <- lm(y ~ poly(x, 4, raw=TRUE)) 

summary(model1)
summary(model2)
summary(model3)

ggplot(NULL, aes(x = x), color = colors) +
  geom_point(aes(y = y), size = 1.7) +
  geom_line(aes(y = predict(model1, data.frame(x=x)), 
                color = '2nd degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model2, data.frame(x=x)), 
                color = '3rd degree'), 
            size = 0.6) +
  geom_line(aes(y = predict(model3, data.frame(x=x)), 
                color = '4th degree'), 
            size = 0.6) +
  labs(x = 'Days', 
       y = 'Number of Deaths (log)', 
       title = 'Spain COVID-19 Deaths') + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.2),
        legend.text = element_text(size=12),
        legend.title = element_blank()) + 
  scale_x_continuous(breaks = seq(0, max(x)+5, 5)) + 
  scale_y_continuous(breaks = seq(0, max(y)+1, 1))
