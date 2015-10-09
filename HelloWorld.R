
#############
### CRAN R - Advance Programming
#############


library(ggplot2)
library(dplyr)

diamonds <- tbl_df(diamonds)

subset(diamonds, carat > 3 & color %in% c("D","E","F") & cut %in% c("Ideal", "Premium"))[, c("carat", "cut", "color", "clarity", "price")]

filter(diamonds, carat > 3 & color %in% c("D","E","F") & cut %in% c("Ideal", "Premium"))

diamonds[,c("carat", "cut", "color", "clarity")]

diamonds %>%
  filter(carat > 3 &
         color %in% c("D","E","F") &
         cut %in% c("Ideal", "Premium")) %>%
  select(carat, cut, color, clarity, price)


diamonds_carat <- order(diamonds[, c("color","price","carat")]$carat, decreasing = TRUE)

diamonds %>%
  select(color, price, carat) %>%
  arrange(color, price, desc(carat))

diamonds %>%
  select(color, price, carat) %>%
  mutate(ppc = price / carat,
         ppc_rnd = round(ppc, 2))

diamonds %>% 
  group_by(color) %>%
  summarise(MIN = min(price), MEAN = mean(price), MAX = max(price))

###### GGPLOT

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point()

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point() +
  stat_smooth(method = "loess", se = FALSE, 
              colour = "red", size = 1.5, linetype = "dashed")

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, 
              colour = "red", size = 1.5, linetype = "dashed") + 
  facet_wrap(~cut) + 
  theme_bw() + 
  theme(panel.grid = element_blank())

p <- ggplot(aes(x = color, y = price), data = diamonds)

p + geom_boxplot() +
  facet_wrap(~cut)

p <- ggplot(aes(x = price), data = diamonds)

p + geom_histogram()
p + geom_density(fill = "blue50") 
