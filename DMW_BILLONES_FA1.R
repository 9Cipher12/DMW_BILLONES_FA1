
#Lecture 2 ==================================================================================
install.packages("tidyverse")
library(tidyverse)
diamonds

?diamonds

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat))
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat)) +
  coord_cartesian(xlim = c(0,3))
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.01) +
  coord_cartesian(xlim = c(0,3))
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y))
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(3.5, 10), ylim = c(3.5,10))
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_cartesian(xlim = c(3.5, 10), ylim = c(3.5,10))


install.packages("dplyr")
library(dplyr)
avg_price_by_carat <- diamonds %>%
  mutate(carat = round(carat, 1)) %>%
  group_by(carat) %>%
  summarise(avg_price = mean(price))
avg_price_by_carat
ggplot(data = avg_price_by_carat) +
  geom_line(mapping = aes(x = carat, y = avg_price))


ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = cut, y = price))
library(stat471)
corrmat <- mixed_color(diamonds)
corrmat

install.packages("ggcorplot")
library(ggcorrplot)
ggcorrplot(corrmat, lab = TRUE, hc.order = TRUE)

ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), binwidth = 0.01)

ggplot(data = diamonds) +
  geom_histogram(aes(x = carat), binwidth = 0.01) +
  scale_x_log10()

ggplot(data = diamonds) +
  geom_point(aes(x = carat, y = price)) +
  scale_x_log10() +
  scale_y_log10()

ggplot(data = avg_price_by_carat) +
  geom_line(mapping = aes(x = carat, y = avg_price)) +
  geom_point(mapping = aes(x = carat, y = avg_price))

ggplot(data = avg_price_by_carat, mapping = aes(x = carat, y = avg_price)) +
  geom_line() +
  geom_point()

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price, color = cut))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  facet_wrap(~cut)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  facet_grid(. ~ cut)

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  facet_grid(clarity ~ cut)


#Lecture 3 ==================================================================================

#1
library(tidyverse)
diamonds

#2.1
# subset to diamonds with price at least $10,000
filter(diamonds, price >= 10000)
# subset to diamonds with price at least $10,000 AND clarity VVS1 or IF
filter(diamonds, price >= 10000 & clarity %in% c("VVS1", "IF"))
#2.2
# select columns corresponding to the "4 C's"
select(diamonds, carat, cut, color, clarity)
#2.3
arrange(diamonds, desc(carat)) # sort diamonds by carat (descending)

#3.1
# add column that is the price per carat of each diamond
mutate(diamonds, price_per_carat = price/carat)
# add column that indicates whether a diamond's price per carat is at least $10k
mutate(diamonds, fancy_diamond = price/carat > 10000)
# use if_else() if you have two cases
mutate(diamonds,
       good_value =
         if_else(
           condition = carat > 2, # check whether carat > 2
           true = price < 5000, # if so, good value if cheaper than $5k
           false = price < 1000 # if not, good value if cheaper than $1k
         )
)
# use case_when() if you have more than two cases
mutate(diamonds,
       value =
         case_when(
           carat > 2 & price < 5000 ~ "good", # if carat > 2 and price < 5000, then good
           carat > 1 & price < 2500 ~ "ok", # if carat > 1 and price < 2500, then ok
           TRUE ~ "bad" # otherwise, bad
         )
)
#3.2
# find the number of "fancy" diamonds (price per carat at least $10000),
summarise(diamonds, num_fancy_diamonds = sum(price/carat > 10000))
# find the number of "fancy" diamonds (price per carat at least $10000),
# as well as the mean price of a diamond
summarise(diamonds,
          num_fancy_diamonds = sum(price/carat > 10000),
          mean_diamond_price = mean(price))

#4.1
diamonds %>% # pipe in the data
  filter(cut == "Premium") %>% # restrict to premium cut diamonds
  mutate(price_per_carat = price/carat) %>% # add price_per_carat variable
  arrange(desc(price_per_carat)) # sort based on price_per_carat
diamonds %>% # pipe in the data
  filter(cut == "Premium") %>% # restrict to premium cut diamonds
  mutate(price_per_carat = price/carat) %>% # add price_per_carat variable
  ggplot() + # start a ggplot
  geom_histogram(aes(x = price_per_carat)) # add a histogram
#4.2
diamonds %>% # pipe in the data
  group_by(cut) %>% # group by cut
  summarise(max_price = max(price)) # find the max price for each cut
diamonds %>% # pipe in the data
  group_by(cut, clarity) %>% # group by both cut and clarity
  summarise(max_price = max(price)) # find the max price for each group
diamonds %>% # pipe in the data
  group_by(cut, clarity) %>% # group by both cut and clarity
  summarise(max_price = max(price)) %>% # find the max price for each group
  ungroup()# remove grouping
count(diamonds, cut)
#4.3
diamonds
max_prices <- diamonds %>% # pipe in the data
  group_by(cut) %>% # group by cut
  summarise(max_price = max(price)) # find the max price for each cut
max_prices



#Lecture 4 ==================================================================================

#1
library(tidyverse)
#2
heights <- read_csv(file = "C:/Users/Cipher/Desktop/CRISTEL_DMW/heights.csv")
#3
table1
table2
table3
table4a
table4b
#4.1
table4a
table4a %>%
  pivot_longer(cols = c(`1999`, `2000`), names_to = "year", values_to = "cases")
#4.2
table2
table2 %>%
  pivot_wider(names_from = type, values_from = count)

#EXERCISE___________________________

#5
tidy4a <- table4a %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
tidy4a
tidy4b
left_join(x = tidy4a, y = tidy4b, by = c("country", "year"))
x <- tribble(
  ~state, ~population,
  "PA", 12.8,
  "TX", 28.6,
  "NY", 19.5
)
y <- tribble(
  ~state, ~capital,
  "TX", "Austin",
  "CA", "Sacramento",
  "NY", "New York City",
  "MI", "Lansing"
)
x
y
#6
table3
table3 %>%
  separate(rate, into = c("cases", "population"))
table3 %>%
  separate(rate, into = c("cases", "population"), sep = "/")
table3 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE)
table3 %>%
  separate(year, into = c("century", "year"), sep = 2)

#7
stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c( 1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
stocks

stocks %>%
  pivot_wider(names_from = year, values_from = return)
stocks %>%
  na.omit()



























 