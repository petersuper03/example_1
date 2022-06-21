library(tidyverse)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
library(nycflights13)
df <- nycflights13::flights
arrange(df, arr_time)

# arrange the column
d1 = arrange(df, desc(arr_delay))

# select specific columns
d2 = select(d1, arr_delay, everything()) # put "arr_delay" column at first and  everything else at behind
select(df, year:dep_delay)
select(df, contains("time")) # select columns which names contain "time"

# create new column by old column
d3 = select(df, year:day, ends_with("delay"), distance, air_time)
mutate(d3, hour = air_time / 60, days = hour / 24)
transmute(d3, hour = air_time / 60, days = hour / 24) # retain newly created variable only

# grouping data and counting the simple statistics
d4 = group_by(df, month)
summarize(d4, dep_time_mean = mean(dep_time, na.rm = T))

delay <- df %>% group_by(dest) %>% 
  summarize(count = n(), 
            ave_dist = mean(distance, na.rm = T), 
            ave_arr_delay = mean(arr_delay, na.rm = T)) %>% 
  filter(count > 20, dest != "HNL")
delay

not_cancelled <- df %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(
    distance_mu = mean(distance), 
    distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd)) %>% 
  head()

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first = min(dep_time), 
    last = max(dep_time)
  ) %>% 
  head()

d3 %>% 
  group_by(year, month, day) %>% 
  filter(rank(desc(arr_delay)) < 10) %>% 
  head()

popular_dests <- df %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% head()  
  
glimpse(mpg)

mpg %>% ggplot(aes(x = displ, y=hwy, color = class)) + 
  geom_point()

mpg %>% ggplot(aes(x = displ, y=hwy)) + 
  geom_point(color = 'blue')

mpg %>% ggplot(aes(x = displ, y=hwy)) + 
  geom_smooth(color = 'blue')

mpg %>% ggplot(aes(x = displ, y=hwy)) + 
  geom_smooth() + 
  geom_point(color = 'blue')

mpg %>% ggplot(aes(x = class)) +
  geom_bar()  
mpg %>% ggplot(aes(x = hwy)) +
  geom_histogram() 

mpg %>% ggplot(aes(x = displ, y=hwy, color = class)) + 
  geom_smooth(se = FALSE) + 
  geom_point()

mpg %>% ggplot(aes(x = displ, y=hwy)) + 
  geom_smooth(se = FALSE) + 
  geom_point(aes(color = class))

table(mpg$class)

mpg %>% ggplot(aes(x = class)) +
  geom_bar()

barplot(table(mpg$class))

mpg %>% 
  count(class) %>% 
  ggplot(aes(x = class, y = n)) + 
  geom_bar(stat = "identity")

mpg %>% ggplot(aes(x = class, fill = drv)) + 
  geom_bar()

mpg %>% ggplot(aes(x = class, fill = drv)) + 
  geom_bar(position = "dodge")

mpg %>% ggplot(aes(x = class, fill = drv)) + 
  geom_bar(position = "fill")

mpg %>% ggplot(aes(displ, hwy)) + 
  geom_point(color = "gray") + 
  stat_summary(fun = "mean", geom = "line", size = 1, linetype = "dashed")

mpg %>% ggplot(aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  scale_color_brewer(palette = "Set1")

mpg %>% ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~class, nrow = 2)

mpg %>% ggplot(aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_grid(year~cyl)

mpg %>% ggplot(aes(displ, hwy, color = class)) + 
  geom_point() + 
  labs(title = "Fuel Efficiency by Engine Power",
       subtitle = "Fuel economy data from 1999 and 2008 for 38 popular models of cars",
       x = "Engine Displacement (liters)",
       y = "Fuel Efficiency (miles per gallon)",
       color = "Car Type")

library(DataExplorer)
insurance <- read_csv("https://raw.githubusercontent.com/Ying-Ju/R_Data_Analytics_Series_NTPU/main/insurance.csv")
glimpse(insurance)
insurance %>% plot_intro()
insurance %>% plot_missing()
insurance %>% plot_bar(with = "charges")

insurance_Q <- insurance %>% 
  select(age, bmi, charges, region) %>% 
  drop_na()
insurance_Q %>% plot_boxplot(by = "region")

insurance_Q %>% select(-region) %>% 
  plot_scatterplot(by = "charges")

insurance_Q %>% select(-region) %>% 
  plot_correlation(cor_args = list("use" = "complete.obs"))
create_report(insurance, output_file = "report.html")





















