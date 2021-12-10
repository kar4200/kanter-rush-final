# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)
library()

# read in the cleaned data
mental_health = read_csv("data/clean/mental_health_clean.csv")

# to do:
# pick features and explore relationship with mentally_unhealthy response

# create histogram of mentally unhealthy days
# save the mean
mean <- mean(mental_health$mentally_unhealthy_days)

# plot mentally_unhealthy days and draw line at the mean
p = mental_health %>%
  ggplot(aes(x = mentally_unhealthy_days)) + 
  geom_histogram() +
  geom_vline(xintercept = mean,
             linetype = "dashed",
             col = "red") +
  labs(x = "Mentally Unhealthy Days (per month)", 
       y = "Number of Counties") +
  theme_bw()

# save the histogram
ggsave(filename = "results/response-histogram.png", 
       plot = p, 
       device = "png", 
       width = 5, 
       height = 3)

# create a heatmap of mentally unhealthy days for cleaned dataset
p2 = map_data("county") %>%
  as_tibble() %>% 
  left_join(mental_health %>% 
              rename(region = state, 
                     subregion = name,
                     `Mentally Unhealthy Days` = mentally_unhealthy_days) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA, size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Mentally Unhealthy Days`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") 

ggsave(filename = "results/map-clean.png", 
       plot = p2, 
       device = "png", 
       width = 7, 
       height = 4)

# create a heatmap of mentally unhealthy days for original health dataset
health_data_clean <- read_csv("data/clean/health_data_clean.csv")

p3 = map_data("county") %>%
  as_tibble() %>% 
  left_join(health_data_clean %>% 
              rename(region = state, 
                     subregion = name,
                     `Mentally Unhealthy Days` = `Mentally Unhealthy Days`) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA,size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Mentally Unhealthy Days`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red")

ggsave(filename = "results/map-all.png", 
       plot = p3, 
       device = "png", 
       width = 7, 
       height = 4)

# create heatmap of highest mentally unhappy days 
wv = map_data("county") %>%
  as_tibble() %>%
  filter(region == "west virginia") %>%
  left_join(mental_health %>% 
              rename(region = state, 
                     subregion = name,
                     `Mentally Unhealthy Days` = mentally_unhealthy_days) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state") %>% filter(region == "west virginia"),
               aes(x=long, y=lat, group=group),
               color="black", fill=NA, size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Mentally Unhealthy Days`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()

ggsave(filename = "results/west-va.png", 
       plot = wv, 
       device = "png", 
       width = 7, 
       height = 4)

nd = map_data("county") %>%
  as_tibble() %>%
  filter(region == "north dakota") %>%
  left_join(mental_health %>% 
              rename(region = state, 
                     subregion = name,
                     `Mentally Unhealthy Days` = mentally_unhealthy_days) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state") %>% filter(region == "north dakota"),
               aes(x=long, y=lat, group=group),
               color="black", fill=NA, size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Mentally Unhealthy Days`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()

ggsave(filename = "results/north-dakota.png", 
       plot = nd, 
       device = "png", 
       width = 7, 
       height = 4)

# examine top 10 counties with lowest mentally unhealthy days
mental_health_clean %>%
  arrange(mentally_unhealthy_days) %>%
  select(state, name, mentally_unhealthy_days) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("State", "County", "Mentally Unhealthy Days"),
        caption = "Top 10 counties with lowest mentally unhealthy days") %>%
  save_kable("low-days.pdf")

# examine top 10 counties with highest mentally unhealthy days
mental_health_clean %>%
  arrange(desc(mentally_unhealthy_days)) %>%
  select(state, name, mentally_unhealthy_days) %>%
  head(10) %>%
  kable(format = "latex",
        booktabs = TRUE, 
        col.names = c("State", "County", "Mentally Unhealthy Days"),
        caption = "Top 10 counties with highest mentally unhealthy days") %>%
  save_kable("high-days.pdf")
  
  
