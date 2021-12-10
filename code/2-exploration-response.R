# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)

# read in the cleaned data
mental_health = read_csv("data/clean/mental_health_clean.csv")

# to do:
# correlation plot (ggcorplot)
# create histogram of mentally_unhealthy_days and plot mean (examine bottom 5, top 5)
# pick features and explore relationship with mentally_unhealthy response
# heatmap 

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

# correlation plot hgidsg

# examine top 10 counties by case fatality rate
covid_data %>% 
  select(county, state, case_fatality_rate) %>%
  arrange(desc(case_fatality_rate)) %>%
  head(10) %>%
  write_tsv("results/top-10-counties-data.tsv")

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
