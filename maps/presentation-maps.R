library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(ggpubr)

set_urbn_defaults(style = 'map')

medicaid <- read.csv('medicaid-expansion-outcome.csv')
medicaid <- left_join(medicaid, states, by = c('State' = 'state_name'))

# reorder levels 
medicaid$Expanded.2014 <- as.factor(medicaid$Expanded.2014)
medicaid$Expanded.2014 <- factor(medicaid$Expanded.2014, 
                                 levels = rev(levels(medicaid$Expanded.2014)))
medicaid$Expanded.2019 <- as.factor(medicaid$Expanded.2019)
medicaid$Expanded.2019 <- factor(medicaid$Expanded.2019, 
                                 levels = rev(levels(medicaid$Expanded.2019)))

medicaid <- medicaid %>% 
    mutate(sample_status = case_when(Expanded.2014 == 1 ~ 0, 
                                     Expanded.2019 == 0 ~ 1, 
                                     Expanded.2019 == 1 ~ 2))

# 2013 map 
map_2013 <- ggplot() + 
    geom_polygon(data = medicaid, 
                 mapping = aes(long, lat, group = group, fill = Expanded.2014), 
                 color = '#ffffff', 
                 size = 0.25) +
    scale_fill_manual(values = c('#1696d2', '#db2b27'),  
                      labels = c('Adopted', 'Has Not Adopted')) + 
    theme(legend.title = element_blank()) + 
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
    labs(title = '2013')  +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom', 
          legend.text = element_text(size = 10)) + 
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
map_2013
ggsave('maps_2013.png', dpi = 500)

# 2019 map
map_2018 <- ggplot() + 
    geom_polygon(data = medicaid, 
                 mapping = aes(long, lat, group = group, fill = Expanded.2019), 
                 color = '#ffffff', 
                 size = 0.25) +
    scale_fill_manual(values = c('#1696d2', '#db2b27'),  
                      labels = c('Adopted', 'Has Not Adopted')) + 
    theme(legend.title = element_blank()) + 
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
    theme(legend.title = element_blank(), 
          legend.position = 'bottom', 
          legend.text = element_text(size = 10)) + 
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
map_2018
ggsave('maps_2018.png', dpi = 500)


# ggarrange(map_2013, map_2018, ncol = 1, nrow = 2, common.legend = T, legend = 'bottom')
# ggsave('maps_2013_2018.png', dpi = 500)

test_map <- ggplot() + 
    geom_polygon(data = medicaid, 
                 mapping = aes(long, lat, group = group, fill = as.factor(sample_status)), 
                 color = '#ffffff', 
                 size = 0.25) +
    scale_fill_manual(values = c('#d2d2d2', '#1696d2', '#fdbf11'),  
                      labels = c('Adopted in 2014', 'Has Not Adopted', 'Adopted After 2014')) + 
    theme(legend.title = element_blank(), 
          legend.position = 'bottom', 
          legend.text = element_text(size = 10)) + 
    coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
test_map
ggsave('maps_tri_color.png', dpi = 500)

