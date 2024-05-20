# libraries needed
library(tidyverse)

# loading the necessary grid
grid_kendall_def <-readRDS("grid_kendall_def.rds")


## Figure 4 --------------------------
# reproduce Figure 4 in the manuscript: 
#Boxplots of normalised Kendall distances between true rankings and estimated one, 
#by competitiveness levels and number of entrants, considering 500 games and 50 potential competitors. 

Figure_four <- grid_kendall_def %>% 
  filter(n_games == 500 & n_comp == 50 & n_entrants != 40) %>% 
  ggplot(aes(x=n_entrants, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of entrants, \nwith 50 competitors and 500 games",
       x = "Number of entrants",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.55), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5), labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))
Figure_four

#The capacity of the Beta models to achieve lower MSE in high-competitive contexts
# compared to ROL is not translated into addition power to correctly rank competitors.

# testing if the same pattern is observed also for different number of games and total competitors
# (go to Figure 5 section if you directly want to reproduce Figure 5)
# n_games = 250 and n_comp = 50
grid_kendall_def %>% 
  filter(n_games == 250 & n_comp == 50) %>% 
  ggplot(aes(x=n_entrants, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of entrants, \nwith 50 competitors and 500 games",
       x = "Number of entrants",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.55), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5), labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_games = 1250 and n_comp = 50
grid_kendall_def %>% 
  filter(n_games == 1250 & n_comp == 50) %>% 
  ggplot(aes(x=n_entrants, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of entrants, \nwith 50 competitors and 500 games",
       x = "Number of entrants",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.55), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5), labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_games = 50 and n_comp = 50
grid_kendall_def %>% 
  filter(n_games == 50 & n_comp == 50) %>% 
  ggplot(aes(x=n_entrants, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of entrants, \nwith 50 competitors and 500 games",
       x = "Number of entrants",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.55), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5), labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_games = 500 and n_comp = 15
grid_kendall_def %>% 
  filter(n_games == 500 & n_comp == 15) %>% 
  ggplot(aes(x=n_entrants, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of entrants, \nwith 50 competitors and 500 games",
       x = "Number of entrants",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.55), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5), labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_games = 500 and n_comp = 250
grid_kendall_def %>% 
  filter(n_games == 500 & n_comp == 250) %>% 
  ggplot(aes(x=n_entrants, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of entrants, \nwith 50 competitors and 500 games",
       x = "Number of entrants",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_continuous(limits = c(0, 0.55), breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5), labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# the same pattern observed in Figure 4 replicates for different number of total competitors and number of games

## Figure 5 -----------
# reproduce Figure 5 in the manuscript: 
#Boxplots of normalised Kendall distances for the different models, 
#by competitiveness levels and number of games, considering 50 potential competitors and 20 entrants. 

Figure_five <- grid_kendall_def %>% 
  filter(n_comp == 50 & n_entrants ==20)  %>% 
  ggplot(aes(x=n_games, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of games, \nwith 50 competitors and 20 entrants",
       x = "Number of games",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))
Figure_five
# All models become more efficient to correctly rank competitors for increasing
# number of games across all competitiveness levels.


# testing if the same pattern is observed also for different number of entrants and total competitors
# n_comp = 50 and 2 entrants
grid_kendall_def %>% 
  filter(n_comp == 50 & n_entrants ==2)  %>% 
  ggplot(aes(x=n_games, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of games, \nwith 50 competitors and 20 entrants",
       x = "Number of games",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_comp = 50 and 8 entrants
grid_kendall_def %>% 
  filter(n_comp == 50 & n_entrants ==8)  %>% 
  ggplot(aes(x=n_games, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of games, \nwith 50 competitors and 20 entrants",
       x = "Number of games",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_comp = 50 and 40 entrants
grid_kendall_def %>% 
  filter(n_comp == 50 & n_entrants ==40)  %>% 
  ggplot(aes(x=n_games, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of games, \nwith 50 competitors and 20 entrants",
       x = "Number of games",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_comp = 250 and 20 entrants
grid_kendall_def %>% 
  filter(n_comp == 250 & n_entrants ==20)  %>% 
  ggplot(aes(x=n_games, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of games, \nwith 50 competitors and 20 entrants",
       x = "Number of games",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# n_comp = 250 and 20 entrants
grid_kendall_def %>% 
  filter(n_comp == 500 & n_entrants ==20)  %>% 
  ggplot(aes(x=n_games, y=kendall_distance, color = model)) + 
  geom_boxplot()+
  labs(title = "Normalised Kendall distances for different competitiveness levels and number of games, \nwith 50 competitors and 20 entrants",
       x = "Number of games",
       y = "Normalised Kendall distance")+
  theme_minimal()+
  scale_y_log10(labels = scales::comma)+
  facet_wrap(.~sigma, nrow = 1) +
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14),
        plot.title = element_text(size = 14))

# also for other combination of number of total potential competitors and number of entrants, 
# Beta models always improved for higher number of games considered. It's worth noting that the superiority of the
# ROL model clearly emerges only in low-competitive scenarios.


