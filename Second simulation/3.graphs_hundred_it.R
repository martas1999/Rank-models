# libraries needed
library(tidyverse)

# loading the necessary grid
grid_kendall_def <-readRDS("grid_kendall_def.rds")



# reproduce Figure 4 in the manuscript: 
#Boxplots of normalised Kendall distances between true rankings and estimated one, 
#by competitiveness levels and number of entrants, considering 500 games and 50 potential competitors. 

Figure_four <- grid_kendall_def %>% 
  filter(n_games == 500 & n_comp == 50 & n_comp_per_game != 40) %>% 
  ggplot(aes(x=n_comp_per_game, y=kendall_distance, color = model)) + 
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

# reproduce Figure 5 in the manuscript: 
#Boxplots of normalised Kendall distances for the different models, 
#by competitiveness levels and number of games, considering 50 potential competitors and 20 entrants. 

Figure_five <- grid_kendall_def %>% 
  filter(n_comp == 50 & n_comp_per_game ==20)  %>% 
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

# same figure but considering the average normalised kendall distances (not present in the manuscript)
figure_four_mean  <- grid_kendall %>% 
  filter(n_games  == 500 & n_comp == 500 ) %>% 
  group_by(n_games, n_comp, n_comp_per_game, sigma, model)%>% 
  summarize(
    mean_kendall_distance = mean(kendall_distance),
  ) %>% 
  ggplot(aes(x=n_comp_per_game, y=mean_kendall_distance, color = model)) + 
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  labs(x = "Number of entrants",
       y = " Average normalised Kendall's distance")+
  facet_wrap(.~sigma, nrow = 1, labeller = "label_both") +
  theme_minimal()+
  theme(axis.text = element_text( size = 14 ),
        axis.text.x = element_text( size = 14 ),
        axis.title = element_text( size = 14),
        legend.position = "right",
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 14))







