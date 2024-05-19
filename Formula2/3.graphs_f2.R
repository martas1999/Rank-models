# libraries needed
library(tidyverse)
library(forcats)
library(kableExtra)


# reorder before ggplot 
# combine together the ability estimates from the different models, created in the script "2.f2_models_run.r"
estimates <- bind_rows(
  rol = rol_est,
  beta0 = beta0_est,
  beta1 = beta1_est,
  beta2 = beta2_est,
  .id = "model") 

# assigning more meaningful names to the models
estimates$model <- fct_recode(estimates$model, "ROL" = "rol", 
                           "Fixed-Beta" = "beta0", "Varying-Beta" = "beta1", "Dependent-Beta" = "beta2")

# Figure 6: Ability's estimates for Formula 2 drivers who finished at least 30 races
pd<- position_dodge(width = 0.5) 
estimates %>% 
  ggplot(aes(x = factor(Driver, levels=c(names_first_thirdy_ROL)), y = theta, color = model)) +
  geom_point(position = pd)+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, position = pd ) +
  labs(title="Ability's estimates for Formula 2 drivers who finished at least 30 races, \nconsidering 2017-2023 seasons ",
       x = "Driver",
       y = "Estimated ability") +
  theme_minimal()+
  theme(axis.text.y = element_text(angle =45, hjust = 1))+
  theme(axis.text = element_text( size = 9 ),
        axis.text.x = element_text( size = 9),
        axis.title = element_text( size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        strip.text = element_text(size = 9),
        plot.title = element_text(size=9))+
  coord_flip()








# Table 2: Ranking of the drivers who competed at least 30 times, according to the different models
rol_est_desc <- as.data.frame(arrange(rol_est, desc(theta)))
beta0_est_desc <- as.data.frame(beta0_est)
beta1_est_desc <- as.data.frame(beta1_est)
beta2_est_desc <- as.data.frame(beta2_est)
table <-cbind(levels(rol_est_desc$Driver)[rol_est_desc$Driver], levels(beta0_est_desc$Driver)[beta0_est_desc$Driver], levels(beta1_est_desc$Driver)[beta1_est_desc$Driver],levels(beta2_est_desc$Driver)[beta2_est_desc$Driver])
table <-as.data.frame(table)
table <- table %>% 
  mutate(ROL = V1,
         Fixed_Beta = V2,
         Varying_Beta = V3,
         Dependent_Beta = V4)
table<- table %>% 
  select(ROL, Fixed_Beta, Varying_Beta, Dependent_Beta)
 
ranks_table<- table %>% 
         mutate(Rank = as.factor(1:13),
                ROL = as.factor(ROL),
                Fixed_Beta = as.factor(Fixed_Beta),
                Varying_Beta = as.factor(Varying_Beta),
                Dependent_Beta = as.factor(Dependent_Beta))
View(ranks_table)









                                       