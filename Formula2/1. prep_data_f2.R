# libraries needed
library(rvest)
library(tidyverse)

# create the vector of names of each race for each season:
races_names_2023 <- c("sakhir","jeddah","melbourne","azerbaijan-grand-prix","monaco","catalunya","red-bull-ring-2","silverstone-2","hungaroring","spa-francorchamps","zandvoort","monza","yas-marina")
races_names_2022 <- c("sakhir","jeddah","imola","barcelona-2","monte-carlo","baku","silverstone-2","spielberg","le-castellet","budapest-2","spa-francorchamps","zandvoort","monza-2","yas-island")
races_names_2021 <-c("sakhir","monte-carlo","baku","silverstone","monza","sochi","jeddah","yas-marina")
races_names_2020 <-c("spielberg-4","spielberg-3","budapest-2","silverstone-3","silverstone-2","barcelona","spa-francorchamps","monza-2","mugello-2","sochi-2","sakhir-3","sakhir-2")
races_names_2019 <-c("bahrain","baku","catalunya-2","monaco","paul-ricard","red-bull-ring-2","silverstone-2","hungaroring","spa-francorchamps","monza-2","sochi-2","yas-marina")
races_names_2018 <-c("bahrain","baku","catalunya","monaco","paul-ricard","red-bull-ring-2","silverstone-2","hungaroring-2","spa-francorchamps","monza","sochi","yas-marina")
races_names_2017 <-c("bahrain","catalunya","monaco","baku","red-bull-ring-2","silverstone","hungaroring-2","spa-francorchamps","monza","jerez","yas-marina")


#' Function that retrieve the data from the webpage where the results of each race in Formula 2 is stored
#' @param year  season id
#' @param races_names  vector with the name of the races previously created
#' 
#' @returns a list where each element represents the results of each race
data_scraping <- function(year,races_names){
  races_list <- list()
  for ( name in races_names){
    races <- paste0("https://fiaresultsandstatistics.motorsportstats.com/results/", year,"-", name, "/classification")
    page <- read_html(races)
    races_list[[name]] <-html_table(page)[1]
  }
  return(races_list)
}

#' Function that remove the unnecessary information from the data from each race, and removes
#' the drivers who could not finish the race for any kind of reason and adds a game id indicator for each race
#' @param list  list of race results, as generated from the data_scraping() function
#' 
#' @returns a tidy data frame with three variables: 1. Pos : final position of the driver in the race ì; 
#'                                                  2. Driver: name of the driver;
#'                                                  3. game_id: race identifier
#'                                                  4. prop_beaten_transformed: proportion of drivers beaten by the driver, excluding 1 and 0
tidy_results_first_races <- function(list){
  res <- data.frame(matrix(NA, nrow = 0, ncol = 5))
  for (i in 1:length(list)){
    game_i <- list[[i]] %>% 
      as.data.frame() %>% 
      select(c("Pos","Driver")) %>% 
      filter(!Pos == "DNF",
             !Pos == "DSQ",
             !Pos == "NC",
             !Pos == "DNS",
             !Pos == "WIT") %>%
      mutate(game_id =rep(i,nrow(.)),
             Pos =as.numeric(Pos))
    n_comp_per_game <- game_i %>% 
      summarize(len = n()) %>% pull(len)
    game_ii <- game_i %>% 
      mutate(prop_beaten = (n_comp_per_game- game_i$Pos) / (n_comp_per_game - 1),
             prop_beaten_transformed = (prop_beaten * (n_comp_per_game - 1) + 0.5) / (n_comp_per_game)) %>% 
      select(-prop_beaten)
    res <- rbind(res,game_ii)
  }
  return(res)
}



#' Function that remove the unnecessary information from the data from each race, and removes
#' the drivers who could not finish the race for any kind of reason and adds a game id indicator for each race
#' @param list  list of race results, as generated from the data_scraping() function
#' 
#' @returns a tidy data frame with three variables: 1. Pos : final position of the driver in the race ì; 
#'                                                  2. Driver: name of the driver;
#'                                                  3. prop_beaten_transformed: proportion of drivers beaten by the driver, excluding 1 and 0
tidy_results_other_races <- function(list){
  res <- data.frame(matrix(NA, nrow = 0, ncol = 5))
  for (i in 1:length(list)){
    game_i <- list[[i]] %>% 
      as.data.frame() %>% 
      select(c("Pos","Driver")) %>% 
      filter(!Pos == "DNF",
             !Pos == "DSQ",
             !Pos == "NC",
             !Pos == "DNS",
             !Pos == "WIT") %>%
       mutate(Pos =as.numeric(Pos))
     n_comp_per_game <- game_i %>% 
          summarize(len = n()) %>% pull(len)
     game_ii <- game_i %>% 
      mutate(prop_beaten = (n_comp_per_game- game_i$Pos) / (n_comp_per_game - 1),
             prop_beaten_transformed = (prop_beaten * (n_comp_per_game - 1) + 0.5) / (n_comp_per_game)) %>% 
      select(-prop_beaten)
    res <- rbind(res,game_ii)
  }
  return(res)
}



# For each season, retrieve race results using data_scraping() and prepare the data using tidy_results_first_races() 

# data for the 2023 season
list_races_2023 <- data_scraping(2023,races_names_2023)
f2_2023 <- tidy_results_first_races(list_races_2023)
# addition of a season id 
f2_2023 <- f2_2023 %>% 
  mutate(season = rep(2023, nrow(.)))
saveRDS(f2_2023, "f2_2023_correct.rds")


# data for the 2022 season
list_races_2022 <- data_scraping(2022,races_names_2022)
f2_2022 <-tidy_results_other_races(list_races_2022)
# addition of a season id and a game id
f2_2022 <-f2_2022 %>% 
  mutate(game_id = rep(14:27,times=c(19,18,17,20,17,16,19,19,17,21,21,17,13,20)),
         season = rep(2022, nrow(.))) 
saveRDS(f2_2022, "f2_2022_correct.rds")

# data for the 2021 season  
list_races_2021 <- data_scraping(2021,races_names_2021)
f2_2021 <-tidy_results_other_races(list_races_2021)
f2_2021 <-f2_2021 %>% 
  mutate(game_id = rep(28:35,times=c(19,18,19,22,16,19,20,20)),
         season = rep(2021, nrow(.))) 
saveRDS(f2_2021, "f2_2021_correct.rds")

# data for the 2020 season  
list_races_2020 <- data_scraping(2020,races_names_2020)
f2_2020 <-tidy_results_other_races(list_races_2020)
f2_2020 <- f2_2020 %>% 
  mutate(game_id = rep(36:47,times=c(18,18,21,19,21,21,17,18,20,20,19,21)),
         season = rep(2020, nrow(.))) 
saveRDS(f2_2020, "f2_2020_correct.rds")


# data for the 2019 season  
list_races_2019 <- data_scraping(2019,races_names_2019)
f2_2019 <-tidy_results_other_races(list_races_2019)
f2_2019 <- f2_2019 %>% 
  mutate(game_id = rep(48:59,times=c(19,13,19,15,19,18,18,19,20,15,17,17)),
         season = rep(2019, nrow(.)))
saveRDS(f2_2019, "f2_2019_correct.rds")

# data for the 2018 season  
list_races_2018 <- data_scraping(2018,races_names_2018)
f2_2018 <-tidy_results_other_races(list_races_2018)
f2_2018 <- f2_2018 %>% 
  mutate(game_id = rep(60:71,times=c(20,15,14,12,18,18,17,17,19,17,15,15)),
         season = rep(2018, nrow(.)))
saveRDS(f2_2018, "f2_2018_correct.rds")


# data for the 2017 season  
list_races_2017 <- data_scraping(2017,races_names_2017)
f2_2017 <-tidy_results_other_races(list_races_2017)
f2_2017 <- f2_2017 %>% 
  mutate(game_id = rep(72:82,times=c(18,18,17,17,17,18,17,18,20,19,18)),
         season = rep(2017, nrow(.)))
saveRDS(f2_2017, "f2_2017_correct.rds")


# aggregate all the seasons' results into one dataset 
f2_tot_correct <- rbind(f2_2023,f2_2022,f2_2021,f2_2020,f2_2019,f2_2018,f2_2017)
saveRDS(f2_tot_correct ,"f2_tot_correct.rds")

