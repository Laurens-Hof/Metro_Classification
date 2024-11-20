rm(list = ls())

library(tidyverse)
library(readxl)

sigma = 0.1

setwd("C:/Users/Laurens/Documents/Semiproductivity/R practice/Organic_metro")

names <- read_excel("Gemeenten_alfabetisch.xlsx")
commutes <- read_delim("woon_werkverkeer.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

commutes <- commutes %>%
  #match codes with full municipality name for origin
  left_join(names, by = c("WoonregioS" = "GemeentecodeGM")) %>%
  select(ID, WoonregioS, WerkregioS, Gemeentenaam, BanenVanWerknemers_1) %>%
  rename(woongemeente = Gemeentenaam) %>%
  #match codes with full municipality name for origin
  left_join(names, by = c("WerkregioS" = "GemeentecodeGM")) %>%
  select(ID, WoonregioS, WerkregioS, woongemeente, Gemeentenaam, BanenVanWerknemers_1) %>%
  rename(werkgemeente = Gemeentenaam) %>%
  #change the jobs variables to full units
  mutate(banen = BanenVanWerknemers_1 * 1000, .keep = "unused") %>%
  #get rid of reorganization artifacts
  drop_na()

#create totals
commutes_fullname <- commutes %>%
  select(woongemeente, werkgemeente, banen) %>%
  spread(werkgemeente, banen) %>%
  mutate(zztotal = rowSums(.[2:356])) %>%
  pivot_longer(cols = 2:357, names_to = "werkgemeente", values_to = "banen")

#initialize a table that will store to what grouping each municipality belongs
groups <- tibble(unique(commutes_fullname$woongemeente)) %>%
  rename(gemeente = 1) %>%
  mutate(grouping = gemeente)

#create a function so we can use recursion
duranton <- function(data){
  
  # Create table of normalized flows. Step 1: deselect the diagonal
  fractions <- data[data$woongemeente != data$werkgemeente,] %>%
    #move to matrix form for vectorized operations
    spread(werkgemeente, banen) %>%
    #divide everything by total resident workers
    mutate(across(!woongemeente, ~ ./zztotal), .keep = "unused") %>%
    #move back to tidy form
    pivot_longer(cols = !woongemeente, names_to = "werkgemeente", values_to = "fraction") %>%
    #drop diagonals that were created by the pivot
    drop_na()
  
  #only continue if the max flow is greater than sigma
  if(max(fractions$fraction) > sigma){
    #get the origin and destination of the heaviest flow
    maxorigin <- fractions$woongemeente[fractions$fraction == max(fractions$fraction)]
    maxdest <- fractions$werkgemeente[fractions$fraction == max(fractions$fraction)]
    
    #note that all regions belonging to the origin now belong to the destination
    groups$grouping[groups$grouping == maxorigin] <- maxdest
    
    data <- data %>%
      #spread the data by residence municipalities
      spread(woongemeente, banen)
    #join the origin and destination
    data[,maxdest] <- data[,maxdest] + data[,maxorigin]
    #pivot back to tidy
    joined <- data %>%
      pivot_longer(!werkgemeente, names_to = "woongemeente", values_to = "banen") %>%
      #remove the origin
      filter(werkgemeente != maxorigin & woongemeente != maxorigin)
    print(spread(joined, werkgemeente, banen))
    duranton(joined)
    
  } else{
    return(groups)
  }
}

groups <- duranton(commutes_fullname)

#deselect the diagonal
fractions <- commutes_fullname[commutes_fullname$woongemeente != commutes_fullname$werkgemeente,] %>%
  #move to matrix form for vectorized operations
  spread(werkgemeente, banen) %>%
  #divide everything by total resident workers
  mutate(across(2:356, ~ ./zztotal), .keep = "unused") %>%
  #move back to tidy form
  pivot_longer(cols = 2:356, names_to = "werkgemeente", values_to = "fraction") %>%
  #drop diagonals that were created by the pivot
  drop_na()



