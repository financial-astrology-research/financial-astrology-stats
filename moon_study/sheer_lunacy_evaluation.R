# Title     : Sheer Lunacy Evaluation
# Objective : Replication of the RBS study on correlation of moon phases and stock markets: https://www.markettiming.nl/img/image/file/RBS_MoonTrading13Jun10.pdf
# Created by: thiloyes
# Created on: 29/01/2021

library(readr)
library(ggplot2)
library(swephR)
library(dplyr)
library(purrr)
library(tidyr)
source('../util/astro_utils.R')

symbol <- "^FTSE"
start_date <-  "1980-01-08"
end_date <- "2010-01-28"
path <- '../data/tmp/'

cut_off_time <- 24
price_col <- rlang::sym('Open') # define the price to be used, can be Open or Close

is_new_moon_partial <- function(calc_date, prev_date) {
  is_new_moon(calc_date, prev_date, cut_off_time)
}

is_full_moon_partial <- function(calc_date, prev_date) {
  is_full_moon(calc_date, prev_date, cut_off_time)
}

plot_index <- function(price_data, show_moons = F) {
  plot_data <- price_data %>% 
    mutate(new_moon=ifelse(new_moon,!!price_col, NA),
           full_moon=ifelse(full_moon, !!price_col, NA))
  p <- ggplot(plot_data, aes(x=Date, y=!!price_col)) +
    geom_line()
  if(show_moons) {
    p <- p +
      geom_point(data=plot_data, aes(x=Date, y=new_moon), size=2, colour='black') +
      geom_point(data=plot_data, aes(x=Date, y=full_moon), size=2, colour='grey')
  }
  
  print(p)
}

plot_performance <- function(strategy, plot_title, show_moons=F) {
  plot_data <- strategy %>% 
    mutate(new_moon=lead(ifelse(new_moon,lag(strategy_performance), NA)), # need to shift the moon dots, to make it look better (otherwise performance for the day of the moon is still shown)
           full_moon=lead(ifelse(full_moon, lag(strategy_performance), NA)))
  plot_data_curves <- plot_data %>% select(Date, base_performance, strategy_performance) %>% 
    gather(strategy, returns, -Date)
  p <- ggplot(plot_data_curves, aes(x=Date, y=returns, color = strategy)) +
    geom_line() +
    ggtitle(plot_title)
  

  if(show_moons) {
    p <- p +
      geom_point(data=plot_data, aes(x=Date, y=new_moon), size=2, colour='black') +
      geom_point(data=plot_data, aes(x=Date, y=full_moon), size=2, colour='grey')
  }
 
  print(p)
}

filename <- paste0(path, symbol, '.csv')
price_data <- read_delim(filename, ',')
price_data <- price_data %>% 
  drop_na(`Adj Close`) %>% 
  mutate(prev_date = lag(Date)) %>% 
  filter(Date > start_date) %>% 
  filter(Date < end_date) %>% 
  mutate(new_moon = map2_lgl(Date, prev_date, is_new_moon_partial),
         full_moon = map2_lgl(Date, prev_date, is_full_moon_partial))

strategy <- price_data %>% 
  mutate(invested=ifelse(new_moon, 1, ifelse(full_moon, 0, NA))) %>% 
  fill(invested) %>% 
  mutate('ratio_base':=lead(!!price_col)/!!price_col) %>% 
  mutate(ratio_strategy=ifelse(invested, ratio_base, 1)) %>% 
  mutate(ratio_strategy=ifelse(is.na(ratio_strategy), 1, ratio_strategy)) %>%
  mutate(ratio_strategy2=ifelse(1-invested, ratio_base, 1)) %>% # strategy2 is the opposite strategy
  mutate(ratio_strategy2=ifelse(is.na(ratio_strategy2), 1, ratio_strategy2)) %>% 
  filter(!is.na(invested)) %>% # start with first new moon
  mutate(base_performance=cumprod(ratio_base),
         strategy_performance=cumprod(ratio_strategy),
         strategy2_performance=cumprod(ratio_strategy2)) 

performance <- strategy %>% 
  summarize(base=prod(ratio_base, na.rm=T),
            moon_strategy=prod(ratio_strategy, na.rm=T),
            moon_strategy2=prod(ratio_strategy2, na.rm=T))

print(performance)

plot_performance(strategy, symbol, show_moons = T)
