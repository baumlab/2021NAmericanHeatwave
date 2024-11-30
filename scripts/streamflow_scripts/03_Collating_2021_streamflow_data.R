# Script to get and plot all 2021 streamflow data manually downloaded
#acknowledgment to Pierre Rogy for starter code

# Load packages
library(dplyr) ## To manipulate data, part of tidyverse
library(tidyr) ## To manipulate data, part of tidyverse
library(stringr) ## Manipulate strings to make the loop go through even if files change
library(lubridate) ## Deal with dates

# Input here the folder path to input data

input_folder <- 
  here::here("streamflow_treeholes")


# Compile  data ----------------------------------------------------
# Make list of all files within the directory with streamflow data
file_list <- 
  dir(path = "streamflow_treeholes/waterflow_2021")

# Make empty list where all plots will be saved
plot_list <- 
  list()

# Make empty df to store data
all_dats <- 
  data.frame()

# For loop that will extract the data from each ibutton and plot it
# For each file
for(i in file_list){ 
  ## Extract station number
  station <- 
    stringr::str_split(i, "_")[[1]][1]
  
  ## Read file into R
  dats <- 
    read.csv(paste0("/Users/sandraemry/Documents/HD_Baum/heatdome/streamflow_treeholes/waterflow_2021/",
                    i), skip = 10)## Remove rows with not useful information
 
   zone<- colnames(dats)[1] %>% 
    str_split(pattern = "\\..")
  
  dats_collated <- dats %>% 
    mutate(station = as.character(station),
           timezone = str_split(string = zone[[1]][2], pattern = "\\.")[[1]][1]) %>% 
    rename(Valuems = "Value..m..s.",
           datetime = 1) %>% 
    mutate(datetime = lubridate::as_datetime(datetime)) %>% 
    mutate(date = floor_date(datetime, unit = "day")) %>%
    group_by(date) %>%
    summarize(mean_flow = mean(Valuems, na.rm=TRUE),
              station = first(station),
              timezone = first(timezone),
              Parameter = first(Parameter),
              Approval.Name = first(Approval.Name),
              Grade.Name = first(Grade.Name))
  
  # Add data to all dats
  all_dats <- 
    rbind(all_dats,
          dats_collated)
  
  # On to ggplot!
  # Add plot to last element of list plus one
  plot_list[[length(plot_list) + 1]] <- 
    ggplot(data = dats_collated, 
           aes(x= date, 
               y = mean_flow)) +
    geom_line(colour = "dodgerblue") +
    ylab("Flow (m3 per s)") +
    xlab("Time") +
    ggtitle(paste0('Station ', station)) +
    theme_bw() 
  
  
} ## end for loop

# Print plot in list
## First open a PDF object
pdf("figures/stream2021_plots.pdf", 20, 15)
## In it get plots 3 by 3
for (i in seq(1, length(plot_list), 3)) {
  ## Save three plots in a temporary list
  templist <- 
    list(plot_list[[i]],
         plot_list[[i + 1]],
         plot_list[[i + 2]])
  ## Print them in a grid composed of one column and three rows
  print(cowplot::plot_grid(plotlist = templist,
                           ncol = 1,
                           nrow = 3))
}
dev.off()

#these stations have less than 90 days data (should be 93)
#1 08EF005    81
#2 08MH005    89
#3 08MH024    45
#4 09CA004    69

all_dats <- all_dats %>% 
  rename(daily_flow = mean_flow) #just to be more precise and avoid confusion later on

# Save all data
write.csv(all_dats,
          "streamflow_treeholes/processed_data/waterflow2021_dailymean.csv",
          row.names = F) ## default saves row numbers as row names, and we dont want that!

  