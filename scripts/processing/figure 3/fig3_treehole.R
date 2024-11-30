pacman::p_load(tidyverse,png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)
treehole <-read.csv("tidy_data/treehole_invertebrate_mortality.csv")

tree_url = "http://phylopic.org/assets/images/submissions/a98546d9-e030-4501-85e6-e9af67f63b02.256.png"
tree = readPNG(getURLContent(tree_url), 
               native = T)
tree2 <- rasterGrob(tree, interpolate = TRUE)

treehole_p <-treehole %>%
  ggplot()+
  #geom_point(aes(x=as.factor(year), y=mortality))+
  geom_jitter(aes(x = as.factor(year), y = mortality), size=1, alpha=0.5, width=0.05, 
              colour="indianred2")+
  geom_pointrange(mapping = aes(x = as.factor(year), y = mortality), colour="indianred2",
                  stat = "summary", size=1.2)+
  theme_bw()+
  theme(legend.position="none")+
  annotation_custom(tree2, xmin=0.3, xmax=0.9, ymin=0.75, ymax=1)+
  xlab("")


  geom_jitter(aes(x = as.factor(year), y = mortality), size=1, alpha=0.5, width=0.05, 
              colour="violred4")+
  geom_pointrange(mapping = aes(x = as.factor(year), y = mortality), colour="violetred4"
                  stat = "summary", size=1.2)
  
  ggsave(file="figures/maggie_proto/tree_hole.png", treehole_p, width = 5, height = 4, dpi = 300)
  
  
################################################################################
##### META-ANALYSIS DATA FORMATTING -----------------------
################################################################################
# Katie Goodwin Dec 6, 2022
library(tidyverse)
  
# control is 2022 (non heat dome year)
# treatment is 2021 (heat dome year)
  
# new variable - survival
treehole <- mutate(treehole, survival = 1 - mortality)

  # split into two years
t2021 <- subset(treehole, year == 2021)  
t2022 <- subset(treehole, year == 2022)
 
# 2021   
treehole21_meta <- t2021 %>%  
    summarize(response_id = 1,
              unique_species = 1,
              study_id = "treehole",
              comparison_type = "temporal_duringpost",
              response_def = "proportion mortality",
              response_units = "proportion",
              region = "terrestrial",
              movement = "motile",
              mean_treatment = mean(survival), 
              var_treatment = sd(mortality),
              samp_treatment = n())
  
# 2022   
treehole22_meta <- t2022 %>%  
  summarize(
            mean_control = mean(survival), 
            var_control = sd(mortality),
            samp_control = n())
  
# combine 2 years
  treehole_meta <- cbind(treehole21_meta, treehole22_meta)
  
  save(treehole_meta, file="./tidy_data/meta_analysis/treehole_meta.RData")
  
  