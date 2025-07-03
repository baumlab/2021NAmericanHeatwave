#loading relevant libraries
pacman::p_load(tidyverse, metafor, patchwork, MuMIn, viridis, emmeans, broom, rphylopic, clubSandwich)


#reading in the data from the CSV (this file is made in the 'meta_analysis_prep.R' script)
meta_species <- read_csv("./data/tidy_data/fig3/meta_analysis/heatdome_meta.csv")

# #calculating the lnRR 
# heatdome_ROM <-escalc(measure = "ROM", m1i=mean_treatment, m2i=mean_control, 
#                       sd1i=var_treatment, sd2i= var_control , n1i=samp_treatment, n2i=samp_control, 
#                       data=meta_species) %>%
#   mutate(comparison_type = factor(comparison_type), 
#          impact = case_when(heatdome_binary == c("post") ~ "after", ## Where is this heatdome binary coming from?
#                             heatdome_binary == c("during") ~ "during")) 

#calculating the lnRR - Sandra's code 
heatdome_ROM <-escalc(measure = "ROM", m1i=mean_treatment, m2i=mean_control, 
                      sd1i=var_treatment, sd2i= var_control , n1i=samp_treatment, n2i=samp_control, 
                      data=meta_species) %>%
  mutate(comparison_type = factor(comparison_type), 
         timing = case_when(comparison_type == c("temporal_during") ~ "during",
                            TRUE ~ "post")) %>%
  rename(taxonomy=larger_group)

#creating a matrix to account for similarity within studies and the reuse of some control trts
V <- vcalc(vi, cluster=study_id, subgroup=unique_species, grp1=mean_treatment, grp2=mean_control, data=heatdome_ROM)


#setting base levels based on the most abundant groups within each moderator
heatdome_ROM$realm <- relevel(factor(heatdome_ROM$realm), "terrestrial") 
heatdome_ROM$taxonomy <- relevel(factor(heatdome_ROM$taxonomy), "birds") # where is this column name coming from?
heatdome_ROM$timing <- relevel(factor(heatdome_ROM$timing), "post")
heatdome_ROM$movement <- relevel(factor(heatdome_ROM$movement), "motile")
heatdome_ROM$response_type <- relevel(factor(heatdome_ROM$response_type), "behaviour")

#intercept only model
mod0 <- rma.mv(yi, V, method="REML", random = ~1|study_id, data=heatdome_ROM)
sav_0<-robust(mod0, cluster=study_id)
sav_0

# fit the full model 
full <- rma.mv(yi, V, mods = ~taxonomy+ response_type + timing + movement,
               random = ~ 1 | study_id, data=heatdome_ROM, method="REML") #response type and taxonomy both have vifs above 3

vif(full) %>% as.data.frame() #taxonomy has the highest vif, removing

#full model minus txonomy
full_2 <- rma.mv(yi, V, mods = ~response_type + timing + movement,
               random = ~ 1 | study_id, data=heatdome_ROM, method="REML")

vif(full_2) %>% as.data.frame() #response type has the highest vif, removing 

#full model minus taxonomy and response type
full_regroup <- rma.mv(yi, V, mods = ~ timing + movement,
                       random = ~ 1 | study_id, data=heatdome_ROM, method="REML")
vif(full_regroup) %>% as.data.frame() #much better

#final model, using the contain method to control for complex random effects 
final_model <- rma.mv(yi, V, mods = ~ timing + movement,
                      random = ~ 1 | study_id, data=heatdome_ROM, method="REML", dfs="contain")

#applying robust inference methods, since random effects might not capture all dependencies
sav<-robust(final_model, cluster=study_id)
sav

#between intercept and timing during
anova(sav, btt=1:2) #p-val = 0.0081

#between intercept and movement sessile
anova(sav, btt=1:3) #p-val = 0.0109

# Figure 3a -------------------------------------------------------------------

est <- as.data.frame(coef(sav)) %>%
  rownames_to_column(var = "coefficient") %>%
  rename(est=`coef(sav)`) %>%
  add_row(coefficient = "timingpost", est=-0.4180004) %>%
  add_row(coefficient = "movementmotile", est=-0.4180004) %>%
  add_row(coefficient = "overall", est= -0.9081)

ci <- as.data.frame(coef(summary(sav))) %>%
  rownames_to_column(var = "coefficient")  %>%
  add_row(coefficient = "timingpost", ci.lb=-1.0008, ci.ub=0.1648) %>%
  add_row(coefficient = "movementmotile", ci.lb=-1.0008, ci.ub=0.1648) %>%
  add_row(coefficient = "overall", ci.lb = -1.4219, ci.ub=-0.3943)

final_model_df <- left_join(est, ci, by ="coefficient") %>%
  mutate(est_adj=case_when(coefficient %in% c("intrcpt", "timingpost", 
                                              "movementmotile", "overall") ~ est, 
                           coefficient %in% c("timingduring", "movementsessile") ~ est+-0.4180), 
         ci_lb_adj = case_when(coefficient %in% c("intrcpt", "timingpost", 
                                                  "movementmotile", "overall") ~ ci.lb, 
                               coefficient %in% c("timingduring", "movementsessile") ~ ci.lb+-0.4180), 
         ci_ub_adj = case_when(coefficient %in% c("intrcpt", "timingpost", 
                                      "movementmotile", "overall") ~ ci.ub, 
                   coefficient %in% c("timingduring", "movementsessile") ~ ci.ub+-0.4180)) %>%
  rename(response=coefficient) %>% mutate(colour=case_when(response == "overall" ~ "red", 
                   TRUE ~ "black")) %>%
  filter(response != "intrcpt") %>%
  mutate(response = recode(response, timingpost = 'post', overall = 'overall', timingduring = 'during', movementsessile = 'sessile', movementmotile = 'motile'))

overall_resp <- heatdome_ROM %>% count(yi) %>% select(-n) %>% mutate (study_id="overall", 
                                                                      variable = "overall", 
                                                                      variable_level = "overall")

heatdome_responses <- heatdome_ROM %>%
  count(study_id, timing, movement, yi) %>%
  pivot_longer(cols=c(timing, movement), names_to = "variable", values_to="variable_level") %>%
  select(-n) %>%
  rbind(overall_resp) %>%
  mutate(colour=case_when(variable_level == "overall" ~ "red", 
                          TRUE ~ "black"))





fig <-  heatdome_responses %>% 
  arrange(variable, variable_level, yi) %>% 
  ggplot(aes(x=variable_level, y = yi, colour=colour)) +
  geom_point(alpha=0.1, size=4)+
  geom_pointrange(data=final_model_df, aes(x=response, y=est_adj, ymin=ci_lb_adj, ymax=ci_ub_adj, colour=colour), linewidth=2,
                position=position_dodge(.9))+
  geom_point(data=final_model_df, aes(x=response, y=est_adj, fill=colour, colour=colour), shape=23, size=6)+
  scale_fill_manual(values=c("black", "#b30000"))+
  scale_colour_manual(values=c("black", "#b30000"))+
  scale_y_continuous(limits = c(-5, 3.5), breaks = seq(-5, 5, 1))+ 
  coord_flip() +
  scale_x_discrete(limits = c("motile", "sessile", "post", "during", "overall"),
                   labels = c("Motile (n=32)", "Sessile (n=25)",
                              "After (n=36)", "During (n=21)", "Overall (n=57)")) +
  geom_vline(xintercept = 2.5, linetype = "dotted", alpha = 0.5) + 
  geom_vline(xintercept = 4.5, linetype = "dotted", alpha = 0.5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Log response ratio", colour = "Variable") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_text(size = 25), 
        legend.text = element_text(size = 25), 
        text=element_text(size=25), axis.title=element_text(size=25, face="bold"))+ 
  guides(color = guide_legend(override.aes = list(size = 0.75)))+
  geom_text(data = NULL, x = 1.5, y = -4, label = "Movement", size=8, 
            colour="grey75", fontface="italic")+
  geom_text(data = NULL, x = 3.5, y = -4, label = "Heat Dome Timing", size=8, 
            colour="grey75", fontface="italic")








# Figure 3b ---------------------------------------------------------------

pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)


#install.packages("remotes")
install.packages("rphylopic")
library(rphylopic)
#code to get the uuid's for the phylopic pngs



#label logistics for the figure
heatdome.labs <- c("during", "after")
names(heatdome.labs) <- c("during", "post")
heatdome_ROM$timing <- factor(heatdome_ROM$timing, levels=c("during", "post"))

## (Sandra) Not sure where the common name column is coming from, so adding it in here
heatdome_ROM$common_name <- c("Surfgrass", "Surfgrass", "Dog whelk", "Acorn barnacle", "Bay mussel",
                             "Lupine", "Rose", "Saskatoon serviceberry", "Blueberry aphid", 
                             "Raspberry aphid", "Bat", "Bat", "Ferruginous hawk", "Ferruginous hawk", 
                             "Western honeybee", "Western honeybee", "Surf scoter", 
                             "Swainson's thrush", "Olive-sided flycatcher", "Western wood peewee", 
                             "Orange-crowned warbler", "Ruby-crowned kinglet", "Pacific Wren", 
                             "Swainson's thrush", "Olive-sided flycatcher", "Western wood peewee", 
                             "Orange-crowned warbler", "Ruby-crowned kinglet", "Pacific Wren", 
                             "Moose", "Wolf", "Snowshow hare", "Mule deer", "Caribou", "Red squirrel", 
                             "Moose", "Wolf", "Snowshow hare", "Mule deer", "Caribou", "Red squirrel",
                             "Thatched barnacle", "Acorn barnacle", "Little brown barnacle", "Thatched barnacle", 
                             "Green rope seaweed", "Green rope seaweed", "Rockweed", "Rockweed", "Sea lettuce", 
                             "Sea lettuce", "Red algae", "Red algae", "Kelp", "Kelp", "Surfgrass", "Surfgrass")



#this is the bar plot with the raw effect sizes


labs <- c("Sea lettuce", "Ruby-crowned kinglet","Bat","Pacific wren", 
          "Western honeybee", "Olive-sided flycatcher", "Surfgrass", 
          "Swainson's thrush", "Mule deer", "Moose", "Little brown barnacle", 
          "Western wood pewee", "Snowshoe hare", "Wolf", "Saskatoon serviceberry", 
          "Orange-crowned warbler", "Red squirrel", "Surfgrass", "Caribou", "Rockweed", 
          "Surf scoter", "Kelp", "Acorn barnacle","Prickly rose", "Dog whelk", "Red algae", "Green rope seaweed", "Ferruginous hawk", 
          "Acorn barnacle", "Raspberry aphid", "Arctic Lupine", "Thatched barnacle", 
          "Bay mussel", "Blueberry aphid")



ROM <- heatdome_ROM %>%
  group_by(study_id, common_name, response_id, timing) %>%
  dplyr::summarise(yi=mean(yi)) %>%
  unite(study_species, c("study_id", "common_name"), sep = "_", remove = F) %>%
  group_by(study_id, study_species) %>%
  ggplot(aes(x = reorder(study_species, -yi), y = yi, fill=timing)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge2(width = 1, preserve = "single"),
           width=0.9) +
  coord_flip() +
  scale_fill_manual(values=c("#B30000", "#FF0000"), limits=c("during", "post"),
                    labels=c("During", "After"), name= "Heat Dome Timing") +
  ylab("Log response ratio") +
  xlab("") +
  theme_classic() +
  theme(legend.position = c(0.745,0.845), text=element_text(size=25), axis.title=element_text(size=25, face="bold"),
        legend.title=element_text(size=25))+
  theme(legend.position = c(0.745,0.845), text=element_text(size=25), axis.title=element_text(size=25, face="bold"),
        legend.title=element_text(size=25),
        axis.text.y = element_text(face = c('bold', 'plain', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain',
                                            'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold',
                                            'plain', 'bold', 'plain', 'bold', 'bold', 'bold', 'plain', 'bold' ,'bold',
                                            'plain', 'bold', 'bold', 'bold', 'bold', 'bold', 'bold' )))+
  scale_y_continuous(limits = c(-5, 3.5), breaks = seq(-5, 5, 1))+
  #geom_blank() +
  add_phylopic(uuid = "cd57935f-334e-4067-8078-3c0dc1f44bf0", x=34, y=-4.8, ysize=1.1, alpha=1,fill = "black")+ #Aphididae
  add_phylopic(uuid = "96784234-e5fb-4de2-8ad8-6db8d0c9b75f", x=33, y=-4.5, ysize=1.2, alpha=1,fill = "black")+ #Mytilus galloprovincialis
  add_phylopic(uuid = "feaab8ab-b887-4875-801c-5c94c4b5da4b", x=32, y=-4.2, ysize=1.7, alpha=1,fill = "black")+ #thatched barnacle
  add_phylopic(uuid = "3d872b2d-624b-46dd-8eb8-8c5fbe615d8e", x=31, y=-2.35, ysize=1.2, alpha=1,fill = "black")+ #Spartium junceum
  add_phylopic(uuid = "cd57935f-334e-4067-8078-3c0dc1f44bf0", x=30, y=-2.1, ysize=1.1, alpha=1,fill = "black")+ #Aphididae
  add_phylopic(uuid = "feaab8ab-b887-4875-801c-5c94c4b5da4b", x=29, y=-2, ysize=1.7, alpha=1,fill = "black")+ #acorn barnacle
  add_phylopic(uuid = "fb93958a-474d-4623-819c-c8a7eb9dea9c", x=28, y=-1.9, ysize=1.35, alpha=1,fill = "black")+ #Buteo rufinus
  add_phylopic(uuid = "6bd74658-1f09-4b3a-8a2b-0bf74f20b2f0", x=27, y=-2.1, ysize=1.2, alpha=1,fill = "black")+ #Codium tomentosum
  add_phylopic(uuid = "0370679f-1fec-42af-9518-aa6c1de3bd90", x=26, y=-1.45, ysize=1.3, alpha=1,fill = "black")+ #Rhodymenia palmata
  add_phylopic(uuid = "f47b5859-b7f7-42a8-a6d0-4b82663b81f6", x=25, y=-1.3, ysize=1.2, alpha=1,fill = "black")+ #Ocenebra erinaceus
  add_phylopic(uuid = "29f553ac-bf42-41c5-941c-5b01b6a21edf", x=24, y=-1.3, ysize=1.7, alpha=1,fill = "black")+ #Fragaria vesca
  add_phylopic(uuid = "feaab8ab-b887-4875-801c-5c94c4b5da4b", x=23, y=-1.05, ysize=2, alpha=1,fill = "black")+ #acorn barnacle
  add_phylopic(uuid = "da150717-f583-4c09-91e1-ffeba0744242", x=22, y=-1.4, ysize=1.7, alpha=1,fill = "black")+ #kelp
  add_phylopic(uuid = "312ae57a-a14f-4934-adb9-3177f2b1757f", x=21, y=-1, ysize=1.2, alpha=1,fill = "black")+ #Melanitta deglandi
  add_phylopic(uuid = "2bb3f2a4-1567-4a8e-bf33-16abb7f0c7c1", x=20, y=-1, ysize=1, alpha=1,fill = "black")+ #fucus
  add_phylopic(uuid = "09c5d6bf-8ed9-4dc3-86f1-6b75dbe63952", x=19, y=-0.95, ysize=1.7, alpha=1,fill = "black")+ #caribou
  add_phylopic(uuid = "2534e10f-ea8e-436e-a8c8-13ac045eb867", x=18, y=-1, ysize=1.2, alpha=1,fill = "black")+ #surfgrass
  add_phylopic(uuid = "93ef011a-b372-4df5-99d5-66b8b92bd644", x=17, y=-0.8, ysize=1, alpha=1,fill = "black")+ #Tamiasciurus hudsonicus
  add_phylopic(uuid = "264f526f-4e32-4536-97c6-901cd5866755", x=16, y=-1, ysize=1, alpha=1,fill = "black")+ #Myioborus miniatus
  add_phylopic(uuid = "c127f813-1ae2-4948-8a8b-9f6324df8dac", x=15, y=-1, ysize=1.2, alpha=1,fill = "black")+ #Dombeya acutangula
  add_phylopic(uuid = "e4e306cd-73b6-4ca3-a08c-753a856f7f12", x=14, y=-1, ysize=1.3, alpha=1,fill = "black")+ #wolf
  add_phylopic(uuid = "8e61e166-11f4-4377-a923-9b5b597b6eba", x=13, y=-0.75, ysize=1.1, alpha=1,fill = "black")+ #snowshoe hare
  add_phylopic(uuid = "1a7e182f-b3a0-448b-8121-29d6bb70bc35", x=12, y=-0.6, ysize=1.7, alpha=1,fill = "black")+ #western wood pewee
  add_phylopic(uuid = "feaab8ab-b887-4875-801c-5c94c4b5da4b", x=11, y=-0.68, ysize=1.7, alpha=1,fill = "black")+ #barnacle
  add_phylopic(uuid = "1a20a65d-1342-4833-a9dd-1611b9fb383c", x=10, y=-0.75, ysize=1.5, alpha=1,fill = "black")+ #moose
  add_phylopic(uuid = "833aec60-8e2d-4275-bd0d-101bf1a6e8e4", x=9, y=-0.75, ysize=1.3, alpha=1,fill = "black")+ #mule deer
  add_phylopic(uuid = "3afc0b52-46fe-49da-aa60-b3bc046cb9e4", x=8, y=-0.3, ysize=1.1, alpha=1,fill = "black")+ #swainson's thrush
  add_phylopic(uuid = "2534e10f-ea8e-436e-a8c8-13ac045eb867", x=7, y=-0.2, ysize=1.4, alpha=1,fill = "black")+ #phyllospadix
  add_phylopic(uuid = "baed3744-96f9-41a4-aa7d-a304ddfbb688", x=6, y=-0.4, ysize=1.2, alpha=1,fill = "black")+ #olive sided flycatcher
  add_phylopic(uuid = "f551f54e-1e17-4b0e-985c-74d34a53cb63", x=5, y=-0.2, ysize=1.2, alpha=1,fill = "black")+ #honeybee
  add_phylopic(uuid = "e38c264a-8989-4e69-a6c1-7778b919980d", x=4, y=-0.3, ysize=1.2, alpha=1,fill = "black")+ #pacific wren
  add_phylopic(uuid = "18bfd2fc-f184-4c3a-b511-796aafcc70f6", x=3, y=-0.4, ysize=1, alpha=1,fill = "black")+ #bats
  add_phylopic(uuid = "7a3cb468-5bc2-4f9a-823a-f81aefd3d575", x=2, y=-0.25, ysize=1, alpha=1,fill = "black")+ #regulus
  add_phylopic(uuid = "efa5ccbb-23ba-46fd-a653-b35bb8fb6e0d", x=1, y=-0.3, ysize=1.2, alpha=1,fill = "black")+ #ulva
  scale_x_discrete(labels= labs)

a <- heatdome_ROM %>%
  group_by(response_type, timing) %>%
  summarise(yi=mean(yi)) %>% 
  ggplot(aes(x = reorder(response_type, -yi), y = yi, fill=timing)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge2(width = 1, preserve = "single"), 
           width=0.9) +
  coord_flip() +
  scale_fill_manual(values=c("#b30000", "#FF0000"), limits=c("during", "post"), 
                    labels=c("During", "After"), name= "Heat Dome Timing") +
  #scale_colour_manual(values=c("black", "black"), guide="none") +
  #scale_colour_manual(values=c("gold1", "slateblue", "burlywood4", "darkolivegreen4"))+
  ylab("Log response ratio") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "none", text=element_text(size=25), axis.title=element_text(size=25, face="bold"), 
        legend.title=element_text(size=25)) +
  scale_y_continuous(limits = c(-2.5, 1), breaks = seq(-5, 5, 1))+
  scale_x_discrete(labels= c("Growth", "Behaviour", "Damage", "Reproduction", "Persistence"))

b <- heatdome_ROM %>%
  group_by(timing, taxonomy) %>%
  summarise(yi=mean(yi)) %>% 
  ggplot(aes(x = reorder(taxonomy, -yi), y = yi, fill=timing)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col(position = position_dodge2(width = 1, preserve = "single"), 
           width=0.9) +
  coord_flip() +
  scale_fill_manual(values=c("#b30000", "#FF0000"), limits=c("during", "post"), 
                    labels=c("During", "After"), name= "Heat Dome Timing") +
  #scale_colour_manual(values=c("black", "black"), guide="none") +
  #scale_colour_manual(values=c("gold1", "slateblue", "burlywood4", "darkolivegreen4"))+
  ylab("Log response ratio") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "none", text=element_text(size=25), axis.title=element_text(size=25, face="bold"), 
        legend.title=element_text(size=25)) +
  scale_y_continuous(limits = c(-2.5, 1), breaks = seq(-5, 5, 1))+
  scale_x_discrete(labels= c("Birds", "Mammals", "Plants", "Algae", "Insects", "Marine invertebrates"))




fig3<- fig / ROM / (a + b ) + plot_layout(heights = c(2, 9, 2)) + 
  plot_annotation(tag_levels = 'a')


ggsave(filename = "./figures_tables/fig3_revised.png", plot =  fig3, dpi=900, width = 17, height = 19.5)

  


EDT2_df<-heatdome_ROM %>%
  count(common_name, movement, impact) %>%
  as.data.frame()

write_csv(EDT2_df, "./figures_tables/supplement/extendeddatatable2.csv")






# SI calculations 
#100 * (exp(lnRR) - 1) ===> the equation to back transform log response ratios



-1.68126648
# Number of species with negative responses -------------------------------

heatdome_ROM %>% 
  group_by(species) %>% 
  summarise(mean_yi = mean(yi), sd_yi = sd(yi)) %>% 
  filter(is.na(sd_yi)) %>% 
  nrow() # 7 species with only negative effect sizes


heatdome_ROM %>% 
  group_by(species) %>% 
  summarise(mean_yi = mean(yi), sd_yi = sd(yi), 
            min_yi = min(yi), max_yi = max(yi)) %>% 
  print(., n = 34)











