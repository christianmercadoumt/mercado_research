#Read in field seasons 18 and 21
require(tidyverse)

load("C:/git/research/mercado_research/data/recent_remeasures/lolo_2018_remeasures_2022-01-03.Rdata")
lolo18 <- ingy_remeasures #This has disturbance cats 1-3

load("C:/git/research/mercado_research/data/recent_remeasures/lolo_koot_2021_remeasures_2022-01-03.Rdata")
lolokoot21 <- ingy_remeasures #This one has an additional disturbance cat 4

lolo18 <- lolo18 %>% 
  rename(SETTING_ID = setting_id, PLOT = plot_id, TAG_ID = TagID, 
         PURPOSE_CODE = purpose_code, LIVE_DEAD = Status, SPECIES_SYMBOL = Species, 
         DIAMETER = DBH, HEIGHT = Height, CROWN_RATIO = CrownRatio, 
         CROWN_CLASS = CrownClass, SNAG_DECAY_CLASS = DecayClass, AZIMUTH = Azimuth, 
         DISTANCE = Distance, REMARKS = Tree_Remark, DISTURB_CATEGORY1=DCategory.1,
         DISTURB_AGENT1= DAgent.1, DISTURB_CATEGORY2=DCategory.2, DISTURB_AGENT2=DAgent.2, 
         DISTURB_AGENT_SEV2=DSeverity.2, DISTURB_CATEGORY3=DCategory.3,DISTURB_AGENT3= DAgent.3, 
         DISTURB_AGENT_SEV3=DSeverity.3) %>%  #rename variable names to fit with rest of data
  mutate(SNAG_DECAY_CLASS = as.character(SNAG_DECAY_CLASS), 
         CROWN_RATIO = CROWN_RATIO*100,
         myear = 2018)


lolokoot21 <- lolokoot21 %>% 
  rename(SETTING_ID = setting_id, PLOT = plot_id, TAG_ID = TagID, 
         PURPOSE_CODE = purpose_code, LIVE_DEAD = Status, SPECIES_SYMBOL = Species,
         DIAMETER = DBH, HEIGHT = Height, CROWN_RATIO = CrownRatio,
         CROWN_CLASS = CrownClass, SNAG_DECAY_CLASS = DecayClass, AZIMUTH = Azimuth, 
         DISTANCE = Distance, REMARKS = Tree_Remark, DISTURB_CATEGORY1=DCategory.1,
         DISTURB_AGENT1= DAgent.1, DISTURB_CATEGORY2=DCategory.2, DISTURB_AGENT2=DAgent.2, 
         DISTURB_AGENT_SEV2=DSeverity.2, DISTURB_CATEGORY3=DCategory.3,DISTURB_AGENT3= DAgent.3, 
         DISTURB_AGENT_SEV3=DSeverity.3) %>% 
  mutate(SNAG_DECAY_CLASS = as.character(SNAG_DECAY_CLASS), 
         CROWN_RATIO = CROWN_RATIO*100,
         myear = 2021)

lolo.koot.18.21 <- bind_rows(lolo18, lolokoot21)

lolo.koot.18.21 <- mutate(lolo.koot.18.21, cluster = 100*floor(PLOT/100))

lolo.koot.18.21 <- lolo.koot.18.21 %>% 
  mutate(TPA_EQUIV = case_when(DIAMETER < dbh_breakpoint | is.na(DIAMETER) ~ 300*TreeCount,
                               DIAMETER >= dbh_breakpoint ~ 20))
