#
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("car") for levene's test
#install.packages("ez")
#install.packages("agricolae")
#install.packages("psych")


## Clear workspace
rm(list = ls())

## Load packages
library(tidyverse)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(car)
library(ez)
library(agricolae)
library(psych)

## Read from Excel
 # Read tobii data
  Mydata <- read_excel("E:/Spring 2018/cogcontrol/tobii.June26th.xlsx", na="-")
  
  #Read in master_subject_list 
  m.s.list <- read_excel("E:/Spring 2018/cogcontrol/mslist26th.xlsx", na="-") %>%
  separate(lang.group,into = c("language", "group")) %>%
  filter(keeper == 1)
    
## Clean Mydata
names(Mydata) <- gsub("Total Fixation Duration_Test", "Test", names(Mydata))
names(Mydata) <- gsub("Total Fixation Duration_Training", "Training", names(Mydata))
names(Mydata) <- gsub("Circle_Sum", "Circle", names(Mydata))
names(Mydata) <- gsub("Target_Sum", "Target", names(Mydata))
names(Mydata) <- gsub("Distractor_Sum", "Distractor", names(Mydata))
names(Mydata) <- gsub("Test", "Test_", names(Mydata))
names(Mydata) <- gsub("Training", "Training_", names(Mydata))
Mydata[is.na(Mydata)] <- 0 

## Convert data from wide to long, and then from long to wide
data_set <- Mydata %>%
  select(recording.name, contains("Circle"), contains("Distractor"), contains("Target")) %>%
  gather(AOI, looking_time, Test_1_Circle:Training_9_Target) %>%
  separate(AOI,into = c("trial_type", "trial_number", "AOI")) %>%
  spread(AOI, looking_time) %>%
##calculating the total looking time & good baby criteria
  mutate(All_looking_time = Circle + Distractor + Target) %>%
  mutate(good_trial = ifelse(All_looking_time >=0.50, "good", "bad")) %>%
  filter(good_trial == "good") %>% 
  group_by(recording.name, trial_type) %>% 
  mutate(num_good_trial = length(recording.name)) %>%
  filter(num_good_trial >=5) %>%
  group_by(recording.name) %>% 
  mutate(good_baby = length(unique(trial_type))) %>%
  filter(good_baby == 2) %>%
##calculating different types of anticipation
  mutate(correct_anticipation = ifelse(Target > Distractor, 1, 0)) %>%
  mutate(incorrect_anticipation = ifelse(Distractor > Target, 1, 0)) %>%
  mutate(no_anticipation = ifelse(Target + Distractor == 0, 1, 0)) %>%
  mutate(all_anticipation = 1-no_anticipation) %>%
  mutate(prop_look_correct = Target/(Target+Distractor)) 
  
## merge data_set with master.subject.list
data_merge <- merge(data_set, m.s.list, by="recording.name") %>% 
  ## Change data types to proper ones
  mutate(trial_number = as.numeric(trial_number)) %>%
  mutate(age.group = as.factor(age.group)) %>%
  mutate(language = as.factor(language)) %>%
  mutate(trial_type = as.factor(trial_type)) %>%
  mutate(total.vocab.prod = as.numeric(total.vocab.prod)) %>%
  mutate(total.concept.prod = as.numeric(total.concept.prod)) %>%
  mutate(recording.name = as.factor(recording.name)) %>%
  mutate(gender = as.factor(gender)) 
  

#Count the number of participants in each group
count <- data_merge %>%
group_by(age.group, language) %>%
summarize(num_participants = length(unique(recording.name)))

# For langauge table
count <- data_merge %>%
  group_by(recording.name, age.group, language, per.fr, per.eng, per.other1, other.lang1, dom.lang) %>%
  summarize(num_participants = length(unique(recording.name))) %>%
  filter (age.group == "20 months") %>%
  filter (language == "Bilinguals") 
  
## calculate proportion of correct anticipations 
  proportion_correct_anticipation <- data_merge %>%
  group_by(age.group, language, trial_type, trial_number) %>%
  filter(no_anticipation == 0) %>% 
  summarise(proportion = mean(correct_anticipation,na.rm=T))
  
##calculate proportion of all anticipations (correct + incorrect) 
  proportion_all_anticipation <- data_merge %>%
  group_by(age.group, language, trial_type, trial_number) %>%
  summarise(proportion = mean(all_anticipation,na.rm=T))
  
## calculating High & Low vocab size
data_vocab <- data_merge %>%
  # note exclude ***three babies*** with no CDI in second language
  filter(recording.name != "CogControl20_S16_42690" &
           recording.name != "CogControl20_S29_44681" &
           recording.name != "CogControl20_S80_47292") %>%
  group_by(language) %>%
  mutate(median.byGroup= median(total.concept.prod,na.rm=T)) %>%
  mutate(vocab.size = ifelse(median.byGroup>total.concept.prod,
                             "Low", 
                             "High")) %>%
  mutate(vocab.size = as.factor(vocab.size)) 
  
##Proportion of all/correct anticipations 

# proportion_correct_anticipation = proportion_correct_anticipation %>%
#   try(rename(Language= language)) %>%
#   mutate(trial.type.fig = ifelse(trial_type =="Training","Pre-switch","Post-switch"))

  fig.1 <-  proportion_all_anticipation %>% 
  ungroup() %>%
  group_by(age.group, language, trial_type, trial_number)
  #group_by(age.group, language, trial.type.fig, trial_number) 

fig.1$age.group <-  fct_rev(fig.1$age.group)
fig.1$trial_type <- fct_rev(fig.1$trial_type)

fig.1%>%
filter(age.group == "7 months") %>% 
  ggplot (aes(color = language,  
              shape = language, 
              x = trial_number, 
              y = proportion)) +
  geom_point(size= 4) +
  #scale_color_manual(values = c("#990033", # bilingual colour
                               # "#070707"))+ # mono colour
  scale_x_discrete(limits=c(1:9))+
  stat_smooth() +
  #stat_smooth(method = "lm", se = TRUE) +
  facet_grid(age.group ~ trial_type) +
  theme_bw((base_size=22)) +
  xlab("Trial number") +
  ylab("Proportion all anticipation")

ggsave("fig.1.pdf")
ggsave("fig.1.pdf",height = 12,width = 25,units = "cm") 


##Proportion correct anticipation with vocab size interaction for 20-mois 
fig.2 <- data_vocab %>%
  group_by(language, trial_type, trial_number, vocab.size) %>%
  filter(age.group =="20 months") %>%
  summarise(proportion_looking = mean(correct_anticipation, na.rm=T)) 

         
fig.2$trial_type <- fct_rev(fig.2$trial_type)

ggplot(data=fig.2, aes(x=trial_number, 
                       y=proportion_looking, 
                       group=interaction(language,vocab.size), 
                       linetype=vocab.size,
                       colour=vocab.size,
                       shape = language)) +
  #geom_line(size=1.2) + 
  geom_point(size=3) +
 #scale_shape_manual(values=c(22,21)) +
  scale_x_discrete(limits=c(1:9))+
  #stat_smooth() +
  stat_smooth(method = "lm", se = TRUE) +
  facet_grid(language ~ trial_type ) + 
  theme_bw((base_size=22)) +
  xlab("Trial number") +
  ylab("Proportion correct anticipation")

ggsave("fig.1.pdf")

ggsave("fig.1.pdf",height = 14,width = 25,units = "cm") 

## creating the blocks for ANOVA
data_blocks <- data_vocab %>%
  mutate(blocks_num = as.factor((as.numeric(trial_number)+2) %/% 3)) %>%
  group_by(recording.name, age.group, language, 
           trial_type, blocks_num, vocab.size) %>%
  summarise(block_anticipation = mean(correct_anticipation,na.rm=T)) %>%
  mutate(block_anticipation = as.numeric(block_anticipation)) 
  
# bayad 7-m ra hazf koni ta figure dorost shavad chon unha vocab.size nadarand
##Proportion correct looking in 3 blocks for 20-mois 
  fig.3 <- data_blocks %>%
  group_by(language, trial_type, blocks_num, vocab.size) %>%
    filter(age.group == "20 months") %>%
  mutate(proportion_anticipation = mean(block_anticipation, na.rm=T)) 
    
  fig.3$trial_type <- fct_rev(fig.3$trial_type)
  
  ggplot(data=fig.3, aes(x=blocks_num, 
                                   y=proportion_anticipation, 
                                   group=interaction(language,vocab.size), 
                                   linetype=vocab.size,
                                   shape=language)) +
                                   geom_line(size=1.2) + 
                                   geom_point(size=3, fill="red") +
                                   scale_shape_manual(values=c(22,21)) +
                                   scale_x_discrete(limits=c(1:3))+
                                   facet_grid(language ~ trial_type) + 
                                   xlab("Block number") +
                                   ylab("Proportion looking")
 
  
  
##  2 x 3 Mixed ANOVA for 7-months at Training
  # note **** 2 babies excluded due to lack of enough blocks****
  test7 <- data_blocks %>%
    filter(age.group == "7 months") %>%
    filter(trial_type == "Training") %>%
    select(-vocab.size)
  options(scipen = 999)
  test7 %>% 
    filter(recording.name != "CogControl7_S46_46742" &
    recording.name != "CogControl7_S89_48892") %>%
    ezANOVA(data = ., 
            dv = block_anticipation,
            wid = recording.name,
            within = blocks_num,
            between = language, 
            detailed = TRUE,
            type = 3)
  
  # independent-samples t-test for general performance
  datat <- proportion_correct_anticipation %>%
    filter (age.group== "7 months") %>%
    filter (trial_type=="Training")
  t.test(proportion~language, datat, var.equal = TRUE)
  
  
  ## 3 X 2 ANOVA for 7-m  at Test
  test7 <- data_blocks %>%
    filter(age.group == "7 months") %>%
    filter(trial_type == "Test") %>%
    select(-vocab.size)
  options(scipen = 999)
  test7 %>% 
    ezANOVA(data = ., 
            dv = block_anticipation,
            wid = recording.name,
            within = blocks_num,
            between = language, 
            detailed = TRUE,
            type = 3)
  
##post hoc test for main effect of block
  # bonferroni for within-s variable (blocks_num) with more than 2 levels
  
  pairwise.t.test(test7$block_anticipation,
                  test7$blocks_num, 
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

  #  one-way ANOVA for each language group
  #look at p for each langugae group for monolinguals 
  
  bi <- subset(test7, language == "Monolinguals") %>%
  ezANOVA(data = ., 
          dv = block_anticipation,
          wid = recording.name,
          within = blocks_num,
          detailed = TRUE,
          type = 3)
 #A paired samples t-test between blocks 
  pairwise.t.test(bi$block_anticipation,
                  bi$blocks_num, 
                  paired = TRUE)
  

  #calculating means of each lang.group 
## split blocks_num
  b1 <- subset(test7, blocks_num == "1")
  b2 <- subset(test7, blocks_num == "2")
  b3 <- subset(test7, blocks_num == "3")
# aov
  b1out <- aov(block_anticipation ~ language, data = b1)
  b2out <- aov(block_anticipation ~ language, data = b2)
  b3out <- aov(block_anticipation ~ language, data = b3)
  SNK.test(b1out, "language", group = FALSE, console = TRUE)
  SNK.test(b2out, "language", group = FALSE, console = TRUE)
  SNK.test(b3out, "language", group = FALSE, console = TRUE)

  # independent-samples t-test for genral performance of the lang. groups
  datat <- proportion_correct_anticipation %>%
    filter (age.group== "7 months") %>%
    filter (trial_type=="Test")
  t.test(proportion~language, datat, var.equal = TRUE)
  
 # the same procedure for all anticipation analysis
  # note at data_blocks need to put ****mean(all_anticipation)
  summarise(block_anticipation = mean(all_anticipation,na.rm=T)) %>%
  
  
  
## 3X2X2 mixed ANOVA for 20-month-olds
  test20 <- data_blocks %>%
    filter(age.group == "20 months") %>%
    filter(trial_type == "Training") %>%
    ezANOVA(data = ., 
                dv = block_anticipation,
                wid = recording.name,
                within = blocks_num,
                between = .(language, vocab.size), 
                detailed = TRUE,
                type = 3)

  # post hoc 
  pairwise.t.test(test20$block_anticipation,
                  test20$blocks_num, 
                  paired = TRUE,
                  p.adjust.method = "bonferroni")
  
  # Repeated measures ANOVA on subsets
  ## monolinguals
  mono <- subset(test20, language == "Monolinguals") %>%
  ezANOVA(data = ., 
          dv = block_anticipation,
          wid = recording.name,
          within = blocks_num,
          detailed = TRUE,
          type = 3)
  
    pairwise.t.test(mono$block_anticipation,
                    mono$blocks_num, 
                    paired = TRUE)
  
    ## for bilinguals
    bi <- subset(test20, language == "Bilinguals") %>%
      ezANOVA(data = ., 
              dv = block_anticipation,
              wid = recording.name,
              within = blocks_num,
              detailed = TRUE,
              type = 3)
    
    pairwise.t.test(bi$block_anticipation,
                    bi$blocks_num, 
                    paired = TRUE)
    
    # # means for each group 
    # b1 <- subset(test20, blocks_num == "1")
    # b2 <- subset(test20, blocks_num == "2")
    # b3 <- subset(test20, blocks_num == "3")
    # # aov
    # b1out <- aov(block_anticipation ~ language, data = b1)
    # b2out <- aov(block_anticipation ~ language, data = b2)
    # b3out <- aov(block_anticipation ~ language, data = b3)
    # SNK.test(b1out, "language", group = FALSE, console = TRUE)
    # SNK.test(b2out, "language", group = FALSE, console = TRUE)
    # SNK.test(b3out, "language", group = FALSE, console = TRUE)
    # 
    # 
    
  # mixed on anova for test phase 
    ## ****note excluding 5 babies with not enough blocks****
    
    test20 <- data_blocks %>%
      filter(age.group == "20 months") %>%
      filter(trial_type == "Test") %>%
      filter(recording.name != "CogControl20_S23_43543" &
      recording.name != "CogControl20_S35_44215" &
      recording.name != "CogControl20_S77_48561" &
      recording.name != "CogControl20_S88_47362" &
      recording.name != "CogControl20_S91_47381") %>%
      ezANOVA(data = ., 
              dv = block_anticipation,
              wid = recording.name,
              within = blocks_num,
              between = .(language, vocab.size), 
              detailed = TRUE,
              type = 3)
    
    # mixed ANOVA on subset: Language monolinguals
    # between-s varivale: vocab.size
    
    test20 <- data_blocks %>%
      filter(age.group == "20 months") %>%
      filter(trial_type == "Test") %>%
      filter(recording.name != "CogControl20_S23_43543" &
               recording.name != "CogControl20_S35_44215" &
               recording.name != "CogControl20_S77_48561" &
               recording.name != "CogControl20_S88_47362" &
               recording.name != "CogControl20_S91_47381")
    
      bi1 <- subset(test20, language == "Bilinguals") %>%
      ezANOVA(data = ., 
              dv = block_anticipation,
              wid = recording.name,
              within = blocks_num,
              between = vocab.size,
              detailed = TRUE,
              type = 3) 
    
# independent t-test on main effect of vocab.size
      t.test(block_anticipation~vocab.size, bi1, var.equal = TRUE)
    
  ## same for MONOlingual subset
      test20 <- data_blocks %>%
        filter(age.group == "20 months") %>%
        filter(trial_type == "Test") %>%
        filter(recording.name != "CogControl20_S23_43543" &
                 recording.name != "CogControl20_S35_44215" &
                 recording.name != "CogControl20_S77_48561" &
                 recording.name != "CogControl20_S88_47362" &
                 recording.name != "CogControl20_S91_47381")
      
      bi2 <- subset(test20, language == "Monolinguals") %>%
        ezANOVA(data = ., 
                dv = block_anticipation,
                wid = recording.name,
                within = blocks_num,
                between = vocab.size,
                detailed = TRUE,
                type = 3) 
    
    
    
    
  
  
  
  
  