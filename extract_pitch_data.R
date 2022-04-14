###########################################################################
# This script extracts data from the pitch-polyrhythm study               #
#                                                                         #                                                                       #
#                                                                         #
# Date: March 2022                                                        #
# Author: Cecilie Møller                                                  #
# Project group: above + Jan Stupacher, Alexandre Celma-Miralles,         #
# Peter Vuust                                                             #
###########################################################################

# install.packages("jsonlite")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("ggplot2")

# INITIALIZE
library(jsonlite)
library(plyr)
library(dplyr)
library(ggplot2)

# SET WORKING DIRECTORY 

setwd("C:/Users/au213911/Documents/musbeat")

# LIST FILES IN WORKING DIRECTORY (ignoring folders and recursives)
files <- setdiff(list.files(paste0(getwd(),"/results/results"),include.dirs=F,all.files=F),list.dirs(paste0(getwd(),"/results/results"),full.names=F))
files <- files[grep(".rds$",files)]
names(files) <- basename(files)
addUniqueID <- ldply(files)

# CREATE OUTPUT FILE
output <- data.frame(id=character(),
                     stringsAsFactors=F)


for (i in 1:length(files)) {
  results <- readRDS(paste0(getwd(),"/results/results/",files[i]))
  
  output[i,"id"] <- results$session$p_id
  output[i,"complete"] <- results$session$complete
  output[i,"currentTime"] <- results$session$current_time
  output[i,"startTime"] <- results$session$time_started
  output[i,"device"] <- results$results$device
  output[i,"browser"] <- results$results$browser
  output[i,"headphones"] <- results$results$headphones

  if ("age"%in%names(results$results)) {

  output[i,"age"] <- results$results$age
  output[i,"gender"] <- results$results$gender
  }

  if ("language"%in%names(results$results)) {

     output[i,"residence"] <- results$results$residence
     output[i,"youth_country"] <- results$results$youth_country
     output[i,"language"] <- results$results$language
  }

  if ("ollen"%in%names(results$results)) {
    
     output[i,"ollen"] <- results$results$ollen
  }
  
  if ("years_instr"%in%names(results$results)) {

    output[i,"MT_06"] <- results$results$MT_06
    output[i,"instrument"] <- results$results$instrument
    output[i,"years_instr"] <- results$results$years_instr
  }


  if ("duplets"%in%names(results$results)) {
    output[i,"duplets"] <- results$results$duplets
  }

  if ("comments"%in%names(results$results)) {
    output[i,"comments"] <- results$results$comments
  }


  if ("poly_ratio"%in%names(results$results)) {

    # make js data into dataframe
    jsdata<- fromJSON(results$results$poly_ratio)


    #look in first row of the dataframe to see which condition (pitch/tempo/ratio) this participant was in and paste into output
    output[i,"experiment"]<-jsdata$stimulus[1]


    # extract relevant rows, i.e. only those containing tapping data
     tapping_all <- subset(jsdata, trial_type== "audio-bpm-button-response")


     # save spontaneous taps (which are in the first column) in the output file
     output[i,"spontaneous_taps"]<-tapping_all$rt[1]

     #remove spontaneous taps (duplicate column name)
     tapping <- subset(tapping_all, stimulus!= "sounds/spontaneous_tap_15s.mp3")

     # and restructure
      reshaped <- t(tapping)

      # Extract stimulus names
      conds<-reshaped['stimulus',]

      # extract taps
      taps<-reshaped['rt',]
      # and paste the taps into the column in output which is named according to conds (stimulus name)
      output[i,conds]<-taps

      
# # extract stimulus presentation order and ratings
# 
#     tr_ind<-reshaped['trial_index',]
#     stim<-reshaped['stimulus',]
#     output[i,tr_ind]<-stim
# 
#     rating<-subset(jsdata, trial_type=="html-slider-response")
#     t_rating <- t(rating)
# 
#     tr_ind<-t_rating['trial_index',]
#     verdict<-t_rating['response',]
#     output[i,tr_ind]<-verdict

  }
 }

output <- bind_cols(addUniqueID, output)
output$id <-output$.id
output<- output%>% select(-.id,-V1 )


# rename values in "experiment"

output$experiment <- gsub("sounds/pitch/poly_pitch_marimba_loudness.mp3", "pitch", output$experiment)


# WHAT HAVE WE GOT OVERALL?

# how many finished the tapping part?
n_tapping<-sum(!is.na(output$experiment))

#how many had tried the experiment at least once before?
n_duplets<-sum(!is.na(output$duplets[!output$duplets == "No"]))

#how many complete responses do we have (tapping + demographics)?
n_complete <-nrow(output[output$complete==TRUE,])


# EXCLUDE DATA from participants who stated that they had tried the experiment before (n=5)
# or who did not answer the duplet question and hence produced incomplete data (+n=5).

# Visualize cases to be excluded

ggplot(output[!is.na(output$experiment),], aes(x = duplets)) +
  geom_bar()+
  labs(title = '"TAPPING ONLY" DATASETS: Been there, done that?')+
  theme(axis.text.x = element_text(angle = 270))+
  facet_wrap(~experiment)

ggplot(output[output$complete==TRUE,], aes(x = duplets)) +
  geom_bar()+
  labs(title = 'COMPLETE DATASETS: Been there, done that?')+
  theme(axis.text.x = element_text(angle = 270))+
  facet_wrap(~experiment)


# # exclude (comment out this section for pilotdata extraction)
# 
# output <- subset(output, duplets== "No")
# output <- subset(output, complete== TRUE) # redundant in this case as incomplete responses were not included in the line above either


#  Do we have double data under different ids?
# diagnose
doubledata<- output[duplicated(output$spontaneous_taps),]

# manually remove the participant whose data was saved twice under different ids
# #remove
# output <- subset(output, id!="id=440&p_id=d2fc24d5b59e5796ff9e46034aa7b598119ea62c8f1d31af04c2252c409f7f5c&save_id=6&pilot=false&complete=true.rds")

# 
# # assess a subsample of possibly safari-related pointy ITI-plots
# 
# output_r$browser[output_r$part[c(2,10,17,20,25,37,43,48,52,59,63,64,67,82,89,105,108,109,114,116)]]

# recode "ollen" into  "musicianship" variable

output$musicianship[output$ollen=="Nonmusician"]  <- "Nonmusician"
output$musicianship[output$ollen=="Music-loving nonmusician"]  <- "Nonmusician"
output$musicianship[output$ollen=="Amateur musician"]  <- "Amateur musician"
output$musicianship[output$ollen=="Serious amateur musician"]  <- "Amateur musician"
output$musicianship[output$ollen=="Semiprofessional musician"]  <- "Semi/professional musician"
output$musicianship[output$ollen=="Professional musician"]  <- "Semi/professional musician"


# MAKE FILES

# pitch
output_musbeat<- subset(output, experiment== "pitch")
emptycols <- colSums(is.na(output_musbeat)) == nrow(output_musbeat)
output_musbeat <- output_musbeat[!emptycols]


# # OUTCOMMENT THIS LINE FOR PILOT DATA
# # exclude participants who used their device’s internal speakers in the pitch experiment
# output_musbeat <- subset(output_musbeat, headphones!= "my device's internal speakers")


#add part no separately for each participant

output_musbeat$part<-1:nrow(output_musbeat)

output_musbeat <- subset(output_musbeat, select=c(part,1:ncol(output_musbeat)-1))

##################################
# WRITE CSV-FILES
# 
# # main data files 
# write.csv(output_musbeat, file = "./analyses/musbeat_pilot.csv",row.names = FALSE)

########## Ns and PROPs #########

n_int_speak_p <- nrow(output_musbeat[output_musbeat$headphones== "my device's internal speakers",])
n_nonmusician<-sum(!is.na(output$ollen[output$ollen == "Nonmusician" | output$ollen == "Music-loving nonmusician"]))
n_musicians<-sum(!is.na(output$ollen[!output$ollen == "Music-loving nonmusician"& !output$ollen == "Nonmusician"]))

n_musicians_p <- sum(!is.na(output_musbeat$ollen[!output_musbeat$ollen == "Music-loving nonmusician"& !output_musbeat$ollen == "Nonmusician"]))
pct_musicians_p <- n_musicians_p/nrow(output_musbeat)*100

##########################

# Extract comments
show_comments<-output %>% select(id, currentTime, comments, experiment, headphones, browser, device)
show_comments<- show_comments[!is.na(show_comments$comments),]
show_comments<- show_comments[!show_comments$comments == "",]

# write.csv(show_comments, file = "./comments.csv")

################### A FEW QUICK PLOTS ######################

my_theme = theme(
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16))

my_vert_theme = my_theme +
  theme(axis.text.x = element_text(size = 10, angle = 270))

my_facet_theme = my_theme +
  theme(strip.text.x = element_text(size = 16))

my_facet_vert_theme = my_facet_theme+
  theme(axis.text.x = element_text(size = 14, angle = 270))

# plot recruitment success by date and n
hist(output_musbeat$currentTime, breaks = "days", main = "Recruitment success by date:all complete datasets", xlab = "Date of data collection", freq = TRUE)

ggplot(output_musbeat, aes(x = currentTime, fill = residence)) +
  geom_histogram()+
  labs(title = 'Recruitment success by date and residence (good data only: excl. incompl.,duplicates and pitch=int.speak.)')+
  theme(axis.text.x = element_text(angle = 270))

#plot n

ggplot(output_musbeat, aes(x = experiment)) +
  geom_bar()+
  labs(title = 'COMPLETE GOOD DATASETS PER EXP.')+
  my_theme

ggplot(output_musbeat, aes(x = ollen)) +
  geom_bar()+
  labs(title = "Participants' self-reported musicianship")+
  my_facet_vert_theme+
  facet_wrap(~experiment)


#####################
#PLOT TECHS

#plot headphones

ggplot(output_musbeat, aes(x = headphones,fill=device)) +
  geom_bar()+
  labs(title = 'HEADPHONES by device')+
  my_vert_theme+
  facet_wrap(~experiment)

ggplot(output_musbeat, aes(x = browser,fill=device)) +
  geom_bar()+
  labs(title = 'BROWSER by device')+
  my_facet_vert_theme+
  facet_wrap(~experiment)

ggplot(output_musbeat, aes(x = headphones, fill= device)) +
  geom_bar()+
  labs(title = 'HEADPHONES by device')+
  my_facet_vert_theme


#####################
#PLOT MUSICIANSHIP

ggplot(output_musbeat[!is.na(output_musbeat$ollen),], aes(x = ollen, fill= gender)) +
  geom_bar()+
  labs(title = 'MUSICIANSHIP')+
  my_facet_vert_theme+
  facet_wrap(~experiment)

ggplot(output_musbeat[!is.na(output_musbeat$instrument),], aes(x = instrument, fill=ollen)) +
  geom_bar()+
  labs(title = 'THE INSTRUMENT I PLAY BEST IS...')+
  my_vert_theme



ggplot(output_musbeat[!is.na(output_musbeat$musicianship),], aes(x = musicianship, fill= gender)) +
  geom_bar()+
  labs(title = 'MUSICIANSHIP')+
  my_facet_vert_theme+
  facet_wrap(~experiment)

# Fix as.numeric
output_musbeat$years_instr[output_musbeat$years_instr=="I don't play any instrument"]  <- 99
output_musbeat$years_instr<-as.numeric(output_musbeat$years_instr)

ggplot(output_musbeat[!is.na(output_musbeat$years_instr),], aes(x = years_instr, fill=ollen)) +
  geom_bar()+
  labs(title = "YEARS_INSTR BY MUSICIANSHIP (99 = I don't play any instrument)")+
  scale_x_continuous(breaks=seq(0, 100, 10))+
  scale_y_continuous(breaks=seq(0, 25, 10))+
  theme(axis.text.x = element_text(angle = 270))

ggplot(output_musbeat[!is.na(output_musbeat$years_instr),], aes(x = MT_06, fill=ollen)) +
  geom_bar()+
  labs(title = 'NO. OF MUSICAL INSTRUMENTS PLAYED')+
  my_theme


#####################
#PLOT DEMOGRAPHICS 

# age and gender
ggplot(output_musbeat[!is.na(output_musbeat$age),], aes(x = age, fill=gender)) +
  geom_bar()+
  labs(title = 'AGE by gender')+
  my_facet_vert_theme
 # facet_wrap(~experiment)

ggplot(output_musbeat[!is.na(output_musbeat$gender),], aes(x = gender)) +
  geom_bar()+
  labs(title = 'GENDER by headphones - good data only')+
  my_facet_vert_theme+
  facet_wrap(~experiment)

# youth country residence and language


ggplot(output_musbeat[!is.na(output_musbeat$language),], aes(x = language, fill=residence)) +
  geom_bar()+
  labs(title = 'LANGUAGE BY CURRENT RESIDENCE')+
  theme(axis.text.x = element_text(angle = 270))

ggplot(output_musbeat[!is.na(output_musbeat$language),], aes(x = language, fill=youth_country)) +
  geom_bar()+
  labs(title = 'LANGUAGE BY YOUTH COUNTRY')+
  theme(axis.text.x = element_text(angle = 270))

ggplot(output_musbeat[!is.na(output_musbeat$residence),], aes(x = residence, fill=youth_country)) +
  geom_bar()+
  labs(title = 'RESIDENCE BY YOUTH COUNTRY')+
  my_vert_theme

ggplot(output_musbeat[!is.na(output_musbeat$youth_country),], aes(x = youth_country, fill=residence)) +
  geom_bar()+
  labs(title = 'YOUTH COUNTRY BY RESIDENCE')+
  my_vert_theme
# ###############################################################################################