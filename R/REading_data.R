oredered= FALSE


df <- read_csv("../csv_storage/fullRatingsFile.csv")

a <- df$perceived_age
df$perceived_age_3cat <- a
df$perceived_age_3cat[a<34] <- "young"
df$perceived_age_3cat[a>=34] <- "middle"
df$perceived_age_3cat[a>=60] <- "old"
if(oredered== TRUE){
  df$perceived_age_3cat <- factor(df$perceived_age_3cat, ordered = TRUE,levels=c("young","middle", "old"))
}else{
  df$perceived_age_3cat <- factor(df$perceived_age_3cat,levels=c("young","middle", "old"))
}

table(df$perceived_age_3cat)

a <- df$participantAge
df$participantAge_3cat <- a
df$participantAge_3cat [a<34] <- "young"
df$participantAge_3cat [a>=34] <- "middle"
df$participantAge_3cat [a>=60] <- "old"

if(oredered== TRUE){
  df$participantAge_3cat  <- factor(df$participantAge_3cat ,ordered = TRUE, levels=c("young","middle", "old"))

}else{
  df$participantAge_3cat  <- factor(df$participantAge_3cat , levels=c("young","middle", "old"))

}

table(df$participantAge_3cat)

# rates code


oredered= TRUE

# df$prolificParticipantID
df$dataset        <- as.factor(df$dataset)
df$beautification <- as.factor(df$beautification)
df$imageGender    <- as.factor(df$imageGender)
df$perceived_gender <- as.factor(df$perceived_gender)
df$condition      <- as.factor(df$condition)
df$participantSex <- as.factor(df$participantSex)
df$intelligence_7L <- factor(df$intelligence,ordered = TRUE)
df$happy_7L <- factor(df$happy,ordered = TRUE)
df$sociability_7L <- factor(df$sociability,ordered = TRUE)
df$unusual_7L <- factor(df$unusual,ordered = TRUE)
df$trustworthy_7L <- factor(df$trustworthy,ordered = TRUE)
df$feminine_7L <- factor(df$feminine,ordered = TRUE)
#summary(df)
############################################################################
# ara tinc dubte :
#cfd$ condition -->  race
df_cfd <- subset(df, df$dataset=="cfd")
df_cfd<- droplevels(df_cfd)
#faces$condition --> age of¿?¿?¡
df_faces <- subset(df, df$dataset=="faces")
df_faces$condition <- droplevels(df_faces$condition)
df_faces$ageGroup <- df_faces$condition
##############################################################################
# attractiveness : As a 3 category ordinal data?

df$perceived_attractivenessfact <- df$perceived_attractiveness
if(oredered== TRUE){
  df$perceived_attractivenessfact <- factor(df$perceived_attractivenessfact,ordered = TRUE)
}else{
  df$perceived_attractivenessfact <- factor(df$perceived_attractivenessfact)
}

df_faces$perceived_attractivenessfact <- df_faces$perceived_attractiveness
if(oredered== TRUE){
  df_faces$perceived_attractivenessfact <- factor(df_faces$perceived_attractivenessfact,ordered = TRUE)
}else{
  df_faces$perceived_attractivenessfact <- factor(df_faces$perceived_attractivenessfact)
}

df$perceived_attractiveness_3cat <- df$perceived_attractiveness
table(df$perceived_attractiveness, df$beautification)
df$perceived_attractiveness_3cat[df$perceived_attractiveness <=3]="Low"
df$perceived_attractiveness_3cat[(df$perceived_attractiveness>2 & df$perceived_attractiveness <=4.5)]="Midd"
df$perceived_attractiveness_3cat[df$perceived_attractiveness >=5]="High"
table(df$perceived_attractiveness_3cat, df$beautification)
df$perceived_attractiveness_3cat <-  as.factor(df$perceived_attractiveness_3cat)

if(oredered== TRUE){
  df$perceived_attractiveness_3cat <- factor(df$perceived_attractiveness_3cat,ordered = TRUE,levels=c("Low","Midd", "High"))

}else{
  df$perceived_attractiveness_3cat <- factor(df$perceived_attractiveness_3cat,levels=c("Low","Midd", "High"))
}
######################################################

df_att_plus_5 <- read_csv("../csv_storage/allAttributesAllRatings_highAttractiveness_df.csv")
