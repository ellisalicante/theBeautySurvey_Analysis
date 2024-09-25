###############
plot_attrac <- function(df_plot,y, title){

# If_Rf_, Im_Rm  If_Rm,  Im_Rf
  p <- ggplot(df_plot,aes(y=y,x=Atrac,shape=image,  color=Rater, linetype = image))+
  geom_point( size=4)+
  ggtitle(title)+
  geom_smooth(method=lm,formula = y ~ splines::bs(x, 3),se=FALSE,fullrange=TRUE)+
  labs(colour = "Rater", linetype="Image")
  #geom_errorbar(aes(ymin=unusual-0.3*unusual_sd, ymax=unusual+0.3*unusual_sd), width=.2)
  # line stile es imatge If, Im  If,  Im
  #scale_linetype_manual(values = c("dotted", "solid", "dotted", "solid"),labels = c("Female ", "Male","female", "Male")) +
  # color es el rater Rf_, Rm  Rm,  Rf
  #scale_color_manual(values=c("Red","Blue","Blue","Red"),labels = c("Female ", "Male","Male", "Female"))+
 #scale_colour_manual(values=c("#999999","#000000", "#900009", "#900009") +
  return(p)
}
#############################
# same as the original but with the reescaled variables
Hello_attrac_Effect_beau_re <- function( images, df, x){
  #N <- length(unique(df[,x]))
  N <-  6

  df_If_Rf <- subset(df, df$imageGender =="female" & df$participantSex=="female")
  df_If_Rm <- subset(df, df$imageGender =="female" & df$participantSex=="male")
  df_Im_Rf <- subset(df, df$imageGender =="male" & df$participantSex=="female")
  df_Im_Rm <- subset(df, df$imageGender =="male" & df$participantSex=="male")

  Vint_If_Rf<- rep(0,N)
  Vint_If_Rm <- rep(0,N)
  Vint_Im_Rf <- rep(0,N)
  Vint_Im_Rm <- rep(0,N)

  Vunusu_If_Rf <- rep(0,N)
  Vunusu_If_Rm <- rep(0,N)
  Vunusu_Im_Rf <- rep(0,N)
  Vunusu_Im_Rm <- rep(0,N)

  Vfemin_If_Rf <- rep(0,N)
  Vfemin_If_Rm <- rep(0,N)
  Vfemin_Im_Rf <- rep(0,N)
  Vfemin_Im_Rm <- rep(0,N)

  Vtrust_If_Rf <- rep(0,N)
  Vtrust_If_Rm <- rep(0,N)
  Vtrust_Im_Rf <- rep(0,N)
  Vtrust_Im_Rm <- rep(0,N)

  Vhappy_If_Rf <- rep(0,N)
  Vhappy_If_Rm <- rep(0,N)
  Vhappy_Im_Rf <- rep(0,N)
  Vhappy_Im_Rm <- rep(0,N)

  Vsocia_If_Rf <- rep(0,N)
  Vsocia_If_Rm <- rep(0,N)
  Vsocia_Im_Rf <- rep(0,N)
  Vsocia_Im_Rm <- rep(0,N)

  Vint_If_Rf_sd <- rep(0,N)
  Vint_If_Rm_sd <- rep(0,N)
  Vint_Im_Rf_sd <- rep(0,N)
  Vint_Im_Rm_sd <- rep(0,N)

  Vunusu_If_Rf_sd <- rep(0,N)
  Vunusu_If_Rm_sd <- rep(0,N)
  Vunusu_Im_Rf_sd <- rep(0,N)
  Vunusu_Im_Rm_sd <- rep(0,N)

  Vfemin_If_Rf_sd <- rep(0,N)
  Vfemin_If_Rm_sd <- rep(0,N)
  Vfemin_Im_Rf_sd <- rep(0,N)
  Vfemin_Im_Rm_sd <- rep(0,N)

  Vtrust_If_Rf_sd <- rep(0,N)
  Vtrust_If_Rm_sd <- rep(0,N)
  Vtrust_Im_Rf_sd <- rep(0,N)
  Vtrust_Im_Rm_sd <- rep(0,N)

  Vhappy_If_Rf_sd <- rep(0,N)
  Vhappy_If_Rm_sd <- rep(0,N)
  Vhappy_Im_Rf_sd <- rep(0,N)
  Vhappy_Im_Rm_sd <- rep(0,N)

  Vsocia_If_Rf_sd <- rep(0,N)
  Vsocia_If_Rm_sd <- rep(0,N)
  Vsocia_Im_Rf_sd <- rep(0,N)
  Vsocia_Im_Rm_sd <- rep(0,N)


  Vint_If_Rf_MSO <- rep(0,N)
  Vint_If_Rm_MSO <- rep(0,N)
  Vint_Im_Rf_MSO <- rep(0,N)
  Vint_Im_Rm_MSO <- rep(0,N)

  Vint_If_Rf_MSO_sd <- rep(0,N)
  Vint_If_Rm_MSO_sd <- rep(0,N)
  Vint_Im_Rf_MSO_sd <- rep(0,N)
  Vint_Im_Rm_MSO_sd <- rep(0,N)

  for (i in c(1:N)) {

    index <- df_If_Rf[,x]==i
    Vint_If_Rf[i]     <- mean(as.numeric(df_If_Rf$intelligence_5L[index]))
    Vhappy_If_Rf[i]   <- mean(as.numeric(df_If_Rf$happy_5L[index]))
    Vfemin_If_Rf[i]   <- mean(as.numeric(df_If_Rf$feminine_6L[index]))
    Vtrust_If_Rf[i]   <- mean(as.numeric(df_If_Rf$trustworthy_6L[index]))
    Vunusu_If_Rf[i]   <- mean(as.numeric(df_If_Rf$unusual_6L[index]))
    Vsocia_If_Rf[i]   <- mean(as.numeric(df_If_Rf$sociability_5L[index]))
    #Vint_If_Rf_MSO[i]   <- mean(as.numeric(df_If_Rf$intelligence_MSOcont[index]))

    Vint_If_Rf_sd[i]     <- sd(as.numeric(df_If_Rf$intelligence_5L[index]))
    Vhappy_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$happy_5L[index]))
    Vfemin_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$feminine_6L[index]))
    Vtrust_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$trustworthy_6L[index]))
    Vunusu_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$unusual_6L[index]))
    Vsocia_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$sociability_5L[index]))


    index <- (df_If_Rm[,x]==i)
    Vint_If_Rm[i]     <- mean(as.numeric(df_If_Rm$intelligence_5L[index]))
    Vhappy_If_Rm[i]   <- mean(as.numeric(df_If_Rm$happy_5L[index]))
    Vfemin_If_Rm[i]   <- mean(as.numeric(df_If_Rm$feminine_6L[index]))
    Vtrust_If_Rm[i]   <- mean(as.numeric(df_If_Rm$trustworthy_6L[index]))
    Vunusu_If_Rm[i]   <- mean(as.numeric(df_If_Rm$unusual_6L[index]))
    Vsocia_If_Rm[i]   <- mean(as.numeric(df_If_Rm$sociability_5L[index]))
    #Vint_If_Rm_5[i]   <- mean(as.numeric(df_If_Rm$intelligence_5cont[index]))
    Vint_If_Rm_sd[i]     <- sd(as.numeric(df_If_Rm$intelligence_5L[index]))
    Vhappy_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$happy_5L[index]))
    Vfemin_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$feminine_6L[index]))
    Vtrust_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$trustworthy_6L[index]))
    Vunusu_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$unusual_6L[index]))
    Vsocia_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$sociability_5L[index]))

    index <- (df_Im_Rm[,x]==i)
    Vint_Im_Rm[i]     <- mean(as.numeric(df_Im_Rm$intelligence_5L[index]))
    Vhappy_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$happy_5L[index]))
    Vfemin_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$feminine_6L[index]))
    Vtrust_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$trustworthy_6L[index]))
    Vunusu_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$unusual_6L[index]))
    Vsocia_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$sociability_5L[index]))
    #Vint_Im_Rm_5[i]   <- mean(as.numeric(df_Im_Rm$intelligence_5cont[index]))
    Vint_Im_Rm_sd[i]     <- sd(as.numeric(df_Im_Rm$intelligence_7L[index]))
    Vhappy_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$happy_5L[index]))
    Vfemin_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$feminine_6L[index]))
    Vtrust_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$trustworthy_6L[index]))
    Vunusu_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$unusual_6L[index]))
    Vsocia_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$sociability_5L[index]))


    index <- (df_Im_Rf[,x]==i)
    Vint_Im_Rf[i]     <- mean(as.numeric(df_Im_Rf$intelligence_5L[index]))
    Vhappy_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$happy_5L[index]))
    Vtrust_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$trustworthy_6L[index]))
    Vfemin_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$feminine_6L[index]))
    Vunusu_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$unusual_6L[index]))
    Vsocia_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$sociability_5L[index]))
    #  Vint_Im_Rf_5[i]   <- mean(as.numeric(df_Im_Rf$intelligence_5cont[index]))
    Vint_Im_Rf_sd[i]     <- sd(as.numeric(df_Im_Rf$intelligence_5L[index]))
    Vhappy_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$happy_5L[index]))
    Vfemin_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$feminine_6L[index]))
    Vtrust_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$trustworthy_6L[index]))
    Vunusu_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$unusual_6L[index]))
    Vsocia_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$sociability_5L[index]))

  }
  # If_Rf_, Im_Rm  If_Rm,  Im_Rf
  df_plot <- data.frame(Atrac= rep(c(1:N),4),
                        color=    c( rep("If_Rf",N),   rep("Im_Rm",N),   rep("If_Rm",N),   rep("Im_Rf",N)),
                        image=    c( rep("Female",N),  rep("Male",N),    rep("Female",N),  rep("Male",N)),
                        Rater=    c( rep("Female",N),  rep("Male",N),   rep("Male",N),    rep("Female",N)),
                        Inte_5L=       c(c(Vint_If_Rf),   c(Vint_Im_Rm),     c(Vint_If_Rm),   c(Vint_Im_Rf)),
                        Happy_5L=      c(c(Vhappy_If_Rf), c(Vhappy_Im_Rm)   ,c(Vhappy_If_Rm), c(Vhappy_Im_Rf)) ,
                        Trust_6L=      c(c(Vtrust_If_Rf), c(Vtrust_Im_Rm)   ,c(Vtrust_If_Rm), c(Vtrust_Im_Rf)) ,
                        unusual_6L=    c(c(Vunusu_If_Rf), c(Vunusu_Im_Rm),   c(Vunusu_If_Rm), c(Vunusu_Im_Rf)),
                        fem_6L=        c(c(Vfemin_If_Rf), c(Vfemin_Im_Rm)   ,c(Vfemin_If_Rm), c(Vfemin_Im_Rf)),
                        sociability_5L=c(c(Vsocia_If_Rf), c(Vsocia_Im_Rm)   ,c(Vsocia_If_Rm), c(Vsocia_Im_Rf)),
                        Inte_sd=       c(c(Vint_If_Rf_sd),   c(Vint_Im_Rm_sd),     c(Vint_If_Rm_sd),   c(Vint_Im_Rf_sd)),
                        Happy_sd=      c(c(Vhappy_If_Rf_sd), c(Vhappy_Im_Rm_sd)   ,c(Vhappy_If_Rm_sd), c(Vhappy_Im_Rf_sd)) ,
                        Trust_sd=      c(c(Vtrust_If_Rf_sd), c(Vtrust_Im_Rm_sd)   ,c(Vtrust_If_Rm_sd), c(Vtrust_Im_Rf_sd)) ,
                        unusual_sd=    c(c(Vunusu_If_Rf_sd), c(Vunusu_Im_Rm_sd),   c(Vunusu_If_Rm_sd), c(Vunusu_Im_Rf_sd)),
                        fem_sd=        c(c(Vfemin_If_Rf_sd), c(Vfemin_Im_Rm_sd)   ,c(Vfemin_If_Rm_sd), c(Vfemin_Im_Rf_sd)),
                        sociability_sd=c(c(Vsocia_If_Rf_sd), c(Vsocia_Im_Rm_sd)   ,c(Vsocia_If_Rm_sd), c(Vsocia_Im_Rf_sd)))
  return(df_plot)
}
###########################
#Halo effect plots but with just the 7L plots instead of doing the collapsing
# Doing this to see if the curves we get are because of the re-scaling or whether they are a feature of the data itself
Hello_attrac_Effect_beau_notRescaled <- function( images, df, x){
  #N <- length(unique(df[,x]))
  N <-  6

  df_If_Rf <- subset(df, df$imageGender =="female" & df$participantSex=="female")
  df_If_Rm <- subset(df, df$imageGender =="female" & df$participantSex=="male")
  df_Im_Rf <- subset(df, df$imageGender =="male" & df$participantSex=="female")
  df_Im_Rm <- subset(df, df$imageGender =="male" & df$participantSex=="male")

  Vint_If_Rf<- rep(0,N)
  Vint_If_Rm <- rep(0,N)
  Vint_Im_Rf <- rep(0,N)
  Vint_Im_Rm <- rep(0,N)

  Vunusu_If_Rf <- rep(0,N)
  Vunusu_If_Rm <- rep(0,N)
  Vunusu_Im_Rf <- rep(0,N)
  Vunusu_Im_Rm <- rep(0,N)

  Vfemin_If_Rf <- rep(0,N)
  Vfemin_If_Rm <- rep(0,N)
  Vfemin_Im_Rf <- rep(0,N)
  Vfemin_Im_Rm <- rep(0,N)

  Vtrust_If_Rf <- rep(0,N)
  Vtrust_If_Rm <- rep(0,N)
  Vtrust_Im_Rf <- rep(0,N)
  Vtrust_Im_Rm <- rep(0,N)

  Vhappy_If_Rf <- rep(0,N)
  Vhappy_If_Rm <- rep(0,N)
  Vhappy_Im_Rf <- rep(0,N)
  Vhappy_Im_Rm <- rep(0,N)

  Vsocia_If_Rf <- rep(0,N)
  Vsocia_If_Rm <- rep(0,N)
  Vsocia_Im_Rf <- rep(0,N)
  Vsocia_Im_Rm <- rep(0,N)

  Vint_If_Rf_sd <- rep(0,N)
  Vint_If_Rm_sd <- rep(0,N)
  Vint_Im_Rf_sd <- rep(0,N)
  Vint_Im_Rm_sd <- rep(0,N)

  Vunusu_If_Rf_sd <- rep(0,N)
  Vunusu_If_Rm_sd <- rep(0,N)
  Vunusu_Im_Rf_sd <- rep(0,N)
  Vunusu_Im_Rm_sd <- rep(0,N)

  Vfemin_If_Rf_sd <- rep(0,N)
  Vfemin_If_Rm_sd <- rep(0,N)
  Vfemin_Im_Rf_sd <- rep(0,N)
  Vfemin_Im_Rm_sd <- rep(0,N)

  Vtrust_If_Rf_sd <- rep(0,N)
  Vtrust_If_Rm_sd <- rep(0,N)
  Vtrust_Im_Rf_sd <- rep(0,N)
  Vtrust_Im_Rm_sd <- rep(0,N)

  Vhappy_If_Rf_sd <- rep(0,N)
  Vhappy_If_Rm_sd <- rep(0,N)
  Vhappy_Im_Rf_sd <- rep(0,N)
  Vhappy_Im_Rm_sd <- rep(0,N)

  Vsocia_If_Rf_sd <- rep(0,N)
  Vsocia_If_Rm_sd <- rep(0,N)
  Vsocia_Im_Rf_sd <- rep(0,N)
  Vsocia_Im_Rm_sd <- rep(0,N)


  Vint_If_Rf_MSO <- rep(0,N)
  Vint_If_Rm_MSO <- rep(0,N)
  Vint_Im_Rf_MSO <- rep(0,N)
  Vint_Im_Rm_MSO <- rep(0,N)

  Vint_If_Rf_MSO_sd <- rep(0,N)
  Vint_If_Rm_MSO_sd <- rep(0,N)
  Vint_Im_Rf_MSO_sd <- rep(0,N)
  Vint_Im_Rm_MSO_sd <- rep(0,N)

  for (i in c(1:N)) {

    index <- df_If_Rf[,x]==i
    Vint_If_Rf[i]     <- mean(as.numeric(df_If_Rf$intelligence_7L[index]))
    Vhappy_If_Rf[i]   <- mean(as.numeric(df_If_Rf$happy_7L[index]))
    Vfemin_If_Rf[i]   <- mean(as.numeric(df_If_Rf$feminine_7L[index]))
    Vtrust_If_Rf[i]   <- mean(as.numeric(df_If_Rf$trustworthy_7L[index]))
    Vunusu_If_Rf[i]   <- mean(as.numeric(df_If_Rf$unusual_7L[index]))
    Vsocia_If_Rf[i]   <- mean(as.numeric(df_If_Rf$sociability_7L[index]))
    #Vint_If_Rf_MSO[i]   <- mean(as.numeric(df_If_Rf$intelligence_MSOcont[index]))

    Vint_If_Rf_sd[i]     <- sd(as.numeric(df_If_Rf$intelligence_7L[index]))
    Vhappy_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$happy_7L[index]))
    Vfemin_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$feminine_7L[index]))
    Vtrust_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$trustworthy_7L[index]))
    Vunusu_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$unusual_7L[index]))
    Vsocia_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$sociability_7L[index]))


    index <- (df_If_Rm[,x]==i)
    Vint_If_Rm[i]     <- mean(as.numeric(df_If_Rm$intelligence_7L[index]))
    Vhappy_If_Rm[i]   <- mean(as.numeric(df_If_Rm$happy_7L[index]))
    Vfemin_If_Rm[i]   <- mean(as.numeric(df_If_Rm$feminine_7L[index]))
    Vtrust_If_Rm[i]   <- mean(as.numeric(df_If_Rm$trustworthy_7L[index]))
    Vunusu_If_Rm[i]   <- mean(as.numeric(df_If_Rm$unusual_7L[index]))
    Vsocia_If_Rm[i]   <- mean(as.numeric(df_If_Rm$sociability_7L[index]))
    #Vint_If_Rm_5[i]   <- mean(as.numeric(df_If_Rm$intelligence_5cont[index]))
    Vint_If_Rm_sd[i]     <- sd(as.numeric(df_If_Rm$intelligence_7L[index]))
    Vhappy_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$happy_7L[index]))
    Vfemin_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$feminine_7L[index]))
    Vtrust_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$trustworthy_7L[index]))
    Vunusu_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$unusual_7L[index]))
    Vsocia_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$sociability_7L[index]))

    index <- (df_Im_Rm[,x]==i)
    Vint_Im_Rm[i]     <- mean(as.numeric(df_Im_Rm$intelligence_7L[index]))
    Vhappy_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$happy_7L[index]))
    Vfemin_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$feminine_7L[index]))
    Vtrust_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$trustworthy_7L[index]))
    Vunusu_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$unusual_7L[index]))
    Vsocia_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$sociability_7L[index]))
    #Vint_Im_Rm_5[i]   <- mean(as.numeric(df_Im_Rm$intelligence_5cont[index]))
    Vint_Im_Rm_sd[i]     <- sd(as.numeric(df_Im_Rm$intelligence_7L[index]))
    Vhappy_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$happy_7L[index]))
    Vfemin_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$feminine_7L[index]))
    Vtrust_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$trustworthy_7L[index]))
    Vunusu_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$unusual_7L[index]))
    Vsocia_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$sociability_7L[index]))


    index <- (df_Im_Rf[,x]==i)
    Vint_Im_Rf[i]     <- mean(as.numeric(df_Im_Rf$intelligence_7L[index]))
    Vhappy_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$happy_7L[index]))
    Vtrust_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$trustworthy_7L[index]))
    Vfemin_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$feminine_7L[index]))
    Vunusu_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$unusual_7L[index]))
    Vsocia_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$sociability_7L[index]))
    #  Vint_Im_Rf_5[i]   <- mean(as.numeric(df_Im_Rf$intelligence_5cont[index]))
    Vint_Im_Rf_sd[i]     <- sd(as.numeric(df_Im_Rf$intelligence_7L[index]))
    Vhappy_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$happy_7L[index]))
    Vfemin_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$feminine_7L[index]))
    Vtrust_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$trustworthy_7L[index]))
    Vunusu_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$unusual_7L[index]))
    Vsocia_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$sociability_7L[index]))

  }
  # If_Rf_, Im_Rm  If_Rm,  Im_Rf
  df_plot <- data.frame(Atrac= rep(c(1:N),4),
                        color=    c( rep("If_Rf",N),   rep("Im_Rm",N),   rep("If_Rm",N),   rep("Im_Rf",N)),
                        image=    c( rep("Female",N),  rep("Male",N),    rep("Female",N),  rep("Male",N)),
                        Rater=    c( rep("Female",N),  rep("Male",N),   rep("Male",N),    rep("Female",N)),
                        Inte_5L=       c(c(Vint_If_Rf),   c(Vint_Im_Rm),     c(Vint_If_Rm),   c(Vint_Im_Rf)),
                        Happy_5L=      c(c(Vhappy_If_Rf), c(Vhappy_Im_Rm)   ,c(Vhappy_If_Rm), c(Vhappy_Im_Rf)) ,
                        Trust_6L=      c(c(Vtrust_If_Rf), c(Vtrust_Im_Rm)   ,c(Vtrust_If_Rm), c(Vtrust_Im_Rf)) ,
                        unusual_6L=    c(c(Vunusu_If_Rf), c(Vunusu_Im_Rm),   c(Vunusu_If_Rm), c(Vunusu_Im_Rf)),
                        fem_6L=        c(c(Vfemin_If_Rf), c(Vfemin_Im_Rm)   ,c(Vfemin_If_Rm), c(Vfemin_Im_Rf)),
                        sociability_5L=c(c(Vsocia_If_Rf), c(Vsocia_Im_Rm)   ,c(Vsocia_If_Rm), c(Vsocia_Im_Rf)),
                        Inte_sd=       c(c(Vint_If_Rf_sd),   c(Vint_Im_Rm_sd),     c(Vint_If_Rm_sd),   c(Vint_Im_Rf_sd)),
                        Happy_sd=      c(c(Vhappy_If_Rf_sd), c(Vhappy_Im_Rm_sd)   ,c(Vhappy_If_Rm_sd), c(Vhappy_Im_Rf_sd)) ,
                        Trust_sd=      c(c(Vtrust_If_Rf_sd), c(Vtrust_Im_Rm_sd)   ,c(Vtrust_If_Rm_sd), c(Vtrust_Im_Rf_sd)) ,
                        unusual_sd=    c(c(Vunusu_If_Rf_sd), c(Vunusu_Im_Rm_sd),   c(Vunusu_If_Rm_sd), c(Vunusu_Im_Rf_sd)),
                        fem_sd=        c(c(Vfemin_If_Rf_sd), c(Vfemin_Im_Rm_sd)   ,c(Vfemin_If_Rm_sd), c(Vfemin_Im_Rf_sd)),
                        sociability_sd=c(c(Vsocia_If_Rf_sd), c(Vsocia_Im_Rm_sd)   ,c(Vsocia_If_Rm_sd), c(Vsocia_Im_Rf_sd)))
  return(df_plot)
}

##################
#Halo effect plot with attractiveness also set to 7 levels
Hello_attrac_Effect_beau_notRescaled_attr7 <- function( images, df, x){
  #N <- length(unique(df[,x]))
  N <-  7

  df_If_Rf <- subset(df, df$imageGender =="female" & df$participantSex=="female")
  df_If_Rm <- subset(df, df$imageGender =="female" & df$participantSex=="male")
  df_Im_Rf <- subset(df, df$imageGender =="male" & df$participantSex=="female")
  df_Im_Rm <- subset(df, df$imageGender =="male" & df$participantSex=="male")

  Vint_If_Rf<- rep(0,N)
  Vint_If_Rm <- rep(0,N)
  Vint_Im_Rf <- rep(0,N)
  Vint_Im_Rm <- rep(0,N)

  Vunusu_If_Rf <- rep(0,N)
  Vunusu_If_Rm <- rep(0,N)
  Vunusu_Im_Rf <- rep(0,N)
  Vunusu_Im_Rm <- rep(0,N)

  Vfemin_If_Rf <- rep(0,N)
  Vfemin_If_Rm <- rep(0,N)
  Vfemin_Im_Rf <- rep(0,N)
  Vfemin_Im_Rm <- rep(0,N)

  Vtrust_If_Rf <- rep(0,N)
  Vtrust_If_Rm <- rep(0,N)
  Vtrust_Im_Rf <- rep(0,N)
  Vtrust_Im_Rm <- rep(0,N)

  Vhappy_If_Rf <- rep(0,N)
  Vhappy_If_Rm <- rep(0,N)
  Vhappy_Im_Rf <- rep(0,N)
  Vhappy_Im_Rm <- rep(0,N)

  Vsocia_If_Rf <- rep(0,N)
  Vsocia_If_Rm <- rep(0,N)
  Vsocia_Im_Rf <- rep(0,N)
  Vsocia_Im_Rm <- rep(0,N)

  Vint_If_Rf_sd <- rep(0,N)
  Vint_If_Rm_sd <- rep(0,N)
  Vint_Im_Rf_sd <- rep(0,N)
  Vint_Im_Rm_sd <- rep(0,N)

  Vunusu_If_Rf_sd <- rep(0,N)
  Vunusu_If_Rm_sd <- rep(0,N)
  Vunusu_Im_Rf_sd <- rep(0,N)
  Vunusu_Im_Rm_sd <- rep(0,N)

  Vfemin_If_Rf_sd <- rep(0,N)
  Vfemin_If_Rm_sd <- rep(0,N)
  Vfemin_Im_Rf_sd <- rep(0,N)
  Vfemin_Im_Rm_sd <- rep(0,N)

  Vtrust_If_Rf_sd <- rep(0,N)
  Vtrust_If_Rm_sd <- rep(0,N)
  Vtrust_Im_Rf_sd <- rep(0,N)
  Vtrust_Im_Rm_sd <- rep(0,N)

  Vhappy_If_Rf_sd <- rep(0,N)
  Vhappy_If_Rm_sd <- rep(0,N)
  Vhappy_Im_Rf_sd <- rep(0,N)
  Vhappy_Im_Rm_sd <- rep(0,N)

  Vsocia_If_Rf_sd <- rep(0,N)
  Vsocia_If_Rm_sd <- rep(0,N)
  Vsocia_Im_Rf_sd <- rep(0,N)
  Vsocia_Im_Rm_sd <- rep(0,N)


  Vint_If_Rf_MSO <- rep(0,N)
  Vint_If_Rm_MSO <- rep(0,N)
  Vint_Im_Rf_MSO <- rep(0,N)
  Vint_Im_Rm_MSO <- rep(0,N)

  Vint_If_Rf_MSO_sd <- rep(0,N)
  Vint_If_Rm_MSO_sd <- rep(0,N)
  Vint_Im_Rf_MSO_sd <- rep(0,N)
  Vint_Im_Rm_MSO_sd <- rep(0,N)

  for (i in c(1:N)) {

    index <- df_If_Rf[,x]==i
    Vint_If_Rf[i]     <- mean(as.numeric(df_If_Rf$intelligence_7L[index]))
    Vhappy_If_Rf[i]   <- mean(as.numeric(df_If_Rf$happy_7L[index]))
    Vfemin_If_Rf[i]   <- mean(as.numeric(df_If_Rf$feminine_7L[index]))
    Vtrust_If_Rf[i]   <- mean(as.numeric(df_If_Rf$trustworthy_7L[index]))
    Vunusu_If_Rf[i]   <- mean(as.numeric(df_If_Rf$unusual_7L[index]))
    Vsocia_If_Rf[i]   <- mean(as.numeric(df_If_Rf$sociability_7L[index]))
    #Vint_If_Rf_MSO[i]   <- mean(as.numeric(df_If_Rf$intelligence_MSOcont[index]))

    Vint_If_Rf_sd[i]     <- sd(as.numeric(df_If_Rf$intelligence_7L[index]))
    Vhappy_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$happy_7L[index]))
    Vfemin_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$feminine_7L[index]))
    Vtrust_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$trustworthy_7L[index]))
    Vunusu_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$unusual_7L[index]))
    Vsocia_If_Rf_sd[i]   <- sd(as.numeric(df_If_Rf$sociability_7L[index]))


    index <- (df_If_Rm[,x]==i)
    Vint_If_Rm[i]     <- mean(as.numeric(df_If_Rm$intelligence_7L[index]))
    Vhappy_If_Rm[i]   <- mean(as.numeric(df_If_Rm$happy_7L[index]))
    Vfemin_If_Rm[i]   <- mean(as.numeric(df_If_Rm$feminine_7L[index]))
    Vtrust_If_Rm[i]   <- mean(as.numeric(df_If_Rm$trustworthy_7L[index]))
    Vunusu_If_Rm[i]   <- mean(as.numeric(df_If_Rm$unusual_7L[index]))
    Vsocia_If_Rm[i]   <- mean(as.numeric(df_If_Rm$sociability_7L[index]))
    #Vint_If_Rm_5[i]   <- mean(as.numeric(df_If_Rm$intelligence_5cont[index]))
    Vint_If_Rm_sd[i]     <- sd(as.numeric(df_If_Rm$intelligence_7L[index]))
    Vhappy_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$happy_7L[index]))
    Vfemin_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$feminine_7L[index]))
    Vtrust_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$trustworthy_7L[index]))
    Vunusu_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$unusual_7L[index]))
    Vsocia_If_Rm_sd[i]   <- sd(as.numeric(df_If_Rm$sociability_7L[index]))

    index <- (df_Im_Rm[,x]==i)
    Vint_Im_Rm[i]     <- mean(as.numeric(df_Im_Rm$intelligence_7L[index]))
    Vhappy_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$happy_7L[index]))
    Vfemin_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$feminine_7L[index]))
    Vtrust_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$trustworthy_7L[index]))
    Vunusu_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$unusual_7L[index]))
    Vsocia_Im_Rm[i]   <- mean(as.numeric(df_Im_Rm$sociability_7L[index]))
    #Vint_Im_Rm_5[i]   <- mean(as.numeric(df_Im_Rm$intelligence_5cont[index]))
    Vint_Im_Rm_sd[i]     <- sd(as.numeric(df_Im_Rm$intelligence_7L[index]))
    Vhappy_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$happy_7L[index]))
    Vfemin_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$feminine_7L[index]))
    Vtrust_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$trustworthy_7L[index]))
    Vunusu_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$unusual_7L[index]))
    Vsocia_Im_Rm_sd[i]   <- sd(as.numeric(df_Im_Rm$sociability_7L[index]))


    index <- (df_Im_Rf[,x]==i)
    Vint_Im_Rf[i]     <- mean(as.numeric(df_Im_Rf$intelligence_7L[index]))
    Vhappy_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$happy_7L[index]))
    Vtrust_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$trustworthy_7L[index]))
    Vfemin_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$feminine_7L[index]))
    Vunusu_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$unusual_7L[index]))
    Vsocia_Im_Rf[i]   <- mean(as.numeric(df_Im_Rf$sociability_7L[index]))
    #  Vint_Im_Rf_5[i]   <- mean(as.numeric(df_Im_Rf$intelligence_5cont[index]))
    Vint_Im_Rf_sd[i]     <- sd(as.numeric(df_Im_Rf$intelligence_7L[index]))
    Vhappy_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$happy_7L[index]))
    Vfemin_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$feminine_7L[index]))
    Vtrust_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$trustworthy_7L[index]))
    Vunusu_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$unusual_7L[index]))
    Vsocia_Im_Rf_sd[i]   <- sd(as.numeric(df_Im_Rf$sociability_7L[index]))

  }
  # If_Rf_, Im_Rm  If_Rm,  Im_Rf
  df_plot <- data.frame(Atrac= rep(c(1:N),4),
                        color=    c( rep("If_Rf",N),   rep("Im_Rm",N),   rep("If_Rm",N),   rep("Im_Rf",N)),
                        image=    c( rep("Female",N),  rep("Male",N),    rep("Female",N),  rep("Male",N)),
                        Rater=    c( rep("Female",N),  rep("Male",N),   rep("Male",N),    rep("Female",N)),
                        Inte_5L=       c(c(Vint_If_Rf),   c(Vint_Im_Rm),     c(Vint_If_Rm),   c(Vint_Im_Rf)),
                        Happy_5L=      c(c(Vhappy_If_Rf), c(Vhappy_Im_Rm)   ,c(Vhappy_If_Rm), c(Vhappy_Im_Rf)) ,
                        Trust_6L=      c(c(Vtrust_If_Rf), c(Vtrust_Im_Rm)   ,c(Vtrust_If_Rm), c(Vtrust_Im_Rf)) ,
                        unusual_6L=    c(c(Vunusu_If_Rf), c(Vunusu_Im_Rm),   c(Vunusu_If_Rm), c(Vunusu_Im_Rf)),
                        fem_6L=        c(c(Vfemin_If_Rf), c(Vfemin_Im_Rm)   ,c(Vfemin_If_Rm), c(Vfemin_Im_Rf)),
                        sociability_5L=c(c(Vsocia_If_Rf), c(Vsocia_Im_Rm)   ,c(Vsocia_If_Rm), c(Vsocia_Im_Rf)),
                        Inte_sd=       c(c(Vint_If_Rf_sd),   c(Vint_Im_Rm_sd),     c(Vint_If_Rm_sd),   c(Vint_Im_Rf_sd)),
                        Happy_sd=      c(c(Vhappy_If_Rf_sd), c(Vhappy_Im_Rm_sd)   ,c(Vhappy_If_Rm_sd), c(Vhappy_Im_Rf_sd)) ,
                        Trust_sd=      c(c(Vtrust_If_Rf_sd), c(Vtrust_Im_Rm_sd)   ,c(Vtrust_If_Rm_sd), c(Vtrust_Im_Rf_sd)) ,
                        unusual_sd=    c(c(Vunusu_If_Rf_sd), c(Vunusu_Im_Rm_sd),   c(Vunusu_If_Rm_sd), c(Vunusu_Im_Rf_sd)),
                        fem_sd=        c(c(Vfemin_If_Rf_sd), c(Vfemin_Im_Rm_sd)   ,c(Vfemin_If_Rm_sd), c(Vfemin_Im_Rf_sd)),
                        sociability_sd=c(c(Vsocia_If_Rf_sd), c(Vsocia_Im_Rm_sd)   ,c(Vsocia_If_Rm_sd), c(Vsocia_Im_Rf_sd)))
  return(df_plot)
}


####################
#  images= "beautified"/ "original"
#  x = perceived_attractiveness / perceived_attractiveness_6L
# images= "original", df = df_o, "perceived_attractiveness_6L"
Hello_attrac_Effect <- function( images, df, x){
  #N <- length(unique(df[,x]))
  N <-  nrow(unique(df[,x]))

  df_If_Rf <- subset(df, df$beautification==images & df$imageGender =="female" & df$participantSex=="female")
  df_If_Rm <- subset(df, df$beautification==images & df$imageGender =="female" & df$participantSex=="male")
  df_Im_Rf <- subset(df, df$beautification==images & df$imageGender =="male" & df$participantSex=="female")
  df_Im_Rm <- subset(df, df$beautification==images & df$imageGender =="male" & df$participantSex=="male")

  Vint_If_Rf<- rep(0,N)
  Vint_If_Rm <- rep(0,N)
  Vint_Im_Rf <- rep(0,N)
  Vint_Im_Rm <- rep(0,N)

  Vunusu_If_Rf <- rep(0,N)
  Vunusu_If_Rm <- rep(0,N)
  Vunusu_Im_Rf <- rep(0,N)
  Vunusu_Im_Rm <- rep(0,N)

  Vfemin_If_Rf <- rep(0,N)
  Vfemin_If_Rm <- rep(0,N)
  Vfemin_Im_Rf <- rep(0,N)
  Vfemin_Im_Rm <- rep(0,N)

  Vtrust_If_Rf <- rep(0,N)
  Vtrust_If_Rm <- rep(0,N)
  Vtrust_Im_Rf <- rep(0,N)
  Vtrust_Im_Rm <- rep(0,N)

  Vhappy_If_Rf <- rep(0,N)
  Vhappy_If_Rm <- rep(0,N)
  Vhappy_Im_Rf <- rep(0,N)
  Vhappy_Im_Rm <- rep(0,N)

  Vsocia_If_Rf <- rep(0,N)
  Vsocia_If_Rm <- rep(0,N)
  Vsocia_Im_Rf <- rep(0,N)
  Vsocia_Im_Rm <- rep(0,N)

  Vint_If_Rf_sd <- rep(0,N)
  Vint_If_Rm_sd <- rep(0,N)
  Vint_Im_Rf_sd <- rep(0,N)
  Vint_Im_Rm_sd <- rep(0,N)

  Vunusu_If_Rf_sd <- rep(0,N)
  Vunusu_If_Rm_sd <- rep(0,N)
  Vunusu_Im_Rf_sd <- rep(0,N)
  Vunusu_Im_Rm_sd <- rep(0,N)

  Vfemin_If_Rf_sd <- rep(0,N)
  Vfemin_If_Rm_sd <- rep(0,N)
  Vfemin_Im_Rf_sd <- rep(0,N)
  Vfemin_Im_Rm_sd <- rep(0,N)

  Vtrust_If_Rf_sd <- rep(0,N)
  Vtrust_If_Rm_sd <- rep(0,N)
  Vtrust_Im_Rf_sd <- rep(0,N)
  Vtrust_Im_Rm_sd <- rep(0,N)

  Vhappy_If_Rf_sd <- rep(0,N)
  Vhappy_If_Rm_sd <- rep(0,N)
  Vhappy_Im_Rf_sd <- rep(0,N)
  Vhappy_Im_Rm_sd <- rep(0,N)

  Vsocia_If_Rf_sd <- rep(0,N)
  Vsocia_If_Rm_sd <- rep(0,N)
  Vsocia_Im_Rf_sd <- rep(0,N)
  Vsocia_Im_Rm_sd <- rep(0,N)


  Vint_If_Rf_MSO <- rep(0,N)
  Vint_If_Rm_MSO <- rep(0,N)
  Vint_Im_Rf_MSO <- rep(0,N)
  Vint_Im_Rm_MSO <- rep(0,N)

  Vint_If_Rf_MSO_sd <- rep(0,N)
  Vint_If_Rm_MSO_sd <- rep(0,N)
  Vint_Im_Rf_MSO_sd <- rep(0,N)
  Vint_Im_Rm_MSO_sd <- rep(0,N)

  for (i in c(1:N)) {

    index <- df_If_Rf[,x]==i
    Vint_If_Rf[i]     <- mean(df_If_Rf$intelligence[index])
    Vhappy_If_Rf[i]   <- mean(df_If_Rf$happy[index])
    Vfemin_If_Rf[i]   <- mean(df_If_Rf$feminine[index])
    Vtrust_If_Rf[i]   <- mean(df_If_Rf$trustworthy[index])
    Vunusu_If_Rf[i]   <- mean(df_If_Rf$unusual[index])
    Vsocia_If_Rf[i]   <- mean(df_If_Rf$sociability[index])
    #Vint_If_Rf_MSO[i]   <- mean(df_If_Rf$intelligence_MSOcont[index])

    Vint_If_Rf_sd[i]     <- sd(df_If_Rf$intelligence[index])
    Vhappy_If_Rf_sd[i]   <- sd(df_If_Rf$happy[index])
    Vfemin_If_Rf_sd[i]   <- sd(df_If_Rf$feminine[index])
    Vtrust_If_Rf_sd[i]   <- sd(df_If_Rf$trustworthy[index])
    Vunusu_If_Rf_sd[i]   <- sd(df_If_Rf$unusual[index])
    Vsocia_If_Rf_sd[i]   <- sd(df_If_Rf$sociability[index])


    index <- (df_If_Rm[,x]==i)
    Vint_If_Rm[i]     <- mean(df_If_Rm$intelligence[index])
    Vhappy_If_Rm[i]   <- mean(df_If_Rm$happy[index])
    Vfemin_If_Rm[i]   <- mean(df_If_Rm$feminine[index])
    Vtrust_If_Rm[i]   <- mean(df_If_Rm$trustworthy[index])
    Vunusu_If_Rm[i]   <- mean(df_If_Rm$unusual[index])
    Vsocia_If_Rm[i]   <- mean(df_If_Rm$sociability[index])
    #Vint_If_Rm_5[i]   <- mean(df_If_Rm$intelligence_5cont[index])
    Vint_If_Rm_sd[i]     <- sd(df_If_Rm$intelligence[index])
    Vhappy_If_Rm_sd[i]   <- sd(df_If_Rm$happy[index])
    Vfemin_If_Rm_sd[i]   <- sd(df_If_Rm$feminine[index])
    Vtrust_If_Rm_sd[i]   <- sd(df_If_Rm$trustworthy[index])
    Vunusu_If_Rm_sd[i]   <- sd(df_If_Rm$unusual[index])
    Vsocia_If_Rm_sd[i]   <- sd(df_If_Rm$sociability[index])

    index <- (df_Im_Rm[,x]==i)
    Vint_Im_Rm[i]     <- mean(df_Im_Rm$intelligence[index])
    Vhappy_Im_Rm[i]   <- mean(df_Im_Rm$happy[index])
    Vfemin_Im_Rm[i]   <- mean(df_Im_Rm$feminine[index])
    Vtrust_Im_Rm[i]   <- mean(df_Im_Rm$trustworthy[index])
    Vunusu_Im_Rm[i]   <- mean(df_Im_Rm$unusual[index])
    Vsocia_Im_Rm[i]   <- mean(df_Im_Rm$sociability[index])
    #Vint_Im_Rm_5[i]   <- mean(df_Im_Rm$intelligence_5cont[index])
    Vint_Im_Rm_sd[i]     <- sd(df_Im_Rm$intelligence[index])
    Vhappy_Im_Rm_sd[i]   <- sd(df_Im_Rm$happy[index])
    Vfemin_Im_Rm_sd[i]   <- sd(df_Im_Rm$feminine[index])
    Vtrust_Im_Rm_sd[i]   <- sd(df_Im_Rm$trustworthy[index])
    Vunusu_Im_Rm_sd[i]   <- sd(df_Im_Rm$unusual[index])
    Vsocia_Im_Rm_sd[i]   <- sd(df_Im_Rm$sociability[index])


    index <- (df_Im_Rf[,x]==i)
    Vint_Im_Rf[i]     <- mean(df_Im_Rf$intelligence[index])
    Vhappy_Im_Rf[i]   <- mean(df_Im_Rf$happy[index])
    Vtrust_Im_Rf[i]   <- mean(df_Im_Rf$trustworthy[index])
    Vfemin_Im_Rf[i]   <- mean(df_Im_Rf$feminine[index])
    Vunusu_Im_Rf[i]   <- mean(df_Im_Rf$unusual[index])
    Vsocia_Im_Rf[i]   <- mean(df_Im_Rf$sociability[index])
    #  Vint_Im_Rf_5[i]   <- mean(df_Im_Rf$intelligence_5cont[index])
    Vint_Im_Rf_sd[i]     <- sd(df_Im_Rf$intelligence[index])
    Vhappy_Im_Rf_sd[i]   <- sd(df_Im_Rf$happy[index])
    Vfemin_Im_Rf_sd[i]   <- sd(df_Im_Rf$feminine[index])
    Vtrust_Im_Rf_sd[i]   <- sd(df_Im_Rf$trustworthy[index])
    Vunusu_Im_Rf_sd[i]   <- sd(df_Im_Rf$unusual[index])
    Vsocia_Im_Rf_sd[i]   <- sd(df_Im_Rf$sociability[index])

  }
  # If_Rf_, Im_Rm  If_Rm,  Im_Rf
  df_plot <- data.frame(Atrac= rep(c(1:N),4),
                        color=    c( rep("If_Rf",N),   rep("Im_Rm",N),   rep("If_Rm",N),   rep("Im_Rf",N)),
                        image=    c( rep("Female",N),  rep("Male",N),    rep("Female",N),  rep("Male",N)),
                        Rater=    c( rep("Female",N),  rep("Male",N),   rep("Male",N),    rep("Female",N)),
                        Inte=       c(c(Vint_If_Rf),   c(Vint_Im_Rm),     c(Vint_If_Rm),   c(Vint_Im_Rf)),
                        Happy=      c(c(Vhappy_If_Rf), c(Vhappy_Im_Rm)   ,c(Vhappy_If_Rm), c(Vhappy_Im_Rf)) ,
                        Trust=      c(c(Vtrust_If_Rf), c(Vtrust_Im_Rm)   ,c(Vtrust_If_Rm), c(Vtrust_Im_Rf)) ,
                        unusual=    c(c(Vunusu_If_Rf), c(Vunusu_Im_Rm),   c(Vunusu_If_Rm), c(Vunusu_Im_Rf)),
                        fem=        c(c(Vfemin_If_Rf), c(Vfemin_Im_Rm)   ,c(Vfemin_If_Rm), c(Vfemin_Im_Rf)),
                        sociability=c(c(Vsocia_If_Rf), c(Vsocia_Im_Rm)   ,c(Vsocia_If_Rm), c(Vsocia_Im_Rf)),
                        Inte_sd=       c(c(Vint_If_Rf_sd),   c(Vint_Im_Rm_sd),     c(Vint_If_Rm_sd),   c(Vint_Im_Rf_sd)),
                        Happy_sd=      c(c(Vhappy_If_Rf_sd), c(Vhappy_Im_Rm_sd)   ,c(Vhappy_If_Rm_sd), c(Vhappy_Im_Rf_sd)) ,
                        Trust_sd=      c(c(Vtrust_If_Rf_sd), c(Vtrust_Im_Rm_sd)   ,c(Vtrust_If_Rm_sd), c(Vtrust_Im_Rf_sd)) ,
                        unusual_sd=    c(c(Vunusu_If_Rf_sd), c(Vunusu_Im_Rm_sd),   c(Vunusu_If_Rm_sd), c(Vunusu_Im_Rf_sd)),
                        fem_sd=        c(c(Vfemin_If_Rf_sd), c(Vfemin_Im_Rm_sd)   ,c(Vfemin_If_Rm_sd), c(Vfemin_Im_Rf_sd)),
                        sociability_sd=c(c(Vsocia_If_Rf_sd), c(Vsocia_Im_Rm_sd)   ,c(Vsocia_If_Rm_sd), c(Vsocia_Im_Rf_sd)))
   return(df_plot)
}
#################
Phi_newy_v1 <-  function(Model){
  N <- length(Model$phi[,1])
  phi <- Model$phi[,1]
  y_new= (N-1) * phi+1
  y_new <- round(y_new,4)
  return(y_new)
}


Phi_newy_v2 <-  function(Model, data, name){
  N_mod= data$N_model[nrow(data)]
  N <- length(Model$phi[,1])
  phi <- Model$phi[,1]
  y_new= (N-1) * phi+1
  y_new <- round(y_new,4)

  V_phi <- c( data$V_phi, phi)
  V_newy <- c(data$V_newy,y_new)
  V_model <- c(data$V_model, rep(name,N))
  N_model <- c(data$N_model,  rep(N_mod+1,N))

  data <- data.frame("V_phi"=V_phi,
                     "V_newy"=V_newy,
                     "V_model"=V_model,
                     "N_model"=N_model)
  return(data)
  }



Delta_atrac <-  function(df2 ){
  # per ara sols fet per df_faces
  #df2 <- df_faces
  V <- unique(df2$imageTag)
  D_attrac_o <- rep(0,  length(V))
  D_Int_o    <- rep(0,  length(V))
  D_attrac_b <- rep(0,  length(V))
  D_Int_b    <- rep(0,  length(V))
  Vgender  <- c()
  Vage  <- c()

  Vfemininine_o <- rep(0,  length(V))
  Vsociability_o <- rep(0,  length(V))
  Vfemininine_b <- rep(0,  length(V))
  Vsociability_b <- rep(0,  length(V))

  for (i in 1:length(V)) {

    index <- which(df2$imageTag==V[i])

    if (df2$beautification[index[1]]=="beautified") {
      D_Int_b[i] <- df2$intelligence[index[1]]
      D_attrac_b[i] <-df2$perceived_attractiveness[index[1]]

      Vfemininine_b[i]   <- df2$feminine[index[1]]
      Vsociability_b[i]<- df2$sociability[index[1]]

      D_Int_o[i]       <- df2$intelligence[index[2]]
      D_attrac_o[i]    <- df2$perceived_attractiveness[index[2]]
      Vfemininine_o[i]   <- df2$feminine[index[2]]
      Vsociability_o[i]<- df2$sociability[index[2]]

    }else{

      D_Int_b[i]        <- df2$intelligence[index[2]]
      D_attrac_b[i]     <- df2$perceived_attractiveness[index[2]]
      Vfemininine_b[i]    <- df2$feminine[index[2]]
      Vsociability_b[i] <- df2$sociability[index[2]]

      D_Int_o[i]       <- df2$intelligence[index[1]]
      D_attrac_o[i]    <- df2$perceived_attractiveness[index[1]]
      Vfemininine_o[i]   <- df2$feminine[index[1]]
      Vsociability_o[i]<- df2$sociability[index[1]]
    }


    Vgender <- c(Vgender,df2$imageGender[index[1]])
    Vage <- c(Vage,df2$condition[index[1]])

  }

  Delta_atrac <- D_attrac_b-D_attrac_o
  Delta_Int <- D_Int_b-D_Int_o
  D_attrac_o2 <- D_attrac_o

  D_attrac_o[D_attrac_o <=3]=1
  D_attrac_o[(D_attrac_o>2 & D_attrac_o <=4.5)]=2
  D_attrac_o[D_attrac_o >=5]=3

  df_faces_delta <- data.frame("Delta_atrac"=Delta_atrac,"D_attrac_o"=D_attrac_o2,"D_attrac_o_rank"=D_attrac_o,
                               "Delta_Int"=Delta_Int,"D_Int_o"=D_Int_o,
                               "Vage"=Vage, "Vgender"=Vgender,
                               "Vfeminine_o"=Vfeminine_o,"Vfeminine_b"=Vfeminine_b,
                               "Vsociability_o"=Vsociability_o,"Vsociability_b"=Vsociability_b)
  return(df_faces_delta)
}

# categarizar en 3 clases
Cat3_atrac <- function(df){

  df2 <- df
  df2$perceived_attractiveness[df2$perceived_attractiveness <=3]=1
  df2$perceived_attractiveness[(df2$perceived_attractiveness>2 & df2$perceived_attractiveness <=4.5)]=2
  df2$perceived_attractiveness[df2$perceived_attractiveness >=5]=3

  return(df2)
}


Cat3_int <- function(df){
  df2 <- df
  df2$intelligence[df2$intelligence <=3.5]=1
  df2$intelligence[(df2$intelligence>2 & df2$intelligence <=4.5)]=2
  df2$intelligence[df2$intelligence >=5]=3
  return(df2)
}
