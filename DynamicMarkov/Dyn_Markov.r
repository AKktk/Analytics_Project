setwd("C:/DDS/Semester2/AnalyticsProject/Git/PPT_Final/AnalyticsProject/DynamicMarkov/")

library(apollo)
apollo_initialise()

database <- read.csv("DynamicMarkov.csv",header = TRUE)


apollo_control=list(modelName  ="MNLModel",
                    modelDescr ="Dynamic Markov Model with Previous decision and Speed difference",
                    indivID    ="Driver_tag",
                    workInLogs = TRUE,
                    outputDirectory = "Output"
                    #,panelData  = FALSE
)

apollo_beta=c(asc_acc     = 0,
              asc_brk     = 0,
              speed_acc   = 0,
              speed_brk   = 0,
              nodv_acc    = 0,
              nodv_brk    = 0,
              dist_acc    = 0,
              dist_brk    = 0,
              time_acc    = 0,
              time_brk    = 0,
              asc_llc     = 0,
              asc_rlc     = 0,
              oldAge_llc  = 0,
              oldAge_rlc  = 0,
              oldAge_acc  = 0,
              oldAge_brk  = 0,
              speed_llc   = 0,
              speed_rlc   = 0,
              nodv_llc    = 0,
              nodv_rlc    = 0,
              dist_llc    = 0,
              dist_rlc    = 0,
              time_llc    = 0,
              time_rlc    = 0,
              pos_acc	= 0,
              pos_brk = 0,
              pos_llc  = 0,
              pos_rlc = 0,
              diff_sp_llc = 0,
              diff_sp_rlc = 0,
              diff_sp_acc = 0,
              diff_sp_brk = 0,
              blag_llc = 0,
              blag_rlc = 0,
              blag_acc = 0,
              blag_brk = 0
)

#all coefficients are estimated, none is fixed
apollo_fixed = c()

#check if you have defined everything necessary 
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)  
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list() ### Create list of probabilities P
  V = list() ### List of utilities
  
  time_to_impact = (Dist_Veh/(GPS*0.28)) * Vehicle_Flag
  
  d_ahead = Dist_Veh * Vehicle_Flag
  V[["llc"]]  = asc_llc + time_llc * time_to_impact + dist_llc * d_ahead + speed_llc * log(GPS) + nodv_llc * No_Of_Veh + pos_llc * X_pos + oldAge_llc * Age + blag_llc*target_lag + gps_speed_diff*diff_sp_llc
  V[["rlc"]]  = asc_rlc + time_rlc * time_to_impact + dist_rlc * d_ahead + speed_rlc * log(GPS) + nodv_rlc * No_Of_Veh + pos_rlc * X_pos + oldAge_rlc * Age + blag_rlc*target_lag + gps_speed_diff*diff_sp_rlc
  V[["acc"]]  = asc_acc + time_acc * time_to_impact + dist_acc * d_ahead + speed_acc * log(GPS) + nodv_acc * No_Of_Veh + pos_acc * X_pos + oldAge_acc * Age + blag_acc*target_lag + gps_speed_diff*diff_sp_acc
  V[["brk"]]  = asc_brk + time_brk * time_to_impact + dist_brk * d_ahead + speed_brk * log(GPS) + nodv_brk * No_Of_Veh + pos_brk * X_pos + oldAge_brk * Age + blag_brk*target_lag + gps_speed_diff*diff_sp_brk
  V[["nor"]]  = 0 
  
  mnl_settings = list(        
    alternatives = c(nor=0, acc=1, brk=2, llc=3, rlc=4),
    avail        = list(nor=norm_avail, acc=accr_avail, brk=brek_avail,llc=llc_avail, rlc=rlc_avail), 
    choiceVar    = Target,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)  
  return(P)
}


Base.1 = apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(Base.1)
apollo_saveOutput(Base.1)

Base.1$LL0
Base.1$LLout

hist(Base.1$avgCP)

head(Base.1$avgCP,13)


forecast <- apollo_prediction(Base.1, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=20))
hist(forecast$at_estimates$chosen)
write.csv(forecast,'C:/DDS/Semester2/AnalyticsProject/Git/PPT_Final/AnalyticsProject/DynamicMarkov/Output/',row.names = FALSE)
