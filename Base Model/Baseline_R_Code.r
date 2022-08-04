setwd("C:/Users/sprab/OneDrive/Documents/Study/University/AP/Presentation 2/Base")

library(apollo)
apollo_initialise()

database <- read.csv("All_Data.csv")


apollo_control=list(modelName  ="BaseModel",
                    modelDescr ="Simple Driver Choice with Time of Impact as an interaction term",
                    indivID    ="Driver_tag",
                    workInLogs = TRUE,
                    outputDirectory = "output"
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
              pos_rlc = 0
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
  V[["llc"]]  = asc_llc + time_llc * time_to_impact + dist_llc * d_ahead + speed_llc * log(GPS) + nodv_llc * No_Of_Veh + pos_llc * X_pos
  V[["rlc"]]  = asc_rlc + time_rlc * time_to_impact + dist_rlc * d_ahead + speed_rlc * log(GPS) + nodv_rlc * No_Of_Veh + pos_rlc * X_pos
  V[["acc"]]  = asc_acc + time_acc * time_to_impact + dist_acc * d_ahead + speed_acc * log(GPS) + nodv_acc * No_Of_Veh + pos_acc * X_pos
  V[["brk"]]  = asc_brk + time_brk * time_to_impact + dist_brk * d_ahead + speed_brk * log(GPS) + nodv_brk * No_Of_Veh + pos_brk * X_pos
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


Base.0 = apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(Base.0)
apollo_saveOutput(Base.0)

Base.0$LL0
Base.0$LLout

hist(Base.0$avgCP)

head(Base.0$avgCP,13)


forecast <- apollo_prediction(Base.0, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=30))
hist(forecast$at_estimates$chosen)
write.csv(forecast,'C:/Users/sprab/OneDrive/Documents/Study/University/AP/Presentation 2/Base/forecast.csv',row.names = FALSE)
