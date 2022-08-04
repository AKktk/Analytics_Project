setwd("C:/Users/sprab/OneDrive/Documents/Study/University/AP/Presentation 2/Lane Change Combined")

library(apollo)
apollo_initialise()

database <- read.csv("All_Data.csv")


apollo_control=list(modelName  ="LccModel",
                    modelDescr ="Lane Change combi",
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
              asc_lc     = 0,
        
              speed_lc   = 0,
          
              nodv_lc    = 0,

              dist_lc    = 0,
 
              time_lc    = 0,
    
              pos_acc	= 0,
              pos_brk = 0,
              pos_lc  = 0

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
  V[["lc"]]  = asc_lc + time_lc * time_to_impact + dist_lc * d_ahead + speed_lc * log(GPS) + nodv_lc * No_Of_Veh + pos_lc * X_pos
 
  V[["acc"]]  = asc_acc + time_acc * time_to_impact + dist_acc * d_ahead + speed_acc * log(GPS) + nodv_acc * No_Of_Veh + pos_acc * X_pos
  V[["brk"]]  = asc_brk + time_brk * time_to_impact + dist_brk * d_ahead + speed_brk * log(GPS) + nodv_brk * No_Of_Veh + pos_brk * X_pos
  V[["nor"]]  = 0
  
  mnl_settings = list(        
    alternatives = c(nor=0, acc=1, brk=2, lc=3),
    avail        = list(nor=norm_avail, acc=accr_avail, brk=brek_avail,lc=lc_avail), 
    choiceVar    = Target,          
    V            = V)
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)  
  return(P)
}


Base.3 = apollo_estimate(apollo_beta,
                         apollo_fixed,
                         apollo_probabilities,
                         apollo_inputs)

apollo_modelOutput(Base.3)
apollo_saveOutput(Base.3)

Base.3$LL0
Base.3$LLout

hist(Base.3$avgCP)

head(Base.3$avgCP,13)


forecast <- apollo_prediction(Base.3, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=30))
hist(forecast$at_estimates$chosen,xlab ='Chosen probabilities',main ='Forecast estimates')
write.csv(forecast,'C:/Users/sprab/OneDrive/Documents/Study/University/AP/Presentation 2/Lane Change Combined/forecast.csv',row.names = FALSE)

#BenakivaSwait Test
apollo_basTest(Base.0,Base.3)
