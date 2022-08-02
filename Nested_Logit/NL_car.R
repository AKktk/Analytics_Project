setwd("C:/DDS/Semester2/AnalyticsProject/Git/PPT_Final/AnalyticsProject/Nested_Logit/")
library(apollo)
apollo_initialise()
database <- read.csv("DynamicMarkov.csv",header = TRUE)

apollo_control=list(modelName  ="NL_2_Level",
                    modelDescr ="Nested Logit Simple",
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
              lambda_1=1,
              lambda_2=1
)

apollo_fixed = c()
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  V = list()
  time_to_impact = (Dist_Veh/(GPS*0.28)) * Vehicle_Flag
  
  d_ahead = Dist_Veh * Vehicle_Flag
  V[["llc"]]  = asc_llc + time_llc * time_to_impact + dist_llc * d_ahead + speed_llc * log(GPS) + nodv_llc * No_Of_Veh + pos_llc * X_pos  + oldAge_llc * Age
  V[["rlc"]]  = asc_rlc + time_rlc * time_to_impact + dist_rlc * d_ahead + speed_rlc * log(GPS) + nodv_rlc * No_Of_Veh + pos_rlc * X_pos  + oldAge_rlc * Age
  V[["acc"]]  = asc_acc + time_acc * time_to_impact + dist_acc * d_ahead + speed_acc * log(GPS) + nodv_acc * No_Of_Veh + pos_acc * X_pos  + oldAge_acc * Age
  V[["brk"]]  = asc_brk + time_brk * time_to_impact + dist_brk * d_ahead + speed_brk * log(GPS) + nodv_brk * No_Of_Veh + pos_brk * X_pos  + oldAge_brk * Age
  V[["nor"]]  = 0   

  nlNests      = list(root=1, n1=lambda_1, n2=lambda_2)

  nlStructure= list()
  nlStructure[["root"]]   = c("nor","n1","n2")
  nlStructure[["n1"]]     = c("acc","brk")
  nlStructure[["n2"]]     = c("llc","rlc")

  nl_settings <- list(
    alternatives = c(nor=0, acc=1, brk=2, llc=3, rlc=4),
    avail        = list(nor=norm_avail, acc=accr_avail, brk=brek_avail,llc=llc_avail, rlc=rlc_avail),
    choiceVar    = Target,
    utilities    = V,
    nlNests      = nlNests,
    nlStructure  = nlStructure
  )
  P[["model"]] = apollo_nl(nl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)

  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

Base_NL = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

                              
apollo_modelOutput(Base_NL)
apollo_saveOutput(Base_NL)

Base_NL$LL0
Base_NL$LLout

hist(Base_NL$avgCP)

head(Base_NL$avgCP,13)


forecast <- apollo_prediction(Base_NL, apollo_probabilities, apollo_inputs, prediction_settings = list(runs=20))
hist(forecast$at_estimates$chosen)