#######################################
########## [1] Preparation ############
#######################################
# external data:
# 1. code-books-200721.xlsx
# 2. ATCcode_mapping.xlsx

library(readxl)
# library(dplyr)
library(data.table)

dir.create("./resultsCNLLS/", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency2", showWarnings=FALSE)
dir.create("./resultsCNLLS/model", showWarnings=FALSE)

#######################################
########## [2] R data load ############
#######################################

load("./Rdata0817.RData")

#######################################
########## [2] R data load ############
#######################################

# covid_diag_agg
covid_diag_agg = covid_diag[,-excl.cols]
colnames(covid_diag_agg)[colnames(covid_diag_agg) == "Y"] = "Confirm"
covid_diag_agg <- data.frame(covid_diag_agg)

# covid_outcomes_agg
if (ncol(covid_outcomes[ ,-excl.cols]) != nrow(code_outcomes_df)) stop("Code names do not match!!!")
vec_or = function(x) {as.numeric(sum(x) > 0)}
covid_outcomes_agg = t( apply(covid_outcomes[ ,-excl.cols], 1, function(x) tapply(x, code_outcomes_df$name, vec_or) ) )
covid_outcomes_agg <- data.frame(covid_outcomes_agg)

# 각 스터디에 대해서
make_df <- function(como_df, ATC_df, study) {
  # como_info
  como_df2 = como_df[ ,-excl.cols]
  #head(como_df2)
  if (ncol(como_df2) != nrow(code_ICD10_df)) stop("[1] Code names do not match!!!")
  #print("ok")
  vec_or = function(x) { as.numeric(sum(x) > 0) }
  
  como_df_agg = t( apply(como_df2, 1, function(x) tapply(x, code_ICD10_df$name, vec_or) ) )
  como_df_agg <- data.frame(como_df_agg)
  
  # ATC_med
  ATC_df2 = ATC_df[ ,-excl.cols]
  code_ATC_df_tmp = code_ATC_df[code_ATC_df$uniqueCode %in% colnames(ATC_df2), ]
  
  if (ncol(ATC_df2) != nrow(code_ATC_df_tmp)) stop("[2] Code names do not match!!!")
  vec_or = function(x) {as.numeric(sum(x) > 0)}

  ATC_df_agg = t( apply(ATC_df2, 1, function(x) tapply(x, code_ATC_df_tmp$name, vec_or) ) )
  ATC_df_agg <- data.frame(ATC_df_agg)
  
  x <- cbind(covid_diag_agg, como_df_agg, ATC_df_agg)
  y <- covid_outcomes_agg
  
  asso.mat = matrix(0, nrow=ncol(x), ncol=ncol(y))
  
  rownames(asso.mat) <- colnames(x)
  colnames(asso.mat) <- colnames(y)
  
  #print("ok")
  for (i in 1:ncol(x)) {
    for (j in 1:ncol(y)) {
      myx = factor(x[,i], levels=c(0,1))
      myy = factor(y[,j], levels=c(0,1))
      res = chisq.test( xtabs( ~ myx + myy) )$p.value
      asso.mat[i,j] = res
    }
  }
  
  cat(sprintf(" (1) [save] %s frequency \n", study))
  #print(colMeans(x))
  write.csv(colSums(cbind(x,y)), sprintf("./resultsCNLLS/frequency2/name_freq_%s.csv", study), col.names=T, row.names=T)
  
  cat(sprintf(" (2) [save] %s frequency (per) \n", study))
  #print(colMeans(y))
  write.csv(round(colMeans(cbind(x,y)),6), sprintf("./resultsCNLLS/frequency2/name_freq_per_%s.csv", study), col.names=T, row.names=T)
  
  cat(sprintf(" (3) [save] %s marginal association \n", study))
  write.csv(round(asso.mat,6), sprintf("./resultsCNLLS/asso_%s.csv", study), col.names=T, row.names=T )
  
  return (data.frame(cbind(como_df[ ,excl.cols],x,y)))
}


# como_df = como_info_statin30; ATC_df = ATC_med_statin30; study = "statin30"
cat(" [start] data for each study \n")
cat("\n")
# [1] statin 30
print(" <-------------- statin 30 --------------> \n")
statin30 <- make_df(como_df = como_info_statin30, 
                          ATC_df = ATC_med_statin30,
                          study = "statin30")


# [2] statin 120
print(" <-------------- statin 120 --------------> \n")
statin120 <- make_df(como_df = como_info_statin120, 
                           ATC_df = ATC_med_statin120,
                           study = "statin120")

# [3] anti 30
print(" <-------------- anti 30 --------------> \n")
anti30 <- make_df(como_df = como_info_anti30, 
                        ATC_df = ATC_med_anti30,
                        study = "anti30")

# [4] anti 120
print(" <-------------- anti 120 --------------> \n")
anti120 <- make_df(como_df = como_info_anti120, 
                         ATC_df = ATC_med_anti120,
                         study = "anti120")

# [5] immune 30
print(" <-------------- immune 30 --------------> \n")
immune30 <- make_df(como_df = como_info_immune30, 
                          ATC_df = ATC_med_immune30,
                          study = "immune30")

# [6] immune 120
print(" <-------------- immune 120 --------------> \n")
immune120 <- make_df(como_df = como_info_immune120, 
                           ATC_df = ATC_med_immune120,
                           study = "immune120")

statin30 <- statin30[statin30$JID %in% age40JID, ]
statin120 <- statin120[statin120$JID %in% age40JID, ]

statin30$Lipid.lowering.agents.excluding.statin <- statin30$Lipid.lowering.agents.including.statin - statin30$Lipid.lowering.agents.including.statin * statin30$statin
statin120$Lipid.lowering.agents.excluding.statin <- statin120$Lipid.lowering.agents.including.statin - statin120$Lipid.lowering.agents.including.statin * statin120$statin

statin30$first_covid <- NULL
statin120$first_covid <- NULL

anti30$first_covid <- NULL
anti120$first_covid <- NULL
anti30$statin <- NULL
anti120$statin <- NULL

immune30$first_covid <- NULL
immune120$first_covid <- NULL
immune30$statin <- NULL
immune120$statin <- NULL

cat(" data for each study R data save \n")
save.image(file = "./resultsCNLLS/Rdata0821.RData")

####################################################################
#####################        Modeling        #######################
####################################################################
# statin variable
statin.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$statin.study == "exposure.and.outcome.related",
                                            c("name", "statin.study")])$name,
                       unique(code_ATC_df[code_ATC_df$statin.study == "exposure.and.outcome.related",
                                          c("name", "statin.study")])$name)

statin.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$statin.study == "outcome.related",
                                         c("name", "statin.study")])$name,
                    unique(code_ATC_df[code_ATC_df$statin.study == "outcome.related",
                                       c("name", "statin.study")])$name)

statin.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", statin.Ex.Out.Rel), gsub("[-,/ ]", ".", statin.Out.Rel), "Lipid.lowering.agents.excluding.statin")

statin.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")
statin.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")

statin30$outcome1 <- ifelse(rowSums(statin30[,statin.outcome1])>0,1,0)
statin120$outcome1 <-  ifelse(rowSums(statin120[,statin.outcome1])>0,1,0)


#  “TIA”, “Stroke”, “Coronary artery disease ”, “Atherosclerosis”, “Peripheral vascular disease”
statin30$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin30$TIA + statin30$Stroke + statin30$Coronary.artery.disease + statin30$Atherosclerosis + statin30$Peripheral.vascular.disease>0,1,0)

statin120$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin120$TIA + statin120$Stroke + statin120$Coronary.artery.disease + statin120$Atherosclerosis + statin120$Peripheral.vascular.disease>0,1,0)

statin.Emodifier <- c("AGE", "GENDER", "Hyperlipidemia", "Hypertension", "Diabetes.mellitus", "TIA.Stroke.Coronary.Atherosclerosis.Peripheral")

statin.exposure <- "statin"

statin.study <- rbind(data.frame(var = statin.exposure, type = "Exposure"),
                      data.frame(var = statin.confounder, type = rep("Confounder", length(statin.confounder))),
                      data.frame(var = statin.outcome1, type = rep("Outcome1", length(statin.outcome1))),
                      data.frame(var = statin.outcome2, type = rep("Outcome2", length(statin.outcome2))), 
                      data.frame(var = statin.Emodifier, type = rep("Effect.Modifier",length(statin.Emodifier))))

# anti variable
anti.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.anticoagulant.study == "exposure.and.outcome.related",
                                          c("name", "antiplatelet.anticoagulant.study")])$name,
                     unique(code_ATC_df[code_ATC_df$antiplatelet.anticoagulant.study == "exposure.and.outcome.related",
                                        c("name", "antiplatelet.anticoagulant.study")])$name)

anti.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.anticoagulant.study == "outcome.related",
                                       c("name", "antiplatelet.anticoagulant.study")])$name,
                  unique(code_ATC_df[code_ATC_df$antiplatelet.anticoagulant.study == "outcome.related",
                                     c("name", "antiplatelet.anticoagulant.study")])$name)

anti.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", anti.Ex.Out.Rel), gsub("[-,/ ]", ".", anti.Out.Rel))
anti.confounder <- setdiff(anti.confounder, "statin")

anti.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Thromboembolism", 
                   "ischemic.stroke.and.TIA", "TIA.transient.ischemic.attack.", "Stroke", "hemorragic.stroke")

# - outcome1 - 
# "myocardial infarction,  ischemic stroke and TIA", "Thromboembolism"
# "bleeding", "transfusion"
anti30$myocardial.storke.TIA.Thromboembolism <- ifelse(anti30$myocardial.infarction.ischemic.stroke.and.TIA + anti30$Thromboembolism>0,1,0)
anti30$bleeding.transfusion <- ifelse(anti30$bleeding + anti30$transfusion>0,1,0)

anti120$myocardial.storke.TIA.Thromboembolism <- ifelse(anti120$myocardial.infarction.ischemic.stroke.and.TIA + anti120$Thromboembolism>0,1,0)
anti120$bleeding.transfusion <- ifelse(anti120$bleeding + anti120$transfusion>0,1,0)

anti.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion")

anti30$outcome1 <- ifelse(rowSums(anti30[,anti.outcome1])>0,1,0)
anti120$outcome1 <- ifelse(rowSums(anti120[,anti.outcome1])>0,1,0)

#  “TIA”, “Stroke”, “Coronary artery disease ”, “Peripheral vascular disease”
#  “Atrial Fibrillation”, “Thromboembolism” 
anti30$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(anti30$TIA + anti30$Stroke + anti30$Coronary.artery.disease + anti30$Peripheral.vascular.disease>0,1,0)
anti30$Atrial.Thromboembolism <- ifelse(anti30$Atrial.fibrillation + anti30$Thromboembolism>0,1,0)

anti120$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(anti120$TIA + anti120$Stroke + anti120$Coronary.artery.disease + anti120$Peripheral.vascular.disease>0,1,0)
anti120$Atrial.Thromboembolism <- ifelse(anti120$Atrial.fibrillation + anti120$Thromboembolism>0,1,0)

anti.Emodifier <- c("AGE", "GENDER", "Atrial.Thromboembolism", "TIA.Stroke.Coronary.Peripheral")
anti.exposure <- "Anticoagulants.antiplatelets"

anti.study <- rbind(data.frame(var = anti.exposure, type = "Exposure"),
                    data.frame(var = anti.confounder, type = rep("Confounder", length(anti.confounder))),
                    data.frame(var = anti.outcome1, type = rep("Outcome1", length(anti.outcome1))),
                    data.frame(var = anti.outcome2, type = rep("Outcome2", length(anti.outcome2))), 
                    data.frame(var = anti.Emodifier, type = rep("Effect.Modifier",length(anti.Emodifier))))

# immune variable
immune.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "exposure.and.outcome.related",
                                            c("name", "immune.suppressant.study")])$name,
                       unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "exposure.and.outcome.related",
                                          c("name", "immune.suppressant.study")])$name)
immune.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "outcome.related",
                                         c("name", "immune.suppressant.study")])$name,
                    unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "outcome.related",
                                       c("name", "immune.suppressant.study")])$name)

immune.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", immune.Ex.Out.Rel), gsub("[-,/ ]", ".", immune.Out.Rel))
immune.confounder <- setdiff(immune.confounder, "statin")

immune.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immune.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immune30$outcome1 <- ifelse(rowSums(immune30[,immune.outcome1])>0,1,0)
immune120$outcome1 <- ifelse(rowSums(immune120[,immune.outcome1])>0,1,0)

immune.Emodifier <- c("AGE", "GENDER", "Malignancy", "autoimmune.disease")

immune.exposure <- "immunosuppressant"

immune.study <- rbind(data.frame(var = immune.exposure, type = "Exposure"),
                      data.frame(var = immune.confounder, type = rep("Confounder", length(immune.confounder))),
                      data.frame(var = immune.outcome1, type = rep("Outcome1", length(immune.outcome1))),
                      data.frame(var = immune.outcome2, type = rep("Outcome2", length(immune.outcome2))), 
                      data.frame(var = immune.Emodifier, type = rep("Effect.Modifier",length(immune.Emodifier))))

cat("\n -- modeling -- \n")

CI.modeling <- function(df, study, name){
  save_path <- sprintf("./resultsCNLLS/model/coef_%s/", name)
  dir.create(save_path, showWarnings=FALSE)
  
  print(study[,c(2,1)])
  study.exposure <- study[study$type == "Exposure","var"]
  study.confounder <- study[study$type == "Confounder","var"]
  study.Effect.Modifier <- study[study$type == "Effect.Modifier","var"]
  study.second <- study[study$type == "Outcome2","var"]
  
  cat("=============================================\n")
  cat("=============================================\n")
  cat("            primary outcome\n\n")
  cat("=============================================\n")
  cat("=============================================\n")
  
  # [0-1] outcome1 ~ exposure (NAIVE)
  fit0.1 <- glm(df[,"outcome1"]~df[,study.exposure], data=df, family="binomial")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-1] Naive Primary Outcome Model \n\n")
  print(summary(fit0.1))
   
  Coef_0.1 <- data.frame("coef" = summary(fit0.1)$coef[,1], # 계수  
                         "s.e" = summary(fit0.1)$coef[,2], # 표준 오차
                         "lower.ci" = summary(fit0.1)$coef[,1] - 1.96 * summary(fit0.1)$coef[,2], # CI
                         "upper.ci" = summary(fit0.1)$coef[,1] + 1.96 * summary(fit0.1)$coef[,2])
  write.csv(Coef_0.1, paste0(save_path, "primary_Naive.csv"), col.names=T, row.names=T)
  
  # [0-2] Naive Multivariate model  outcome1 ~ exposure + confounder : wt (X)
  fit0.2 <- glm(df[,"outcome1"]~., data=df[,c(study.exposure, study.confounder)], family="binomial")
  pred0.2 <- predict(fit0.2, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-2] Naive Multivariate Primary Outcome Model \n\n")
  print(summary(fit0.2))
  
  Coef_0.2 <- data.frame("coef" = summary(fit0.2)$coef[,1], # 계수  
                         "s.e" = summary(fit0.2)$coef[,2], # 표준 오차
                         "lower.ci" = summary(fit0.2)$coef[,1] - 1.96 * summary(fit0.2)$coef[,2], # CI
                         "upper.ci" = summary(fit0.2)$coef[,1] + 1.96 * summary(fit0.2)$coef[,2])
  write.csv(Coef_0.2, paste0(save_path, "primary_MultiNaive.csv"), col.names=T, row.names=T)
  
  # [1] propensity score
  #   (a) exposure ~ confounders
  fit1a<- glm(df[,study.exposure] ~ ., data = df[,study.confounder], family="binomial")
  df$ps1a <- predict(fit1a, type="response")
  df$wt1a <- df$ps1a*df[,study.exposure] + (1-df$ps1a)*(1-df[,study.exposure])
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [1] Propensity Score Model (a)\n\n")
  cat(" - propensity score dist\n\n")
  print(summary(df$ps1a))
  cat("\n\n")
  cat(" - weight dist\n\n")
  print(summary(df$wt1a))
  cat("\n\n")
  cat(" - fit1a model summary \n\n")
  print(summary(fit1a))
  cat("\n\n")
  
  Coef_1a <- data.frame("coef" = summary(fit1a)$coef[,1], # 계수  
                         "s.e" = summary(fit1a)$coef[,2], # 표준 오차
                         "lower.ci" = summary(fit1a)$coef[,1] - 1.96 * summary(fit1a)$coef[,2], # CI
                         "upper.ci" = summary(fit1a)$coef[,1] + 1.96 * summary(fit1a)$coef[,2])
  write.csv(Coef_1a, paste0(save_path, "primary_PS.csv"), col.names=T, row.names=T)
  
  # [2] outcome model (main)
  #   (a) outcome1 ~ exposure (wt 1a)
  fit2a <- glm(df[,"outcome1"]~df[,study.exposure], data=df, weight = 1/df$wt1a, family="binomial")
  pred2a <- predict(fit2a, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [2] Primary Outcome Model (a)\n\n")
  print(summary(fit2a))
  cat("\n\n")
  
  Coef_2a <- data.frame("coef" = summary(fit2a)$coef[,1], # 계수  
                         "s.e" = summary(fit2a)$coef[,2], # 표준 오차
                         "lower.ci" = summary(fit2a)$coef[,1] - 1.96 * summary(fit2a)$coef[,2], # CI
                         "upper.ci" = summary(fit2a)$coef[,1] + 1.96 * summary(fit2a)$coef[,2])
  write.csv(Coef_2a, paste0(save_path, "primary_IPTW.csv"), col.names=T, row.names=T)
  
  # [3] outcome ~ exposure + effect modifier 
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  
  for (v in study.Effect.Modifier){
    cat(sprintf(" [3] Effect Modifier Primary Outcome Model : %s \n\n", v))
    df[,paste0(study.exposure,"*",v)] <- df[,study.exposure] * df[,v]
    
    # outcome1 ~ exposure + v + exposure*v (wt1a)
    fit3_v <- glm(df[,"outcome1"]~., data=df[,c(study.exposure, v, paste0(study.exposure,"*",v))], weight = 1/df$wt1a, family="binomial")
    pred3_v <-  predict(fit3_v, type="response")
    
    print(summary(fit3_v))
    
    Coef_3_v <- data.frame("coef" = summary(fit3_v)$coef[,1], # 계수  
                          "s.e" = summary(fit3_v)$coef[,2], # 표준 오차
                          "lower.ci" = summary(fit3_v)$coef[,1] - 1.96 * summary(fit3_v)$coef[,2], # CI
                          "upper.ci" = summary(fit3_v)$coef[,1] + 1.96 * summary(fit3_v)$coef[,2])
    
    E.mod <- sprintf("primary_EffectModifier_%s.csv", v)
    
    write.csv(Coef_3_v, paste0(save_path, E.mod), col.names=T, row.names=T)
    
    cat("\n\n")
  }

  cat("=============================================\n")
  cat("=============================================\n")
  cat("            secondary outcome\n\n")
  cat("=============================================\n")
  cat("=============================================\n")
  
  m <- 1
  for (outcome2 in study.second) {
    # study confounder 초기화
    study.confounder <- study[study$type == "Confounder","var"]
    study.scr <- study[study$type == "Confounder","var"]
    
    #MI, TIA, stroke, or thromboembolism - 생기는 사람이 소수임
    #Bleeding(except skin bleeding) or transfusion - primary에 포함X
    if (outcome2 %in% c("myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion")){ 
      cat("============================================================\n")
      cat("============================================================\n")
      cat(sprintf("           %d. ", m));cat(outcome2);cat("\n\n")
      m <- m+1
      
      for (c in study.confounder[4:length(study.confounder)]){
        study.scr <- c("AGE", "GENDER", "MedInfo")
        # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
        fit.scr <- glm(df[,outcome2]~df[,c], data=df, family = "binomial")
        if ( length(summary(fit.scr)$coef) == 4) {
          print("ERROR!!, exposure coefficeints is NA.")
          next
        }
        if (summary(fit.scr)$coef[2,4] < 0.2){
          study.scr <- c(study.scr, confounder)
        }
      }
      cat("\n [Confounder after p-value Screening(0.2)] \n")
      print(study[study$var %in% study.scr, c(2,1)])
      
      study.confounder <- study.scr
      
      # [1] ps model for screening variable
      # (a) exposure ~ confounders
      fit1a<- glm(df[,study.exposure] ~ ., data = df[,study.confounder], family="binomial")
      df$ps1a <- predict(fit1a, type="response")
      df$wt1a <- df$ps1a*df[,study.exposure] + (1-df$ps1a)*(1-df[,study.exposure])
      
      cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
      cat(" [1] Propensity Score Model - new (a)\n\n")
      cat(" - propensity score dist\n\n")
      print(summary(df$ps1a))
      cat("\n\n")
      cat(" - weight dist\n\n")
      print(summary(df$wt1a))
      cat("\n\n")
      cat(" - model summary \n\n")
      print(summary(fit1a))
      cat("\n\n")
      
      secondary_PS <- sprintf("Secondary_PS_%s.csv", outcome2)
      Coef_1a <- data.frame("coef" = summary(fit1a)$coef[,1], # 계수  
                             "s.e" = summary(fit1a)$coef[,2], # 표준 오차
                             "lower.ci" = summary(fit1a)$coef[,1] - 1.96 * summary(fit1a)$coef[,2], # CI
                             "upper.ci" = summary(fit1a)$coef[,1] + 1.96 * summary(fit1a)$coef[,2])
      write.csv(Coef_1a, paste0(save_path, secondary_PS), col.names=T, row.names=T)
    }
    
    # [0-1] Naive model  outcome2 ~ exposure : wt (X)
    fit0.1 <- glm(df[,outcome2]~df[,study.exposure], data=df, family="binomial")
    pred0.1 <- predict(fit0.1, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [0-1] Naive Secondary Outcome Model \n\n")
    print(summary(fit0.1))
    
    Coef_0.1 <- data.frame("coef" = summary(fit0.1)$coef[,1], # 계수  
                           "s.e" = summary(fit0.1)$coef[,2], # 표준 오차
                           "lower.ci" = summary(fit0.1)$coef[,1] - 1.96 * summary(fit0.1)$coef[,2], # CI
                           "upper.ci" = summary(fit0.1)$coef[,1] + 1.96 * summary(fit0.1)$coef[,2])
    write.csv(Coef_0.1, paste0(save_path, "secondary_Naive.csv"), col.names=T, row.names=T)
    
    # [0-2] Naive model  outcome2 ~ exposure + confounder : wt (X)
    fit0.2 <- glm(df[,outcome2]~., data=df[,c(study.exposure, study.confounder)], family="binomial")
    pred0.2 <- predict(fit0.2, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [0-2] Naive Multivariate Secondary Outcome Model \n\n")
    print(summary(fit0.2))
    
    Coef_0.2 <- data.frame("coef" = summary(fit0.2)$coef[,1], # 계수  
                           "s.e" = summary(fit0.2)$coef[,2], # 표준 오차
                           "lower.ci" = summary(fit0.2)$coef[,1] - 1.96 * summary(fit0.2)$coef[,2], # CI
                           "upper.ci" = summary(fit0.2)$coef[,1] + 1.96 * summary(fit0.2)$coef[,2])
    write.csv(Coef_0.2, paste0(save_path, "secondary_MultiNaive.csv"), col.names=T, row.names=T)
    
    # [2] outcome model (main)
    #   (a) outcome2 ~ exposure (wt1a)
    fit2a <- glm(df[,outcome2]~df[,study.exposure], data=df, weight = 1/df$wt1a, family="binomial")
    pred2a <- predict(fit2a, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [2-a] Secondary Outcome Model\n\n")
    print(summary(fit2a))
    cat("\n\n")
    
    Coef_2a <- data.frame("coef" = summary(fit2a)$coef[,1], # 계수  
                           "s.e" = summary(fit2a)$coef[,2], # 표준 오차
                           "lower.ci" = summary(fit2a)$coef[,1] - 1.96 * summary(fit2a)$coef[,2], # CI
                           "upper.ci" = summary(fit2a)$coef[,1] + 1.96 * summary(fit2a)$coef[,2])
    write.csv(Coef_2a, paste0(save_path, "secondary_IPTW.csv"), col.names=T, row.names=T)
    
  }
}

# statin 30
sink("./resultsCNLLS/model/results_statin30.txt")
cat("\n <--------- [statin 30] Result (primary) ---------> \n")
CI.modeling(df = statin30, study=statin.study[order(statin.study$type, statin.study$var),], name="statin30")
sink()

# statin 120
sink("./resultsCNLLS/model/results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
CI.modeling(df = statin120, study=statin.study[order(statin.study$type, statin.study$var),], name="statin120")
sink()


# anti 30
sink("./resultsCNLLS/model/results_anti30.txt")
cat("\n <--------- [anti 30] Result (primary) ---------> \n")
CI.modeling(df = anti30, study=anti.study[order(anti.study$type, anti.study$var),], name="anti30")
sink()

# anti 120
sink("./resultsCNLLS/model/results_anti120.txt")
cat("\n <--------- [anti 120] Result (primary) ---------> \n")
CI.modeling(df = anti120, study=anti.study[order(anti.study$type, anti.study$var),], name="anti120")
sink()

# immune 30
sink("./resultsCNLLS/model/results_immune30.txt")
cat("\n <--------- [immune 30] Result (primary) ---------> \n")
CI.modeling(df = immune30, study=immune.study[order(immune.study$type, immune.study$var),], name="immune30")
sink()

# immune 120
sink("./resultsCNLLS/model/results_immune120.txt")
cat("\n <--------- [immune 120] Result (primary) ---------> \n")
CI.modeling(df = immune120, study=immune.study[order(immune.study$type, immune.study$var),], name="immune120")
sink()

save.image(file = "./resultsCNLLS/Rdata0817.RData")
