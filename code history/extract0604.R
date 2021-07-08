
# load raw data (sample data for now)
# load code book 2_JL.xlsx
# load ATC mapping data
# define an empty new data frame to save cleaned data

####### for each row of all worksheets (excluding `general`) in the codebook, ###
####### define a new column of size N by 1  #####################################
# N : sample size
# code each cell as 1 if the value in `Code` exists at his/her `searchFor`, 0 otherwise

# caution 1: uniqueID 251번은 우선 스킵.
# caution 2: if codeType is ATC, it needs one more lookup from the ATC mapping data

########################## END of the for loop ##################################

# merge several columns by `or` opeartions

# final check
#print(colSums(df))

# Corona claim data
data_path <- "./sample_schema.xlsx"
co19_t200 = read_excel(data_path, sheet=4)
co19_t300 = read_excel(data_path, sheet=5)
co19_t400 = read_excel(data_path, sheet=6)
co19_t530 = read_excel(data_path, sheet=7)

# Medical use history data
co19_twjhe200 = read_excel(data_path, sheet=8)
co19_twjhe300 = read_excel(data_path, sheet=9)
co19_twjhe400 = read_excel(data_path, sheet=10)
co19_twjhe530 = read_excel(data_path, sheet=11)


# MID, JID 추가됨
# t200: MAIN_SICK, SUB_SICK
# t300: DIV_CD, GNL_CD
# t400: SICK_CD
# t530: DIV_CD, GNL_CD
co19_t200_trans_dn <- co19_t200[,c("JID", "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t300_trans_dn <- co19_t300[,c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[,c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[,c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[,c("JID", "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK")]
co19_twjhe300_trans_dn <- co19_twjhe300[,c("JID", "MID", "DIV_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[,c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[,c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "."
codebook <- "code-books-200629.xlsx"

code_ICD10 <- read_excel(file.path(codebook_path, codebook), sheet = 2)
code_ATC <- read_excel(file.path(codebook_path, codebook), sheet = 3)
code_outcomes <- read_excel(file.path(codebook_path, codebook), sheet = 4)
code_covid <- read_excel(file.path(codebook_path, codebook), sheet = 5)
ATC_mapping <- read_excel(file.path(codebook_path, "ATCcode_mapping.xlsx"))

code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "code", "codeType", "searchFor")])
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "code", "codeType", "searchFor")])
code_outcomes_df <- data.frame(code_outcomes[,c("uniqueID", "code", "codeType", "searchFor")])
code_covid_df <- data.frame(code_covid[,c("uniqueID", "code", "codeType", "searchFor")])
#colnames(code_covid_df)

ATC_mapping_df <- data.frame(ATC_mapping)[,c("주성분코드", "제품코드", "ATC코드")]
colnames(ATC_mapping_df) <- c("GNL_CD", "DIV_CD", "ATC")

gen_dummies <- function(codeBook){
  final <- NULL
  for (i in 1:nrow(codeBook)) {
    code_val <- codeBook[i,"code"]
    code_len <- nchar(codeBook[i,"code"])
    sF_split1 <- strsplit(codeBook[i,"searchFor"], "; ")
    
    cat(sprintf("Unique ID : %d. [%s]", codeBook[i,"uniqueID"], code_val))
    print(Sys.time())
    
    j = 1; dummy <- NULL
    while(!is.na(sF_split1[[1]][j])){
      sF_split2 <- strsplit(sF_split1[[1]][j], "/")
      
      df_name <- sF_split2[[1]][1]
      var_name <- sF_split2[[1]][2]
      df <- data.frame(get(df_name)) # df 초기화, cbind() 불가
      
      if (codeBook[i,"codeType"] == "ATC") {
        rule = (toupper(substr(ATC_mapping_df[ ,"ATC"],1,code_len)) == toupper(code_val))
        #print(sum(rule))
        
        if (sum(rule)==0){
          cat(" ATC code", i,". [", code_val,"] mapping X ")
          print(" ")
          j = j+1 #while 문 사용해서 +1
          next
        } else {
          mapped_var <- ATC_mapping_df[rule, var_name]
          dummy2 <- NULL
          #each_var = mapped_var
          for (each_var in mapped_var) {
            dummy2_temp <- data.frame("JID" = df$JID, "var" = ifelse(df[,var_name] == each_var, 1, 0))
            dummy2_temp[is.na(dummy2_temp[,"var"]),"var"] <- 0
            dummy2_temp2 <- dummy2_temp %>% group_by(JID) %>% summarise(var = sum(dummy2_temp[,"var"])) %>% data.frame()
            
            colnames(dummy2_temp2) <- c("JID", each_var)
            # dummy2_temp : each_var 에 대한 정보 저장
            if (is.null((dummy2))) {
              dummy2 <- rbind(dummy2, dummy2_temp2)
            } else { 
              dummy2 <- merge(dummy2, dummy2_temp2, by=c("JID"), all=TRUE)
            }
          }
          # dummy_temp2 : searchFor 하나에 대한 정보 저장 (dummy2 의 ifelse(rowSums>-1,1,0) )
          #print("dummy2 생성")
          dummy2$zero <- 0 # mapped_value 가 한 개일 경우 rowSums 이 작동하지 않기 때문에 생성
          dummy_temp2 <- data.frame("JID" = dummy2$JID, "var" = ifelse(rowSums(dummy2[,-1])>0,1,0))
        }
      } else {
        dummy_temp <- data.frame("JID" = df$JID, "var" = ifelse(toupper(substr(df[,var_name],1,code_len)) == toupper(code_val),1,0))
        dummy_temp[is.na(dummy_temp[,"var"]),"var"] <- 0
        dummy_temp2 <- dummy_temp %>% group_by(JID) %>% summarise(var = sum(dummy_temp[,"var"])) %>% data.frame()
      }
      if (j == 1) {
        # [?] 왜 rbind 만 되는지
        dummy <- rbind(dummy, dummy_temp2)
      } else { 
        dummy <- merge(dummy, dummy_temp2, by=c("JID"), all=TRUE)
        #print(nrow(dummy))
      }
      j = j+1
    }
    
    #head(final)
    #print(nrow(final))
    if(is.null(dummy)) next
    #print("final_temp 생성")
    dummy$zero <- 0
    final_temp <- data.frame("JID" = dummy$JID, "var" = ifelse(rowSums(dummy[,-1])>0,1,0))
    colnames(final_temp) <- c("JID", code_val)
    if (i==1) {
      final <- rbind(final, final_temp)
    } else { 
      final <- merge(final, final_temp, by=c("JID"), all=TRUE)
    }
  }
  return (final)
}

# [1] comorbidties ICD10 (sheet2)
phx_info <- gen_dummies(code_ICD10_df)

# [2] outcomes (sheet4)
# [?] outcomes code book 에 중복된 셀 존재
covid_sick <- gen_dummies(code_outcomes_df)

# [3] medications ATC code (sheet2)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_med <- gen_dummies(code_ATC_df)

# [4] COVID19 code (sheet5)
covid_diag <- gen_dummies(code_covid_df)

cat("\n<----------- RESULTS ----------->\n")
cat("\n [1] comorbidties ICD10 (sheet2)")
cat(sprintf("  (Dim) # of JID = %d, code = %d", dim(phx_info)[1], dim(phx_info)[2]))
cat("\n  # of JID for each code ")
print(colSums(phx_info[,-1]))
cat("\n  % of JID for each code ")
print(colSums(phx_info[,-1])/nrow(phx_info[,-1]))
print(phx_info) # 전체 출력 어떻게.

cat("\n [2] medication ATC (sheet3)")
cat(sprintf("  (Dim) # of JID = %d, code = %d", dim(ATC_med)[1], dim(ATC_med)[2]))
cat("\n  # of JID for each code ")
print(colSums(ATC_med[,-1]))
cat("\n  % of JID for each code ")
print(colSums(ATC_med[,-1])/nrow(ATC_med[,-1]))
print(ATC_med) # 전체 출력 어떻게.

cat("\n [3] outcomes (sheet4)")
cat(sprintf("\n  (Dim) # of JID = %d, code = %d", dim(covid_sick)[1], dim(covid_sick)[2]))
cat("\n  # of JID for each code ")
print(colSums(covid_sick[,-1]))
cat("\n  % of JID for each code ")
print(colSums(covid_sick[,-1])/nrow(covid_sick[,-1]))

cat("\n [4] covid (sheet5)")
cat(sprintf("\n  (Dim) # of JID = %d, code = %d", dim(covid_sick)[1], dim(covid_sick)[2]))
cat("\n  # of JID for each code ")
print(colSums(ATC_med[,-1]))
cat("\n  % of JID for each code ")
print(colSums(ATC_med[,-1])/nrow(ATC_med[,-1]))

for (j in 3:ncol(covid_diag)) {
  cat(sprintf(" → Confirm by HIRA (Y) versus code [%s]\n", colnames(covid_diag)[j]))
  print(table(covid_diag[ ,2], "code" = covid_diag[ ,j]))
  cat("\n")
}
 
