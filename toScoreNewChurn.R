rm(list=ls())
################## required packages################################
print("Loading Libraries")
require(RJDBC)
library(e1071)
require("ROCR")
library(rpart)


################ Get Data ####################################
print("Establishing Connections")
CLASSPATH <- "C:\\Users\\aai8260\\Desktop\\JAVA\\ojdbc6.jar"
driver <- JDBC("oracle.jdbc.driver.OracleDriver",classPath= CLASSPATH," ")
UID = "AAI8260"

#APPEX
URL = "jdbc:oracle:thin:@a05129.sys.ds.wolseley.com:1550:APXPRD00"
PASS = "J321C#i"
con <- dbConnect(driver,URL,UID,PASS)

#DWFEI
URL2 = "jdbc:oracle:thin:@nnldwp01.sys.ds.wolseley.com:1521:DWFEI"
PASS2 = "PA55W0RD"
con2 <- dbConnect(driver,URL2,UID,PASS2)



#################read in SQL###############################
print("Getting Historic Data to Score")
#mydata <- dbGetQuery(con,"SELECT * FROM NORTH.X_CHURN_HISTORY_TO_SCORE")
chrunHistToScoreSQL_path <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\churn project\\sql\\churn_1.6_CREATE_TABLE.sql'
chrunHistToScoreSQL <- readChar(chrunHistToScoreSQL_path, file.info(chrunHistToScoreSQL_path)$size)
mydata <- dbGetQuery(con,chrunHistToScoreSQL)
#scoredata <- dbGetQuery(con,"select * from NORTH.X_CHURN_TM")
print("Getting current Data to Score")
CHURN_THISMONTH_ESTIMATE_1.2_path <- 'S:\\eCommerce\\E-Commerce\\Analysis & Reporting\\Omni-Channel\\churn project\\sql\\CHURN_THISMONTH_ESTIMATE_1.2.sql'
CHURN_THISMONTH_ESTIMATE <- readChar(CHURN_THISMONTH_ESTIMATE_1.2_path, file.info(CHURN_THISMONTH_ESTIMATE_1.2_path)$size)
scoredata <- dbGetQuery(con,CHURN_THISMONTH_ESTIMATE)

###################### change necessary fields to factors ##########################
print("Converting Chars to Factors")
for(i in 1:length(mydata)){
  if(length(levels(factor(mydata[,i]))) < 5){mydata[,i] <- factor(mydata[,i])}
  if(class(mydata[,i]) == 'character'){mydata[,i] <- factor(mydata[,i])}
}
for(i in 1:length(scoredata)){
  if(length(levels(factor(scoredata[,i]))) < 5){scoredata[,i] <- factor(scoredata[,i])}
  if(class(scoredata[,i]) == 'character'){scoredata[,i] <- factor(scoredata[,i])}
}



############################ models #################################################
############################# Formula #########################################
frmla <- CUST_CHURN_CAT ~ MON + DOM_TYPE_II + DIVISION_NAME + LCREDIT_LIMIT + OS_HA  + USED_ACCTS_PAYABLE + USED_AVAIL_CHECK + USED_COPY_QUOTE + USED_MY_LIST + USED_OLBP + USED_ORDER_INQUIRE + USED_QUICK_ORDER + USED_REDEEM_PPP + USED_S2S_ORDER + USED_UPLOAD_SPREADSHEET + ACCEPT_PRO_PLUS + HIST_MONTHS_TENURE + HIST_FOL_MON_TENURE + PCT_TENURE_FOL + CNT_ROLLING_MONTHS + LTOT_ROLLING_SLS + LROLLING_FOL_SLS + LROLLING_FOL_LINES + LROLLING_TOT_LINES + LROLLING_TOT_SSES + ROLLING_HV_SSES + ROLLING_SHARE_NON_SEARCH_SSES + ROLLING_MDI + ROLLING_MAN_PCT + ROLLING_JOB_PCT + ROLLING_FOL_SHR_SALES_PCT + ROLLING_NET_BID_PCT + ROLLING_FOL_BID_PCT + ROLLING_FOL_JOB_PCT + ROLLING_FOL_SHR_LINES_PCT + MOVAVG_TOT_SLS_RATIO + MOVAVG_NETJOBSLS_RATIO + MOVAVG_FOL_SLS_RATIO + MOVAVG_NETNONBIDSLS_RATIO + MOVAVG_FOLNONBIDSLS_RATIO + MOVAVG_FOLJOBSLS_RATIO + MOVAVG_FOLLINES_RATIO + MOVAVG_TOTLINES_RATIO + MOVAVG_TOTSSE_RATIO + DLOG_NET_SLS + DLOG_FOL_SLS + DLOG_NET_JOBSLS + DLOG_NET_BIDSLS + DLOG_FOL_BIDSLS + DLOG_FOL_JOBSLS + DLOG_FOL_LINES + DLOG_TOT_LINES + DLOG_TOT_SSES + DLOG_HV_SSES + LLAG1_NET_SLS + LLAG1_FOL_SLS + TOT_MONTH_CONSEC + FOL_MONTH_CONSEC + LLAG_VISITS + LAG_ACCOUNTS_PAYABLE + LAG_OLBP_BILLTRUST + LAG_ORDER_INQUIRY + LAG_REQUEST_QUOTE + LAG_MY_LIST_ADDS + LAG_MY_LIST_VIEWS + LAG_UPLOAD_SPREADSHEET + LAG_QUICK_ORDER + LAG_AVAILABILITY_CHECKS + LAG_REDEEM_PRO_PLUS_POINTS + LAG_COPY_QUOTE + LAG_CHANGE_JOB_ACCOUNT
outcome <- "CUST_CHURN_CAT"

pos <- "1"

######################### mylogit4
print("Scoring Logit")
mylogit4 <- glm(frmla, data = mydata, family = "binomial")
scoredata$MYLOGIT_CHURN_PROB <- predict(mylogit4, scoredata[,which( colnames(scoredata)!=outcome )], type="response")

######################### tree
print("Scoring tree")
tree1 <- rpart (frmla, data=mydata, control = rpart.control (cp=0.001 ,minsplit=500 ,minbucket=200 ,maxdepth=10))
scoredata$TREE_CHURN_PROB <- as.data.frame(predict(tree1, scoredata[,which( colnames(scoredata)!=outcome )]))[,2]

######################### nb
print("Scoring nb")
nb <- naiveBayes(as.formula(frmla), data = mydata, laplace = 3)
scoredata$NB_CHURN_PROB <- predict(nb, scoredata[,which( colnames(scoredata)!=outcome )],type="raw")[,2]

#####################combining table
print("cbinding")
predictedVars <- c("ZKEY","MYLOGIT_CHURN_PROB","TREE_CHURN_PROB","NB_CHURN_PROB")
sdupload <- scoredata[,predictedVars]
sdupload$WA_PREDICT <- exp(-2.870910+ 5.287079*sdupload$MYLOGIT_CHURN_PROB + 0.336200*sdupload$TREE_CHURN_PROB + 0.007973*sdupload$NB_CHURN_PROB)/(1+exp(-2.870910+ 5.287079*sdupload$MYLOGIT_CHURN_PROB + 0.336200*sdupload$TREE_CHURN_PROB + 0.007973*sdupload$NB_CHURN_PROB))
sdupload$LIB_CHURN <- ifelse(sdupload$WA_PREDICT>.5,1,0)
sdupload$CONSERV_CHURN <- ifelse(sdupload$WA_PREDICT>.75,1,0)
sdupload$LAST_UPDATED <- format(Sys.Date(), format="%Y%m%d")

print("write to table")
dbWriteTable(con2, sdupload, name = "XCP_NEW", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"CREATE TABLE XCP_MEDIUM AS 
             SELECT ZKEY, MYLOGIT_CHURN_PROB, TREE_CHURN_PROB, NB_CHURN_PROB, WA_PREDICT, LIB_CHURN, CONSERV_CHURN, 
              SUBSTR(LAST_UPDATED,1,6) AS YM FROM XCP_NEW 
             UNION 
              SELECT ZKEY, MYLOGIT_CHURN_PROB, TREE_CHURN_PROB, NB_CHURN_PROB, WA_PREDICT, LIB_CHURN, CONSERV_CHURN,YM 
              FROM XCP_HIST
             WHERE YM <> to_char(sysdate,'YYYYMM')")
dbSendUpdate(con2,"DROP TABLE XCP_NEW")
dbSendUpdate(con2,"DROP TABLE XCP_HIST")
dbSendUpdate(con2,"ALTER TABLE XCP_MEDIUM RENAME TO XCP_HIST")
dbWriteTable(con2, sdupload, name = "XCP", append=TRUE, row.names=FALSE, overwrite=TRUE)
dbSendUpdate(con2,"grant select on AAI8260.XCP to public")
dbSendUpdate(con2,"grant select on AAI8260.XCP_HIST to public")
dbDisconnect(con)
print("FINISHED")
