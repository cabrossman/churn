################## required packages################################
require(RJDBC)
library(e1071)
require(ggplot2)
require("ROCR")
library(rpart)
library(data.table)
################ Get Data ####################################
CLASSPATH <- "C:\\Users\\aai8260\\Desktop\\JAVA\\ojdbc6.jar"
driver <- JDBC("oracle.jdbc.driver.OracleDriver",classPath= CLASSPATH," ")
UID = "AAI8260"

#APPEX
URL = "jdbc:oracle:thin:@a01121.sys.ds.wolseley.com:1521:APEXNNP"
PASS = "J321C#i"
con <- dbConnect(driver,URL,UID,PASS)

mydata <- dbGetQuery(con,"SELECT * FROM NORTH.X_CHURN_HISTORY_TO_SCORE")
#mydata <- read.csv("CHURN_DATA.csv")

###################### change necessary fields to factors ##########################
for(i in 3:length(mydata)){
  if(length(levels(factor(mydata[,i]))) < 5){mydata[,i] <- factor(mydata[,i])}
  if(class(mydata[,i]) == 'character'){mydata[,i] <- factor(mydata[,i])}
}

################Split data into training, test and calibration sets##################
mydata$genkey <- runif(dim(mydata)[1])
testset <- subset(mydata, mydata$genkey <=(1/5))
trainset <- subset(mydata, mydata$genkey > (1/5))
useForCal <- rbinom(n=length(trainset),size=1,prob=.1)>0
calset <- subset(trainset,useForCal)
trainset <- subset(trainset,!useForCal)

################ group values ###########################################
vars <-  colnames(trainset[,seq.int(3, length(mydata)-2, 1)])
catVars <- vars[sapply(trainset[,vars],class) %in% c('factor','character')]
numericVars <- vars[sapply(trainset[,vars],class) %in% c('numeric','integer')]
outcome <- "CUST_CHURN_CAT"
pos <- "1"

################ null model log likelihood ###############################
pNull <- sum(ifelse(testset[,outcome]==1,1,0))/length(testset[,outcome])
nll <- sum(ifelse(testset[,outcome]==1,1,0))*log(pNull) + sum(ifelse(testset[,outcome]==1,1,0))*log(1-pNull)
#log likelihood conditional probability example
#sum(ifelse(testset[,outcome]=='CHURN'& testset[,"TOT_MONTH_CONSEC"] == '1CONSEC',#1,0))*log(0.5858159) + sum(ifelse(testset[,outcome]=='1 & testset[,"TOT_MONTH#_CONSEC"] == '1CONSEC',1,0))*log(1-0.5858159)

################ Functions: Cat, Numeric, AUC, insert row ###############################
mkPredC <- function(outCol,varCol,appCol){
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+0.001*pPos)/(colSums(vTab)+0.001)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
  #rm(pPos,naTab,pPosWna,vTab,pPosWv,pred)
}

mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(testset[,v],probs=seq(0, 1, 0.2),na.rm=T)))
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
  #rm(cuts,varC,appC)
}

calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
  #rm(perf)
}
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}
logLikelyhood <- function(outCol,predCol) {
  sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
}
############## Conditional Probabilities########
for(v in catVars){
  pi <- paste('pred',v,sep='')
  trainset[,pi] <- mkPredC(trainset[,outcome],trainset[,v],trainset[,v])
  calset[,pi] <- mkPredC(trainset[,outcome],trainset[,v],calset[,v])
  testset[,pi] <- mkPredC(trainset[,outcome],trainset[,v],testset[,v])
  dftemp <-cbind(unique(testset[,c(v,pi)]) ,round(pNull,4), round(unique(testset[,pi]-pNull),4),factor(unique(testset[,pi]-pNull)>0))
  cat <- colnames(dftemp)[1]
  dftemp[,1] <- factor(dftemp[,1])
  colnames(dftemp)[2] <- "CondProb"
  colnames(dftemp)[3] <- "probNull"
  colnames(dftemp)[4] <- "Diff"
  colnames(dftemp)[5] <- "POSITIVE"
  dftemp[,5] <- factor(dftemp[,5])
  print(dftemp[,1:4]); print(nlevels(dftemp[,1]))
  if(nlevels(dftemp[,1])<3){
    print(ggplot(dftemp, aes(x=dftemp[,1], y=CondProb, fill=POSITIVE)) + geom_bar(stat="identity", color="black", position=position_dodge()) + geom_hline(aes(yintercept=probNull), colour="#990000", linetype="dashed") + xlab(cat))
  }
  else{
    print(ggplot(dftemp, aes(x=dftemp[,1], y=CondProb, fill=POSITIVE)) + geom_bar(stat="identity", color="black", position=position_dodge()) + geom_hline(aes(yintercept=probNull), colour="#990000", linetype="dashed") + coord_flip() + xlab(cat))
  }
  rm(pi,dftemp)
}
#####Num Vars are cut into decilies
for(v in numericVars){
  pi <- paste('pred',v,sep='')
  trainset[,pi] <- mkPredN(trainset[,outcome],trainset[,v],trainset[,v])
  testset[,pi] <- mkPredN(trainset[,outcome],trainset[,v],testset[,v])
  calset[,pi] <- mkPredN(trainset[,outcome],trainset[,v],calset[,v])
  cuts <- unique(as.numeric(quantile(testset[,v],probs=seq(0, 1, 0.2),na.rm=T)))
  dftemp <- cbind.data.frame(pi,round(cuts,4),round(unique(testset[,pi]),4) ,round(pNull,4), round(unique(testset[,pi]-pNull),4),unique(testset[,pi]-pNull)>0)
  cat <- colnames(dftemp)[1]
  dftemp[,1] <- factor(dftemp[,1])
  dftemp[,2] <- factor(dftemp[,2])
  colnames(dftemp)[2] <- "Cut"
  colnames(dftemp)[3] <- "CondProb"
  colnames(dftemp)[4] <- "probNull"
  colnames(dftemp)[5] <- "Diff"
  colnames(dftemp)[6] <- "POSITIVE"
  
  print(dftemp); print(nlevels(dftemp[,2]))
  print(ggplot(dftemp, aes(x=Cut, y=CondProb, fill=POSITIVE)) + geom_bar(stat="identity", color="black", position=position_dodge()) + geom_hline(aes(yintercept=as.numeric(probNull)), colour="#990000", linetype="dashed") + xlab("Bins") + ggtitle(paste(dftemp[,1],"\n Binned")))
  rm(pi,dftemp)
}
################ Calculate AUC ################################
df1 =  data.frame( "Type" = character(),"Var" = character(), "TrainAuc" = numeric(),"CalAuc" = numeric(), stringsAsFactors=FALSE)
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(trainset[,pi],trainset[,outcome])
  if(aucTrain>=0.6) {
    aucCal <- calcAUC(calset[,pi],calset[,outcome])
    x1 <- data.frame("Cat",pi,aucTrain,aucCal);
    df1 <- rbind(df1,x1)
    #print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",pi,aucTrain,aucCal))
  }
}
df2 =  data.frame( "Type" = character(),"Var" = character(), "TrainAuc" = numeric(),"CalAuc" = numeric(), stringsAsFactors=FALSE)
for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(trainset[,pi],trainset[,outcome])
  if(aucTrain>=0.6) {
    aucCal <- calcAUC(calset[,pi],calset[,outcome])
    x2 <- data.frame("Numeric",pi,aucTrain,aucCal);
    df2 <- rbind(df2,x2)
    #print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",pi,aucTrain,aucCal))
  }
}
df3 <- as.data.frame(rbindlist(list(df1,df2)))
df3[order (df3[,1], -df3[,4]),]
rm(df1,df2)

##########not working, cross validation
#aucs <- rep(0:100)
#for(rep in 1:length(aucs)) {
#useForCalRep <- rbinom(n=dim(trainset)[[1]],size=1,prob=0.1)>0
#predRep <- mkPredC(trainset[!useForCalRep,outcome],trainset[!useForCalRep,va#rs],trainse#t[useForCalRep,vars])
#aucs[rep] <- calcAUC(predRep,trainset[useForCalRep,outcome])
#}

########### log likelihood function to select variables #########################
selVars <- c()
minStep <- 5
baseRateCheck <- logLikelyhood(calset[,outcome],sum(calset[,outcome]==pos)/length(calset[,outcome]))

################# pick best cat & numeric vars using function above #####################
df1 =  data.frame( "Type" = character(),"Var" = character(), "Val" = numeric(), stringsAsFactors=FALSE)
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(calset[,outcome],calset[,pi]) - baseRateCheck))
  if(liCheck>minStep) {
    x1 <- data.frame("Cat",v,liCheck)
    df1 <- rbind(df1,x1)
    selVars <- c(selVars,catVars)
    #print(sprintf("%s, calibrationScore: %g",pi,liCheck))
  }
}
df2 =  data.frame( "Type" = character(),"Var" = character(), "Val" = numeric(), stringsAsFactors=FALSE)
for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(calset[,outcome],calset[,pi]) - baseRateCheck))
  if(liCheck>minStep) {
    x2 <- data.frame("Numeric",v,liCheck)
    df2 <- rbind(df2,x2)
    selVars <- c(selVars,numericVars)
    #print(sprintf("%s, calibrationScore: %g",pi,liCheck))
  }
}
df3 <- as.data.frame(rbindlist(list(df1,df2)))
df3[order (df3[,1], -df3[,3]),]
rm(df1,df2)


frmla <- CUST_CHURN_CAT ~ MON + DOM_TYPE_II + DIVISION_NAME + LCREDIT_LIMIT + OS_HA  + USED_ACCTS_PAYABLE + USED_AVAIL_CHECK + USED_COPY_QUOTE + USED_MY_LIST + USED_OLBP + USED_ORDER_INQUIRE + USED_QUICK_ORDER + USED_REDEEM_PPP + USED_S2S_ORDER + USED_UPLOAD_SPREADSHEET + ACCEPT_PRO_PLUS + HIST_MONTHS_TENURE + HIST_FOL_MON_TENURE + PCT_TENURE_FOL + CNT_ROLLING_MONTHS + LTOT_ROLLING_SLS + LROLLING_FOL_SLS + LROLLING_FOL_LINES + LROLLING_TOT_LINES + LROLLING_TOT_SSES + ROLLING_HV_SSES + ROLLING_SHARE_NON_SEARCH_SSES + ROLLING_MDI + ROLLING_MAN_PCT + ROLLING_JOB_PCT + ROLLING_FOL_SHR_SALES_PCT + ROLLING_NET_BID_PCT + ROLLING_FOL_BID_PCT + ROLLING_FOL_JOB_PCT + ROLLING_FOL_SHR_LINES_PCT + MOVAVG_TOT_SLS_RATIO + MOVAVG_NETJOBSLS_RATIO + MOVAVG_FOL_SLS_RATIO + MOVAVG_NETNONBIDSLS_RATIO + MOVAVG_FOLNONBIDSLS_RATIO + MOVAVG_FOLJOBSLS_RATIO + MOVAVG_FOLLINES_RATIO + MOVAVG_TOTLINES_RATIO + MOVAVG_TOTSSE_RATIO + DLOG_NET_SLS + DLOG_FOL_SLS + DLOG_NET_JOBSLS + DLOG_NET_BIDSLS + DLOG_FOL_BIDSLS + DLOG_FOL_JOBSLS + DLOG_FOL_LINES + DLOG_TOT_LINES + DLOG_TOT_SSES + DLOG_HV_SSES + LLAG1_NET_SLS + LLAG1_FOL_SLS + TOT_MONTH_CONSEC + FOL_MONTH_CONSEC + LLAG_VISITS + LAG_ACCOUNTS_PAYABLE + LAG_OLBP_BILLTRUST + LAG_ORDER_INQUIRY + LAG_REQUEST_QUOTE + LAG_MY_LIST_ADDS + LAG_MY_LIST_VIEWS + LAG_UPLOAD_SPREADSHEET + LAG_QUICK_ORDER + LAG_AVAILABILITY_CHECKS + LAG_REDEEM_PRO_PLUS_POINTS + LAG_COPY_QUOTE + LAG_CHANGE_JOB_ACCOUNT

############################ models #################################################
######################## mylogit1 
mylogit <- glm(CUST_CHURN_CAT ~ USED_COPY_QUOTE + USED_QUICK_ORDER + USED_REDEEM_PPP + CNT_ROLLING_MONTHS + LROLLING_FOL_SLS + LROLLING_FOL_LINES + ROLLING_FOL_SHR_LINES_PCT + DLOG_FOL_SLS + LLAG1_FOL_SLS + FOL_MONTH_CONSEC + MOVAVG_FOLLINES_RATIO, data = trainset, family = "binomial")
mylogit.pred <- predict(mylogit, testset[,which( colnames(testset)!=outcome )], type="response")
mylogit.table <- table(pred = mylogit.pred > .5, true = testset[,outcome]); mylogit.table
mylogit.accuracy <- sum(diag(mylogit.table))/sum(mylogit.table); mylogit.accuracy
mylogit.recall <- sum(mylogit.table [2,2])/sum(mylogit.table[,2]); mylogit.recall
mylogit.precision <- sum(mylogit.table[2,2])/sum(mylogit.table[2,]); mylogit.precision
mylogit.f1 <- (2* mylogit.table[2,2])/( 2*mylogit.table[2,2] + mylogit.table[1,2] + mylogit.table[2,1]); mylogit.f1

######################### mylogit4
mylogit4 <- glm(frmla, data = trainset, family = "binomial"(link = "logit"))
mylogit4.pred <- predict(mylogit4, testset[,which( colnames(testset)!=outcome )], type="response")
mylogit4.table <- table(pred = mylogit4.pred > .5,true = testset[,outcome]);mylogit4.table
mylogit4.accuracy <- sum(diag(mylogit4.table))/sum(mylogit4.table);mylogit4.accuracy
mylogit4.recall <- sum(mylogit4.table [2,2])/sum(mylogit4.table[,2]); mylogit4.recall
mylogit4.precision <- sum(mylogit4.table[2,2])/sum(mylogit4.table[2,]); mylogit4.precision
mylogit4.f1 <- (2* mylogit4.table[2,2])/( 2*mylogit4.table[2,2] + mylogit4.table[1,2] + mylogit4.table[2,1]); mylogit4.f1
ggplot(data=testset) + geom_density(aes(x=mylogit4.pred, color=testset[,outcome]))
eval<- prediction(mylogit4.pred, testset[,outcome])
plot(performance(eval,"tpr","fpr"))
abline(0,1)
sum(ifelse(testset[,outcome]==pos,log(mylogit4.pred),log(1-mylogit4.pred)),na.rm=TRUE
)
sum(ifelse(testset[,outcome]==pos,log(mylogit4.pred),log(1-mylogit4.pred)) ,na.rm=TRUE)/ length(which(!is.na(mylogit4.pred)))
sum(ifelse(testset[,outcome]==pos,1,0), na.rm=TRUE)/ length(which(!is.na(mylogit4.pred)))
#print(calcAUC(predict(mylogit4.pred,newdata=trainset),trainset[,outcome]))

######################### tree
tree1 <- rpart(frmla,data=trainset,control=rpart.control(cp=0.001,minsplit=500,minbucket=200,maxdepth=10))
#printcp(tree1)
#printcp(tree1) # display the results
#plotcp(tree1) # visualize cross-validation results
#summary(tree1) # detailed summary of splits
#plot(tree1, uniform=TRUE, main="ds tree")
par(cex=.3)
plot(tree1)
text(tree1)
tree1.pred <- as.data.frame(predict(tree1, testset[,which( colnames(testset)!=outcome )]))
tree1.pred$highestValue = ifelse(tree1.pred[,1]> tree1.pred[,2],0,1)
tree1.table <- table(pred = tree1.pred$highestValue,true = testset[,outcome]);tree1.table
tree1.accuracy <- sum(diag(tree1.table))/sum(tree1.table); tree1.accuracy
tree1.recall <- sum(tree1.table [2,2])/sum(tree1.table[,2]); tree1.recall
tree1.precision <- sum(tree1.table[2,2])/sum(tree1.table[2,]); tree1.precision
tree1.f1 <- (2* tree1.table[2,2])/( 2* tree1.table[2,2] + tree1.table[1,2] + tree1.table[2,1]); tree1.f1
ggplot(data=testset) + geom_density(aes(x=tree1.pred$highestValue, color=testset[,outcome]))
eval<- prediction(tree1.pred$highestValue, testset[,outcome])
plot(performance(eval,"tpr","fpr"))
abline(0,1)
sum(ifelse(testset[,outcome]==pos,log(tree1.pred[,2]),log(1-tree1.pred[,2])))
sum(ifelse(testset[,outcome]==pos, log(tree1.pred[,2]),log(1-tree1.pred[,2])))/ length(testset[,outcome])
sum(ifelse(testset[,outcome]==pos,1,0), na.rm=TRUE)/ length(testset[,outcome])


######################### nb
nb <- naiveBayes(as.formula(frmla), data = trainset, laplace = 3)
nb.pred <- as.data.frame(predict(nb, testset[,which( colnames(testset)!=outcome )],type="raw"))
nb.pred$highestValue = ifelse(nb.pred[,1]> nb.pred[,2],0,1)
nb.table <- table(pred = nb.pred$highestValue,true = testset[,outcome]); nb.table
nb.accuracy <- sum(diag(nb.table))/sum(nb.table); nb.accuracy
nb.recall <- sum(nb.table [2,2])/sum(nb.table[,2]); nb.recall
nb.precision <- sum(nb.table[2,2])/sum(nb.table[2,]); nb.precision
nb.f1 <- (2* nb.table[2,2])/( 2* nb.table[2,2] + nb.table[1,2] + nb.table[2,1]); nb.f1
ggplot(data=testset) + geom_density(aes(x= nb.pred$highestValue, color=testset[,outcome]))
eval<- prediction(nb.pred$highestValue, testset[,outcome])
plot(performance(eval,"tpr","fpr"))
abline(0,1)
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


josh <- cbind.data.frame(mylogit4.pred, tree1.pred[,2], nb.pred[,2], testset[,outcome])
colnames(josh)[1] <- "logit"
colnames(josh)[2] <- "tree"
colnames(josh)[3] <- "nb"
colnames(josh)[4] <- "outcome"
formform <- as.formula(josh$outcome~ josh$logit + josh$tree + josh$nb)



logit5 <- glm(formform, data = josh, family = "binomial"(link = "logit"))
