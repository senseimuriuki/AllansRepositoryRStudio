####################################################################################################################################################################
####### Econometrics 3389 Assignment 3 - Allan Muriuki - R-Studio Code####### 
####################################################################################################################################################################

##Part 1 - Factor List##



install.packages("forecast")
install.packages("neuralnet")
install.packages("rnn")
install.packages("h2o")
install.packages("zoo")
install.packages("xts")
library(zoo)
library(xts)
library(neuralnet)
library(forecast)
library(zoo)
library(h2o)
current <- read.csv("C:\\Users\\allan\\OneDrive\\Desktop\\Winter 2022\\Econ 3389\\Final Assignment\\current_with_FF.csv")
transforms <- read.csv("C:\\Users\\allan\\OneDrive\\Desktop\\Winter 2022\\Econ 3389\\Final Assignment\\current_transform_with_FF.csv")
class(current)

# missing observation -- linear interpolation
current[736,"CP3Mx"] <- 0.76
current[736,"COMPAPFFx"] <- 0.41

# convert to time series
ts.curr <- ts(data = current, start = c(1959, 1), frequency = 12)
head(ts.curr)
tail(ts.curr)
ts.curr
# zoo object (?)
zoo.curr <- zoo(ts.curr, frequency = 12)
head(zoo.curr)
zoo.curr
help(zoo.curr)
# remove sasdate
ts.curr <- ts.curr[,-1]
summary(ts.curr)
ts.curr
# apply transformation
for(i in 1:length(transforms)){
  tform <- transforms[i]
  tmpts <- ts.curr[,i]
  tmplen <- length(tmpts)
  #if(tform == 7){print(tail(tmpts))}
  if(tform == 2){ tmpts <- diff(tmpts)}
  if(tform == 3){ tmpts <- diff(diff(tmpts))}
  if(tform == 4){ tmpts <- log(tmpts)}
  if(tform == 5){ tmpts <- diff(log(tmpts))}
  if(tform == 6){ tmpts <- diff(diff(log(tmpts)))}
  if(tform == 7){ tmpts <- diff((tmpts/lag(tmpts)-1))}
  #if(tform == 7){print(tail(tmpts))}
  #(tform == 7){print(i)}
  dlen <- tmplen-length(tmpts)
  if(dlen==1){tmpts <- c(NA,tmpts)}
  if(dlen==2){tmpts <- c(NA,NA,tmpts)}
  ts.curr[,i] <- tmpts
}

class(ts.curr)
print(ts.curr[,"MPOLICY"])

# remove some columns with lots of NA's
new<-ts.curr[,!(colnames(ts.curr) %in% c("sasdate","VIXCLSx","UMCSENTx","TWEXAFEGSMTHx","ANDENOx","ACOGNO"))]
# remove rows with NA's
new <- new[-(755:756),]
new <- new[-(1:12),]

# verify no NA's
sum(new)
summary(new)
class(new)
new

#==============================================
# convert to matrix
new.mat <- as.matrix(new)

# matrix * matrix
new.new <- t(new.mat) %*% new.mat

# get eigen vector/values
ev <- eigen(new.new)

ev$vectors[,1:4]
ev$values[1:4] / sum(ev$values)

plot((ev$values/sum(ev$values)), ylim = c(0,1))
ev$values/sum(ev$values)
# first factor
f1 <- new.mat %*% as.matrix(ev$vectors[,1])
plot(f1)

# second factor
f2 <- new.mat %*% as.matrix(ev$vectors[,2])
points(f2)
plot(f2)
# factors are orthogonal
t(f1) %*% f2

#==============================================
# Select Output and Income group from paper 
# This will result in an output factor(s)

# -- some names have changed/dropped
new.names <- colnames(new)
grp01 <- c("RPI","W875RX1","INDPRO","IPFPNSS","IPFINAL","IPCONGD","IPDCONGD",
           "IPNCONGD","IPBUSEQ","IPMAT","IPDMAT","IPNMAT","IPMANSICS","IPB51222S",
           "IPFUELS","NAPMPI","CUMFNS")
sum(new.names %in% grp01)
grp01 %in% new.names
help(t)
new.mat.01 <- new.mat[,(new.names %in% grp01)]
new.new.01 <- t(new.mat.01) %*% new.mat.01
ev.01 <- eigen(new.new.01)

plot((ev.01$values/sum(ev.01$values)), ylim = c(0,1))

f1.01 <- new.mat.01 %*% as.matrix(ev.01$vectors[,1])
plot(f1.01)



#==============================================

# -- some names have changed/dropped
grp02 <- c("HWI", 
           "HWIURATIO", 
           "CLF16OV", 
           "CE16OV", 
           "UNRATE", 
           "UEMPMEAN", 
           "UEMPLT5", 
           "UEMP5TO14", 
           "UEMP15OV", 
           "UEMP15T26", 
           "UEMP27OV", 
           "CLAIMSx", 
           "PAYEMS", 
           "USGOOD", 
           "CES1021000001", 
           "USCONS", 
           "MANEMP", 
           "DMANEMP", 
           "NDMANEMP", 
           "SRVPRD", 
           "USTPU", 
           "USWTRADE", 
           "USTRADE", 
           "USFIRE", 
           "USGOVT", 
           "CES0600000007", 
           "AWOTMAN", 
           "AWHMAN", 
           "NAPMEI", 
           "CES0600000008", 
           "CES2000000008", 
           "CES3000000008"
)
sum(new.names %in% grp02)
grp02 %in% new.names

new.mat.02 <- new.mat[,(new.names %in% grp02)]
new.new.02 <- t(new.mat.02) %*% new.mat.02
ev.02 <- eigen(new.new.02)

plot((ev.02$values/sum(ev.02$values)), ylim = c(0,1))

# 2 factors are relevant
f1.02 <- new.mat.02 %*% as.matrix(ev.02$vectors[,1])
f2.02 <- new.mat.02 %*% as.matrix(ev.02$vectors[,2])
summary(f1.02)
nrow(f1.02)
plot(f1.02)
plot(f2.02)


#==============================================
grp03 <- c("HOUST","HOUSTNE","HOUSTMW","HOUSTS","HOUSTW",
           "PERMIT","PERMITNE","PERMITMW","PERMITS","PERMITW")
sum(new.names %in% grp03)
grp03 %in% new.names

new.mat.03 <- new.mat[,(new.names %in% grp03)]
new.new.03 <- t(new.mat.03) %*% new.mat.03
ev.03 <- eigen(new.new.03)

plot((ev.03$values/sum(ev.03$values)), ylim = c(0,1))

f1.03 <- new.mat.03 %*% as.matrix(ev.03$vectors[,1])
summary(f1.03)
nrow(f1.03)
plot(f1.03)

#==============================================
grp04 <- c("DPCERA3M086SBEA",
           "CMRMTSPLx",
           "RETAILx",
           "NAPM",
           "NAPMNOI",
           "NAPMSDI",
           "NAPMII",
           "ACOGNO",
           "AMDMNOx",
           "ANDENOx",
           "AMDMUOx",
           "BUSINVx",
           "ISRATIOx",
           "UMCSENTx")
sum(new.names %in% grp04)
grp04 %in% new.names

new.mat.04 <- new.mat[,(new.names %in% grp04)]
new.new.04 <- t(new.mat.04) %*% new.mat.04
ev.04 <- eigen(new.new.04)

plot(cumsum(ev.04$values/sum(ev.04$values)), ylim = c(0,1))


f1.04 <- new.mat.04 %*% as.matrix(ev.04$vectors[,1])
f2.04 <- new.mat.04 %*% as.matrix(ev.04$vectors[,2])
summary(f1.04)
nrow(f1.04)
plot(f1.04)
plot(f2.04)

#==============================================
grp05 <- c("M1SL","M2SL","M2REAL","AMBSL","TOTRESNS","NONBORRES","BUSLOANS",
           "REALLN","NONREVSL"," CONSPI","MZMSL","DTCOLNVHFNM","DTCTHFNM",
           "INVEST")

sum(new.names %in% grp05)
grp05 %in% new.names

new.mat.05 <- new.mat[,(new.names %in% grp05)]
new.new.05 <- t(new.mat.05) %*% new.mat.05
ev.05 <- eigen(new.new.05)

plot((ev.05$values/sum(ev.05$values)), ylim = c(0,1))
f1.05 <- new.mat.05 %*% as.matrix(ev.05$vectors[,1])
summary(f1.05)
nrow(f1.05)
plot(f1.05)

#==============================================
grp06 <- c("FEDFUNDS","CP3Mx","TB3MS","TB6MS","GS1","GS5","GS10","AAA","BAA",
           "COMPAPFFx","TB3SMFFM","TB6SMFFM","T1YFFM","T5YFFM","T10YFFM",
           "AAAFFM","BAAFFM","TWEXMMTH","EXSZUSx","EXJPUSx","EXUSUKx","EXCAUSx")

sum(new.names %in% grp06)
grp06 %in% new.names

new.mat.06 <- new.mat[,(new.names %in% grp06)]
new.new.06 <- t(new.mat.06) %*% new.mat.06
ev.06 <- eigen(new.new.06)

plot(cumsum(ev.06$values/sum(ev.06$values)), ylim = c(0,1))

f1.06 <- new.mat.06 %*% as.matrix(ev.06$vectors[,1])
f2.06 <- new.mat.06 %*% as.matrix(ev.06$vectors[,2])
summary(f1.06)
nrow(f1.06)
plot(f1.06)
plot(f2.06)

#==============================================
grp07 <- c("PPIFGS",
           "PPIFCG",
           "PPIITM",
           "PPICRM",
           "OILPRICEx",
           "PPICMM",
           "NAPMPRI",
           "CPIAUCSL",
           "CPIAPPSL",
           "CPITRNSL",
           "CPIMEDSL",
           "CUSR0000SAC",
           "CUUR0000SAD",
           "CUSR0000SAS",
           "CPIULFSL",
           "CUUR0000SA0L2",
           "CUSR0000SA0L5",
           "PCEPI",
           "DDURRG3M086SBEA",
           "DNDGRG3M086SBEA",
           "DSERRG3M086SBEA")
sum(new.names %in% grp07)
grp07 %in% new.names

new.mat.07 <- new.mat[,(new.names %in% grp07)]
new.new.07 <- t(new.mat.07) %*% new.mat.07
ev.07 <- eigen(new.new.07)

plot((ev.07$values/sum(ev.07$values)), ylim = c(0,1))

f1.07 <- new.mat.07 %*% as.matrix(ev.07$vectors[,1])
summary(f1.07)
nrow(f1.07)
plot(f1.07)

#==============================================
grp08 <- c("S.P.500",
           "S.P..indust",
           "S.P.div.yield",
           "S.P.PE.ratio")
sum(new.names %in% grp08)
grp08 %in% new.names

new.mat.08 <- new.mat[,(new.names %in% grp08)]
new.new.08 <- t(new.mat.08) %*% new.mat.08
ev.08 <- eigen(new.new.08)

plot((ev.08$values/sum(ev.08$values)), ylim = c(0,1))

f1.08 <- new.mat.08 %*% as.matrix(ev.08$vectors[,1])
summary(f1.08)
nrow(f1.08)
plot(f1.08)

#==============================================

# variables used in factors
all.grp <- c(grp01,grp02,grp03,grp04,grp05,grp06,grp07,grp08)
all.mat <- new.mat[,(new.names %in% all.grp)]
nrow(all.mat)
ncol(all.mat)
summary(all.mat)


# all large factors
factors <- cbind(f1.01,f1.02,f2.02,f1.03,f1.04,f2.04,f1.05,f1.06,f2.06,f1.07,f1.08)
summary(factors)




df.fact <- data.frame(factors)
ts.factors <- ts(data = df.fact, start = c(1960, 3), frequency = 12)
class(ts.factors)
df.fact.1 <- as.data.frame(ts.factors)
df.fact.1
ts.factors
write.csv(ts.factors, "C:\\Users\\allan\\OneDrive\\Desktop\\Winter 2022\\Econ 3389\\Final Assignment\\factors.csv", row.names = FALSE  )
#At the end of this step you should have a csv with factors and the date for each factor###




########################################################################################################

## Part 2: Senitment Analysis ##


install.packages("SentimentAnalysis")  # for sentiment analysis
library(SentimentAnalysis)

install.packages("syuzhet")  # for sentiment analysis
library(syuzhet)

install.packages("tm")  # for text mining
library("tm")

install.packages("wordcloud") # word-cloud generator 
library("wordcloud")

install.packages("SnowballC") # for text stemming
library("SnowballC")

# Load the statements document

#  SOURCE: https://www.federalreserve.gov/monetarypolicy/fomc_historical_year.htm
txt.file <- "FOMCmeetingStatements.txt"

# Read in each line of text
text <- readLines(txt.file)
class(text)
length(text)
# Convert to a single string of characters
text <- toString(text)
class(text)
length(text)
# text
# Remove readLines artifacts
text <- gsub(x=text,pattern = ", ,", replacement = "")
text <- gsub(x=text,pattern = "Share,", replacement = "Share.")
# text

# Convert to list of sentences
sen.text <- get_sentences(text)
class(sen.text)
head(sen.text, 10)
tail(sen.text, 20)


# Find particular text in a sentence 
# -- useful for removing irrelevant sentences or phrases
sen.text[grepl("FOMC statement", sen.text, fixed = T)]
sen.text[!(grepl("Voting", sen.text, fixed = T))]
sen.text[!grepl("Share", sen.text, fixed = T)]
sen.text[!grepl("EST", sen.text, fixed = T)]
sen.text[grepl("EDT", sen.text, fixed = T)]

# remove sentence based on unique identifier
sen.text <- sen.text[!(grepl("Voting", sen.text, fixed = T))]
# sen.text
# remove unique words
sen.text <- gsub("EDT Share., "," ", sen.text)
sen.text <- gsub("EST Share., "," ", sen.text)
sen.text
# Use this to identify where each statement starts/stops
# sen.text <- sen.text[!(grepl("Federal Reserve issues FOMC", sen.text, fixed = T))]
# sen.text
state.start <- (1:length(sen.text))[(grepl("FOMC statement", sen.text, fixed = T))]
state.start <- c(state.start,(length(sen.text)+1))
length(state.start)
state.start


# Collapse statements by date of release
for(i in 1:(length(state.start)-1)){
  tmp.1 <- sen.text[((state.start[i]):(state.start[i+1]-1))]
  tmp.2 <- tmp.1[(grepl("FOMC statement", tmp.1, fixed = T))]
  tmp.1 <- tmp.1[!(grepl("FOMC statement", tmp.1, fixed = T))]
  if(i==1){tmp.3 <- tmp.2}else{tmp.3 <- c(tmp.3,tmp.2)}
  if(i==1){tmp.4 <- toString(tmp.1)}else{tmp.4 <- c(tmp.4,toString(tmp.1))}
}
statements <- tmp.4
state.date <- tmp.3
state.date
statements
# Sentiment Analysis

statements.sent <- analyzeSentiment((statements))
summary(statements.sent)
summary(statements.sent[,c("SentimentGI","SentimentQDAP","SentimentHE","SentimentLM")])
new <- as.data.frame(statements)
new2 <- as.data.frame(statements.sent)
new3 <- as.data.frame(state.date)
write.csv(new, "C:\\Users\\allan\\OneDrive\\Desktop\\Winter 2022\\Econ 3389\\Final Assignment\\statements.csv", row.names = FALSE  )
write.csv(new2, "C:\\Users\\allan\\OneDrive\\Desktop\\Winter 2022\\Econ 3389\\Final Assignment\\sentiment.csv", row.names = FALSE  )
write.csv(new3, "C:\\Users\\allan\\OneDrive\\Desktop\\Winter 2022\\Econ 3389\\Final Assignment\\startdate.csv", row.names = FALSE  )
### These three csvs can be combined to yield a Csv file with the sentiment and the start date. Merge the two and match the sentiment to each start date manually. Below is the new csv## 

sentiment

## I manually matched the dates here with the dates from the factor dataset to have a combnied sentiment and factor dataset which will be used for ANN analsis. Attached below is said document
View(Sentiment_MFactor)


##/ End of Part 2: Sentiment Analysis##







##########################################################################################################
#Part 3: Neural Network Analsis#


data <- Sentiment_MFactor
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
# 
scaled.fact <- as.data.frame(scale(data, center = mins, scale = (maxs - mins) ))  # bound (0,1)
# scaled.fact <- as.data.frame(scale(df.fact, center = T, scale = (maxs - mins)/2 ))  # bound (-1,1) centered at 0
summary(scaled.fact)






set.seed(42)
start.time <- Sys.time()
nn<- neuralnet(SentimentGI ~., data = scaled.fact, 
                      hidden=c(3,2), 
                      act.fct = "logistic",
                      linear.output=TRUE, 
                      rep = 5, 
                      stepmax = 1e+06)
nn
Sys.time()-start.time 
plot(nn, rep="best")
nn$net.result
nn$net.result[[1]]
nn$weights
scaled.fact$SentimentGI
nn$result.matrix
nn1 = ifelse(nn$net.result[[1]]>0.5,1,0)
nn1
misClassificationError = mean(scaled.fact$SentimentGI != nn1)
misClassificationError
OutvsPred = cbind(scaled.fact$SentimentGI,nn1)
OutvsPred 
