getwd()
setwd("L:/Students/ZZhang")
# install.packages("XLConnect")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("MASS")
# install.packages("lme4")
# install.packages("gridExtra")
# install.packages("nlme")
# install.packages("lsmeans")
library(nlme)
library(lsmeans)
library(lme4)
library(MASS)
library(dplyr)
library(magrittr)
library(XLConnect)
library(gridExtra)

# get the first df --------------------------------------------------------


df2 <- readWorksheetFromFile( "Working File MyCare_110316.xlsx", sheet = 2, header = TRUE )
df3 <- readWorksheetFromFile( "Working File MyCare_110316.xlsx", sheet = 3, header = TRUE )
binl = 30
lapply(df2, class)
lapply(df3, class)


df2$PATIENT.ID <- as.numeric(df2$PATIENT.ID)
colnames(df3)[colnames(df3) == 'Patient.ID'] <- 'PATIENT.ID'
#merge the two dateset.get the time difference between encounter and enrollment

no.allpts <- length(unique(df3$PATIENT.ID))
df2$ENCOUNTER.DATE <- as.Date(df2$ENCOUNTER.DATE,"%m/%d/%Y") 
df3$Enrollment.Date <- as.Date(as.POSIXct(df3$Enrollment.Date))
df3$moniter_days_after <- as.numeric(as.Date('2015-09-15',"%Y-%m-%d") - df3$Enrollment.Date)
#find mismatch ID in two datasets
unique(df2$PATIENT.ID[!(df2$PATIENT.ID %in% df3$PATIENT.ID)])
df <- merge(df2,df3,by="PATIENT.ID")


# delete record without encounter date
df <- df[!is.na(df$ENCOUNTER.DATE),]


df$DIFFTIME <- as.numeric(df$ENCOUNTER.DATE - df$Enrollment.Date)
min(df$DIFFTIME,na.rm = T)
max(df$DIFFTIME,na.rm = T)

tp <- arrange(df,DIFFTIME)

tp[1:10,]
#get rid of three miss record 
df <- df[df$DIFFTIME> -30000,]


first_measure <- df %>% group_by(PATIENT.ID) %>%
  filter(DIFFTIME == min(DIFFTIME)) %>%
  arrange(PATIENT.ID)

min(first_measure$DIFFTIME)
max(first_measure$DIFFTIME)



startbin <- -12
endpoint <- max(df3$moniter_days_after,na.rm = T)
endbin <- floor(abs(endpoint/binl))


breaks <- seq(startbin,endbin)*binl
df$bin_index <- cut(as.numeric(df$DIFFTIME), breaks,labels = FALSE)
df$bin_index <- df$bin_index + startbin - 1
df$bin_interval <- cut(as.numeric(df$DIFFTIME), breaks)

df[is.na(df$PATIENT.ID),]
df[is.na(df$TYPE),]
df[is.na(df$Gender),]
df[is.na(df$Race),]
df[is.na(df$DIFFTIME),]
nrow(df[is.na(df$bin_index),])
df <- df[!is.na(df$bin_index),]
# exclude the after 12month time
df <- subset(df,bin_index <= 11)
# function: count visits function--------------------------------
events_count <- function(df,type){
  res <- as.data.frame(matrix(NA,nrow = 0, ncol = 4)) 
  colnames(res) <- c("id","t","count","no.patients")
  for (id in unique(df$PATIENT.ID)){
    tmp.1 <- subset(df, PATIENT.ID == id)
    bin_index.max <- floor(tmp.1$moniter_days_after[1]/30)-1 
    bin_index.max <- min(bin_index.max,11)
    tb <- as.data.frame(matrix(NA,nrow = bin_index.max + 13, ncol = 4)) 
    colnames(tb) <- c("id","t","count","no.patients")
    for (bin in -12:bin_index.max){
      tmp.2 <- subset(df, PATIENT.ID == id & bin_index == bin)
      tb$id[bin + 13] <- id
      tb$t[bin + 13] <- bin
      tb$count[bin + 13] <- nrow(subset(tmp.2, TYPE %in% type))
      tb$no.patients[bin+13] <- patients_count(df,bin) 
    }
    res <- rbind(res,tb)
  }
  return(res)
}

# function: how many patients in each month -------------------------------
patients_count <- function(df,bin_count_pts){
  tp <- subset(df,floor(moniter_days_after/30) >= bin_count_pts)
  no. <- length(unique(tp$PATIENT.ID)) 
  return(no.)
}

tp <- subset(df,floor(moniter_days_after/30) >= 7)
length(unique(tp$PATIENT.ID))



# function: count readmissions------------------------------------------------------
readm_count <- function(df,type){
  # res is the return data frame
  res <- as.data.frame(matrix(NA,nrow = 0, ncol = 5)) 
  colnames(res) <- c("id","t","adm.count","readm.count","no.patients")
  #loop for id
  for (id in unique(df$PATIENT.ID)){
    tmp.1 <- subset(df, PATIENT.ID == id)
    bin_index.max <- floor(tmp.1$moniter_days_after[1]/30)-1 
    bin_index.max <- min(bin_index.max,11)
    # tb is the df for each patient, will be binded in res
    tb <- as.data.frame(matrix(NA,nrow = bin_index.max + 13, ncol = 5)) 
    colnames(tb) <- c("id","t","adm.count","readm.count","no.patients")
    #loop for month
    for (bin in -12:bin_index.max){
      tb$id[bin + 13] <- id
      tb$t[bin + 13] <- bin
      tb$no.patients[bin + 13] <- patients_count(df,bin)
      tmp.2 <- subset(df, PATIENT.ID == id & bin_index == bin & TYPE %in% type)
      adm_count <- nrow(tmp.2)
      tb$adm.count[bin+13] <- adm_count
      readm.count <- 0
      for (i in 1:adm_count)
      {
        tp1 <- subset(tmp.1, as.numeric(ENCOUNTER.DATE - tmp.2[i,"ENCOUNTER.DATE"]) <= 30 &
                        as.numeric(ENCOUNTER.DATE - tp[i,"ENCOUNTER.DATE"]) > 0 &
                        TYPE %in% c(5,6))
        tp1.flg <- as.logical(nrow(tp1))
        #type 2
        tmp <- subset(tmp.1, TYPE %in% c(5,6,7,8)) %>% arrange(ENCOUNTER.DATE) %>% subset(ENCOUNTER.DATE > tmp.2[i,"ENCOUNTER.DATE"])
        
        if (nrow(tmp)==0) tp2.flg <- F
        else if (tmp.2[i,"TYPE"]+2 == tmp$TYPE[1])
        {
          discharge_date <- tmp$ENCOUNTER.DATE[1]
          tp2 <- subset(tmp, as.numeric(ENCOUNTER.DATE - discharge_date) <= 30 &
                          as.numeric(ENCOUNTER.DATE - discharge_date) > 0 &
                          TYPE %in% c(5,6))
          tp2.flg <- as.logical(nrow(tp2))
        }
        else {tp2.flg <- F}
        # if type1 or type2 readmission occurs 
        readm.count <- readm.count + as.numeric(tp1.flg | tp2.flg)
      }
      tb$readm.count <- readm.count
    }
    res <- rbind(res,tb)
  }
  return(res)
}

# regression --------------------------------------------------------------
main_event <- function(type){
  if (all(type== 1)) {title.type <- 'ER visit, ACSC'}
  else if (all(type == c(1,3))) {title.type <- 'ER visit, ACSC & non - ACSC'}
  else if (all(type == 9)) {title.type <- 'Clinic visit, PCD'}
  else if (all(type == 10)) {title.type <- 'Clinic visit, Specialist'}
  else {stop("error: not valid type")}
  
  tb <- events_count(df,type) 
  tb$I <- tb$t >=0
  tb$I <- as.numeric(tb$I)
  tb$t2 <- (tb$t -0)*tb$I
  mod.poi <- glmer(count ~ t*I + (1 | id), data = tb, family = "poisson")
  summary(mod.poi)
  
  tp1 <- aggregate(count ~ t, data=tb,FUN = sum)
  tp2 <- aggregate(no.patients ~ t, data=tb,FUN = mean)
  tp <- merge(tp1,tp2)
  rate <- tp$count/tp$no.patients
  
  pmmean1 <-
    lsmeans(mod.poi, ~ t + t*I,
            at = list(t = -12:-1,I = 0),type = "response") %>%
    summary()
  
  pmmean2 <-
    lsmeans(mod.poi, ~ t + t*I,
            at = list(t = 0:11,I = 1),type = "response") %>%
    summary()
  
  mypath <- file.path("L:","Students","ZZhang",paste("reg_", title.type, ".png", sep = "")) 
  png(file = mypath,width = 480, height = 580)
  
  plot(-12:11,rate,type = 'p',xlab = "time/month",ylab = "occurance rate",main = title.type)
  lines(-12:11,c(pmmean1$rate[1:12],pmmean2$rate[1:12]),col = 'red')
  lines(-12:11,c(pmmean1$asymp.LCL[1:12],pmmean2$asymp.LCL[1:12]))
  lines(-12:11,c(pmmean1$asymp.UCL[1:12],pmmean2$asymp.UCL[1:12]))
  
  
  
  dev.off()
}




main_readm <- function(type){
  
  if (all(type== 5)) {title.type <- 'Readmission, ACSC'}
  else if (all(type == c(5,6))) {title.type <- 'Readmission, ACSC & non - ACSC'}
  else {stop("error: not valid type")}
  
  tb <- readm_count(df,type)
  tb$readm.rate <- ifelse(tb$adm.count >0, tb$readm.count/tb$adm.count,0)
  tb$I <- tb$t >=0
  tb$I <- as.numeric(tb$I)
  mod.poi <- glmer(readm.count ~ t*I + (1 | id), data = tb, family = "poisson",offset = log(adm.count))
  #mod.nb <- glmer.nb(readm.rate ~ t + t2 + (1 | id), data = tb)
  summary(mod.poi)
  
  
  tp1 <- aggregate(readm.count ~ t, data=tb,FUN = sum)
  tp2 <- aggregate(adm.count ~ t, data=tb,FUN = mean)
  tp <- merge(tp1,tp2)
  rate <- tp$readm.count/tp$adm.count
  
  # tp1 <- aggregate(readm.count ~ t, data=tb,FUN = sum)
  # tp2 <- aggregate(no.patients ~ t, data=tb,FUN = mean)
  # tp <- merge(tp1,tp2)
  # rate <- tp$readm.count/tp$no.patients
  
  
  
  pmmean1 <-
    lsmeans(mod.poi, ~ t + t*I,
            at = list(t = -12:-1,I = 0),type = "response") %>%
    summary()
  
  pmmean2 <-
    lsmeans(mod.poi, ~ t + t*I,
            at = list(t = 0:11,I = 1),type = "response") %>%
    summary()
  
  mypath <- file.path("L:","Students","ZZhang",paste("reg_readm_", title.type, ".png", sep = "")) 
  png(file = mypath,width = 480, height = 580)
  
  plot(-12:11,rate,type = 'p',xlab = "time/month",ylab = "occurance rate",main = title.type)
  lines(-12:11,c(pmmean1$rate[1:12],pmmean2$rate[1:12]),col = 'red')
  lines(-12:11,c(pmmean1$asymp.LCL[1:12],pmmean2$asymp.LCL[1:12]))
  lines(-12:11,c(pmmean1$asymp.UCL[1:12],pmmean2$asymp.UCL[1:12]))
  
  dev.off()
  
}


main_event(1)
main_event(c(1,3))
main_event(9)
main_event(10)

main_readm(5)
main_readm(c(5,6))

# check readm_count some thing wrong-------------------------------------------------------

which(tb$readm.count == max(tb$readm.count))
tb[90:130,]

which(tb$readm.count == max(tb$readm.count))
[1] 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 241 242
[27] 243 244 245 246 247 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264