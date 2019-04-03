library(GDINA)
# A simulated data in GDINA package
#setwd("E:\\知识图谱推荐\\全部实验结果")
#dat <- read.table("finalresultS.txt",sep = ",",header = FALSE);dat
setwd("E:\\知识图谱推荐")
dat <- read.table("pmfcd_cp.txt",sep = ",",header = FALSE);dat
#dat <- sim10GDINA$simdat
setwd("E:\\知识图谱推荐")
Q <-  read.table("Q.txt",sep = ",",header = FALSE);Q
#Q <- sim10GDINA$simQ
# Estimating GDINA model
est <- GDINA(dat = dat, Q = Q, model = "GDINA", sequential = TRUE)
print("123")
x=est
df <- personparm(x,"mp")
person = c(1, 2,3,4,5,6,7,8,9,10,11,12)
att.names = NULL
item = "all"
withSE = FALSE
SE.type = 2
if(is.null(att.names)){
  att.names <- colnames(df)
}else{
  att.names <- att.names
}
np <- length(person)

if(np>1){
  dff <- c(t(df[person,]))
  cat("123")
  dat <- data.frame(att = rep(att.names,np),mp = dff,person = factor(rep(person,each = ncol(df))))
  str(dat)
  write.table(dat, "C:\\Users\\admin\\Desktop\\result1.csv", col.names=T, sep=",")
  print(ggplot2::ggplot(data = dat, ggplot2::aes_string(x = "att", y = "mp")) +
          geom_bar(stat = "identity", position = "dodge",ggplot2::aes_string(fill = "person")) +
          ylim(0,1)+ geom_text(aes(label = paste(dff),colour = "black", vjust=00))+
          labs(x = "Attributes", y = "Mastery probabilities",
               title = paste("Mastery probabilities")))
}else{
  dff <- c(df[person,])
  dat <- data.frame(att = att.names,mp = dff,person = factor(rep(person,ncol(df))))
  print(ggplot2::ggplot(data = dat, ggplot2::aes_string(x = "att", y = "mp")) +
          geom_bar(stat = "identity", position = "dodge") +
          ylim(0,1)+ geom_text(aes(label = dtf,colour = "black",position=position_stack(.9), vjust=1)) +
          labs(x = "Attributes",y = "Mastery probabilities",
               title = paste("Mastery probabilities for individual",person)))
}

