options(warn=1)
msg <- file("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/mnps.Rout", open="wt")
sink(msg, type="message")
.libPaths('C:/Users/chris/AppData/Local/TWANG')
if (!is.element("twang", installed.packages()[,1])) install.packages("twang", repos="http://cran.us.r-project.org")
update.packages(lib.loc="C:/Users/chris/AppData/Local/TWANG",
repos="http://cran.us.r-project.org",
instlib="C:/Users/chris/AppData/Local/TWANG",
ask=F,
oldPkgs="twang")
library(twang)
if(compareVersion(installed.packages()['twang','Version'],'1.4-0')== -1){stop('Your version of TWANG is out of date. \n It must version 1.4-0 or later.')}
set.seed(1)

inputds<-read.csv("C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/datafile.csv")
vnames <- names(inputds)
#names(inputds) <- tolower(names(inputds))
inputds[,'matchindextreatment'] <- as.factor(inputds[, 'matchindextreatment'])
mnps1 <- mnps(matchindextreatment ~ saemhi_pt + saepovrtall_pt + annualsubsfullstate + annualpercofullstate + unemploymentinsurancecompens + retirementandother + personaldividendincome + monetaryrent + populationpersons3 + percapitapersonalincome4 + percapitaunemploymentinsuran + proprietorsemployment + farmproprietorsemployment6 + averagewagesandsalaries + averagenonfarmproprietorsinc,
data = inputds,
n.trees = 3000,
interaction.depth = 3,
shrinkage = .01,
perm.test.iters = 0,
stop.method = c('es.mean','ks.mean'),
estimand = "ATE",
sampw = NULL,
treatATT = NULL,
verbose = FALSE
)

baltab<-bal.table(mnps1, collapse.to="pair")
bnames <- as.character(baltab$var)
bnames <- as.character(baltab$var)
bnames1 <- sapply(strsplit(bnames, ':'), function(x){return(x[[1]])})


bnames1 <- vnames[match(bnames1, vnames)]
substr(bnames, 1, nchar(bnames1)) <- bnames1
baltab$var <- bnames
baltab[baltab==Inf] <- NA
baltab[baltab==(-Inf)] <- NA
write.table(baltab,file="C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/baltab.csv",row.names=FALSE,col.names=TRUE,sep=',',na='.')


w <- sapply(mnps1$stopMethods, get.weights, ps1=mnps1)
w<-as.data.frame(w)
names(w) <- paste(mnps1$stopMethods, mnps1$estimand, sep='')
w$tempid<- inputds$tempid

write.table(w,file="C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/wts.csv",row.names=FALSE,col.names=TRUE,sep=',')
if("ATE"=="ATE"){
   summ <- summary(mnps1)
   summ1 <- summ[[1]]
   summ2 <- summ[[2]]
   write.table(summ1,file="C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/summary1.csv",row.names=FALSE,col.names=TRUE,sep=',',na='.')
   write.table(summ2,file="C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/summary2.csv",row.names=FALSE,col.names=TRUE,sep=',',na='.')
}else{
   summ<-summary(mnps1)
   ctx <- nrow(summ$summaryList[[1]])
   ctx <- rep(summ$levExceptTreatATT, each=ctx)
   summ <- do.call(rbind, summ$summaryList)
   tmp <- row.names(summ)
   rownames(summ) <- NULL
   summ <- data.frame(comp_treat=ctx, row_name=tmp, summ)
   write.table(summ,file="C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/summary.csv",row.names=FALSE,col.names=TRUE,sep=',',na='.')
}

pdf('C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/mnps_example_plot.pdf')
plot(mnps1,plots=1, multiPage=TRUE)
plot(mnps1,plots=2, multiPage=TRUE)
plot(mnps1,plots=3, multiPage=TRUE)
plot(mnps1,plots=4, multiPage=TRUE)
plot(mnps1,plots=5, multiPage=TRUE)
dev.off()
save(mnps1, file='C:/Users/chris/Documents/GeorgetownMPPMSFS/McCourtMPP/Semester5Fall2018/Thesis/mnps.RData')
