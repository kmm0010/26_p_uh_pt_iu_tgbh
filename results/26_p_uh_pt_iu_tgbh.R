# spread
# mode 

# porportion of d/b 



library(Hmisc)
library(lattice)
library(languageR)
library(lme4)

directory <- '/Volumes/SSD Part2/_userDataSSD2/Experiments/kmDissKM/ibexFarm/21_p_sp_uh_pt_rfvg/results'

setwd(directory)


resultfile <- 'trialdata.txt'
columnlist <- c('workerID', 'timestamp', 'IPhash', 'trialtype', 'stepC', 'stim', 'context', 'resp', 'RT', 'disorder', 'lang', 'headphones', 'buttons')
results <- read.delim(resultfile, header = FALSE, col.names = columnlist)

# results <- read.delim(resultfile, header = FALSE)

head(results)


length(unique(results$workerID))





computerproblem <- c('A1QESNF8S4ERY9')
results <- results[!(results $workerID %in% computerproblem), ]
results $workerID <- factor(results $workerID)








excluded <- results[(results$disorder != 'disorderno' | results$lang != 'eng' | results$headphones != 'headphoneyes'), ]



results <- results[(results$disorder == 'disorderno' & results$lang == 'eng' & results$headphones == 'headphoneyes'), ]
results $workerID <- factor(results $workerID)




subjs <- unique(results$workerID)
length(subjs)








results <- results[ , c('workerID', 'trialtype', 'stepC', 'context', 'stim', 'resp', 'RT', 'buttons')]








results$dummy <- 1



results$RTadj <- results$RT 
results$RTadj <- ifelse(results$context == 'b', results$RT-(0.01959706171625 * 1000), results$RT-(0.34624194055132 * 1000))

# # 0.20640589569161-0.18680883397535722 = 0.01959706171625

# # 0.01959706171625 before zero crossing before [p-t] continuum

# # 0.3658390022675737 - 0.01959706171625 = 0.34624194055132







head(results)








results $respNumV <- ifelse(results $resp == 'P', 0, 
                           ifelse(results $resp == 'T', 1, 5))


head(results)




duration <- NULL
for (s in subjs) {
	subj.results <- results[results$workerID == s, ]
	xtab <- xtabs( ~ stim, data = subj.results)
	print(s)
	howlong <- sum(subj.results$RTadj)/1000/30
	print(howlong)
	duration <- c(duration, howlong)
	# print( length(subj.results$resp) )
}
duration
summary(duration)







tfamily <- 'DTLArgoT'
tiles <- c(5, 5)




results$RTadj
summary(results$RTadj)





par(family = tfamily)
plot(results$RTadj, pch=16, cex=0.6)
plot(results$RTadj, typ='l')
hist(results$RTadj)
hist(results$RTadj, breaks='Scott')
hist(results$RTadj, breaks='FD')



####################
## practice
####################




plot(results$RTadj[results$trialtype == 'p'], pch=16, cex=0.6)
plot(results$RTadj[results$trialtype == 'p'], typ='l')


xtab <- xtabs( ~ resp, data = results[results$trialtype == 'p', ])
barplot(xtab)

summary(results$RTadj[results$trialtype == 'p'])


par(mfrow= tiles, mar=c(1.9, 2, 1, 0.5), cex=0.5, family = tfamily)
for (s in subjs) {
	s.result <- results[ results$workerID == s & results$trialtype == 'p', ]
	# plot(s.result$RTadj, main=s,typ='l', lwd='1.5', col='blue')
	plot(s.result$RTadj, main=s, ylim=c(0, max(results $RTadj)), typ='l', lwd='1.5', col='blue')
}


results <- results[results $trialtype == 't', ]


####################






# ## remove extreme slow (6289.3262076)
# results <- results[results $RTadj < 6288, ]
# results[results $RTadj > 6287, ]



par(family = tfamily)
plot(results$RTadj, pch=16, cex=0.5, col='darkblue')
plot(results$RTadj, typ='l', col='darkblue')


plot(sort(results$RTadj), pch=16, cex=0.6, col='darkblue')




par(family = tfamily)
xtab <- xtabs( ~ resp, data = results)
barplot(xtab)



summary(results$RTadj)



range(results$RTadj)
range(results$RTadj)*1.1


## RTs
par(mfrow= tiles, mar=c(1.9, 2, 2, 0.5), cex=0.4, family = tfamily)
for (s in subjs) {
	s.result <- results[ results$workerID == s, ]
	print(dim(s.result)[1])
	plot(s.result$RTadj, main=s, ylim=c(-200, 5000), typ='l', col='darkblue')
}


## V responses
par(mfrow= tiles, mar=c(1.9, 2, 2, 0.5), cex=0.4, family = tfamily)
for (s in subjs) {
	s.result <- results[ results$workerID == s, ]
	# print(dim(s.result)[1])
	plot(s.result$respNumV, main=s, ylim=c(-0.5, 5.5), pch=16, col='darkblue')
}




dim(s.result)





par(mfrow= tiles, mar=c(1.9, 2, 2, 0.5), cex=0.5, family = tfamily)
for (s in subjs) {
	s.result <- results[ results$workerID == s, ]
	xtab <- xtabs( ~ resp, data = s.result)
	barplot(xtab, ylim=c(0, 200), main=s)
	print(xtab)
}



look at:
A27BSYPO6JCB4Q
AW34JRQ2PDQAV
A2R8DAX6L4RWDZ
A3MTAJHYHCIJK4
A2PWKE19HWQO63




percentNoResp <- NULL
par(mfrow= tiles, mar=c(1.9, 2, 2, 0.5), cex=0.5, family = tfamily)
for (s in subjs) {
	s.result <- results[ results$workerID == s, ]
	xtab <- xtabs( ~ resp, data = s.result)
	barplot(xtab, ylim=c(0, 150), main=s)
	# print(xtab)
	noResp <- xtab['NULL'] / sum(xtab) * 100
	percentNoResp <- c(percentNoResp, noResp)
	print(s)
	print(noResp)
}



## percent no response
par(mar=c(10, 6, 2, 1), family = tfamily, cex=0.48)
barplot(percentNoResp, names.arg=subjs, ylim=c(0,15), las=2, space=0.2, legend.text=FALSE)
abline(mean(percentNoResp), 0, col='red')
abline(median(percentNoResp), 0, col='blue')



## all responses
par(mar=c(8, 6, 2, 1), family = tfamily, cex=0.4)
xtab <- xtabs( ~ resp + workerID, data = results)
barplot(xtab, las=2, space=0.2, legend.text=TRUE)



#############################################
## remove folks who didnt respond
#############################################



alotoftimeouts <- c(
                    'AXJDHS9J0664F'# # 34.92647
)


resultsGood <- results
subjsGood   <- unique(resultsGood $workerID)




#############################################
## exclude no response
#############################################



resultsNoResponse <- resultsGood[resultsGood$resp == 'NULL', ]
dim(resultsNoResponse)



resultsRespGood <- resultsGood[resultsGood$resp != 'NULL', ]
resultsRespGood $resp <- factor(resultsRespGood $resp)
dim(resultsRespGood)



summary(resultsRespGood $RTadj)


par(family = tfamily)
plot(resultsRespGood $RTadj, pch=16, cex=0.1)
abline(225, 0, col='blue')
center <- mean(resultsRespGood $RTadj) * 0.5 + median(resultsRespGood $RTadj) * 0.5
abline(center, 0, col='red', lwd=2)
abline(3700, 0, col='black')
points(resultsRespGood $RTadj, pch=16, cex=0.5)

plot(resultsRespGood $RTadj, typ='l')


hist(resultsRespGood $RTadj)
hist(resultsRespGood $RTadj, breaks='Scott')
# hist(resultsRespGood $RTadj, breaks='FD')


borders <- c(2, 2, 1.2, 1)
text.size <- 0.4

par(mfrow=tiles, mar=borders, cex=text.size, family=tfamily)
for (s in subjsGood) {
	print(s)
	s.result <- resultsRespGood[ resultsRespGood $workerID == s, ]
	plot(s.result$RTadj, main=s, ylim=c(-500, 3900), pch=16, cex=0.2)
	points(s.result$RTadj, pch=16, cex=0.8)
	center <- mean(s.result$RTadj) * 0.5 + median(s.result$RTadj) * 0.5
	abline(225, 0, col='blue')
	abline(center, 0, col='red', lwd=2)
	# print(dim(s.result))
}




7.2% of your response times were before the onset of the acoustic information for 'p' and 't'. And, 21.6% of your responses were just 200 milliseconds after this onset. The human brain generally needs about 200 milliseconds to just make a decision and it takes an even longer time to physically click a mouse button (and the internet interface should be make this slower). So, it appears that you were just pressing buttons fast in order to get through much of the experiment.

Your median respond time was 143 milliseconds after the onset of the acoustic information for 'p' and 't'. The human brain generally needs about 200 milliseconds to just make a decision and it takes an even longer time to click a mouse button. So, it appears that you were just pressing buttons fast in order to get through the experiment.

Your median respond time was 369 milliseconds before the onset of the acoustic information for 'p' and 't'. This means you were pressing buttons before you even heard the sound you were supposed to be listening for.



## for individual
s <- 'A622MVSK40IGL'  # (too fast, rejected)


s.result <- resultsRespGood[ resultsRespGood $workerID == s, ]
# plot(s.result$RTadj, main=s, pch=16, cex=0.2)
# plot(s.result$RTadj, main=s, typ='l')
plot(s.result$RTadj, main=s, ylim=c(-500, 4500), pch=16, cex=0.2)
points(s.result$RTadj, pch=16, cex=0.8)
center <- mean(s.result$RTadj) * 0.5 + median(s.result$RTadj) * 0.5
abline(225, 0, col='blue')
abline(center, 0, col='red', lwd=2)
summary(s.result$RTadj)
summary(s.result$RT)


dim(s.result[s.result$RTadj <= 277, ])
dim(s.result[s.result$RTadj > 277, ])
dim(s.result[s.result$RTadj <= 200, ])
dim(s.result[s.result$RTadj > 200, ])
dim(s.result[s.result$RTadj <= 1, ])
dim(s.result[s.result$RTadj > 1, ])
41/(41+126)
36/(36+131)
12/(12+155)




problempeople <- c('A622MVSK40IGL')
resultsRespGood <- resultsRespGood[!(resultsRespGood $workerID %in% problempeople), ]
resultsRespGood $workerID <- factor(resultsRespGood $workerID)

subjsGood <- unique(resultsRespGood $workerID)


resultsRespGood$resp <- factor(resultsRespGood$resp)




#############################################
## exclude faster than 200ms & practice items
#############################################



responses <- resultsRespGood[resultsRespGood$RTadj > 300, ]
responses$RTlog <- log(responses$RTadj)






summary(responses $RTadj)
summary(responses $RTlog)


par(family = tfamily)
plot(responses $RTlog, pch=16, cex=0.4)
abline(log(225), 0, col='blue')
center <- mean(responses $RTlog) * 0.5 + median(responses $RTlog) * 0.5
abline(center, 0, col='red', lwd=2)


plot(responses $RTlog, typ='l')



hist(responses $RTadj)
hist(responses $RTadj, breaks='Scott')
# hist(responses $RTadj, breaks='FD')


hist(responses $RTlog)
hist(responses $RTlog, breaks='Scott')
# hist(responses $RTlog, breaks='FD')






par(mfrow=tiles, mar=borders, cex=text.size, family=tfamily)
for (s in subjsGood) {
	s.result <- responses[ responses $workerID == s, ]
	plot(s.result$RTlog, main=s, ylim=c(5, 8.5), pch=16, cex=0.5)
	# plot(s.result$RTlog, main=s, xlim=c(0, 212), ylim=c(4, 7.9), typ='l')
	abline(log(225), 0, col='blue')
	center <- mean(s.result$RTlog) * 0.5 + median(s.result$RTlog) * 0.5
	abline(center, 0, col='red', lwd=2)
	print(dim(s.result))
}



par(mfrow= tiles, mar= borders, cex= text.size, family = tfamily)
for (s in subjsGood) {
	s.result <- responses[ responses $workerID == s, ]
	xtab <- xtabs( ~ resp, data = s.result)
	barplot(xtab, ylim=c(0, 300), main=s)
	print(xtab)
}










#############################################
## ID function
#############################################




head(responses)



xtab <- xtabs( ~ workerID  + stepC, data = responses)
xtab






xlabel <- '1 [p] – [t] 9'
ylabel <- 'prop. ‘t’ resp.'


borders <- c(4.1,4.3,1.5,0.4)

responses $dummy <- 1

length(responses $respNumV)
length(responses $stepC)
length(responses $dummy)


# change v steps to 1, 13, 19, 22, 25, 34, 54



par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responses$stepC, responses $dummy, responses $respNumV, xlab=xlabel, ylab=ylabel, legend=TRUE, ylim=c(0, 1), main='pooled', lwd=2.3)
vm   <- tapply(responses $respNumV, responses $stepC, mean)
vsd  <- tapply(responses $respNumV, responses $stepC, sd)
vsem <- vsd / sqrt( length(subjsGood) ) ##* 1.96
errbar( c(1:length(vm)), vm, vm + vsem, vm - vsem, add=TRUE, pch="")


borders <- c(4, 4, 1.2, 0.3)
text.size <- 0.35
par(mfrow= tiles, mar= borders, cex= text.size, family = tfamily)
for (s in subjsGood) {
	s.result <- responses[ responses $workerID == s, ]
	ccol <- 'darkblue'
	interaction.plot(s.result $stepC, s.result $dummy, s.result $respNumV, xlab=xlabel, ylab=ylabel, legend=FALSE, ylim=c(0, 1), main=s, lwd=2, col= ccol)
}








problempeople <- c('')
resultsRespGood <- resultsRespGood[!(resultsRespGood $workerID %in% problempeople), ]
resultsRespGood $workerID <- factor(resultsRespGood $workerID)

subjsGood <- unique(resultsRespGood $workerID)


resultsRespGood$resp <- factor(resultsRespGood$resp)










#############################################
## % correct
#############################################


head(responses)

sort(unique(responses$stepC ))


endpoints <- responses[responses$stepC == 1 | responses$stepC == 18, ]
endpoints$stepC <- factor(endpoints$stepC)




xtab <- xtabs( ~ resp + stepC, data = endpoints)
xtab


# # 17 / 24
# # 24-17 = 7


subjCorrect <- NULL
subjWrong   <- NULL
par(mfrow= tiles, mar= borders, cex= text.size, family = tfamily)
for (s in subjsGood) {
	s.result <- endpoints[ endpoints $workerID == s, ]
	xtab <- xtabs( ~ resp + stepC, data = s.result)
	totalp <- sum(xtab[ , 1])
	correctp <- xtab['P', 1] 
	totalt <- sum(xtab[ , 2])
	correctt <- xtab['T', 2] 
	percentcorrectp <- correctp / totalp * 100
	percentcorrectt <- correctt / totalt * 100
	print(s)
	# print( percentcorrect )
	print( xtab )
	barplot( percentcorrectp, ylim=c(0, 100), main=s)
	# if (percentcorrectp > 65 & percentcorrectt > 65 ) {
	if (percentcorrectp > 70 & percentcorrectt > 70 ) {
		subjCorrect <- c(subjCorrect, s)
	} else {subjWrong <- c(subjWrong, s)}
}
length(subjCorrect)
length(subjWrong)




tiles <- c(4, 5)
borders <- c(4, 4, 1.2, 0.3)

text.size <- 0.35


## correct
par(mfrow= tiles, mar= borders, cex= text.size, family = tfamily)
for (s in subjCorrect) {
	s.result <- responses[ responses $workerID == s, ]
	ccol <- 'darkblue'
	interaction.plot(s.result $stepC, s.result $dummy, s.result $respNumV, xlab=xlabel, ylab=ylabel, legend=FALSE, ylim=c(0, 1), main=s, lwd=2, col= ccol)
}




## wrong
par(mfrow=c(1,2), mar= borders, cex= text.size, family = tfamily)
for (s in subjWrong) {
	s.result <- responses[ responses $workerID == s, ]
	ccol <- 'darkblue'
	interaction.plot(s.result $stepC, s.result $dummy, s.result $respNumV, xlab=xlabel, ylab=ylabel, legend=FALSE, ylim=c(0, 1), main=s, lwd=2, col= ccol)
}




xtab <- xtabs( ~ resp + stepC, data = endpoints)
xtab







responsesCorrect <- responses[(responses$workerID %in% subjCorrect), ]
# responsesCorrect <- responses


borders <- c(4.1,4.3,1.5,0.4)
par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responsesCorrect $stepC, responsesCorrect $dummy, responsesCorrect $respNumV, xlab=xlabel, ylab=ylabel, legend=FALSE, ylim=c(0, 1), main='pooled', lwd=2.3)
vm   <- tapply(responsesCorrect $respNumV, responsesCorrect $stepC, mean)
vsd  <- tapply(responsesCorrect $respNumV, responsesCorrect $stepC, sd)
vsem <- vsd / sqrt( length(subjCorrect) ) ##* 1.96
errbar( c(1:length(vm)), vm, vm + vsem, vm - vsem, add=TRUE, pch="")




head(responsesCorrect)

borders <- c(4.1,4.3,1.5,0.4)
par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responsesCorrect $stepC, responsesCorrect $context, responsesCorrect $respNumV, xlab=xlabel, ylab=ylabel, legend=TRUE, ylim=c(0, 1), main='pooled', lwd=2.3, col=1:7, lty='solid')


borders <- c(4.1,4.3,1.5,0.4)
par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responsesCorrect $context, responsesCorrect $stepC, responsesCorrect $respNumV, xlab='', ylab=ylabel, legend=TRUE, ylim=c(0, 1), main='pooled', lwd=2.3, col=1:7)


borders <- c(4.1,4.3,1.5,0.4)
par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responsesCorrect $context, responsesCorrect $dummy, responsesCorrect $respNumV, xlab=xlabel, ylab=ylabel, legend=TRUE, ylim=c(0, 1), main='pooled', lwd=2.3, col=1:7)








par(mfrow=c(5, 4), mar= borders, cex= 0.4, family = tfamily)
for (s in subjCorrect) {
	s.result <- responsesCorrect[ responsesCorrect $workerID == s, ]
	ccol <- 'darkblue'
	interaction.plot(s.result $stepC, s.result $dummy, s.result $respNumV, xlab=xlabel, ylab=ylabel, legend= TRUE, ylim=c(0, 1), main=s, lwd=2, col=1:7, lty='solid')
}











head(responsesCorrect)

responsesCorrect[responsesCorrect $workerID == s, ]
xtabs( ~ stepC, data = responsesCorrect)
xtabs( ~ respNumV, data = responsesCorrect)
xtabs( ~ respNumV + stepC, data = responsesCorrect)



subjrank <- NULL
for (s in subjCorrect) {
	s.result <- responsesCorrect[ responsesCorrect $workerID == s, ]
	iursp <- xtabs( ~ respNumV + stepC, data = s.result)
	iutot <- sum(iursp[ , c('8', '9', '10')])
	ursp <- sum(iursp['0', c('8', '9', '10')])
	irsp <- sum(iursp['1', c('8', '9', '10')])
	ipercent <- irsp / iutot * 100
	subjrank <- c(subjrank, ipercent)

}


subjrankdf <- data.frame(s= subjCorrect, ip = subjrank)

subjrankdf <- subjrankdf[order(subjrankdf$ip), ]




par(mfrow=c(5, 4), mar= borders, cex= 0.3, family = tfamily)
for (s in subjrankdf$s) {
	s.result <- responsesCorrect[ responsesCorrect $workerID == s, ]
	ccol <- 'darkblue'
	interaction.plot(s.result $stepC, s.result $dummy, s.result $respNumV, xlab=xlabel, ylab=ylabel, legend= TRUE, ylim=c(0, 1), main=s, lwd=2, col=1:7, lty='solid')
}






## sp=red, b=black
par(mfrow=c(5, 4), mar= borders, cex= 0.3, family = tfamily)
for (s in subjrankdf$s) {
	print(s)
	s.result <- responsesCorrect[ responsesCorrect $workerID == s, ]
	ccol <- 'darkblue'
	interaction.plot(s.result $stepC, s.result $context, s.result $respNumV, xlab=xlabel, ylab=ylabel, legend= TRUE, ylim=c(0, 1), main=s, lwd=2, col=1:7, lty='solid')
}







weird <- c('A14NP6X071S7GK')
responsesCorrect <- responsesCorrect[!(responsesCorrect $workerID %in% weird), ]
responsesCorrect $workerID <- factor(responsesCorrect $workerID)

subjCorrect <- unique(responsesCorrect $workerID)


responsesCorrect $resp <- factor(responsesCorrect $resp)




borders <- c(4.1,4.3,1.5,0.4)
par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responsesCorrect $stepC, responsesCorrect $context, responsesCorrect $respNumV, xlab=xlabel, ylab=ylabel, legend=TRUE, ylim=c(0, 1), main='pooled', lwd=2.3, col=1:7, lty='solid')




borders <- c(4.1,4.3,1.5,0.4)
par(mfrow=c(1,1), mar= borders, cex.lab=1.2, cex.axis=1.1, family = tfamily)
interaction.plot(responsesCorrect $stepC, responsesCorrect $dummy, responsesCorrect $respNumV, xlab=xlabel, ylab=ylabel, legend=TRUE, ylim=c(0, 1), main='pooled', lwd=2.3, col=1:7, lty='solid')




# # better steps:
# # 1, 5, 6, 7, 8, 9, 18





########
## mixed effects modeling
########



head(responsesCorrect, n=22)


sort(unique(responsesCorrect$stepC))
mean(unique(responsesCorrect$stepC))
responsesCorrect$stepCc <- responsesCorrect$stepC - mean(unique(responsesCorrect$stepC))
mean(responsesCorrect$stepCc )


## b = -1, sp = +1
responsesCorrect$contextc <- ifelse(responsesCorrect$context == 'b', -1, 1)


## b = 0, sp = +1
responsesCorrect$context01 <- ifelse(responsesCorrect$context == 'b', 0, 1)




vResp.stepC <- lmer(respNumV ~ stepCc + contextc + (1 | workerID), family = "binomial", data = responsesCorrect )
summary(vResp.stepC)


Generalized linear mixed model fit by maximum likelihood ['glmerMod']
 Family: binomial ( logit )
Formula: respNumV ~ stepCc + contextc + (1 | workerID) 
   Data: responsesCorrect 

      AIC       BIC    logLik  deviance 
1838.7057 1862.7485 -915.3529 1830.7057 

Random effects:
 Groups   Name        Variance Std.Dev.
 workerID (Intercept) 1.569    1.253   
Number of obs: 3013, groups: workerID, 18

Fixed effects:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  1.81376    0.30677   5.912 3.37e-09 ***
stepCc       1.08724    0.04952  21.956  < 2e-16 ***
contextc     0.31628    0.06004   5.268 1.38e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
         (Intr) stepCc
stepCc   0.180        
contextc 0.038  0.138 








vResp.stepC.slopes <- lmer(respNumV ~ stepCc + contextc + (1 + stepCc + contextc | workerID), family = "binomial", data = responsesCorrect )	
summary(vResp.stepC.slopes)

Generalized linear mixed model fit by maximum likelihood ['glmerMod']
 Family: binomial ( logit )
Formula: respNumV ~ stepCc + contextc + (1 + stepCc + contextc | workerID) 
   Data: responsesCorrect 

      AIC       BIC    logLik  deviance 
1731.1737 1785.2699 -856.5869 1713.1737 

Random effects:
 Groups   Name        Variance Std.Dev. Corr       
 workerID (Intercept) 1.88216  1.3719              
          stepCc      0.29465  0.5428   -0.03      
          contextc    0.05412  0.2326    0.55 -0.71
Number of obs: 3013, groups: workerID, 18

Fixed effects:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.03053    0.33844   6.000 1.98e-09 ***
stepCc       1.36802    0.14141   9.674  < 2e-16 ***
contextc     0.34152    0.08451   4.041 5.32e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation of Fixed Effects:
         (Intr) stepCc
stepCc    0.060       
contextc  0.379 -0.380



anova(vResp.stepC, vResp.stepC.slopes)


Data: responsesCorrect
Models:
vResp.stepC: respNumV ~ stepCc + contextc + (1 | workerID)
vResp.stepC.slopes: respNumV ~ stepCc + contextc + (1 + stepCc + contextc | workerID)
                   Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
vResp.stepC         4 1838.7 1862.8 -915.35   1830.7                             
vResp.stepC.slopes  9 1731.2 1785.3 -856.59   1713.2 117.53      5  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1











vResp.X.stepC.slopes <- lmer(respNumV ~ stepCc * contextc + (1 + stepCc * contextc | workerID), family = "binomial", data = responsesCorrect )	
summary(vResp.X.stepC.slopes)



anova(vResp.stepC.slopes, vResp.X.stepC.slopes)





vResp.stepC.notcent <- lmer(respNumV ~ stepC + context + (1 | workerID), family = "binomial", data = responsesCorrect )
summary(vResp.stepC.notcent)

vResp.stepC.slopes.notcent <- lmer(respNumV ~ stepC + context + (1 + stepC + context | workerID), family = "binomial", data = responsesCorrect )	
summary(vResp.stepC.slopes.notcent)



