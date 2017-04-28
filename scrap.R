r=glm(dist~speed,data=cars,family=poisson)
P=predict(r,type="response",
          newdata=data.frame(speed=seq(-1,35,by=.2)))
plot(cars,xlim=c(0,31),ylim=c(0,170))
abline(v=30,lty=2)
lines(seq(-1,35,by=.2),P,lwd=2,col="red")
P1=predict(r,type="response",se.fit=TRUE,
           newdata=data.frame(speed=30))
points(30,P1$fit,pch=4,lwd=3)

i1=sum(predict(r,type="response"))
i2=sum(cars$speed*predict(r,type="response"))
i3=sum(cars$speed^2*predict(r,type="response"))

I=matrix(c(i1,i2,i2,i3),2,2)
V=solve(I)

x=30
P2=predict(r, type="link", se.fit=TRUE,
           newdata=data.frame(speed=x))

segments(30, exp(P2$fit-1.96*P2$se.fit),
         30, exp(P2$fit+1.96*P2$se.fit),
         col="blue",lwd=3)


#########################
#########################


pred.data <- predict(AY.glm, type="response",
                     newdata = data.frame(Insurance.Count = seq(55, 105, by = .5)))
plot(AY.std$Insurance.Count, AY.std$Personal.Doctor.Count, 
     xlim = c(60,100), ylim = c(60,100),
     main = "Five Year Average Model",
     xlab = "Percentage of Individuals with Insurance",
     ylab = "Mean Percentage of Individuals with a Personal Doctor")
lines(seq(55, 105, by = .5), pred.data, lwd=2, col="red")

i1=sum(predict(AY.glm, type="response"))
i2=sum(AY.std$Insurance.Count*predict(AY.glm, type="response"))
i3=sum(AY.std$Insurance.Count^2*predict(AY.glm, type="response"))

I=matrix(c(i1,i2,i2,i3),2,2)
V=solve(I)

x <- data.frame(Insurance.Count = seq(55, 105, by = .5))

fit.mean.conf <- function(x, alpha = 0.05, explanatory = seq(55, 105, by = .5) ){
  
  z <- qnorm(alpha/2, lower.tail = F)
  
  out <- data.frame("Lower.Bound" = rep(NA, length(x$fit)), 
                    "Esitmate" = rep(NA, length(x$fit)), 
                    "Upper.Bound" = rep(NA, length(x$fit)),
                    "x" = explanatory)
  
  for(i in 1:length(x$fit)){
    out$Esitmate[i] <- exp(x$fit[i])
    out$Lower.Bound[i] <- exp(x$fit[i] - z*x$se.fit[i])
    out$Upper.Bound[i] <- exp(x$fit[i] + z*x$se.fit[i])
  }
  
  out
}

pred.conf <- predict(AY.glm, x, se.fit=TRUE)
conf.plot <- fit.mean.conf(x = pred.conf)

#lines(conf.plot$x, conf.plot$Esitmate, lty = 1, lwd = 2)
lines(conf.plot$x, conf.plot$Lower.Bound, lty = 2, lwd = 2)
lines(conf.plot$x, conf.plot$Upper.Bound, lty = 2, lwd = 2)

P2=predict(AY.glm, type="link", se.fit=TRUE,
           newdata=x)

lines(x$Insurance.Count, exp(P2$fit - 1.96*P2$se.fit), lty = 3, lwd = 3)


segments(30, exp(P2$fit-1.96*P2$se.fit),
         30, exp(P2$fit+1.96*P2$se.fit),
         col="blue",lwd=3)



##########
##########

pred.interval <- function(model, x, dp = 0.207465, alpha = 0.05){
  
  z <- qnorm(alpha/2, lower.tail = F)
  odata <- model$data
  
  pred.data <- predict(model, type="link", se.fit=TRUE, newdata=x)
  fit <- exp(pred.data$fit)
  mean.conf.LB <- exp(pred.data$fit - z*pred.data$se.fit)
  mean.conf.UB <- exp(pred.data$fit + z*pred.data$se.fit)
  pred.conf.LB <- exp(pred.data$fit) - ( z*sqrt(exp(pred.data$fit) * dp) + (fit - exp(pred.data$fit - z*pred.data$se.fit)))
  pred.conf.UB <- exp(pred.data$fit) + ( z*sqrt(exp(pred.data$fit) * dp) + (exp(pred.data$fit + z*pred.data$se.fit) - fit))
  
  png("plots/mod_bands.png",
      height = 620, width = 620)
  
  plot(odata$Insurance.Count, odata$Personal.Doctor.Count, 
       xlim = c(60,100), ylim = c(min(pred.conf.LB),100),
       main = "Five Year Average Model",
       xlab = "Percentage of Individuals with Insurance",
       ylab = "Mean Percentage of Individuals with a Personal Doctor"
       )
  
  lines(x$Insurance.Count, fit, lwd=2, col="red")
  
  lines(x$Insurance.Count, mean.conf.LB, lty = 2, lwd = 2)
  lines(x$Insurance.Count, mean.conf.UB, lty = 2, lwd = 2)
  
  lines(x$Insurance.Count, pred.conf.LB, lty = 3, lwd = 2)
  lines(x$Insurance.Count, pred.conf.UB, lty = 3, lwd = 2)
  
  legend("topleft",
         legend = c("Fitted Model", "95% Confidence Interval", "95% Prediction Interval"),
         lty = c(1, 2, 3),
         lwd = 2,
         col = c("red", "black", "black"))
  
  dev.off()
  
  out <- data.frame(Estimate = fit, 
                    Conf.Int.LB =  mean.conf.LB, Conf.Int.UB =  mean.conf.UB,
                    Pred.Int.LB =  pred.conf.LB, Pred.Int.UB =  pred.conf.UB
                    )
  
  out
  
}

model.fit <- pred.interval(AY.glm, x = data.frame(Insurance.Count = seq(60, 105, by = .5)))






x <- data.frame("Personal.Doctor.Count" = seq(60, 90, .25), 
                "Insurance.Count" = seq(70, 100, .25), 
                "Total.Respondents" = rep(NA, 121),
                "Medicaid.Expansion" = rep(NA, 121))

pred.conf <- predict(AY.glm, x, se.fit=TRUE)

fit.mean.conf <- function(x, alpha = 0.05, explanatory = seq(70, 100, .25) ){
  
  z <- qnorm(alpha/2, lower.tail = F)
  
  out <- data.frame("Lower.Bound" = rep(NA, length(x$fit)), 
                    "Esitmate" = rep(NA, length(x$fit)), 
                    "Upper.Bound" = rep(NA, length(x$fit)),
                    "x" = explanatory)
  
  for(i in 1:length(x$fit)){
    out$Esitmate[i] <- exp(x$fit[i])
    out$Lower.Bound[i] <- exp(x$fit[i] - z*x$se.fit[i])
    out$Upper.Bound[i] <- exp(x$fit[i] + z*x$se.fit[i])
  }
  
  out
}


png("plots/mod_conf.png",
    height = 620, width = 960)

par(mfrow = c(1,2), omi = c(0,0,1,0))

plot(conf.plot$x, conf.plot$Esitmate, type = "n",
     main = "Five Year Average Model",
     xlab = "Percentage of Individuals with Insurance",
     ylab = "Mean Percentage of Individuals with a Personal Doctor")
points(AY.std$Insurance.Count, AY.std$Personal.Doctor.Count)
lines(conf.plot$x, conf.plot$Esitmate, lty = 1, lwd = 2)
lines(conf.plot$x, conf.plot$Lower.Bound, lty = 2, lwd = 2)
lines(conf.plot$x, conf.plot$Upper.Bound, lty = 2, lwd = 2)

#####

plot(conf.plot$x, conf.plot$Esitmate, type = "n",
     main = "Individual Years Superimposed",
     xlab = "Percentage of Individuals with Insurance",
     ylab = "Mean Percentage of Individuals with a Personal Doctor")
lines(conf.plot$x, conf.plot$Esitmate, lty = 1, lwd = 2)
lines(conf.plot$x, conf.plot$Lower.Bound, lty = 2, lwd = 2)
lines(conf.plot$x, conf.plot$Upper.Bound, lty = 2, lwd = 2)

pred.conf11 <- predict(t11.glm, x, se.fit=TRUE)
conf.plot11 <- fit.mean.conf(x = pred.conf11)
lines(conf.plot11$x, conf.plot11$Esitmate, type = "l", col = "darkmagenta")

pred.conf12 <- predict(t12.glm, x, se.fit=TRUE)
conf.plot12 <- fit.mean.conf(x = pred.conf12)
lines(conf.plot12$x, conf.plot12$Esitmate, type = "l", col = "blue")

pred.conf13 <- predict(t13.glm, x, se.fit=TRUE)
conf.plot13 <- fit.mean.conf(x = pred.conf13)
lines(conf.plot13$x, conf.plot13$Esitmate, type = "l", col = "green")

pred.conf14 <- predict(t14.glm, x, se.fit=TRUE)
conf.plot14 <- fit.mean.conf(x = pred.conf14)
lines(conf.plot14$x, conf.plot14$Esitmate, type = "l", col = "orange")

pred.conf15 <- predict(t15.glm, x, se.fit=TRUE)
conf.plot15 <- fit.mean.conf(x = pred.conf15)
lines(conf.plot15$x, conf.plot15$Esitmate, type = "l", col = "red")

legend(x = 70, y = 93.5,
       legend = c("2011", "2012", "2013", "2014", "2015", "Five Year Average"),
       col = c("darkmagenta", "blue", "green", "orange", "red", "black"),
       lty = 1,
       lwd = 2)

mtext('95% Confidence Band for Means', outer = TRUE, font = 2, padj = -1, cex = 1.3)

dev.off()





































png("plots/PP_counts_vs_insur.png",
    height = 696, width = 960)

par(mar = c(5.1, 4.1, 2.1, 2.1), oma = c(0,1,3,0))
layout(matrix(c(1,1,2,2,3,3,4,4,0,5,5,6,6,7,7,0), 2, 8, byrow = TRUE))


plot(t11.std$Insurance.Count, t11.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2011",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",  
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20, col = "darkmagenta")

plot(t12.std$Insurance.Count, t12.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2012",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20, col = "blue")

plot(t13.std$Insurance.Count, t13.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2013",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20, col = "green")

plot(t14.std$Insurance.Count, t14.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2014",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20, col = "orange")

plot(t15.std$Insurance.Count, t15.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2015",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20, col = "red")

plot(t11.std$Insurance.Count, t11.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2011 to 2015",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20, col = "darkmagenta")

points(t12.std$Insurance.Count, t12.std$Personal.Doctor.Count,
       pch = 20, col = "blue")

points(t13.std$Insurance.Count, t13.std$Personal.Doctor.Count,
       pch = 20, col = "green")


points(t14.std$Insurance.Count, t14.std$Personal.Doctor.Count,
       pch = 20, col = "orange")

points(t15.std$Insurance.Count, t15.std$Personal.Doctor.Count,
       pch = 20, col = "red")

plot(AY.std$Insurance.Count, AY.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "Five Year Average",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.4, pch = 20)

mtext('Personal Doctor Counts v.s. Insurance Counts', outer = TRUE, font = 2, padj = -1, cex = 1.2)

dev.off()









png("plots/Exp_Slope_Conf.png",
    height = 620, width = 960)

plot(x = c(1:6), y = exp(Parameters[,6]), 
     main = "Exponentiated Slope", 
     ylab = NA,
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(1.006, 1.018), 
     pch=20, col = "red",
     axes = FALSE, cex.main = 2, cex = 2, cex.lab = 1)
box()

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "Five Year Average"), cex.axis = 1.2)
axis(2, at = seq(from = 1.004, to = 1.018, by = 0.002), las = 2, cex.axis = 1.2)


segments(c(1:6), exp(Parameters[,5]), c(1:6), exp(Parameters[,7]), lwd = 1.5)
arrows(c(1:6), exp(Parameters[,5]), c(1:6), exp(Parameters[,7]), lwd = 1.5, angle = 90, code = 3, length = 0.03)

dev.off()




















splitter <- function(x){
  
  fitset <- x[1,]
  testset <- x[1,]
  
  for(i in unique(x$x.state)){
    
    dfs <- x[x$x.state==i,]
    n   <- nrow(dfs)
    dfs[,"index"] <- 1:n
    sf <- sample(1:n, size = ceiling(n/2))
    st <- setdiff(1:n, sf)
    
    fitset <- rbind(fitset, dfs[sf,1:4])
    testset <- rbind(testset, dfs[st,1:4])
    
  }
  
  fitset <- fitset[2:nrow(fitset),]
  testset <- testset[2:nrow(testset),]
  
  out <- list(fitset, testset)
  
  out
}














































png("plots/counts_vs_insur.png",
    height = 768, width = 620)

par(mar = c(5.1, 4.1, 2.1, 2.1), oma = c(0,0,3,0))
layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,8,8), 3, 6, byrow = TRUE))


plot(t11.std$Insurance.Count, t11.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2011",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20, col = "blue")

plot(t12.std$Insurance.Count, t12.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2012",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1, cex.main = 1.2, pch = 20, col = "cyan")

plot(t13.std$Insurance.Count, t13.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2013",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20, col = "chartreuse")

plot(t14.std$Insurance.Count, t14.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2014",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20, col = "orange")

plot(t15.std$Insurance.Count, t15.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2015",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20, col = "red")

plot(t11.std$Insurance.Count, t11.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "2011 to 2015",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20, col = "blue")

points(t12.std$Insurance.Count, t12.std$Personal.Doctor.Count,
       pch = 20, col = "cyan")

points(t13.std$Insurance.Count, t13.std$Personal.Doctor.Count,
       pch = 20, col = "chartreuse")


points(t14.std$Insurance.Count, t14.std$Personal.Doctor.Count,
       pch = 20, col = "orange")

points(t15.std$Insurance.Count, t15.std$Personal.Doctor.Count,
       pch = 20, col = "red")

plot(AY.std$Insurance.Count, AY.std$Personal.Doctor.Count,
     ylim = c(60, 90),
     xlim = c(65, 100),
     main = "All Years Combined",
     ylab = "Standardized Personal Doctor Counts",
     xlab = "Standardized Insurance Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20)


plot(t15.std$Insurance.Count - t11.std$Insurance.Count, 
     t15.std$Personal.Doctor.Count - t11.std$Personal.Doctor.Count,
     ylim = c(-25, 20),
     xlim = c(-15, 20),
     main = "Change between 2011 and 2015",
     ylab = "Difference in Personal Doctor Counts",
     xlab = "Difference in Insured Counts",
     cex.lab = 1.2, cex.main = 1.2, pch = 20)

mtext('Personal Doctor Counts v.s. Insurance Counts', outer = TRUE, font = 2, padj = -1, cex = 1.2)

dev.off()





















png("plots/Param_Conf.png",
    height = 400, width = 960)

par(mfrow = c(1,2))

plot(x = c(1:6), y = Parameters[,2], 
     main = "Intercept", 
     ylab = NA,
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(2.95, 3.75), 
     pch=20, col = "red",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "Average"), cex.axis = 1)
axis(2, at = seq(from = 2.90, to = 3.80, by = 0.1), las = 2, cex.axis = 1)

segments(c(1:6), Parameters[,1], c(1:6), Parameters[,3], 
         lwd = 1.5)

arrows(c(1:6), Parameters[,1], c(1:6), Parameters[,3], 
       lwd = 1.5, angle = 90, code = 3, length = 0.05)

##

plot(x = c(1:6), y = Parameters[,6], 
     main = "Slope", 
     ylab = NA,
     xlab = NA,
     xlim = c(1,6), 
     ylim = c(0.006, 0.016), 
     pch=20, col = "red",
     axes = FALSE, cex.main = 1, cex = 1, cex.lab = 1)
box()

axis(1, at = c(1:6), labels = c ("2011", "2012", "2013", "2014", "2015", "Average"), cex.axis = 1)
axis(2, at = seq(from = 0.006, to = 0.016, by = 0.002), las = 2, cex.axis = 1)


segments(c(1:6), Parameters[,5], c(1:6), Parameters[,7], lwd = 1.5)
arrows(c(1:6), Parameters[,5], c(1:6),Parameters[,7], lwd = 1.5, angle = 90, code = 3, length = 0.03)

mtext('95% Confidence Intervals for Model Parameters', 
      outer = T, font = 2, cex = 1.2, adj = .5, padj = 1.5)

dev.off()