


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