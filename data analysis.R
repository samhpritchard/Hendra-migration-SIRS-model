hist(lomig_time[,2], breaks=seq(0,800,40))

par(mfrow=c(1,2), cex.axis=2)
boxplot(directional_migration[,2], ylim = c(0,1000))
boxplot(directional_migration[,3], ylim = c(0,1000))

c1 <- (low_migration[,2]+low_migration[,3])
c2 <- (high_migration[,2]+high_migration[,3])
c3 <- (directional_migration[,2]+directional_migration[,3])


lmts <- range(0, 1500)

par(mfrow = c(1, 3), cex.axis=2)
boxplot(c1,ylim=lmts)
boxplot(c2,ylim=lmts)
boxplot(c3,ylim=lmts)

c1 <- (lomig_time[,2])
c2 <- (himig_time[,2])
c3 <- (dirmig_time[,2])


lmts <- range(0, 1000)

par(mfrow = c(1, 3), cex.axis=2)
boxplot(c1,ylim=lmts)
boxplot(c2,ylim=lmts)
boxplot(c3,ylim=lmts)

sum_cases<-(high_migration[,2]+high_migration[,3])
hist(sum_cases, breaks=seq(0,1500,40))
large_epi<-high_migration[sum_cases>40 & !is.na(sum_cases),]
hist(large_epi[,1], breaks=seq(0,800,40))
boxplot(large_epi[,1])
boxplot(large_epi)

par(mfrow = c(1, 6), cex.axis=2)
low_success1<-low_migration[,2][!low_migration[,2]<40]
boxplot(low_success1, ylim=lmts)
low_success2<-low_migration[,3][!low_migration[,3]<40]
boxplot(low_success2, ylim=lmts)
high_success1<-high_migration[,2][!high_migration[,2]<40]
boxplot(high_success1, ylim=lmts)
high_success2<-high_migration[,3][!high_migration[,3]<40]
boxplot(high_success2, ylim=lmts)
dir_success1<-directional_migration[,2][!directional_migration[,2]<40]
boxplot(dir_success1, ylim=lmts)
dir_success2<-directional_migration[,3][!directional_migration[,3]<40]
boxplot(dir_success2, ylim=lmts)

print(sd(low_success1))
print(sd(low_success2))
print(sd(high_success1))
print(sd(high_success2))
print(sd(dir_success1))
print(sd(dir_success2))

quantile(low_success1, c(0.025,0.975))
quantile(low_success2, c(0.025,0.975))
quantile(high_success1, c(0.025,0.975))
quantile(high_success2, c(0.025,0.975))
quantile(dir_success1, c(0.025,0.975))
quantile(dir_success2, c(0.025,0.975))

par(mfrow = c(1,3), cex.axis=2)
low_time1<-lomig_time[,2][!lomig_time[,2]<40]
boxplot(low_time1, ylim=lmts)
high_time1<-himig_time[,2][!himig_time[,2]<40]
boxplot(high_time1, ylim=lmts)
dir_time1<-dirmig_time[,2][!dirmig_time[,2]<40]
boxplot(dir_time1, ylim=lmts)