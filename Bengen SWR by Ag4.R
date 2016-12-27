# Bengen Sustainable Withdrawal Rates by Age
#
#
# Enter Bengen SWR findings for 10, 15, 20, 25,and 30 years
#
swr <- c(.09,.072,
         .06,.052,.047,.044)
age <- c (10,12.5,15,20,25,30)
lo <- loess(swr ~ age)
plot (x=age,y=swr,main="Bengen SWR by Time Horizon",ylab="Sustainable Withdrawal Rate",xlab="Years Remaining in Retirement")
#lo <- loess(swr ~ age)
#
lines(predict(lo), col='red', lwd=2)