###############################################################
#Set-Up Before Coding Design
###############################################################
#Randomization
rmo <- matrix(0,nrow=4,ncol=4)
for(i in 1:4){
  rmo[,i]<-sample(1:4)
}
set.seed(16)
colnames(rmo) <- c("D", "M", "E", "X") #participants
rownames(rmo) <- c("no,G1", "no,G2", "yes,G1", "yes,G2") 
rmo

#######
# Key #
#######
  #no = did not eat breakfast before playing game
  #yes = eat breakfast before playing game
  #G1 = play memory game 1
  #G2 = play memory game 2

#Input Data
mem_score <- c(12, 8, 9, 14,
               7, 11, 10, 9,
               14, 13, 8, 10,
               8, 10, 12, 12)

participant <- rep(c("D", "M", "E", "X"), each = 4)

mgame <- c(rep(c(1,2), 1), rep(c(2,1), 1), 
              rep(c(2,1), 1), rep(c(1,2), 1),
              rep(c(1,2), each = 2),
              rep(c(2,1), each = 2))
              
days <- c(rep(c(1,2), 2),
          rep(c(2,1), each = 2),
          rep(c(2,1), 2),
          rep(c(1,2), 1), rep(c(2,1), 1))

treatments <- vector(length=16) 
  for(i in 1:16) {
    if(days[i]==1) treatments[i]="nobreakfast"
    if(days[i]==2) treatments[i]="breakfast"
}  
       
data <- data.frame(mem_score, participant, mgame, treatments, days)
data

###############################################################
#Original Design: RCBD
###############################################################

treatments <- as.factor(treatments)
blocks <- as.factor(mgame)

m <- lm(mem_score ~ treatments + blocks)
anova(m)

par(mfrow = c(1,2))
boxplot(mem_score ~ treatments, col = 7)
boxplot(mem_score ~ blocks, col = 3)

#Model Assumptions
shapiro.test(m$residuals)
bartlett.test(mem_score, treatments)
bartlett.test(mem_score, blocks)

par(mfrow = c(1,2))
qqnorm(m$residuals, pch = 16, col = 2)
qqline(m$residuals, col = "red")
plot(x = m$fitted.values, y = m$residuals, xlab = "Predicted Values",
     ylab = "Residuals", main = "Residuals vs Predicted",
     col = 4, pch = 16)
abline(h=0, col = 2)

###############################################################
#Revised Design: 2^3 Factorial with replicates within 4 blocks
###############################################################

#Randomization
set.seed(61)
rmo2 <- matrix(sample(1:16), nrow = 4)
colnames(rmo2) <- c("D", "M", "E", "X") 
rownames(rmo2) <- c("no,G1", "no,G2", "yes,G1", "yes,G2") 
rmo2

mem_score2 <- c(14, 7, 13, 11,
               10, 12, 9, 8,
               9, 8, 10, 12,
               10, 12, 14, 8)

participant2 <- rep(c("E", "M", "E", "M",
                     "X", "X", "D", "X",
                     "M", "D", "E", "D",
                     "M", "X", "D", "E"))

mgame2 <- c(rep(c(1,2), 1), rep(c(1), each = 2), 
           rep(c(2,1), 1), rep(c(2), each = 4),
           rep(c(2,1), 1),
           rep(c(1), each = 2), rep(c(1,2), 1))

days2 <- c(rep(c(2), each = 2), rep(c(1,2), 1),
          rep(c(2,1), 1), rep(c(1), 3),
          rep(c(2,1), 1), rep(c(1), 2),
          rep(c(2), 3))

treatments2 <- vector(length=16) 
for(i in 1:16) {
  if(days2[i]==1) treatments2[i]="nobreakfast"
  if(days2[i]==2) treatments2[i]="breakfast"
}  

data2 <- data.frame(mem_score2, participant2, mgame2, treatments2, days2)
data2

A <- treatments2
for(i in 1:16) {
  if(A[i]=="nobreakfast") A[i]=-1
  if(A[i]=="breakfast") A[i]=1
}  
A <- as.numeric(A) 

B <- days2
for(i in 1:16) {
  if(B[i]==1) B[i]=-1
  if(B[i]==2) B[i]=1
}  

C <- mgame2
for(i in 1:16) {
  if(C[i]==1) C[i]=-1
  if(C[i]==2) C[i]=1
}  

AB <- A*B #confounded treatments & days 

A <- as.factor(A) #treatments
B <- as.factor(B) #days
C <- as.factor(C) #games
D <- AB #confounded treatments & days 
reps <- as.factor(participant2) #participant = replicates

data2.1 <- data.frame(A, B, C, D, mem_score2)
data2.1


blocks <- c(rep(c(1,2,3,1), 1),
            rep(c(2,3,4,4), 1),
            rep(c(4,2,4,3), 1),
            rep(c(3,1,1,2), 1))

data2.2 <- data.frame(A, B, C, D, blocks, reps, mem_score2)
data2.2

m2 <- lm(mem_score2 ~ reps + blocks%in%reps + C + D)
anova(m2)

#Model Assumptions
shapiro.test(m2$residuals)
bartlett.test(mem_score2, reps)
bartlett.test(mem_score2, C)


