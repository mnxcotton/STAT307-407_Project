############################################################
#Set-Up Before Coding Design
############################################################
#Randomization
rmo <- matrix(0,nrow=4,ncol=4)
for(i in 1:4){
  rmo[,i]<-sample(1:4)
}
set.seed(16)
rmo

#Input Data
mem_score <- c(12, 8, 9, 14,
               7, 11, 10, 9,
               14, 13, 8, 10,
               8, 10, 12, 12)

participant <- rep(c("D", "M", "E", "X"), each = 4)
participant

mem_game <- c(rep(c(1,2), 1), rep(c(2,1), 1), 
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
treatments
       
data <- data.frame(mem_score, participant, mem_game, treatments, days)
data

############################################################
#Initial Design: RCBD
############################################################

treatments <- as.factor(treatments)
blocks <- as.factor(mem_game)

m <- lm(mem_score ~ treatments + blocks)
anova(m)

shapiro.test(m$residuals)
bartlett.test(mem_score, treatments)
bartlett.test(mem_score, blocks)

############################################################
#Revised Design: 2^3 Factorial with 4 blocks as replicates 
############################################################

treatments <- as.character(treatments)
A <- treatments
for(i in 1:16) {
  if(A[i]=="nobreakfast") A[i]=-1
  if(A[i]=="breakfast") A[i]=1
}  
A <- as.numeric(A) 

B <- days
for(i in 1:16) {
  if(B[i]==1) B[i]=-1
  if(B[i]==2) B[i]=1
}  

C <- mem_game
for(i in 1:16) {
  if(C[i]==1) C[i]=-1
  if(C[i]==2) C[i]=1
}  

AB <- A*B #confounded treatments & days 

A <- as.factor(A) #treatments
B <- as.factor(B) #days
C <- as.factor(C) #games
D <- AB #confounded treatments & days 
E <- as.factor(participant) #participant = replicates

data2 <- data.frame(A, B, C, D, E, mem_score)

blocks <- c(rep(c(1,2,3,4), 1),
            rep(c(2,4,1,3), 1),
            rep(c(4,1,2,3), 1),
            rep(c(3,2,4,1), 1))

data3 <- data.frame(A, B, C, D, blocks, mem_score)
data3

m2 <- lm(mem_score ~ E + blocks%in%E + C)
anova(m2)

shapiro.test(m2$residuals)

bartlett.test(mem_score, E)
bartlett.test(mem_score, C)

