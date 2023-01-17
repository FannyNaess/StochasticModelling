# ==============================================================================
# Insurance claims : Simulation of The poisson process:
# ======================  =======================================================
library(ggpubr)
n=1000
rep = 10
#Function for the poisson distribution
success<-0
for (i in 1:n) {
  val<-rpois(1,88.5)
  if (val > 100){
    success<-success+1
  }
}
success/n

#Figure for discussion
claim_list<-array(data = NA, dim = c(60,10))
claim_list[1,]<-rep(0,10)
time<-60

for (i in 2:time) {
  claim_list[i,]<-rpois(10,lambda = 1.5)+claim_list[i-1,]
}

gg_color_hue <- function(n) {
  "Creats a vector of n length with color hues"
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors<-rep(0,10)
for (i in 1:10){
  colors[i]<-toString(i)
}


cols<-gg_color_hue(10)
data<-data.frame(claim_list)
gg_insurance<-ggplot(data,aes(x = 1:60))+
  #This was supposed to be a for loop, but R didn't like that
  geom_line(aes(y=claim_list[,1],color = "1"))+
  geom_line(aes(y=claim_list[,2],color = "2"))+
  geom_line(aes(y=claim_list[,3],color = "3"))+
  geom_line(aes(y=claim_list[,4],color = "4"))+
  geom_line(aes(y=claim_list[,5],color = "5"))+
  geom_line(aes(y=claim_list[,6],color = "6"))+
  geom_line(aes(y=claim_list[,7],color = "7"))+
  geom_line(aes(y=claim_list[,8],color = "8"))+
  geom_line(aes(y=claim_list[,9],color = "9"))+
  geom_line(aes(y=claim_list[,10],color = "10"))+
  labs(x = "Time", y = "Realizations")+
  scale_colour_manual("",
                      breaks = colors,
                      values = cols)
gg_insurance


# ==============================================================================
# Total claim amount
# =============================================================================
rep=1000
value=c()
#Function for the poisson distribution
success<-0
for (i in 1:rep) {
  
  val<-rpois(1,88.5)
  vec <- rexp(val,rate=10)
  total <- sum(vec)
  
  if(total > 8){
    success<-success +1
  }
}
success/rep


claim_list<-array(data = NA, dim = c(60,10))
claim_list[1,]<-rep(0,10)
claim_list
for (i in 2:60) {
  val<-rpois(10,1.5)
  amount<-rep(0,10)
  for (j in 1:10){
    a<-rexp(val[j],rate=10)
    amount[j]<-sum(a)
  }
  claim_list[i,]<-amount+claim_list[i-1,]
}
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colors<-rep(0,10)
for (i in 1:10){
  colors[i]<-toString(i)
}
colors

cols<-gg_color_hue(10)
data<-data.frame(claim_list)
gg_total<-ggplot(data,aes(x = 1:60))+
  geom_line(aes(y=claim_list[,1],color = "1"))+
  geom_line(aes(y=claim_list[,2],color = "2"))+
  geom_line(aes(y=claim_list[,3],color = "3"))+
  geom_line(aes(y=claim_list[,4],color = "4"))+
  geom_line(aes(y=claim_list[,5],color = "5"))+
  geom_line(aes(y=claim_list[,6],color = "6"))+
  geom_line(aes(y=claim_list[,7],color = "7"))+
  geom_line(aes(y=claim_list[,8],color = "8"))+
  geom_line(aes(y=claim_list[,9],color = "9"))+
  geom_line(aes(y=claim_list[,10],color = "10"))+
  labs(x = "Time", y = "Total amount")+
  scale_colour_manual("",
                      breaks = colors,
                      values = cols)
gg_total