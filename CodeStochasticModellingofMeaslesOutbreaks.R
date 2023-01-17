library(ggplot2)
library(ggpubr)
library(Rmisc)
library(expm)

#Stochastic Modelling of measles outbreaks and insurance claims

#Measles Outbreaks

#Markov matrix - Transition probability matrix

P<-rbind(c(0.99,0.01,0),c(0,0.9,0.1),c(0.005,0,0.995))
P%^%2#To check if P is regular.
#1)
sim_vec<-function(P,n,inital){
  '
  Computes a vector that simulate the statetransitions,
  where x[t] is the state at time t.
  :param P: 3x3 probability matrix.
  :param n: The total time to simulate.
  :param inital: Inital state.
  :return: The state vector.
  '
  x<-rep(0,n)
  x[1]<-inital
  for(i in 2:n){
    u = runif(1)
    if(u<P[x[i-1],1]){
      x[i]<-1
    }else if (u<(P[x[i-1],1]+P[x[i-1],2])){
      x[i]<-2
    }else{
      x[i]<-3
    }
  }
  return(x)
}

#Plot of the 7300 day (2 year) simulation.
days = 7300
x<-c(1:days)
y<-sim_vec(P,days,1)
plot(x,y,type="l")

#2)
sick_dist<-function(x,days){
  "Returns a vector with respectively number of
  susceptible,infected and recovering.
  "
  return(c(sum(x==1),sum(x==2),sum(x==3))/days)
}

days1 = 7300
days2 = 3650
x1<-sim_vec(P,days1,1)
x2<-sim_vec(P,days2,1)
data1<-data.frame(x1,x2)

#Visual representation
h1<-ggplot(data1,aes(x1))+
  ggtitle("Distribution after 10 years")+
  geom_histogram()+
  xlab("State")+
  geom_bar()+
  theme_classic()+

h2<-ggplot(data1,aes(x2), col = I("red"), fill = I("blue") )+
  ggtitle("Distribution after 20 years")+
  geom_histogram()+
  xlab("State")+
  geom_bar()+
  theme_classic()
h1
h2

#The distribution of respectively susceptible,
#infected and recovering individuals after 10 and 20
#years.
dist1<-sick_dist(x1,days1) #Etter 10 er
dist2<-sick_dist(x2,days2) #Etter 20 er
dist1
dist2

#3)
N = 30 
days = 3650 #Write 3650 for the 10 year simulation 
#and 7300 for 20 years.
#Probability of beeing respectively susceptible,
#infected eller recovered.
prob_sus<-prob_inf<-prob_rec<-vector("numeric",N)
for(i in 1:N){
  x<-sim_vec(P,days,1)
  prob_sus[i]<-sum(x==1)/days
  prob_inf[i]<-sum(x==2)/days
  prob_rec[i]<-sum(x==3)/days
}

#Calculating the confidence intervals assuming t-distribution

#Manual calculation
#Mean and sd
sus_mean<-mean(prob_sus)
sus_sd<-sd(prob_sus)
inf_mean<-mean(prob_inf)
inf_sd<-sd(prob_inf)
rec_mean<-mean(prob_rec)
rec_sd<-sd(prob_rec)
#Error
sus_me<-qt(0.975,N-1)*sus_sd/sqrt(N)
inf_me<-qt(0.975,N-1)*inf_sd/sqrt(N)
rec_me<-qt(0.975,N-1)*rec_sd/sqrt(N)
#CI is then
cat("CI for susceptible: ",c(sus_mean-sus_me,sus_mean+sus_me),"\n")
cat("CI for infected: ",c(inf_mean-inf_me,inf_mean+inf_me),"\n")
cat("CI for recovering: ",c(rec_mean-rec_me,rec_mean+rec_me),"\n")

#Using t.test or CI from Rmisc to confirm 
#CI using t.test:
t.test(prob_sus)$conf[1:2]#Susceptible
t.test(prob_inf)$conf[1:2]#Infected
t.test(prob_rec)$conf[1:2]#Recovering
#CI using the "CI" function from the Rmisc library.
#Gives the same result
CI(prob_sus,ci = 0.95)
CI(prob_inf,ci = 0.95)
CI(prob_rec,ci = 0.95)

# 1.5: Development through phases of the measles outbreak

N<-300#Number of repetitions/time
pop<-1000#Total population (sum(Y))
Y<-c(950,50,0)
gamma<-0.1#Prob. that inf->rec
alph<-0.005#Prob. that rec->sus
beta<-0.5#Prob. that sus->inf
sus_vec<-inf_vec<-rec_vec<-c()

for (i in 1:N){
  #Calculate the current prob. that sus->inf
  #each iteration
  beta_n = Y[2]*beta/pop
  inf<-rbinom(1,Y[1],beta_n)
  rec<-rbinom(1,Y[2],gamma)
  sus<-rbinom(1,Y[3],alph)
  Y[1]<-Y[1]+sus-inf
  sus_vec<-append(sus_vec,Y[1])
  Y[2]<-Y[2]+inf-rec
  inf_vec<-append(inf_vec,Y[2])
  Y[3]<-Y[3]+rec-sus
  rec_vec<-append(rec_vec,Y[3])
}

data_development<-data.frame(sus_vec,inf_vec,rec_vec)
gg_development<-ggplot(data_development,aes(x=1:N))+
  geom_line(aes(y=sus_vec, color = "susceptible"))+
  geom_line(aes(y=inf_vec, color = "infected"))+
  geom_line(aes(y=rec_vec, color = "recovering"))+
  labs(x = "time", y = "individuals")+
  scale_colour_manual("",
                      breaks = c("susceptible", "infected","recovering"),
                      values = c("blue","red","green"))
gg_development


#Individuals infected for different vaccination ratio

#1)
N<-300#Time periode
gamma<-0.1
alph<-0.005
beta<-0.5
#Vector of max number individuals infected 
#and the time at witch it is achieved
inf_max_vec<-time_vec<-c()
rep<-1000#Number of simulations
for (j in 1:rep){
  inf_max<-50
  time<-0
  Y<-c(950,50,0)
  for (i in 1:N){
    beta_n = Y[2]*beta/sum(Y)
    inf<-rbinom(1,Y[1],beta_n)
    rec<-rbinom(1,Y[2],gamma)
    sus<-rbinom(1,Y[3],alph)
    Y[1]<-Y[1]+sus-inf
    Y[2]<-Y[2]+inf-rec
    Y[3]<-Y[3]+rec-sus
    if(Y[2]>inf_max){
      time<- i
      inf_max<-Y[2]
    }
  }
  inf_max_vec<-append(inf_max_vec,inf_max)
  time_vec<-append(time_vec,time)
}
inf_mean<-mean(inf_max_vec)#Mean max infected
time_mean<-mean(time_vec)#Mean time
cat("Mean max infected is: ",inf_mean,"\n")
cat("Mean time befor max number of infected is: ",time_mean,"\n")

#2)
inf_sd<-sd(inf_max_vec)
time_sd<-sd(time_vec)
#Error
inf_error<-qt(0.975,df = rep-1)*inf_sd/sqrt(rep)
time_error<-qt(0.975,df = rep-1)*time_sd/sqrt(rep)
#Manual
cat("CI for max infected:",c(inf_mean-inf_error,inf_mean+inf_error),"\n")
cat("CI for time befor max infected:",c(time_mean-time_error,time_mean+time_error),"\n")
#CI with t.test
cat("95% CI for individuals: ",t.test(inf_max_vec)$conf[1:2],"\n")
cat("95% CI for time: ",t.test(time_vec)$conf[1:2],"\n")

#A major interest in the modelling of infectious diseases lies in the explosive behaviour during the initial outbreak of the disease.


# Estimating the expected maximum number of infected individuals during the simulated time steps
N<-300
pop<-1000
vac_0<-c(950,50,0)
vac_100<-c(850,50,0)
vac_600<-c(350,50,0)
vac_800<-c(150,50,0)
Y<-rbind(vac_0,vac_100,vac_600,vac_800)
gamma<-0.1
alph<-0.005
beta<-0.5
#2D array containing number of infected for each group
#1-4 at each time step.
inf_vec<-array(data = NA, dim = c(N,4))

for (i in 1:N){
  for (j in 1:4){
    beta_n = Y[j,2]*beta/pop
    inf<-rbinom(1,Y[j,1],beta_n)
    rec<-rbinom(1,Y[j,2],gamma)
    sus<-rbinom(1,Y[j,3],alph)
    Y[j,1]<-Y[j,1]+sus-inf
    Y[j,2]<-Y[j,2]+inf-rec
    Y[j,3]<-Y[j,3]+rec-sus
    inf_vec[i,j]<-Y[j,2]
  }
}
#Plot
data_infected<-data.frame(inf_vec[,1],inf_vec[,2],inf_vec[,3],inf_vec[,4])

gg_infected<-ggplot(data,aes(x=1:N))+
  geom_line(aes(y=inf_vec[,1], color = "0 vacsinated"))+
  geom_line(aes(y=inf_vec[,2], color = "100 vacsinated"))+
  geom_line(aes(y=inf_vec[,3], color = "600 vacsinated"))+
  geom_line(aes(y=inf_vec[,4], color = "800 vacsinated"))+
  labs(x = "time", y = "individuals")+
  scale_colour_manual("",
                      breaks = c( "0 vaccinated", "100 vaccinated","600 vaccinated","800 vaccinated"),
                      values = c("cyan","red","green","blue"))

gg_infected

#Calculating mean and CI for 100, 600 and 800 
#vaccinations. 
rep<-1000
tot_max_inf<-array(data = NA, dim = c(rep,3))
tot_max_time<-array(data = NA, dim = c(rep,3))
for(k in 1:rep){
  vac_100<-c(850,50,0)
  vac_600<-c(350,50,0)
  vac_800<-c(150,50,0)
  Y<-rbind(vac_100,vac_600,vac_800)
  max_inf<-c(50,50,50)
  max_time<-c(0,0,0)
  for(i in 1:N){
    for(j in 1:3){
      beta_n = Y[j,2]*beta/pop
      inf<-rbinom(1,Y[j,1],beta_n)
      rec<-rbinom(1,Y[j,2],gamma)
      sus<-rbinom(1,Y[j,3],alph)
      Y[j,1]<-Y[j,1]+sus-inf
      Y[j,2]<-Y[j,2]+inf-rec
      Y[j,3]<-Y[j,3]+rec-sus
      if(max_inf[j]<Y[j,2]){
        max_inf[j]<-Y[j,2]
        max_time[j]<-i
      }
    }
  }
  for(j in 1:3){
    tot_max_time[k,j]<-max_time[j]
    tot_max_inf[k,j]<-max_inf[j]
  }
}

# How would we use them to assess the potential severity of the outbreak
# Trail 1-3 corresponds to
#100, 600 and 800 vaccinated, respectively.
for(i in 1:3){
  cat("Mean max infected for trail",i," is:\n")
  print(mean(tot_max_inf[,i]))
  cat("Mean time befor max infected for trail",i," is:\n")
  print(mean(tot_max_time[,i])[1])
}
#Manually
for (i in 1:3){
  inf_sd<-sd(tot_max_inf[,i])
  inf_error<-qt(0.975,df = rep)*inf_sd/sqrt(rep)
  time_sd<-sd(tot_max_time[,i])
  time_error<-qt(0.975,df = rep)*time_sd/sqrt(rep)
  cat("Max infected confidence interval for trail",
      i,"is:\n")
  print(c(mean(tot_max_inf[,i])-inf_error,mean(tot_max_inf[,i])+inf_error))
  cat("Time befor max infected confidence interval for trail",
      i,"is:\n")
  print(c(mean(tot_max_time[,i])-time_error,mean(tot_max_time[,i])+time_error))
}

#With t.test
for (i in 1:3){
  cat("Max infected confidence interval for trail",
      i,"is:\n")
  print(t.test(tot_max_inf[,i])$conf[1:2])
  cat("Time befor max infected confidence interval for trail",
      i,"is:\n")
  print(t.test(tot_max_time[,i])$conf[1:2])
}

