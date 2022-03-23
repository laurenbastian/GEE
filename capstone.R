##LIBRARIES
#install.packages("ggplot2")
#install.packages("cardata")
#install.packages("geepack")
library(ggplot2)
library(catdata)
library(geepack)

##LOAD DATA
#catdata package info found here:
#https://cran.r-project.org/web/packages/catdata/catdata.pdf
#Gerhard Tutz (2012), Regression for Categorical Data, Cambridge University Press
data(knee)

##get descriptives
xtabs(~Th + Sex, data = knee)
mean(knee$Age) #29.5 years on average
mean(knee$Sex) #70.1% female
mean(knee[knee$Th == 1, ]$Age) #placebo has mean age 30.6
mean(as.numeric(knee[knee$Th == 2, ]$Age)) #therapy has mean age 28.5
mean(as.numeric(knee[knee$Th == 1, ]$Sex)) #placebo 73.0% female
mean(as.numeric(knee[knee$Th == 2, ]$Sex)) #therapy 67.2% female

knee$Sex = as.factor(knee$Sex)
knee$Sex = ifelse(knee$Sex == 1, "Female", "Male")
knee$Th = as.factor(knee$Th) #1 = Placebo, 2 = Treatment


##DICHOMOTIZE PAIN, 1 = High pain, 0 = Low pain
for (i in 1:127)
{
  for (j in 5:8)
  {
    if(knee[i,j] >= 3)
    {
      knee[i,j] = 1
    }
    else
    {
        knee[i,j] = 0
    }
  }
}

rm(i, j)

##RESHAPE TO LONG
knee.long = reshape(knee,
                    idvar = "N",
                    varying = 5:8,
                    timevar = "time",
                    v.names = "pain",
                    direction = "long",
                    times = c(0, 3, 7, 10))
knee.long = knee.long[order(knee.long$N),]
knee.long$Sex = as.factor(knee.long$Sex)
knee.long$time = as.factor(knee.long$time)

########################
##EXPLORATORY ANALYSIS

high.R1 = c(nrow(knee[knee$Th == 1  & knee$R1 == 1,]), nrow(knee[knee$Th == 2  & knee$R1 == 1,]))
high.R2 = c(nrow(knee[knee$Th == 1  & knee$R2 == 1,]), nrow(knee[knee$Th == 2  & knee$R2 == 1,]))
high.R3 = c(nrow(knee[knee$Th == 1  & knee$R3 == 1,]), nrow(knee[knee$Th == 2  & knee$R3 == 1,]))
high.R4 = c(nrow(knee[knee$Th == 1  & knee$R4 == 1,]), nrow(knee[knee$Th == 2  & knee$R4 == 1,]))
Th = as.character(c("1", "2"))
high.df = as.data.frame(cbind(Th, high.R1, high.R2, high.R3, high.R4))
                       


cor(knee[,5:8])

#Font
windowsFonts(A = windowsFont("Times New Roman"))




ggplot(knee.long) +
  geom_col(aes(x = time, y = pain, fill = Sex))+
  theme_light() +
  labs(fill = "Sex") +
  scale_fill_manual(labels = c("Female", "Male"),
                    values = c("orchid3", "seagreen3")) +
  xlab("Time in Days") + 
  ylab("# High Pain Reports") +
  theme(text = element_text(family = "A")) +
  ggtitle("High Sports Injury Pain Reports by Sex")

#Pain counts by therapy
ftable(xtabs(~time + pain + Th ,data = knee.long))
therapy.df = as.data.frame(xtabs(~time + pain + Th ,data = knee.long))
therapy.df = therapy.df[order(therapy.df$time),]
therapy.df

ggplot(knee.long) +
  geom_col(aes(x = time, y = pain, fill = Th))+
  theme_light() +
  labs(fill = "Treatment") +
  scale_fill_manual(labels = c("Placebo", "Therapy"),
                    values = c("orchid3", "seagreen3")) +
  xlab("Time in Days") + 
  ylab("# of High Pain Reports") +
  theme(text = element_text(family = "A")) +
  ggtitle("High Sports Injury Pain Reports by Treatment")



##GEE MODELS
##INDEPENDENT
knee.long$time = as.numeric(knee.long$time)
mod.ind = geeglm(pain ~ Th + Age + Sex + time, data = knee.long, 
                    family = binomial,
                    id = N, corstr = "independence")
summary(mod.ind)


##EXCHANGEABLE
mod.ex = geeglm(pain ~ Th + Age + Sex + time, data = knee.long, 
                    family = binomial,
                    id = N, corstr = "exchangeable")
summary(mod.ex)

##AR1
mod.ar = geeglm(pain ~ Th + Age + Sex + time, data = knee.long, 
                    family = binomial,
                    id = N, corstr = "ar1")
summary(mod.ar)



##UNSTRUCTURED
mod.un = geeglm(pain ~ Th + Age + Sex+ time, data = knee.long, 
                    family = binomial,
                    id = N, corstr = "unstructured")
summary(mod.un)

##COMPARE OUTPUT
coef(summary(mod.ind))[,c(1,2,4)]
coef(summary(mod.ex))[,c(1,2,4)]
coef(summary(mod.ar))[,c(1,2,4)]
coef(summary(mod.un))[,c(1,2,4)]

##Pearson residuals vs fitted
par(mfrow = c(2,2))
plot(mod.ind, main = "Independence")
plot(mod.ex, "Exchangeable")
plot(mod.ar, "AR(1)")
plot(mod.un, "Unstructured")

##QIC to choose structure
QIC(mod.ind)
QIC(mod.ex)
QIC(mod.ar)
QIC(mod.un)

#########################
##MODEL SELECTION UNDER AR1
summary(mod.ar)

mod.ar.Thtime = geeglm(pain ~ Th + time, data = knee.long, family = binomial, id = N, corstr = "ar")
summary(mod.ar.Thtime)

anova(mod.ar, mod.ar.Thtime)

par(mfrow = c(1,1))
plot(mod.ar.Thtime)

exp(mod.ar.Thtime$coefficients)

###########################
#ordgee function
#independence
mod.or.ind = ordgee(ordered(pain, levels = c(0,1)) ~ Th + Age + Sex,
                   data = knee.long, 
                   mean.link = "logit",
                   id = N, corstr = "independence")
summary(mod.or.ind)

#exchangeable
mod.or.ex = ordgee(ordered(pain, levels = c(0,1)) ~ Th + Age + Sex,
                   data = knee.long, 
                   mean.link = "logit",
                   id = N, corstr = "exchangeable")
summary(mod.or.ex)

#unstructured
mod.or.un = ordgee(ordered(pain, levels = c(0,1)) ~ Th + Age + Sex,
                  data = knee.long, 
                  mean.link = "logit",
                  id = N, corstr = "unstructured")
summary(mod.or.un)


mod.or.ind$alpha
mod.or.ex$alpha
mod.or.un$alpha


############################
#GEE
#install.packages("gee")
library(gee)

mod.ind2 = gee(pain ~ Th + Age + Sex, family = binomial(logit), data = knee.long, id = N, corstr = "independence")
summary(mod.ind2)

mod.ex2 = gee(pain ~ Th + Age + Sex, family = binomial(logit), data = knee.long, id = N, corstr = "exchangeable")
summary(mod.ex2)

mod.ar2 = gee(pain ~ Th + Age + Sex, family = binomial(logit), data = knee.long, id = N, corstr = "AR-M")
summary(mod.ar2)

mod.un2 = gee(pain ~ Th + Age + Sex, family = binomial(logit), data = knee.long, id = N, corstr = "unstructured")
summary(mod.un2)




