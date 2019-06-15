
#MLE on the Whole Reach 15N addition Denitrification Calculation in Fish Br in 2008
input <- as.data.frame(read.table("Fish15NO3WholeReach2008.txt", header=TRUE, sep="\t"))

#given parameters
Ao=486 # ug/s of 15NO3-N added
k1=0        # uptake rate of 15NO3-N (essentially 0)
k2=0.00168  # rate of gas exchange (loss of N2)
x=input$Distance
y=input$Predicted.N2.ugs

#model
model = nls(y ~ (((kden * Ao) / (k2 - k1)) * ((2.718128 ^ (-1 * k1 * x)) - 
                                                (2.718128 ^ (-1 * k2 * x)))), trace = TRUE, start = c(kden = 0.00001))

summary(model)
p = (coef(model))
get
kden.hat = p[1]

confint(model)
yhat=predict(model)
plot(input$Distance, yhat, ylim=c(0,4))
points(input$Distance, input$Observed.N2.ugs, col="red")

###################
#MLE on the STS Cutoff 15N addition Denitrification Calculation in Fish Br in 2008

input <- as.data.frame(read.table("Fish15NO3STS2008.txt", header=TRUE, sep="\t"))

#given parameters
Ao=20 #ug/l
k1=0.1875 #1/hr
x=input$Time
y=input$Predicted.N2.ugs

#model
model = nls(y ~ (((kden * Ao) / (k2 - k1)) * ((2.718128 ^ (-1 * k1 * x)) -
                                                (2.718128 ^ (-1 * k2 * x)))), trace = TRUE, start = c(kden = 0.01, k2 = 0.0017))
#  (2.718128 ^ (-1 * k2 * x)))), trace = TRUE, start = c(kden = 0.01, k2 = 0.05))

# model assuming re-aeration from whole reach gas addition, k2=0.00168
model = nls(y ~ (((kden * Ao) / (0.00168 - k1)) * ((2.718128 ^ (-1 * k1 * x)) -
                                                     (2.718128 ^ (-1 * 0.00168 * x)))), trace = TRUE, start = c(kden = 0.01))

# model assuming re-aeration = 0
model = nls(y ~ (((kden * Ao) / (0 - k1)) * ((2.718128 ^ (-1 * k1 * x)) -
                                               (2.718128 ^ (-1 * 0 * x)))), trace = TRUE, start = c(kden = 0.01))

summary(model)
p = (coef(model))
get
kden.hat = p[1]
k2.hat = p[2]

confint(model)
yhat=predict(model)
plot(x, yhat, ylim=c(0,0.1))
points(x, input$Observed.N2.ugs, col="red")
summary(lm(yhat ~ y))


#######################################################################################
#######################################################################################
#ANCOVA on the Pond Expts EL, compared to LINX - NOTE CURRENTLY THIS INPUT FILE INCLUDES DENITRIFICATION RESULTS FROM LINX, NOT TOTAL UPTAKE
input.temp <- as.data.frame(read.table("PondExpts_ELplusMMplusLINX.txt", header=TRUE, sep="\t"))

# ANCOVA - compare regression parameters
summary(lm(log(input.temp$Vf.myr) ~ log(input.temp$Conc) * input.temp$Site)) 
mod1 <- aov(log(Vf.myr)~log(Conc)*Site, data = input.temp)
mod2 <- aov(log(Vf.myr)~log(Conc)+Site, data = input.temp)
anova(mod1,mod2)

# Repeat without Pye, since it does not have enough range
input.temp2 <- subset(input.temp, input.temp$Site != c("2-Pye"))
output <- summary(lm(log(input.temp2$Vf.myr) ~ log(input.temp2$Conc) * input.temp2$Site)) 
mod1 <- aov(log(Vf.myr)~log(Conc)*Site, data = input.temp2)
mod2 <- aov(log(Vf.myr)~log(Conc)+Site, data = input.temp2)
anova(mod1,mod2)


# conduct comparison of slope and intercept terms for multi-level model using contrasts, using multcomp package
# see: http://www.inside-r.org/packages/cran/multcomp/docs/Ftest
#   and: http://casoilresource.lawr.ucdavis.edu/drupal/node/716

library(multcomp)
test1 <- aov(log(Vf.myr) ~ log(Conc) + Site, data = input.temp2)
output1 <- (glht(test1, linfct = mcp(Site = "Tukey" )))
plot(print(confint(output1)))
summary(output1)
coef(output1)
vcov(output1)
confint(output1)
plot(output1)

# RESULT of AOV:  slopes do not differ among sites
# RESULT OF GLHT:  the intercepts differ among sites based on Tukeys.  The models for three of the sites (Lubbers, Wildes and Mill) are identical to each other, as are Martins and Chest.  Because the slopes do not differ, differences are in the intercepts.


#######################################################################################
#######################################################################################

# Repeat for kw, INCLUDING linx
# ANCOVA - compare regression parameters
summary(lm(log(input.temp$Kw.perhr) ~ log(input.temp$Conc) * input.temp$Site)) 
mod1 <- aov(log(Kw.perhr)~log(Conc)*Site, data = input.temp)
mod2 <- aov(log(Kw.perhr)~log(Conc)+Site, data = input.temp)
anova(mod1,mod2)

# Repeat without Pye, since it does not have enough range
input.temp2 <- subset(input.temp, input.temp$Site != c("2-Pye"))
output <- summary(lm(log(input.temp2$Kw.perhr) ~ log(input.temp2$Conc) * input.temp2$Site)) 
mod1 <- aov(log(Kw.perhr)~log(Conc)*Site, data = input.temp2)
mod2 <- aov(log(Kw.perhr)~log(Conc)+Site, data = input.temp2)
anova(mod1,mod2)

test1 <- aov(log(Kw.perhr) ~ log(Conc) + Site, data = input.temp2)
output1 <- (glht(test1, linfct = mcp(Site = "Tukey" )))
plot(print(confint(output1)))
summary(output1)
coef(output1)
vcov(output1)
confint(output1)
plot(output1)

# RESULT of AOV:  slopes do not differ among sites
# RESULT OF GLHT:  the models of all sites are similar, except, LINX headwaters differ from all the others, and Lubbers differed from Wildes and SBChest




#######################################################################################
#######################################################################################

#############################
# MLE on Michaelis-Menten
input.temp <- as.data.frame(read.table("PondExpts_MM.txt", header=TRUE, sep="\t"))
input.temp <- as.data.frame(read.table("PondExpts_ELplusMMplusLINX.txt", header=TRUE, sep="\t"))

# 2,4,8 did not show saturation
input <- subset(input.temp, Site==c("1-Andover"))
input <- subset(input.temp, Site==c("2-Pye"))
input <- subset(input.temp, Site==c("3-Wildes"))
input <- subset(input.temp, Site==c("4-Lubbers"))
input <- subset(input.temp, Site==c("5-MillCanal"))
input <- subset(input.temp, Site==c("6-SBChest"))
input <- subset(input.temp, Site==c("7-MartinsCentral"))

x = input$Conc
y = input$Uptake.Obs
model = nls(y ~ ((Umax * x) /(Ks + x)), trace = TRUE, start = c(Umax = 1000, Ks = 0.5))
summary(model)
p = (coef(model))
Umax = p[1]
ks = p[2]
confint(model)
yhat = predict(model)
summary(lm(yhat ~ y))
plot(x,yhat)
points(x,y, col = "red")
p = (coef(model))
Umax = p[1]
ks = p[2]
confint(model)
yhat = predict(model)
plot(yhat, y)
resid <- (yhat -y)
input$resid <- (yhat -y)
t.test(input$resid ~ input$Day.Night)
plot(input$resid ~ input$Day.Night)


Umax <- c(141.4, 1408.812627, 908, 1321.036036, 539.3455848, 381.1388582)

depths <- c(4, 22, 13, 33, 6, 7)

summary(lm(Umax ~ depths))

