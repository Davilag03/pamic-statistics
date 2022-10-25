setwd("~/R_Analisis/PAMIC/")

#Data loading
dta<-read.csv("Antigua/Scenarios_ANT.csv", dec = ".", header = T, sep = ",")
str(dta)# Shows the structure of data
names(dta) #"BASIN" "POINTS" "ID" "NAME" "SCENARIO" "WYIELD" "EROSION" "N" "P"

summary(dta)
summary(dta$NAME)#number of pixels in the analysis per subbasin. Is non balanced
dta$Id<-as.factor(dta$ID)
############################################
##### Exploration

dummy_stats= data.frame(
  Minimum = apply(dta[,5:7], 2, min, na.rm=T),
  Maximum = apply(dta[,5:7], 2, max, na.rm=T),
  Mean = apply(dta[,5:7], 2, mean, na.rm=T),
  Std_Dev = apply(dta[,5:7], 2, sd, na.rm=T))

print(dummy_stats, print.gap = 3)


#plots show a difference among Subbasin rather than among scenarios
install.packages("lattice")
library(lattice)
bwplot(WYIELD~SCENARIO|BASIN, data=dta)
bwplot(EROSION~SCENARIO|BASIN, data=dta)
bwplot(N~SCENARIO|BASIN, data=dta)
bwplot(P~SCENARIO|BASIN, data=dta)


################ Interaction plot. Seems that the interaction is not relevant 
## but we are goin to test it in the models
interaction.plot(dta$ID,dta$SCENARIO, dta$WYIELD)
interaction.plot(dta$ID,dta$SCENARIO, dta$EROSION)
interaction.plot(dta$ID,dta$SCENARIO, dta$N)
interaction.plot(dta$ID,dta$SCENARIO, dta$P)

############################################
#####Test for parametric assumtions

####### Normality test
mod <- lm(cbind(WYIELD,EROSION,N,P)~ID+SCENARIO, data=dta)
summary(mod)
hist(resid(mod))
shapiro.test(resid(mod)[1:5000])#residuals are not normal

####### Homoscedasticity test
#### model for WATER YIELD
mod <- lm(WYIELD~ID+SCENARIO, data=dta)#No homogenity
boxplot(resid(mod)~ID+SCENARIO,data=dta, main="WATER YIELD")
bartlett.test(resid(mod)~ID,data=dta)
bartlett.test(resid(mod)~SCENARIO,data=dta)

#### model for EROSION
mod <- lm(EROSION~ID+SCENARIO, data=dta)#No homogenity
boxplot(resid(mod)~ID+SCENARIO,data=dta, main="EROSION")
bartlett.test(resid(mod)~ID,data=dta)
bartlett.test(resid(mod)~SCENARIO,data=dta)

#### model for N
mod <- lm(N~ID+SCENARIO, data=dta)#No homogenity
boxplot(resid(mod)~ID+SCENARIO,data=dta, main="N")
bartlett.test(resid(mod)~ID,data=dta)
bartlett.test(resid(mod)~SCENARIO,data=dta)#Here there is homogeneity of variance
boxplot(resid(mod)~SCENARIO,data=dta, main="N per scenario")

#### model for P
mod <- lm(P~ID+SCENARIO, data=dta)#No homogenity
boxplot(resid(mod)~ID+SCENARIO,data=dta, main="P")
bartlett.test(resid(mod)~ID,data=dta)
bartlett.test(resid(mod)~SCENARIO,data=dta)#Here there is homogeneity of variance
boxplot(resid(mod)~SCENARIO,data=dta, main="P per scenario")


####### Independence of variables #  ACP #
install.packages("FactoMineR")
library(FactoMineR)
names(dta)

myacp<-PCA(dta[,c(3,6:9)], quali.sup = 1, graph = F) #we can see that wyield and erosion are negative correlated with N y P.

plot(myacp, choix = "var", title= "Variables correlation")
#we can see that HFWQ is negative correlated with watbalance and WatStress.

#Plots ACP
#plot(myacp, choix = "ind", invisible = "ind", habillage= 1, label = "none", title= "Data variability among subbasins") #gradient of points based on Subassin and scenarios

#myacp<-PCA(dta[,c(2,5:7)], quali.sup = 1, graph=F)
#plot(myacp, choix = "ind", invisible = c("ind", "quali"), habillage=1,legend = F, title= "Data variability among subbasins", ylim =c(-2,3), xlim = c(-1,1) )
#text(myacp$quali.sup$coord[,1:2], labels= as.character(c(1:9)), pos = 2, repel=T)

#summary(myacp) #First 2 dimensions explain 85% of the variance among points.
#The first 2 dimensions are mainly explained by the negative correlation among HFWQ and WatStress
# Eigenvalues
# Dim.1   Dim.2   Dim.3
# 1.661   0.914   0.425

############################################
################################# Kruskal wallis
install.packages("dunn.test")
library(dunn.test)
 
#### model for WATER YIELD
sink("results_Wyield.txt")
kruskal.test(WYIELD~BASIN, data=dta)
dunn.test(dta$WYIELD,dta$BASIN,method = "bonferroni")#The bassins that are no significant different

sink("results_Wyield_SCENARIO.txt")
kruskal.test(WYIELD~SCENARIO, data=dta)
dunn.test(dta$WYIELD,dta$SCENARIO,method = "bonferroni")#All the scenarios are different to the baseline

#### model for EROSION
sink("results_EROSION.txt")
kruskal.test(EROSION~BASIN, data=dta)
dunn.test(dta$EROSION,dta$BASIN,method = "bonferroni") #There are differences for all the bassins except for
#TRI-ESP

sink("results_EROSION_SCENARIO.txt")
kruskal.test(EROSION~SCENARIO, data=dta)
dunn.test(dta$EROSION,dta$SCENARIO,method = "bonferroni")#All scenarios are different respect to the baseline

#### model for N
sink("results_n.txt")
kruskal.test(N~BASIN, data=dta)
dunn.test(dta$N,dta$BASIN,method = "bonferroni")#No difference between  

sink("results_n_SCENARIOS.txt")
kruskal.test(N~SCENARIO, data=dta)
dunn.test(dta$N,dta$SCENARIO,method = "bonferroni")#NS difference among any scenario 

#### model for P
sink("results_P.txt")
kruskal.test(P~BASIN, data=dta)
dunn.test(dta$P,dta$BASIN,method = "bonferroni")#No difference between  

sink("results_P_SCENARIOS.txt")
kruskal.test(P~SCENARIO, data=dta)
dunn.test(dta$P,dta$SCENARIO,method = "bonferroni")#NS difference among any scenario 




##################### Assessment of interaction between Scenario*Subassin
#install.packages("agricolae")
library(agricolae)
#install.packages("car")
require(car)


##### HFWQ
#with interaction
model1<-glm(log(HFWQ+0.5)~Subbasin*Scenario, data = dta)
#Without interaction
model2<-glm(log(HFWQ+0.5)~Subbasin+Scenario, data = dta)
AIC(model1, model2) #AIC is lower with the interaction, we should keep it
#HFWQ is better explained when we keep in the model the subassin and scenario interaction

####Watbalance
#With interaction
model1<-glm(log(Watbalance+0.5)~Subbasin*Scenario, data = dta)
#Without interaction
model2<-glm(log(Watbalance+0.5)~Subbasin+Scenario, data = dta)
AIC(model1, model2) #AIC is lower without the interaction, we drop it and keep the model without interaction

####WatStress
#With interaction
model1<-glm(log(WatStress+0.5)~Subbasin*Scenario, data = dta)
#Without interaction
model2<-glm(log(WatStress+0.5)~Subbasin+Scenario, data = dta)
AIC(model1, model2) #model without interactions has lower AIC. We drop interaction

############################################
####### Boxplots

library(lattice)
bwplot(HFWQ~ factor(dta$ID)|Scenario, data=dta)
bwplot(Watbalance~factor(dta$ID)|Scenario, data=dta)
bwplot(WatStress~factor(dta$Id)|Scenario, data=dta)

