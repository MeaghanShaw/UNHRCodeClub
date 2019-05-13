#R club for UNH
#Multivariate stats 101
#with some ggplot2 work throughs

#The dataset includes arbuscular mycorrhizal fungal operational taxonomic unit (OTUs) relative abundances
#in control, garlic mustard invasion, and soil warming plots from the Harvard Forest LTER
#and some environmental co-variates.


#download files from github
#make directory on your computer
#and set the working directory
setwd("/Users/mark/Documents/service/Rworkshop")

#upload the y-matrix
Ymat<- read.csv("master.csv", row.names = 1)
str(Ymat)
#create Xmat from ymat
Xmat<- data.frame(Treatment = Ymat$Treatment, Invasion = Ymat$Invasion, Horizon = Ymat$Horizon, Rep = Ymat$Rep)
#and remove these columns from Ymat
Ymat$Treatment<- NULL
Ymat$Invasion<- NULL
Ymat$Horizon<- NULL
Ymat$Rep<- NULL



############################################################################################################
############################################################################################################
############################################################################################################
#############################################ggplot2 work throughs##########################################
############################################################################################################
############################################################################################################
############################################################################################################
#We will use the vegan package for most analyses
require(vegan)

#First let's look at the distribution of OTUs
hist(colSums(Ymat)) #most taxa are very rare

#Does species richness or Shannon Diversity vary across the treatments?
Rich<- specnumber(Ymat) #calculates species richness
Diversity<- diversity(Ymat) #calcualtes Shannon Diversity Index


#make quick ggplot2 dataframe for analysis
#ggplot works off of dataframes
Rich_plot<- cbind(Xmat, Rich, Diversity)

#require ggplot2
require(ggplot2)

#Set axis order so control is situated at the first position
Rich_plot$Invasion<- factor(Rich_plot$Invasion, levels = c("Uninvaded", "Invaded"))

ggplot(Rich_plot, aes(x = Treatment, y = Rich, fill = Invasion))+
  geom_boxplot()+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+ #set color
  theme_bw() + #remove ugly gray style
  theme(panel.grid.major = element_blank(), #removes grid lines
        panel.grid.minor = element_blank())+ #removes grid lines
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+ #sets x axis unit text
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+ #sets y axis unit text
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+ #sets x axis title text
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+ #sets y axis title text
  labs(x = "Treatment", y = "Arbuscular mycorrhizal fungal richness (# of OTUs)")+ #Custom x and y labels
  theme(legend.title=element_blank())+ #title for legend? this removes it
  theme(legend.key.size = unit(8, 'mm')) #sets the size of the legend and is good to control for across plots

#invasion seems to reduce AMF species richness, especially in the warming plots

#What about Shannon diversity?
ggplot(Rich_plot, aes(x = Treatment, y = Diversity, fill = Invasion))+
  geom_boxplot()+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+ #set color
  theme_bw() + #remove ugly gray style
  theme(panel.grid.major = element_blank(), #removes grid lines
        panel.grid.minor = element_blank())+ #removes grid lines
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+ #sets x axis unit text
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+ #sets y axis unit text
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+ #sets x axis title text
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+ #sets y axis title text
  labs(x = "Treatment", y = "Arbuscular mycorrhizal fungal diversity (Shannon Index)")+ #Custom x and y labels
  theme(legend.title=element_blank())+ #title for legend? this removes it
  theme(legend.key.size = unit(8, 'mm'))#sets the size of the legend and is good to control for across plots

#warming seems to reduces AMF evenness regardless of invasion status

#What about if you wanted to get averages/se/sd/ranges for plotting or producing tables
#use the psych package
require(psych)

#get the average Shannon Index values and summary statistics
Shan.avg<- describeBy(Rich_plot$Diversity, group = list(Rich_plot$Treatment, Rich_plot$Invasion), mat = TRUE)

#set distance between errorbars and bars
dodge<- position_dodge(0.99) #do not let the bars and errorbars overlap

#and now plot
Shan.avg$group2<- factor(Shan.avg$group2, levels = c("Uninvaded", "Invaded"))

ggplot(Shan.avg, aes(x = group1, y = mean, fill = group2))+
  geom_bar(stat = "identity", position = dodge)+
  geom_errorbar(aes(ymax=(mean+se), ymin=(mean-se)), width = 0.15, position = dodge)+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+ #set color
  theme_bw() + #remove ugly gray style
  theme(panel.grid.major = element_blank(), #removes grid lines
        panel.grid.minor = element_blank())+ #removes grid lines
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+ #sets x axis unit text
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+ #sets y axis unit text
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+ #sets x axis title text
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+ #sets y axis title text
  labs(x = "Treatment", y = "Arbuscular mycorrhizal fungal diversity (Shannon Index)")+ #Custom x and y labels
  theme(legend.title=element_blank())+ #title for legend? this removes it
  theme(legend.key.size = unit(8, 'mm')) #sets the size of the legend and is good to control for across plots

#Are they actually different?
#Let's make a model to test this
#make a replicate column

#Let's just make a simple linear mixed model and make room for unequal variance since there are different
#numbers of sampling units across treatments
require(nlme)
str(Rich_plot)
#make base model
lme1<- lme(Diversity ~ (Treatment + Invasion + Horizon)^3, random = ~1|Rep, data = Rich_plot)
qqnorm(lme1) #looks pretty good
shapiro.test(lme1$residuals) #passes

#get results
anova(lme1, type = "sequential")

#Let's see what it looks like with invasion x horizon effect
#since organic horizon comes before mineral soil, set axis positions
Rich_plot$Horizon<- factor(Rich_plot$Horizon, levels = c("Organic", "Mineral"))
ggplot(Rich_plot, aes(x = Treatment, y = Diversity, fill = Invasion))+
  geom_boxplot()+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+ #set color
  theme_bw() + #remove ugly gray style
  theme(panel.grid.major = element_blank(), #removes grid lines
        panel.grid.minor = element_blank())+ #removes grid lines
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+ #sets x axis unit text
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+ #sets y axis unit text
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+ #sets x axis title text
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+ #sets y axis title text
  labs(x = "Treatment", y = "Arbuscular mycorrhizal fungal diversity (Shannon Index)")+ #Custom x and y labels
  theme(legend.title=element_blank())+ #title for legend? this removes it
  theme(legend.key.size = unit(8, 'mm'))+
  facet_wrap(.~Horizon, ncol = 1, strip.position = "right")+
  theme(strip.background = element_rect(color = "white", fill = "white"))+
  theme(strip.background = element_blank())+
  theme(strip.text.x = element_text(size = 10))

############################################################################################################
############################################################################################################
############################################################################################################
#############################################Multivariate stats section#####################################
############################################################################################################
############################################################################################################
############################################################################################################




##################
##################
##################
#PERMANOVA
##################
##################
##################


#since this is community data with many zeros
#and it is relative abundance of taxa
#we will use Bray-Curtis dissimilarity for the analysis, as is quite conventional

#for other types of data, consider Euclidean distance
#e.g. physical data where zeros are true zeros, where there are more values than zeros, 
#and when data fits all assumptions of normality and heteroscedastcity 

#use the vegdist function to convert our Ymat into a Bray-Curtis dissimilarity matrix
AMF_BC<- vegdist(Ymat, method = "bray") #default of is Bray so you do not actually need to write out

#Now let's look at the effects of treatment, invasion, and horizon on community composition 
#we will use PERMANOVA and the adonis function
adonis(AMF_BC ~ (Treatment + Invasion + Horizon)^3, perms = 1000, data = Xmat) #treatment, invasion, and horizon



##################
##################
##################
#NMDS
##################
##################
##################

#Let's look at the treatment effect using NMDS

#first make an NMDS object
AMF_meta<- metaMDS(AMF_BC)
AMF_meta #stress = 0.17, which is quite good. When it is between 25-30 it is getting high
#when stress is basiclly 0 there is likely not enough data for NMDS to find separation

#use ordiplot to make this into an ordination object
AMF_ord<- ordiplot(AMF_meta)

#Now let's extract the useful information
NMDS<- data.frame(AMF_ord$sites)
NMDS

#And let's pair with the Xmat
pNMDS<- cbind(NMDS, Xmat)

#Let's make a basic plot first
#ignoring horizon convex hulls
#use plyr for this
require(plyr)
find_hull<- function(pNMDS) pNMDS[chull(pNMDS$NMDS1, pNMDS$NMDS2), ]
hulls<- ddply (pNMDS, c("Treatment", "Invasion"), find_hull)

#and plot ussing ggplot2
ggplot(pNMDS, aes(x= NMDS1, y = NMDS2, color = Treatment, shape = Invasion)) +
  scale_color_manual(values = c("navyblue", "darkgreen"))+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+
  scale_shape_manual(values = c(24,20))+
  geom_point(size = 6)+
  theme_bw() +
  geom_polygon(data = hulls, alpha = 0.1, show.legend=FALSE)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+
  labs(x = "NMDS1  (stress 0.17)", y = "NMDS2")+
  theme(legend.position = "bottom")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 10, face = "bold"))

#It looks like there are some differences in variation in community composition across replicate plots (beta diversity)
#let's test this using the the betadisper function and 
#for analysis of heterogeneity of variance (dispersion)


##################
##################
##################
#Heterogeneity of variance
##################
##################
##################

#vegdist output x Xmat variables
#make interaction term for treatment x invasion
Xmat$TI<- interaction(Xmat$Treatment, Xmat$Invasion)

disper<- betadisper(AMF_BC, Xmat$TI)
anova(disper) #not different



#so the impacts of invasion and treatment are due to differnece in 
#mean community composition, not heterogeneity of variance

#let's plot the data this way calculating mean values for the axes
#NMDS1
NMDS1<- describeBy(pNMDS$NMDS1, 
                   group = list(pNMDS$Treatment, pNMDS$Invasion),
                   na.rm = TRUE, mat = TRUE)
#NMDS2
NMDS2<- describeBy(pNMDS$NMDS2, 
                   group = list(pNMDS$Treatment, pNMDS$Invasion),
                   na.rm = TRUE, mat = TRUE)

NMDSp<- data.frame(NMDS1 = NMDS1$mean, NMDS2 = NMDS2$mean, Treatment = NMDS1$group1,
                   Invasion = NMDS1$group2,
                   NMDS1se = NMDS1$se, NMDS2se = NMDS2$se)
NMDSp

#set order
NMDSp$Invasion<- factor(NMDSp$Invasion, levels = c("Uninvaded", "Invaded"))

#plot an invasion centric versino of the NMDS
ggplot(NMDSp, aes(x=NMDS1, y = NMDS2, 
                  shape = Invasion, 
                  color = Treatment, 
                  fill = Treatment)) +
  geom_vline(xintercept = 0, color = "red", linetype = 2)+
  geom_hline(yintercept = 0, color = "red", linetype = 2)+
  scale_color_manual(values = c("navyblue", "darkgreen"))+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax=NMDS2+NMDS2se, ymin=NMDS2-NMDS2se))+
  geom_errorbarh(aes(xmax=NMDS1+NMDS1se, xmin=NMDS1-NMDS1se))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+
  labs(x = "NMDS1", y = "NMDS2")+
  theme(legend.position = "right")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 10, face = "bold"))



##################
##################
##################
#Fitting co-variables to NMDS
##################
##################
##################


#Are there environmental properties associated with these treatments?
covars<- read.csv("Co-variates.csv", row.names = 1)
#remove factor columns
covars$Invasion<-NULL
covars$Plot<-NULL
covars$Horizon<-NULL
covars$Treatment<-NULL

#fit environmental variables using the envfit function
#use the Bray matrix for this
fit<- envfit(AMF_BC, env=covars, perms = 1000, na.rm = TRUE)
fit #significant correlations include inorganic N, soil moisture, and soil pH is nearly significant

#Now plot these as loading vectors
envs<- data.frame(Dim1 = fit$vectors$arrows[,1],
                  Dim2 = fit$vectors$arrows[,2],
                  labels = c("Soil pH", "Total soil C", "Soil moisture", "Inorganic N contents",
                             "N-mineralization", "Labile C contents"))

#add center points for plotting at 0,0
envs$Dim3<- c(0,0,0,0,0,0)
envs$sig<- c("Sig", "NS", "Sig", "Sig", "NS", "NS")

#must rescale to fit the plot space
#reduce by 75%
envs$Dim1a<- (0.25*envs$Dim1)
envs$Dim2a<- (0.25*envs$Dim2)

#remove the NS for plotting purposes
envs<- envs[-2,]
envs<- envs[-4,]
envs<- envs[-4,]

#Now add these to the NMDS

#we will use something called ggrepel for this
require(ggrepel)
ggplot(NMDSp, aes(x=NMDS1, y = NMDS2, 
                  shape = Invasion, 
                  color = Treatment, 
                  fill = Treatment)) +
  geom_vline(xintercept = 0, color = "red", linetype = 2)+
  geom_hline(yintercept = 0, color = "red", linetype = 2)+
  scale_color_manual(values = c("navyblue", "darkgreen"))+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax=NMDS2+NMDS2se, ymin=NMDS2-NMDS2se))+
  geom_errorbarh(aes(xmax=NMDS1+NMDS1se, xmin=NMDS1-NMDS1se))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+
  labs(x = "NMDS1", y = "NMDS2")+
  theme(legend.position = "right")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 10, face = "bold"))+
  annotate("segment", x = envs$Dim3, y = envs$Dim3, xend = envs$Dim1a, yend = envs$Dim2a,
           arrow = arrow(length = unit(0.025, 'npc')))+
  annotate("text", x = (0.02+envs$Dim1a), y = (0.01+envs$Dim2a), label = envs$labels)



##################################################################################################
##################################################################################################
####################Are the soil properties also impacted by these treatments?####################
##################################################################################################
##################################################################################################

#let's make a PCoA of the soil properties
#Are there environmental properties associated with these treatments?
covars<- read.csv("Co-variates.csv", row.names = 1)

#remove factor columns
str(covars)
covars
#Let's put everythign on the same scale
covars_log<- log(1+covars)
covars_log

#conver to Euclidean distance
soilp<- vegdist(covars_log, method = "euclidean")

#effect of treatments?
adonis(soilp ~ (Treatment + Invasion + Horizon)^3, perms = 1000, data = Xmat) #No differences
edaphic_disper<- betadisper(soilp, Xmat$TI)
anova(edaphic_disper) #no change in edaphic soil heterogeneity either

#We will still make a pcoa using the pcoa function in ape for practice
require(ape)
pcoa_edaphic<- pcoa(soilp)

pcoa_edaphic #axis 1 explains 63%, axis 2 explains 22%

#get axes
pcoa.df<- data.frame(PCOA1 = pcoa_edaphic$vectors[,1], PCOA2 = pcoa_edaphic$vectors[,2])
pcoa.df<- cbind(pcoa.df, Xmat)
#let's plot this
#let's plot the data this way calculating mean values for the axes
#NMDS1
PCOA1<- describeBy(pcoa.df$PCOA1, 
                   group = list(pcoa.df$Treatment, pcoa.df$Invasion),
                   na.rm = TRUE, mat = TRUE)
#NMDS2
PCOA2<- describeBy(pcoa.df$PCOA2, 
                   group = list(pcoa.df$Treatment, pcoa.df$Invasion),
                   na.rm = TRUE, mat = TRUE)

PCOAp<- data.frame(PCOA1 = PCOA1$mean, PCOA2 = PCOA2$mean, Treatment = PCOA1$group1,
                   Invasion = PCOA1$group2,
                   PCOA1se = PCOA1$se, PCOA2se = PCOA2$se)


#set order
PCOAp$Invasion<- factor(PCOAp$Invasion, levels = c("Uninvaded", "Invaded"))

#plot an invasion centric versino of the NMDS
ggplot(PCOAp, aes(x=PCOA1, y = PCOA2, 
                  shape = Invasion, 
                  color = Treatment, 
                  fill = Treatment)) +
  geom_vline(xintercept = 0, color = "red", linetype = 2)+
  geom_hline(yintercept = 0, color = "red", linetype = 2)+
  scale_color_manual(values = c("navyblue", "darkgreen"))+
  scale_fill_manual(values = c("navyblue", "darkgreen"))+
  geom_point(size = 4)+
  geom_errorbar(aes(ymax=PCOA2+PCOA2se, ymin=PCOA2-PCOA2se))+
  geom_errorbarh(aes(xmax=PCOA1+PCOA1se, xmin=PCOA1-PCOA1se))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.text.y = element_text(family="sans", size = 10, color = "black"))+
  theme(axis.title.x = element_text(family="sans", size = 12, color = "black"))+
  theme(axis.title.y = element_text(family="sans", size = 12, color = "black"))+
  labs(x = "PCoA1 (63%)", y = "PCoA2 (22%")+
  theme(legend.position = "right")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 10, face = "bold"))


#correlating two distance matrices
#using axes versus mantel test

#Use PCoA axis for edaphics and NMDS axis for fungal community
#Is there a linear correlation between AMF communities and edaphic properies?
summary(lm(pcoa.df$PCOA1 ~ NMDS$NMDS1)) #Yes, a weak correlation 


##################
##################
##################
#Mantel test
##################
##################
##################

#PERFORM A MANTEL TEST TO LOOK AT CORRELATION BETWEN MATRICES
mantel(AMF_BC, soilp, perm = 1000) #No correlation


#We now know that soil properties are not necessairly driving variation in fungal communities
#So let's hone in on the taxa that contribute to this variation

##################################################################################################
##################################################################################################
####################Which taxa drive variation in fungal community composition####################
##################################################################################################
##################################################################################################


##################
##################
##################
#Similarity percentage analysis
##################
##################
##################

#For this analysis we will use similarity percentage analysis
#use the raw OTU table for this analysis
sim<- simper(Ymat, Xmat$TI, permutations = 1000)

str(sim)

#Invasion alone?
IvsU<- summary(sim)[[1]]
IvsU

#Warming alone
CvsW<- summary(sim)[[5]]
str(CvsW)
#sort by significant p-values contribution to dissimilarity
CvsW_sort<- CvsW[order(CvsW$p) , ]
CvsW_sort



##################
##################
##################
#Indicator species analysis
##################
##################
##################

#We will use the indicspecies package
require(indicspecies)

#use the multipatt function
IA<- multipatt(Ymat, Xmat$TI, func = "IndVal")

summary(IA, indvalcomp=TRUE) #Here are taxa significantly associated with a particlar treatment
#A and B indicates how frequently that taxon was found in the treatment group and how much fidelity was
#associated to the treatment group, respectively


