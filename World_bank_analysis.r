setwd("/media/parth/CE02917E02916BE9/R")    
input=read.csv(("world_bank.csv"))
#View(input) 
names(input)
input=input[,c(3,5)]
#View(input)
#input1=input[,2]
#View(input1)
#extracting the population
population_total=input[1:217,]
population_total
c1=population_total
c2=input[218:(218+216),]
c2
218+216
c3=input[435:(435+216),]
c3
435+216
c4=input[652:(652+216),]
c4
652+216
c5=input[869:(869+216),]
c5
869+216
c6=input[1086:(1086+216),]
c6
1086+216
c7=input[1303:(1303+216),]
c7
1303+216
c8=input[1520:(1520+216),]
c8
1520+216
c9=input[1737:(1737+216),]
c9
1737+216
c10=input[1954:(1954+216),]
c10
1954+216
c11=input[2171:(2171+216),]
c11
2171+216
c12=input[2388:(2388+216),]
c12
2388+216
c13=input[2605:(2605+216),]
c13
2605+216
c14=input[2822:(2822+216),]
c14
2822+216
c15=input[3039:(3039+216),]
c15
3039+216
c16=input[3256:(3256+216),]
c16
3256+216
c17=input[3473:(3473+216),]
c17
3473+216
c18=input[3690:(3690+216),]
c18
3690+216
c19=input[3907:(3907+216),]
c19
3907+216
c20=input[4124:(4124+216),]
c20
4124+216
c21=input[4341:(4341+216),]
c21
4341+216
c22=input[4558:(4558+216),]
c22
4558+216
c23=input[4775:(4775+216),]
c23
4775+216
c24=input[4992:(4992+216),]
c24
4992+216
c25=input[5209:(5209+216),]
c25
5209+216
c26=input[5426:(5426+216),]
c26
5426+216
c27=input[5643:(5643+216),]
c27
5643+216
c28=input[5860:(5860+216),]
c28
5860+216
c29=input[6077:(6077+216),]
c29
6077+216
c30=input[6294:(6294+216),]
c30
6294+216
c31=input[6511:(6511+216),]
c31
6511+216
c32=input[6728:(6728+216),]
c32
6728+216
c33=input[6945:(6945+216),]
c33
6945+216
c34=input[7162:(7162+216),]
c34
7162+216
c35=input[7379:(7379+216),]
c35
7379+216
c36=input[7596:(7596+216),]
c36
7596+216
c37=input[7813:(7813+216),]
c37
7813+216
c38=input[8030:(8030+216),]
c38
8030+216
c39=input[8247:(8247+216),]
c39
8247+216
c40=input[8464:(8464+216),]
c40
c41=input[8681:(8681+216),]
c41
c42=input[8898:(8898+216),]
c42
c43=input[9115:(9115+216),]
c43
c44=input[9332:(9332+216),]
c44
c45=input[9549:(9549+216),]
c45
c46=input[9766:(9766+216),]
c46
c47=input[9983:(9983+216),]
c47
c48=input[10200:(10200+216),]
c48
c49=input[10417:(10417+216),]
c49
c50=input[10634:(10634+216),]
c50
c51=input[10851:(10851+216),]
c51
c52=input[11068:(11068+216),]
c52
c53=input[11285:(11285+216),]
c53
c54=input[11502:(11502+216),]
c54
df=data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,c51,c52,c53,c54)
#removed name column from it
dff=df[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108)]
View(dff)
dff1=dff[-c(5,10,22,28,37,40,51,62,65,69,75,94,103,113,114,131,141,152,158,164,172,183,190,201,211),]
# 53 col is GDP per capita
#now I have (..) value in my data
dff1[dff1==".."]<-NA
#NA value introduced at specific locations
fit=kmeans(na.omit(dff1[,53]),2)
#find NA index
a=subset(dff1, !is.na(dff1[,53]))
# I have used na.omit() function to remove NA values
#a is a new data frame with NA value removed corresponding to per capita
R_df=data.frame()
P_df=data.frame()
a=cbind(a,fit$cluster)
# adding the result of cluster in data frame
#separating the data based on clustser
for(i in 1:nrow(a)){
  if(a[i,56]==1){
    R_df=rbind(R_df,a[i,])    
  }
  else{
    P_df=rbind(P_df,a[i,])
  }
}
#now I have seperated data based on the result of clustering in two different category
View(R_df)
View(P_df)
PCI_mean_R=mean(as.numeric(as.vector(R_df[,53])))
PCI_mean_P=mean(as.numeric(as.vector(P_df[,53])))
barplot(c(PCI_mean_R,PCI_mean_P),xlab = "rich vs poor",col=rainbow(8))
#########################################################333
#population growth
PG_mean_R=mean(as.numeric(as.vector(R_df[,3])))
PG_mean_P=mean(as.numeric(as.vector(P_df[,3])))
barplot(c(PG_mean_R,PG_mean_P),xlab = "rich vs poor",ylab="population growth",col=rainbow(8))
#############################################################
#surface area
SA_mean_R=mean(as.numeric(as.vector(R_df[,4])))
SA_mean_P=mean(as.numeric(as.vector(P_df[,4])))
barplot(c(SA_mean_R,SA_mean_P),xlab = "rich vs poor",ylab="Surface Area",col=rainbow(8))

#################################################################
# life expectancy at birth
LE_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,11]))))
LE_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,11]))))
barplot(c(LE_mean_R,LE_mean_P),xlab = ("Rich vs Poor"),ylab = "Life Expectancy",col = rainbow(8))

#################################################################
#fertility rate
FR_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,12]))))
FR_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,12]))))
barplot(c(FR_mean_R,FR_mean_P),xlab="Rich Vs Poor",ylab = "Fertility rate",col=rainbow(3))

#############################################################

#Adolescent Fertility Rate (15-19)
AFR_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,13]))))
AFR_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,13]))))
barplot(c(AFR_mean_R,AFR_mean_P),xlab="Rich Vs Poor",ylab = "Adolescent Fertility rate",col=rainbow(3))

###############################################################3

# mortality rate
MR_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,16]))))
MR_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,16]))))
barplot(c(MR_mean_R,MR_mean_P),xlab="Rich Vs Poor",ylab = "mortality rate",col=rainbow(3))
pm=as.numeric(as.vector(P_df[149,16]))
im=as.numeric(as.vector(P_df[89,16]))
barplot(c(pm,im),xlab = "pakistan vs india",col=c("black","grey"))

################################################################33

# immunization, measles
IMM_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,18]))))
IMM_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,18]))))
barplot(c(IMM_mean_R,IMM_mean_P),xlab="Rich Vs Poor",ylab = "Immunization,Measles",col=rainbow(3))

####################################################################33
# Primary completion rate
PR_C_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,19]))))
PR_C_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,19]))))
barplot(c(PR_C_mean_R,PR_C_mean_P),xlab="Rich Vs Poor",ylab = "Primary Completion rate",col=rainbow(3))

####################################################################
#school enrollment secondary
#20th column
SES_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,20]))))
SES_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,20]))))
barplot(c(SES_mean_R,SES_mean_P),xlab="Rich Vs Poor",ylab = "School Enrollment Secondary",col=rainbow(3))
######################################################################

# school enrollment primary and secondary with gender parity
# ratio of girls and boys
SE_GP_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,21]))))
SE_GP_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,21]))))
barplot(c(SE_GP_mean_R,SE_GP_mean_P),xlab="Rich Vs Poor",ylab = "Gender Parity in P&S school",col=rainbow(3))

###############################################################

# prevalence of HIV in the population
HIV_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,22]))))
HIV_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,22]))))
barplot(c(HIV_mean_R,HIV_mean_P),xlab="Rich Vs Poor",ylab = "HIV Prevalence",col=rainbow(3))

#########################################################################

# forest area
FA_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,23]))))
FA_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,23]))))
barplot(c(FA_mean_R,FA_mean_P),xlab="Rich Vs Poor",ylab = "Forest Area",col=rainbow(3))

#####################################################################

#improved water source
#column 25
IWS_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,25]))))
IWS_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,25]))))
barplot(c(IWS_mean_R,IWS_mean_P),xlab="Rich Vs Poor",ylab = "Improved water source",col=rainbow(3))

#################################################################

#improved sanitation facility
ISF_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,26]))))
ISF_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,26]))))
barplot(c(ISF_mean_R,ISF_mean_P),xlab="Rich Vs Poor",ylab = "improved sanitation facility",col=rainbow(3))

############################################################3

# energy use
EU_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,27]))))
EU_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,27]))))
barplot(c(EU_mean_R,EU_mean_P),xlab="Rich Vs Poor",ylab = "Energy Use",col=rainbow(3))

################################################################

# GDP growth
GDP_G_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,31]))))
GDP_G_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,31]))))
barplot(c(GDP_G_mean_R,GDP_G_mean_P),xlab="Rich Vs Poor",ylab = "GDP growth",col=rainbow(3))
#####################################################################

#inflation rate
INF_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,32]))))
INF_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,32]))))
barplot(c(INF_mean_R,INF_mean_P),xlab="Rich Vs Poor",ylab = "Inflation",col=rainbow(3))

####################################################################

#agriculture value added
AG_VA_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,33]))))
AG_VA_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,33]))))
barplot(c(AG_VA_mean_R,AG_VA_mean_P),xlab="Rich Vs Poor",ylab = "Agriculture value added",col=rainbow(3))

###############################################################

#industry value added
IVA_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,34]))))
IVA_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,34]))))
barplot(c(IVA_mean_R,IVA_mean_P),xlab="Rich Vs Poor",ylab = "industry value added",col=rainbow(3))

############################################################

#service value added
SVA_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,35]))))
SVA_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,35]))))
barplot(c(SVA_mean_R,SVA_mean_P),xlab="Rich Vs Poor",ylab = "service value added",col=rainbow(3))

##############################################################
# export of good
EOG_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,36]))))
EOG_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,36]))))
barplot(c(EOG_mean_R,EOG_mean_P),xlab="Rich Vs Poor",ylab = "Export of Good",col=rainbow(3))

######################################################################

#military expenditure
ME_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,42]))))
ME_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,42]))))
barplot(c(ME_mean_R,ME_mean_P),xlab="Rich Vs Poor",ylab = "Military Expenditure",col=rainbow(3))

################################################################

#MObile subscription
MCS_mean_R=mean(as.numeric(as.vector(na.omit(R_df[,43]))))
MCS_mean_P=mean(as.numeric(as.vector(na.omit(P_df[,43]))))
barplot(c(MCS_mean_R,MCS_mean_P),xlab="Rich Vs Poor",ylab = "Mobile Cellular Suscription",col=rainbow(3))

