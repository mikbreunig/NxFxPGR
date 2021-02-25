

setwd("C:/Users/mbreu/OneDrive - Michigan State University/Fertility 033120/")

ipak <- function( pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("agricolae", "ggpubr","pwr","broom","stringr", "tidyverse","ggplot2", "drc","lattice","car", "lme4", "lsmeans", "plyr", "plotrix", "knitr", "ggplot2", "lmtest", "lmerTest", "Rmisc", "gridExtra", "plotly", "webshot", "ggpmisc", "ggsci","scales")
ipak(packages)


Fdata<-read_delim(file = "fungicideplots_metric.csv",delim=",",na=".")
reorder
Ndata<-read_delim(file = "NitrogenPlots.csv",delim=",",na=".")

levels(Fdata$treatment)
sapply(Fdata)
Fdata$treatment <- factor(Fdata$treatment , levels=c("Fks 6", "Fks 10.5.1", "Fks 6 + Fks 10.5.1"))


ggplot(data=Fdata,aes(x=treatment,y=estimate))+
  facet_grid(cols=vars(year))+
  #geom_col(position="Dodge")+
  #geom_dotplot(binaxis="y", stackdir = "center")+
  #facet_grid(row=vars(treatment),cols = vars(resistance),
  #scales = "free",)+
  geom_pointrange(aes(ymin=Lower, ymax=Upper), size=0.7)+
  theme(axis.text.x=element_text(size=16,angle=90,hjust=0,vjust=0.5),axis.title.x=element_blank(),axis.text.y=element_text(size=16))+
  #theme(axis.title.x = element_text(size=12))+
  #ylab("Yield increase from fungcide application (kg/ha)")+
  #ylim(-12,20)+
  #ylab(" ")+
  theme(strip.text.x = element_text(size = 14))+
  geom_hline(aes(yintercept=0),linetype="dashed")+
coord_flip()

ggplot(data=Fdata,aes(y=treatment, x=foliar_estimate))+
  facet_grid(cols=vars(year))+
  #geom_col(position="Dodge")+
  #geom_dotplot(binaxis="y", stackdir = "center")+
  #facet_grid(row=vars(treatment),cols = vars(resistance),
  #scales = "free",)+
  geom_pointrange(aes(xmin=f_lower, xmax=f_upper), size=0.7)+
  theme(axis.text.x=element_text(size=16,angle=90,hjust=0,vjust=0.5),axis.title.x=element_blank(),axis.text.y=element_text(size=16))+
  #theme(axis.title.x = element_text(size=12))+
  xlim(0,60)+
  #xlab("Fungicide Treatment")+
  ylab("Final Area of Flag Leaf Infected (%)")+
  theme(strip.text.x = element_text(size = 14))

ggplot(data=Fdata,aes(y=treatment,x=estimate))+
  facet_grid(rows=vars(year))+
  geom_pointrange(aes(xmin=Lower, xmax=Upper), size=0.7)+
  theme(axis.text.x=element_text(size=16,angle=90,hjust=0,vjust=0.5),axis.title.x=element_blank(),axis.text.y=element_text(size=16))+
  #theme(axis.title.x = element_text(size=12))+
  xlab("Yield increase (kg/ha)")+
  #xlim(-12,20)+
  ylab(" ")+
  theme(strip.text.x = element_text(size = 14))+
  geom_vline(aes(xintercept=0),linetype="dashed")+
  theme_bw()

ggplot(data=Ndata,aes(y=year,x=`estimate_yield `))+
  #facet_grid(rows=vars(year))+
  #geom_col(position="Dodge")+
  #geom_dotplot(binaxis="y", stackdir = "center")+
  #facet_grid(row=vars(treatment),cols = vars(resistance),
  #scales = "free",)+
  geom_pointrange(aes(xmin=Lower, xmax=Upper), size=0.7)+
  theme(axis.text.x=element_text(size=16,angle=90,hjust=0,vjust=0.5),axis.title.x=element_blank(),axis.text.y=element_text(size=16))+
  #theme(axis.title.x = element_text(size=12))+
  xlab("Yield difference between high and low nitrogen (bu/ac)")+
  xlim(-20,10)+
  ylab(" ")+
  theme(strip.text.x = element_text(size = 14))+
  geom_vline(aes(xintercept=0),linetype="dashed")

View(Ndata)
##############################################################
YieldN<-read_delim(file = "Yield_estimates.csv",delim=",",na=".")
YieldN$trt <- factor(YieldN$trt , levels=c("low N", "high N", "high N + PGR"))


ggplot(data=YieldN,aes(x=trt,y=yield_estimate))+
  facet_grid(cols=vars(year))+
  #geom_col(position="Dodge")+
  #geom_dotplot(binaxis="y", stackdir = "center")+
  #facet_grid(row=vars(treatment),cols = vars(resistance),
  #scales = "free",)+
  geom_crossbar(aes(ymin=lower, ymax=upper), size=0.2)+
  theme(axis.text.x=element_text(size=16,angle=90,hjust=0,vjust=0.5),axis.title.x=element_blank(),axis.text.y=element_text(size=16))+
  #theme(axis.title.x = element_text(size=12))+
  xlab("Yield difference between high and low nitrogen (bu/ac)")+
  ylim(0,125)+
  ylab(" ")+
  theme(strip.text.x = element_text(size = 14))+
  geom_vline(aes(xintercept=0),linetype="dashed")


#########################################################################
LodgedPlots$Treatment<-as.factor(LodgedPlots$Treatment)



ggplot() + 
  geom_pointrange(data=Fdata, mapping=aes(x=treatment, y=estimate, ymin=Lower, ymax=Upper), width=0.2, size=1, color="blue", fill="white", shape=22)+
  facet_wrap(~year)

View(LodgedPlots)
ggplot(data=LodgedPlots, aes(x=Treatment, y=SevDiff))+
  geom_boxplot(aes(x=Treatment, y=SevDiff),binaxis="y",stackdir = "center")+
  annotate(geom="text",x=1,y=30, label="A")+
annotate(geom="text",x=2,y=23, label="AB")+
annotate(geom="text",x=3,y=12, label="BC")+
annotate(geom="text",x=4,y=-1, label="C")+
annotate(geom="text",x=5,y=30, label="A")+
  annotate(geom="text",x=6,y=23, label="AB")+
annotate(geom="text",x=7,y=-1, label="C")+
annotate(geom="text",x=8,y=-1, label="C")+
  ylab("Difference in FHB Severity (%)")+
  theme_bw()+
  theme(axis.text.x=element_text(size=14),
        axis.text.y=element_text(size=14),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))
  
  
LodgedPlots<-read_delim(file = "LodgedCSV.csv",delim=",",na=".")



##################################

Lodging<-read_delim(file = "2018_csv.csv",delim=",",na=".")

View(Lodging)
FHBSet<-Lodging %>% 
  filter(Diff_DI>0) %>% 
  mutate(AdjDiff_DI=Diff_DI*100)

Flag<-Lodging %>% 
  filter(JunNon_Flag>0) %>% 
  mutate(Diff_Flag=JunLodged_Flag-JunNon_Flag)
View(Flag)

DI<-ggplot(data=FHBSet)+
  geom_histogram(aes(x=AdjDiff_DI),binwidth = 5,color="grey")+
  theme(axis.text.x=element_text(size=12,angle=0,hjust=0.5,vjust=0.5), axis.text.y=element_text(size=12))+
  theme(axis.title.x = element_text(size=12),axis.title.y=element_text(size=12))+
  ylim(0,6)+
  xlim(-40,60)+
  xlab("Difference in FHB Incidence between Lodged and Non-lodged portions")+
  ylab("No. of plots")+
  geom_vline(aes(xintercept=0),linetype="dashed")

  
DS<-ggplot(data=FHBSet)+
  geom_histogram(aes(x=Diff_DS),binwidth = 5,color="grey")+
  theme(axis.text.x=element_text(size=12,angle=0,hjust=0.5,vjust=0.5), axis.text.y=element_text(size=12))+
  theme(axis.title.x = element_text(size=12),axis.title.y=element_text(size=12))+
  ylim(0,6)+
  xlim(-40,60)+
  xlab("Difference in FHB severity between Lodged and Non-lodged portions")+
  ylab("No. of plots")+
  geom_vline(aes(xintercept=0),linetype="dashed")


flag<-ggplot(data=Flag)+
  geom_histogram(aes(x=Diff_Flag),binwidth = 5,color="grey")+
  theme(axis.text.x=element_text(size=12,angle=0,hjust=0.5,vjust=0.5), axis.text.y=element_text(size=12))+
  theme(axis.title.x = element_text(size=12),axis.title.y=element_text(size=12))+
  ylim(0,6)+
  xlim(-40,60)+
  xlab("Difference in Flag Leaf Disease between Lodged and Non-lodged portions")+
  ylab("No. of plots")+
  geom_vline(aes(xintercept=0),linetype="dashed")

figure <- ggarrange(DI, DS, flag,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)


#######################################################################3


Early<-read_delim(file = "EarlyLodging.csv",delim=",",na=".")

leveneTest(lodging_assess ~ app, Early, center=mean)

anova(lodging_assess~ app + (1|block))


Model<-lmer(lodging_assess ~ app + (1|block),
                    data=Early,
                    REML=TRUE)
anova.results<-anova(Model)

results<-emmeans(Model,list(pairwise~ app), adjust="tukey")




