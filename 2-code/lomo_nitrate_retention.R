
library(ggplot2)
library(dplyr)
library(caret)
library(lme4)
library(MuMIn)

d<-read.csv("1-data/2-k-estimation/Cheng_WRR_2017_data.csv")

d$l_day<-d$Flow/1000/365 # converting flow to liters per day

d$N_conc_mg_l<-(d$L_in_NO3/(10**6))/d$l_day

### Negative concentrations and flows don't make sense we I am removing them here. 
set.seed(207)

#
cheng_modeling_data<- d %>% select(k_NO3_PFR,Hydraulic.Residence.Time..tau.,L_in_NO3,N_conc_mg_l,Reference.Paper) %>% 
  filter(k_NO3_PFR>0 & Hydraulic.Residence.Time..tau.>0)
#
cheng_modeling_data$k_NO3_PFR<-as.numeric(cheng_modeling_data$k_NO3_PFR)

######### Selecting the best model to describe K. comparing models where K is only a function of HRT
######### to models where K is a function of HRT and N load or concentration. There may be some 
######### effeciency loss where higher concentartions load reduce effeciency of retention

### getting complete cases for the simple model with just hrt and k
cheng_modeling_data_1<- cheng_modeling_data %>% select(k_NO3_PFR,Hydraulic.Residence.Time..tau.,Reference.Paper) %>% 
  filter(k_NO3_PFR>0 & Hydraulic.Residence.Time..tau.>0)

cheng_modeling_data_1<-cheng_modeling_data_1[complete.cases(cheng_modeling_data_1),]

train_control <- trainControl(method = "cv", 
                              number = 10)

### using simple linear models for model selection - there is likely some variation explained at the source study level
### due to different methods used. But both Shen and Cheng did not include this in their modeling. 
### will refit the best model with a random effect for reference to try to get the best parmater estimates

cheng_model_1 <- caret::train(log(k_NO3_PFR)~log(Hydraulic.Residence.Time..tau.), data = cheng_modeling_data_1, 
                              trControl = train_control,
                              method = "lm")
cheng_model_1
### adding in N loading to the dataset
### idea is that there may be effeciency loss
cheng_modeling_data_2<- cheng_modeling_data %>% select(k_NO3_PFR,Hydraulic.Residence.Time..tau.,L_in_NO3) %>% 
  filter(k_NO3_PFR>0 & Hydraulic.Residence.Time..tau.>0 & L_in_NO3>0)
cheng_modeling_data_2<-cheng_modeling_data_2[complete.cases(cheng_modeling_data_2),]

cheng_model_2 <- caret::train(log(k_NO3_PFR)~log(Hydraulic.Residence.Time..tau.)+log(L_in_NO3), data = cheng_modeling_data_2, 
                              method = "lm")
cheng_model_2

### adding in n concentration
cheng_modeling_data_3<- cheng_modeling_data %>% select(k_NO3_PFR,Hydraulic.Residence.Time..tau.,N_conc_mg_l) %>% 
  filter(k_NO3_PFR>0 & Hydraulic.Residence.Time..tau.>0 & N_conc_mg_l>0)
cheng_modeling_data_3<-cheng_modeling_data_3[complete.cases(cheng_modeling_data_3),]
cheng_modeling_data_3<-cheng_modeling_data_3[cheng_modeling_data_3$N_conc_mg_l<10**10,]

cheng_model_3 <- caret::train(log(k_NO3_PFR)~log(Hydraulic.Residence.Time..tau.)+log(N_conc_mg_l), data = cheng_modeling_data_3, 
                              method = "lm")
cheng_model_3

###############################

t<-bind_rows(cheng_model_1$results,cheng_model_2$results,cheng_model_3$results)
t$n=c(339,303,293)
t$model=c('cheng_hrt','cheng_hrt_load','cheng_hrt_conc')

write.csv(t,"1-data/2-k-estimation/summary_model_selection_nitrate.csv")




mixed_model<-lmer(log(k_NO3_PFR)~log(Hydraulic.Residence.Time..tau.)+(1|Reference.Paper),data=cheng_modeling_data_1)
r.squaredGLMM(mixed_model)

cheng_model_data_clean <- cheng_modeling_data_1 %>% rename("k_no3" = "k_NO3_PFR", "hrt" = "Hydraulic.Residence.Time..tau.") %>% dplyr::select(-Reference.Paper)
head(cheng_model_data_clean)







estimate_R_no3 <- function(hrt){
  ## ordinarily I would reference the model object but pasting the numbers here should make this easier
  ## to paste over to a different script
  intercept=rnorm(1,-0.71399,0.14063)
  slope=rnorm(1,-0.68205,0.05412)
  log_k<-intercept+slope*log(hrt)
  k=exp(log_k)
  ## please check this math! this is my rearrangement of equation 2 in Shen et al to calculate R from K
  R=100-100*exp(-k*hrt)
  return(R)
  
}

test_function<-data.frame(hrt=rep(seq(1,50),times=20))
test_function$R<-NA

for (i in 1:nrow(test_function)){
  test_function$R[i]<-estimate_R_no3(test_function$hrt[i])
  
}

# visually the results make sense, as hrt increases so does R. Also seems like our precision declines as HRT increases
# ultimately I think using this relationship will help constrain our final estiamtes a fair bit.
# looking at Fig. 3 in Shen the middle 50% of the distribution for N retained by wetlands is between
# 30% and 50%. Looking at the figure below, if we know HRT then we are sampling from much smaller ranges - which
# will be helpful


test_function<-ggplot(test_function,aes(x=hrt,y=R))+geom_point()+
  theme_classic()+theme(text=element_text(size=20))+
  xlab(expression('Hydraulic Residence Time (d'^-1*')'))+
  ylab(expression('Retention Effeciency (%)'))
test_function


