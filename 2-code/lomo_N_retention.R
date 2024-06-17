library(ggplot2)
library(dplyr)
library(caret)
library(lme4)
library(MuMIn)

### I went through by hand and removed the duplicated studies from this dataset
d<-read.csv("1-data/2-k-estimation/k_TN_meta_analysis_data.csv")

d$l_day<-d$cheng_flow_m3_year/1000/365 # converting flow to liters per day
d$N_conc_mg_l<-(d$cheng_N_load_kg_d/(10**6))/d$l_day ##LCN: I think this math is wrong

kd_vs_hrt<-ggplot(d,aes(x=hrt_d,y=k_d))+geom_point()+scale_x_log10()+scale_y_log10()+
  geom_smooth(method='lm',se=FALSE)+theme_classic()+theme(text=element_text(size=20))+
  xlab(expression('Hydraulic Residence Time (d'^-1*')'))+
  ylab(expression('Rate Constant, k (d'^-1*')'))

tiff(filename="./figures/plot_of_raw_data.tif",units='in',compression='lzw',width=8,height=8,res=900)
kd_vs_hrt
dev.off()

### There are negative K values that are lost during a log transform. Both meta-analysis 
### removed them from the dataset. They represent less than 2% of the obs here so I will remove them too


### Negative concentrations and flows don't make sense we I am removing them here. 
set.seed(207)

#
cheng_modeling_data<- d %>% select(k_d,hrt_d,meta_analysis,cheng_N_load_kg_d,N_conc_mg_l) %>% 
  filter(k_d>0 & hrt_d>0 & meta_analysis=='cheng') 
#

######### Selecting the best model to describe K. comparing models where K is only a function of HRT
######### to models where K is a function of HRT and N load or concentration. There may be some 
######### effeciency loss where higher concentartions load reduce effeciency of retention

### getting complete cases for the simple model with just hrt and k
cheng_modeling_data_1<- d %>% select(k_d,hrt_d,meta_analysis) %>% 
  filter(k_d>0 & hrt_d>0 & meta_analysis=='cheng')
cheng_modeling_data_1<-cheng_modeling_data_1[complete.cases(cheng_modeling_data_1),]

train_control <- trainControl(method = "cv", 
                              number = 10)

### using simple linear models for model selection - there is likely some variation explained at the source study level
### due to different methods used. But both Shen and Cheng did not include this in their modeling. 
### will refit the best model with a random effect for reference to try to get the best parmater estimates

cheng_model_1 <- caret::train(log(k_d)~log(hrt_d), data = cheng_modeling_data_1, 
                      trControl = train_control, 
                      method = "lm")
### adding in N loading to the dataset
### idea is that there may be effeciency loss
cheng_modeling_data_2<- d %>% select(k_d,hrt_d,meta_analysis,cheng_N_load_kg_d) %>% 
  filter(k_d>0 & hrt_d>0 & meta_analysis=='cheng' & cheng_N_load_kg_d>0)
cheng_modeling_data_2<-cheng_modeling_data_2[complete.cases(cheng_modeling_data_2),]

cheng_model_2 <- caret::train(log(k_d)~log(hrt_d)+log(cheng_N_load_kg_d), data = cheng_modeling_data_2, 
                      trControl = train_control, 
                      method = "lm")
cheng_model_2

### adding in n concentration
cheng_modeling_data_3<- d %>% select(k_d,hrt_d,meta_analysis,N_conc_mg_l) %>% 
  filter(k_d>0 & hrt_d>0 & meta_analysis=='cheng' & N_conc_mg_l>0)
cheng_modeling_data_3<-cheng_modeling_data_3[complete.cases(cheng_modeling_data_3),]

cheng_model_3 <- caret::train(log(k_d)~log(hrt_d)+log(N_conc_mg_l), data = cheng_modeling_data_3, 
                              trControl = train_control, 
                              method = "lm")
cheng_model_3

### full modeling data
full_modeling_data<- d %>% select(k_d,hrt_d,meta_analysis,references) %>% 
  filter(k_d>0 & hrt_d>0)
full_modeling_data<-full_modeling_data[complete.cases(full_modeling_data),]

full_model <- caret::train(log(k_d)~log(hrt_d), data = full_modeling_data, 
                              trControl = train_control, 
                              method = "lm")
full_model

######### model selection table

t<-bind_rows(cheng_model_1$results,cheng_model_2$results,cheng_model_3$results,full_model$results)
t$n=c(358,325,319,1007)
t$model=c('cheng_hrt','cheng_hrt_load','cheng_hrt_conc',"all_hrt")

write.csv(t,"./data/summary_model_selection.csv")

### Model with all of the data and only HRT as a predictor is the best
### refitting with a random effect for study here

mixed_model<-lmer(log(k_d)~log(hrt_d)+(1|references),data=full_modeling_data)
r.squaredGLMM(mixed_model)

summary(mixed_model)

fixed_effects_model <- lm(log(k_d)~log(hrt_d),data=full_modeling_data)
summary(fixed_effects_model)

new_data <- data.frame(hrt_d = 10)
predict(fixed_effects_model, new_data, interval = "prediction")

# how to add the prediction interval in 

estimate_pred <- function(hrt){
  
  new_data <- data.frame(hrt_d = hrt)
  predict(fixed_effects_model, new_data, interval = "prediction")
  # how are we sampling the prediction interval? Uniform?
}

estimate_R <- function(hrt){
  ## ordinarily I would reference the model object but pasting the numbers here should make this easier
  ## to paste over to a different script
  intercept=rnorm(1,-0.72162,0.05635)
  slope=rnorm(1,-0.82655,0.02718)
  log_k<-intercept+slope*log(hrt)
  k=exp(log_k)
  ## please check this math! this is my rearrangement of equation 2 in Shen et al to calculate R from K
  R=100-100*exp(-k*hrt)
  return(R)
  
}

test_function<-data.frame(hrt=rep(seq(1,50),times=20))
test_function$R<-NA

for (i in 1:nrow(test_function)){
  test_function$R[i]<-estimate_R(test_function$hrt[i])
  
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

tiff(filename="./figures/testing_function.tif",units='in',compression='lzw',width=8,height=8,res=900)
test_function
dev.off()
