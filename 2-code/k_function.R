# need to add in covariance which is why this probably different 
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

