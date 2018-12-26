library(Synth)
library(readstata13)

  data <- read.dta13("C:/Users/Teich/Desktop/Data diplomka/Total/Total_eurozone.dta")
  cols<-ncol(data)
  rows<-nrow(data)
  data <- cbind(data,as.numeric(as.factor(data[,2])))
  data <- cbind(data,as.numeric(as.factor(data[,1])))
  pred<-c(which(colnames(data) == "GDP"),which(colnames(data) == "Population"),which(colnames(data) == "FMA"),which(colnames(data) == "Country_risk"),which(colnames(data) == "Labor_index"),which(colnames(data) == "RER"),which(colnames(data) == "FDI_share"))
  dep<-which(colnames(data) == "Exports")
  time<-(cols+2)
  unit_name <- 2
  unit_num<-(cols+1)
  pretreat<-unique(data[,(cols+2)])[which(unique(data[,1]) == "2005Q1"):which(unique(data[,1]) == "2013Q3")]
  optimizer_per<- c(which(unique(data[,1]) == "2005Q1"):which(unique(data[,1]) == "2016Q4"))
  plot_per<-c(which(unique(data[,1]) == "2005Q1"):which(unique(data[,1]) == "2016Q4"))
  quarters <-unique(data[,1])[c(which(unique(data[,1]) == "2005Q1"):which(unique(data[,1]) == "2016Q4"))]
  pretreat_alt<-unique(data[,(cols+2)])[which(unique(data[,1]) == "2005Q1"):which(unique(data[,1]) == "2009Q4")]
  
  treat<- "CZE"
  controls<- c('AUT', 'BEL', 'BGR', 'HRV', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'NLD', 'POL', 'PRT', 'ROM', 'SVK', 'SVN', 'ESP', 'SWE', 'GBR')
 
  data_0<-data[data[,3]==0,]
  data_prepared_0<-dataprep(foo = data_0, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_0<-synth(data.prep.obj = data_prepared_0,verbose = FALSE)
  donor_w_0 <-synt_res_0$solution.w
  rownames(donor_w_0)<- controls
  tab_res_0<-cbind(data_prepared_0$Y1plot,data_prepared_0$Y0plot %*% synt_res_0$solution.w,data_prepared_0$Y1plot - (data_prepared_0$Y0plot %*% synt_res_0$solution.w))
  rownames(tab_res_0)<-quarters
  colnames(tab_res_0)<-c("Treated","Synthetic","Gap")
  
  df_results_0<-list() 
  gap_0<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_0)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_0, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_0[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_0[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_0[[i]]$solution.w)
    
  }
  gap_treat_0 <- data_prepared_0$Y1plot - (data_prepared_0$Y0plot %*% synt_res_0$solution.w)
  
  gap_0_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_0_red)<-controls
  for (i in controls){
   if(df_results_0[[i]]$loss.v<5*synt_res_0$loss.v){
     gap_0_red[,i]<-gap_0[,i]}
  }

  data_prepared_0_time_sens<-dataprep(foo = data_0, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat_alt,
                            time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_0_time_sens<-synth(data.prep.obj = data_prepared_0_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_0,synth.res = synt_res_0,Main="Category_0")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_0$solution.v
  donor_w_0
  tab_res_0
  
  plot(gap_treat_0,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_0)),2*max(abs(gap_treat_0))),main="Category_0")
  for( i in 1:ncol(gap_0)) {
    lines(gap_0[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_0,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_0,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_0)),2*max(abs(gap_treat_0))),main="Category_0")
  for( i in 1:ncol(gap_0_red)) {
    lines(gap_0_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_0,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_0_time_sens,synth.res = synt_res_0_time_sens,Main="Category_0")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  #Category 1
  data_1<-data[data[,3]==1,]
  data_prepared_1<-dataprep(foo = data_1, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_1<-synth(data.prep.obj = data_prepared_1,verbose = FALSE)
  donor_w_1 <-synt_res_1$solution.w
  rownames(donor_w_1)<- controls
  tab_res_1<-cbind(data_prepared_1$Y1plot,data_prepared_1$Y0plot %*% synt_res_1$solution.w,data_prepared_1$Y1plot - (data_prepared_1$Y0plot %*% synt_res_1$solution.w))
  rownames(tab_res_1)<-quarters
  colnames(tab_res_1)<-c("Treated","Synthetic","Gap")
  
  df_results_1<-list() 
  gap_1<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_1)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_1, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_1[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_1[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_1[[i]]$solution.w)
    
  }
  gap_treat_1 <- data_prepared_1$Y1plot - (data_prepared_1$Y0plot %*% synt_res_1$solution.w)
  
  gap_1_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_1_red)<-controls
  for (i in controls){
    if(df_results_1[[i]]$loss.v<5*synt_res_1$loss.v){
      gap_1_red[,i]<-gap_1[,i]}
  }
  
  data_prepared_1_time_sens<-dataprep(foo = data_1, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_1_time_sens<-synth(data.prep.obj = data_prepared_1_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_1,synth.res = synt_res_1,Main="Category_1")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_1$solution.v
  donor_w_1
  tab_res_1
  
  plot(gap_treat_1,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_1)),2*max(abs(gap_treat_1))),main="Category_1")
  for( i in 1:ncol(gap_1)) {
    lines(gap_1[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_1,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_1,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_1)),2*max(abs(gap_treat_1))),main="Category_1")
  for( i in 1:ncol(gap_1_red)) {
    lines(gap_1_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_1,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_1_time_sens,synth.res = synt_res_1_time_sens,Main="Category_1")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #Category 2
  data_2<-data[data[,3]==2,]
  data_prepared_2<-dataprep(foo = data_2, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_2<-synth(data.prep.obj = data_prepared_2,verbose = FALSE)
  donor_w_2 <-synt_res_2$solution.w
  rownames(donor_w_2)<- controls
  tab_res_2<-cbind(data_prepared_2$Y1plot,data_prepared_2$Y0plot %*% synt_res_2$solution.w,data_prepared_2$Y1plot - (data_prepared_2$Y0plot %*% synt_res_2$solution.w))
  rownames(tab_res_2)<-quarters
  colnames(tab_res_2)<-c("Treated","Synthetic","Gap")
  
  df_results_2<-list() 
  gap_2<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_2)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_2, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_2[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_2[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_2[[i]]$solution.w)
    
  }
  gap_treat_2 <- data_prepared_2$Y1plot - (data_prepared_2$Y0plot %*% synt_res_2$solution.w)
  
  gap_2_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_2_red)<-controls
  for (i in controls){
    if(df_results_2[[i]]$loss.v<5*synt_res_2$loss.v){
      gap_2_red[,i]<-gap_2[,i]}
  }
  
  data_prepared_2_time_sens<-dataprep(foo = data_2, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_2_time_sens<-synth(data.prep.obj = data_prepared_2_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_2,synth.res = synt_res_2,Main="Category_2")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_2$solution.v
  donor_w_2
  tab_res_2
  
  plot(gap_treat_2,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_2)),2*max(abs(gap_treat_2))),main="Category_2")
  for( i in 1:ncol(gap_2)) {
    lines(gap_2[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_2,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_2,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_2)),2*max(abs(gap_treat_2))),main="Category_2")
  for( i in 1:ncol(gap_2_red)) {
    lines(gap_2_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_2,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_2_time_sens,synth.res = synt_res_2_time_sens,Main="Category_2")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #Category 3
  
  data_3<-data[data[,3]==3,]
  data_prepared_3<-dataprep(foo = data_3, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_3<-synth(data.prep.obj = data_prepared_3,verbose = FALSE)
  donor_w_3 <-synt_res_3$solution.w
  rownames(donor_w_3)<- controls
  tab_res_3<-cbind(data_prepared_3$Y1plot,data_prepared_3$Y0plot %*% synt_res_3$solution.w,data_prepared_3$Y1plot - (data_prepared_3$Y0plot %*% synt_res_3$solution.w))
  rownames(tab_res_3)<-quarters
  colnames(tab_res_3)<-c("Treated","Synthetic","Gap")
  
  df_results_3<-list() 
  gap_3<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_3)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_3, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_3[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_3[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_3[[i]]$solution.w)
    
  }
  gap_treat_3 <- data_prepared_3$Y1plot - (data_prepared_3$Y0plot %*% synt_res_3$solution.w)
  
  gap_3_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_3_red)<-controls
  for (i in controls){
    if(df_results_3[[i]]$loss.v<5*synt_res_3$loss.v){
      gap_3_red[,i]<-gap_3[,i]}
  }
  
  data_prepared_3_time_sens<-dataprep(foo = data_3, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_3_time_sens<-synth(data.prep.obj = data_prepared_3_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_3,synth.res = synt_res_3,Main="Category_3")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_3$solution.v
  donor_w_3
  tab_res_3
  
  plot(gap_treat_3,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_3)),2*max(abs(gap_treat_3))),main="Category_3")
  for( i in 1:ncol(gap_3)) {
    lines(gap_3[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_3,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_3,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_3)),2*max(abs(gap_treat_3))),main="Category_3")
  for( i in 1:ncol(gap_3_red)) {
    lines(gap_3_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_3,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_3_time_sens,synth.res = synt_res_3_time_sens,Main="Category_3")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #Category 4
  data_4<-data[data[,3]==4,]
  data_prepared_4<-dataprep(foo = data_4, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_4<-synth(data.prep.obj = data_prepared_4,verbose = FALSE)
  donor_w_4 <-synt_res_4$solution.w
  rownames(donor_w_4)<- controls
  tab_res_4<-cbind(data_prepared_4$Y1plot,data_prepared_4$Y0plot %*% synt_res_4$solution.w,data_prepared_4$Y1plot - (data_prepared_4$Y0plot %*% synt_res_4$solution.w))
  rownames(tab_res_4)<-quarters
  colnames(tab_res_4)<-c("Treated","Synthetic","Gap")
  
  df_results_4<-list() 
  gap_4<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_4)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_4, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_4[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_4[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_4[[i]]$solution.w)
    
  }
  gap_treat_4 <- data_prepared_4$Y1plot - (data_prepared_4$Y0plot %*% synt_res_4$solution.w)
  
  gap_4_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_4_red)<-controls
  for (i in controls){
    if(df_results_4[[i]]$loss.v<5*synt_res_4$loss.v){
      gap_4_red[,i]<-gap_4[,i]}
  }
  
  data_prepared_4_time_sens<-dataprep(foo = data_4, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_4_time_sens<-synth(data.prep.obj = data_prepared_4_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_4,synth.res = synt_res_4,Main="Category_4")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_4$solution.v
  donor_w_4
  tab_res_4
  
  plot(gap_treat_4,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_4)),2*max(abs(gap_treat_4))),main="Category_4")
  for( i in 1:ncol(gap_4)) {
    lines(gap_4[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_4,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_4,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_4)),2*max(abs(gap_treat_4))),main="Category_4")
  for( i in 1:ncol(gap_4_red)) {
    lines(gap_4_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_4,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_4_time_sens,synth.res = synt_res_4_time_sens,Main="Category_4")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #category 5
  data_5<-data[data[,3]==5,]
  data_prepared_5<-dataprep(foo = data_5, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_5<-synth(data.prep.obj = data_prepared_5,verbose = FALSE)
  donor_w_5 <-synt_res_5$solution.w
  rownames(donor_w_5)<- controls
  tab_res_5<-cbind(data_prepared_5$Y1plot,data_prepared_5$Y0plot %*% synt_res_5$solution.w,data_prepared_5$Y1plot - (data_prepared_5$Y0plot %*% synt_res_5$solution.w))
  rownames(tab_res_5)<-quarters
  colnames(tab_res_5)<-c("Treated","Synthetic","Gap")
  
  df_results_5<-list() 
  gap_5<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_5)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_5, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_5[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_5[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_5[[i]]$solution.w)
    
  }
  gap_treat_5 <- data_prepared_5$Y1plot - (data_prepared_5$Y0plot %*% synt_res_5$solution.w)
  
  gap_5_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_5_red)<-controls
  for (i in controls){
    if(df_results_5[[i]]$loss.v<5*synt_res_5$loss.v){
      gap_5_red[,i]<-gap_5[,i]}
  }
  
  data_prepared_5_time_sens<-dataprep(foo = data_5, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_5_time_sens<-synth(data.prep.obj = data_prepared_5_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_5,synth.res = synt_res_5,Main="Category_5")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_5$solution.v
  donor_w_5
  tab_res_5
  
  plot(gap_treat_5,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_5)),2*max(abs(gap_treat_5))),main="Category_5")
  for( i in 1:ncol(gap_5)) {
    lines(gap_5[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_5,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_5,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_5)),2*max(abs(gap_treat_5))),main="Category_5")
  for( i in 1:ncol(gap_5_red)) {
    lines(gap_5_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_5,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_5_time_sens,synth.res = synt_res_5_time_sens,Main="Category_5")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #Category 6
  data_6<-data[data[,3]==6,]
  data_prepared_6<-dataprep(foo = data_6, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_6<-synth(data.prep.obj = data_prepared_6,verbose = FALSE)
  donor_w_6 <-synt_res_6$solution.w
  rownames(donor_w_6)<- controls
  tab_res_6<-cbind(data_prepared_6$Y1plot,data_prepared_6$Y0plot %*% synt_res_6$solution.w,data_prepared_6$Y1plot - (data_prepared_6$Y0plot %*% synt_res_6$solution.w))
  rownames(tab_res_6)<-quarters
  colnames(tab_res_6)<-c("Treated","Synthetic","Gap")
  
  df_results_6<-list() 
  gap_6<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_6)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_6, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_6[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_6[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_6[[i]]$solution.w)
    
  }
  gap_treat_6 <- data_prepared_6$Y1plot - (data_prepared_6$Y0plot %*% synt_res_6$solution.w)
  
  gap_6_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_6_red)<-controls
  for (i in controls){
    if(df_results_6[[i]]$loss.v<5*synt_res_6$loss.v){
      gap_6_red[,i]<-gap_6[,i]}
  }
  
  data_prepared_6_time_sens<-dataprep(foo = data_6, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_6_time_sens<-synth(data.prep.obj = data_prepared_6_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_6,synth.res = synt_res_6,Main="Category_6")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_6$solution.v
  donor_w_6
  tab_res_6
  
  plot(gap_treat_6,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_6)),2*max(abs(gap_treat_6))),main="Category_6")
  for( i in 1:ncol(gap_6)) {
    lines(gap_6[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_6,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_6,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_6)),2*max(abs(gap_treat_6))),main="Category_6")
  for( i in 1:ncol(gap_6_red)) {
    lines(gap_6_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_6,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_6_time_sens,synth.res = synt_res_6_time_sens,Main="Category_6")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  #Category 7
  data_7<-data[data[,3]==7,]
  data_prepared_7<-dataprep(foo = data_7, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_7<-synth(data.prep.obj = data_prepared_7,verbose = FALSE)
  donor_w_7 <-synt_res_7$solution.w
  rownames(donor_w_7)<- controls
  tab_res_7<-cbind(data_prepared_7$Y1plot,data_prepared_7$Y0plot %*% synt_res_7$solution.w,data_prepared_7$Y1plot - (data_prepared_7$Y0plot %*% synt_res_7$solution.w))
  rownames(tab_res_7)<-quarters
  colnames(tab_res_7)<-c("Treated","Synthetic","Gap")
  
  df_results_7<-list() 
  gap_7<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_7)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_7, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_7[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_7[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_7[[i]]$solution.w)
    
  }
  gap_treat_7 <- data_prepared_7$Y1plot - (data_prepared_7$Y0plot %*% synt_res_7$solution.w)
  
  gap_7_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_7_red)<-controls
  for (i in controls){
    if(df_results_7[[i]]$loss.v<5*synt_res_7$loss.v){
      gap_7_red[,i]<-gap_7[,i]}
  }
  
  data_prepared_7_time_sens<-dataprep(foo = data_7, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_7_time_sens<-synth(data.prep.obj = data_prepared_7_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_7,synth.res = synt_res_7,Main="Category_7")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_7$solution.v
  donor_w_7
  tab_res_7
  
  plot(gap_treat_7,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_7)),2*max(abs(gap_treat_7))),main="Category_7")
  for( i in 1:ncol(gap_7)) {
    lines(gap_7[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_7,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_7,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_7)),2*max(abs(gap_treat_7))),main="Category_7")
  for( i in 1:ncol(gap_7_red)) {
    lines(gap_7_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_7,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_7_time_sens,synth.res = synt_res_7_time_sens,Main="Category_7")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #Category 8
  data_8<-data[data[,3]==8,]
  data_prepared_8<-dataprep(foo = data_8, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_8<-synth(data.prep.obj = data_prepared_8,verbose = FALSE)
  donor_w_8 <-synt_res_8$solution.w
  rownames(donor_w_8)<- controls
  tab_res_8<-cbind(data_prepared_8$Y1plot,data_prepared_8$Y0plot %*% synt_res_8$solution.w,data_prepared_8$Y1plot - (data_prepared_8$Y0plot %*% synt_res_8$solution.w))
  rownames(tab_res_8)<-quarters
  colnames(tab_res_8)<-c("Treated","Synthetic","Gap")
  
  df_results_8<-list() 
  gap_8<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_8)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_8, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_8[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_8[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_8[[i]]$solution.w)
    
  }
  gap_treat_8 <- data_prepared_8$Y1plot - (data_prepared_8$Y0plot %*% synt_res_8$solution.w)
  
  gap_8_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_8_red)<-controls
  for (i in controls){
    if(df_results_8[[i]]$loss.v<5*synt_res_8$loss.v){
      gap_8_red[,i]<-gap_8[,i]}
  }
  
  data_prepared_8_time_sens<-dataprep(foo = data_8, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_8_time_sens<-synth(data.prep.obj = data_prepared_8_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_8,synth.res = synt_res_8,Main="Category_8")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_8$solution.v
  donor_w_8
  tab_res_8
  
  plot(gap_treat_8,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_8)),2*max(abs(gap_treat_8))),main="Category_8")
  for( i in 1:ncol(gap_8)) {
    lines(gap_8[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_8,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_8,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_8)),2*max(abs(gap_treat_8))),main="Category_8")
  for( i in 1:ncol(gap_8_red)) {
    lines(gap_8_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_8,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_8_time_sens,synth.res = synt_res_8_time_sens,Main="Category_8")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  #Category 9
  data_9<-data[data[,3]==9,]
  data_prepared_9<-dataprep(foo = data_9, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_9<-synth(data.prep.obj = data_prepared_9,verbose = FALSE)
  donor_w_9 <-synt_res_9$solution.w
  rownames(donor_w_9)<- controls
  tab_res_9<-cbind(data_prepared_9$Y1plot,data_prepared_9$Y0plot %*% synt_res_9$solution.w,data_prepared_9$Y1plot - (data_prepared_9$Y0plot %*% synt_res_9$solution.w))
  rownames(tab_res_9)<-quarters
  colnames(tab_res_9)<-c("Treated","Synthetic","Gap")
  
  df_results_9<-list() 
  gap_9<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_9)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data_9, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_9[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_9[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_9[[i]]$solution.w)
    
  }
  gap_treat_9 <- data_prepared_9$Y1plot - (data_prepared_9$Y0plot %*% synt_res_9$solution.w)
  
  gap_9_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_9_red)<-controls
  for (i in controls){
    if(df_results_9[[i]]$loss.v<5*synt_res_9$loss.v){
      gap_9_red[,i]<-gap_9[,i]}
  }
  
  data_prepared_9_time_sens<-dataprep(foo = data_9, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_9_time_sens<-synth(data.prep.obj = data_prepared_9_time_sens,verbose = FALSE)
  
  #results
  
  path.plot(dataprep.res = data_prepared_9,synth.res = synt_res_9,Main="Category_9")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  synt_res_9$solution.v
  donor_w_9
  tab_res_9
  
  plot(gap_treat_9,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_9)),2*max(abs(gap_treat_9))),main="Category_9")
  for( i in 1:ncol(gap_9)) {
    lines(gap_9[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_9,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  plot(gap_treat_9,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_9)),2*max(abs(gap_treat_9))),main="Category_9")
  for( i in 1:ncol(gap_9_red)) {
    lines(gap_9_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_9,type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  
  path.plot(dataprep.res = data_prepared_9_time_sens,synth.res = synt_res_9_time_sens,Main="Category_9")
  abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
  abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
  
  
  
  
 # for (k in 0:9){
 #   assign(paste("data_",k,sep=""),data[data[,3]==k,])
 #   assign(paste("data_prepared_",k,sep=""),dataprep(foo = eval(parse(text=(paste("data_",k,sep="")))), predictors = pred,
 #                                                    dependent = dep, unit.variable = unit_num,
 #                                                    time.variable = time, treatment.identifier = treat,
 #                                                    controls.identifier = controls, time.predictors.prior = pretreat,
 #                                                    time.optimize.ssr = pretreat, time.plot = plot_per,
 #                                                    unit.names.variable = unit_name))
  
 #   assign(paste("synt_res_",k,sep=""),synth(data.prep.obj = eval(parse(text=(paste("data_prepared_",k,sep="")))),verbose = FALSE))
 #   path.plot(dataprep.res = eval(parse(text=(paste("data_prepared_",k,sep="")))),synth.res = eval(parse(text=(paste("synt_res_",k,sep="")))),Main=paste(treat,k,sep="_"))
 #   abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
 # }
  
  load("C:/Users/Teich/Desktop/Data diplomka/Total/Synth_base_eurozone_results.RData")
  ticks<-seq(from = 4,by=8,length.out=6)
  axis<- c("2005Q4","2007Q4","2009Q4","2011Q4","2013Q4","2015Q4")
  model_version<-"base_eu"
  setwd("C:/Users/Teich/Desktop/Data diplomka/Total/grafy")
  
  for (i in c(0:6,8,9)){
    pdf(paste("SCM_plot_",i,"_",model_version,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,1,1))
    matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),xaxt='n',xlab = "Quarter",ylab="Exports (mil. EUR)")
    axis(1, at= (ticks-1+which(unique(data[,1]) == "2005Q1")),labels = axis)
    abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
    dev.off()
  } 
  
  
  for (j in c(0:6,8,9)){
    pdf(paste("gap_plot_",j,"_",model_version,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,1,1))
    plot(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))) ,type="l",col="black",lty=1,lwd=2,ylab="Gap in exports (mil. EUR)",xlab = "Quarter",xaxt='n',,ylim=c(-2*max(abs(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))) )),2*max(abs(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))) ))))
    axis(1, at= ticks,labels = axis)
    for( i in 1:ncol(cbind(eval(parse(text=(paste("gap_",j,sep=""))))) )) {
      lines(cbind(eval(parse(text=(paste("gap_",j,sep=""))))) [,i],type="l",col="gray",lty=1,lwd=1)
    }
    lines(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))),type="l",col="black",lty=1,lwd=2)
    abline(v=length(pretreat), col="black")
    dev.off()
  }  
  
  
  for (j in c(0:6,8,9)){
    pdf(paste("gap_plot_red_",j,"_",model_version,".png",sep=""),width=7, height=4)  
    par(mar=c(4,4,1,1))
    plot(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))) ,type="l",col="black",lty=1,lwd=2,ylab="Gap in exports (mil. EUR)",xlab = "Quarter",xaxt='n',,ylim=c(-2*max(abs(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))) )),2*max(abs(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))) ))))
    axis(1, at= ticks,labels = axis)
    for( i in 1:ncol(cbind(eval(parse(text=(paste("gap_",j,"_red",sep=""))))) )) {
      lines(cbind(eval(parse(text=(paste("gap_",j,"_red",sep=""))))) [,i],type="l",col="gray",lty=1,lwd=1)
    }
    lines(cbind(eval(parse(text=(paste("gap_treat_",j,sep=""))))),type="l",col="black",lty=1,lwd=2)
    abline(v=length(pretreat), col="black")
    dev.off()
  }
  
  for (i in c(0:6,8,9)){
    pdf(paste("SCM_time_sens_",i,"_",model_version,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,1,1))
    matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,"_time_sens",sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),xaxt='n',xlab = "Quarter",ylab="Exports (mil. EUR)")
    axis(1, at= (ticks-1+which(unique(data[,1]) == "2005Q1")),labels = axis)
    abline(v=which(unique(data[,1]) == "2013Q3"), col="black")
    abline(v=which(unique(data[,1]) == "2009Q4"), col="black",lty=2)
    dev.off()
  }   
  

