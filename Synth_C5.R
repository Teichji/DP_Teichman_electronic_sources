library(Synth)
library(readstata13)


data <- read.dta13("C:/Users/T_doma/Desktop/Data diplomka/Cat_5/Category_5.dta")
cols<-ncol(data)
  rows<-nrow(data)
  data[,cols] <-as.numeric(data[,cols])
  summary(data)
  cols<-ncol(data)
  data <- cbind(data,as.numeric(as.factor(data[,1])))
  pred<-c(which(colnames(data) == "Dist"),which(colnames(data) == "GDP_o"),which(colnames(data) == "GDP_d"),which(colnames(data) == "GPD_PC_o"),which(colnames(data) == "GPD_PC_d"),which(colnames(data) == "Population_o"),which(colnames(data) == "Population_d"),which(colnames(data) == "RER"),which(colnames(data) == "Rem_o"),which(colnames(data) == "Rem_d"),which(colnames(data) == "contig"),which(colnames(data) == "comcur"),which(colnames(data) == "Trade_value_l"))
  dep<-cols
  time<-4
  unit_name <- 1
  unit_num<-(cols+1)
  pretreat<-c(2000:2012)
  optimizer_per<- c(2000:2016)
  plot_per<-c(2000:2016)
  pretreat_alt<-c(2000:2009)
  
  
  treat<- "CZE_DEU"
  control_origins<-c("POL","AUT","FRA","BEL","NLD","LUX","DNK","HUN","SVK","SVN","ROM","HRV","LTU")
  control_destinations<-c("DEU","FRA","ESP","POL","ITA","NLD","GBR","AUT","BEL","SVN","HUN","SVK")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_1<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_1<-synth(data.prep.obj = data_prepared_1,verbose = FALSE)
  donor_w_1 <-synt_res_1$solution.w
  rownames(donor_w_1)<- controls
  tab_res_1<-cbind(data_prepared_1$Y1plot,data_prepared_1$Y0plot %*% synt_res_1$solution.w,data_prepared_1$Y1plot - (data_prepared_1$Y0plot %*% synt_res_1$solution.w))
  rownames(tab_res_1)<-optimizer_per
  colnames(tab_res_1)<-c("Treated","Synthetic","Gap")
  
  df_results_1<-list() 
  gap_1<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_1)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_1_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_1_time_sens<-synth(data.prep.obj = data_prepared_1_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_1,synth.res = synt_res_1,Main=treat)
  abline(v="2012", col="black")
  synt_res_1$solution.v
  donor_w_1
  tab_res_1
  
  plot(gap_treat_1,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_1)),2*max(abs(gap_treat_1))),main=treat)
  for( i in 1:ncol(gap_1)) {
    lines(gap_1[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_1,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_1,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_1)),2*max(abs(gap_treat_1))),main=treat)
  for( i in 1:ncol(gap_1_red)) {
    lines(gap_1_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_1,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_1_time_sens,synth.res = synt_res_1_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  treat<- "CZE_SVK"
  control_origins<-c("AUT","POL","HUN","SVK","SVN","ROM","HRV","ESP","PTE","LAT","LTU","EST")
  control_destinations<-c("SVK","SVN","HUN","AUT","ROM","HRV","ESP","PTE","LAT","LTU","EST","DEU")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
    
  data_prepared_2<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_2<-synth(data.prep.obj = data_prepared_2,verbose = FALSE)
  donor_w_2 <-synt_res_2$solution.w
  rownames(donor_w_2)<- controls
  tab_res_2<-cbind(data_prepared_2$Y1plot,data_prepared_2$Y0plot %*% synt_res_2$solution.w,data_prepared_2$Y1plot - (data_prepared_2$Y0plot %*% synt_res_2$solution.w))
  rownames(tab_res_2)<-optimizer_per
  colnames(tab_res_2)<-c("Treated","Synthetic","Gap")
  
  df_results_2<-list() 
  gap_2<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_2)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_2_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_2_time_sens<-synth(data.prep.obj = data_prepared_2_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_2,synth.res = synt_res_2,Main=treat)
  abline(v="2012", col="black")
  synt_res_2$solution.v
  donor_w_2
  tab_res_2
  
  plot(gap_treat_2,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_2)),2*max(abs(gap_treat_2))),main=treat)
  for( i in 1:ncol(gap_2)) {
    lines(gap_2[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_2,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_2,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_2)),2*max(abs(gap_treat_2))),main=treat)
  for( i in 1:ncol(gap_2_red)) {
    lines(gap_2_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_2,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_2_time_sens,synth.res = synt_res_2_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  treat<- "CZE_POL"
  control_origins<-c("DEU","SVK","LTU","POL","HUN","SVN","ROM","HRV")
  control_destinations<-c("POL","AUT","FRA","BEL","NLD","LUX","DNK","HUN","LAT","ROM","SVK","LTU","DEU")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_3<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_3<-synth(data.prep.obj = data_prepared_3,verbose = FALSE)
  donor_w_3 <-synt_res_3$solution.w
  rownames(donor_w_3)<- controls
  tab_res_3<-cbind(data_prepared_3$Y1plot,data_prepared_3$Y0plot %*% synt_res_3$solution.w,data_prepared_3$Y1plot - (data_prepared_3$Y0plot %*% synt_res_3$solution.w))
  rownames(tab_res_3)<-optimizer_per
  colnames(tab_res_3)<-c("Treated","Synthetic","Gap")
  
  df_results_3<-list() 
  gap_3<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_3)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_3_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_3_time_sens<-synth(data.prep.obj = data_prepared_3_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_3,synth.res = synt_res_3,Main=treat)
  abline(v="2012", col="black")
  synt_res_3$solution.v
  donor_w_3
  tab_res_3
  
  plot(gap_treat_3,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_3)),2*max(abs(gap_treat_3))),main=treat)
  for( i in 1:ncol(gap_3)) {
    lines(gap_3[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_3,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_3,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_3)),2*max(abs(gap_treat_3))),main=treat)
  for( i in 1:ncol(gap_3_red)) {
    lines(gap_3_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_3,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_3_time_sens,synth.res = synt_res_3_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  treat<- "CZE_HUN"
  control_origins<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  control_destinations<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_4<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_4<-synth(data.prep.obj = data_prepared_4,verbose = FALSE)
  donor_w_4 <-synt_res_4$solution.w
  rownames(donor_w_4)<- controls
  tab_res_4<-cbind(data_prepared_4$Y1plot,data_prepared_4$Y0plot %*% synt_res_4$solution.w,data_prepared_4$Y1plot - (data_prepared_4$Y0plot %*% synt_res_4$solution.w))
  rownames(tab_res_4)<-optimizer_per
  colnames(tab_res_4)<-c("Treated","Synthetic","Gap")
  
  df_results_4<-list() 
  gap_4<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_4)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_4_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_4_time_sens<-synth(data.prep.obj = data_prepared_4_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_4,synth.res = synt_res_4,Main=treat)
  abline(v="2012", col="black")
  synt_res_4$solution.v
  donor_w_4
  tab_res_4
  
  plot(gap_treat_4,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_4)),2*max(abs(gap_treat_4))),main=treat)
  for( i in 1:ncol(gap_4)) {
    lines(gap_4[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_4,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_4,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_4)),2*max(abs(gap_treat_4))),main=treat)
  for( i in 1:ncol(gap_4_red)) {
    lines(gap_4_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_4,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_4_time_sens,synth.res = synt_res_4_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
 
  treat<- "CZE_ITA"
  control_origins<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  control_destinations<-c("ITA","ESP","FRA","DEU","GBR")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  data_prepared_5<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_5<-synth(data.prep.obj = data_prepared_5,verbose = FALSE)
  donor_w_5 <-synt_res_5$solution.w
  rownames(donor_w_5)<- controls
  tab_res_5<-cbind(data_prepared_5$Y1plot,data_prepared_5$Y0plot %*% synt_res_5$solution.w,data_prepared_5$Y1plot - (data_prepared_5$Y0plot %*% synt_res_5$solution.w))
  rownames(tab_res_5)<-optimizer_per
  colnames(tab_res_5)<-c("Treated","Synthetic","Gap")
  
  df_results_5<-list() 
  gap_5<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_5)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_5_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_5_time_sens<-synth(data.prep.obj = data_prepared_5_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_5,synth.res = synt_res_5,Main=treat)
  abline(v="2012", col="black")
  synt_res_5$solution.v
  donor_w_5
  tab_res_5
  
  plot(gap_treat_5,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_5)),2*max(abs(gap_treat_5))),main=treat)
  for( i in 1:ncol(gap_5)) {
    lines(gap_5[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_5,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_5,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_5)),2*max(abs(gap_treat_5))),main=treat)
  for( i in 1:ncol(gap_5_red)) {
    lines(gap_5_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_5,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_5_time_sens,synth.res = synt_res_5_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  
  treat<- "CZE_AUT"
  control_origins<-c("DEU","SVK","HUN","SVN","ITA","POL","ROM","HRV","LTU")
  control_destinations<-c("AUT","FRA","BEL","NLD","LUX","DNK","HUN","POL","HRV","SVN","SVK","ROM","ITA","FRA","SWE","FIN","DEU")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_6<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_6<-synth(data.prep.obj = data_prepared_6,verbose = FALSE)
  donor_w_6 <-synt_res_6$solution.w
  rownames(donor_w_6)<- controls
  tab_res_6<-cbind(data_prepared_6$Y1plot,data_prepared_6$Y0plot %*% synt_res_6$solution.w,data_prepared_6$Y1plot - (data_prepared_6$Y0plot %*% synt_res_6$solution.w))
  rownames(tab_res_6)<-optimizer_per
  colnames(tab_res_6)<-c("Treated","Synthetic","Gap")
  
  df_results_6<-list() 
  gap_6<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_6)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_6_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_6_time_sens<-synth(data.prep.obj = data_prepared_6_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_6,synth.res = synt_res_6,Main=treat)
  abline(v="2012", col="black")
  synt_res_6$solution.v
  donor_w_6
  tab_res_6
  
  plot(gap_treat_6,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_6)),2*max(abs(gap_treat_6))),main=treat)
  for( i in 1:ncol(gap_6)) {
    lines(gap_6[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_6,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_6,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_6)),2*max(abs(gap_treat_6))),main=treat)
  for( i in 1:ncol(gap_6_red)) {
    lines(gap_6_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_6,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_6_time_sens,synth.res = synt_res_6_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  
  treat<- "CZE_FRA"
  control_origins<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  control_destinations<-c("FRA","GBR","DEU","ESP","ITA","BEL","NLD")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  data_prepared_7<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_7<-synth(data.prep.obj = data_prepared_7,verbose = FALSE)
  donor_w_7 <-synt_res_7$solution.w
  rownames(donor_w_7)<- controls
  tab_res_7<-cbind(data_prepared_7$Y1plot,data_prepared_7$Y0plot %*% synt_res_7$solution.w,data_prepared_7$Y1plot - (data_prepared_7$Y0plot %*% synt_res_7$solution.w))
  rownames(tab_res_7)<-optimizer_per
  colnames(tab_res_7)<-c("Treated","Synthetic","Gap")
  
  df_results_7<-list() 
  gap_7<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_7)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_7_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_7_time_sens<-synth(data.prep.obj = data_prepared_7_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_7,synth.res = synt_res_7,Main=treat)
  abline(v="2012", col="black")
  synt_res_7$solution.v
  donor_w_7
  tab_res_7
  
  plot(gap_treat_7,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_7)),2*max(abs(gap_treat_7))),main=treat)
  for( i in 1:ncol(gap_7)) {
    lines(gap_7[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_7,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_7,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_7)),2*max(abs(gap_treat_7))),main=treat)
  for( i in 1:ncol(gap_7_red)) {
    lines(gap_7_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_7,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_7_time_sens,synth.res = synt_res_7_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  treat<- "CZE_BEL"
  control_origins<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  control_destinations<-c("NLD","BEL","LUX","SWE","DNK","FRA","DEU","AUT")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_8<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_8<-synth(data.prep.obj = data_prepared_8,verbose = FALSE)
  donor_w_8 <-synt_res_8$solution.w
  rownames(donor_w_8)<- controls
  tab_res_8<-cbind(data_prepared_8$Y1plot,data_prepared_8$Y0plot %*% synt_res_8$solution.w,data_prepared_8$Y1plot - (data_prepared_8$Y0plot %*% synt_res_8$solution.w))
  rownames(tab_res_8)<-optimizer_per
  colnames(tab_res_8)<-c("Treated","Synthetic","Gap")
  
  df_results_8<-list() 
  gap_8<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_8)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_8_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_8_time_sens<-synth(data.prep.obj = data_prepared_8_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_8,synth.res = synt_res_8,Main=treat)
  abline(v="2012", col="black")
  synt_res_8$solution.v
  donor_w_8
  tab_res_8
  
  plot(gap_treat_8,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_8)),2*max(abs(gap_treat_8))),main=treat)
  for( i in 1:ncol(gap_8)) {
    lines(gap_8[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_8,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_8,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_8)),2*max(abs(gap_treat_8))),main=treat)
  for( i in 1:ncol(gap_8_red)) {
    lines(gap_8_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_8,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_8_time_sens,synth.res = synt_res_8_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  treat<- "CZE_NLD"
  control_origins<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  control_destinations<-c("NLD","BEL","LUX","SWE","DNK","FRA","DEU","AUT")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_9<-dataprep(foo = data, predictors = pred,
                            dependent = dep, unit.variable = unit_num,
                            time.variable = time, treatment.identifier = treat,
                            controls.identifier = controls, time.predictors.prior = pretreat,
                            time.optimize.ssr = pretreat, time.plot = plot_per,
                            unit.names.variable = unit_name)
  
  synt_res_9<-synth(data.prep.obj = data_prepared_9,verbose = FALSE)
  donor_w_9 <-synt_res_9$solution.w
  rownames(donor_w_9)<- controls
  tab_res_9<-cbind(data_prepared_9$Y1plot,data_prepared_9$Y0plot %*% synt_res_9$solution.w,data_prepared_9$Y1plot - (data_prepared_9$Y0plot %*% synt_res_9$solution.w))
  rownames(tab_res_9)<-optimizer_per
  colnames(tab_res_9)<-c("Treated","Synthetic","Gap")
  
  df_results_9<-list() 
  gap_9<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_9)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
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
  
  data_prepared_9_time_sens<-dataprep(foo = data, predictors = pred,
                                      dependent = dep, unit.variable = unit_num,
                                      time.variable = time, treatment.identifier = treat,
                                      controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                      time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                      unit.names.variable = unit_name)
  
  synt_res_9_time_sens<-synth(data.prep.obj = data_prepared_9_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_9,synth.res = synt_res_9,Main=treat)
  abline(v="2012", col="black")
  synt_res_9$solution.v
  donor_w_9
  tab_res_9
  
  plot(gap_treat_9,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_9)),2*max(abs(gap_treat_9))),main=treat)
  for( i in 1:ncol(gap_9)) {
    lines(gap_9[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_9,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_9,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_9)),2*max(abs(gap_treat_9))),main=treat)
  for( i in 1:ncol(gap_9_red)) {
    lines(gap_9_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_9,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_9_time_sens,synth.res = synt_res_9_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  
  
  treat<- "CZE_GBR"
  control_origins<-c("SVK","HUN","SVN","POL","ROM","HRV","LTU")
  control_destinations<-c("GBR","ITA","FRA","DEU","ESP","NLD","BEL","IRL")
  conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
  conditions_2 <-eval(parse(text=paste("data[,3]=='", control_destinations,"'",sep="", collapse=" | ")))
  control_num<- length(control_origins)*length(control_destinations)
  controls<-unique(data[conditions_1 & conditions_2,1])
  
  
  data_prepared_10<-dataprep(foo = data, predictors = pred,
                             dependent = dep, unit.variable = unit_num,
                             time.variable = time, treatment.identifier = treat,
                             controls.identifier = controls, time.predictors.prior = pretreat,
                             time.optimize.ssr = pretreat, time.plot = plot_per,
                             unit.names.variable = unit_name)
  
  synt_res_10<-synth(data.prep.obj = data_prepared_10,verbose = FALSE)
  donor_w_10 <-synt_res_10$solution.w
  rownames(donor_w_10)<- controls
  tab_res_10<-cbind(data_prepared_10$Y1plot,data_prepared_10$Y0plot %*% synt_res_10$solution.w,data_prepared_10$Y1plot - (data_prepared_10$Y0plot %*% synt_res_10$solution.w))
  rownames(tab_res_10)<-optimizer_per
  colnames(tab_res_10)<-c("Treated","Synthetic","Gap")
  
  df_results_10<-list() 
  gap_10<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_10)<-controls
  l<-0
  for (i in controls){
    l<-l+1
    d_pre<-dataprep(foo = data, predictors = pred,
                    dependent = dep, unit.variable = unit_num,
                    time.variable = time, treatment.identifier = i,
                    controls.identifier = c(treat,controls[controls!=i]), time.predictors.prior = pretreat,
                    time.optimize.ssr = pretreat, time.plot = plot_per,
                    unit.names.variable = unit_name)
    df_results_10[[i]]<-synth(data.prep.obj = d_pre,verbose = FALSE)
    gap_10[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_10[[i]]$solution.w)
    
  }
  gap_treat_10 <- data_prepared_10$Y1plot - (data_prepared_10$Y0plot %*% synt_res_10$solution.w)
  
  gap_10_red<-matrix(0,nrow=length(optimizer_per),ncol=length(controls))
  colnames(gap_10_red)<-controls
  for (i in controls){
    if(df_results_10[[i]]$loss.v<5*synt_res_10$loss.v){
      gap_10_red[,i]<-gap_10[,i]}
  }
  
  data_prepared_10_time_sens<-dataprep(foo = data, predictors = pred,
                                       dependent = dep, unit.variable = unit_num,
                                       time.variable = time, treatment.identifier = treat,
                                       controls.identifier = controls, time.predictors.prior = pretreat_alt,
                                       time.optimize.ssr = pretreat_alt, time.plot = plot_per,
                                       unit.names.variable = unit_name)
  
  synt_res_10_time_sens<-synth(data.prep.obj = data_prepared_10_time_sens,verbose = FALSE)
  
  #Results
  
  path.plot(dataprep.res = data_prepared_10,synth.res = synt_res_10,Main=treat)
  abline(v="2012", col="black")
  synt_res_10$solution.v
  donor_w_10
  tab_res_10
  
  plot(gap_treat_10,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_10)),2*max(abs(gap_treat_10))),main=treat)
  for( i in 1:ncol(gap_10)) {
    lines(gap_10[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_10,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  plot(gap_treat_10,type="l",col="black",lty=1,lwd=2,ylim=c(-2*max(abs(gap_treat_10)),2*max(abs(gap_treat_10))),main=treat)
  for( i in 1:ncol(gap_10_red)) {
    lines(gap_10_red[,i],type="l",col="gray",lty=1,lwd=1)
  }
  lines(gap_treat_10,type="l",col="black",lty=1,lwd=2)
  abline(v="2012", col="black")
  
  path.plot(dataprep.res = data_prepared_10_time_sens,synth.res = synt_res_10_time_sens,Main=treat)
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  
  load("C:/Users/Teich/Desktop/Data diplomka/Cat_5/Synth_C5_results.RData")
  pairs<- c("CZE_DEU","CZE_SVK","CZE_POL","CZE_HUN","CZE_ITA","CZE_AUT","CZE_FRA","CZE_BEL","CZE_NLD")
  destinations<-c("Germany","Slovakia","Poland","Hungary","Italy","Austria","France","Belgium","Netherlands")
  axis<-c("2000","2005","2010","2015")
  ticks<-c(1,6,11,16)
  pairs_num<-c(1:length(pairs))
  cat_ver<- "5_Base"
  
  for (i in pairs_num){
    matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),main=pairs[i],xlab = "Year",ylab="Exports (mil. EUR)")
    abline(v="2012", col="black")
    
    plot(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2,xlab="Year", ylab= "Gap in exports (mil. EUR)",main=pairs[i],xaxt='n',ylim=c(-2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep="")))))),2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep=""))))))))
    axis(1, at= ticks,labels = axis)
    for( j in 1:ncol(eval(parse(text=(paste("gap_",i,sep="")))))) {
      lines(eval(parse(text=(paste("gap_",i,sep=""))))[,j],type="l",col="gray",lty=1,lwd=1)
    }
    lines(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2)
    abline(v=length(pretreat), col="black")
    
    plot(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2,xlab="Year", ylab= "Gap in exports (mil. EUR)",main=pairs[i],xaxt='n',ylim=c(-2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep="")))))),2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep=""))))))))
    axis(1, at= ticks,labels = axis)
    for( j in 1:ncol(eval(parse(text=(paste("gap_",i,"_red",sep="")))))) {
      lines(eval(parse(text=(paste("gap_",i,"_red",sep=""))))[,j],type="l",col="gray",lty=1,lwd=1)
    }
    lines(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2)
    abline(v=length(pretreat), col="black")
    
    matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,"_time_sens",sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),main=pairs[i],xlab = "Year",ylab="Exports (mil. EUR)")
    abline(v="2012", col="black")
    abline(v="2009", col="black",lty=2)
  } 
  
  setwd("C:/Users/Teich/Desktop/Data diplomka/Grafy_bil")
  for (i in pairs_num){
    pdf(paste("SCM_plot_",i,"_",cat_ver,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,2,1))
    matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),main=destinations[i],xlab = "Year",ylab="Exports (mil. EUR)")
    abline(v="2012", col="black")
    dev.off()
  }
  
  for (i in pairs_num){
    pdf(paste("gap_plot_",i,"_",cat_ver,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,2,1))
    plot(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2,xlab="Year",main=destinations[i], ylab= "Gap in exports (mil. EUR)",xaxt='n',ylim=c(-2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep="")))))),2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep=""))))))))
    axis(1, at= ticks,labels = axis)
    for( j in 1:ncol(eval(parse(text=(paste("gap_",i,sep="")))))) {
      lines(eval(parse(text=(paste("gap_",i,sep=""))))[,j],type="l",col="gray",lty=1,lwd=1)
    }
    lines(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2)
    abline(v=length(pretreat), col="black")
    dev.off()
  }
  
  for (i in pairs_num){
    pdf(paste("gap_plot_red_",i,"_",cat_ver,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,2,1))
    plot(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2,xlab="Year",main=destinations[i], ylab= "Gap in exports (mil. EUR)",xaxt='n',ylim=c(-2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep="")))))),2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep=""))))))))
    axis(1, at= ticks,labels = axis)
    for( j in 1:ncol(eval(parse(text=(paste("gap_",i,"_red",sep="")))))) {
      lines(eval(parse(text=(paste("gap_",i,"_red",sep=""))))[,j],type="l",col="gray",lty=1,lwd=1)
    }
    lines(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2)
    abline(v=length(pretreat), col="black")
    dev.off()
  }
  
  for (i in pairs_num){
    pdf(paste("SCM_time_sens_",i,"_",cat_ver,".png",sep=""),width=7, height=4) 
    par(mar=c(4,4,2,1))
    matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,"_time_sens",sep=""))))$solution.w),main=destinations[i],type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),xlab = "Year",ylab="Exports (mil. EUR)")
    abline(v="2012", col="black")
    abline(v="2009", col="black",lty=2)
    dev.off()
  }
  