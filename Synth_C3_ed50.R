library(Synth)
library(readstata13)
library(kernlab)
library(robustHD)
library(amap)

data <- read.dta13("C:/Users/Teich/Desktop/Data diplomka/Cat_3/Category_3.dta")
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

control_origins<- c('AUT', 'BEL', 'BGR', 'HRV', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'NLD', 'POL', 'PRT', 'ROM', 'SVK', 'SVN', 'ESP', 'SWE', 'GBR')

conditions_1 <-eval(parse(text=paste("data[,2]=='", control_origins,"'",sep="", collapse=" | ")))
conditions_2 <-eval(parse(text=paste("data[,3]=='", control_origins,"'",sep="", collapse=" | ")))
control_num<- length(control_origins)*(length(control_origins)-1)
controls<-unique(data[conditions_1 & conditions_2,1])

pretreat<-c(2000:2012)
optimizer_per<- c(2000:2016)
plot_per<-c(2000:2016)
pretreat_alt<-c(2000:2009)

treat<- "CZE_DEU"
X_1<- data[data[,1]==treat,]
X_1<-as.matrix(apply(X_1[X_1[,4]<2013,c(pred,dep)],2, mean))

X_0_prep<-data[conditions_1 & conditions_2,c(pred,dep,dep+1)]
colnames(X_0_prep) <- colnames(data)[c(pred,dep,dep+1)]
X_0_prep<-X_0_prep[data[,4]<2013,]
X_0<-matrix(0,nrow=(length(pred)+1),ncol=control_num)
for (i in 1:(length(pred)+1)){
  X_0[i,]<-tapply(X_0_prep[,i], X_0_prep[,ncol(X_0_prep)], mean)
}
colnames(X_0)<-controls

dt<- cbind(X_1,X_0)
for(i in 1:(length(pred)+1)){
  dt[i,]<- standardize(dt[i,], centerFun = mean, scaleFun = sd)
}
X_1<-dt[,1]
X_0<-dt[,2:(control_num+1)]
dist_mat<- rep(0, control_num)
names(dist_mat)<-controls

for (i in 1:control_num){
  dist_mat[i]<-Dist(t(cbind(X_1,X_0[,i])), method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}

dist_mat<-dist_mat[order(dist_mat, na.last=TRUE, decreasing=FALSE)[1:50]]
new_cont_1<-names(dist_mat)

data_prepared_1<-dataprep(foo = data, predictors = pred,
                        dependent = dep, unit.variable = unit_num,
                        time.variable = time, treatment.identifier = treat,
                        controls.identifier = new_cont_1, time.predictors.prior = pretreat,
                        time.optimize.ssr = pretreat, time.plot = plot_per,
                        unit.names.variable = unit_name)

synt_res_1<-synth(data.prep.obj = data_prepared_1,verbose=FALSE)
donor_w_1 <-synt_res_1$solution.w
rownames(donor_w_1)<- new_cont_1
tab_res_1<-cbind(data_prepared_1$Y1plot,data_prepared_1$Y0plot %*% synt_res_1$solution.w,data_prepared_1$Y1plot - (data_prepared_1$Y0plot %*% synt_res_1$solution.w))
rownames(tab_res_1)<-optimizer_per
colnames(tab_res_1)<-c("Treated","Synthetic","Gap")

df_results_1<-list() 
gap_1<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_1))
colnames(gap_1)<-new_cont_1
l<-0
for (i in new_cont_1){
  l<-l+1
  d_pre<-dataprep(foo = data, predictors = pred,
                  dependent = dep, unit.variable = unit_num,
                  time.variable = time, treatment.identifier = i,
                  controls.identifier = c(treat,new_cont_1[new_cont_1!=i]), time.predictors.prior = pretreat,
                  time.optimize.ssr = pretreat, time.plot = plot_per,
                  unit.names.variable = unit_name)
  df_results_1[[i]]<-synth(data.prep.obj = d_pre,verbose=FALSE)
  gap_1[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_1[[i]]$solution.w)
  
}
gap_treat_1 <- data_prepared_1$Y1plot - (data_prepared_1$Y0plot %*% synt_res_1$solution.w)

gap_1_red<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_1))
colnames(gap_1_red)<-new_cont_1
for (i in new_cont_1){
  if(df_results_1[[i]]$loss.v<5*synt_res_1$loss.v){
    gap_1_red[,i]<-gap_1[,i]}
}

data_prepared_1_time_sens<-dataprep(foo = data, predictors = pred,
                                    dependent = dep, unit.variable = unit_num,
                                    time.variable = time, treatment.identifier = treat,
                                    controls.identifier = new_cont_1, time.predictors.prior = pretreat_alt,
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
X_1<- data[data[,1]==treat,]
X_1<-as.matrix(apply(X_1[X_1[,4]<2013,c(pred,dep)],2, mean))

X_0_prep<-data[conditions_1 & conditions_2,c(pred,dep,dep+1)]
colnames(X_0_prep) <- colnames(data)[c(pred,dep,dep+1)]
X_0_prep<-X_0_prep[data[,4]<2013,]
X_0<-matrix(0,nrow=(length(pred)+1),ncol=control_num)
for (i in 1:(length(pred)+1)){
  X_0[i,]<-tapply(X_0_prep[,i], X_0_prep[,ncol(X_0_prep)], mean)
}
colnames(X_0)<-controls

dt<- cbind(X_1,X_0)
for(i in 1:(length(pred)+1)){
  dt[i,]<- standardize(dt[i,], centerFun = mean, scaleFun = sd)
}
X_1<-dt[,1]
X_0<-dt[,2:(control_num+1)]
dist_mat<- rep(0, control_num)
names(dist_mat)<-controls

for (i in 1:control_num){
  dist_mat[i]<-Dist(t(cbind(X_1,X_0[,i])), method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}

dist_mat<-dist_mat[order(dist_mat, na.last=TRUE, decreasing=FALSE)[1:50]]
new_cont_2<-names(dist_mat)

data_prepared_2<-dataprep(foo = data, predictors = pred,
                          dependent = dep, unit.variable = unit_num,
                          time.variable = time, treatment.identifier = treat,
                          controls.identifier = new_cont_2, time.predictors.prior = pretreat,
                          time.optimize.ssr = pretreat, time.plot = plot_per,
                          unit.names.variable = unit_name)

synt_res_2<-synth(data.prep.obj = data_prepared_2,verbose=FALSE)
donor_w_2 <-synt_res_2$solution.w
rownames(donor_w_2)<- new_cont_2
tab_res_2<-cbind(data_prepared_2$Y1plot,data_prepared_2$Y0plot %*% synt_res_2$solution.w,data_prepared_2$Y1plot - (data_prepared_2$Y0plot %*% synt_res_2$solution.w))
rownames(tab_res_2)<-optimizer_per
colnames(tab_res_2)<-c("Treated","Synthetic","Gap")

df_results_2<-list() 
gap_2<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_2))
colnames(gap_2)<-new_cont_2
l<-0
for (i in new_cont_2){
  l<-l+1
  d_pre<-dataprep(foo = data, predictors = pred,
                  dependent = dep, unit.variable = unit_num,
                  time.variable = time, treatment.identifier = i,
                  controls.identifier = c(treat,new_cont_2[new_cont_2!=i]), time.predictors.prior = pretreat,
                  time.optimize.ssr = pretreat, time.plot = plot_per,
                  unit.names.variable = unit_name)
  df_results_2[[i]]<-synth(data.prep.obj = d_pre,verbose=FALSE)
  gap_2[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_2[[i]]$solution.w)
  
}
gap_treat_2 <- data_prepared_2$Y1plot - (data_prepared_2$Y0plot %*% synt_res_2$solution.w)

gap_2_red<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_2))
colnames(gap_2_red)<-new_cont_2
for (i in new_cont_2){
  if(df_results_2[[i]]$loss.v<5*synt_res_2$loss.v){
    gap_2_red[,i]<-gap_2[,i]}
}

data_prepared_2_time_sens<-dataprep(foo = data, predictors = pred,
                                    dependent = dep, unit.variable = unit_num,
                                    time.variable = time, treatment.identifier = treat,
                                    controls.identifier = new_cont_2, time.predictors.prior = pretreat_alt,
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

treat<- "CZE_AUT"
X_1<- data[data[,1]==treat,]
X_1<-as.matrix(apply(X_1[X_1[,4]<2013,c(pred,dep)],2, mean))

X_0_prep<-data[conditions_1 & conditions_2,c(pred,dep,dep+1)]
colnames(X_0_prep) <- colnames(data)[c(pred,dep,dep+1)]
X_0_prep<-X_0_prep[data[,4]<2013,]
X_0<-matrix(0,nrow=(length(pred)+1),ncol=control_num)
for (i in 1:(length(pred)+1)){
  X_0[i,]<-tapply(X_0_prep[,i], X_0_prep[,ncol(X_0_prep)], mean)
}
colnames(X_0)<-controls

dt<- cbind(X_1,X_0)
for(i in 1:(length(pred)+1)){
  dt[i,]<- standardize(dt[i,], centerFun = mean, scaleFun = sd)
}
X_1<-dt[,1]
X_0<-dt[,2:(control_num+1)]
dist_mat<- rep(0, control_num)
names(dist_mat)<-controls

for (i in 1:control_num){
  dist_mat[i]<-Dist(t(cbind(X_1,X_0[,i])), method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}

dist_mat<-dist_mat[order(dist_mat, na.last=TRUE, decreasing=FALSE)[1:50]]
new_cont_3<-names(dist_mat)

data_prepared_3<-dataprep(foo = data, predictors = pred,
                          dependent = dep, unit.variable = unit_num,
                          time.variable = time, treatment.identifier = treat,
                          controls.identifier = new_cont_3, time.predictors.prior = pretreat,
                          time.optimize.ssr = pretreat, time.plot = plot_per,
                          unit.names.variable = unit_name)

synt_res_3<-synth(data.prep.obj = data_prepared_3,verbose=FALSE)
donor_w_3 <-synt_res_3$solution.w
rownames(donor_w_3)<- new_cont_3
tab_res_3<-cbind(data_prepared_3$Y1plot,data_prepared_3$Y0plot %*% synt_res_3$solution.w,data_prepared_3$Y1plot - (data_prepared_3$Y0plot %*% synt_res_3$solution.w))
rownames(tab_res_3)<-optimizer_per
colnames(tab_res_3)<-c("Treated","Synthetic","Gap")

df_results_3<-list() 
gap_3<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_3))
colnames(gap_3)<-new_cont_3
l<-0
for (i in new_cont_3){
  l<-l+1
  d_pre<-dataprep(foo = data, predictors = pred,
                  dependent = dep, unit.variable = unit_num,
                  time.variable = time, treatment.identifier = i,
                  controls.identifier = c(treat,new_cont_3[new_cont_3!=i]), time.predictors.prior = pretreat,
                  time.optimize.ssr = pretreat, time.plot = plot_per,
                  unit.names.variable = unit_name)
  df_results_3[[i]]<-synth(data.prep.obj = d_pre,verbose=FALSE)
  gap_3[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_3[[i]]$solution.w)
  
}
gap_treat_3 <- data_prepared_3$Y1plot - (data_prepared_3$Y0plot %*% synt_res_3$solution.w)

gap_3_red<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_3))
colnames(gap_3_red)<-new_cont_3
for (i in new_cont_3){
  if(df_results_3[[i]]$loss.v<5*synt_res_3$loss.v){
    gap_3_red[,i]<-gap_3[,i]}
}

data_prepared_3_time_sens<-dataprep(foo = data, predictors = pred,
                                    dependent = dep, unit.variable = unit_num,
                                    time.variable = time, treatment.identifier = treat,
                                    controls.identifier = new_cont_3, time.predictors.prior = pretreat_alt,
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

treat<- "CZE_POL"
X_1<- data[data[,1]==treat,]
X_1<-as.matrix(apply(X_1[X_1[,4]<2013,c(pred,dep)],2, mean))

X_0_prep<-data[conditions_1 & conditions_2,c(pred,dep,dep+1)]
colnames(X_0_prep) <- colnames(data)[c(pred,dep,dep+1)]
X_0_prep<-X_0_prep[data[,4]<2013,]
X_0<-matrix(0,nrow=(length(pred)+1),ncol=control_num)
for (i in 1:(length(pred)+1)){
  X_0[i,]<-tapply(X_0_prep[,i], X_0_prep[,ncol(X_0_prep)], mean)
}
colnames(X_0)<-controls

dt<- cbind(X_1,X_0)
for(i in 1:(length(pred)+1)){
  dt[i,]<- standardize(dt[i,], centerFun = mean, scaleFun = sd)
}
X_1<-dt[,1]
X_0<-dt[,2:(control_num+1)]
dist_mat<- rep(0, control_num)
names(dist_mat)<-controls

for (i in 1:control_num){
  dist_mat[i]<-Dist(t(cbind(X_1,X_0[,i])), method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}



dist_mat<-dist_mat[order(dist_mat, na.last=TRUE, decreasing=FALSE)[1:50]]
new_cont_4<-names(dist_mat)

data_prepared_4<-dataprep(foo = data, predictors = pred,
                          dependent = dep, unit.variable = unit_num,
                          time.variable = time, treatment.identifier = treat,
                          controls.identifier = new_cont_4, time.predictors.prior = pretreat,
                          time.optimize.ssr = pretreat, time.plot = plot_per,
                          unit.names.variable = unit_name)

synt_res_4<-synth(data.prep.obj = data_prepared_4,verbose=FALSE)
donor_w_4 <-synt_res_4$solution.w
rownames(donor_w_4)<- new_cont_4
tab_res_4<-cbind(data_prepared_4$Y1plot,data_prepared_4$Y0plot %*% synt_res_4$solution.w,data_prepared_4$Y1plot - (data_prepared_4$Y0plot %*% synt_res_4$solution.w))
rownames(tab_res_4)<-optimizer_per
colnames(tab_res_4)<-c("Treated","Synthetic","Gap")

df_results_4<-list() 
gap_4<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_4))
colnames(gap_4)<-new_cont_4
l<-0
for (i in new_cont_4){
  l<-l+1
  d_pre<-dataprep(foo = data, predictors = pred,
                  dependent = dep, unit.variable = unit_num,
                  time.variable = time, treatment.identifier = i,
                  controls.identifier = c(treat,new_cont_4[new_cont_4!=i]), time.predictors.prior = pretreat,
                  time.optimize.ssr = pretreat, time.plot = plot_per,
                  unit.names.variable = unit_name)
  df_results_4[[i]]<-synth(data.prep.obj = d_pre,verbose=FALSE)
  gap_4[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_4[[i]]$solution.w)
  
}
gap_treat_4 <- data_prepared_4$Y1plot - (data_prepared_4$Y0plot %*% synt_res_4$solution.w)

gap_4_red<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_4))
colnames(gap_4_red)<-new_cont_4
for (i in new_cont_4){
  if(df_results_4[[i]]$loss.v<5*synt_res_4$loss.v){
    gap_4_red[,i]<-gap_4[,i]}
}

data_prepared_4_time_sens<-dataprep(foo = data, predictors = pred,
                                    dependent = dep, unit.variable = unit_num,
                                    time.variable = time, treatment.identifier = treat,
                                    controls.identifier = new_cont_4, time.predictors.prior = pretreat_alt,
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


treat<- "CZE_HUN"
X_1<- data[data[,1]==treat,]
X_1<-as.matrix(apply(X_1[X_1[,4]<2013,c(pred,dep)],2, mean))

X_0_prep<-data[conditions_1 & conditions_2,c(pred,dep,dep+1)]
colnames(X_0_prep) <- colnames(data)[c(pred,dep,dep+1)]
X_0_prep<-X_0_prep[data[,4]<2013,]
X_0<-matrix(0,nrow=(length(pred)+1),ncol=control_num)
for (i in 1:(length(pred)+1)){
  X_0[i,]<-tapply(X_0_prep[,i], X_0_prep[,ncol(X_0_prep)], mean)
}
colnames(X_0)<-controls

dt<- cbind(X_1,X_0)
for(i in 1:(length(pred)+1)){
  dt[i,]<- standardize(dt[i,], centerFun = mean, scaleFun = sd)
}
X_1<-dt[,1]
X_0<-dt[,2:(control_num+1)]
dist_mat<- rep(0, control_num)
names(dist_mat)<-controls

for (i in 1:control_num){
  dist_mat[i]<-Dist(t(cbind(X_1,X_0[,i])), method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}



dist_mat<-dist_mat[order(dist_mat, na.last=TRUE, decreasing=FALSE)[1:50]]
new_cont_5<-names(dist_mat)

data_prepared_5<-dataprep(foo = data, predictors = pred,
                          dependent = dep, unit.variable = unit_num,
                          time.variable = time, treatment.identifier = treat,
                          controls.identifier = new_cont_5, time.predictors.prior = pretreat,
                          time.optimize.ssr = pretreat, time.plot = plot_per,
                          unit.names.variable = unit_name)

synt_res_5<-synth(data.prep.obj = data_prepared_5,verbose=FALSE)
donor_w_5 <-synt_res_5$solution.w
rownames(donor_w_5)<- new_cont_5
tab_res_5<-cbind(data_prepared_5$Y1plot,data_prepared_5$Y0plot %*% synt_res_5$solution.w,data_prepared_5$Y1plot - (data_prepared_5$Y0plot %*% synt_res_5$solution.w))
rownames(tab_res_5)<-optimizer_per
colnames(tab_res_5)<-c("Treated","Synthetic","Gap")

df_results_5<-list() 
gap_5<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_5))
colnames(gap_5)<-new_cont_5
l<-0
for (i in new_cont_5){
  l<-l+1
  d_pre<-dataprep(foo = data, predictors = pred,
                  dependent = dep, unit.variable = unit_num,
                  time.variable = time, treatment.identifier = i,
                  controls.identifier = c(treat,new_cont_5[new_cont_5!=i]), time.predictors.prior = pretreat,
                  time.optimize.ssr = pretreat, time.plot = plot_per,
                  unit.names.variable = unit_name)
  df_results_5[[i]]<-synth(data.prep.obj = d_pre,verbose=FALSE)
  gap_5[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_5[[i]]$solution.w)
  
}
gap_treat_5 <- data_prepared_5$Y1plot - (data_prepared_5$Y0plot %*% synt_res_5$solution.w)

gap_5_red<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_5))
colnames(gap_5_red)<-new_cont_5
for (i in new_cont_5){
  if(df_results_5[[i]]$loss.v<5*synt_res_5$loss.v){
    gap_5_red[,i]<-gap_5[,i]}
}

data_prepared_5_time_sens<-dataprep(foo = data, predictors = pred,
                                    dependent = dep, unit.variable = unit_num,
                                    time.variable = time, treatment.identifier = treat,
                                    controls.identifier = new_cont_5, time.predictors.prior = pretreat_alt,
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


treat<- "CZE_GBR"
X_1<- data[data[,1]==treat,]
X_1<-as.matrix(apply(X_1[X_1[,4]<2013,c(pred,dep)],2, mean))

X_0_prep<-data[conditions_1 & conditions_2,c(pred,dep,dep+1)]
colnames(X_0_prep) <- colnames(data)[c(pred,dep,dep+1)]
X_0_prep<-X_0_prep[data[,4]<2013,]
X_0<-matrix(0,nrow=(length(pred)+1),ncol=control_num)
for (i in 1:(length(pred)+1)){
  X_0[i,]<-tapply(X_0_prep[,i], X_0_prep[,ncol(X_0_prep)], mean)
}
colnames(X_0)<-controls

dt<- cbind(X_1,X_0)
for(i in 1:(length(pred)+1)){
  dt[i,]<- standardize(dt[i,], centerFun = mean, scaleFun = sd)
}
X_1<-dt[,1]
X_0<-dt[,2:(control_num+1)]
dist_mat<- rep(0, control_num)
names(dist_mat)<-controls

for (i in 1:control_num){
  dist_mat[i]<-Dist(t(cbind(X_1,X_0[,i])), method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}



dist_mat<-dist_mat[order(dist_mat, na.last=TRUE, decreasing=FALSE)[1:50]]
new_cont_6<-names(dist_mat)

data_prepared_6<-dataprep(foo = data, predictors = pred,
                          dependent = dep, unit.variable = unit_num,
                          time.variable = time, treatment.identifier = treat,
                          controls.identifier = new_cont_6, time.predictors.prior = pretreat,
                          time.optimize.ssr = pretreat, time.plot = plot_per,
                          unit.names.variable = unit_name)

synt_res_6<-synth(data.prep.obj = data_prepared_6,verbose=FALSE)
donor_w_6 <-synt_res_6$solution.w
rownames(donor_w_6)<- new_cont_6
tab_res_6<-cbind(data_prepared_6$Y1plot,data_prepared_6$Y0plot %*% synt_res_6$solution.w,data_prepared_6$Y1plot - (data_prepared_6$Y0plot %*% synt_res_6$solution.w))
rownames(tab_res_6)<-optimizer_per
colnames(tab_res_6)<-c("Treated","Synthetic","Gap")

df_results_6<-list() 
gap_6<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_6))
colnames(gap_6)<-new_cont_6
l<-0
for (i in new_cont_6){
  l<-l+1
  d_pre<-dataprep(foo = data, predictors = pred,
                  dependent = dep, unit.variable = unit_num,
                  time.variable = time, treatment.identifier = i,
                  controls.identifier = c(treat,new_cont_6[new_cont_6!=i]), time.predictors.prior = pretreat,
                  time.optimize.ssr = pretreat, time.plot = plot_per,
                  unit.names.variable = unit_name)
  df_results_6[[i]]<-synth(data.prep.obj = d_pre,verbose=FALSE)
  gap_6[,l] <- d_pre$Y1plot - (d_pre$Y0plot %*% df_results_6[[i]]$solution.w)
  
}
gap_treat_6 <- data_prepared_6$Y1plot - (data_prepared_6$Y0plot %*% synt_res_6$solution.w)

gap_6_red<-matrix(0,nrow=length(optimizer_per),ncol=length(new_cont_6))
colnames(gap_6_red)<-new_cont_6
for (i in new_cont_6){
  if(df_results_6[[i]]$loss.v<5*synt_res_6$loss.v){
    gap_6_red[,i]<-gap_6[,i]}
}

data_prepared_6_time_sens<-dataprep(foo = data, predictors = pred,
                                    dependent = dep, unit.variable = unit_num,
                                    time.variable = time, treatment.identifier = treat,
                                    controls.identifier = new_cont_6, time.predictors.prior = pretreat_alt,
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

load("C:/Users/Teich/Desktop/Data diplomka/Cat_3/Synth_C3_ed50_results.RData")
pairs<- c("CZE_DEU","CZE_SVK","CZE_AUT","CZE_POL","CZE_HUN")
axis<-c("2000","2005","2010","2015")
ticks<-c(1,6,11,16)
pairs_num<-c(1:length(pairs))
cat_ver<- "3_ed50"

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
  pdf(paste("SCM_plot_",i,"_",cat_ver,".pdf",sep=""),width=7, height=4) 
  par(mar=c(4,4,1,1))
  matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),xlab = "Year",ylab="Exports (mil. EUR)")
  abline(v="2012", col="black")
  dev.off()
}

for (i in pairs_num){
  pdf(paste("gap_plot_",i,"_",cat_ver,".pdf",sep=""),width=7, height=4) 
  par(mar=c(4,4,1,1))
  plot(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2,xlab="Year", ylab= "Gap in exports (mil. EUR)",xaxt='n',ylim=c(-2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep="")))))),2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep=""))))))))
  axis(1, at= ticks,labels = axis)
  for( j in 1:ncol(eval(parse(text=(paste("gap_",i,sep="")))))) {
    lines(eval(parse(text=(paste("gap_",i,sep=""))))[,j],type="l",col="gray",lty=1,lwd=1)
  }
  lines(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  dev.off()
}

for (i in pairs_num){
  pdf(paste("gap_plot_red_",i,"_",cat_ver,".pdf",sep=""),width=7, height=4) 
  par(mar=c(4,4,1,1))
  plot(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2,xlab="Year", ylab= "Gap in exports (mil. EUR)",xaxt='n',ylim=c(-2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep="")))))),2*max(abs(eval(parse(text=(paste("gap_treat_",i,sep=""))))))))
  axis(1, at= ticks,labels = axis)
  for( j in 1:ncol(eval(parse(text=(paste("gap_",i,"_red",sep="")))))) {
    lines(eval(parse(text=(paste("gap_",i,"_red",sep=""))))[,j],type="l",col="gray",lty=1,lwd=1)
  }
  lines(eval(parse(text=(paste("gap_treat_",i,sep="")))),type="l",col="black",lty=1,lwd=2)
  abline(v=length(pretreat), col="black")
  dev.off()
}

for (i in pairs_num){
  pdf(paste("SCM_time_sens_",i,"_",cat_ver,".pdf",sep=""),width=7, height=4) 
  par(mar=c(4,4,1,1))
  matplot(plot_per, cbind(eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y1plot,eval(parse(text=(paste("data_prepared_",i,"_time_sens",sep=""))))$Y0plot %*% eval(parse(text=(paste("synt_res_",i,"_time_sens",sep=""))))$solution.w),type="l",col=c("black","black"),lty=c(1,5),lwd=c(2,2),xlab = "Year",ylab="Exports (mil. EUR)")
  abline(v="2012", col="black")
  abline(v="2009", col="black",lty=2)
  dev.off()
}
