
################################################# PART 2
############################# Here you change nothing. It is creating matrices
posRMT <- function(i,j,t){
  pos<-0
  if(i<=nR){if(j<=nM){if(t<=nT){
    pos=i+nR*(j-1)+nR*nM*(t-1)
  }}}
  return(pos)
}
posJMT <- function(i,j,t){
  pos<-0
  if(i<=nIMP){if(j<=nM){if(t<=nT){
    pos=i+nIMP*(j-1)+nIMP*nM*(t-1)
  }}}
  return(pos)
}
posMTS <- function(j,t,s){
  pos<-0
  if(j<=nM){if(t<=nT){if(s<=nS){
    pos=j+nM*(t-1)+nM*nT*(s-1)
  }}}
  return(pos)
}
posRTS <- function(i,t,s){
  pos<-0
  if(i<=nR){if(t<=nT){if(s<=nS){
    pos=i+nR*(t-1)+nR*nT*(s-1)
  }}}
  return(pos)
}
posRT <- function(i,t){
  pos<-0
  if(i<=nR){if(t<=nT){
    pos=i+nR*(t-1)
  }}
  return(pos)
}
posJT <- function(i,t){
  pos<-0
  if(i<=nIMP){if(t<=nT){
    pos=i+nIMP*(t-1)
  }}
  return(pos)
}
posRS <- function(i,s){
  pos<-0
  if(i<=nR){if(s<=nS){
    pos=i+nR*(s-1)
  }}
  return(pos)
}
posMT <- function(j,t){
  pos<-0
  if(j<=nM){if(t<=nT){
    pos=j+nM*(t-1)
  }}
  return(pos)
}
posRM <- function(i,j){
  pos<-0
  if(i<=nR){if(j<=nM){
    pos=i+nR*(j-1)
  }}
  return(pos)
}


## read inputs 

## generate matrix for optimization problem

#################################################################################################################
######## Matrix generator in R - Laureline
######## laureline.josset@gmail.com
#################################################################################################################dim_Q <- nM*nR*nT
dim_Q <- nM*nR*nT
dim_IMP <- nM*nIMP*nT
dim_F <- nR*nT*nS
dim_releases <- nR*nT
dim_reservoirs <- nR*nT*nS
dim_2F <- nR*nT*nS
dim_tot <- dim_Q + dim_F + dim_releases + dim_reservoirs + dim_IMP + dim_2F

start_Q <- 0
index_Q <- 1:dim_Q
start_F <- dim_Q
index_F <- dim_Q + (1:dim_F)
start_releases <- dim_Q+dim_F
index_releases <- start_releases + (1:dim_releases)
start_reservoirs <- dim_Q+dim_F+dim_releases
index_reservoirs <- start_reservoirs + (1:dim_reservoirs)
start_IMP <- dim_Q+dim_F+dim_releases+dim_reservoirs
index_IMP <- start_IMP + (1:dim_IMP)
start_2F <- start_IMP + dim_IMP
index_2F <- start_2F + (1:dim_2F)

#__________________________________________________________________________________________________
### OBJECTIVE
Obj <- matrix(data=0,ncol=1,nrow=dim_tot)
Obj[index_Q,] <- array(costQ_rmt,dim=c(dim_Q,1))
Obj[index_F,] <- array(costF_rts,dim=c(dim_F,1))
Obj[index_2F,] <- array(0*costF_rts,dim=c(dim_2F,1))
if (nIMP > 0){
  Obj[index_IMP,] <- array(costIMP_jmt,dim=c(dim_IMP,1))
}

#__________________________________________________________________________________________________
## GENERATE CONSTRAINTS MATRIX
#-- DEMANDS
rhs_d <- array(D_mt,dim=c(nM*nT,1))
s_d <- array(">=",dim=c(nM*nT,1))
A_d <- array(0,dim=c(nM*nT,dim_tot))
for(t in 1:nT){for(m in 1:nM){
  for(r in 1:nR){
    A_d[posMT(m,t),start_Q + posRMT(r,m,t)] <- Mrm[r,m]  ## extraction from reservoir m to mun r
  }
  if (nIMP > 0){
    for(j in 1:nIMP){
      A_d[posMT(m,t),start_IMP + posJMT(j,m,t)] <- Mjm[j,m]
    }
  }
}}


#-- STREAMS
if(with_Pconstraints){
  # MIN
  rhs_pmin <- array(PCmin_rt,dim=c(dim_releases,1))
  s_pmin <- array(">",dim=c(dim_releases,1))
  A_pmin <- array(0,dim=c(dim_releases,dim_tot))
  for(l in 1:(nR*nT)){
    A_pmin[l, start_releases + l] <- 1
  }
  # MAX
  rhs_pmax <- array(PCmax_rt,dim=c(dim_releases,1))
  s_pmax <- array("<",dim=c(dim_releases,1))
  A_pmax <- array(0,dim=c(nR*nT,dim_tot))
  for(l in 1:(nR*nT)){
    A_pmax[l, start_releases + l] <- 1
  }
}

#-- IMPORTS
if(nIMP > 0){
  rhs_impmax <- array(IMPmax_jt,dim=c(nIMP*nT,1))
  s_impmax <- array("<",dim=c(nIMP*nT,1))
  A_impmax <- array(0,dim=c(nIMP*nT,dim_tot))
  for(j in 1:nIMP){for(t in 1:nT){for(m in 1:nM){
    A_impmax[posJT(j,t), start_IMP + posJMT(j,m,t)] <- 1
  }}}}

#-- RESERVOIR
# MIN
if(with_Smin_constraints){
  rhs_smin <- array(SCmin_rt,dim=c(dim_reservoirs,1))
  s_smin <- array(">",dim=c(dim_reservoirs,1))
  A_smin <- array(0,dim=c(dim_reservoirs,dim_tot))
  for(l in 1:(nR*nT*nS)){
    A_smin[l,start_reservoirs+l] <- 1
  }
}
# MAX
if(with_Smax_constraints){
  rhs_smax <- array(SCmax_rt,dim=c(dim_reservoirs,1))
  s_smax <- array("<",dim=c(dim_reservoirs,1))
  A_smax <- array(0,dim=c(dim_reservoirs,dim_tot))
  for(l in 1:(nR*nT*nS)){
    A_smax[l,l+start_reservoirs] <- 1
  }
} 

# RESERVOIR STATE EQUATIONS
rhs_s <- array(I_rts,dim=c(dim_reservoirs,1))
for(r in 1:nR){for(s in 1:nS){
  rhs_s[posRTS(r,1,s)] <- S_r0s[r,s]*(1-e_rts[r,1,s]) + I_rts[r,1,s]}}

s_s  <- array("=",dim=c(dim_reservoirs,1))

A_sQ <- array(0,dim=c(dim_reservoirs,dim_Q))
A_sF <- array(0,dim=c(dim_reservoirs,dim_F))
A_s2F <- array(0,dim=c(dim_reservoirs,dim_2F))
A_sP <- array(0,dim=c(dim_reservoirs,dim_releases))
A_sS <- array(0,dim=c(dim_reservoirs,dim_reservoirs))

for(r in 1:nR){
  for(s in 1:nS){
    t <- 1
    for(m in 1:nM){
      A_sQ[posRTS(r,t,s),posRMT(r,m,t)] <- Mrm[r,m]
    }
    A_sF[posRTS(r,t,s),posRTS(r,t,s)]<- -1
    A_s2F[posRTS(r,t,s),posRTS(r,t,s)]<- 1
    A_sS[posRTS(r,t,s),posRTS(r,t,s)]<- 1
    for(r_ in 1:nR){
      A_sP[posRTS(r,t,s),posRT(r_,t)] <- (r==r_)-Mrr[r,r_]}
    
    for(t in 2:nT){
      for(m in 1:nM){
        A_sQ[posRTS(r,t,s),posRMT(r,m,t)] <-Mrm[r,m]
      }
      A_sF[posRTS(r,t,s),posRTS(r,t,s)] <- -1
      A_s2F[posRTS(r,t,s),posRTS(r,t,s)] <- 1
      for(r_ in 1:nR){
        A_sP[posRTS(r,t,s),posRT(r_,t)] <- (r==r_)-Mrr[r,r_]
        for(t_ in 1:nT){
          A_sS[posRTS(r,t,s),posRTS(r_,t_,s)] <- (posRT(r,t)==posRT(r_,t_)) - (1-e_rts[r,t_,s])*((r==r_)*(t_+1==t))
        }}}}}

A_s <- array(0,dim=c(dim_reservoirs,dim_tot))
A_s[,index_Q] <- A_sQ
A_s[,index_F] <- A_sF
A_s[,index_2F] <- A_s2F
A_s[,index_releases] <- A_sP
A_s[,index_reservoirs] <- A_sS

#__________________________________________________________________________________________________
## COMPLETE OPTIMIZATION PROBLEM
rhs   <- rbind(rhs_d,rhs_s)
sense <- rbind(  s_d,  s_s)
A     <- rbind(  A_d,  A_s)
if(nIMP>0){
  rhs   <- rbind(rhs,rhs_impmax)
  sense <- rbind(sense,  s_impmax)
  A     <- rbind(  A,  A_impmax)  
}
if(with_Pconstraints){
  rhs   <- rbind(rhs,rhs_pmin,rhs_pmax)
  sense <- rbind(sense,  s_pmin,  s_pmax)
  A     <- rbind(  A,  A_pmin,  A_pmax)
}
if(with_Smax_constraints){
  rhs   <- rbind(rhs,rhs_smax)
  sense <- rbind(sense,  s_smax)
  A     <- rbind(  A,  A_smax)
}
if(with_Smin_constraints){
  rhs   <- rbind(rhs,rhs_smin)
  sense <- rbind(sense,  s_smin)
  A     <- rbind(  A,  A_smin)
}


## solving
require(lpSolve)
lpsol <- lp(direction = "min", objective.in=Obj, const.mat=A, const.dir=sense, const.rhs=rhs)
xval <- lpsol$solution
objval <- lpsol$objval



rQ=array(data=xval[index_Q],dim=c(nR,nM,nT))   ### The withdrawals from all reservoirs to all users
if(nIMP>0){
  rQIMP=array(data=xval[index_IMP],dim=c(nIMP,nM,nT))  ## Withdrawals from all import sources to all users
}
rF=array(data=xval[index_F],dim=c(nR,nT,nS))  ## Failure at each reservoir for each ensemble forecast
rF[rF < 0] <- 0
rS=array(data=xval[index_reservoirs],dim=c(nR,nT,nS))  ## Storage for each reservoir
rP=array(data=xval[index_releases],dim=c(nR,nT))  ## Releases from each reservoir
r2F=array(data=xval[index_2F],dim=c(nR,nT,nS))  ## Failure 2 -- Ignore this **** Laureline, can you figure out a better way to write this?

mLOC=apply(rQ,MARGIN=c(1,3),sum)
if (nIMP > 0){
  mIMP=apply(rQIMP,MARGIN=c(1,3),sum)
}
mALLres<-colSums(mLOC)
AB<-3

Withdrawal_All<-colSums(mLOC)
if (nIMP > 0){
  Imp_All<-colSums(mIMP)
}


rF_ensemble<-t(apply(rF, MARGIN=c(2,3), sum))


rFmedian_by_res<-apply(rF,MARGIN=c(1,2),median)
rFmedian<-colSums(rFmedian_by_res)
rFmedian_MF<-as.matrix(rFmedian)


rFmean_by_res<-apply(rF,MARGIN=c(1,2),mean)
rFmean<-colSums(rFmean_by_res)
rFmean_MF<-as.matrix(rFmean)

Withdrawal_by_res_minus_mean_fail<-mLOC - rFmean_by_res
Withdrawal_by_res_minus_mean_fail[Withdrawal_by_res_minus_mean_fail < 0] <- 0

Withdrawal_by_res_minus_median_fail<-mLOC - rFmedian_by_res
Withdrawal_by_res_minus_median_fail[Withdrawal_by_res_minus_median_fail < 0] <- 0

Withdrawal_All_Res_minus_mean_fail<-Withdrawal_All - rFmean
Withdrawal_All_Res_minus_mean_fail[Withdrawal_All_Res_minus_mean_fail < 0] <- 0

Withdrawal_All_Res_minus_median_fail<-Withdrawal_All - rFmedian
Withdrawal_All_Res_minus_median_fail[Withdrawal_All_Res_minus_median_fail < 0] <- 0

rQ_rt <- apply(rQ, MARGIN=c(1,3), sum)
rQ_rt_mean <- rQ_rt - rFmean_by_res
rQ_rt_mean<- round(rQ_rt_mean, digits = 4)

rQ_rt_median <- rQ_rt - rFmedian_by_res
rQ_rt_median<- round(rQ_rt_median, digits = 4)


Title<-c(" ", "Total Cost (in Million)", "Annual Average Cost (in Million)")



############ THIS PART IS TO CREATE THE TABLE WITH ALL THE RESULTS
mean_res_cost<- apply(costQ_rmt, MARGIN=c(1,3), mean)
median_res_cost<- apply(costQ_rmt, MARGIN=c(1,3), median)
Cost_Res_rt_mean<-rQ_rt_mean*mean_res_cost
Cost_Res_rt_mean <- round(Cost_Res_rt_mean, digits = 4)

Cost_Res_rt_median<-rQ_rt_median*median_res_cost
Cost_Res_rt_median <- round(Cost_Res_rt_median, digits = 4)


Cost_Res_r_mean<-rowSums(Cost_Res_rt_mean)
Res_name<-as.matrix(resname)
Cost_Res_r_annual_avg_mean<-Cost_Res_r_mean*(12/nT)
Cost_Res_r_annual_avg_mean<-round(Cost_Res_r_annual_avg_mean, digits = 4)

Cost_Res_r_median<-rowSums(Cost_Res_rt_median)
Cost_Res_r_annual_avg_median<-Cost_Res_r_median*(12/nT)
Cost_Res_r_annual_avg_median<-round(Cost_Res_r_annual_avg_median, digits = 4)


## Each Reservoir
Cost_from_each_reservoir_ALL_mean<-cbind(Res_name, Cost_Res_r_mean, Cost_Res_r_annual_avg_mean)
Cost_from_each_reservoir_ALL_median<-cbind(Res_name, Cost_Res_r_median, Cost_Res_r_annual_avg_median)
Total_Cost_Supply_Res_mean<-sum(Cost_Res_r_mean)
Total_Cost_Supply_Res_median<-sum(Cost_Res_r_median)
Annual_Avg_Cost_Supply_Res_mean<-sum(Cost_Res_r_annual_avg_mean)
Annual_Avg_Cost_Supply_Res_median<-sum(Cost_Res_r_annual_avg_median)

## Total Cost from Reservoirs 
Total_Cost_Supply_Res_ALL_mean<-cbind("Total Cost of Supply from Reservoirs",Total_Cost_Supply_Res_mean, Annual_Avg_Cost_Supply_Res_mean)
Total_Cost_Supply_Res_ALL_median<-cbind("Total Cost of Supply from Reservoirs",Total_Cost_Supply_Res_median, Annual_Avg_Cost_Supply_Res_median)

if(nIMP > 0){
  Cost_Imp_jmt<-rQIMP*costIMP_jmt
  Cost_Imp_jmt = round(Cost_Imp_jmt, digits = 4)
  Cost_Imp_jt<-apply(Cost_Imp_jmt, MARGIN=c(1,3), sum)
  Cost_Imp_j<-rowSums(Cost_Imp_jt)
  Imp_name<-as.matrix(impname)
  Cost_Imp_j_annual_avg<-Cost_Imp_j*(12/nT)
  Cost_Imp_j_annual_avg = round(Cost_Imp_j_annual_avg, digits = 4)
  
  ## Each Import
  Cost_from_each_Import_ALL<-cbind(Imp_name, Cost_Imp_j, Cost_Imp_j_annual_avg)
  
  Total_Cost_Supply_Imp<-sum(Cost_Imp_j)
  Annual_Avg_Cost_Supply_Imp<-sum(Cost_Imp_j_annual_avg)
  
  ## Total Cost from Import
  Total_Cost_Supply_Imp_ALL<-cbind("Total Cost of Supply from Import",Total_Cost_Supply_Imp, Annual_Avg_Cost_Supply_Imp)
}

if(nIMP > 0){
  Supply_Cost_mean<-sum(Cost_Res_r_mean,Cost_Imp_j)
  Annual_Avg_Supply_Cost_mean<-sum(Annual_Avg_Cost_Supply_Res_mean, Annual_Avg_Cost_Supply_Imp)
}else{
  Supply_Cost_mean<-sum(Cost_Res_r_mean)
  Annual_Avg_Supply_Cost_mean<-sum(Annual_Avg_Cost_Supply_Res_mean)
}

if(nIMP > 0){
  Supply_Cost_median<-sum(Cost_Res_r_median,Cost_Imp_j)
  Annual_Avg_Supply_Cost_median<-sum(Annual_Avg_Cost_Supply_Res_median, Annual_Avg_Cost_Supply_Imp)
}else{
  Supply_Cost_median<-sum(Cost_Res_r_median)
  Annual_Avg_Supply_Cost_median<-sum(Annual_Avg_Cost_Supply_Res_median)
}


## Total Cost of water Supply 
Total_Cost_of_water_supply_mean<-cbind("Total Cost of Water Supply from all sources", Supply_Cost_mean, Annual_Avg_Supply_Cost_mean)
Total_Cost_of_water_supply_median<-cbind("Total Cost of Water Supply from all sources", Supply_Cost_median, Annual_Avg_Supply_Cost_median)

mean_Failure_rt<- apply(rF, MARGIN = c(1,2), mean)
mean_Failure_rt<- round(mean_Failure_rt, digits = 4)
median_Failure_rt<- apply(rF, MARGIN = c(1,2), median)
median_Failure_rt<- round(median_Failure_rt, digits = 4)


mean_total_Failure<-sum(mean_Failure_rt)
median_total_Failure<-sum(median_Failure_rt)

mean_annual_avg_Failure<- mean_total_Failure*(12/nT)
mean_annual_avg_Failure<- round(mean_annual_avg_Failure, digits = 4)
median_annual_avg_Failure<- median_total_Failure*(12/nT)
median_annual_avg_Failure<- round(median_annual_avg_Failure, digits = 4)

## Total Cost of Failure (mean)
Total_Cost_Failure_mean<-cbind("Total Cost of Failure (Mean)", mean_total_Failure, mean_annual_avg_Failure)

## Total Cost of Failure (median)
Total_Cost_Failure_median<-cbind("Total Cost of Failure (Median)", median_total_Failure, median_annual_avg_Failure)


Total_Cost_mean<-sum(Supply_Cost_mean, mean_total_Failure)
Annual_Average_Cost_mean<-sum(Annual_Avg_Supply_Cost_mean, mean_total_Failure)

Total_Cost_median<-sum(Supply_Cost_median, median_total_Failure)
Annual_Average_Cost_median<-sum(Annual_Avg_Supply_Cost_median, median_total_Failure)

## Total Cost (Supply + Mean Failure)
Total_Cost_All_mean<-cbind("Total Cost (Supply + Mean Failure)", Total_Cost_mean, Annual_Average_Cost_mean)

## Total Cost (Supply + Median Failure)

Total_Cost_All_median<-cbind("Total Cost (Supply + Median Failure)", Total_Cost_median, Annual_Average_Cost_median)

### TABLE
if(nIMP > 0){
  Table_Results_mean <- rbind(Cost_from_each_reservoir_ALL_mean, 
                         Cost_from_each_Import_ALL, 
                         Total_Cost_Failure_mean, 
                         Total_Cost_Supply_Res_ALL_mean, 
                         Total_Cost_Supply_Imp_ALL, 
                         Total_Cost_of_water_supply_mean, 
                         Total_Cost_All_mean)
  colnames(Table_Results_mean)<-Title
  
}else{
  Table_Results_mean <- rbind(Cost_from_each_reservoir_ALL_mean, 
                         Total_Cost_Failure_mean, 
                         Total_Cost_Supply_Res_ALL_mean, 
                         Total_Cost_of_water_supply_mean, 
                         Total_Cost_All_mean)
  colnames(Table_Results_mean)<-Title
}


if(nIMP > 0){
  Table_Results_median <- rbind(Cost_from_each_reservoir_ALL_median, 
                              Cost_from_each_Import_ALL, 
                              Total_Cost_Failure_median, 
                              Total_Cost_Supply_Res_ALL_median, 
                              Total_Cost_Supply_Imp_ALL, 
                              Total_Cost_of_water_supply_median, 
                              Total_Cost_All_median)
  colnames(Table_Results_median)<-Title
}else{
  Table_Results_median <- rbind(Cost_from_each_reservoir_ALL_median, 
                              Total_Cost_Failure_median, 
                              Total_Cost_Supply_Res_ALL_median, 
                              Total_Cost_of_water_supply_median, 
                              Total_Cost_All_median)
  colnames(Table_Results_median)<-Title
}

######### Data Manager for downloading

### 1. Withdrawal 

if (nIMP > 0){
  rQ_IMP_rt<-apply(rQIMP, MARGIN=c(1,3), sum)
  rQ_IMP_rt<- round(rQ_IMP_rt, digits = 4)
}

if(nIMP > 0){
  all_supply_source_names<-rbind(Res_name, Imp_name)
}else{
  all_supply_source_names<-Res_name
}
## 1a. Withdrawal using mean failure
if (nIMP > 0){
  all_supply_source_withdrawal_mean<-rbind(rQ_rt_mean, rQ_IMP_rt)
  row.names(all_supply_source_withdrawal_mean)<-all_supply_source_names
}else{
  all_supply_source_withdrawal_mean<-rbind(rQ_rt_mean)
  row.names(all_supply_source_withdrawal_mean)<-all_supply_source_names
}

## 1b. Withdrawal using median failure
if (nIMP > 0){
  all_supply_source_withdrawal_median<-rbind(rQ_rt_median, rQ_IMP_rt)
  row.names(all_supply_source_withdrawal_median)<-all_supply_source_names
}else{
  all_supply_source_withdrawal_median<-rbind(rQ_rt_median)
  row.names(all_supply_source_withdrawal_median)<-all_supply_source_names
}



### 2. Failure
rF_ts<-t(apply(rF, MARGIN = c(2,3), sum))
rF_ts<- round(rF_ts, digits = 4)
Ensemble_number <- c(paste0("Ensemble_", 1:nS))
Ensemble_number<-as.matrix(Ensemble_number)
Failure_Names<- rbind(Ensemble_number, "Mean Ensemble Failure", "Median Ensemble Failure")
rF_median_t<-apply(rF_ts, MARGIN=2, median)
rF_median_t<- round(rF_median_t, digits = 4)
rF_mean_t<-apply(rF_ts, MARGIN=2, mean)
rF_mean_t<- round(rF_mean_t, digits = 4)
rF_mean_t<-apply(rF_ts, MARGIN=2, mean)
## 2a. Failure
rF_for_ensembles_mean_and_median_results<-rbind(rF_ts, rF_mean_t, rF_median_t)
row.names(rF_for_ensembles_mean_and_median_results)<-Failure_Names


### 3. Supply to Municipalities from reservoirs 
rQ_mt <- apply(rQ, MARGIN=c(2,3), sum)
rQ_mt <- round(rQ_mt, digits = 4)
row.names(rQ_mt) <- munname

### 4. Supply to Municipalities from import
if (nIMP > 0){
  rQ_IMP_mt <- apply(rQIMP, MARGIN=c(2,3), sum)
  rQ_IMP_mt <- round(rQ_IMP_mt, digits = 4)
  row.names(rQ_mt) <- munname
}

### 5. Cost
Total_Res_Cost_t_mean<-colSums(Cost_Res_rt_mean)
if(nIMP > 0){
  Total_Imp_Cost_t<-colSums(Cost_Imp_jt)
  Total_SS_Cost_t_mean<-rbind(Total_Res_Cost_t_mean, Total_Imp_Cost_t)
  Total_SS_Cost_t_mean<-colSums(Total_SS_Cost_t_mean)
}else{
  Total_SS_Cost_t_mean<-Total_Res_Cost_t_mean
}

Total_Cost_mean_t<-rbind(Total_SS_Cost_t_mean, rF_mean_t)
Total_Cost_mean_t<-colSums(Total_Cost_mean_t)
# Total_Cost_median_t<-rbind(Total_SS_Cost_t_median, rF_median_t)
# Total_Cost_median_t<-colSums(Total_Cost_median_t)
if(nIMP > 0){
  Cost_t_mean<-rbind(Cost_Res_rt_mean, 
                Cost_Imp_jt, 
                rF_mean_t, 
                Total_Res_Cost_t_mean, 
                Total_Imp_Cost_t, 
                Total_SS_Cost_t_mean, 
                Total_Cost_mean_t)
  row_names_cost_mean<-c(resname,
                    impname,
                    "Total Cost of Failure (mean)", 
                    "Total Cost of Supply from Reservoirs", 
                    "Total Cost of Supply from Import",
                    "Total Cost of Water Supply from all sources",
                    "Total Cost (Supply + mean Failure)"
  )
  row.names(Cost_t_mean)<-row_names_cost_mean
}else{
  Cost_t_mean<-rbind(Cost_Res_rt_mean, 
                rF_mean_t,
                Total_Res_Cost_t_mean, 
                Total_SS_Cost_t_mean, 
                Total_Cost_mean_t)
  row_names_cost<-c(resname,
                    "Total Cost of Failure (mean)", 
                    "Total Cost of Supply from Reservoirs", 
                    "Total Cost of Water Supply from all sources",
                    "Total Cost (Supply + mean Failure)"
  )
  row.names(Cost_t_mean)<-row_names_cost
}


Total_Res_Cost_t_median<-colSums(Cost_Res_rt_median)
if(nIMP > 0){
  Total_Imp_Cost_t<-colSums(Cost_Imp_jt)
  Total_SS_Cost_t_median<-rbind(Total_Res_Cost_t_median, Total_Imp_Cost_t)
  Total_SS_Cost_t_median<-colSums(Total_SS_Cost_t_median)
}else{
  Total_SS_Cost_t_median<-Total_Res_Cost_t_median
}

Total_Cost_median_t<-rbind(Total_SS_Cost_t_median, rF_median_t)
Total_Cost_median_t<-colSums(Total_Cost_median_t)
if(nIMP > 0){
  Cost_t_median<-rbind(Cost_Res_rt_median, 
                       Cost_Imp_jt, 
                       rF_median_t, 
                       Total_Res_Cost_t_median, 
                       Total_Imp_Cost_t, 
                       Total_SS_Cost_t_median, 
                       Total_Cost_median_t)
  row_names_cost_median<-c(resname,
                           impname,
                           "Total Cost of Failure (median)", 
                           "Total Cost of Supply from Reservoirs", 
                           "Total Cost of Supply from Import",
                           "Total Cost of Water Supply from all sources",
                           "Total Cost (Supply + median Failure)"
  )
  row.names(Cost_t_median)<-row_names_cost_median
}else{
  Cost_t_median<-rbind(Cost_Res_rt_median, 
                       rF_median_t,
                       Total_Res_Cost_t_median, 
                       Total_SS_Cost_t_median, 
                       Total_Cost_median_t)
  row_names_cost<-c(resname,
                    "Total Cost of Failure (median)", 
                    "Total Cost of Supply from Reservoirs", 
                    "Total Cost of Water Supply from all sources",
                    "Total Cost (Supply + median Failure)"
  )
  row.names(Cost_t_median)<-row_names_cost
}

########################################################################
########################################################################
################## WITHDRAWAL PLOTS -- SORTING DATA ####################
########################################################################
########################################################################

mLOC_melt_mean<-melt(Withdrawal_by_res_minus_mean_fail)
mLOC_melt_mean<-arrange(mLOC_melt_mean, Var1)
mLOC_melt_mean<-as.matrix(mLOC_melt_mean)

mLOC_melt_median<-melt(Withdrawal_by_res_minus_median_fail)
mLOC_melt_median<-arrange(mLOC_melt_median, Var1)
mLOC_melt_median<-as.matrix(mLOC_melt_median)


if (nIMP>0){
mIMP_melt<-melt(mIMP)
mIMP_melt<-arrange(mIMP_melt, Var1)
mIMP_melt<-as.matrix(mIMP_melt)
}

rF_ensemble_melt<-melt(rF_ensemble)
rF_ensemble_melt<-arrange(rF_ensemble_melt, Var1)
rF_ensemble_melt<-as.matrix(rF_ensemble_melt)

Withdrawal_Reservoir_mean<-as.matrix(mLOC_melt_mean[,3])
Withdrawal_Reservoir_median<-as.matrix(mLOC_melt_median[,3])
Withdrawal_All_Reservoir_mean<-as.matrix(Withdrawal_All_Res_minus_mean_fail)
Withdrawal_All_Reservoir_median<-as.matrix(Withdrawal_All_Res_minus_median_fail)

if(nIMP>0){
Withdrawal_All_Import<-as.matrix(Imp_All)
Withdrawal_Import<-as.matrix(mIMP_melt[,3])
}

Ensemble_Failure<-as.matrix(rF_ensemble_melt[,3])


if (nIMP>0){
All_Res_Import_MeanFail<-rbind(Withdrawal_All_Reservoir_mean, Withdrawal_All_Import, rFmean_MF)
All_Res_Import_MedianFail<-rbind(Withdrawal_All_Reservoir_mean, Withdrawal_All_Import, rFmedian_MF)
All_Name_All_mean<-rep(c("All Reservoirs", "All Import Sources", "Mean Failure"), each=nT)
All_Name_All_median<-rep(c("All Reservoirs", "All Import Sources", "Median Failure"), each=nT)
Time_all<-rep(1:nT, times=3)
}else{
  All_Res_Import_MeanFail<-rbind(Withdrawal_All_Reservoir_mean, rFmean_MF)
  All_Res_Import_MedianFail<-rbind(Withdrawal_All_Reservoir_mean, rFmedian_MF)
  All_Name_All_mean<-rep(c("All Reservoirs", "Mean Failure"), each=nT)
  All_Name_All_median<-rep(c("All Reservoirs", "Median Failure"), each=nT)
  Time_all<-rep(1:nT, times=2)
}

ALL_Mean_Total<-data.frame(Legend=All_Name_All_mean,
                           Time_all=Time_all,
                           All_Res_Import_MeanFail=All_Res_Import_MeanFail)


ALL_Median_Total<-data.frame(Legend=All_Name_All_median,
                             Time_all=Time_all,
                             All_Res_Import_MedianFail=All_Res_Import_MedianFail)



Individual_Res_Name<-rep(resname,each=nT)
Tt<-rep(1:nT, times=nR)
Withdrawal_Reservoir_mean_DF<-data.frame(Individual_Res_Name=Individual_Res_Name,
                                         Tt=Tt,
                                         Withdrawal_Reservoir_mean=Withdrawal_Reservoir_mean)
 
Withdrawal_Reservoir_median_DF<-data.frame(Individual_Res_Name,
                                           Tt,
                                           Withdrawal_Reservoir_median)


if(nIMP>0){
Individual_Imp_Name<-rep(impname,each=nT)
TT<-rep(1:nT, times=nIMP)
Withdrawal_Import_DF<-data.frame(Individual_Imp_Name=Individual_Imp_Name,
                                 TT=TT,
                                 Withdrawal_Import=Withdrawal_Import)
}



################################################################################
################################################################################
################################   STORAGE PLOT ################################
################################################################################
################################################################################
################################################################################

rS_Melt<-melt(rS)
resname_rS<-rep(resname, times=nR*nT*nS)
Storage<-data.frame(
  Reservoir=resname_rS,
  Stor_Time=rS_Melt[,2],
  Stor_Ens=rS_Melt[,3],
  Storage=rS_Melt[,4]
)


################################################################################
################################################################################
################   ENSEMBLE, MEAN AND MEDIAN FAILURE ###########################
################################################################################
################################################################################
################################################################################

All_Failure_Matrix<-rbind(Ensemble_Failure, rFmean_MF, rFmedian_MF)
Mean_Med_Fail<-as.matrix(rbind(rFmean_MF, rFmedian_MF))
Mean_Med_Failure_Names<-c("Mean Failure", "Median Failure")
Mean_Med_Failure_Names<-rep(Mean_Med_Failure_Names, each=nT)
TimeFMM<-rep(1:12, times=2)
Mean_Med_Failure <- data.frame(
  Mean_Med_Failure_Names = Mean_Med_Failure_Names,
  TimeFMM = TimeFMM,
  Mean_Med_Fail = Mean_Med_Fail
)


Failure_Names<-rep(Ensemble_number, each=nT)
timeF<-rep(1:12, times=nS)
Ens_Fail <- data.frame(
  Failure_Names = Failure_Names,
  timeF = timeF,
  Ensemble_Failure = Ensemble_Failure
)