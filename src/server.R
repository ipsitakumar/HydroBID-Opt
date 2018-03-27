function(input, output, session) {
  Months <- reactive({input$Time})
  Ensembles <- reactive({input$Ensemble})
  Reservoirs <- reactive({input$Reservoirs})
  Users <- reactive({input$Users})
  Import <- reactive({input$Imports})
  
  
  ## 1. Names of Reservoirs
  Resnamecsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Reservoir_Names.csv"] , check.names=F, header = T)
  })
  
  ## 2. Names of Import Sources
  Impnamecsv <- reactive({
    if (input$Imports > 0){
    read.csv(file=input$directory$datapath[input$directory$name=="Import_Names.csv"] , check.names=F, header = T)
      }
  })
  
  # 3. Names of Users
  Munnamecsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="User_Names.csv"] , check.names=F, header = T)
  })
  
  ## 4. Total Demand
  Demandcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Total_Demand.csv"] , check.names=F, header = T)
  })
  
  ## 5. Cost of Water Supply
  Costcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Cost.csv"] , check.names=F, header = T)
  })
  
  ## 6. Maximum Capacity of Reservoirs
  Resmaxcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Maximum_Reservoir_Capacity.csv"] , check.names=F, header = T)
  })
  
  ## 7. Maximum Capacity of Import Sources
  Impmaxcsv <- reactive({
    if (input$Imports > 0){
      read.csv(file=input$directory$datapath[input$directory$name=="Maximum_Import_Capacity.csv"] , check.names=F, header = T)
    }
  })
  
  ## 8. Evapotranspiration and Other Losses
  Evapcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Evapotranspiration_and_Losses.csv"] , check.names=F, header = T)
  })
  
  ## 9. Initial Reservoir Storage
  InitStorcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Initial_Reservoir_Storage.csv"] , check.names=F, header = T)
  })
  
  ## 10. Link between reservoir and Users
  mRmcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Reservoir_to_User.csv"] , check.names=F, header = T)
  })
  
  ## 11. Link Between Import and Users
  mIMPmcsv <- reactive({
    if (input$Imports > 0){
      read.csv(file=input$directory$datapath[input$directory$name=="Import_to_User.csv"] , check.names=F, header = T)
    }
  })
  
  ## 12. Link between the reservoirs
  mrrcsv <- reactive({
    read.csv(file=input$directory$datapath[input$directory$name=="Reservoir_to_Reservoir.csv"] , check.names=F, header = T)
  })
  
  ## 13. Inflows from the various reservoirs
  Reservoir_Inflow<- reactive({
    Inflows_rts<-array(data=0 , c(input$Reservoirs,input$Time,input$Ensemble))
    for (abc in 1:input$Reservoirs){
      Inflows_rts[abc,,]<-as.matrix(read.csv(file = input$directory$datapath[input$directory$name==paste0("Reservoir_", abc, ".csv" )], check.names=F, header = T))
    }
    Inflows_rts
  })
  
  
  ######### Numeric Check 
  
  Num_check<-reactive({
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    nM<-Users()
    nIMP<-Import()
    Table_Numeric<- array(data=NA, dim<-c(5,2))
    Col_One<-c("Total Number of Reservoirs:", "Total Number of Users:", "Total Number of Import Sources:", "Total Number of Months for Future Decision:", "Total Number of Ensemble Streamflow Forecasts:")
    Col_Two<-rbind(nR, nM, nIMP, nT, nS)
    Table_Numeric<-cbind(Col_One, Col_Two)
    colnames(Table_Numeric)<-c(" ", " ")
    Table_Numeric
  })
  
  ###### Checking the CSV Data --- NO INFLOW
  
  CSV_Check_No_Inflow <- reactive({
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    nM<-Users()
    nIMP<-Import()
    
    ## 1. Names of Reservoirs
    Resname<-Resnamecsv()

    ## 2. Names of Import
    impname<-Impnamecsv()
    
    ## 3. Names of Users
    munname<-Munnamecsv()
    
    ## 4. Total Demand
    TotalDemand<-Demandcsv()
    
    ## 5. Cost of Water Supply
    costcsv<-Costcsv()
    
    ## 6. Maximum Capacity of Reservoirs
    resmaxcapacity<-Resmaxcsv()
    
    ## 7. Maximum Capacity of Import Sources
    IMPmax<-Impmaxcsv()
    
    ## 8. Evapotranspiration and Other Losses
    evaporation<-Evapcsv()
    
    ## 9. Initial Reservoir Storage
    InitStorage<-InitStorcsv()
    
    ## 10. Link between reservoir and Users
    rm_connectivity<-mRmcsv()
    
    ## 11. Link Between Import and Users
    im_connectivity<-mIMPmcsv()
    
    ## 12. Link between the reservoirs
    r_connectivity<-mrrcsv()
    
    source("Error_Source_Table.R", local = TRUE)
    
    Table_For_CSV
  })
  
  CSV_Only_Inflow <- reactive({
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    
    Checking_dim_Inflow<-array(data=0, dim=c(nR,1))
    colnames(Checking_dim_Inflow)<-" "
    
    Inflows_rts<-array(data=0 , c(nR,nT,nS))
    for (abc in 1:nR){
      dummy <- as.matrix(read.csv(file = input$directory$datapath[input$directory$name==paste0("Reservoir_", abc, ".csv" )], check.names=F, header = T))
      if(dim(dummy)[1] == nT && dim(dummy)[2] == nS){
        Inflows_rts[abc,,]<-dummy
        Checking_dim_Inflow[abc,]<- print(paste("The dimension of the Reservoir_", abc, ".csv file is correct"))
      }else{ 
        Checking_dim_Inflow[abc,]<- print(paste("The dimension of the Reservoir_", abc, ".csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES"))}
    }
    
    Checking_dim_Inflow
    
    
  })
  
  
  
  #### Running the model
  Cost <- eventReactive(input$Run_Model, {
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    nM<-Users()
    nIMP<-Import()
    
    ## 1. Names of Reservoirs
    Resname<-Resnamecsv()
    resname<-as.matrix(Resname)
    
    ## 2. Names of Import
    impname<-Impnamecsv()
    impname<-as.matrix(impname)
    
    ## 3. Names of Users
    munname<-Munnamecsv()
    munname<-as.matrix(munname)
    
    ## 4. Total Demand
    TotalDemand<-Demandcsv()
    D_mt <- array(data = TotalDemand[,2], dim = c(nM, nT)) * 1e-6 #in mio m3
    
    ## 5. Cost of Water Supply
    costcsv<-Costcsv()
    cost<-as.matrix(costcsv[,2:(nM+1)])
    costQ_rmt <- array(data = cost[1:nR, 1:nM], dim = c( nR, nM, nT))
    costIMP_jmt <- array(data = cost[nR + 1:nIMP, 1:nM], dim = c(nIMP, nM, nT))
    costF_rts <- array(data = cost[(nR+nIMP+1), 1:nM], dim = c(nR, nT, nS))
    
    ## 6. Maximum Capacity of Reservoirs
    resmaxcapacity<-Resmaxcsv()
    SCmax_rt<-array(data = resmaxcapacity[,2] * 1e-6, dim = c(nR, nT))
    
    ## 7. Maximum Capacity of Import Sources
    IMPmax<-Impmaxcsv()
    IMPmax_jt <- array(data = IMPmax[,2], dim = c(nIMP, nT))
    
    ## 8. Evapotranspiration and Other Losses
    evaporation<-Evapcsv()
    evaporation<-evaporation/100
    e_rts <- array(data = evaporation[,2], dim = c(nR, nT, nS))
    
    ## 9. Initial Reservoir Storage
    InitStorage<-InitStorcsv()
    S_r0s<-array(data = InitStorage[,2] * 1e-6, dim = c(nR, nS))
    
    ## 10. Link between reservoir and Users
    rm_connectivity<-mRmcsv()
    Mrm <- rm_connectivity[1:nR, 2:(nM+1)] 
    
    ## 11. Link Between Import and Users
    im_connectivity<-mIMPmcsv()
    Mjm <- im_connectivity[ 2:(nIMP+1), 2:(nM+1)]
    
    ## 12. Link between the reservoirs
    r_connectivity<-mrrcsv()
    Mrr <- r_connectivity[1:nR, 1+1:nR]
    
    ## 13. Inflows from the various reservoirs
    I_rts<-Reservoir_Inflow()
    I_rts<-array(I_rts, c(nR, nT, nS))
    
    ## Other constraints
    with_Pconstraints <- FALSE
    with_Smin_constraints <- FALSE
    with_Smax_constraints <- TRUE
    
    source("Source_Code_Feb5.R", local=TRUE)
    Table_Results
    })
  
  
  
  Plotting_Mean <- eventReactive(input$Run_Model, {
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    nM<-Users()
    nIMP<-Import()
    
    ## 1. Names of Reservoirs
    Resname<-Resnamecsv()
    resname<-as.matrix(Resname)
    
    ## 2. Names of Import
    impname<-Impnamecsv()
    impname<-as.matrix(impname)
    
    ## 3. Names of Users
    munname<-Munnamecsv()
    munname<-as.matrix(munname)
    
    ## 4. Total Demand
    TotalDemand<-Demandcsv()
    D_mt <- array(data = TotalDemand[,2], dim = c(nM, nT)) * 1e-6 #in mio m3
    
    ## 5. Cost of Water Supply
    costcsv<-Costcsv()
    cost<-as.matrix(costcsv[,2:(nM+1)])
    costQ_rmt <- array(data = cost[1:nR, 1:nM], dim = c( nR, nM, nT))
    costIMP_jmt <- array(data = cost[nR + 1:nIMP, 1:nM], dim = c(nIMP, nM, nT))
    costF_rts <- array(data = 20, dim = c(nR, nT, nS))
    
    ## 6. Maximum Capacity of Reservoirs
    resmaxcapacity<-Resmaxcsv()
    SCmax_rt<-array(data = resmaxcapacity[,2] * 1e-6, dim = c(nR, nT))
    
    ## 7. Maximum Capacity of Import Sources
    IMPmax<-Impmaxcsv()
    IMPmax_jt <- array(data = IMPmax[,2], dim = c(nIMP, nT))
    
    ## 8. Evapotranspiration and Other Losses
    evaporation<-Evapcsv()
    evaporation<-evaporation/100
    e_rts <- array(data = evaporation[,2], dim = c(nR, nT, nS))
    
    ## 9. Initial Reservoir Storage
    InitStorage<-InitStorcsv()
    S_r0s<-array(data = InitStorage[,2] * 1e-6, dim = c(nR, nS))
    
    ## 10. Link between reservoir and Users
    rm_connectivity<-mRmcsv()
    Mrm <- rm_connectivity[1:nR, 2:(nM+1)] 
    
    ## 11. Link Between Import and Users
    im_connectivity<-mIMPmcsv()
    Mjm <- im_connectivity[ 2:(nIMP+1), 2:(nM+1)]
    
    ## 12. Link between the reservoirs
    r_connectivity<-mrrcsv()
    Mrr <- r_connectivity[1:nR, 1+1:nR]
    
    ## 13. Inflows from the various reservoirs
    I_rts<-Reservoir_Inflow()
    I_rts<-array(I_rts, c(nR, nT, nS))
    
    ## Other constraints
    with_Pconstraints <- FALSE
    with_Smin_constraints <- FALSE
    with_Smax_constraints <- TRUE
    
    source("Source_Code_Feb5.R", local=TRUE)
    
    plot((Withdrawal_All),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1, cex.axis=1.25, cex.lab=1.25, cex=1.25)
    lines(rFmean,type="l", col=3, lwd=3,lty=6)
    lines(Imp_All, col = 2,lwd=3,lty=3)
    legend("topleft", c("","All Reservoirs","Import","Mean Failure"),  col = c(0,1,2,3),lty=c(0,1,2,3),pt.cex=1,lwd=3, cex=1.25,bty="n")
    title(paste0("Withdrawals from reservoirs and imports and \n mean failure for ", nT/12, " year(s)"), cex.main=1)
    
  })
  
  
  Plotting_Median <- eventReactive(input$Run_Model, {
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    nM<-Users()
    nIMP<-Import()
    
    ## 1. Names of Reservoirs
    Resname<-Resnamecsv()
    resname<-as.matrix(Resname)
    
    ## 2. Names of Import
    impname<-Impnamecsv()
    impname<-as.matrix(impname)
    
    ## 3. Names of Users
    munname<-Munnamecsv()
    munname<-as.matrix(munname)
    
    ## 4. Total Demand
    TotalDemand<-Demandcsv()
    D_mt <- array(data = TotalDemand[,2], dim = c(nM, nT)) * 1e-6 #in mio m3
    
    ## 5. Cost of Water Supply
    costcsv<-Costcsv()
    cost<-as.matrix(costcsv[,2:(nM+1)])
    costQ_rmt <- array(data = cost[1:nR, 1:nM], dim = c( nR, nM, nT))
    costIMP_jmt <- array(data = cost[nR + 1:nIMP, 1:nM], dim = c(nIMP, nM, nT))
    costF_rts <- array(data = 20, dim = c(nR, nT, nS))
    
    ## 6. Maximum Capacity of Reservoirs
    resmaxcapacity<-Resmaxcsv()
    SCmax_rt<-array(data = resmaxcapacity[,2] * 1e-6, dim = c(nR, nT))
    
    ## 7. Maximum Capacity of Import Sources
    IMPmax<-Impmaxcsv()
    IMPmax_jt <- array(data = IMPmax[,2], dim = c(nIMP, nT))
    
    ## 8. Evapotranspiration and Other Losses
    evaporation<-Evapcsv()
    evaporation<-evaporation/100
    e_rts <- array(data = evaporation[,2], dim = c(nR, nT, nS))
    
    ## 9. Initial Reservoir Storage
    InitStorage<-InitStorcsv()
    S_r0s<-array(data = InitStorage[,2] * 1e-6, dim = c(nR, nS))
    
    ## 10. Link between reservoir and Users
    rm_connectivity<-mRmcsv()
    Mrm <- rm_connectivity[1:nR, 2:(nM+1)] 
    
    ## 11. Link Between Import and Users
    im_connectivity<-mIMPmcsv()
    Mjm <- im_connectivity[ 2:(nIMP+1), 2:(nM+1)]
    
    ## 12. Link between the reservoirs
    r_connectivity<-mrrcsv()
    Mrr <- r_connectivity[1:nR, 1+1:nR]
    
    ## 13. Inflows from the various reservoirs
    I_rts<-Reservoir_Inflow()
    I_rts<-array(I_rts, c(nR, nT, nS))
    
    ## Other constraints
    with_Pconstraints <- FALSE
    with_Smin_constraints <- FALSE
    with_Smax_constraints <- TRUE
    
    source("Source_Code_Feb5.R", local=TRUE)
    
    plot((Withdrawal_All),type="l",ylab="Withdrawal [mio m3]",xlab = "months",col=1,ylim=c(min(0),max(sum(D_mt[,1]))),lwd=3,lty=1, cex.axis=1.25, cex.lab=1.25, cex=1.25)
    lines(rFmedian,type="l", col=3, lwd=3,lty=6)
    lines(Imp_All, col = 2,lwd=3,lty=3)
    legend("topleft", c("","All Reservoirs","Import","Median Failure"),  col = c(0,1,2,3),lty=c(0,1,2,3),pt.cex=1,lwd=3, cex=1.25,bty="n")
    title(paste0("Withdrawals from reservoirs and imports and \n median failure for ", nT/12, " year(s)"), cex.main=1)
    
  })
  
  Download_Data <- eventReactive(input$Run_Model, {
    nT<-Months()
    nS<-Ensembles()
    nR<-Reservoirs()
    nM<-Users()
    nIMP<-Import()
    
    ## 1. Names of Reservoirs
    Resname<-Resnamecsv()
    resname<-as.matrix(Resname)
    
    ## 2. Names of Import
    impname<-Impnamecsv()
    impname<-as.matrix(impname)
    
    ## 3. Names of Users
    munname<-Munnamecsv()
    munname<-as.matrix(munname)
    
    ## 4. Total Demand
    TotalDemand<-Demandcsv()
    D_mt <- array(data = TotalDemand[,2], dim = c(nM, nT)) * 1e-6 #in mio m3
    
    ## 5. Cost of Water Supply
    costcsv<-Costcsv()
    cost<-as.matrix(costcsv[,2:(nM+1)])
    costQ_rmt <- array(data = cost[1:nR, 1:nM], dim = c( nR, nM, nT))
    costIMP_jmt <- array(data = cost[nR + 1:nIMP, 1:nM], dim = c(nIMP, nM, nT))
    costF_rts <- array(data = 20, dim = c(nR, nT, nS))
    
    ## 6. Maximum Capacity of Reservoirs
    resmaxcapacity<-Resmaxcsv()
    SCmax_rt<-array(data = resmaxcapacity[,2] * 1e-6, dim = c(nR, nT))
    
    ## 7. Maximum Capacity of Import Sources
    IMPmax<-Impmaxcsv()
    IMPmax_jt <- array(data = IMPmax[,2], dim = c(nIMP, nT))
    
    ## 8. Evapotranspiration and Other Losses
    evaporation<-Evapcsv()
    evaporation<-evaporation/100
    e_rts <- array(data = evaporation[,2], dim = c(nR, nT, nS))
    
    ## 9. Initial Reservoir Storage
    InitStorage<-InitStorcsv()
    S_r0s<-array(data = InitStorage[,2] * 1e-6, dim = c(nR, nS))
    
    ## 10. Link between reservoir and Users
    rm_connectivity<-mRmcsv()
    Mrm <- rm_connectivity[1:nR, 2:(nM+1)] 
    
    ## 11. Link Between Import and Users
    im_connectivity<-mIMPmcsv()
    Mjm <- im_connectivity[ 2:(nIMP+1), 2:(nM+1)]
    
    ## 12. Link between the reservoirs
    r_connectivity<-mrrcsv()
    Mrr <- r_connectivity[1:nR, 1+1:nR]
    
    ## 13. Inflows from the various reservoirs
    I_rts<-Reservoir_Inflow()
    I_rts<-array(I_rts, c(nR, nT, nS))
    
    ## Other constraints
    with_Pconstraints <- FALSE
    with_Smin_constraints <- FALSE
    with_Smax_constraints <- TRUE
    
    source("Source_Code_Feb5.R", local=TRUE)
    
    switch(input$Data_Results,
           "Withdrawal from each source of supply (reservoirs and import sources) for each month" = all_supply_source_withdrawal,
           "Ensemble, mean, median failure for each month" = rF_for_ensembles_mean_and_median_results,
           "Supply for each use from all reservoirs for each month" = rQ_mt,
           "Supply for each user from all import sources for each month" = rQ_IMP_mt,
           "Cost of supply and failure for each month"= Cost_t )    
  })
  
  output$Numeric_Check<-renderText({(paste0("There are ", 
                                            input$Reservoirs, 
                                            " reservoirs <br> Serving ", 
                                            input$Users, 
                                            " users <br>",
                                            input$Imports, " import sources also provide water to these users<br>
                                            We are making water management decisions for ",
                                            input$Time,
                                            " months into the future using ",
                                            input$Ensemble,
                                            " ensemble streamflow forecasts for each reservoir<br><br><br><br><br><br>"
  ))
  })

  output$Numeric_Check<-renderTable(Num_check())
  
  
  output$csv_table<-renderTable(CSV_Check_No_Inflow())
  output$csv_table_Inflow<-renderTable(CSV_Only_Inflow())
  
  
  output$Table_Cost<-renderTable(Cost())
  output$plot_mean<-renderPlot({Plotting_Mean()})
  output$plot_median<-renderPlot({Plotting_Median()})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Data_Results, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Download_Data(), file)
    }
  )
}
