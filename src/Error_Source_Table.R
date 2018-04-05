## 1. Names of Reservoirs
Reservoir_Names_dim<-dim(Resname)

if(Reservoir_Names_dim[1]==(nR) && Reservoir_Names_dim[2]==1){
    Reservoir_Names_dim_A<-print("The dimension of the Reservoir_Names.csv file is correct")
  } else {
    Reservoir_Names_dim_A<-print("The dimension of the Reservoir_Names.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }


## 2. Names of Import
if (input$Imports > 0){
Import_Names_dim<-dim(impname)
if(Import_Names_dim[1]==nIMP && Import_Names_dim[2]==1){
    Import_Names_dim_A<-print("The dimension of the Import_Names.csv file is correct")
  } else {
    Import_Names_dim_A<-print("The dimension of the Import_Names.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }
}else{
  Import_Names_dim_A<-print("Import is ZERO, so Import_Names.csv not needed")
}

## 3. Names of Municipalities
Municipality_Names_dim<-dim(munname)

if(Municipality_Names_dim[1]==(nM) && Municipality_Names_dim[2]==1){
    Municipality_Names_dim_A<-print("The dimension of the Municipality_Names.csv file is correct")
  } else {
    Municipality_Names_dim_A<-print("The dimension of the Municipality_Names.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }

## 4. Total Demand
Total_Demand_dim<-dim(TotalDemand)

if(Total_Demand_dim[1]==(nM) && Total_Demand_dim[2]==2){
    Total_Demand_dim_A<-print("The dimension of the Total_Demand.csv file is correct")
  } else {
    Total_Demand_dim_A<-print("The dimension of the Total_Demand.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }

## 5. Cost of Water Supply
Cost_dim<-dim(costcsv)

if(Cost_dim[1]==(nR+nIMP+1) && Cost_dim[2]==(nM+1)){
    Cost_dim_A <- print("The dimension of the Cost.csv file is correct")
  } else {
    Cost_dim_A<-print("The dimension of the Cost.ccv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }

## 6. Maximum Capacity of Reservoirs
Maximum_Reservoir_Capacity_dim<-dim(resmaxcapacity)

if(Maximum_Reservoir_Capacity_dim[1]==(nR) && Maximum_Reservoir_Capacity_dim[2]==2){
    Maximum_Reservoir_Capacity_dim_A<- print("The dimension of the Maximum_Reservoir_Capacity.csv file is correct")
  } else {
    Maximum_Reservoir_Capacity_dim_A<-print("The dimension of the Maximum_Reservoir_Capacity.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }


## 7. Maximum Capacity of Import Sources
if (input$Imports > 0){
Maximum_Import_Capacity_dim<-dim(IMPmax)
if(Maximum_Import_Capacity_dim[1]==nIMP && Maximum_Import_Capacity_dim[2]==2){
    Maximum_Import_Capacity_dim_A<- print("The dimension of the Maximum_Import_Capacity.csv file is correct")
  } else {
    Maximum_Import_Capacity_dim_A<-print("The dimension of the Maximum_Import_Capacity.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }
}else{
  Maximum_Import_Capacity_dim_A<- print("Import is ZERO, so Maximum_Import_Capacity.csv not needed")
}

## 8. Evapotranspiration and Other Losses
Evapotranspiration_and_Losses_dim<-dim(evaporation)

if(Evapotranspiration_and_Losses_dim[1]==nR && Evapotranspiration_and_Losses_dim[2]==2){
    Evapotranspiration_and_Losses_dim_A<-print("The dimension of the Evapotranspiration_and_Losses.csv file is correct")
  } else {
    Evapotranspiration_and_Losses_dim_A<-print("The dimension of the Evapotranspiration_and_Losses.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }

## 9. Initial Reservoir Storage
Initial_Reservoir_Storage_dim<-dim(InitStorage)

if(Initial_Reservoir_Storage_dim[1]==nR && Initial_Reservoir_Storage_dim[2]==2){
    Initial_Reservoir_Storage_dim_A<- print("The dimension of the Initial_Reservoir_Storage.csv file is correct")
  } else {
    Initial_Reservoir_Storage_dim_A<-print("The dimension of the Initial_Reservoir_Storage.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }

## 10. Link between reservoir and Municipality
Reservoir_to_Municipality_dim<-dim(rm_connectivity)

if(Reservoir_to_Municipality_dim[1]==nR && Reservoir_to_Municipality_dim[2]==(nM+1)){
    Reservoir_to_Municipality_dim_A<-print("The dimension of the Reservoir_to_Municipality.csv file is correct")
  } else {
    Reservoir_to_Municipality_dim_A<-print("The dimension of the Reservoir_to_Municipality.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }


## 11. Link Between Import and Municipality
if (input$Imports > 0){
Import_to_Municipality_dim<-dim(im_connectivity)
if(Import_to_Municipality_dim[1]==nIMP && Import_to_Municipality_dim[2]==(nM+1)){
    Import_to_Municipality_dim_A<-print("The dimension of the Import_to_Municipality.csv file is correct")
  } else {
    Import_to_Municipality_dim_A<-print("The dimension of the Import_to_Municipality.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }
}else{
  Import_to_Municipality_dim_A<- print("Import is ZERO, so Import_to_Municipality.csv not needed")
}

## 12. Link between the reservoirs
Reservoir_to_Reservoir_dim<-dim(r_connectivity)

if(Reservoir_to_Reservoir_dim[1]==(nR+nIMP) && Reservoir_to_Reservoir_dim[2]==(nR+nIMP+1)){
    Reservoir_to_Reservoir_dim_A<-print("The dimension of the Reservoir_to_Reservoir.csv file is correct")
  } else {
    Reservoir_to_Reservoir_dim_A<-print("The dimension of the Reservoir_to_Reservoir.csv file is INCORRECT -- PLEASE CHECK YOUR .CSV FILES")
  }


Table_For_CSV<-rbind(Reservoir_Names_dim_A, 
          Import_Names_dim_A, 
          Municipality_Names_dim_A, 
          Total_Demand_dim_A, 
          Cost_dim_A, 
          Maximum_Reservoir_Capacity_dim_A, 
          Maximum_Import_Capacity_dim_A, 
          Evapotranspiration_and_Losses_dim_A, 
          Initial_Reservoir_Storage_dim_A, 
          Reservoir_to_Municipality_dim_A,
          Import_to_Municipality_dim_A,
          Reservoir_to_Reservoir_dim_A)
colnames(Table_For_CSV)<-" "
