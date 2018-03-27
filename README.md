# HydroBID-OPT
The present model permits the optimization of reservoirs management under uncertain climate described by an ensemble of streamflow forecast. The illustration provided here is focused on the State of Pernambuco, Brazil, and comprises the 5 reservoirs and water trucks to provide water to 19 municipalities.

The challenge is formulated as a linear optimization problem focused on the optimzation of costs and where failure is represented through a penalty.

# How to run the model
The model is written in R and requires the installation of lpSolve package to perform the optimization. One only need to run the main script in the src folder, which will produce analysis plots stored in the results folder.

# How to run the interface
The model is developed in the R Shiny interface, and is available at https://columbia-water-center.shinyapps.io/hydrobid_opt_reservoir_optimization/

# Developers 
The model, code, and interface have been developed by Ipsita Kumar, Laureline Josset, and Upmanu Lall, Columbia Water Center, Columbia University

# Acknowledgement
This study is funded by the Inter-American Development Bank (IADB), in partnership with Agência Pernambucana de Águas e Clima – APAC, RTI, and Arizona State University under the title “A Water Resources Decision Support System to Reduce Drought Vulnerability and Enable Adaptation to Climate Variability and Change in Pernambuco.” Laureline Josset is funded by the Swiss National Science Foundation (SNSF grant P2LAP2_161876).
