# MARopt
### MARopt: a tool for optimization of managed aquifer recharge measures

The MARopt tool is developed in the EviBAN project (https://www.sintef.no/projectweb/eviban/). MARopt tool is created for optimizing the placement of Managed Aquifer Recharge (MAR) measures at rural catchments, based on coupling a node-based hydrological model with a multi-objective optimization algorithm. MARopt allows the user to create a simple node model that can be calibrated and compared with measurement. Then, the calibrated hydrological model can be used to generate pareto optimal solutions for NBS based on cost and performance, which can be visualised and compared in the tool. 

The MARopt tool is available online in this link: https://eviban.shinyapps.io/maropt/ .

The input to the tool is an excel-file where the user can create a node model for the catchment and provide some inputs to the optimization algorithm. A template of the excel-file is provided in the folder "Example" in this repository. The excel file has the following sheets: 

![ScreenShot](/Figures/fig_excel.JPG)

* The first sheet "Setup" provide general information about the other sheets
* In the sheet "Measures", the user can proivde different MAR measures with their unit costs and perfromance (infiltration rate per unit area). 
* In the sheet "Nodes", the user can provide properties of the different nodes in the area. Each entity (e.g. sub-catchments, reservoirs, agricultural projects) in the study area must be presented as a node. Each node has an area, mean annual precipitation, precipitation station, and Retention. The retention represents all water losses and consumption that take place at each node. In MARopt, the retention of each node is tuned to calibrate the model in order to match observation provided by the user.
* The sheet "catchments nodes" provides information about the subcatchment nodes (i.e., nodes that represent sub-catchment) since the MAR measures will be implemented in the sub-catchment nodes (i.e., increase the retention of the subcatchment nodes). In thi sheet the user must provide some constraints for the optimization (read the sheet information)
* The sheet "Network" can be used to create the network of the nodes (i.e., how the nodes are connected to one another) 
* The sheet "Rainfall" provides the precipitation of each station in the project. The precipitation is provided as a percentage of the mean annual precipitaion value of each month. 
* The sheet "Inflow" provides any external (from outside model boundaries) inflows to to each node for each month in the period modelled [Mm3/month]. Leave zero if not applicable. 
* The sheet "Measured flow" provides measured flow for model calibration in the period modelled [Mm3/month]. Leave zero if not available. 


The following steps explain how to use MARopt to optimize MAR measures for a catchment. we will use the file "MAR_uncalibrated.xlsx" as input to the tool: 

1. Click the link https://eviban.shinyapps.io/maropt/. The following page should appear:  

![ScreenShot](/Figures/fig1.JPG)

2. Choose tthe file "MAR_uncalibrated.xlsx". The tool will plot the results of the current model and compare it with observations at the selected node as shown below

![ScreenShot](/Figures/fig2.JPG)

3. To calibrate the model, choose the node that has measured flow and click "Run calibration". The results of the model calibration will be printed as follow   

![ScreenShot](/Figures/fig3.JPG)

4. You can export the calibrated parameters (the retention of each node) and paste in the retention column in the sheet "Nodes"

![ScreenShot](/Figures/fig4b.JPG)

5. To run multi objective optimization for MAR measures, go to the tab "Optimization", upload a calibrated model, and click on "Run multi objective optimization". After the optimization is finished, export the results file (.json) 

![ScreenShot](/Figures/fig5.JPG)

6. To visualize the results, go to the tab "Optimization results" and upload the result file (.json).  

![ScreenShot](/Figures/fig6.JPG)


7. You can visulaize the pareto front of the MAR measures and present different pareto optimal scenarios. You aslo can compare different pareto optiaml solutions together 

![ScreenShot](/Figures/fig7.JPG)



## License

[MIT](https://choosealicense.com/licenses/mit/)

# Acknowledgements 
This work was supported by the EviBAN project (Evidence based assessment of NWRM for sustainable water management). EviBAN is funded under the Water JPI 2018 Joint Call - WaterWorks 2017 on “Closing the Water Cycle Gap – Sustainable Management of Water Resources”, the research council of Norway (grant no 300281), and the Academy of Finland (no 326787). 
