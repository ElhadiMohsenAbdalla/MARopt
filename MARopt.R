library(shiny)
library(shinydashboard)
library(mco)
library(nsga2R)
library(swmmr)
library(lubridate)
library(zoo)
library(factorial2x2)
library(ggplot2)
library(shinybusy)
library(nloptr)
library(ggpubr)
library(rlang)
library(igraph)
library(DT)
library(readxl)
library(plotly)
library(viridis)
library(rio)
library(dplyr)
library(ggnewscale)
library(shinyFiles)
library(rlist)
library(hydroGOF)
library(DiagrammeR)
library(chron)
library(openxlsx)
library(gtools)
library(stringr)
#library(xlsx)
library(treemap)
library(data.tree)
library(DEoptim)
run_model=function(param,nodeID,inputrain,
                   inputNoder,inputNetwork,
                   inputinflow,sort1){
  ##### 
  Rainfall = as.matrix(inputrain[,-1])
  T1 = nrow(Rainfall)
  N1 = nrow(inputNoder)
  
  rain_mat = matrix(0,ncol =N1,nrow = T1)
  if(length(param)>0){
    inputNoder$Retention = param 
  }
  
  for(n in 1:(N1)){
    ind_rain = which(colnames(Rainfall)==inputNoder$Rainfall[n])
    rain1 = as.numeric(Rainfall[,ind_rain])*(inputNoder$MAP[n]/100000)*
      (inputNoder$`Area (km2)`[n])
    rain_mat[,n] = as.array(rain1*(1-inputNoder$Retention[n]))
  }
  
  ######
  Out_mat = rain_mat
  
  inflow_mat = as.matrix(inputinflow[,-1])
  
  N2 = nrow(inputNetwork)
  
  for(n in sort1){
    attr1 = which(inputNetwork$`To node`==n)
    k = 1
    if(length(attr1)>0){
      share2 = inputNetwork$Share[attr1]
      node2 = inputNetwork$`From node`[attr1]
      # rain1 = as.matrix(rain_mat2[,attr1])
      for(j in 1:length(attr1)){
        share1 = share2[j]
        
        if(j ==1){
          sum1 = share1*Out_mat[,node2[j]]
        }else{
          sum1 = sum1+(share1*Out_mat[,node2[j]])
        }
      }
      Out_mat[,n] = Out_mat[,n] + ((sum1 + inflow_mat[,n])*(1-inputNoder$Retention[n]))
    }
  }
  
  return(Out_mat[,as.numeric(nodeID)])
  
}

obj_fun_calib = function(param,nodeID,Qobs,inputrain,
                         inputNoder,inputNetwork,
                         inputinflow,sort1){
  
  ##### 
  Rainfall = as.matrix(inputrain[,-1])
  T1 = nrow(Rainfall)
  N1 = nrow(inputNoder)
  
  rain_mat = matrix(0,ncol =N1,nrow = T1)
  
  inputNoder$Retention = param 
  for(n in 1:(N1)){
    ind_rain = which(colnames(Rainfall)==inputNoder$Rainfall[n])
    rain1 = as.numeric(Rainfall[,ind_rain])*(inputNoder$MAP[n]/100000)*
      (inputNoder$`Area (km2)`[n])
    rain_mat[,n] = as.array(rain1*(1-inputNoder$Retention[n]))
  }
  
  
  Out_mat = rain_mat
  
  inflow_mat = as.matrix(inputinflow[,-1])
  
  N2 = nrow(inputNetwork)
  
  for(n in sort1){
    attr1 = which(inputNetwork$`To node`==n)
    k = 1
    if(length(attr1)>0){
      share2 = inputNetwork$Share[attr1]
      node2 = inputNetwork$`From node`[attr1]
      # rain1 = as.matrix(rain_mat2[,attr1])
      for(j in 1:length(attr1)){
        share1 = share2[j]
        
        if(j ==1){
          sum1 = share1*Out_mat[,node2[j]]
        }else{
          sum1 = sum1+(share1*Out_mat[,node2[j]])
        }
      }
      Out_mat[,n] = Out_mat[,n] + ((sum1 + inflow_mat[,n])*(1-inputNoder$Retention[n]))
    }
  }
  sim_q <- Out_mat[,as.numeric(nodeID)]
  
  kge1 =  hydroGOF::KGE(as.matrix(sim_q), Qobs)
  
  return(kge1*-1)
}

##########UI: ############################
ui <- dashboardPage(
  dashboardHeader(title = "MARopt"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calibration",  tabName = "calib", icon = icon("dashboard")),
      menuItem("Optimization",  tabName = "Optimization", icon = icon("dashboard")),
      menuItem("Optimization results",  tabName = "optim_results", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "cube-grid", margins = c(0, 10), color = "#FFF"),
    tabItems(
      tabItem(tabName = "calib",
              fluidRow(
                h1("Calibration of the Node model"),
                column(width = 12,
                box(fileInput("file_calib", "Choose uncalibrated model file (.xlsx)",
                              accept = ".xlsx"),
                numericInput("node1","Plot Node number",value = 5),
                plotlyOutput("plot_current"),
                status = "primary", 
                solidHeader = TRUE,
                title = "Current model",
                width = 2.4
                ),
                box(
                  numericInput("num_calib","Number of iterations",value = 5),
                  actionButton("act_calib","Run calibration"),
                  textOutput("Calib"),
                  downloadButton("downloadcalib", "Export calibrated parameters (.csv)"),
                  plotlyOutput("plot_calib"),
                  status = "primary", 
                  solidHeader = TRUE,
                  title = "Calibration",
                  width = 2.4
                ))
      )),
      
      tabItem(tabName = "Optimization",
              fluidPage(
                
                h1("Multi objective optimization of MAR"),
                fileInput("file1", "Choose calibrated model file (.xlsx)", accept = ".xlsx"),
                box(
                  numericInput("num1","Number of iterations",value = 200),
                  actionButton("act1","Run multi objective optimization"),
                  textOutput("MultiOpt"),
                  downloadButton("downloadData", "Export results")
              )
      )),
      # First tab content
      tabItem(tabName = "optim_results",
              fluidPage(
                h1("Optimization results"),
                
                tabsetPanel(type = "tabs",
                            #tabPanel("Catchment", tableOutput("Cathment")),
                            tabPanel("Load optimization results (Multi objectives)", 
                                     fileInput("file2", "Choose .json result File", accept = ".json"),
                                     numericInput("num6","Pareto solution",value = 1),
                                     box(plotlyOutput("pareto_front"),title = "Optimization results"),
                                     box(plotlyOutput("Rainfall_Runoff_pareto"),title = "Optimization results")),
                            tabPanel("Compare pareto solutions", 
                                     box(dataTableOutput("compare_pareto_input"),title = "Compare pareto solutions (input)"),
                                     box(plotlyOutput("pareto_plot1"),title = "Solution 1"),
                                     box(plotlyOutput("pareto_plot2"),title = "Solution 2"),
                                     box(plotlyOutput("pareto_plot3"),title = "Solution 3"),
                                     box(plotlyOutput("pareto_plot4"),title = "Solution 4"),
                                     box(plotlyOutput("pareto_plot5"),title = "Solution 5"))
                            
                )
                
              )
      )
    )
  )
)



server <- function(input, output){
  
  currresplot = reactive({
    inFile <- input$file_calib
    if(length(inFile)>0){
    ExcelInOrig <- inFile$datapath
      
   
    inputNoder= read_excel(ExcelInOrig, sheet='Nodes',skip=1)
    inputNetwork= read_excel(ExcelInOrig, sheet='Network',skip=1)
    inputrain= read_excel(ExcelInOrig, sheet="Rainfall")
    inputcatch= read_excel(ExcelInOrig, sheet='catchments nodes',skip=1)
    inputmeasures= read_excel(ExcelInOrig, sheet='Measures',skip=1)
    inputinflow= read_excel(ExcelInOrig, sheet='Inflow')
    inputmeasured= read_excel(ExcelInOrig, sheet='Measured flow')
    ###########################################################
    nc = nrow(inputcatch) # number of catchments 
    ns = nrow(inputmeasures) # number of solutions
    snames = inputmeasures$Measure # names of solutions
    cnames = inputcatch$`catchment names` # names of catchments
    node_catch =inputcatch$`catchment outlet node`
    cost1 = inputmeasures$Cost # cost per m2 of NBS, DAM, pump
    rate1 = inputmeasures$`Mm3/m2/month` # rate of recharge per month per m2
    outlet1 = as.numeric(inputcatch$node[1]) # outlet of the whole system
    kt = as.data.frame(t(inputcatch[,c(5:(5+ns-1))])) # transportation factor
    ke = as.data.frame(t(inputcatch[,c((5+ns):(5+ns+ns-1))])) # energy factor
    kl = as.data.frame(t(inputcatch[,c((5+ns+ns):(5+ns+ns+ns-1))])) # local condition factor 
    limit1 =  as.data.frame(t(inputcatch[,c((5+ns+ns+ns):(5+ns+ns+ns+ns-1))])) # limit of NBS for each catchment
    inf_cap = as.numeric(inputcatch$`Mm3/m2/month`)  # infiltration capacity of each basin
    ###########################################################
    tree_data = as.data.frame(inputNetwork)[c(1,2,3)]
    colnames(tree_data) <- c('from', 'to', 'w')
    tree1 <- graph_from_data_frame(tree_data)
    sort1 = topo_sort(tree1,mode = "out")
    nodeID = input$node1
    Qobs = as.data.frame(inputmeasured[,(nodeID+1)])
    ######################
    qsim = run_model(param=NULL,
                     nodeID=nodeID,
                     inputrain=inputrain,
                     inputNoder=inputNoder,
                     inputNetwork=inputNetwork,
                     inputinflow=inputinflow,
                     sort1=sort1)
    time_step = c(1:nrow(inputrain))
    dataplot <- data.frame(time_step,Qobs,qsim)
    kge = hydroGOF::KGE(as.matrix(qsim), Qobs)
    title1 = paste0("Node no",nodeID,", KGE = ", round(kge, 2))
    colnames(dataplot) <- c("time_step","Qobs","qsim")
    fig <- plot_ly(dataplot, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~time_step, y = ~Qobs, name = 'Observed flow')%>%
      add_trace(x = ~time_step, y = ~qsim, name = 'Simulated flow')%>%
      layout(title = title1,xaxis = list(title = 'Time step (month)'), 
             yaxis = list(title = 'Outflow (Mm3/month)'))
    fig
    }
  })
  
  calibplot = reactive({
    inFile <- input$file_calib
    if(length(inFile)>0){
      res = calib_res()
      if(length(res)>0){
        ExcelInOrig <- inFile$datapath
        inputNoder= read_excel(ExcelInOrig, sheet='Nodes',skip=1)
        inputNetwork= read_excel(ExcelInOrig, sheet='Network',skip=1)
        inputrain= read_excel(ExcelInOrig, sheet="Rainfall")
        inputcatch= read_excel(ExcelInOrig, sheet='catchments nodes',skip=1)
        inputmeasures= read_excel(ExcelInOrig, sheet='Measures',skip=1)
        inputinflow= read_excel(ExcelInOrig, sheet='Inflow')
        inputmeasured= read_excel(ExcelInOrig, sheet='Measured flow')
        ###########################################################
        nc = nrow(inputcatch) # number of catchments 
        ns = nrow(inputmeasures) # number of solutions
        snames = inputmeasures$Measure # names of solutions
        cnames = inputcatch$`catchment names` # names of catchments
        node_catch =inputcatch$`catchment outlet node`
        cost1 = inputmeasures$Cost # cost per m2 of NBS, DAM, pump
        rate1 = inputmeasures$`Mm3/m2/month` # rate of recharge per month per m2
        outlet1 = as.numeric(inputcatch$node[1]) # outlet of the whole system
        kt = as.data.frame(t(inputcatch[,c(5:(5+ns-1))])) # transportation factor
        ke = as.data.frame(t(inputcatch[,c((5+ns):(5+ns+ns-1))])) # energy factor
        kl = as.data.frame(t(inputcatch[,c((5+ns+ns):(5+ns+ns+ns-1))])) # local condition factor 
        limit1 =  as.data.frame(t(inputcatch[,c((5+ns+ns+ns):(5+ns+ns+ns+ns-1))])) # limit of NBS for each catchment
        inf_cap = as.numeric(inputcatch$`Mm3/m2/month`)  # infiltration capacity of each basin
        ###########################################################
        tree_data = as.data.frame(inputNetwork)[c(1,2,3)]
        colnames(tree_data) <- c('from', 'to', 'w')
        tree1 <- graph_from_data_frame(tree_data)
        sort1 = topo_sort(tree1,mode = "out")
        nodeID = input$node1
        Qobs = as.data.frame(inputmeasured[,(nodeID+1)])
        ######################
        qsim = run_model(param=res,
                         nodeID=nodeID,
                         inputrain=inputrain,
                         inputNoder=inputNoder,
                         inputNetwork=inputNetwork,
                         inputinflow=inputinflow,
                         sort1=sort1)
        time_step = c(1:nrow(inputrain))
        dataplot <- data.frame(time_step,Qobs,qsim)
        kge = hydroGOF::KGE(as.matrix(qsim), Qobs)
        title1 = paste0("Node no",nodeID,", KGE = ", round(kge, 2))
        colnames(dataplot) <- c("time_step","Qobs","qsim")
        fig <- plot_ly(dataplot, type = 'scatter', mode = 'lines')%>%
          add_trace(x = ~time_step, y = ~Qobs, name = 'Observed flow')%>%
          add_trace(x = ~time_step, y = ~qsim, name = 'Simulated flow')%>%
          layout(title = title1,xaxis = list(title = 'Time step (month)'), 
                 yaxis = list(title = 'Outflow (Mm3/month)'))
        fig
      }
    }
  })
  calib_res <- eventReactive(input$act_calib, {
    inFile <- input$file_calib
    if(length(inFile)>0){
      ExcelInOrig <- inFile$datapath
      inputNoder= read_excel(ExcelInOrig, sheet='Nodes',skip=1)
      inputNetwork= read_excel(ExcelInOrig, sheet='Network',skip=1)
      inputrain= read_excel(ExcelInOrig, sheet="Rainfall")
      inputcatch= read_excel(ExcelInOrig, sheet='catchments nodes',skip=1)
      inputmeasures= read_excel(ExcelInOrig, sheet='Measures',skip=1)
      inputinflow= read_excel(ExcelInOrig, sheet='Inflow')
      inputmeasured= read_excel(ExcelInOrig, sheet='Measured flow')
      ###########################################################
      nc = nrow(inputcatch) # number of catchments 
      ns = nrow(inputmeasures) # number of solutions
      snames = inputmeasures$Measure # names of solutions
      cnames = inputcatch$`catchment names` # names of catchments
      node_catch =inputcatch$`catchment outlet node`
      cost1 = inputmeasures$Cost # cost per m2 of NBS, DAM, pump
      rate1 = inputmeasures$`Mm3/m2/month` # rate of recharge per month per m2
      outlet1 = as.numeric(inputcatch$node[1]) # outlet of the whole system
      kt = as.data.frame(t(inputcatch[,c(5:(5+ns-1))])) # transportation factor
      ke = as.data.frame(t(inputcatch[,c((5+ns):(5+ns+ns-1))])) # energy factor
      kl = as.data.frame(t(inputcatch[,c((5+ns+ns):(5+ns+ns+ns-1))])) # local condition factor 
      limit1 =  as.data.frame(t(inputcatch[,c((5+ns+ns+ns):(5+ns+ns+ns+ns-1))])) # limit of NBS for each catchment
      inf_cap = as.numeric(inputcatch$`Mm3/m2/month`)  # infiltration capacity of each basin
      ###########################################################
      tree_data = as.data.frame(inputNetwork)[c(1,2,3)]
      colnames(tree_data) <- c('from', 'to', 'w')
      tree1 <- graph_from_data_frame(tree_data)
      sort1 = topo_sort(tree1,mode = "out")
      nodeID = input$node1
      Qobs = as.data.frame(inputmeasured[,(nodeID+1)])
      ######################
      calibration_res2 <- DEoptim(
        fn = obj_fun_calib, 
        lower = rep(0,nrow(inputNoder)),
        upper = rep(1,nrow(inputNoder)),
        DEoptim.control(trace=TRUE,parallelType=0,
                        packages=c(),
                        parVar=c(),
                        itermax = input$num_calib),
        Qobs = Qobs,
        nodeID=nodeID,
        inputrain=inputrain,
        inputNoder=inputNoder,
        inputNetwork=inputNetwork,
        inputinflow=inputinflow,
        sort1 = sort1
      )
      
      calibration_res2$optim$bestmem
    }
  })
  output$plot_current <- renderPlotly({
    inFile <- input$file_calib
    if(length(inFile)>0){
      currresplot()
    }
  })
  output$downloadcalib <- downloadHandler(
    
    filename = function() {
      paste("calibParam.csv", sep = "")
    },
    content = function(file) {
      
      res = calib_res()
      write.csv(res,file,sep = ",",col.names = F,row.names = F)
      
    }
  )
  output$plot_calib <- renderPlotly({
    inFile <- input$file_calib
    if(length(inFile)>0){
      calibplot()
    }
  })
  ##############
  
  initial_volume = reactive({
    inFile <- input$file1
    if(length(inFile)>0){
      ExcelInOrig = inFile$datapath
      ###########################################################
      inputNoder= read_excel(ExcelInOrig, sheet='Nodes',skip=1)
      inputNetwork= read_excel(ExcelInOrig, sheet='Network',skip=1)
      inputrain= read_excel(ExcelInOrig, sheet="Rainfall")
      inputcatch= read_excel(ExcelInOrig, sheet='catchments nodes',skip=1)
      inputmeasures= read_excel(ExcelInOrig, sheet='Measures',skip=1)
      inputinflow= read_excel(ExcelInOrig, sheet='Inflow')
      ###########################################################
      nc = nrow(inputcatch) # number of catchments 
      ns = nrow(inputmeasures) # number of solutions
      snames = inputmeasures$Measure # names of solutions
      cnames = inputcatch$`catchment names` # names of catchments
      node_catch =inputcatch$`catchment outlet node`
      cost1 = inputmeasures$Cost # cost per m2 of NBS, DAM, pump
      rate1 = inputmeasures$`Mm3/m2/month` # rate of recharge per month per m2
      outlet1 = as.numeric(inputcatch$node[1]) # outlet of the whole system
      kt = as.data.frame(t(inputcatch[,c(5:(5+ns-1))])) # transportation factor
      ke = as.data.frame(t(inputcatch[,c((5+ns):(5+ns+ns-1))])) # energy factor
      kl = as.data.frame(t(inputcatch[,c((5+ns+ns):(5+ns+ns+ns-1))])) # local condition factor 
      limit1 =  as.data.frame(t(inputcatch[,c((5+ns+ns+ns):(5+ns+ns+ns+ns-1))])) # limit of NBS for eavch catchment
      inf_cap = as.numeric(inputcatch$`Mm3/m2/month`)  # infiltration capacity of each basin
      ###########################################################
      tree_data = as.data.frame(inputNetwork)[c(1,2,3)]
      colnames(tree_data) <- c('from', 'to', 'w')
      tree1 <- graph_from_data_frame(tree_data)
      ###########################################################
      sort1 = topo_sort(tree1,mode = "out")
      np = ns*nc
      lb <- rep(0,np)
      optim_fun = function(param){
        
        ############
        # nc = 4 # number of catchments 
        # ns = 3 # number of solutions
        # node_catch = c(2,9,12,15)
        # cost1 = c(10,15,30) # cost per m2 of NBS, DAM, pump
        # rate1 = c(0.01,0.014,0.04) # rate of recharge per month per m2
        # kt = c(2.12,2,1.41,1)
        # ke = c(1,1.5,3.5)
        # kl = matrix(1,nrow = ns,ncol = nc)
        k = 1
        Ctotal = 0
        Marc = array()
        for(c in 1:nc){
          Mar1 = 0
          for(s in 1:ns){
            ma1 = param[k] * min(rate1[s],inf_cap[c])
            Ctotal = Ctotal + (param[k] * cost1[s]*kt[s,c]*ke[s,c]*kl[s,c]) 
            Mar1 = Mar1 + ma1
            k = k+1
          }
          Marc[c] = Mar1
        }
        dat_marc = data.frame(node = node_catch,
                              MAR = Marc)
        ############
        
        Rainfall = as.matrix(inputrain[,-1])
        T1 = nrow(Rainfall)
        N1 = nrow(inputNoder)
        
        rain_mat = matrix(0,ncol =N1,nrow = T1)
        
        
        for(n in 1:(N1)){
          ind_rain = which(colnames(Rainfall)==inputNoder$Rainfall[n])
          rain1 = as.numeric(Rainfall[,ind_rain])*(inputNoder$MAP[n]/100000)*
            (inputNoder$`Area (km2)`[n])
          rain_mat[,n] = as.array(rain1*(1-inputNoder$Retention[n]))
        }
        
        ######
        
        
        #Out_mat = matrix(0,ncol = (N1),nrow = T1)
        
        Out_mat = rain_mat
        
        inflow_mat = as.matrix(inputinflow[,-1])
        
        N2 = nrow(inputNetwork)
        
        for(n in sort1){
          
          if(n %in% node_catch){
            MR = dat_marc[which(dat_marc$node==n),2] 
          }else{
            MR = 0 
          }
          
          attr1 = which(inputNetwork$`To node`==n)
          k = 1
          if(length(attr1)>0){
            share2 = inputNetwork$Share[attr1]
            node2 = inputNetwork$`From node`[attr1]
            # rain1 = as.matrix(rain_mat2[,attr1])
            for(j in 1:length(attr1)){
              share1 = share2[j]
              
              if(j ==1){
                sum1 = share1*Out_mat[,node2[j]]
              }else{
                sum1 = sum1+(share1*Out_mat[,node2[j]])
              }
            }
            Out_mat[,n] = Out_mat[,n] + ((sum1 + inflow_mat[,n])*(1-inputNoder$Retention[n]))- MR
          }
        }
        
        ################
        
        
        y = array()
        
        y[1] = sum(Out_mat[,outlet1]) # Total volume at the outlet
        y[2] = Ctotal
        return(y)
        
      }
      
      optim_fun(lb)[1]
      
    }
  })
  
  res = eventReactive(input$act1, {
    #############
    inFile <- input$file1
    if(length(inFile)>0){
      ExcelInOrig <- inFile$datapath
      
      ###########################################################
      inputNoder= read_excel(ExcelInOrig, sheet='Nodes',skip=1)
      inputNetwork= read_excel(ExcelInOrig, sheet='Network',skip=1)
      inputrain= read_excel(ExcelInOrig, sheet="Rainfall")
      inputcatch= read_excel(ExcelInOrig, sheet='catchments nodes',skip=1)
      inputmeasures= read_excel(ExcelInOrig, sheet='Measures',skip=1)
      inputinflow= read_excel(ExcelInOrig, sheet='Inflow')
      ###########################################################
      nc = nrow(inputcatch) # number of catchments 
      ns = nrow(inputmeasures) # number of solutions
      snames = inputmeasures$Measure # names of solutions
      cnames = inputcatch$`catchment names` # names of catchments
      node_catch =inputcatch$`catchment outlet node`
      cost1 = inputmeasures$Cost # cost per m2 of NBS, DAM, pump
      rate1 = inputmeasures$`Mm3/m2/month` # rate of recharge per month per m2
      outlet1 = as.numeric(inputcatch$node[1]) # outlet of the whole system
      kt = as.data.frame(t(inputcatch[,c(5:(5+ns-1))])) # transportation factor
      ke = as.data.frame(t(inputcatch[,c((5+ns):(5+ns+ns-1))])) # energy factor
      kl = as.data.frame(t(inputcatch[,c((5+ns+ns):(5+ns+ns+ns-1))])) # local condition factor 
      limit1 =  as.data.frame(t(inputcatch[,c((5+ns+ns+ns):(5+ns+ns+ns+ns-1))])) # limit of NBS for eavch catchment
      inf_cap = as.numeric(inputcatch$`Mm3/m2/month`)  # infiltration capacity of each basin
      ###########################################################
      tree_data = as.data.frame(inputNetwork)[c(1,2,3)]
      colnames(tree_data) <- c('from', 'to', 'w')
      tree1 <- graph_from_data_frame(tree_data)
      ###########################################################
      sort1 = topo_sort(tree1,mode = "out")
      optim_fun = function(param){
        
        ############
        # nc = 4 # number of catchments 
        # ns = 3 # number of solutions
        # node_catch = c(2,9,12,15)
        # cost1 = c(10,15,30) # cost per m2 of NBS, DAM, pump
        # rate1 = c(0.01,0.014,0.04) # rate of recharge per month per m2
        # kt = c(2.12,2,1.41,1)
        # ke = c(1,1.5,3.5)
        # kl = matrix(1,nrow = ns,ncol = nc)
        k = 1
        Ctotal = 0
        Marc = array()
        for(c in 1:nc){
          Mar1 = 0
          for(s in 1:ns){
            ma1 = param[k] * min(rate1[s],inf_cap[c])
            Ctotal = Ctotal + (param[k] * cost1[s]*kt[s,c]*ke[s,c]*kl[s,c]) 
            Mar1 = Mar1 + ma1
            k = k+1
          }
          Marc[c] = Mar1
        }
        dat_marc = data.frame(node = node_catch,
                              MAR = Marc)
        ############
        
        Rainfall = as.matrix(inputrain[,-1])
        T1 = nrow(Rainfall)
        N1 = nrow(inputNoder)
        
        rain_mat = matrix(0,ncol =N1,nrow = T1)
        
        
        for(n in 1:(N1)){
          ind_rain = which(colnames(Rainfall)==inputNoder$Rainfall[n])
          rain1 = as.numeric(Rainfall[,ind_rain])*(inputNoder$MAP[n]/100000)*
            (inputNoder$`Area (km2)`[n])
          rain_mat[,n] = as.array(rain1*(1-inputNoder$Retention[n]))
        }
        
        ######
        
        
        #Out_mat = matrix(0,ncol = (N1),nrow = T1)
        
        Out_mat = rain_mat
        
        inflow_mat = as.matrix(inputinflow[,-1])
        
        N2 = nrow(inputNetwork)
        
        for(n in sort1){
          
          if(n %in% node_catch){
            MR = dat_marc[which(dat_marc$node==n),2] 
          }else{
            MR = 0 
          }
          
          attr1 = which(inputNetwork$`To node`==n)
          k = 1
          if(length(attr1)>0){
            share2 = inputNetwork$Share[attr1]
            node2 = inputNetwork$`From node`[attr1]
            # rain1 = as.matrix(rain_mat2[,attr1])
            for(j in 1:length(attr1)){
              share1 = share2[j]
              
              if(j ==1){
                sum1 = share1*Out_mat[,node2[j]]
              }else{
                sum1 = sum1+(share1*Out_mat[,node2[j]])
              }
            }
            Out_mat[,n] = Out_mat[,n] + ((sum1 + inflow_mat[,n])*(1-inputNoder$Retention[n]))- MR
          }
        }
        
        ################
        
        
        y = array()
        
        y[1] = sum(Out_mat[,outlet1]) # Total volume at the outlet
        y[2] = Ctotal
        return(y)
        
      }
      constraint1 <- function(param) {
        ############
        # nc = 4 # number of catchments 
        # ns = 3 # number of solutions
        # node_catch = c(2,9,12,15)
        # cost1 = c(10,15,30) # cost per m2 of NBS, DAM, pump
        # rate1 = c(0.01,0.014,0.04) # rate of recharge per month per m2
        # kt = c(2.12,2,1.41,1)
        # ke = c(1,1.5,3.5)
        # kl = matrix(1,nrow = ns,ncol = nc)
        k = 1
        Ctotal = 0
        Marc = array()
        for(c in 1:nc){
          Mar1 = 0
          for(s in 1:ns){
            ma1 = param[k] * min(rate1[s],inf_cap[c])
            Ctotal = Ctotal + (param[k] * cost1[s]*kt[c]*ke[s]*kl[s,c]) 
            Mar1 = Mar1 + ma1
            k = k+1
          }
          Marc[c] = Mar1
        }
        dat_marc = data.frame(node = node_catch,
                              MAR = Marc)
        ############
        Rainfall = as.matrix(inputrain[,-1])
        T1 = nrow(Rainfall)
        N1 = nrow(inputNoder)
        
        
        rain_mat = matrix(0,ncol =(N1),nrow = T1)
        
        for(n in 1:(N1)){
          ind_rain = which(colnames(Rainfall)==inputNoder$Rainfall[n])
          rain1 = as.numeric(Rainfall[,ind_rain])*(inputNoder$MAP[n]/100000)*
            (inputNoder$`Area (km2)`[n])
          rain_mat[,n] = as.array(rain1*(1-inputNoder$Retention[n]))
        }
        
        ######
        
        
        #Out_mat = matrix(0,ncol = (N1),nrow = T1)
        
        Out_mat = rain_mat
        
        inflow_mat = as.matrix(inputinflow[,-1])
        
        
        for(n in sort1){
          
          if(n %in% node_catch){
            MR = dat_marc[which(dat_marc$node==n),2] 
          }else{
            MR = 0 
          }
          
          attr1 = which(inputNetwork$`To node`==n)
          k = 1
          if(length(attr1)>0){
            share2 = inputNetwork$Share[attr1]
            node2 = inputNetwork$`From node`[attr1]
            # rain1 = as.matrix(rain_mat2[,attr1])
            for(j in 1:length(attr1)){
              share1 = share2[j]
              
              if(j ==1){
                sum1 = share1*Out_mat[,node2[j]]
              }else{
                sum1 = sum1+(share1*Out_mat[,node2[j]])
              }
            }
            Out_mat[,n] = Out_mat[,n] + ((sum1 + inflow_mat[,n])*(1-inputNoder$Retention[n]))- MR
          }
        }
        
        
        
        g_array =  apply(Out_mat[,node_catch],2,sum)
        return(as.array(g_array))
      }
      np = ns*nc
      lb <- rep(0,np)
      ub <-as.numeric(as.vector(as.matrix((limit1))))
      ncon <- length(node_catch)
      nsga2(optim_fun, np, 2,
            generations=input$num1,
            lower.bounds=lb, 
            upper.bounds=ub,
            constraints=constraint1,
            cdim=ncon)
      
    }
  })
  
  
  
  output$MultiOpt <- renderText({ 
    inFile <- input$file1
    if(length(inFile)>0){
      res = res()
      if(length(res)>0){
        print("Optimization is finished. Please export the results")
      }
    }
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("MultiOpt.json", sep = "")
    },
    content = function(file) {
      
      res = res()
      inFile <- input$file1
      ExcelInOrig <- inFile$datapath
      
      ###########################################################
      inputsetup= read_excel(ExcelInOrig, sheet='Setup',skip=2)
      inputNoder= read_excel(ExcelInOrig, sheet='Nodes',skip=1)
      inputNetwork= read_excel(ExcelInOrig, sheet='Network',skip=1)
      inputrain= read_excel(ExcelInOrig, sheet="Rainfall")
      inputcatch= read_excel(ExcelInOrig, sheet='catchments nodes',skip=1)
      inputmeasures= read_excel(ExcelInOrig, sheet='Measures',skip=1)
      inputinflow= read_excel(ExcelInOrig, sheet='Inflow')
      ###########################################################
      curr = as.character(inputsetup[which(inputsetup[,1]=="Define currency"),2])
      Rainfall = as.matrix(inputrain[,-1])
      ny = nrow(Rainfall)/12 
      res[[2]][,1] = res[[2]][,1]/ny
      ###########################################################
      nc = nrow(inputcatch) # number of catchments 
      ns = nrow(inputmeasures) # number of solutions
      snames = inputmeasures$Measure # names of solutions
      cnames = inputcatch$`catchment names` # names of catchments
      node_catch =inputcatch$`catchment outlet node`
      cost1 = inputmeasures$Cost # cost per m2 of NBS, DAM, pump
      rate1 = inputmeasures$`Mm3/m2/month` # rate of recharge per month per m2
      outlet1 = as.numeric(inputcatch$node[1]) # outlet of the whole system
      kt = as.data.frame(t(inputcatch[,c(5:(5+ns-1))])) # transportation factor
      ke = as.data.frame(t(inputcatch[,c((5+ns):(5+ns+ns-1))])) # energy factor
      kl = as.data.frame(t(inputcatch[,c((5+ns+ns):(5+ns+ns+ns-1))])) # local condition factor 
      limit1 =  as.data.frame(t(inputcatch[,c((5+ns+ns+ns):(5+ns+ns+ns+ns-1))])) # limit of NBS for eavch catchment
      inf_cap = as.numeric(inputcatch$`Mm3/m2/month`)  # infiltration capacity of each basin
      ###########################################################
      tree_data = as.data.frame(inputNetwork)[c(1,2,3)]
      colnames(tree_data) <- c('from', 'to', 'w')
      tree1 <- graph_from_data_frame(tree_data)
      ###########################################################
      np = ns*nc
      lb <- rep(0,np)
      initial_volume = initial_volume()/ny
      MAR_volume = initial_volume-res[[2]][,1]
      list1 = list(snames = snames,
                   cnames = cnames,
                   currency = curr,
                   parameters = res[[1]],
                   objectives = res[[2]],
                   MAR = MAR_volume,
                   pareto = res[[3]])
      list.save(list1, file)
      
    }
  )
  
  #############
  
  output$pareto_front <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      #ind1 = which(res3$pareto=="FALSE")[1] -1 
      x = c(1:length(res3$pareto)) 
      
      ind_order = order(res3$objectives[,1])
      res3$objectives = res3$objectives[ind_order,]
      
      pareto1 = cbind.data.frame(x,res3$objectives,res3$MAR)
      colnames(pareto1) = c("Index","Total outflow (Mm3/year)","Cost","MAR volume (Mm3/year)")
      
      plot_ly(data = pareto1, x = ~`Cost`, y = ~`Total outflow (Mm3/year)`, text = ~Index,
              marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',
                            line = list(color = 'rgba(152, 0, 0, .8)',
                                        width = 2)))
      
    }
    
  })
  
  output$Rainfall_Runoff_pareto <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      #ind1 = which(res3$pareto=="FALSE")[1] -1 
      cnames = res3$cnames
      snames = res3$snames
      nc = length(cnames)
      ns = length(snames)
      dat = data.frame()
      
      ind_order = order(res3$objectives[,1])
      res3$objectives = res3$objectives[ind_order,]
      res3$parameters = res3$parameters[ind_order,]
      ind1 = input$num6
    
      areas = res3$parameters[ind1,]
      dat = data.frame()
      
      k = 1
      for(c in 1:nc){
        for(s in 1:ns){
          dat[k,1] = cnames[c]
          dat[k,2] = snames[s]
          dat[k,3] =areas[k]
          k = k+1
        }
      }
      
      # Grouped
      
      # Grouped
      np = ns*nc
      lb <- rep(0,np)
      initial_volume = initial_volume()
      mar_vol = round(initial_volume - res3$objectives[ind1,1],2)
      mar_cost = round(res3$objectives[ind1,2],2)
      title1 = paste0("Total MAR volume = ",mar_vol, " Mm3. Total cost = ",mar_cost," USD")
      colnames(dat) = c("Catchment","MAR method","Area")
      
      p = ggplot(dat, aes(fill=`MAR method`, y=Area, x=Catchment)) + 
        geom_bar(position="dodge", stat="identity")+
        scale_fill_viridis_d() +
        ggtitle(title1)+
        xlab("Catchment")+
        ylab("Area (m2)")+
        theme_bw()
      
      ggplotly(p)
      
    }
    
  })
  
  ############
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- numeric(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }
  
  # 1.4.2 obtain the values of inputs
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value <- input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }
  
  compare_df <- reactive({
    
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      data.frame(
        Solutions =paste0("Solution ",c(1:5)),
        Number = shinyInput(selectInput,5,
                            'select_pareto', choices = c(1:100), width = "100px"),
        stringsAsFactors = FALSE
      )
    }
    
  })
  
  # 1.4.4 final output tags table 
  
  output$compare_pareto_input <- DT::renderDataTable(
    compare_df(), rownames = FALSE, escape = FALSE, options = list(
      autoWidth = TRUE, scrollX = TRUE, #scrollY = '400px',
      dom = 't', ordering = FALSE,
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
  )
  
  output$pareto_plot1 <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
        dat2 <- data.frame(
          Solutions = paste0("Solution",c(1:5)),
          Number = shinyValue('select_pareto', 5)
        )
       
        j = as.numeric(dat2$Number[1])
       
        cnames = res3$cnames
        snames = res3$snames
        nc = length(cnames)
        ns = length(snames)
        dat = data.frame()
        
        ind_order = order(res3$objectives[,1])
        res3$objectives = res3$objectives[ind_order,]
        res3$parameters = res3$parameters[ind_order,]
        ind1 = j
        
        areas = res3$parameters[ind1,]
        dat = data.frame()
        
        k = 1
        for(c in 1:nc){
          for(s in 1:ns){
            dat[k,1] = cnames[c]
            dat[k,2] = snames[s]
            dat[k,3] =areas[k]
            k = k+1
          }
        }
        
        # Grouped
        
        # Grouped
        np = ns*nc
        lb <- rep(0,np)
        initial_volume = initial_volume()
        mar_vol = round(initial_volume - res3$objectives[ind1,1],2)
        mar_cost = round(res3$objectives[ind1,2],2)
        title1 = paste0("Total MAR volume = ",mar_vol, " Mm3. Total cost = ",mar_cost," USD")
        colnames(dat) = c("Catchment","MAR method","Area")
        
        p = ggplot(dat, aes(fill=`MAR method`, y=Area, x=Catchment)) + 
          geom_bar(position="dodge", stat="identity")+
          scale_fill_viridis_d() +
          ggtitle(title1)+
          xlab("Catchment")+
          ylab("Area (m2)")+
          theme_bw()
        
        ggplotly(p)
      
    }
  }) 
  
  output$pareto_plot2 <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      
      
      dat2 <- data.frame(
        Solutions = paste0("Solution",c(1:5)),
        Number = shinyValue('select_pareto', 5)
      )
      
      j = as.numeric(dat2$Number[2])
      
      cnames = res3$cnames
      snames = res3$snames
      nc = length(cnames)
      ns = length(snames)
      dat = data.frame()
      
      ind_order = order(res3$objectives[,1])
      res3$objectives = res3$objectives[ind_order,]
      res3$parameters = res3$parameters[ind_order,]
      ind1 = j
      print(ind1)
      areas = res3$parameters[ind1,]
      dat = data.frame()
      
      k = 1
      for(c in 1:nc){
        for(s in 1:ns){
          dat[k,1] = cnames[c]
          dat[k,2] = snames[s]
          dat[k,3] =areas[k]
          k = k+1
        }
      }
      
      # Grouped
      
      # Grouped
      np = ns*nc
      lb <- rep(0,np)
      initial_volume = initial_volume()
      mar_vol = round(initial_volume - res3$objectives[ind1,1],2)
      mar_cost = round(res3$objectives[ind1,2],2)
      title1 = paste0("Total MAR volume = ",mar_vol, " Mm3. Total cost = ",mar_cost," USD")
      colnames(dat) = c("Catchment","MAR method","Area")
      
      p = ggplot(dat, aes(fill=`MAR method`, y=Area, x=Catchment)) + 
        geom_bar(position="dodge", stat="identity")+
        scale_fill_viridis_d() +
        ggtitle(title1)+
        xlab("Catchment")+
        ylab("Area (m2)")+
        theme_bw()
      
      ggplotly(p)
      
    }
  }) 
  
  output$pareto_plot3 <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      
      
      dat2 <- data.frame(
        Solutions = paste0("Solution",c(1:5)),
        Number = shinyValue('select_pareto', 5)
      )
      
      j = as.numeric(dat2$Number[3])
      
      cnames = res3$cnames
      snames = res3$snames
      nc = length(cnames)
      ns = length(snames)
      dat = data.frame()
      
      ind_order = order(res3$objectives[,1])
      res3$objectives = res3$objectives[ind_order,]
      res3$parameters = res3$parameters[ind_order,]
      ind1 = j
      
      areas = res3$parameters[ind1,]
      dat = data.frame()
      
      k = 1
      for(c in 1:nc){
        for(s in 1:ns){
          dat[k,1] = cnames[c]
          dat[k,2] = snames[s]
          dat[k,3] =areas[k]
          k = k+1
        }
      }
      
      # Grouped
      
      # Grouped
      np = ns*nc
      lb <- rep(0,np)
      initial_volume = initial_volume()
      mar_vol = round(initial_volume - res3$objectives[ind1,1],2)
      mar_cost = round(res3$objectives[ind1,2],2)
      title1 = paste0("Total MAR volume = ",mar_vol, " Mm3. Total cost = ",mar_cost," USD")
      colnames(dat) = c("Catchment","MAR method","Area")
      
      p = ggplot(dat, aes(fill=`MAR method`, y=Area, x=Catchment)) + 
        geom_bar(position="dodge", stat="identity")+
        scale_fill_viridis_d() +
        ggtitle(title1)+
        xlab("Catchment")+
        ylab("Area (m2)")+
        theme_bw()
      
      ggplotly(p)
      
    }
  }) 
  
  output$pareto_plot4 <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      
      
      dat2 <- data.frame(
        Solutions = paste0("Solution",c(1:5)),
        Number = shinyValue('select_pareto', 5)
      )
      
      j = as.numeric(dat2$Number[4])
      
      cnames = res3$cnames
      snames = res3$snames
      nc = length(cnames)
      ns = length(snames)
      dat = data.frame()
      
      ind_order = order(res3$objectives[,1])
      res3$objectives = res3$objectives[ind_order,]
      res3$parameters = res3$parameters[ind_order,]
      ind1 = j
      
      areas = res3$parameters[ind1,]
      dat = data.frame()
      
      k = 1
      for(c in 1:nc){
        for(s in 1:ns){
          dat[k,1] = cnames[c]
          dat[k,2] = snames[s]
          dat[k,3] =areas[k]
          k = k+1
        }
      }
      
      # Grouped
      
      # Grouped
      np = ns*nc
      lb <- rep(0,np)
      initial_volume = initial_volume()
      mar_vol = round(initial_volume - res3$objectives[ind1,1],2)
      mar_cost = round(res3$objectives[ind1,2],2)
      title1 = paste0("Total MAR volume = ",mar_vol, " Mm3. Total cost = ",mar_cost," USD")
      colnames(dat) = c("Catchment","MAR method","Area")
      
      p = ggplot(dat, aes(fill=`MAR method`, y=Area, x=Catchment)) + 
        geom_bar(position="dodge", stat="identity")+
        scale_fill_viridis_d() +
        ggtitle(title1)+
        xlab("Catchment")+
        ylab("Area (m2)")+
        theme_bw()
      
      ggplotly(p)
      
    }
  }) 
  
  output$pareto_plot5 <- renderPlotly({
    inFile <- input$file2
    res3 <- list.load(inFile$datapath)
    if(length(res3)>0){
      
      
      dat2 <- data.frame(
        Solutions = paste0("Solution",c(1:5)),
        Number = shinyValue('select_pareto', 5)
      )
      
      j = as.numeric(dat2$Number[5])
      
      cnames = res3$cnames
      snames = res3$snames
      nc = length(cnames)
      ns = length(snames)
      dat = data.frame()
      
      ind_order = order(res3$objectives[,1])
      res3$objectives = res3$objectives[ind_order,]
      res3$parameters = res3$parameters[ind_order,]
      ind1 = j
      
      areas = res3$parameters[ind1,]
      dat = data.frame()
      
      k = 1
      for(c in 1:nc){
        for(s in 1:ns){
          dat[k,1] = cnames[c]
          dat[k,2] = snames[s]
          dat[k,3] =areas[k]
          k = k+1
        }
      }
      
      # Grouped
      
      # Grouped
      np = ns*nc
      lb <- rep(0,np)
      initial_volume = initial_volume()
      mar_vol = round(initial_volume - res3$objectives[ind1,1],2)
      mar_cost = round(res3$objectives[ind1,2],2)
      title1 = paste0("Total MAR volume = ",mar_vol, " Mm3. Total cost = ",mar_cost," USD")
      colnames(dat) = c("Catchment","MAR method","Area")
      
      p = ggplot(dat, aes(fill=`MAR method`, y=Area, x=Catchment)) + 
        geom_bar(position="dodge", stat="identity")+
        scale_fill_viridis_d() +
        ggtitle(title1)+
        xlab("Catchment")+
        ylab("Area (m2)")+
        theme_bw()
      
      ggplotly(p)
      
    }
  })
  
}

shinyApp(ui, server)

