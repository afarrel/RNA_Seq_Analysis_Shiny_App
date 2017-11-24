#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(pheatmap)

if (interactive()) {
Data.folder = "/Users/farrelal/Documents/Shiny_apps/Test1/RNAseq_vSV/Data"
Data.folder.files = list.files(Data.folder)
Data_File_path = ""
Group_Count=0
Reads.Data.folder = "/Users/farrelal/Documents/Shiny_apps/Test1/New_RNAseq_webapp/Data"
Reads.Data.folder.files = list.files(Reads.Data.folder)
Reads.Data_File_path = ""

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("RNAseq Explorer.SV"),
   
   # Sidebar with a slider input for number of bins 
 
        navbarPage("My Application",
        #################################
#########################################        
#########################################        
        tabPanel("EdgeR\nComparison",
                 sidebarLayout(
                   sidebarPanel(
                     fileInput("file1", "Choose Reads File",
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                     ),
                     radioButtons(inputId = "Reads.Select_input",label = "Select Input Source",choices = c("Database"="dbase","Upload File"="upload"),selected = "dbase"),
                     selectInput(inputId="Reads.Data_User", label="Select User", c(Reads.Data.folder.files), selected = NULL, multiple = F),
                     selectInput(inputId="Reads.Data_set", label="Select Dataset", c(""), selected = NULL, multiple = F),
                     actionButton(inputId="Reads.Load_Data", label="Load Data"),
                     textOutput("Reads.User_Data_File"),
                     textOutput("Reads.User_Data_Info"),
                     #tags$hr(),
                     #checkboxInput("header", "Header", TRUE),
                     selectInput(inputId="Comparison", label="Select Library", c("No Libraries Loaded"), multiple = T),
                     actionButton(inputId="CompareButton", label="Compare Libraries"),
                     textInput(inputId="Log2FCThreshold", label="Log2FC", value = "1", width = '60px', placeholder = NULL),
                     textInput(inputId="FDR_PvalThreshold", label="P-value", value = "0.05", width = '60px', placeholder = NULL),
                     selectInput(inputId="Pval_Correction", label="P.val adj", c("none","BH", "fdr","BY","holm"), selected = "fdr", multiple = F)
                   ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Summary_plots", 
                                plotOutput("MDSPlot"),
                                plotOutput("ComparePlot1"),
                                plotOutput("ComparePlot2"),
                                plotOutput("ComparePlot3"),
                                plotOutput("ComparePlot4"),
                                plotOutput("ComparePlot5"),
                                plotOutput("ComparePlot6"),
                                plotOutput("ComparePlot7"),
                                plotOutput("ComparePlot8")),
                       tabPanel("GeneLists",textAreaInput(inputId="GeneList_Result", label = "Gene List", value = NULL,height = '400px')
                       ),
                       tabPanel("GeneTable",textAreaInput(inputId="Gene_Table", label = "Gene Table", value = NULL,height = '400px',width = '800px'),
                                downloadButton("downloadData", "Download CSV"),
                                textAreaInput(inputId="Reads_Table", label = "Reads Table", value = NULL,height = '400px',width = '800px')
                       )
                     )
                     
                     #tableOutput("contents")
                   )
                 )
                 
        ),
##########################################
##########################################
##########################################

tabPanel("EdgeR Group\nComparison",
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId="Groups.Control", label="Select Control", c(""), selected = NULL, multiple = F),
             selectInput(inputId="Groups.Treatment", label="Select Treatment", c(""), selected = NULL, multiple = F),
              #selectInput(inputId="Comparison", label="Select Library", c("No Libraries Loaded"), multiple = T),
             actionButton(inputId="Groups.CompareButton", label="Compare Groups"),
             textInput(inputId="Groups.Log2FCThreshold", label="Log2FC", value = "1", width = '60px', placeholder = NULL),
             textInput(inputId="Groups.FDR_PvalThreshold", label="P-value", value = "0.05", width = '60px', placeholder = NULL),
             selectInput(inputId="Groups.Pval_Correction", label="P.val adj", c("none","BH", "fdr","BY","holm"), selected = "fdr", multiple = F)
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("Summary_plots", 
                        plotOutput("Groups.ComparePlot1"),
                        plotOutput("Groups.ComparePlot2"),
                        plotOutput("Groups.ComparePlot3"),
                        plotOutput("Groups.ComparePlot4"),
                        plotOutput("Groups.ComparePlot5"),
                        plotOutput("Groups.ComparePlot6"),
                        plotOutput("Groups.ComparePlot7"),
                        plotOutput("Groups.ComparePlot8")),
               tabPanel("Groups.GeneLists",textAreaInput(inputId="Groups.GeneList_Result", label = "Gene List", value = NULL,height = '400px')
               )
             )
             
             #tableOutput("contents")
           )
         )
         
),        


##########################################
##########################################
##########################################
        tabPanel("RPKM Data",
                 sidebarLayout(
                   sidebarPanel(
                     
                     fileInput("file_RPKM", "Choose RPKM File",
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                     ),
                     radioButtons(inputId = "Select_input",label = "Select Input Source",choices = c("Database"="dbase","Upload File"="upload"),selected = "dbase"),
                     selectInput(inputId="Data_User", label="Select User", c(Data.folder.files), selected = NULL, multiple = F),
                     selectInput(inputId="Data_set", label="Select Dataset", c(""), selected = NULL, multiple = F),
                     actionButton(inputId = "Load_File",label = "Load File")
                     
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     plotOutput("distPlot"),
                     tableOutput("User_Data_File")
                   )
                 )
        ),
        tabPanel("Gene Comparison (Samples)",
                  fluidRow(
                     column(width=4,selectInput(inputId="Single.Compare.GeneList",label="Select Genes",c(""),selected=NULL,multiple=T)),
                     column(width=4,selectInput(inputId="Single.Compare.Conditions", label="Condition/Library", c(""), selected = NULL, multiple = T))
                  ),
                  actionButton(inputId = "Compare.Sample.Exp",label = "Compare Gene Expressions"),
                  textOutput("Single.Sample.barplot.label"),
                  plotOutput("Single.Sample.barplot"),
                  textOutput("Single.Sample.heatmap.label"),
                  plotOutput("Single.Sample.heatmap"),
                  textOutput("Stacked_Plot.label"),
                  plotOutput("Stacked_Plot"),
                  textOutput("MDS_PLOT.Samples.label"),
                  plotOutput("MDS_PLOT.Samples")
                 
                 ),
        tabPanel("Gene Comparison (Groups)",
                 fluidRow(
                    column(width=4,selectInput(inputId="Group.Compare.GeneList",label="Select Genes",c(""),selected=NULL,multiple=T)),
                    column(width=4,selectInput(inputId="Group.Compare.Condition", label="Select Groups", c(""), selected = "NULL", multiple = T))
                  ),
                  actionButton(inputId = "Compare.Group.Exp",label = "Compare Groups' Expressions"),
                  textOutput("Group.boxplot.label"),
                  plotOutput("Group.boxplot",height = "800px"),
                  textOutput("Group.barplot.label"),
                  plotOutput("Group.barplot"),
                  textOutput("Group.barplot.sem.label"),
                  plotOutput("Group.barplot.sem"),
                  textOutput("Group.barplot.maxmin.label"),
                  plotOutput("Group.barplot.maxmin"),
                  textOutput("Group.heatmap.label"),
                  plotOutput("Group.heatmap"),
                  textOutput("Group.members.heatmap.label"),
                  plotOutput("Group.members.heatmap"),
                  textOutput("Group.stackedPlot.label"),
                  plotOutput("Group.stackedPlot"),
                  textOutput("Group.stackedPlot.maxmin.label"),
                  plotOutput("Group.stackedPlot.maxmin"),
                  textOutput("Group.MDS_PLOT.Samples.label"),
                  plotOutput("Group.MDS_PLOT.Samples")
                 
                 ),
        tabPanel("Create Groups",
                 
                 fluidRow(
                   column(width=4,textInput(inputId="Group_Name",label="Group Name",placeholder="Enter Group Name")),
                   column(width=4,selectInput(inputId="All_Conditions", label="Select Group Members", c(""), selected = NULL, multiple = T))
                 ),
                 actionButton(inputId = "Create_Group",label = "Create Group"),
                 fileInput("Upload_Groups_File", "Choose 'Groups' File"),
                 actionButton(inputId = "Upload_Group",label = "Upload Groups"),
                 actionButton(inputId = "Confirm_Group",label = "Confirm Groups"),
                 textAreaInput(inputId = "Groups",label = "Groups",width = 800, height = 200),
                 downloadButton(outputId = "downloadGroups", label = "Download Groups")
                 ),
        tabPanel("Advanced Analysis")
        #)
         
      #)
   )
)

#DATA.Values = c()
#DATA.Values.5min = c()
#Groups =c()
#Group.Members=c()
#GeneList=c()
# Define server logic required to draw a histogram
server <- function(input,output,session,DATA.Values,DATA.Values.5min,Groups,Group.Members,GeneList,Reads.Data_File_path,readData,pengRPKMTable) {

#####################################
#####################################
#output$User_Data_File
  #####################################
  #####################################      

  obs <- observe({    
  
    
    Dataset.files = list.files(paste(Data.folder,"/",input$Data_User,sep=""))
    Dataset.files = subset(Dataset.files,(substr(Dataset.files,(nchar(Dataset.files)-4),nchar(Dataset.files))!=".info" ))
    data.file.select = input$Data_set
    updateSelectizeInput(session, inputId="Data_set", label = "Select Dataset", choices = Dataset.files,selected = data.file.select)
   
  })
  
#####################################
#####################################  
obs.Load_File.Group.button <- observeEvent(input$Load_File,{      
#####################################
#####################################  
  #####################
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "Processing Data", value = 0)
  # Increment the progress bar, and update the detail text.
  progress$inc(0.3, detail = "Please Wait")
  Sys.sleep(0.001)
  ################
  readData <<- c()
  Groups <<- c()
  Group.Members <<- c()
  updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices =c(""),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes", choices = c(""),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c(""),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = c(""),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = c(""),selected = NULL)
  updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes", choices = c(""),selected = NULL)
  updateTextAreaInput(session,inputId = "Groups",label = "Groups",value="")
  updateSelectizeInput(session, inputId="All_Conditions",selected = "")
  updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c(""),selected = NULL)
  updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = "")
  updateTextAreaInput(session,inputId="Reads_Table", label = paste("Reads Table"), value = "")
  updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = c(""))
  updateTextAreaInput(session,inputId="GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
  updateTextAreaInput(session,inputId="Groups.GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
  updateSelectizeInput(session,inputId="Groups.Control", label="Select Control",choices = c(""), selected = NULL)
  updateSelectizeInput(session,inputId="Groups.Treatment", label="Select Treatment",choices = c(""), selected = NULL)
  
  output$MDSPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.barplot.label <- renderText({" "})
  output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.heatmap.label <- renderText({" "})
  output$Single.Sample.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Stacked_Plot.label <- renderText({" "})
  output$Stacked_Plot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$MDS_PLOT.Samples.label <- renderText({" "})
  output$MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.barplot.label <- renderText({" "})
  output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.boxplot.label <- renderText({" "})
  output$Group.boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.label <- renderText({" "})
  output$Group.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.sem.label <- renderText({" "})
  output$Group.barplot.sem <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.maxmin.label <- renderText({" "})
  output$Group.barplot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.heatmap.label <- renderText({" "})
  output$Group.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.members.heatmap.label <- renderText({" "})
  output$Group.members.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.stackedPlot.label <- renderText({" "})
  output$Group.stackedPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.stackedPlot.maxmin.label <- renderText({" "})
  output$Group.stackedPlot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.MDS_PLOT.Samples.label <- renderText({" "})
  output$Group.MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  
  
  
  
  #isolate(Reads.data.file.name <- Reads.data.file.name())
  
  inp_source <- input$Select_input
  
  
  if(input$Select_input=="dbase"){isolate(Data_File_path <- paste(Data.folder,"/",input$Data_User,"/",input$Data_set,sep=""))}
  if(input$Select_input=="upload"){
    inFile.rpkm <- input$file_RPKM
    if (is.null(inFile.rpkm)) Data_File_path = ""
    if (!is.null(inFile.rpkm)) Data_File_path <- inFile.rpkm$datapath}
  

  
  File.Check = readLines(Data_File_path)
  if(length(File.Check)==0)
    return(NULL)
    
    Group_Count <<- 0
    Groups <<- c()
    Group.Members <<- c()
    

    DATA1=c("")
    if(nchar(input$Data_set)>1 | length(File.Check)!=0){
      DATA.RAW <- read.table(Data_File_path,header=T,sep="\t")
      GeneList <<- DATA.RAW[,1]
      DATA.temp =data.frame(lapply(DATA.RAW[,2:ncol(DATA.RAW)], as.character), stringsAsFactors=FALSE)
      
      duplicate.genes = unique(GeneList[which(duplicated(GeneList))])
      if(length(duplicate.genes) > 0){
      for(x in 1:length(duplicate.genes))
      {
        temp = DATA.temp[which(as.character(GeneList)==as.character(duplicate.genes[x])),]
        temp = data.frame(lapply(temp, as.numeric), stringsAsFactors=FALSE)
        temp[is.na(temp)] <- 0
        temp.sum = colSums(temp)
        
        DATA.temp[min(which(as.character(GeneList)==as.character(duplicate.genes[x]))),] = temp.sum
      }
      DATA.temp = DATA.temp[which(!duplicated(GeneList)),]
      GeneList <<- GeneList[which(!duplicated(GeneList))]
      #which(c(2,3,4,5,2,3,5,2)==2)
      }
      DATA.temp =data.frame(lapply(DATA.temp, as.numeric), stringsAsFactors=FALSE)
      DATA.temp[is.na(DATA.temp)] <- 0
            isolate(DATA.Values <<- DATA.temp)
    }
    
  
    progress$inc(0.6, detail = "Please Wait")
    Sys.sleep(0.001)
#####################################
#####################################
#obs <- output$distPlot
  #####################################
  #####################################      
   
   output$distPlot <- renderPlot({
     
     
     if(nchar(input$Data_set)>1 | length(File.Check)!=0)
     {
       
       d = dist(t(DATA.Values[,1:ncol(DATA.Values)]))
       fit <- isoMDS(d, k=2)
       
       par(xpd=NA)
       x <- fit$points[,1]
       y <- fit$points[,2]
       plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
            main="Nonmetric MDS (Combined Medium)\nAll genes",pch=16,type='n')
        text(x, y, labels = colnames(DATA.Values[,1:ncol(DATA.Values)]), cex=.8)
       
     }
      # generate bins based on input$bins from ui.R
   
   })

#####################################
#####################################
#obs <- observe
   #####################################
   #####################################       
   #obs <- observe({
     
     
     if(nchar(input$Data_set)>1 | length(File.Check)!=0)
     {
       DATA.Values.5min <<- DATA.Values[which (GeneList != "-"), ]
       GeneList <<- GeneList[which (GeneList != "-")]
       GeneList <<- GeneList[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5))]
       DATA.Values.5min <<- DATA.Values.5min[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5)), ]
       #DATA.Values.5min[DATA.Values.5min=="FPKM"] <<- 0
       #DATA.Values.5min[DATA.Values.5min>100000] <<- 0
       
       rownames(DATA.Values.5min) <<- GeneList
       Gene.Choices = as.character(GeneList)
       #Gene.Choices = as.matrix(unique(rownames(DATA.Values.5min)))
       updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices = colnames(DATA.Values[,1:ncol(DATA.Values)]),selected = NULL)
       updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes", choices = Gene.Choices,selected = NULL)
       updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c("ALL",colnames(DATA.Values)[1:ncol(DATA.Values)]),selected = NULL)
       updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
       updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
       updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes", choices = Gene.Choices,selected = NULL)
     }
   #})
#############################    
    progress$inc(1, detail = "Processing Complete")
    Sys.sleep(0.001)
    ##############
})#Load_File_button
   
#####################################
#####################################
#obs.Create.Group.button
   #####################################
   #####################################       
   obs.Create.Group.button <- observeEvent(input$Create_Group,{
     
     if(nchar(as.character(input$Group_Name))>=1 & length(as.character(input$All_Conditions))>=1)
     {
      
       temp.grp <- ""
       temp.grp <- input$Group_Name
       
       
       
       if(Group_Count==0)
       {
         Groups <<- c()
         Group.Members <<- c()
       }
       
       
       if(any(as.character(Groups)==as.character(input$Group_Name)))
       {
         Grp.Indx = which(as.character(Groups)==as.character(input$Group_Name))
         Group.Members[Grp.Indx] <<- paste(unique(c(unlist(strsplit(Group.Members[Grp.Indx],split=";")),input$All_Conditions)),collapse = ";")
       }
       if(!any(as.character(Groups)==as.character(input$Group_Name)))
       {
          Group_Count <<- Group_Count+1
          Groups <<- c(Groups,input$Group_Name)
          Group.Members<<- c(Group.Members,paste(input$All_Conditions,collapse = ";"))
       }

       
       
       if(nchar(nrow(DATA.Values))>1)
       {
         
        
         updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse=""))
         updateSelectizeInput(session, inputId="All_Conditions",selected = "")
         updateTextInput(session,inputId="Group_Name", value="")
         updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c("ALL",Groups),selected = NULL)
         
         if(length(readData)>1)
         {
          updateSelectizeInput(session,inputId="Groups.Control", label="Select Control",choices = Groups, selected = NULL)
          updateSelectizeInput(session,inputId="Groups.Treatment", label="Select Treatment",choices = Groups, selected = NULL)
         }
       }#if(nchar(input$Data_set)>1)
     }#if(nchar(as.character(input$Group_Name))>=1 & nchar(as.character(input$All_Conditions))>=1)
   })

#####################################
#####################################
#obs.Confirm.Group.button
   #####################################
   #####################################  
   obs.Confirm.Group.button <- observeEvent(input$Confirm_Group,{
  
      if(nchar(input$Groups)>=3)
      {
       
       isolate(Total_Groups <- length(grep(":",unlist(strsplit(input$Groups,"")))))
       isolate(Group_Sets <- unlist(strsplit(input$Groups,split="\n")))
       
       Group_Count <<- Total_Groups
       Groups <<- c()
       Group.Members <<- c()
       
       if(Total_Groups >= 1)
         {
         for(i in 1:Total_Groups)
         {
           Group_set_temp = unlist(strsplit(Group_Sets[i],split=":"))
           Group_set_members = unlist(strsplit(Group_set_temp[2],split="\n"))[1]
           
                      
           member.array = as.character(unlist(strsplit(Group_set_members,split=";")))
           colnames(DATA.Values)
           
           Group_set_members = member.array[which(!is.na(match(member.array,colnames(DATA.Values))))]
           Group_set_members = paste(Group_set_members,collapse=";")
           
           Groups <<- c(Groups,Group_set_temp[1])
           Group.Members <<- c(Group.Members,Group_set_members)
         }
         
         updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=paste(paste(Groups,":",Group.Members,sep="",collaspe="\n"),collapse=""))
         updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c("ALL",Groups),selected = NULL)
       }#if(Total_Groups>=1)
      }#if(length(Groups>=1))
   })
   
   
#####################################
#####################################
#obs.Upload.Group.button
   #####################################
   ##################################### 
   obs.Upload.Group.button <- observeEvent(input$Upload_Group,{
     Group_Data = readLines(input$Upload_Groups_File$datapath)
     Group_Data = Group_Data[which(nchar(Group_Data)>=3)]
     Group_Data = paste(Group_Data,collapse="\n")
     updateTextAreaInput(session,inputId = "Groups",label = "Groups",value=Group_Data)
     
   })
   

#####################################
#####################################
#obs.downloadGroups.button
   #####################################
   #####################################
   
   
   
   
   output$downloadGroups <- downloadHandler(
     filename = function() {
       paste("GroupData", ".txt", sep = "")
     },
     content = function(file) {
       write(input$Groups, file)
     })
   
#####################################
#####################################
#obs.Single.Sample.Compare.button
   #####################################
   ##################################### 
 obs.Single.Sample.Compare.button <- observeEvent(input$Compare.Sample.Exp,{
#####################
    progress <- shiny::Progress$new()
   # Make sure it closes when we exit this reactive, even if there's an error
   on.exit(progress$close())
   progress$set(message = "Comparing Samples", value = 0)
   # Increment the progress bar, and update the detail text.
   progress$inc(0.3, detail = "Please Wait")
   Sys.sleep(0.001)
################
   
   output$Single.Sample.barplot.label <- renderText({" "})
   output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Single.Sample.heatmap.label <- renderText({" "})
   output$Single.Sample.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Stacked_Plot.label <- renderText({" "})
   output$Stacked_Plot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$MDS_PLOT.Samples.label <- renderText({" "})
   output$MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   
   
   Single.Compare.GeneList <- isolate(input$Single.Compare.GeneList)
   Single.Compare.Conditions <- isolate(input$Single.Compare.Conditions)
   

   if(length(Single.Compare.GeneList)>=1)
   {
     gene.index.list = match( as.character(unlist(Single.Compare.GeneList)),as.character(unlist(rownames(DATA.Values.5min))))
     
     if(length(Single.Compare.Conditions) > 0 & all(as.character(unlist(Single.Compare.Conditions))!="ALL"))
     {
       Condition.index.list = match( as.character(unlist(Single.Compare.Conditions)),as.character(unlist(colnames(DATA.Values.5min))))
     }
     if(length(Single.Compare.Conditions) == 0 | any(as.character(unlist(Single.Compare.Conditions))=="ALL"))
     {
       Condition.index.list = c(1:ncol(DATA.Values)) 
     }
     
     expr.table = as.numeric(as.matrix(DATA.Values[gene.index.list[1],Condition.index.list]))
     if(length(gene.index.list)>1)
     {
       for(hij in 2:length(gene.index.list))
       {
         expr.table = rbind(expr.table,as.numeric(as.matrix(DATA.Values.5min[gene.index.list[hij],Condition.index.list])))
       }
     }
     rownames(expr.table) = GeneList[gene.index.list]
     colnames(expr.table) = colnames(DATA.Values.5min[,Condition.index.list])
     
     output$Single.Sample.barplot.label <- renderText({"Barplots: Collated genes expressions in each condition"})
     output$Single.Sample.barplot = renderPlot({
       
       BP7 = barplot(expr.table,beside=T,names.arg=colnames(DATA.Values.5min)[Condition.index.list],cex.names=0.7,col=c(2:(length(Single.Compare.GeneList)+1)),las=2,legend.text = Single.Compare.GeneList)
     })   
     if(nrow(expr.table)>=2)
     {
        output$Single.Sample.heatmap.label <- renderText({"Heatmap: Relative gene expression"})
        output$Single.Sample.heatmap = renderPlot({
          
          Temp.expr.table = expr.table
          for(k in 1:nrow(Temp.expr.table))
          {
            if(length(unlist(unique(Temp.expr.table[1,])))==1)
            {
              Temp.expr.table[k,1]=0.000001
            }
          }
       
       BP8 = pheatmap(Temp.expr.table, cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 6, main = paste("Norm. Exp. of Selected Genes"))
       
       })

       output$Stacked_Plot.label = renderText("Barplots: Individual Gene expressions compared among conditions")
       output$Stacked_Plot <- renderPlot({
         
         n.rows =  ceiling(length(gene.index.list)/3)
         
         par(mfrow=c(n.rows,3))
         for(xyz in 1:length(gene.index.list))
         {
           barplot(as.numeric(as.matrix(DATA.Values.5min[gene.index.list[xyz],Condition.index.list])),names.arg=colnames(DATA.Values.5min)[Condition.index.list],cex.names=0.8,main=GeneList[gene.index.list[xyz]],las=2,col=(xyz+1))
         }
         par(mfrow=c(1,1))
       })
       
       
     }#if(nrow(expr.table)>=2)
     
     if((nrow(expr.table)>=3) & (ncol(expr.table)>=3))
     {
      output$MDS_PLOT.Samples.label <- renderText("MDSPlot: Based on selected genes only")
      output$MDS_PLOT.Samples <- renderPlot({
       d = dist(t(expr.table))
       fit <- isoMDS(d, k=2)
       par(xpd=NA)
       x <- fit$points[,1]
       y <- fit$points[,2]
       plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
            main="Nonmetric MDS plot using selected genes",pch=16,type='n')
       text(x, y, labels = colnames(expr.table), cex=.8) 
       par(xpd=F)
      })
     }#if(nrow(expr.table)>=3)
     
   }#if(length(Single.Compare.GeneList)>=1)
   
   progress$inc(1, detail = "Comparing Complete")
   Sys.sleep(0.001)
 })
 
  
#####################################
#####################################
#obs.Group.Compare.button
 #####################################
 ##################################### 
 obs.Group.Compare.button <- observeEvent(input$Compare.Group.Exp,{
   
######################
   progress <- shiny::Progress$new()
   # Make sure it closes when we exit this reactive, even if there's an error
   on.exit(progress$close())
   progress$set(message = "Comparing Gene Expressions", value = 0)
   # Increment the progress bar, and update the detail text.
   progress$inc(0.3, detail = "Please Wait")
   Sys.sleep(0.001)
######################
   
   output$Single.Sample.barplot.label <- renderText({" "})
   output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   
   output$Group.boxplot.label <- renderText({" "})
   output$Group.boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.barplot.label <- renderText({" "})
   output$Group.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.barplot.sem.label <- renderText({" "})
   output$Group.barplot.sem <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.barplot.maxmin.label <- renderText({" "})
   output$Group.barplot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.heatmap.label <- renderText({" "})
   output$Group.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.members.heatmap.label <- renderText({" "})
   output$Group.members.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.stackedPlot.label <- renderText({" "})
   output$Group.stackedPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.stackedPlot.maxmin.label <- renderText({" "})
   output$Group.stackedPlot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   output$Group.MDS_PLOT.Samples.label <- renderText({" "})
   output$Group.MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
   
   
   
    Group.Compare.GeneList <- isolate(input$Group.Compare.GeneList)
    Group.Compare.Condition <- isolate(input$Group.Compare.Condition)
    
          
   if(length(Group.Compare.Condition)>=1 & length(Group.Compare.GeneList)>=1)
   {
     Group.Index=c()
     Gene.Index=c()
     
     if(any(as.character(unlist(Group.Compare.Condition)) == "ALL")){Group.Compare.Condition=Groups}

     for(a in 1:length(Group.Compare.Condition))
     {
       Group.Index[a] = which(Groups == Group.Compare.Condition[a])
     }
     for(b in 1:length(Group.Compare.GeneList))
     {
        Gene.Index[b] = which(GeneList == Group.Compare.GeneList[b])
     }
     
     Groups.Medians = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
     Groups.sem.max = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
     Groups.sem.min = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
     Groups.Max = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
     Groups.Min = matrix(nrow=length(Gene.Index),ncol=length(Group.Index))
 
     Group.Member.matrix = c()
     Group.Member.matrix.labels = c()
     BP.Lengths = c()
     for(k in 1:length(Group.Index))
     {
       Group.set.matrix = c()
       for(l in 1:length(Gene.Index))
       {
          Groups.Medians[l,k] = median(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
          Groups.Max[l,k] = max(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
          Groups.Min[l,k] = min(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
          Groups.sem.max[l,k] = Groups.Medians[l,k] + 2*sd(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))/sqrt(length(unlist(strsplit(Group.Members[Group.Index[k]],split=";"))))
          Groups.sem.min[l,k] = Groups.Medians[l,k] - 2*sd(as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))/sqrt(length(unlist(strsplit(Group.Members[Group.Index[k]],split=";"))))
          
          if(is.na(Groups.sem.max[l,k]))Groups.sem.max[l,k]=0
          if(is.na(Groups.sem.min[l,k]))Groups.sem.min[l,k]=0
          
          Group.set.matrix = rbind(Group.set.matrix, as.numeric(as.matrix(DATA.Values.5min[Gene.Index[l],match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))
       }
    
       BP.Lengths[k] = length(unlist(strsplit(Group.Members[Group.Index[k]],split=";")))
       Group.Member.matrix.labels = c(Group.Member.matrix.labels, paste(Groups[k],unlist(strsplit(Group.Members[Group.Index[k]],split=";")),sep=":"))
       Group.Member.matrix = cbind(Group.Member.matrix,Group.set.matrix)
     }
     
     rownames(Groups.Medians) = Group.Compare.GeneList
     colnames(Groups.Medians) = Group.Compare.Condition
     
     output$Group.boxplot.label <- renderText({"Boxplots: Group RPKM values"})
     output$Group.boxplot <- renderPlot({
       
       n.rows =  ceiling(length(Gene.Index)/3)
       par(mfrow=c(n.rows,3))
       for(x in 1:length(Gene.Index))
       {
         start=1
         for(h in 1:length(BP.Lengths))
         {
           if(h==1){BP.List = list(Group.Member.matrix[x,(start):(start+BP.Lengths[h]-1)])}
           if(h>1){BP.List = c(BP.List,list(Group.Member.matrix[x,(start):(start+BP.Lengths[h]-1)]))}
           
           start = (start+BP.Lengths[h]) 
         }
         boxplot(BP.List,names = Group.Compare.Condition, notch=F,lwd=2,frame=F,main = Group.Compare.GeneList[x] )
       }
       par(mfrow=c(1,1))
       
     })
     output$Group.barplot.label <- renderText({"Barplots: Collated gene RPKM medians in each Group"})
     output$Group.barplot <- renderPlot({
       bp.gp = barplot(Groups.Medians,beside=T,names.arg=Group.Compare.Condition,cex.names=0.7,col=c(2:(length(Group.Compare.GeneList)+1)),las=2,legend.text = Group.Compare.GeneList)
     })
     
     output$Group.barplot.sem.label <- renderText({"Barplots: Collated gene RPKM medians in each Group (error bars: standard error)"})
     output$Group.barplot.sem <- renderPlot({
       bp.gp.sem = barplot(Groups.Medians,beside=T,names.arg=Group.Compare.Condition,cex.names=0.7,col=c(2:(length(Group.Compare.GeneList)+1)),las=2,legend.text = Group.Compare.GeneList,ylim = c(min(Groups.sem.min),max(Groups.sem.max)))
       segments(bp.gp.sem,as.numeric(as.matrix(paste(Groups.sem.min))),bp.gp.sem,as.numeric(as.matrix(paste(Groups.sem.max))))
       arrows(bp.gp.sem,as.numeric(as.matrix(paste(Groups.sem.min))),bp.gp.sem,as.numeric(as.matrix(paste(Groups.sem.max))),angle = 90,code = 3,length = 0.02)
    })
     
     output$Group.barplot.maxmin.label <- renderText({"Barplots: Collated gene RPKM medians in each Group (error bars: Min/Max range)"})
     output$Group.barplot.maxmin <- renderPlot({
       bp.gp.minmax = barplot(Groups.Medians,beside=T,names.arg=Group.Compare.Condition,cex.names=0.7,col=c(2:(length(Group.Compare.GeneList)+1)),las=2,legend.text = Group.Compare.GeneList,ylim = c(0,max(Groups.Max)))
       segments(bp.gp.minmax,as.numeric(as.matrix(paste(Groups.Min))),bp.gp.minmax,as.numeric(as.matrix(paste(Groups.Max))))
       arrows(bp.gp.minmax,as.numeric(as.matrix(paste(Groups.Min))),bp.gp.minmax,as.numeric(as.matrix(paste(Groups.Max))),angle = 90,code = 3,length = 0.02)
     })
     
     if(nrow(Groups.Medians)>=2)
     {
       output$Group.heatmap.label <- renderText({"heatmap: Relative gene expression among selected groups"})
       output$Group.heatmap = renderPlot({
         
         Temp.Groups.Medians = Groups.Medians
         for(k in 1:nrow(Groups.Medians))
         {
           if(length(unique(as.numeric(unlist(Temp.Groups.Medians[1,]))))==1)
           {
             Temp.Groups.Medians[k,1]=0.000001
           }
         }
         
         HM.GP = pheatmap((Temp.Groups.Medians), cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 6, main = paste("Norm. Exp. of Selected Genes"))
         #pheatmap(cbind(c(0,0.000001),c(0,.000001)))
       })
       output$Group.members.heatmap.label <- renderText({"heatmap: Relative gene expression among group members"})
       output$Group.members.heatmap = renderPlot({
         
         colnames(Group.Member.matrix)=Group.Member.matrix.labels
         rownames(Group.Member.matrix)=Group.Compare.GeneList
         
         Temp.Group.Member.matrix = Group.Member.matrix
         for(k in 1:nrow(Temp.Group.Member.matrix))
         {
           if(length(unique(unlist(Temp.Group.Member.matrix[1,])))==1)
           {
             Temp.Group.Member.matrix[k,1]=0.000001
           }
         }
         
         HM.Mem.GP = pheatmap(Temp.Group.Member.matrix, cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 6, main = paste("Norm. Exp. of Selected Genes"))
         
         
       })
       
     }
     
     output$Group.stackedPlot.label <- renderText({"Barplots: Median Gene expressions compared between groups"})
     output$Group.stackedPlot <- renderPlot({
       
       n.rows =  ceiling(length(Gene.Index)/3)
       
       par(mfrow=c(n.rows,3))
       for(xyz in 1:length(Gene.Index))
       {
         barplot(as.numeric(as.matrix(Groups.Medians[xyz,])),names.arg=colnames(Groups.Medians),cex.names=0.8,main=rownames(Groups.Medians)[xyz],las=2,col=(xyz+1))
       }
       par(mfrow=c(1,1))
     })
     output$Group.stackedPlot.maxmin.label <- renderText({"Barplots: Median Gene expressions compared between groups (error bars: Min/Max range)"})
     output$Group.stackedPlot.maxmin <- renderPlot({
       
       n.rows =  ceiling(length(Gene.Index)/3)
       
       par(mfrow=c(n.rows,3))
       for(xyz in 1:length(Gene.Index))
       {
         bpgp.stackMM = barplot(as.numeric(as.matrix(Groups.Medians[xyz,])),names.arg=colnames(Groups.Medians),cex.names=0.8,main=rownames(Groups.Medians)[xyz],las=2,col=(xyz+1),ylim=c(0,max(Groups.Max[xyz,])))
         segments(bpgp.stackMM,as.numeric(as.matrix(Groups.Min[xyz,])),bpgp.stackMM,as.numeric(as.matrix(Groups.Max[xyz,])))
         arrows(bpgp.stackMM,as.numeric(as.matrix(Groups.Min[xyz,])),bpgp.stackMM,as.numeric(as.matrix(Groups.Max[xyz,])),angle = 90,code = 3,length = 0.02)
         
       }
       par(mfrow=c(1,1))
     })
     
     if((nrow(Groups.Medians)>=3) & (ncol(Groups.Medians)>=3))
     {
       output$MDS_PLOT.Samples.label <- renderText({"Group MDS Plot: Based on selected genes only"})
       output$MDS_PLOT.Samples <- renderPlot({
         d = dist(t(Groups.Medians))
         fit <- isoMDS(d, k=2)
         par(xpd=NA)
         x <- fit$points[,1]
         y <- fit$points[,2]
         plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
              main="Nonmetric MDS plot using selected genes",pch=16,type='n')
         text(x, y, labels = colnames(Groups.Medians), cex=.8) 
         par(xpd=F)
       })
     }#if(nrow(Groups.Medians)>=3)
       
     
   }#if(length(Group.Compare.Condition)>=1 & length(Group.Compare.GeneList)>=1)
##################  
    progress$inc(1, detail = "Comparison Complete")
    Sys.sleep(0.001)
    ######################
  })
   
#####################################
#####################################
#Reads.data.file.name
   #####################################
   ##################################### 
   Reads.data.file.name = output$Reads.User_Data_File <- renderText({

     Reads.Dataset.files = list.files(paste(Reads.Data.folder,"/",input$Reads.Data_User,sep=""))
     Reads.Dataset.files = subset(Reads.Dataset.files,(substr(Reads.Dataset.files,(nchar(Reads.Dataset.files)-4),nchar(Reads.Dataset.files))!=".info" ))
     Reads.data.file.select = input$Reads.Data_set
     updateSelectizeInput(session, inputId="Reads.Data_set", label = "Select Dataset", choices = Reads.Dataset.files,selected = Reads.data.file.select)
     

     Reads.Data_File_path = paste(Reads.Data.folder,"/",input$Reads.Data_User,"/",input$Reads.Data_set,sep="")

     
     return(Reads.Data_File_path)
   })
   output$Reads.User_Data_Info <- renderText({
     
     Reads.Dataset.files = list.files(paste(Reads.Data.folder,"/",input$Reads.Data_User,sep=""))
     Reads.Dataset.files = subset(Reads.Dataset.files,(substr(Reads.Dataset.files,(nchar(Reads.Dataset.files)-4),nchar(Reads.Dataset.files))!=".info" ))
     
     Reads.Data_File_path = paste(Reads.Data.folder,"/",input$Reads.Data_User,"/",input$Reads.Data_set,sep="")
     Output_Message = "\nNo File Info Available"
     if(file.exists(paste(Reads.Data_File_path,".info",sep="")))
     {
       dataINFO = readLines(paste(Reads.Data_File_path,".info",sep=""))
       Output_Message = dataINFO
     }
     
     
   })
   
   
   
   
   
   
#####################################
#####################################
#EdgeR Load Data obs.Edgr.Reads.Load_Data.button
   #####################################
   #####################################  
obs.Edgr.Reads.Load_Data.button <- observeEvent(input$Reads.Load_Data,{  
  
  Groups <<- c()
  Group.Members <<- c()
  updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices =c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes", choices = c(),selected = NULL)
  updateTextAreaInput(session,inputId = "Groups",label = "Groups",value="")
  updateSelectizeInput(session, inputId="All_Conditions",selected = "")
  updateSelectizeInput(session, inputId="Group.Compare.Condition", label = "Select Groups", choices = c(),selected = NULL)
  updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = "")
  updateTextAreaInput(session,inputId="Reads_Table", label = paste("Reads Table"), value = "")
  updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = c())
  updateTextAreaInput(session,inputId="GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
  updateTextAreaInput(session,inputId="Groups.GeneList_Result", label = paste("Differentially Expressed Genes"), value = "")
  updateSelectizeInput(session, inputId="Groups.Control", label = "Select Group", choices = c(),selected = NULL)
  updateSelectizeInput(session, inputId="Groups.Treatment", label = "Select Group", choices = c(),selected = NULL)
  
  output$MDSPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Groups.ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.barplot.label <- renderText({" "})
  output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.heatmap.label <- renderText({" "})
  output$Single.Sample.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Stacked_Plot.label <- renderText({" "})
  output$Stacked_Plot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$MDS_PLOT.Samples.label <- renderText({" "})
  output$MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Single.Sample.barplot.label <- renderText({" "})
  output$Single.Sample.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.boxplot.label <- renderText({" "})
  output$Group.boxplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.label <- renderText({" "})
  output$Group.barplot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.sem.label <- renderText({" "})
  output$Group.barplot.sem <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.barplot.maxmin.label <- renderText({" "})
  output$Group.barplot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.heatmap.label <- renderText({" "})
  output$Group.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.members.heatmap.label <- renderText({" "})
  output$Group.members.heatmap <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.stackedPlot.label <- renderText({" "})
  output$Group.stackedPlot <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.stackedPlot.maxmin.label <- renderText({" "})
  output$Group.stackedPlot.maxmin <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$Group.MDS_PLOT.Samples.label <- renderText({" "})
  output$Group.MDS_PLOT.Samples <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  
  
  output$MDSPlot <- renderPlot({

  ################################   
     progress <- shiny::Progress$new()
     # Make sure it closes when we exit this reactive, even if there's an error
     on.exit(progress$close())
     
     progress$set(message = "Processing Reads", value = 0)
     nstep <- 10
     # Increment the progress bar, and update the detail text.
     progress$inc(1/nstep, detail = paste("Progress: ",10,"%",sep=""))
     Sys.sleep(0.001)
     
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, it will be a data frame with 'name',
     # 'size', 'type', and 'datapath' columns. The 'datapath'
     # column will contain the local filenames where the data can
     # be found.
    
     isolate(Reads.data.file.name <- Reads.data.file.name())

     inp_source <- input$Reads.Select_input
     
     
     if(input$Reads.Select_input=="dbase"){file.datapath <- Reads.data.file.name}
     if(input$Reads.Select_input=="upload"){
       inFile <- input$file1
       if (is.null(inFile)) file.datapath = ""
       if (!is.null(inFile)) file.datapath <- inFile$datapath}
     
     File.Check = readLines(file.datapath)
     if(length(File.Check)==0)
       return(NULL)
     
#######################################################
     progress$inc(2/nstep, detail = paste("Progress: ",20,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################     
     ## edgeR Pipeline ##
     
     ## stolen momentarily from Peng ##
     
     # Note: Be careful with dplyr functions. They are wonderfully self-explanatory, but they often (e.g. select, filter) silently drop rownames. #
     # Since rownames = gene_symbol is used for much of the ordering and plotting, I have limited the use of dplyr functions. #
     

     library(edgeR)
     library(RColorBrewer)
     library(MASS)
     library(ggplot2)
     library(dplyr)
     library(pheatmap)
     library(VennDiagram)
     library(heatmaply)
     library(gridExtra)
     
     
     
     #filename = args[1] 
     #filename = inFile$datapath
     filename = file.datapath
     
     #allData = read.delim("total.mm9.CD8.IL2.IL15.IL21.reads.txt") #import total reads file
     
     allData = read.delim(filename);
     
     #allData = read.delim("Total.reads.Erin.MM10.txt") #import total reads file
     
     allData = allData[order(allData$len,decreasing=TRUE),] #order by decreasing length
     
     allData = allData[!duplicated(allData$symbol),] #remove redundant rows
     
     allData = allData[order(allData$symbol),] #order by gene symbol
     
     rownames(allData) = allData$symbol #name rows by gene symbol
 
#######################################################
#######################################################
     progress$inc(3/nstep, detail = paste("Progress: ",30,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################
     
     REPLICATES=F
     
     #################################################################################################################
     
     ## Make a table with normalized reads of all genes ##
     
     readData <- allData[,c(4:length(colnames(allData)))] #create matrix of just read data
     
     #experimentGroup <- c("Naive.WT.Media","mem.WT.media.aCD3","mem.WT.TSLP.aCD3","D8R.WT"," D8R.KO", "Naive.WT.TSLP","mem.WT.media","mem.WT.TSLP","Naive.WT.media.3.28 ","Naieve.WT.TSLP.3.28") #make column titles
     
     experimentGroup <- colnames(allData)[4:length(colnames(allData))] #make column titles
     
     readData <- DGEList(counts = readData, group = experimentGroup) #make DGEList object of reads
     
     readData <- calcNormFactors(readData) #normalization
     
     logCPM <- cpm(readData, normalized.lib.sizes = TRUE, log = TRUE, prior.count = .125) #log2 counts per million
     
     # RPKM <- rpkm(readData, gene.length = allData$len, normalized.lib.sizes = TRUE) #testing - this is the new rpkm function
     
     pengRPKM <- (2**logCPM)*1e3/allData[rownames(logCPM),3] #counts per million divided by gene length
     
     # rpkmTable <- data.frame(allData[rownames(RPKM), c(1,2)], RPKM) #testing - this goes with the new rpkm function
     
     pengRPKMTable <- data.frame(allData[rownames(logCPM), c(1,2)], pengRPKM) #make data frame with gene name/symbol and rpkm
     
     pengRPKMTable_allgenes = pengRPKMTable
     #write.table(pengRPKMTable, file = "normalized_allgenes.txt", sep = "\t", quote = FALSE, row.names = FALSE) #make a file
     
     #################################################################################################################

#######################################################
#######################################################
     progress$inc(4/nstep, detail = paste("Progress: ",40,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################
          
     ## Make a table with normalized reads of genes that are expressed significantly ## 
     
     readData <- allData[, c(4:length(colnames(allData)))] #create matrix of just read data
     
     readData <- DGEList(counts = readData, group = experimentGroup) #make DGEList object of reads
     
     counts_per_milli <- cpm(readData, normalized.lib.sizes = TRUE) #unnormalized cpm (norm factors all set to 1 in readData$samples)
     
     RPKM <- 1000 * counts_per_milli / allData$len #unnormalized rpkm
     
     readData <- readData[rowMeans(RPKM) >= 1 & rowSums(RPKM >5) >=2,] #eliminate rows with small means and small sums
     
     readData <- calcNormFactors(readData) #normalize "expressed" genes
     
     logCPM <- cpm(readData, normalized.lib.sizes = TRUE, log = TRUE, prior.count = .125) #log2 counts per million
     
     # RPKM <- rpkm(readData, gene.length = allData$len, normalized.lib.sizes = TRUE) #testing - this is the new rpkm function
     
     pengRPKM <- (2**logCPM)*1e3/allData[rownames(logCPM),3] #counts per million divided by gene length
     
     # rpkmTable <- data.frame(allData[rownames(RPKM), c(1,2)], RPKM) #testing - this goes with the new rpkm function
     
     pengRPKMTable <- data.frame(allData[rownames(logCPM), c(1,2)], pengRPKM) #make data frame with gene name/symbol and rpkm
     
     
     #write.table(pengRPKMTable, file = "normalized_expressed.txt", sep = "\t", quote = FALSE, row.names = FALSE) #make a file
     
     #################################################################################################################
     
     ## Make a Multidimensional Scaling Plot of different experiments to show clustering ##
     
     ## Use the top two commands if there aren't experimental replicates.  Use the bottom two if there are. ##

#######################################################
#######################################################
     progress$inc(5/nstep, detail = paste("Progress: ",50,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################     
     
     if(REPLICATES==T)
     {
       readData <- estimateCommonDisp(readData)
       
       readData <- estimateTagwiseDisp(readData)
     }else
     {
       readData <- estimateGLMCommonDisp(readData, method = "deviance", robust = TRUE, subset = NULL)
     }
     dataDispersion <- readData$common.dispersion^2
     
     
#######################################################
#######################################################
     progress$inc(6/nstep, detail = paste("Progress: ",60,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################   
     
     if(length(readData[1,])>=3)
     {
       P6 = plotMDS.DGEList(readData, main = "Multidimensional Scaling Plot", labels = experimentGroup, top = 200)
       P6
     }
#######################################################
#######################################################
     #progress$inc(6/nstep, detail = paste("Progress: ",65,"%",sep=""))
     #Sys.sleep(0.001)
     #######################################################  
     #######################################################      
     #################################################################################################################
     #experimentGroup
     #paired.combs = combn(length(experimentGroup),2)
     
     Gene.Table.Data = paste(paste(as.matrix(colnames(pengRPKMTable)),collapse=","),"\n",sep="")
     Gene.Table.Data2 = paste(paste(as.matrix(colnames(pengRPKMTable)[2:ncol(pengRPKMTable)]),collapse=","),"\n",sep="")
     #Reads.Table.Data = paste(paste(as.matrix(colnames(readData)),collapse=","),"\n",sep="")

     Progress.value = ceiling(nrow(readData)/10)
     for(i in 1:nrow(readData))
     {
#############       
       
       if((i %% Progress.value)==0)
       {
        progress$inc(6/nstep, detail = paste("Progress: ",(60+i/Progress.value),"%",sep=""))
        Sys.sleep(0.001)
#############     
       }
       Gene.Table.Data = paste(Gene.Table.Data,paste(as.matrix(pengRPKMTable[i,]),collapse=","),"\n",sep="")
       Gene.Table.Data2 = paste(Gene.Table.Data2,paste(as.matrix(pengRPKMTable[i,2:ncol(pengRPKMTable)]),collapse="\t"),"\n",sep="")
     }

#######################################################
#######################################################
     #progress$inc(7/nstep, detail = paste("Progress: ",70,"%",sep=""))
     #Sys.sleep(0.001)
     #######################################################  
     #######################################################    
     
     updateTextAreaInput(session,inputId="Gene_Table", label = paste("Gene Table"), value = Gene.Table.Data)
     updateTextAreaInput(session,inputId="Reads_Table", label = paste("Reads Table"), value = Gene.Table.Data2)
     updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = experimentGroup)
     
     #ALL_Genes_List =  rownames(pengRPKMTable)
     #ALL_conditions = c("ALL",colnames(readData))      
     
     pengRPKMTable <<- pengRPKMTable 
     readData <<- readData
     
     #updateSelectInput(session,inputId="All_Genes", label="All Genes",choices = ALL_Genes_List)
     #updateSelectInput(session,inputId="Conditions", label="Conditions",choices = ALL_conditions,selected = 1)
     
     
     #############################################
     
     ###############################################       
    
#######################################################
#######################################################
     progress$inc(8/nstep, detail = paste("Progress: ",80,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################     
  
    Group_Count <<- 0
     Groups <<- c()
     Group.Members <<- c()
     
     
     DATA1=c("")
     #if(nchar(input$Data_set)>1)
     {
       DATA.RAW <- pengRPKMTable[,2:ncol(pengRPKMTable)]
       #colnames(DATA.RAW) = pengRPKMTable[1,2:ncol(pengRPKMTable)]
       GeneList <<- DATA.RAW[,1]
       DATA.temp =data.frame(lapply(DATA.RAW[,2:ncol(DATA.RAW)], as.character), stringsAsFactors=FALSE)
       
       duplicate.genes = unique(GeneList[which(duplicated(GeneList))])
       if(length(duplicate.genes) > 0){
         for(x in 1:length(duplicate.genes))
         {
           temp = DATA.temp[which(as.character(GeneList)==as.character(duplicate.genes[x])),]
           temp = data.frame(lapply(temp, as.numeric), stringsAsFactors=FALSE)
           temp[is.na(temp)] <- 0
           temp.sum = colSums(temp)
           
           DATA.temp[min(which(as.character(GeneList)==as.character(duplicate.genes[x]))),] = temp.sum
         }
         DATA.temp = DATA.temp[which(!duplicated(GeneList)),]
         GeneList <<- GeneList[which(!duplicated(GeneList))]
       }
       
       DATA.temp =data.frame(lapply(DATA.temp, as.numeric), stringsAsFactors=FALSE)
       DATA.temp[is.na(DATA.temp)] <- 0
       
       isolate(DATA.Values <<- DATA.temp)
     }
 
     
#######################################################
#######################################################
     progress$inc(9/nstep, detail = paste("Progress: ",90,"%",sep=""))
     Sys.sleep(0.001)
     #######################################################  
     #######################################################      
     
     DATA.Values.5min <<- DATA.Values[which (GeneList != "-"), ]
     GeneList <<- GeneList[which (GeneList != "-")]
     GeneList <<- GeneList[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5))]
     DATA.Values.5min <<- DATA.Values.5min[apply(as.matrix(DATA.Values.5min), MARGIN = 1, function(x) any(x >= 5)), ]
     #DATA.Values.5min[DATA.Values.5min=="FPKM"] <<- 0
     #DATA.Values.5min[DATA.Values.5min>100000] <<- 0
     
     rownames(DATA.Values.5min) <<- GeneList
     Gene.Choices = as.character(GeneList)
     #Gene.Choices = as.matrix(unique(rownames(DATA.Values.5min)))
     updateSelectizeInput(session, inputId="All_Conditions", label = "Select Dataset", choices = colnames(DATA.Values[,1:ncol(DATA.Values)]),selected = NULL)
     updateSelectizeInput(session, inputId="Single.Compare.GeneList", label = "Select Genes", choices = Gene.Choices,selected = NULL)
     updateSelectizeInput(session, inputId="Single.Compare.Conditions", label = "Select Samples", choices = c("ALL",colnames(DATA.Values)[1:ncol(DATA.Values)]),selected = NULL)
     updateSelectizeInput(session, inputId="Single.Control", label = "Select Control", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
     updateSelectizeInput(session, inputId="Single.Treatment", label = "Select Treatment", choices = colnames(DATA.Values)[1:ncol(DATA.Values)],selected = NULL)
     updateSelectizeInput(session, inputId="Group.Compare.GeneList", label = "Select Genes", choices = Gene.Choices,selected = NULL)
     
     
     
     
     
     ###############################################
     
     ###############################################    
     
     
     
     progress$inc(10/nstep, detail = paste("Progress: ",99,"%",sep=""))
   })
})#-obs.Edgr.Reads.Load_Data.button
     
     
#####################################
#####################################
#EdgeR Sample Pairwise obs.Edgr.Compare.Conditions.button
   #####################################
   ##################################### 
obs.Edgr.Compare.Conditions.button <- observeEvent(input$CompareButton,{  
  
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  
  progress$set(message = "Calculating Differential Expression", value = 0)
  nstep <- 10
  # Increment the progress bar, and update the detail text.
###############################################         
  progress$inc(1/nstep, detail = paste("Progress: ",10,"%",sep=""))
  Sys.sleep(0.001)
  ###############################################  
  
  output$ComparePlot1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  output$ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
  
  
  
  output$ComparePlot1 <- renderPlot({
       isolate({
        
       
         
       ###########################################
       ###################################
       ###################################
       ###################################
       
       ## Do a pairwise comparison between experimental conditions ##
       
       ## Volcano Plot, followed by Smear Plot, followed by Heat Map ##
       
       ## Volcano Plot
       
         if(length(input$Comparison)>1){
         
         xLabel <- "logCPM"
         
         yLabel <- "logFC"
         
         Sample1 = input$Comparison[1]
         Sample2 = input$Comparison[2]
         
         
         
         experimentGroup = colnames(readData)
         rownames(readData) = pengRPKMTable[,2]
         rownames(pengRPKMTable) = pengRPKMTable[,2]
         readData <- DGEList(counts = readData, group = colnames(readData))
         readData <- estimateGLMCommonDisp(readData, method = "deviance", robust = TRUE, subset = NULL)
         dataDispersion <- readData$common.dispersion^2
         
         
        
         experimentPair <- c(Sample1,Sample2) #choose a pair of experiments to compare
         
         comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
         
         comparisonTable <- comparison$table #rename the variable within comparison so RStudio won't complain all the time
         
         thresholdRatio <- as.numeric(input$Log2FCThreshold)
         
         mainTitle <- paste(Sample2,"vs.",Sample1,"(FC >",2**thresholdRatio,", FDR <",as.numeric(as.matrix(input$FDR_PvalThreshold)) ,")") #set up a title for graphs
         
         
         numGenes <- sum(abs(decideTestsDGE(comparison,adjust.method = input$Pval_Correction, p.value = input$FDR_PvalThreshold))) # number of genes with significant differential expression

                 
         if (numGenes > 0) { #check to make sure there is any differential expression
           
           
           if(length(readData[1,])>=3)
           {
             P6 <- plotMDS.DGEList(readData, main = "Multidimensional Scaling Plot", labels = experimentGroup, top = 200)
           }
           
           plot(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)],pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]
                ,xlab = paste(Sample1,"rpkm"), ylab = paste(Sample2,"rpkm"))
           #)
           
           topGenes <- topTags(comparison, n = numGenes) #top genes
           
           allGenes <- topTags(comparison, n = nrow(comparisonTable)) #all genes
           
           topGenesTable <- topGenes$table[topGenes$table$logCPM > 1,] #filter top genes for logCPM > 1, keep original variable intact
           
           allGenesTable <- allGenes$table #rename for ease
           
           FC2 <- rownames(topGenesTable[abs(topGenesTable$logFC) > log2(thresholdRatio),]) #FC2 = rownames of fold change # greater than log2 threshold ratio
           
           FC2_table <- data.frame(pengRPKMTable[FC2,], topGenesTable[FC2,], 2**topGenesTable[FC2, c(1)]) #make a table of FC2 genes with their expression ratio
           
           colnames(FC2_table)[length(colnames(FC2_table))] <- "foldChange" #rename last column
           
           #write.table(FC2_table[1:5,], file = paste(Directory,Sample1,".vs.",Sample2,".FDR0.05.FC2.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
           
           #FC2_table
           
           comparedAllGenes <- data.frame(pengRPKMTable[rownames(allGenesTable),], allGenesTable, 2**allGenesTable[, c(1)]) #make a table of all compared genes
           
           colnames(comparedAllGenes)[length(colnames(comparedAllGenes))] <- "foldChange" #rename last column
           
           #write.table(comparedAllGenes, file = paste(Directory,Sample1,".vs.",Sample2,".allGenes.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
           
           evenExpression <- setdiff(comparedAllGenes, FC2_table) #make a table of evenly expressed genes
           
           #write.table(evenExpression, file = paste(Directory,Sample1,".vs.",Sample2,".evenExpression.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file

             
           p1 <- ggplot(xlab = "log2FC (treat/control)", ylab = "-log10(p-value)")+
             ggtitle(mainTitle)+ #put in the title
             theme_bw()+ #make background white, must come before theme function call
             theme(plot.title = element_text(hjust = .8, size = 15))+ #center title and make it bigger
             geom_point(data = evenExpression, aes(x = logFC, y= -log10(PValue + 1e-300)), alpha = 1/150)+ #plot the even expression points
             geom_point(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), color = "darkblue", alpha = 2/5)+ #plot the differentially expressed points
             geom_vline(xintercept = c(-log2(thresholdRatio), log2(thresholdRatio)), color = "forestgreen")+ #plot the vertical boundary lines
             geom_text(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), label = rownames(FC2_table), color = "firebrick", size = 2, nudge_y = -1) #label the diff. expr. points

        
           output$ComparePlot2 = renderPlot({
             p1 #must be done to render the image
             
           })#output$ComparePlot2 
           
           ## Smear Plot
           
           yLabel <- expression(M == log[2](treat/control)) #change the y label
           
           up <- nrow(topGenesTable[topGenesTable$logFC > log2(thresholdRatio),]) #number of rows of upregulated genes
           
           down <- nrow(topGenesTable[topGenesTable$logFC < -log2(thresholdRatio),]) #number of rows of downregulated genes
           
           #plot the compared genes, highlighting the differentially expressed ones
           #mirroring horizontal orange boundary lines
           #vertical grey boundary line
           #label the upregulation region of the graph
           #label the downregulation region of the graph
 
           
           output$ComparePlot3 = renderPlot({
             {plotSmear(comparison, de.tags = rownames(topGenesTable), main = mainTitle, xlab = xLabel, ylab = yLabel, ylim = c(-5,5), col = "grey")
               abline(h=c(-1, 1), col = rep("orange", 2))
               abline(v=1, col = "grey")
               text(8, 3, paste("upregulated=",up))
               text(8, -3, paste("downregulated=",down))
             }
           })#output$ComparePlot3
           #grid.arrange(p1, p2, ncol = 2, nrow = 2)

  
           
           ## Heat Map
           
           FC2_table <- FC2_table[!duplicated(FC2_table$symbol),] #remove duplicated genes from table
           FC2_table.sorted <- FC2_table[order(FC2_table$FDR,decreasing = F),]
           
           FC_Genes <- FC2_table.sorted$symbol
           #FC_Genes <- FC2_table$symbol
           rownames(FC2_table) <- FC2_table$symbol #name rows for gene symbols
           
           
           FC2_table <- FC2_table[,3:(length(experimentGroup)+2)]
           
           
           if(length(readData[1,])>=3)
           {
             P7=P6
             Sorted.distances <- c()
             Sorted.distances[1] = 1
             
             K=1
             for(J in 1:(length(P7$distance.matrix[,1])-1))
             {
               P7$distance.matrix[P7$distance.matrix==0] = 1000
               if(K==1)
               {
                 Dists =c(P7$distance.matrix[K,1:K], P7$distance.matrix[(K+1):length(P7$distance.matrix[,1]),K])
               }else
               {
                 Dists =c(P7$distance.matrix[K,1:(K-1)], P7$distance.matrix[(K):length(P7$distance.matrix[,1]),K])
               }
               
               Sorted.distances[J+1]=which.min(Dists)
               P7$distance.matrix[,K] = 1000
               P7$distance.matrix[K,] = 1000
               K = which.min(Dists)
             }
             
             
             FC2_table <- FC2_table[, Sorted.distances] #re-order columns
           }
           
           output$ComparePlot4 = renderPlot({
             if(length(FC2_table[,1])>=2)
               pheatmap(log2(FC2_table + 1), cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1)"))
           })#output$ComparePlot4
          
           
           if(length(FC2_table[,1])>100){
             output$ComparePlot5 = renderPlot( pheatmap(log2(FC2_table[1:50,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 50 genes ranked")))
             output$ComparePlot6 = renderPlot(pheatmap(log2(FC2_table[1:50,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 50 genes clustered")))
             output$ComparePlot7 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes ranked")))
             output$ComparePlot8 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes clustered")))
             
           }#if(length(FC2_table[,1])>100)
           
   
           updateTextAreaInput(session,inputId="GeneList_Result", label = paste("Differentially Expressed Genes",mainTitle), value = paste(FC_Genes,collapse="\n"))

         }#if (numGenes > 0)
         
         updateSelectizeInput(session, inputId="Comparison", label = "Select Library", choices = experimentGroup,selected = c(Sample1,Sample2))
         
         
      
         #################################################################################################################
         
         ##################################
         ##################################
         ##################################
         ##################################
       }
       #})
       
     })#isolate
     
   
     #################################
     #################################
     #################################
   })
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste("RPKM_data", ".csv", sep = "")
     },
     content = function(file) {
       write(input$Gene_Table, file)
  })
   
###############################################         
   progress$inc(10/nstep, detail = paste("Progress: ",99,"%",sep=""))
   Sys.sleep(0.001)
   ###############################################   
})#-obs.Edgr.Compare.Conditions.button

 
#####################################
#####################################
#EdgeR Group Pairwise obs.Edgr.Compare.C.button
   #####################################
   #####################################    
   obs.Edgr.Compare.Groups.button <- observeEvent(input$Groups.CompareButton,{  
     
#####################
     progress <- shiny::Progress$new()
     # Make sure it closes when we exit this reactive, even if there's an error
     on.exit(progress$close())
     progress$set(message = "Comparing Groups", value = 0)
     # Increment the progress bar, and update the detail text.
     progress$inc(0.3, detail = "Please Wait")
     Sys.sleep(0.001)
################
     
     output$Groups.ComparePlot1 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot2 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot3 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot4 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot5 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot6 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot7 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     output$Groups.ComparePlot8 <- renderPlot(plot(0,cex=0,axes=F,xlab="",ylab=""))
     
     output$Groups.ComparePlot1 = renderPlot({
       isolate({
         ###########################################
         ###################################
         ###################################
         ###################################
         
         ## Do a pairwise comparison between experimental conditions ##
         
         ## Volcano Plot, followed by Smear Plot, followed by Heat Map ##
         
         ## Volcano Plot
                 

         
        
         if(length(input$Groups.Control)==1 & length(input$Groups.Treatment)==1){
           
           xLabel <- "logCPM"
           
           yLabel <- "logFC"
           
           
           
          
           isolate(Sample1 <- input$Groups.Control)
           isolate(Sample2 <- input$Groups.Treatment)
           
           Group.Index = c()
           Group.Index[1] = which(Groups == Sample1)
           Group.Index[2] = which(Groups == Sample2)
           
           #Groups.readData.Medians[l,k] = median(as.numeric(as.matrix(readData[,match(unlist(strsplit(Group.Members[Group.Index[k]],split=";")),as.character(unlist(colnames(DATA.Values.5min))))])))

           Groups.readData.ctrl = readData[,match(unlist(strsplit(Group.Members[Group.Index[1]],split=";")),as.character(unlist(colnames(readData))))]
           Groups.readData.treat = readData[,match(unlist(strsplit(Group.Members[Group.Index[2]],split=";")),as.character(unlist(colnames(readData))))]
           
           Groups.readData.ctrl.medians = apply(Groups.readData.ctrl,1, median, na.rm = TRUE)
           Groups.readData.treat.medians = apply(Groups.readData.treat,1, median, na.rm = TRUE)

           Groups.readData.med = cbind(Groups.readData.ctrl.medians,Groups.readData.treat.medians)
           colnames(Groups.readData.med) = as.character(c(Sample1,Sample2))
           
           Groups.pengRPKMTable.ctrl = pengRPKMTable[,match(unlist(strsplit(Group.Members[Group.Index[1]],split=";")),as.character(unlist(colnames(pengRPKMTable))))]
           Groups.pengRPKMTable.treat = pengRPKMTable[,match(unlist(strsplit(Group.Members[Group.Index[2]],split=";")),as.character(unlist(colnames(pengRPKMTable))))]
           
           Groups.pengRPKMTable.ctrl.medians = apply(Groups.pengRPKMTable.ctrl,1, median, na.rm = TRUE)
           Groups.pengRPKMTable.treat.medians = apply(Groups.pengRPKMTable.treat,1, median, na.rm = TRUE)
           
           Groups.pengRPKMTable.med = cbind(pengRPKMTable[,1:2],Groups.readData.ctrl.medians,Groups.readData.treat.medians)
           colnames(Groups.pengRPKMTable.med) = as.character(c("gene_name","symbol",Sample1,Sample2))
           
           pengRPKMTable=Groups.pengRPKMTable.med
           readData = Groups.readData.med
           
           
           
           #readData = read.table(text=gsub("(?<=[a-z])\\s+", "\n", input$Reads_Table, perl=TRUE),header=T,sep=",")
           
           #pengRPKMTable = read.table(text=gsub("(?<=[a-z])\\s+", "\n", input$Gene_Table, perl=TRUE), header=T,sep=",")
           
           experimentGroup = colnames(readData)
           rownames(readData) = pengRPKMTable[,2]
           rownames(pengRPKMTable) = pengRPKMTable[,2]
           readData <- DGEList(counts = readData, group = colnames(readData))
           readData <- estimateGLMCommonDisp(readData, method = "deviance", robust = TRUE, subset = NULL)
           dataDispersion <- readData$common.dispersion^2
           
           if(length(readData[1,])>=3)
           {
             P6 = plotMDS.DGEList(readData, main = "Multidimensional Scaling Plot", labels = experimentGroup, top = 200)
           }
           
           #readData=readData[,2:ncol(readData)]
           #rownames(pengRPKMTable) = pengRPKMTable[1,]
           
           #pengRPKMTable = pengRPKMTable[2:nrow(pengRPKMTable),]
           #readData = readData[2:nrow(readData),]
           #Naive.WT.Media vs mem.WT.media.aCD3
           experimentPair <- c(Sample1,Sample2) #choose a pair of experiments to compare
           
           comparison <- exactTest(readData, pair = experimentPair) #compare the pair, add argument `dispersion = dataDispersion` if there aren't replicates
           
           comparisonTable <- comparison$table #rename the variable within comparison so RStudio won't complain all the time
           
           thresholdRatio <- as.numeric(input$Groups.Log2FCThreshold)
           
           mainTitle <- paste(Sample2,"vs.",Sample1,"(FC >",2**thresholdRatio,", FDR <",as.numeric(as.matrix(input$Groups.FDR_PvalThreshold)) ,")") #set up a title for graphs
           
           
           numGenes <- sum(abs(decideTestsDGE(comparison,adjust.method = input$Groups.Pval_Correction, p.value = input$Groups.FDR_PvalThreshold))) # number of genes with significant differential expression
           
           if (numGenes > 0) { #check to make sure there is any differential expression
             
             #pdf(paste(Directory,"summary.",Sample1,".vs.",Sample2,".pdf",sep=""),height=8,width=8)
             
             
             #if(length(readData[1,])>=3)
             #{
             #  plotMDS.DGEList(readData, main = "Multidimensional Scaling Plot", labels = experimentGroup, top = 200)
             #}
             
             plot(pengRPKMTable[,which(colnames(pengRPKMTable)==Sample1)],pengRPKMTable[,which(colnames(pengRPKMTable)==Sample2)]
                  ,xlab = paste(Sample1,"rpkm"), ylab = paste(Sample2,"rpkm"))
             #)
             
             topGenes <- topTags(comparison, n = numGenes) #top genes
             
             allGenes <- topTags(comparison, n = nrow(comparisonTable)) #all genes
             
             topGenesTable <- topGenes$table[topGenes$table$logCPM > 1,] #filter top genes for logCPM > 1, keep original variable intact
             
             allGenesTable <- allGenes$table #rename for ease
             
             FC2 <- rownames(topGenesTable[abs(topGenesTable$logFC) > log2(thresholdRatio),]) #FC2 = rownames of fold change # greater than log2 threshold ratio
             
             FC2_table <- data.frame(pengRPKMTable[FC2,], topGenesTable[FC2,], 2**topGenesTable[FC2, c(1)]) #make a table of FC2 genes with their expression ratio
             
             colnames(FC2_table)[length(colnames(FC2_table))] <- "foldChange" #rename last column
             
             #write.table(FC2_table[1:5,], file = paste(Directory,Sample1,".vs.",Sample2,".FDR0.05.FC2.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
             
             #FC2_table
             
             comparedAllGenes <- data.frame(pengRPKMTable[rownames(allGenesTable),], allGenesTable, 2**allGenesTable[, c(1)]) #make a table of all compared genes
             
             colnames(comparedAllGenes)[length(colnames(comparedAllGenes))] <- "foldChange" #rename last column
             
             #write.table(comparedAllGenes, file = paste(Directory,Sample1,".vs.",Sample2,".allGenes.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
             
             evenExpression <- setdiff(comparedAllGenes, FC2_table) #make a table of evenly expressed genes
             
             #write.table(evenExpression, file = paste(Directory,Sample1,".vs.",Sample2,".evenExpression.txt",sep=""), sep = "\t", quote = FALSE, row.names = FALSE) #make a file
             
             
             p1 <- ggplot(xlab = "log2FC (treat/control)", ylab = "-log10(p-value)")+
               ggtitle(mainTitle)+ #put in the title
               theme_bw()+ #make background white, must come before theme function call
               theme(plot.title = element_text(hjust = .8, size = 15))+ #center title and make it bigger
               geom_point(data = evenExpression, aes(x = logFC, y= -log10(PValue + 1e-300)), alpha = 1/150)+ #plot the even expression points
               geom_point(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), color = "darkblue", alpha = 2/5)+ #plot the differentially expressed points
               geom_vline(xintercept = c(-log2(thresholdRatio), log2(thresholdRatio)), color = "forestgreen")+ #plot the vertical boundary lines
               geom_text(data = FC2_table, aes(x = logFC, y = -log10(PValue + 1e-300)), label = rownames(FC2_table), color = "firebrick", size = 2, nudge_y = -1) #label the diff. expr. points
             
             output$Groups.ComparePlot2 = renderPlot({
               p1 #must be done to render the image
               
             })#output$ComparePlot2 
             
             ## Smear Plot
             
             yLabel <- expression(M == log[2](treat/control)) #change the y label
             
             up <- nrow(topGenesTable[topGenesTable$logFC > log2(thresholdRatio),]) #number of rows of upregulated genes
             
             down <- nrow(topGenesTable[topGenesTable$logFC < -log2(thresholdRatio),]) #number of rows of downregulated genes
             
             #plot the compared genes, highlighting the differentially expressed ones
             #mirroring horizontal orange boundary lines
             #vertical grey boundary line
             #label the upregulation region of the graph
             #label the downregulation region of the graph
             
             output$Groups.ComparePlot3 = renderPlot({
               {plotSmear(comparison, de.tags = rownames(topGenesTable), main = mainTitle, xlab = xLabel, ylab = yLabel, ylim = c(-5,5), col = "grey")
                 abline(h=c(-1, 1), col = rep("orange", 2))
                 abline(v=1, col = "grey")
                 text(8, 3, paste("upregulated=",up))
                 text(8, -3, paste("downregulated=",down))
               }
             })#output$ComparePlot3
             #grid.arrange(p1, p2, ncol = 2, nrow = 2)
#################
             progress$inc(1, detail = "Please Wait: 65%")
             Sys.sleep(0.001)  
##################                          
             ## Heat Map
             
             FC2_table <- FC2_table[!duplicated(FC2_table$symbol),] #remove duplicated genes from table
             FC2_table.sorted <- FC2_table[order(FC2_table$FDR,decreasing = F),]
             
             FC_Genes <- FC2_table.sorted$symbol
             #FC_Genes <- FC2_table$symbol
             rownames(FC2_table) <- FC2_table$symbol #name rows for gene symbols
             
             
             FC2_table <- FC2_table[,3:(length(experimentGroup)+2)]
             
             
             if(length(readData[1,])>=3)
             {
               
               P7=P6
               Sorted.distances <- c()
               Sorted.distances[1] = 1
               
               K=1
               for(J in 1:(length(P7$distance.matrix[,1])-1))
               {
                 P7$distance.matrix[P7$distance.matrix==0] = 1000
                 if(K==1)
                 {
                   Dists =c(P7$distance.matrix[K,1:K], P7$distance.matrix[(K+1):length(P7$distance.matrix[,1]),K])
                 }else
                 {
                   Dists =c(P7$distance.matrix[K,1:(K-1)], P7$distance.matrix[(K):length(P7$distance.matrix[,1]),K])
                 }
                 
                 Sorted.distances[J+1]=which.min(Dists)
                 P7$distance.matrix[,K] = 1000
                 P7$distance.matrix[K,] = 1000
                 K = which.min(Dists)
               }
               
               
               FC2_table <- FC2_table[, Sorted.distances] #re-order columns
             }
             
             output$Groups.ComparePlot4 = renderPlot({
               if(length(FC2_table[,1])>=2)
                 pheatmap(log2(FC2_table + 1), cluster_cols = FALSE, cluster_rows = TRUE, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1)"))
             })#output$ComparePlot4
             
             if(length(FC2_table[,1])>100){
               output$Groups.ComparePlot5 = renderPlot( pheatmap(log2(FC2_table[1:50,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 50 genes ranked")))
               output$Groups.ComparePlot6 = renderPlot(pheatmap(log2(FC2_table[1:50,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 50 genes clustered")))
               output$Groups.ComparePlot7 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = F, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes ranked")))
               output$Groups.ComparePlot8 = renderPlot(pheatmap(log2(FC2_table[1:100,] + 1), cluster_cols = FALSE, cluster_rows = T, scale = "row", cellwidth = 15, fontsize_row = 4, main = paste(mainTitle,"Norm. Exp. of Genes\nlog2(rpkm+1) - Top 100 genes clustered")))
               
             }#if(length(FC2_table[,1])>100)
             
             
             updateTextAreaInput(session,inputId="Groups.GeneList_Result", label = paste("Differentially Expressed Genes",mainTitle), value = paste(FC_Genes,collapse="\n"))
             
           }#if (numGenes > 0)
           
           updateSelectizeInput(session, inputId="Groups.Control", label = "Select Group", choices = Groups,selected = Sample1)
           updateSelectizeInput(session, inputId="Groups.Treatment", label = "Select Group", choices = Groups,selected = Sample2)
           
           
           
           #################################################################################################################
           
           ##################################
           ##################################
           ##################################
           ##################################
         }
         #})
         
       })#isolate
       
       #################################
       #################################
       #################################
     })
     
     output$Groups.downloadData <- downloadHandler(
       filename = function() {
         paste("RPKM_data", ".csv", sep = "")
       },
       content = function(file) {
         write(input$Groups.Gene_Table, file)
       })
     
##################
     progress$inc(1, detail = "Comparison Complete")
     Sys.sleep(0.001)     
#################     
   })#-obs.Edgr.Compare.Conditions.button
   
   
     
}#server

# Run the application 
shinyApp(ui = ui, server = server)

}


