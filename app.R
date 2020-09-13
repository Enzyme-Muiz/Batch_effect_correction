#batch_correction
library("shinyWidgets")
#library(BiocManager)
#BiocManager::install("flowClust")
#options(repos = BiocManager::repositories("flowCore", "Biobase", "FlowSOM", "flowClust", "flowViz"))
#options(repos = BiocManager::repositories())
#


#getOption("repos")
#getOption("repos")
#library(ggplot2)
#library(Biobase)
#library(flowCore)
#library(FlowSOM)
#library(flowClust)
#library(flowViz)
ui<- fluidPage(
  tabsetPanel(
    tabPanel(title= "Batch_effect_viz",
             fluidRow
             (
               column(12,align="center", tags$div(class= "fade",
                                                 tags$em(tags$h1("Batch Effect Visualization")))),
               
             ),
  sidebarLayout
             (
               
  sidebarPanel(width= 7,           
                 
             
  fluidRow(
    column(6,center="align",
  fileInput(
    inputId="file1",
    label= NULL,
    multiple = FALSE,
    accept = ".FCS",
    width = '250px',
    buttonLabel = "Browse",
    placeholder = "anchor_sam_1"
  )),
  column(6, align="center",
  fileInput(
    inputId="file2",
    label= NULL,
    multiple = FALSE,
    accept = ".FCS",
    width = '250px',
    buttonLabel = "Browse",
    placeholder = "anchor_sam_2"
  ))),
  fluidRow(
    column(6,center="align",
           fileInput(
             inputId="file3",
             label= NULL,
             multiple = FALSE,
             accept = ".FCS",
             width = '250px',
             buttonLabel = "Browse",
             placeholder = "anchor_sam_3"
           )),
    column(6, align="center",
           fileInput(
             inputId="file4",
             label= NULL,
             multiple = FALSE,
             accept = ".FCS",
             width = '250px',
             buttonLabel = "Browse",
             placeholder = "anchor_sam_4"
           ))),
    fluidRow(
      column(6,center="align",
             fileInput(
               inputId="file5",
               label= NULL,
               multiple = FALSE,
               accept = ".FCS",
               width = '250px',
               buttonLabel = "Browse",
               placeholder = "anchor_sam_5"
             )),
      column(6, align="center",
             fileInput(
               inputId="file6",
               label= NULL,
               multiple = FALSE,
               accept = ".FCS",
               width = '250px',
               buttonLabel = "Browse",
               placeholder = "anchor_sam_6"
             )))
  ),
               mainPanel(width=4, 

                align="center", plotOutput("oid2", height = "300px"))
               
               ),
  fluidRow(
    column(2, 
      pickerInput("variable", choices=c(),  options = list(`actions-box` = TRUE), multiple = TRUE)
      ),
column(2,

  dropdown(
             actionButton("action", "untransformed"),
             actionButton("action4", "arcsinh_trans"),

             label = "Preprocess",
             inputId = "dataset_types"
           )
#actionButton(inputId = "action",label = "column_selection")
)
,
    column(2, 

      dropdown(
             actionButton("action2", "KS_batch"),
             actionButton("action3", "UMAP_scatter"),
             actionButton("action5", "clusters_proportion"),

             label = "Batch_viz",
             inputId = "Batch_viz2"
           )

      #actionButton(inputId = "action2",label = "KS_batch")
      ),

  column(2,
  downloadButton('foo')
   ),

column(2,
  dropdown(label = "FILES_INPUT",
             inputId = "files_input",

    fileInput(
    inputId="file90",
    label= NULL,
    multiple = TRUE,
    accept = ".FCS",
    width = '200px',
    buttonLabel = "Browse",
    placeholder = "batch_1"
  ),
    fileInput(
    inputId="file91",
    label= NULL,
    multiple = TRUE,
    accept = ".FCS",
    width = '200px',
    buttonLabel = "Browse",
    placeholder = "batch_2"
  ),

  fileInput(
    inputId="file92",
    label= NULL,
    multiple = TRUE,
    accept = ".FCS",
    width = '200px',
    buttonLabel = "Browse",
    placeholder = "batch_3"
  ),

  fileInput(
    inputId="file93",
    label= NULL,
    multiple = TRUE,
    accept = ".FCS",
    width = '200px',
    buttonLabel = "Browse",
    placeholder = "batch_4"
  ),
  fileInput(
    inputId="file94",
    label= NULL,
    multiple = TRUE,
    accept = ".FCS",
    width = '200px',
    buttonLabel = "Browse",
    placeholder = "batch_5"
  ),

  fileInput(
    inputId="file95",
    label= NULL,
    multiple = TRUE,
    accept = ".FCS",
    width = '200px',
    buttonLabel = "Browse",
    placeholder = "batch_6"
  )







    )
   )



  )
  ,
  
  tableOutput("contents")
  
),

tabPanel(title= "Batch_Adjust",

fluidRow
             (
               column(12,align="center", tags$div(class= "per_channel",
                                                 tags$em(tags$h1("Per_channel Normalization")))),
               
             ), 

sidebarLayout(
sidebarPanel(   

fluidRow
             (
               column(4,align="center", tags$div(class= "parameter",
                                                 tags$em(tags$h4("Parameter_selection"))))),
               


sliderInput("integer1", "Percentile:",
                  min = 1, max = 100,
                  value = 5),

 pickerInput("integer2", label= "parameters" ,choices=c("SD", "mean", "median"),  options = list(`actions-box` = TRUE), multiple = TRUE),

 actionButton("action6", "Batch_Adjust")
     

  ),


mainPanel(textOutput("checking"))





  )





  ),
tabPanel(title= "CytoNorm"),
tabPanel(title= "VAE")

),  
shinyWidgets::setBackgroundColor("lightgreen"),

# tags$head(
#     tags$script(src = "jquery-ui/jquery-ui.min.js"),
#     tags$script("$(function() {
#                     $('.fade').draggable();
#                 })")
#   ),

tags$head(tags$style(HTML("
                          .fade {opacity: 1;
                      transition: opacity .25s ease-in-out;
                      -moz-transition: opacity .25s ease-in-out;
                      }"))),

tags$script(HTML(
    "document.body.style.backgroundColor = 'skyblue';
  
    var x = document.getElementById('action').onclick= function()
    {document.body.style.backgroundColor = 'green'};"
    
#"var x = document.getElementByClassName("fade");
#setTimeout(function(){ x.value = "2 seconds" }, 2000);
#setTimeout(function(){ x.value = "4 seconds" }, 4000);
#setTimeout(function(){ x.value = "6 seconds" }, 6000);
  #"
  ))



)


server<- function(input, output, session)
{output$checking<- renderText({input$integer2})

   options(shiny.maxRequestSize=100*1024^2)
  observe({
  file11 <- input$file1
  print(file11$datapath)
  file.copy(file11$datapath, "to_be_corrected\\Batch_1_anchorstim.fcs", overwrite = TRUE)
  file22 <- input$file2
  print(file22$datapath)
  file.copy(file22$datapath, "to_be_corrected\\Batch_2_anchorstim.fcs", overwrite = TRUE)
  file33 <- input$file3
  print(file33$datapath)
  file.copy(file33$datapath, "to_be_corrected\\Batch_3_anchorstim.fcs", overwrite = TRUE)
  file44 <- input$file4
  print(file44$datapath)
  file.copy(file44$datapath, "to_be_corrected\\Batch_4_anchorstim.fcs", overwrite = TRUE)
  file55 <- input$file5
  print(file55$datapath)
  file.copy(file55$datapath, "to_be_corrected\\Batch_5_anchorstim.fcs", overwrite = TRUE)
  file66 <- input$file6
  print(file66$datapath)
  file.copy(file66$datapath, "to_be_corrected\\Batch_6_anchorstim.fcs", overwrite = TRUE)
  
  onSessionEnded(function() {do.call(file.remove, list(list.files("C:\\Users\\oaona\\app_batch_correction\\www\\to_be_corrected\\", full.names = TRUE)))})
  onSessionEnded(function() {do.call(file.remove, list(list.files("C:\\Users\\oaona\\app_batch_correction\\www\\fcs_untransformed\\", full.names = TRUE)))})
  onSessionEnded(function() {do.call(file.remove, list(list.files("C:\\Users\\oaona\\app_batch_correction\\www\\concat\\", full.names = TRUE)))})
  


  choicelist2<- append(c("SD", "mean", "median"), input$integer1)

  updatePickerInput(session, inputId = "integer2", choices= choicelist2)





    #{system(paste("rm -f", "C:\\Users\\oaona\\app_batch_correction\\www\\to_be_corrected\\*.*"))})
  if (file.exists("to_be_corrected\\Batch_1_anchorstim.fcs"))
  {jj<- "to_be_corrected\\Batch_1_anchorstim.fcs"
  hjk<- flowCore::read.FCS(jj)
  for (i in attributes(colnames(flowCore::exprs(hjk))))
  {jk<- paste(substr(i,1,nchar(i)-1), "S", sep="")}
  choice<- c()
  for (i in jk)
  {jkl<- flowCore::description(hjk)[[i]]
  choice<- append(choice, jkl)}
  choicelist<- c()
  j<- 0
  for (i in choice)
  {j=j+1
  choicelist[[i]]<- j
  }
  updatePickerInput(session, inputId = "variable", choices= choicelist)}
  
  
    
      else
      {choice<- c()
      cc<- c("all", "ball")
      for (i in cc)
      {choice[[i]]<- 2
      }
        
        updatePickerInput(session, inputId = "variable", choices= choice)}
  renderPrint(paste(input$variable, "b", sep=""))
  
  })
  observeEvent(input$action, {print(as.numeric(input$variable))})
  
  
  observeEvent(input$action, {files <- list.files(path="to_be_corrected\\", pattern="*.fcs", full.names=TRUE, recursive=FALSE)
  
  hjk<- "to_be_corrected\\Batch_1_anchorstim.fcs"
  #hjk<- flowCore::read.FCS(jj)
  #rom <- list.files(path = "C:/Users/oaona/app_batch_correction/www/csv_untransformed/", pattern= "*.fcs", full.names = TRUE)
  hjk<- flowCore::read.FCS(hjk)
for (i in attributes(colnames(flowCore::exprs(hjk))))
    {jk<- paste(substr(i,1,nchar(i)-1), "S", sep="")
    
}

choice2<- c()
for (i in jk)
    {jkl<- flowCore::description(hjk)[[i]]
    choice2<- append(choice2, jkl)}
jjk1<- choice2[as.numeric(input$variable)]

#print(jjk1)
   for (file in files)  
   { jjk<- flowCore::exprs(flowCore::read.FCS(file))[ ,as.numeric(input$variable)]
    colnames(jjk)<- jjk1
   
  #meta <- data.frame(name=choice2, desc=paste('this is column',choice2,'from your CSV'))
   meta <- data.frame(name=colnames(jjk), desc=paste('this is column',colnames(jjk),'from your CSV'))
 
  meta$range <- apply(apply(jjk,2,range),2,diff)
  meta$minRange <- apply(jjk,2,min)
  meta$maxRange <- apply(jjk,2,max)
  ff <- new("flowFrame", exprs= data.matrix(jjk), parameters=Biobase::AnnotatedDataFrame(meta))


    

  flowCore::write.FCS(ff, paste("fcs_untransformed\\", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file)), ".fcs", sep= ""))
  }
#print(colnames(jjk))

  })


observeEvent(input$action4, {files <- list.files(path="C:\\Users\\oaona\\app_batch_correction\\www\\to_be_corrected\\", pattern="*.fcs", full.names=TRUE, recursive=FALSE)
  
  hjk<- "C:\\Users\\oaona\\app_batch_correction\\www\\to_be_corrected\\Batch_1_anchorstim.fcs"
  #hjk<- flowCore::read.FCS(jj)
  #rom <- list.files(path = "C:/Users/oaona/app_batch_correction/www/csv_untransformed/", pattern= "*.fcs", full.names = TRUE)
  hjk<- flowCore::read.FCS(hjk)
for (i in attributes(colnames(flowCore::exprs(hjk))))
    {jk<- paste(substr(i,1,nchar(i)-1), "S", sep="")
    
}

choice2<- c()
for (i in jk)
    {jkl<- flowCore::description(hjk)[[i]]
    choice2<- append(choice2, jkl)}
jjk1<- choice2[as.numeric(input$variable)]

print(jjk1)
   for (file in files)  
   { BB<- flowCore::read.FCS(file)
    translist<- flowCore::transformList(colnames(BB), CytoNorm::cytofTransform)
    BB <- flowCore::transform(BB, translist)
    jjk<- flowCore::exprs(BB)[ ,as.numeric(input$variable)]
    colnames(jjk)<- jjk1
   
  #meta <- data.frame(name=choice2, desc=paste('this is column',choice2,'from your CSV'))
   meta <- data.frame(name=colnames(jjk), desc=paste('this is column',colnames(jjk),'from your CSV'))
 
  meta$range <- apply(apply(jjk,2,range),2,diff)
  meta$minRange <- apply(jjk,2,min)
  meta$maxRange <- apply(jjk,2,max)
  ff <- new("flowFrame", exprs= data.matrix(jjk), parameters=Biobase::AnnotatedDataFrame(meta))


    

  flowCore::write.FCS(ff, paste("C:\\Users\\oaona\\app_batch_correction\\www\\fcs_untransformed\\", sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file)), ".fcs", sep= ""))
  }
print(colnames(jjk))

  })




  
  
  observeEvent(input$action2, {
   rom <- list.files(path = "C:/Users/oaona/app_batch_correction/www/fcs_untransformed/", pattern= "*.fcs", full.names = TRUE)
   length<- c()
   for (xyz in rom)
    {yz<- flowCore::read.FCS(xyz)
    kkk<- dim(flowCore::exprs(yz))[1]
    length<- append(length, kkk)}
  kkj<- dim(flowCore::exprs(yz))[2]
  pp<- cumsum(length)
 qq<- pp
 qq<- R.utils::insert(qq, 1, 1)
 qq[1]<- 0


  concatBBT<-c()
  for (jj in rom)
    {BB<- flowCore::read.FCS(jj)
     newBBT<- flowCore::exprs(BB)[ ,c(1:kkj)]
    
    concatBBT<- rbind(concatBBT, newBBT)}


number_of_markers<- dim(concatBBT)[2]
num_of_fcs<- length(qq)-1
kkk<- c()
for (i in c(1:(num_of_fcs-1)))
    {jjj<- c()
     j= c((i+1):num_of_fcs)
     for (q in j)
         {assign(paste("S",i, q, sep=""), c())
         jjj<- append(jjj, paste("S",i, q, sep=""))}
kkk<- append(kkk, jjj)}

distance<- c()
for (i in c(1:(num_of_fcs-1)))
    {j= c((i+1):num_of_fcs)
     for (q in j)
         {for (w in c(1:number_of_markers))
                       {   bbc<- qq[i]+1
                           cnn<- qq[i+1]
                           nta<- qq[q]+1
                           fox<- qq[q+1]
         
                       rommX=concatBBT[c(bbc: cnn), ]
                       rommY= concatBBT[c(nta: fox), ]
                      suppressWarnings(e<- dgof::ks.test(rommX[sample(nrow(rommX), 5000), ][ ,w],
                    rommY[sample(nrow(rommY), 5000), ][ ,w]))
                      attributes(e$statistic)<- NULL
                                assign(paste("S",i, q, sep=""), append(get(paste("S",i, q, sep="")), e$statistic))
        
         
     }
          distance<- cbind(distance, get(paste("S",i, q, sep="")))
     
          }
          }




  jk<-(colnames(concatBBT))
attributes(jk)<-NULL
colnames(distance)<- kkk
rownames(distance)<- jk
xdata<- data.frame(distance)
data.table::setDT(xdata, keep.rownames = "ID")[]
names(xdata)[1] <- "ID"
melted = reshape2::melt(xdata, id.vars = "ID")


plot1= ggplot2::ggplot(data=melted, ggplot2::aes(x= value, y=ID,group=variable, col= variable))+
ggplot2::geom_point()+
ggplot2::labs(x="KS_Distance", y= "Markers", title="KS_Distance for each marker", fill= "Batches")+
ggplot2::guides(fill=ggplot2::guide_legend(title="Batches"))+
ggplot2::theme(legend.title = ggplot2::element_text(color = "blue", size = 10))+
ggplot2::theme(panel.background = ggplot2::element_rect(fill= "grey"))+
ggplot2::theme(plot.title= ggplot2::element_text(hjust=0.5))

  
output$oid2= renderPlot({
  
plot1


}) 
output$foo = downloadHandler(
      filename = 'test.png',
      content = function(file) {
        ggplot2::ggsave(file, plot = plot1, device = "png")
    }


      )


}

    )
  


observeEvent(input$action5, {
hjk<- "C:\\Users\\oaona\\app_batch_correction\\www\\to_be_corrected\\Batch_1_anchorstim.fcs"
  rom <- list.files(path = "C:/Users/oaona/app_batch_correction/www/fcs_untransformed/", pattern= "*.fcs", full.names = TRUE)
  
length<- c()
   for (xyz in rom)
    {yz<- flowCore::read.FCS(xyz)
    kkk<- dim(flowCore::exprs(yz))[1]
    length<- append(length, kkk)}
  kkj<- dim(flowCore::exprs(yz))[2]
  pp<- cumsum(length)
 qq<- pp
 qq<- R.utils::insert(qq, 1, 1)
 qq[1]<- 0




  concatBBT<-c()
  for (jj in rom)
    {BB<- flowCore::read.FCS(jj)
     newBBT<- flowCore::exprs(BB)[ ,c(1:kkj)]
    
    concatBBT<- rbind(concatBBT, newBBT)}



meta <- data.frame(name=colnames(newBBT),
   desc=paste('this is column',colnames(newBBT),'from your CSV'))


meta$range <- apply(apply(newBBT,2,range),2,diff)
meta$minRange <- apply(newBBT,2,min)
meta$maxRange <- apply(newBBT,2,max)




ff <- new("flowFrame", exprs= data.matrix(concatBBT), parameters=AnnotatedDataFrame(meta))
write.FCS(ff, paste( "C:/Users/oaona/app_batch_correction/www/concat/", "concatBBT1", '.fcs', sep=''))



set.seed(45)
fSOM <- FlowSOM( "C:/Users/oaona/app_batch_correction/www/concat/concatBBT1.fcs",
# Input options:
 compensate = FALSE, transform = FALSE,
  scale = FALSE,
 # SOM options:
 colsToUse = c(1:length(input$variable)), xdim = 10, ydim = 10,
 # Metaclustering options:
 nClus = 10)


 xjx=10
yyy<- fSOM[[2]]
attributes(yyy)<- NULL
for (i in c(1:xjx))
    {assign(paste("yyy", i, sep=""), which(i == yyy))}
vv<- c()
for (x in c(1:xjx))
    {yy<- paste("yyy", x, sep="")
    vv= append(vv, yy)}

pqr<- c()
for (i in c(2:(length(rom)+1)))
    {new<- c()
    kjk<- fSOM[[1]]$map$mapping[ ,1][(qq[i-1]+1):qq[i]]
    kjk<- table(kjk)
    for (p in vv)
                #"yyy9", "yyy10", "yyy11", "yyy12", "yyy13", "yyy14", "yyy15", "yyy16", "yyy17", "yyy18", "yyy19", "yyy20"))
            {yk<- kjk[get(p)]
            attributes(yk)<- NULL
            yk[is.na(yk)] <- 0
            yk<- sum(yk)
            new<- append(new, yk)}
    #new[is.na(new)] <- 0
    new1<- rep(c(sum(new)), times = xjx)  
    new2<- (new/new1)*100
    pqr<- rbind(pqr, new2)}  

#generate the labels for proportion
batch_name<- c()
for (batchsname in rom)
 {blm<- substring(basename(batchsname), 1, 7)
  batch_name<- append(batch_name, blm)}
rownames(pqr)<- NULL
rownames(pqr)<- batch_name


xdata<- pqr

xdata<- t(xdata)
xdata<- data.frame(xdata)
xdata<-cbind(id<- c(1:10), xdata)
names(xdata)[1] <- "ID"
melted = reshape2::melt(xdata, id.vars = "ID")


plot2= ggplot2::ggplot(data=melted, aes(x= ID, y=value,group=variable, col= variable))+
geom_line()+
labs(x="proportions", y= "cluster_number", title="Proportion of each cluster", fill= "Batches")+
guides(fill=guide_legend(title="Batches"))+
theme(legend.title = element_text(color = "blue", size = 10))+
theme(panel.background = element_rect(fill= "grey"))+
theme(plot.title= element_text(hjust=0.5))+
scale_y_continuous(labels = function(x) paste0(x, "%"))



output$oid2= renderPlot({
  
plot2


}) 
output$foo = downloadHandler(
      filename = 'test.png',
      content = function(file) {
        ggplot2::ggsave(file, plot = plot2, device = "png")
    }


      )



  })  
 }


shinyApp(ui= ui, server=server)
