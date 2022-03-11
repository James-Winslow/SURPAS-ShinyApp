### load packages ###
library(tidyverse)
library(shiny)
library(kableExtra)
library(ggplot2)
library(extrafont)
library(gridExtra)
library(shinyjs)
### ### ### ###
### Read in data
### ### ### ###

betas <- read.csv("./Beta99_100_edited.csv", stringsAsFactors = F)
cptdat <- read.csv("./cptFile.csv", stringsAsFactors = F)
namescpt <- read.csv("./namescpt.csv", stringsAsFactors = F)
surgeon_cpt_frequencies <- read.csv("./surgeon_cpt_frequencies.csv", stringsAsFactors = F)
specCPT <- read.csv("./specCPT.csv", stringsAsFactors = F)


### ### ### ### ### ### ### ### ### ### ### ###
### Begin the Shiny application part of the code ###
### ### ### ### ### ### ### ### ### ### ### ###


### ### ### ### The UI creates the html envi which allows the user to
# interact and ultimately pass desired computations to the R server

ui <- fluidPage(
  tags$head(tags$style(HTML('* {font-family: "Arial"}, sans-serif !important;'))),
  shinyjs::useShinyjs(),
  titlePanel("SURPAS: Surgical Risk Preoperative Assessment System"),
  sidebarLayout(
    sidebarPanel(
            selectInput("User Type", label = "User Type",
                  choices = c("Choose"="","Surgeon","Surgical Resident",
                              "Advanced Practice Provider","Nurse",
                              "Other Physician","Patient","Other"),
                  selected = 'Choose'),
      selectInput("User Purpose", label = "Purpose",
                  choices = c("Choose"="","Discussing with Patient","Exploratory","Other"),
                  selected = 'Choose'),   
      selectInput(inputId = "specialtyInput", label = "Surgical Specialty",
                  choices = c("Choose"="","General Surgery","Gynecology","Neurosurgery"
                              ,"Orthopedics","Otolaryngology (ENT)","Plastics"
                              ,"Thoracic","Urology","Vascular"),
                  selected = 'Choose'),
      selectizeInput(inputId = "CPTin", label = "CPT",
                   choices = NULL,
                   ),      
      numericInput(inputId = "AgeInput", label = "Age",
                   value = '',
                   min = 0,
                   max = 99 ,
                   step = 1),
      selectInput(inputId = "fhsInput", label = "FHS",
                  choices = c("Choose"="", "Independent","Partially Dependent"
                              ,"Totally Dependent"),
                  selected = 'Choose'),
      selectInput(inputId = "asaInput", label = "ASA",
                  choices = c("Choose"="","1","2","3","4","5"),
                  selected = 'Choose'),
      selectInput(inputId = "epInput", label = "Emergency",
                  choices = c("Choose"="","Yes","No"),
                  selected = 'Choose'),
      selectInput(inputId = "inoutInput", label = "In/Out-Patient",
                  choices = c("Choose"="","Outpatient","Inpatient"),
                  selected = 'Choose'),     
      # makes a clickable button to submit user response data #
      actionButton("submit","Submit", class = "btn-primary"),
      
      # makes a clickable button that will generate a specified 'report'
      downloadButton("export"),
    width = 3)
  ,
  mainPanel(
    plotOutput("bar1", width = "60%"),
    tableOutput("table1"),
    width = 9)
),

# Where the Footer goes #
hr(),
print("The Surgical Risk Preoperative Assessment System (SURPAS) is a risk 
      assessment tool intended for use by patient care providers to estimate 
      individual patient risk of complications after a surgical procedure. 
      This quantitative preoperative risk assessment is based upon an individual
      patient's values for eight preoperatively available risk factors. 
      These eight preoperatively available risk factors carry nearly all of the 
      prognostic information available in 40 preoperatively available risk 
      factors contained in the American College of Surgeons (ACS) National 
      Surgical Quality Improvement Program (NSQIP) database because many risk 
      factors carry redundant prognostic information. The SURPAS tool contains 
      equations based on analyses of over 4 million surgical procedures 
      contained in the ACS NSQIP database (2005-2015). 
      The information provided on or through this tool does not constitute 
      medical advice, and should not be used for diagnosing or treating a 
      health or medical condition. The information, including but not limited 
      to the results derived from use of this tool, is provided 'as-is' for 
      information purposes only and any use of or reliance on the information 
      is solely at the user's own risk. This risk assessment is only an estimate
      and not exact; individual patient risks may be greater or lower. 
      The Regents of the University of Colorado make no warranties or 
      representations of any kind, express or implied (including those of 
      merchantability or fitness for a particular purpose), as to the accuracy 
      or completeness of the information provided through this tool, and will 
      not be liable for any claim or damage related in any way to any of the 
      information provided by or through this tool, irrespective of cause or 
      legal theory.")
)



### ### ### ### ### ### ### ### ### ### ### ###
### some code below adapted from Dean Attali's article 
### "Mimicking a Google Form with a Shiny app"
### ### ### ### ### ### ### ### ### ### ### ###
### Server part of the app ###
server <- function(input, output,session) {
  
  ### Add section for user data collection ###
  # directory where responses get stored
  responsesDir <-  file.path("./userdata")
  # which fields get saved 
  record <- c("User Type", "User Purpose","specialtyInput","CPTin","AgeInput","fhsInput","asaInput","epInput","inoutInput")
  
  # get a formatted string of the timestamp (exclude colons as they are invalid
  # characters in Windows filenames)
  humanTime <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
  }
  
  # get current Epoch time
  epochTime <- function() {
    return(as.integer(Sys.time()))
  }
  formData <- reactive({
    data <- sapply(record, function(x) input[[x]])
    data <- c(data, timestamp = epochTime(),time = humanTime())
    data <- t(data)
    data
  })

  
  # save the results to a file
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    saveData(formData())
  })
  # # load all responses into a data.frame
  # loadData <- function() {
  #   files <- list.files(file.path(responsesDir), full.names = TRUE)
  #   data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #   data <- dplyr::rbind_all(data)
  #   data <- do.call(rbind, data)
  #   data
  # }
  
  
  
  vals <- reactiveValues(bar1=NULL,table1=NULL)
  # selectize input 
  # in server
  # Barplot
  observe({
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         "CPTin",
                         "CPT",
  choices = c("Choose"="",specCPT[as.character(specCPT$Specialty) == input$specialtyInput,]$procedure),
   selected = 'Choose',
  options = list(placeholder = 'Choose' ,create = TRUE) )
})
  
### ### ### ### ### ### ### ### ### ### ### ###
### Begin code to generate plot ###
### ### ### ### ### ### ### ### ### ### ### ###
  output$bar1 <- renderPlot({
    
  cptdat[is.na(cptdat)] <- 0
  req(input$CPTin)

  proc <- subset(namescpt,namescpt$procedure==as.character(input$CPTin))
  cpt <- proc[,1]

  
  age <- input$AgeInput
  fhs <- input$fhsInput
  asa <- input$asaInput
  specialty <- input$specialtyInput
  ep <- input$epInput
  inout <- input$inoutInput
  
  getcpt <- subset(cptdat, CPT==cpt)
  workrvu <- getcpt[c("WorkRVU")][[1]]
  
  
  
  fhs <- factor(fhs, levels = c("Independent", "Partially Dependent", "Totally Dependent"))
  asa <- factor(asa, levels = c("1", "2", "3", "4", "5"))
  specialty <- factor(specialty, levels = c("General surgery", "Gynecology", "Neurosurgery", "Orthopedics",
                                            "Otolaryngology (ENT)", "Plastics", "Thoracic", "Urology", "Vascular"))
  ep <- factor(ep, levels = c("No", "Yes"))
  inout <- factor(inout, levels = c("Outpatient", "Inpatient"))
  
  dfDth <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("Death30DayRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfOv <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("OverallComplicationRate")][[1]]
                          , table(ep), table(fhs), table(inout), table(specialty))))
  dfResp <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("RespiratoryRate")][[1]]
                            , table(ep), table(fhs), table(inout), table(specialty))))
  dfInf <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("InfectionRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfUti <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("UTIRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfVte <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("VTERate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfCar <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("CardiacRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfRen <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("RenalRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfStr <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("StrokeRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfBld <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("BleedingRate")][[1]]
                           , table(ep), table(fhs), table(inout), table(specialty))))
  dfUR <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("UnplannedreadmissionRate")][[1]]
                          , table(ep), table(fhs), table(inout), table(specialty))))
  dfNH <- as.vector(c(1,c(age, workrvu, table(asa), getcpt[,c("NotHomeRate")][[1]]
                          , table(ep), table(fhs), table(inout), table(specialty))))
  
  
  ### Select betas ###
  
  # Select the correct betas for most outcomes #
  selBetas <- ifelse(getcpt$Frequency >= 100, "beta100", "beta99")
  betas2 <- subset(betas, Freq==selBetas)
  
  # Separate look for bleeding #
  selBlBetas <- ifelse(getcpt$BleedFrequency >= 100, "beta100", "beta99")
  betasBl <- subset(betas, Freq==selBlBetas)
  
  # separate look for unplanned readmission
  selURBetas <- ifelse(getcpt$UplanFrequency >= 100, "beta100", "beta99")
  betasUR <- subset(betas, Freq==selURBetas)
  
  # separate look for not home #
  selNHBetas <- ifelse(getcpt$NotHomeFrequency >= 100, "beta100", "beta99")
  betasNH <- subset(betas, Freq==selNHBetas)
  
  
  ### Fit models ###
  death <- dfDth %*% betas2[,c("Death30Day")]
  deathP <- round((exp(death)/(1+exp(death)))*100,2)
  
  overall <- dfOv %*% betas2[,c("OverallComplication")]
  overallP <- round((exp(overall)/(1+exp(overall)))*100,2)
  
  resp <- dfResp %*% betas2[,c("Respiratory")]
  respP <- round((exp(resp)/(1+exp(resp)))*100,2)
  
  inf <- dfInf %*% betas2[,c("Infection")]
  infP <- round((exp(inf)/(1+exp(inf)))*100,2)
  
  uti <- dfUti %*% betas2[,c("UTI")]
  utiP <- round((exp(uti)/(1+exp(uti)))*100,2)
  
  vte <- dfVte %*% betas2[,c("VTE")]
  vteP <- round((exp(vte)/(1+exp(vte)))*100,2)
  
  cardiac <- dfCar %*% betas2[,c("Cardiac")]
  cardiacP <- round((exp(cardiac)/(1+exp(cardiac)))*100,2)
  
  bleed <- dfBld %*% betasBl[,c("Bleeding")]
  bleedP <- round((exp(bleed)/(1+exp(bleed)))*100,2)
  
  renal <- dfRen %*% betas2[,c("Renal")]
  renalP <- round((exp(renal)/(1+exp(renal)))*100,2)
  
  stroke <- dfStr %*% betas2[,c("Stroke")]
  strokeP <- round((exp(stroke)/(1+exp(stroke)))*100,2)
  
  unre <- dfUR %*% betasUR[,c("UnplannedReadmission")]
  unreP <- round((exp(unre)/(1+exp(unre)))*100,2)
  
  nothome <- dfNH %*% betasNH[,c("NotHome")]
  nothomeP <- round((exp(nothome)/(1+exp(nothome)))*100,2)
  
  allP <- as.data.frame(cbind(c(deathP, overallP, unreP, infP, bleedP, respP,
  utiP, vteP, cardiacP, renalP, strokeP, nothomeP),
  c(getcpt[c(4, 5, 16, 7, 14, 6, 8, 9, 10, 11, 12, 18)])
  )
  )
  colnames(allP) <- c("Individual Patient Risk", "National Average Risk")
  rownames(allP) <- c("Death", "Any", "Unplanned readmission",
                      "Infections", "Bleeding", "Pulmonary", "Urinary",
                      "VTE", "Cardiac", "Renal", "Stroke", "Non-home discharge")
  
  allP$`National Average Risk` <- round(as.numeric(allP$`National Average Risk`), 2)
  
  allP$Complication <- rownames(allP)
  allP$National <- as.numeric(allP$`National Average Risk`)
  allP$Patient <- as.numeric(allP$`Individual Patient Risk`)


positions <- as.vector(rownames(allP))
plot1 <- function(){
      ggplot(data=allP) + 
      geom_bar(aes(x=Complication, y=Patient), stat = "identity", width=0.4,show.legend = TRUE,
               color = "sandybrown", fill = "sandybrown") +
      geom_point(aes(x=Complication, y=National),
                 color = "dodgerblue", size = 3,show.legend = TRUE) +
      geom_line(aes(x=Complication, y=National, group=1,show.legend = TRUE),
                color = "dodgerblue", size = .7) +
      ylab("Risk") +
          labs(x=NULL,
           y = "Risk",
           title = "Complication", subtitle = NULL,
       caption  = "Individual Risk(bar) vs. National Risk(line)")+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot")+ #NEW parameter 
      coord_flip() +
      theme(axis.title.x = element_text(size = rel(1.8))) +
      theme(axis.title.y = element_text(size = rel(1.8))) +
      theme(axis.text.x = element_text(size = rel(1.5))) +
      theme(axis.text.y = element_text(size = rel(1.5))) +
      scale_x_discrete(limits=rev(positions)) +
    theme_classic()
     # base_size = 14,
     # base_family = "Arial" 

}
plot2 <- function(){
      ggplot(data=allP) + 
      geom_bar(aes(x=Complication, y=Patient), stat = "identity", width=0.4,show.legend = TRUE,
               color = "sandybrown", fill = "sandybrown") +
      ylab("Risk") +
          labs(x=NULL,
           y = "Risk",
           title = "Complication",
                  subtitle = NULL,
       caption  = "NOTE: No national risk for this procedure")+
      theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot")+ #NEW parameter 
      coord_flip() +
      theme(axis.title.x = element_text(size = rel(1.8))) +
      theme(axis.title.y = element_text(size = rel(1.8))) +
      theme(axis.text.x = element_text(size = rel(1.5))) +
      theme(axis.text.y = element_text(size = rel(1.5))) +
      scale_x_discrete(limits=rev(positions))+
    theme_classic(
     # base_size = 14,
     # base_family = "Arial"
    ) 
}


if(allP$National!=0){
  bar1 <- plot1()
} else if (allP$National==0){
  bar1 <- plot2()
}

vals$bar1 <- bar1
vals$bar1


  })
  
  
### ### ### ### ### ### ### ### ### ### ### ###
### Begin code to generate table ###
### ### ### ### ### ### ### ### ### ### ### ###
  output$table1 <- renderTable({
  noNA <- cptdat
  cptdat[is.na(cptdat)] <- 0
  req(input$CPTin)

  proc <- subset(namescpt,namescpt$procedure==as.character(input$CPTin))
  cpt <- proc[,1]
  
  age <- input$AgeInput
  fhs <- input$fhsInput
  asa <- input$asaInput
  specialty <- input$specialtyInput
  ep <- input$epInput
  inout <- input$inoutInput
  
  getcpt <- subset(cptdat, CPT==cpt)
  
  
  workrvu <- getcpt[c("WorkRVU")][[1]]
  
  fhs <- factor(fhs,
          levels = c("Independent", "Partially Dependent", "Totally Dependent"))
  asa <- factor(asa,
                levels = c("1", "2", "3", "4", "5"))
  specialty <- factor(specialty,
            levels = c("General surgery", "Gynecology", "Neurosurgery",
                       "Orthopedics","Otolaryngology (ENT)", "Plastics",
                       "Thoracic", "Urology", "Vascular"))
  ep <- factor(ep, levels = c("No", "Yes"))
  inout <- factor(inout, levels = c("Outpatient", "Inpatient"))
  
  dfDth <- as.vector(c(1,c(age, workrvu, table(asa),
           getcpt[,c("Death30DayRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfOv <- as.vector(c(1,c(age, workrvu, table(asa),
           getcpt[,c("OverallComplicationRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfResp <- as.vector(c(1,c(age, workrvu, table(asa),
           getcpt[,c("RespiratoryRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfInf <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("InfectionRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfUti <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("UTIRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfVte <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("VTERate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfCar <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("CardiacRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfRen <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("RenalRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfStr <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("StrokeRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfBld <- as.vector(c(1,c(age, workrvu, table(asa), 
           getcpt[,c("BleedingRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfUR <- as.vector(c(1,c(age, workrvu, table(asa),
           getcpt[,c("UnplannedreadmissionRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  dfNH <- as.vector(c(1,c(age, workrvu, table(asa),
           getcpt[,c("NotHomeRate")][[1]],
           table(ep), table(fhs), table(inout), table(specialty))))
  
  
  ### Select betas ###
  
  # Select the correct betas for most outcomes #
  selBetas <- ifelse(getcpt$Frequency >= 100, "beta100", "beta99")
  betas2 <- subset(betas, Freq==selBetas)
  
  # Separate look for bleeding #
  selBlBetas <- ifelse(getcpt$BleedFrequency >= 100, "beta100", "beta99")
  betasBl <- subset(betas, Freq==selBlBetas)
  
  # separate look for unplanned readmission
  selURBetas <- ifelse(getcpt$UplanFrequency >= 100, "beta100", "beta99")
  betasUR <- subset(betas, Freq==selURBetas)
  
  # separate look for not home #
  selNHBetas <- ifelse(getcpt$NotHomeFrequency >= 100, "beta100", "beta99")
  betasNH <- subset(betas, Freq==selNHBetas)
  
  
  ### Fit models ###
  death <- dfDth %*% betas2[,c("Death30Day")]
  deathP <- round((exp(death)/(1+exp(death)))*100,2)
  
  overall <- dfOv %*% betas2[,c("OverallComplication")]
  overallP <- round((exp(overall)/(1+exp(overall)))*100,2)
  
  resp <- dfResp %*% betas2[,c("Respiratory")]
  respP <- round((exp(resp)/(1+exp(resp)))*100,2)
  
  inf <- dfInf %*% betas2[,c("Infection")]
  infP <- round((exp(inf)/(1+exp(inf)))*100,2)
  
  uti <- dfUti %*% betas2[,c("UTI")]
  utiP <- round((exp(uti)/(1+exp(uti)))*100,2)
  
  vte <- dfVte %*% betas2[,c("VTE")]
  vteP <- round((exp(vte)/(1+exp(vte)))*100,2)
  
  cardiac <- dfCar %*% betas2[,c("Cardiac")]
  cardiacP <- round((exp(cardiac)/(1+exp(cardiac)))*100,2)
  
  bleed <- dfBld %*% betasBl[,c("Bleeding")]
  bleedP <- round((exp(bleed)/(1+exp(bleed)))*100,2)
  
  renal <- dfRen %*% betas2[,c("Renal")]
  renalP <- round((exp(renal)/(1+exp(renal)))*100,2)
  
  stroke <- dfStr %*% betas2[,c("Stroke")]
  strokeP <- round((exp(stroke)/(1+exp(stroke)))*100,2)
  
  unre <- dfUR %*% betasUR[,c("UnplannedReadmission")]
  unreP <- round((exp(unre)/(1+exp(unre)))*100,2)
  
  nothome <- dfNH %*% betasNH[,c("NotHome")]
  nothomeP <- round((exp(nothome)/(1+exp(nothome)))*100,2)
  
  allP <- as.data.frame(cbind(c(deathP, overallP, unreP, infP, bleedP, respP,
        utiP, vteP, cardiacP, renalP, strokeP, nothomeP),
        c(getcpt[c(4, 5, 16, 7, 14, 6, 8,9, 10, 11, 12, 18)])
  )
  )


  
  
  colnames(allP) <- c("Individual Patient Risk", "National Average Risk")
  rownames(allP) <- c("Death", "Any", "Unplanned readmission",
                      "Infections", "Bleeding", "Pulmonary", "Urinary",
                      "VTE", "Cardiac", "Renal", "Stroke", "Non-home discharge")
  

allP$`National Average Risk`<-round(as.numeric(allP$`National Average Risk`),2)
  

  allP$Complication <- rownames(allP)
  allP$National <- as.numeric(allP$`National Average Risk`)
  allP$Patient <- as.numeric(allP$`Individual Patient Risk`)

  table1 <- as.data.frame(allP)

  
  column_order <- c("Complication",
                      "Individual Patient Risk","National Average Risk")
  
  table1 <- table1[,column_order]
    table1$`Individual Patient Risk` <- paste0(table1$`Individual Patient Risk`
                                               ,"%")

if (table1[12,2]=='NA%'){
  table1[12,2] <- substr(table1[12,2],1,2)
}

if (table1[,3]==0){
  table1$`National Average Risk` <- 'NA'
}
    
if (table1[3,3]!='NA'){
    table1$`National Average Risk` <- paste0(table1$`National Average Risk`,"%")  
} 

if (table1[12,3]=='NA%'){
    table1[12,3] <- substr(table1[12,3],1,2)
}
    
custom <- ttheme_default(core = list(fg_params = list(hjust=0,x=0,
       fontsize=11),colhead = list(fg_params=list(hjust=0,x=0))),
          base_size = 11, base_colour = "black",
          base_family = "",parse = FALSE,
          padding = unit(c(2,2), "mm"))
  
  
  vals$table1 <- tableGrob(table1,rows = NULL,
  theme = custom )
  table1



}
  ,  striped = FALSE,
  bordered = TRUE,
  hover = TRUE,
  spacing = "xs",
  width = "60%",
  align = 'l',
  rownames = FALSE,
  colnames = TRUE,
  digits = NULL
)
  
  ## clicking on the export button will generate a pdf file 
  ## containing all grobs
  output$export = downloadHandler(
    filename = function() {"plots.pdf"},
    content = function(file) {
    tempReport <- file.path(tempdir(),"report.Rmd")
    file.copy("report.Rmd",tempReport,overwrite = TRUE)
     pdf(file, onefile = TRUE)
     grid.arrange(vals$bar1,vals$table1,ncol=1)
     dev.off()
    }
  )
}



### nothing comes after this last line of code
shinyApp(ui = ui, server = server)
### the end ###