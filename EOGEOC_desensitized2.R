
library(stringr)
library(readxl)
library(dplyr)
library(RODBC)
library(odbc)
library(ggplot2)
library(scales)
library(ggthemes)
library(DT)
library(condformat)

################EOC#####################
EOC_raw <- read_excel('>>>660_MILESTONES_EOC_SU18_FULTON_COUNTY_DATA_MAIN_20180716.xlsx')
EOC <- EOC_raw[c(1,5,9:10,12,40,47,84,88,89,94,109)]


################# Last Year #####################

EOG_lastyr <- read_excel('>>>660_MILESTONES_EOG_SP18_FULTON_COUNTY_DATA_MAIN_20180615.xlsx')
EOGlyColsrem <- EOG_lastyr[c(1,5,7,9,10,13,18,40,41,76,196,197,202,207,229,230,247,248,265,266,283,284,290)]


con <- dbConnect(odbc::odbc(), .connection_string = "driver={SQL Server};server={SERVERNAME}; Database={DBNAME}")


studem1718 <- dbGetQuery(con,"SELECT [SISStudentIdentifier]
                     ,[GeorgiaTestingId]
                     ,[GenderCode]
                     ,[GradeLevel]
                     ,[FederalEthnicityCode]
                     ,[ELLCode]
                     ,[SpecialEdFlag]
                     ,CASE 
                     WHEN FreeReducedMealCode = 'F' THEN 'Y'
                     WHEN FreeReducedMealCode = 'R' THEN 'Y'
                     WHEN FreeReducedMealCode = 'N' THEN 'N'
                     END AS 'FreeReducedMealCode'
                     ,[DistrictFAYFlag]     
                     FROM [FCS EIM CIM].[edw].[vwRptStudentDemographics_LastByYear]
                     WHERE SchoolYear = '2017-2018'
                     ORDER BY SISStudentIdentifier")
studemdedup1718 <- studem1718 %>% distinct(SISStudentIdentifier, .keep_all = TRUE)



EOGlyTest_raw <- merge(EOGlyColsrem,studemdedup1718,by.x = "GTID_RPT", by.y = "GeorgiaTestingId",all.x = TRUE,all.y=FALSE)
EOGlyMatched <- EOGlyTest_raw %>% filter(is.na(SISStudentIdentifier) == FALSE)
EOGlyUnmatched <- EOGlyTest_raw %>% filter(is.na(SISStudentIdentifier)==TRUE) %>% select(c(1:23))
EOGlyMatchedPass2 <- merge(EOGlyUnmatched, studemdedup1718, by.x = "GTID_RPT", by.y = "SISStudentIdentifier", all.x=TRUE, all.y=FALSE)
colnames(EOGlyMatchedPass2)[24] <- "SISStudentIdentifier"
EOGlyUnmatched <- EOGlyMatchedPass2 %>% filter(is.na(SISStudentIdentifier)==TRUE) %>% select(c(1:31))
EOGlyMatchedPass2 <- EOGlyMatchedPass2 %>% filter(is.na(SISStudentIdentifier) == FALSE)
EOGly <- rbind(EOGlyMatched,EOGlyMatchedPass2,EOGlyUnmatched)


EOGlyELA <- EOGlyTest_raw[c(1:10,13:14,21:31,11:12)]
EOGlyELA$Subject <- "ELA"
EOGlyMath <- EOGlyTest_raw[c(1:10,13:14,21:31,15:16)]
EOGlyMath$Subject <- "Math"
EOGlySci <- EOGlyTest_raw[c(1:10,13:14,21:31,17:18)]
EOGlySci$Subject <- "Sci"
EOGlySS <- EOGlyTest_raw[c(1:10,13:14,21:31,19:20)]
EOGlySS$Subject <- "SS"
newcolnames <- c("GTID_RPT","SchCode_RPT"    ,       "SchoolName", "StuLastName_RPT",     "StuFirstName_RPT"   ,  "TestedGrade_RPT"   ,   "EthnicityRace_RPT" ,  
                 "RetestFlag_ELA"  ,     "RetestFlag_Math" ,     "SWDFlag"   ,           "Lexile"    ,          "ReadingStatus" ,       "TestAdmin"   ,        
                 "TestDate"  ,           "StudentID_DRCUse"  ,   "SISStudentIdentifier", "GenderCode"   ,        "GradeLevel"   ,        "FederalEthnicityCode",
                 "ELLCode"          ,    "SpecialEdFlag"     ,   "FreeReducedMealCode" , "DistrictFAYFlag"  ,    "SS" ,              "ACHLevel" ,       
                 "Subject" )
names(EOGlyELA) <- newcolnames
names(EOGlyMath) <- newcolnames
names(EOGlySci) <- newcolnames
names(EOGlySS) <-newcolnames
EOGly <-rbind(EOGlyELA,EOGlyMath,EOGlySci,EOGlySS)
EOGly <- EOGly[!is.na(EOGly$SS),]



#####dedup
EOGly <- EOGly[order(EOGly$Subject, -EOGly$SS),]
EOGly <- EOGly %>% distinct(Subject, GTID_RPT, .keep_all = TRUE)


EOGly$ACHLevel <- as.factor(EOGly$ACHLevel)
EOGly$SchoolName <- as.factor(EOGly$SchoolName)

SCHNamesly <- unique(sort(EOGly$SchoolName))
GradeLevels <- as.factor(unique(sort(EOGly$TestedGrade_RPT)))
TestPeriods <- sort(unique(EOG$TestAdmin))
Summaryly <- EOGly %>% group_by(SchoolName, Subject, TestedGrade_RPT) %>% 
  summarise(Beginning = round(100*sum(ACHLevel=='1')/n(),2), Developing = round(100*sum(ACHLevel=='2')/n(),2), 
            Proficient = round(100*sum(ACHLevel=='3')/n(),2), Advanced = round(100*sum(ACHLevel=='4')/n(),2))   
Summaryly$SchoolName <- as.factor(Summary$SchoolName)
Summaryly$Subject <- as.factor(Summary$Subject)


write.csv(EOGly, file = "EOGly.csv")



#################EOG####################
EOGly <- read.csv('EOGly.csv')
EOG_raw <- read_excel('>>>660_MILESTONES_EOG_SP19_FULTON_COUNTY_DATA_MAIN_20190425.xlsx')
EOGColsrem <- EOG_raw[c(1,5,7,9,10,13,18,40,41,77,201,202,207,212,234,235,252,253,270,271,288,289,295)]

con <- dbConnect(odbc::odbc(), .connection_string = "driver={SQL Server};server={SERVERNAME}; Database={DBNAME}")


studem <- dbGetQuery(con,"SELECT [SISStudentIdentifier]
                     ,[GeorgiaTestingId]
                     ,[GenderCode]
                     ,[GradeLevel]
                     ,[FederalEthnicityCode]
                     ,[ELLCode]
                     ,[SpecialEdFlag]
                     ,CASE 
                     WHEN FreeReducedMealCode = 'F' THEN 'Y'
                     WHEN FreeReducedMealCode = 'R' THEN 'Y'
                     WHEN FreeReducedMealCode = 'N' THEN 'N'
                     END AS 'FreeReducedMealCode'
                     ,[DistrictFAYFlag]     
                     FROM [FCS EIM CIM].[edw].[vwRptStudentDemographics_LastByYear]
                     WHERE SchoolYear = '2018-2019'
                     ORDER BY SISStudentIdentifier")
studemdedup <- studem %>% distinct(SISStudentIdentifier, .keep_all = TRUE)
EOGTest_raw <- merge(EOGColsrem,studemdedup,by.x = "GTID_RPT", by.y = "GeorgiaTestingId",all.x = TRUE,all.y=FALSE)

########Code in case things don't all match...
#EOGMatched <- EOGTest_raw %>% filter(is.na(SISStudentIdentifier) == FALSE)
#EOGUnmatched <- EOGTest_raw %>% filter(is.na(SISStudentIdentifier)==TRUE) %>% select(c(1:18))
#EOGMatchedPass2 <- merge(EOGUnmatched, studemdedup, by.x = "GTID_RPT", by.y = "SISStudentIdentifier", all.x=TRUE, all.y=FALSE)
#colnames(EOGMatchedPass2)[19] <- "SISStudentIdentifier"
#EOGUnmatched <- EOGMatchedPass2 %>% filter(is.na(SISStudentIdentifier)==TRUE) %>% select(c(1:18))
#EOGMatchedPass2 <- EOGMatchedPass2 %>% filter(is.na(SISStudentIdentifier) == FALSE)
#EOG <- rbind(EOGMatched,EOGMatchedPass2)


##Break lists apart and rbind
EOGELA <- EOGTest_raw[c(1:10,13:14,21:31,11:12)]
EOGELA$Subject <- "ELA"
EOGMath <- EOGTest_raw[c(1:10,13:14,21:31,15:16)]
EOGMath$Subject <- "Math"
EOGSci <- EOGTest_raw[c(1:10,13:14,21:31,17:18)]
EOGSci$Subject <- "Sci"
EOGSS <- EOGTest_raw[c(1:10,13:14,21:31,19:20)]
EOGSS$Subject <- "SS"
newcolnames <- c("GTID_RPT","SchCode_RPT"    ,       "SchoolName", "StuLastName_RPT",     "StuFirstName_RPT"   ,  "TestedGrade_RPT"   ,   "EthnicityRace_RPT" ,  
                 "RetestFlag_ELA"  ,     "RetestFlag_Math" ,     "SWDFlag"   ,           "Lexile"    ,          "ReadingStatus" ,       "TestAdmin"   ,        
                 "TestDate"  ,           "StudentID_DRCUse"  ,   "SISStudentIdentifier", "GenderCode"   ,        "GradeLevel"   ,        "FederalEthnicityCode",
                 "ELLCode"          ,    "SpecialEdFlag"     ,   "FreeReducedMealCode" , "DistrictFAYFlag"  ,    "SS" ,              "ACHLevel" ,       
                 "Subject" )
names(EOGELA) <- newcolnames
names(EOGMath) <- newcolnames
names(EOGSci) <- newcolnames
names(EOGSS) <-newcolnames
EOG <-rbind(EOGELA,EOGMath,EOGSci,EOGSS,EOGly)
EOG <- EOG[!is.na(EOG$SS),]
EOG$ACHLevel <- as.factor(EOG$ACHLevel)
EOG$SchoolName <- as.factor(EOG$SchoolName)

SCHNames <- unique(sort(EOG$SchoolName))
GradeLevels <- as.factor(unique(sort(EOG$TestedGrade_RPT)))
TestPeriods <- sort(unique(EOG$TestAdmin))
Summary <- EOG %>% group_by(SchoolName, SchCode_RPT, TestAdmin, Subject, TestedGrade_RPT) %>% 
  summarise(TotalTests = n(), NumBeg = sum(ACHLevel == '1'),Beginning = round(100*sum(ACHLevel=='1')/n(),2), NumDev = sum(ACHLevel == '2'),
            Developing = round(100*sum(ACHLevel=='2')/n(),2), NumProf = sum(ACHLevel == '3'),
            Proficient = round(100*sum(ACHLevel=='3')/n(),2), NumAdv = sum(ACHLevel == '4'),
            Advanced = round(100*sum(ACHLevel=='4')/n(),2))

Summary$SchoolName <- as.factor(Summary$SchoolName)
Summary$Subject <- as.factor(Summary$Subject)

Summarytemp <- Summary %>% mutate(ProficientPlus = NumProf + NumAdv, 
                                  PercentProfPlus = round(100*(NumProf + NumAdv)/TotalTests,2),
                                  DevPlus = NumProf +NumDev + NumAdv,
                                  PercentDevPlus = round(100 *(NumProf + NumDev +NumAdv)/TotalTests,2)) %>%
  select(-NumBeg,-Beginning,-NumDev,-Developing,-NumProf,-Proficient,-NumAdv,-Advanced)
Summaryly <- Summarytemp %>% filter(TestAdmin == 'Spring 2018')
Summarycy <- Summarytemp %>% filter(TestAdmin == 'Spring 2019')
Summary2 <- merge(Summaryly,Summarycy,by = c("SchCode_RPT","Subject","TestedGrade_RPT"),all.x = TRUE,all.y=TRUE)
Summary2 <- Summary2 %>% select(-SchCode_RPT,-TestAdmin.x, -SchoolName.y,-TestAdmin.y)
colnames(Summary2) <- c( "Subject","Grade","SchoolName", "TotalTests.2018","ProficientPlus.2018", 
                         "PercentProfPlus.2018", "DevPlus.2018","PercentDevPlus.2018",  "TotalTests.2019",  "ProficientPlus.2019", 
                         "PercentProfPlus.2019", "DevPlus.2019",  "PercentDevPlus.2019" )
Summary2 <- Summary2 %>% mutate(DiffDevPlus = PercentDevPlus.2019 - PercentDevPlus.2018, 
                                DiffProfPlus = PercentProfPlus.2019 - PercentProfPlus.2018,DiffTests = TotalTests.2019 - TotalTests.2018)
Summary2 <- Summary2 %>% select(c(3,1,2,4,9,16,8,13,14,6,11,15))
Summary2 <- Summary2[!is.na(Summary2$SchoolName),]
Summary2$Grade <- as.factor(Summary2$Grade)



library(shiny)
library(ggplot2)
library(DT)
library(formattable)

# Define UI for application that draws a stacked bar chart
ui <- fluidPage( titlePanel("Milestones Preliminary Data - EMBARGOED"),
                 fluidRow(
                   selectInput("subject","Select a Subject",c("ELA","Math","Sci","SS"),selected = 'Sci', 
                               multiple = FALSE, selectize = FALSE, width = NULL, size = NULL ), 
                   selectInput("School","Select a School",SCHNames,selected = 'ABBOTTS HILL ELEMENTARY SCHOOL', 
                               multiple = TRUE, selectize = FALSE, width = NULL, size = NULL ),
                   #selectInput("Grade", "Select a Grade", GradeLevels, selected = NULL,multiple = FALSE, 
                   #            selectize = FALSE, width = NULL, size = NULL ), 
                   plotOutput("data"),
                   tabsetPanel(position = "below",
                               tabPanel("Comparison", DT::dataTableOutput("table2")),
                               tabPanel("Raw",tableOutput("table"))
                   )))

# Define server logic required to draw a stacked bar chart
server <- function(input, output, session) {
  output$data <- renderPlot({
    test <- ggplot(data = EOG[EOG$SchoolName == input$School & EOG$Subject == input$subject ,]) +
      geom_bar(aes(x = TestAdmin, fill = factor(ACHLevel, levels = c("4","3","2","1")), stat = 'identity'), position = 'fill') + 
      scale_y_continuous(labels = scales::percent)  +
      facet_grid(.~as.factor(TestedGrade_RPT)) 
    plot(test)})
  
  
  output$table <- renderTable(Summary[Summary$SchoolName == input$School & Summary$Subject == input$subject,])
  
  #output$table2 <- renderTable(Summary2[Summary2$SchoolName == input$School & Summary2$Subject == input$subject,])
  
  
  output$table2 = DT::renderDT(
    datatable(Summary2) %>% formatStyle(c(1:12),textAlign = 'center') %>%
      formatStyle('DiffTests', backgroundColor = styleInterval(0, c('#EF5350','lightgreen')), fontWeight = 'bold') %>% 
      formatStyle('DiffDevPlus', backgroundColor = styleInterval(0,c('#EF5350','lightgreen')), fontWeight = 'bold') %>%
      formatStyle('DiffProfPlus', backgroundColor = styleInterval(0,c('#EF5350','lightgreen')), fontWeight = 'bold') %>%
      formatRound(columns = c(7:12),digits = 2)
  )
  
  proxy = dataTableProxy('table2')
  observeEvent(input$School, {
    proxy %>% updateSearch(keywords = list(global = paste(input$School, " ",input$subject))) })
  
}
# Run the application 
shinyApp(ui = ui, server = server)