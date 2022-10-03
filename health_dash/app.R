# load libraries

library(shiny)
library(survival)
library(plyr)
library(survminer)
library(dplyr)
library(table1)
library(ggalluvial)

#load data

data("nafld1")
data("rats")


#Create app layout 

ui <- fluidPage(
  
  
  titlePanel("Health Dashboard Examples"),
  
  navlistPanel(
    "Table 1",
    tabPanel("NAFLD", 
             tags$h4("Table 1 Demographic Information for NAFLD Cohort"), 
             tableOutput("table"), 
             tags$h6("Data sets containing the data from a population study 
                     of non-alcoholic fatty liver disease (NAFLD). Subjects with 
                     the condition and a set of matched control subjects were 
                     followed forward for metabolic conditions, cardiac 
                     endpoints, and death. Source: Allen 2018")),
    "Survival Curves",
    tabPanel("Tumors in Rats", 
             tags$h4("Survival Analysis: Tumor Incidence in Treated and
                     Untreated Rats"), 
             selectInput(inputId = "groupselector",label="Treatment Group", 
                         choices=c("No Treatment" = 0, "Treatment" = 1)), 
             plotOutput("p1"), tags$h6("Rat treatment data from Mantel et al. 
                                       Three rats were chosen from each of 100 
                                       litters, one of which was treated with a 
                                       drug, and then all followed for tumor 
                                       incidence. Source: N. Mantel, N. R. 
                                       Bohidar and J. L. Ciminera. 
                                       Mantel-Haenszel analyses of litter-matched
                                       time to response data, with modifications 
                                       for recovery of interlitter information. 
                                       Cancer Research, 37:3863-3868, 1977.")),
    "Treatment Switching",
    tabPanel("Depression Treatment Sankey", 
             tags$h4("Example Sankey Diagram For Treatment Switching"),
             plotOutput("p2"), 
             tags$h6("The data in this example was fabricated for illustration 
                     purposes. This diagram was generated using the ggalluvial 
                     package: http://corybrunson.github.io/ggalluvial/"))
  )
  
  
)


# Define server logic

server <- function(input, output){
  
  #Rats data and plot
  groupchoices=unique(rats$rx)
  
  filter=reactive({
    rats=rats[rats$rx==input$groupselector,]
    rats['rx'] = ifelse(rats$rx==input$groupselector,
                        input$groupselector,
                        "Others")
    return(rats)
  })
  
  
  output$p1=renderPlot({
    fit=survfit(Surv(time, status)~rx + sex, data = filter())
    ggsurvplot(fit,data=filter(), color = "sex", pval=TRUE,
               xlim=c(0,max(filter()$time)+1),
               title=paste("Survival Plot:",
                           ifelse(input$groupselector== "0", "No Treatment", 
                                  "Treatment")), 
               xlab="Time (Days)",
               legend.labs=c("Female","Male"))
  })
  
  #NAFLD data and table
  nafld_clean <- nafld1 %>%
    mutate(Sex = as.factor(case_when(male == 0 ~ "Female",
                                     male == 1 ~ "Male")),
           Weight = weight, 
           Height = height,
           Status = case_when(status == 1 ~ "Dead", 
                              status == 0 ~ "Alive"))
  output$table <-  renderTable({
    table1(~ Sex + Weight + Height | Status, data=nafld_clean)})
  
  #Sankey data and plot
  set.seed(1)
  patient <- as.character(rep(1:100,each=5))
  time <- rep(c(0, 6, 12, 18, 24), 100)
  Drug <- factor(sample(c("Escitalopram", "Citalopram", "Sertraline",  
                          "Fluvoxamine","Missing"), 100, replace=T))
  df <- data.frame(patient, time, Drug)
  
  
  output$p2=renderPlot({
    ggplot(df, aes(x = time, stratum = Drug, alluvium = patient, 
                   fill = Drug, label = Drug)) +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      geom_flow(stat = "alluvium", lode.guidance = "rightleft", 
                color = "darkgray") +
      geom_stratum() +
      scale_x_discrete(name ="Months", 
                       limits=c(0, 6, 12, 18, 24)) +
      ylab("Number of Patients") +
      theme(legend.position = "bottom") +
      ggtitle("Treatment Switching Patterns over 24 Months")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
