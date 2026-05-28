# Health Dashboard
#
# A small Shiny app showcasing three common epidemiological figures:
#   1. A "Table 1" demographic summary (NAFLD cohort)
#   2. Kaplan-Meier survival curves (rat tumour-incidence study)
#   3. A Sankey / alluvial diagram of treatment switching (fabricated data)

# ---- Libraries -------------------------------------------------------------

library(shiny)
library(survival)   # nafld1, rats datasets + Surv()/survfit()
library(survminer)  # ggsurvplot()
library(dplyr)      # data wrangling
library(table1)     # table1() demographic tables
library(ggplot2)    # plotting (also pulled in by survminer)
library(ggalluvial) # alluvial / Sankey-style diagrams

# ---- Data ------------------------------------------------------------------

data("nafld1")
data("rats")

# A consistent theme for every ggplot in the app.
theme_set(theme_minimal(base_size = 13))

# NAFLD cohort: derive human-readable labels once, up front.
nafld_clean <- nafld1 %>%
  mutate(
    Sex    = factor(case_when(male == 0 ~ "Female",
                              male == 1 ~ "Male")),
    Weight = weight,
    Height = height,
    Status = factor(case_when(status == 1 ~ "Dead",
                              status == 0 ~ "Alive"))
  )

# Label the table1 columns so the summary reads nicely.
label(nafld_clean$Weight) <- "Weight (kg)"
label(nafld_clean$Height) <- "Height (cm)"

# Fabricated treatment-switching data for the Sankey diagram. Built once
# (with a fixed seed) rather than on every render so the plot is stable.
set.seed(1)
n_patients <- 100
visit_months <- c(0, 6, 12, 18, 24)
drug_levels <- c("Escitalopram", "Citalopram", "Sertraline",
                 "Fluvoxamine", "Missing")
switching <- data.frame(
  patient = as.character(rep(seq_len(n_patients), each = length(visit_months))),
  month   = factor(rep(visit_months, n_patients), levels = visit_months),
  Drug    = factor(sample(drug_levels, n_patients * length(visit_months),
                          replace = TRUE),
                   levels = drug_levels)
)

# ---- UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Health Dashboard Examples"),

  navlistPanel(
    widths = c(3, 9),

    "Table 1",
    tabPanel(
      "NAFLD",
      tags$h4("Table 1: Demographic Information for NAFLD Cohort"),
      htmlOutput("table"),
      tags$h6("Data from a population study of non-alcoholic fatty liver
              disease (NAFLD). Subjects with the condition and a set of
              matched control subjects were followed forward for metabolic
              conditions, cardiac endpoints, and death. Source: Allen 2018.")
    ),

    "Survival Curves",
    tabPanel(
      "Tumors in Rats",
      tags$h4("Survival Analysis: Tumor Incidence in Treated and Untreated Rats"),
      selectInput(
        inputId = "groupselector",
        label   = "Treatment Group",
        choices = c("No Treatment" = "0", "Treatment" = "1")
      ),
      plotOutput("p1", height = "520px"),
      tags$h6("Rat treatment data from Mantel et al. Three rats were chosen
              from each of 100 litters, one of which was treated with a drug,
              and then all followed for tumor incidence. Source: N. Mantel,
              N. R. Bohidar and J. L. Ciminera. Mantel-Haenszel analyses of
              litter-matched time to response data, with modifications for
              recovery of interlitter information. Cancer Research,
              37:3863-3868, 1977.")
    ),

    "Treatment Switching",
    tabPanel(
      "Depression Treatment Sankey",
      tags$h4("Example Sankey Diagram for Treatment Switching"),
      plotOutput("p2", height = "520px"),
      tags$h6("The data in this example was fabricated for illustration
              purposes. This diagram was generated using the ggalluvial
              package: http://corybrunson.github.io/ggalluvial/")
    )
  )
)

# ---- Server ----------------------------------------------------------------

server <- function(input, output) {

  # Rats: keep only the selected treatment group, then compare survival by sex.
  rats_group <- reactive({
    subset(rats, rx == input$groupselector)
  })

  output$p1 <- renderPlot({
    dat <- rats_group()
    fit <- survfit(Surv(time, status) ~ sex, data = dat)
    group_label <- if (input$groupselector == "1") "Treatment" else "No Treatment"

    ggsurvplot(
      fit, data = dat,
      pval        = TRUE,
      conf.int    = TRUE,
      risk.table  = TRUE,
      palette     = c("#E64B35", "#4DBBD5"),
      xlim        = c(0, max(dat$time) + 1),
      title       = paste("Survival Plot:", group_label),
      xlab        = "Time (Days)",
      ylab        = "Tumor-free Probability",
      legend.labs = c("Female", "Male"),
      legend.title = "Sex",
      ggtheme     = theme_minimal(base_size = 13)
    )
  })

  # NAFLD: table1() returns HTML, so render it via renderUI / htmlOutput.
  output$table <- renderUI({
    table1(~ Sex + Weight + Height | Status, data = nafld_clean)
  })

  # Sankey / alluvial diagram of treatment switching.
  output$p2 <- renderPlot({
    ggplot(switching,
           aes(x = month, stratum = Drug, alluvium = patient,
               fill = Drug, label = Drug)) +
      scale_fill_brewer(type = "qual", palette = "Set3") +
      geom_flow(stat = "alluvium", lode.guidance = "rightleft",
                color = "darkgray", alpha = 0.7) +
      geom_stratum(alpha = 0.9) +
      labs(x = "Months", y = "Number of Patients",
           title = "Treatment Switching Patterns over 24 Months",
           fill = "Drug") +
      theme(legend.position = "bottom")
  })
}

# ---- Run -------------------------------------------------------------------

shinyApp(ui = ui, server = server)
