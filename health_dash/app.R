# Health Dashboard
#
# A small Shiny app showcasing three common epidemiological figures:
#   1. A "Table 1" demographic summary (NAFLD cohort)
#   2. Kaplan-Meier survival curves (rat tumour-incidence study)
#   3. A Sankey / alluvial diagram of treatment switching (fabricated data)

# ---- Libraries -------------------------------------------------------------

library(shiny)
library(bslib)      # modern Bootstrap 5 theming, cards, layouts
library(survival)   # nafld1, rats datasets + Surv()/survfit()
library(survminer)  # ggsurvplot()
library(dplyr)      # data wrangling
library(table1)     # table1() demographic tables
library(ggplot2)    # plotting (also pulled in by survminer)
library(ggalluvial) # alluvial / Sankey-style diagrams

# ---- Look & feel -----------------------------------------------------------

# A cohesive, professional colour palette reused across the whole app.
palette_primary   <- "#2c5f8a"  # muted slate blue
palette_accent    <- "#d1495b"  # warm red for contrast
palette_navbar_bg <- "#1c2733"  # dark slate for the navbar
palette_muted     <- "#b0b7be"  # neutral grey

# Drug colours for the alluvial diagram (curated rather than a stock palette).
drug_palette <- c(
  Escitalopram = "#1f4e79",
  Citalopram   = "#2e75b6",
  Sertraline   = "#2e9e8f",
  Fluvoxamine  = "#e0a458",
  Missing      = palette_muted
)

# Bootstrap 5 theme: clean system font stack, consistent brand colours.
app_theme <- bs_theme(
  version    = 5,
  bg         = "#ffffff",
  fg         = "#1c2733",
  primary    = palette_primary,
  secondary  = "#5a7184",
  base_font  = font_collection("system-ui", "Segoe UI",
                               "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("system-ui", "Segoe UI",
                                 "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "0.5rem"
)

# A single ggplot theme so every figure shares the same typography & spacing.
# Base size is generous because the plots render across a wide canvas, where
# smaller fonts look cramped.
app_ggtheme <- theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(face = "bold", size = 19,
                                 margin = margin(b = 8)),
    plot.subtitle = element_text(colour = "#5a7184", size = 14,
                                 margin = margin(b = 12)),
    axis.title    = element_text(colour = "#3a4a59", size = 15),
    axis.text     = element_text(size = 13),
    legend.position = "bottom",
    legend.title  = element_text(face = "bold", size = 14),
    legend.text   = element_text(size = 13),
    panel.grid.minor = element_blank()
  )
theme_set(app_ggtheme)

# ---- Data ------------------------------------------------------------------

data("nafld1")
data("rats")

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

ui <- page_navbar(
  title       = "Health Analytics Dashboard",
  window_title = "Health Analytics Dashboard",
  theme       = app_theme,
  bg          = palette_navbar_bg,
  underline   = TRUE,

  # 1. Demographics ---------------------------------------------------------
  nav_panel(
    title = "Demographics",
    icon  = icon("table"),
    card(
      card_header(
        "Table 1 — Demographic summary, NAFLD cohort",
        class = "fw-semibold"
      ),
      card_body(
        div(class = "table-responsive", htmlOutput("table"))
      ),
      card_footer(
        class = "text-muted small",
        "Population study of non-alcoholic fatty liver disease (NAFLD). ",
        "Subjects with the condition and matched controls were followed ",
        "forward for metabolic conditions, cardiac endpoints, and death. ",
        tags$em("Source: Allen, 2018.")
      )
    )
  ),

  # 2. Survival curves ------------------------------------------------------
  nav_panel(
    title = "Survival Curves",
    icon  = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Controls",
        selectInput(
          inputId = "groupselector",
          label   = "Treatment group",
          choices = c("No Treatment" = "0", "Treatment" = "1")
        ),
        helpText(
          "Kaplan-Meier tumour-free survival, split by sex, re-fit for the ",
          "selected treatment group."
        )
      ),
      card(
        card_header(
          "Survival analysis — tumour incidence in rats",
          class = "fw-semibold"
        ),
        card_body(plotOutput("p1", height = "520px")),
        card_footer(
          class = "text-muted small",
          "Three rats were chosen from each of 100 litters, one treated ",
          "with a drug, then all followed for tumour incidence. ",
          tags$em("Source: Mantel, Bohidar & Ciminera, Cancer Research, ",
                  "37:3863-3868, 1977.")
        )
      )
    )
  ),

  # 3. Treatment switching --------------------------------------------------
  nav_panel(
    title = "Treatment Switching",
    icon  = icon("shuffle"),
    card(
      card_header(
        "Treatment switching — antidepressant medications",
        class = "fw-semibold"
      ),
      card_body(plotOutput("p2", height = "520px")),
      card_footer(
        class = "text-muted small",
        "Illustrative Sankey / alluvial diagram built with the ",
        tags$a(href = "http://corybrunson.github.io/ggalluvial/",
               target = "_blank", "ggalluvial"),
        " package. ", tags$em("Data fabricated for demonstration only.")
      )
    )
  ),

  nav_spacer(),
  nav_item(
    tags$span(class = "navbar-text small text-light opacity-75",
              "Epidemiology & clinical-research examples")
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
      palette     = c(palette_accent, palette_primary),
      xlim        = c(0, max(dat$time) + 1),
      title       = paste("Tumour-free survival —", group_label),
      subtitle    = "Kaplan-Meier estimate with 95% confidence intervals",
      xlab        = "Time (days)",
      ylab        = "Tumour-free probability",
      legend.labs = c("Female", "Male"),
      legend.title = "Sex",
      pval.size   = 6,
      fontsize    = 4.5,
      risk.table.height = 0.25,
      risk.table.fontsize = 4.5,
      risk.table.title = "Number at risk",
      ggtheme     = app_ggtheme,
      tables.theme = theme_cleantable(base_size = 14)
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
      scale_fill_manual(values = drug_palette) +
      geom_flow(stat = "alluvium", lode.guidance = "rightleft",
                color = "white", linewidth = 0.2, alpha = 0.75) +
      geom_stratum(alpha = 0.95, color = "white", linewidth = 0.3) +
      labs(x = "Months", y = "Number of patients",
           title = "Treatment switching patterns over 24 months",
           subtitle = "Antidepressant medication by visit (fabricated data)",
           fill = "Drug")
  })
}

# ---- Run -------------------------------------------------------------------

shinyApp(ui = ui, server = server)
