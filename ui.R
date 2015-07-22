library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Total Quality Points for CAHPS" = "CAHPS_score",
  "Total Number of Assigned Beneficiaries" = "benes",
  "Total Benchmark Expenditures" = "benchmark_exp",
  "Total Benchmark Expenditures Minus Total Assigned Beneficiary Expenditures" = "bench_minus_assign_bene_exp",
  "Patient/Caregiver Expiernce: Getting Timely Care and Appointments" = "ACO.1",
  "Patient/Caregiver Expiernce: How Well Your Doctors Communicate" = "ACO.2",
  "Patient/Caregiver Expiernce: Patients' Rating of Doctor" = "ACO.3",
  "Patient/Caregiver Expiernce: Access to Specialists" = "ACO.4",
  "Patient/Caregiver Expiernce: Health Promotion and Education" = "ACO.5",
  "Patient/Caregiver Expiernce: Shared Decision Making" = "ACO.6",
  "Patient/Caregiver Expiernce: Health and Functional Status" = "ACO.7",  
  "Risk Standardized, All Condition Readmissions" = "ACO.8.",
  "COPD or Asthma in Older Adults " = "ACO.9.",
  "Heart Failure" = "ACO.10.",
  "Percent of PCPs who Qualified for EHR Incentive Payment" = "ACO.11",
  "Medication Reconciliation" = "ACO.12",
  "Falls: Screening for Fall Risk" = "ACO.13",
  "Influenza Immunization" = "ACO.14",
  "Pneumococcal Vaccination" = "ACO.15",
  "Adult Weight Screening and Follow-up" = "ACO.16",
  "Tobacco Use Assessment and Cessation Intervention" = "ACO.17",
  "Depression Screening" = "ACO.18",
  "Colorectal Cancer Screening" = "ACO.19",
  "Mammography Screening" = "ACO.20",
  "Proportion of Adults who had blood pressure screened in past 2 years" = "ACO.21",
  "Diabetes Control: Hemoglobin A1c under control" = "ACO.22",
  "Diabetes Control: Low Density Lipoprotein (LDL) under control" = "ACO.23",
  "Diabetes Control: Blood Pressure under control" = "ACO.24",
  "Diabetes Control: Tobacco Non-Use" = "ACO.25",
  "Diabetes Control: Aspirin" = "ACO.26",
  "Percent of beneficiaries with diabetes whose HbA1c in poor control" = "ACO.27.",
  "Percent of beneficiaries with hypertension whose BP < 140/90" = "ACO.28",
  "Percent of beneficiaries with IVD with complete lipid profile and LDL control < 100 mg/dl" = "ACO.29",
  "Percent of beneficiaries with IVD who use Aspirin or other antithrombotic" = "ACO.30",
  "Beta-Blocker Therapy for LVSD" = "ACO.31"
)

shinyUI(navbarPage("Year 1 ACO CAHPS and Clinical Scores", id="nav",
                   
 tabPanel("Interactive ACO Map",
  div(class="outer",
                        
    tags$head(
      # Include our custom CSS
      includeCSS("styles.css"),
      includeScript("gomap.js")
    ),
                              
    leafletOutput("map", width="100%", height="100%"),
                                
    # Shiny versions prior to 0.11 should use class="modal" instead.
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
      width = 330, height = "auto",
                              
      h2("ACO Quality Explorer"),
                                
      selectInput("color", "Color", vars, selected = "CAHPS_score"),
      selectInput("size", "Size", vars, selected = "benes"),
      conditionalPanel("input.color == 'benes' || input.size == 'benes'"
       # Only prompt for threshold when coloring or sizing by superzip
       #  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
       )
                                              
#          plotOutput("histCentile", height = 200),
#          plotOutput("scatterCollegeIncome", height = 250)
       ),
                              
      tags$div(id="cite",
       'Data compiled for Medicare Shared Savings Program Accountable Care Organizations Performance Year 1 Results.'
       )
      )
   )
))