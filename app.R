library(caret)
library(randomForest)
library(shiny)
library(shinyWidgets)
library(bslib)
library(shinyjs)
library(shinydashboard)
library(openxlsx)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  useShinyjs(),
  
  tags$head(tags$style(HTML("
    .main-header { font-weight: bold; color: #2c3e50; margin-bottom: 10px; }
    .compact-slider { margin-bottom: 8px; font-size: 13px; }
    .radio-buttons { margin-bottom: 8px; font-size: 13px; }
    .section-header { margin-top: 15px; margin-bottom: 10px; }
  "))),
  
  titlePanel(
    div(
      icon("hospital-user", class = "fa-2x"),
      span("30-Day Readmission Risk Calculator: Kidney Transplant", style = "margin-left:10px;"),
      style = "display:flex; align-items:center; color:#2c3e50;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(width=4,
                 h4("Recipient Data", class="main-header"),
                 
                 sliderInput("age","Age at Transplant (years)", min=18,max=80,value=50),
                 awesomeRadio("sex","Gender",choices=c("Female","Male"),inline=T,status="primary"),
                 awesomeRadio("diabetes","Diabetes Mellitus",choices=c("No","Yes"),inline=T,status="primary"),
                 
                 h4("Dialysis & Donor", class="main-header"),
                 
                 sliderInput("dialysis_months","Dialysis Duration (months)", min=0,max=240,value=24),
                 awesomeRadio("dialysis_type","Dialysis Modality",
                              choices=c("Predialysis","Hemodialysis","Peritoneal Dialysis"),inline=F,status="primary"),
                 awesomeRadio("transplant_type","Transplant Type",
                              choices=c("Deceased Donor","Living Donor"),inline=T,status="primary"),
                 awesomeRadio("ecd","Expanded Criteria Donor",choices=c("No","Yes"),inline=T,status="primary"),
                 
                 h4("Postoperative Data", class="main-header"),
                 
                 sliderInput("gfr_day7","GFR Day 7 (mL/min)",min=0,max=150,value=60),
                 sliderInput("length_hosp","Hospital Stay (days)",min=0,max=60,value=7),
                 awesomeRadio("delayed_graft","Delayed Graft Function",choices=c("No","Yes"),inline=T,status="primary"),
                 awesomeRadio("icu","ICU Admission",choices=c("No","Yes"),inline=T,status="primary"),
                 awesomeRadio("surg_reint","Surgical Reintervention",choices=c("No","Yes"),inline=T,status="primary"),
                 awesomeRadio("surg_site_infect","Surgical Site Infection",choices=c("No","Yes"),inline=T,status="primary"),
                 awesomeRadio("uti","Urinary Tract Infection",choices=c("No","Yes"),inline=T,status="primary"),
                 
                 actionBttn("predict","Calculate Risk",style="gradient",color="primary",block=T)
    ),
    
    mainPanel(
      width = 8,
      fluidRow(
        valueBoxOutput("readmit_risk", width = 12)
      ),
      div(
        style = "text-align: right; margin-top: 25px;",
        downloadButton("descargar", "Export Results", icon = icon("download"), class = "btn-sm")
      ),
              
              # Footer texts
              div(class = "footer-text-container",
                  div(class = "copyright-text",
                      style = "font-family: 'Arial', sans-serif; font-size: 11px; background-color:#f0f4f8; padding: 10px; border-radius: 6px; border: 1px solid #cfd8dc;",
                      "COPYRIGHT: Copyright of the text contained in this website is owned by Colombiana de Trasplantes.
          These text materials may be used, downloaded, reproduced, publicly displayed, or reprinted for personal
          or nonprofit educational or research purposes provided that the following attribution appears in all copies:
          'Reproduced with permission of Colombiana de Trasplantes.' Written permission is required for all other uses.
          The text reproduced from this Web site must not be modified in any way."
                  ),
                  
                  div(class = "disclaimer-text",
                      style = "font-family: 'Arial', sans-serif; font-size: 11px; background-color:#f8d7da; padding: 10px; border-radius: 6px; border: 1px solid #f5c6cb; margin-top: 10px;",
                      "NOT MEDICAL ADVICE: The content contained on this site is not intended to and does not constitute medical advice, and no doctor/patient relationship is formed."
                  )
              )
    )
  )
)



server <- function(input, output, session) {
  
  rf_model <- readRDS("rf_model.rds")
  
  # Validaciones condicionales
  observeEvent(input$dialysis_type, {
    if (input$dialysis_type == "Predialysis") {
      updateSliderInput(session, "dialysis_months", value = 0)
      shinyjs::disable("dialysis_months")
    } else {
      shinyjs::enable("dialysis_months")
    }
  })
  
  observeEvent(input$transplant_type, {
    if (input$transplant_type == "Living Donor") {
      updateAwesomeRadio(session, "ecd", selected = "No")
      shinyjs::disable("ecd")
    } else {
      shinyjs::enable("ecd")
    }
  })
  
  # Habilita botón solo si campos están completos
  observe({
    campos_requeridos <- c(
      "age", "sex", "diabetes", "dialysis_months", "dialysis_type",
      "transplant_type", "ecd", "gfr_day7", "length_hosp",
      "delayed_graft", "icu", "surg_reint", "surg_site_infect", "uti"
    )
    
    campos_completos <- sapply(campos_requeridos, function(x) {
      val <- input[[x]]
      if(is.null(val)) return(FALSE)
      if(is.character(val)) val != "" else !is.na(val)
    })
    
    toggleState("predict", all(campos_completos))
  })
  
  # Predicción del modelo
  prediccion <- eventReactive(input$predict, {
    show_alert(
      title = "Calculating Risk...",
      text = "Processing clinical data, please wait.",
      type = "info",
      timer = 1500,
      closeOnClickOutside = FALSE,
      showConfirmButton = FALSE
    )
    
    input_data <- data.frame(
      Age_at_Transplant = input$age,
      Recipient_Gender = input$sex,
      Diabetes_Mellitus = input$diabetes,
      Calculated_Dialysis_Time = input$dialysis_months,
      Dialysis_Type = input$dialysis_type,
      Transplant_Type = input$transplant_type,
      Expanded_Donor_Criteria = input$ecd,
      GFR_Day_7 = input$gfr_day7,
      Days_of_Hospitalization_Post_transplant = input$length_hosp,
      Delayed_Graft_Function = input$delayed_graft,
      ICU_Admission = input$icu,
      Surgical_Reintervention = input$surg_reint,
      Surgical_Site_Infection = input$surg_site_infect,
      Urinary_Tract_Infection = input$uti
    )
    
    predict(rf_model, input_data, type = "prob")[,"Yes"]
  })
  
  # Salida del riesgo con colores sin emojis
  output$readmit_risk <- renderValueBox({
    req(prediccion())
    prob <- prediccion()
    
    valueBox(
      value = paste0(round(prob * 100, 1), "%"),
      subtitle = "Probability of 30-day Readmission"
      # No color argument
    )
  })
  
  # Descarga en Excel
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("readmission-risk-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      resultados <- data.frame(
        Age = input$age,
        Gender = input$sex,
        Diabetes = input$diabetes,
        Dialysis_Months = input$dialysis_months,
        Dialysis_Type = input$dialysis_type,
        Transplant_Type = input$transplant_type,
        ECD = input$ecd,
        GFR_Day7 = input$gfr_day7,
        Hospital_Stay = input$length_hosp,
        Delayed_Graft_Function = input$delayed_graft,
        ICU_Admission = input$icu,
        Surgical_Reintervention = input$surg_reint,
        Surgical_Site_Infection = input$surg_site_infect,
        UTI = input$uti,
        Readmission_Risk_Percent = round(prediccion() * 100, 1)
      )
      
      wb <- createWorkbook()
      addWorksheet(wb, "Results")
      writeDataTable(wb, "Results", resultados, withFilter = TRUE)
      saveWorkbook(wb, file)
    }
  )
}

shinyApp(ui, server)
