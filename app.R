# ==================== 乙肝肝硬化失代偿风险在线计算器 ====================
library(shiny)

# ---------- 风险计算核心函数 ----------
calc_risk <- function(hb, alb) {
  # 线性预测值（使用最终模型系数）
  lp <- 6.622057 - 0.025892 * hb - 0.117673 * alb
  
  # 失代偿概率
  prob <- 1 / (1 + exp(-lp))
  
  # 简易评分计算 (参考基线 Hb=130, ALB=35)
  score <- (130 - hb) / 10 * 1.0 + (35 - alb) / 5 * 2.3
  if (score < 0) score <- 0  # 低于参考值不计分
  
  # 风险分层
  if (score < 2.6) {
    risk <- "Low Risk"
    color <- "green"
    suggestion <- "Routine follow-up recommended (every 3-6 months)."
  } else if (score < 4.7) {
    risk <- "Intermediate Risk"
    color <- "orange"
    suggestion <- "Close monitoring advised. Evaluate for early signs of decompensation."
  } else {
    risk <- "High Risk"
    color <- "red"
    suggestion <- "Urgent evaluation for ascites, varices, and other complications is warranted."
  }
  
  list(
    hb = hb,
    alb = alb,
    lp = round(lp, 3),
    prob = round(prob * 100, 1),
    score = round(score, 1),
    risk = risk,
    color = color,
    suggestion = suggestion
  )
}

# ---------- UI 界面设计 ----------
ui <- fluidPage(
  titlePanel("Decompensation Risk Calculator for HBV-related Cirrhosis"),
  helpText("Based on Hemoglobin (Hb) and Albumin (ALB) — A Data-Driven Prediction Model"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("hb", "Hemoglobin, Hb (g/L)", 
                   value = 130, min = 50, max = 200, step = 1),
      numericInput("alb", "Albumin, ALB (g/L)", 
                   value = 35, min = 10, max = 60, step = 0.1),
      actionButton("calc", "Calculate Risk", class = "btn-primary"),
      hr(),
      h5("Model Formula:"),
      p("logit(P) = 6.622 - 0.026×Hb - 0.118×ALB", 
        style = "font-style:italic; color:gray;"),
      h5("Simplified Score:"),
      p("Hb: +1 point per 10 g/L decrease (from 130 g/L)"),
      p("ALB: +2.3 points per 5 g/L decrease (from 35 g/L)")
    ),
    
    mainPanel(
      h3("Prediction Result"),
      tableOutput("result_table"),
      br(),
      h4("Predicted Probability of Decompensation"),
      textOutput("prob_text"),
      br(),
      h4("Risk Stratification"),
      uiOutput("risk_ui"),
      br(),
      h4("Clinical Recommendation"),
      textOutput("suggestion_text"),
      br(),
      p("Disclaimer: This tool is for clinical reference only and does not replace professional medical judgment.",
        style = "color:gray; font-size:small;")
    )
  )
)

# ---------- 服务器逻辑 ----------
server <- function(input, output) {
  
  result <- eventReactive(input$calc, {
    calc_risk(input$hb, input$alb)
  })
  
  output$result_table <- renderTable({
    res <- result()
    data.frame(
      Parameter = c("Hemoglobin", "Albumin", "Simplified Score", "Linear Predictor"),
      Value = c(paste0(res$hb, " g/L"),
                paste0(res$alb, " g/L"),
                res$score,
                res$lp)
    )
  })
  
  output$prob_text <- renderText({
    paste0(result()$prob, "%")
  })
  
  output$risk_ui <- renderUI({
    res <- result()
    tags$span(res$risk, 
              style = paste("font-size:24px; font-weight:bold; color:", res$color))
  })
  
  output$suggestion_text <- renderText({
    result()$suggestion
  })
}

# ---------- 运行应用 ----------
shinyApp(ui = ui, server = server)