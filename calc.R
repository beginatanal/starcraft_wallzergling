library(tidyverse)
library(shiny)
library(ggpubr)
library(psych)
library(waiter)

# Get the directory of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)



ui <- fluidPage(
  titlePanel(
    HTML("EUD 벽짓고 백만 저글링 막기 3 V0.03 확률 시뮬레이터")
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="slevel", label="시작 레벨", min=0, max=20, value=0, step=1),
      sliderInput(inputId="elevel", label="목표 레벨", min=0, max=20, value=1, step=1),
      numericInput(inputId="sgas", label="시작 파방 가격", min=1, max=99999, step=1, value=1),
      sliderInput(inputId="suc", label="추가 강화 확률", min=0, max=9, step=1, value=0),
      sliderInput(inputId="des", label="파괴 확률 감소", min=0, max=12, step=3, value=0),
      sliderInput(inputId="sim", label="시뮬레이션 수", min=500, max=10000, step=500, value=10000),
      actionButton(inputId="run", label="시뮬레이션 시작")
    ),
    mainPanel(
      selectInput(inputId="choice", label="선택지", choices=c("총 미네랄 비용", "총 가스 비용", "실패 횟수")),
      uiOutput("resplot"),
      verbatimTextOutput("restext"),
      use_waiter()
    )
  )    
  
)


df <- read.csv("Upgrade.csv")

server <- function(input, output, session){
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$resplot <- renderUI(renderPlot(plot(x=NULL, y=NULL, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="")))
  observeEvent(input$slevel, {
    if (input$slevel == 20){
      updateSliderInput(session=session, inputId="slevel", label="시작 레벨", min=0, max=20, value=19, step=1)
    }
    if (input$slevel > input$elevel){
      updateSliderInput(session=session, inputId="elevel", label="목표 레벨", min=0, max=20, value=(input$slevel+1), step=1)
    } 
  })
  min <- reactiveVal()
  gas <- reactiveVal()
  fail <- reactiveVal()
  
  observeEvent(input$run, {
    
    waiter <- Waiter$new(id="resplot")
    waiter$show()
    on.exit(waiter$hide())
    
    starfunc <- function(ini, end, gas=1, suc=0, des=0){
      lev <- ini
      mcost <- 0
      mcostcum <- 0
      gcost <- gas
      gcostcum <- 1
      fail <- 0
      
      while (lev != end){
        dice <- runif(1)
        mcost <- df[df$Level==lev, "Mineral.Cost"]
        mcostcum <- mcostcum + mcost
        sc <- (df[df$Level==lev, "Success"]+suc)/100
        dc <- (df[df$Level==lev, "Destroy"]-des)/100
        if (sc>1){sc<-1}
        if (dc<0){dc=0}
        rc <- 1-sc-dc
        outcome <- sample(c(1:3), size=1, prob=c(sc,dc,rc))
        if (outcome==1){
          lev = lev+1
        }else{
          if (outcome==2){
            gcost <- gcost+1
            gcostcum <- gcostcum+gcost
          }
          fail <- fail+1
        }
      }
      a <- data.frame(min=c(mcostcum), gas=c(gcostcum), gaslevel=c(gcost), fail=c(fail))
      return(a)
    }
    
    res <- map_df(1:input$sim, ~starfunc(input$slevel, input$elevel, input$sgas, input$suc, input$des))
    min(res$min)
    gas(res$gas)
    fail(res$fail)
  })
  
  observeEvent({
    input$run
    input$choice
  }, {
    req(min())
    req(gas())
    plot <- switch(input$choice,
                   "총 미네랄 비용" = gghistogram(min())+xlab("총 미네랄 비용"),
                   "총 가스 비용" = gghistogram(gas())+xlab("총 가스 비용"),
                   "실패 횟수" = gghistogram(fail())+xlab("실패 횟수"),
    )
    rtext <- switch(input$choice,
                    "총 미네랄 비용" = describe(data.frame(mineral=min())),
                    "총 가스 비용" = describe(data.frame(gas=gas())),
                    "실패 횟수" = describe(data.frame(fail=fail())),
    )
    output$resplot <- renderUI(renderPlot(plot))
    output$restext <- renderText({
      mean <- round(rtext[1,"mean"], digits=0)
      median <- round(rtext[1, "median"], digits=0)
      se <- rtext[1,"se"]
      l95 <- round(mean-(1.96*se), digits=0)
      u95 <- round(mean+(1.96*se), digits=0)
      mi <- rtext[1,"min"]
      ma <- rtext[1,"max"]
      paste0("평균값 : ", mean, "\n", "중간값 : ", median, "\n",
             "95% 신뢰구간 : ", l95, "~", u95, "\n",
             "최소값 : ", mi, "\n", "최대값 : ", ma)
    })
    
  })
  
  
}

shinyApp(ui, server)
