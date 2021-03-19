#MSband - HR분석

library(shiny)
library(tidyverse)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme(theme = 'united'),
  titlePanel(title = '심박수 분석'),
  sidebarPanel(
    fileInput(inputId = 'file',
              label = '`csv` 또는 `txt` 파일을 선택하세요.',
              multiple = FALSE,
              buttonLabel = icon(name = 'search'),
              placeholder = '아직 파일이 선택되지 않았습니다.'),
    
    radioButtons(inputId = 'sep',
                 label = '구분자를 선택하세요',
                 choices = c('콤마' = ',', '세미콜론' = ';',
                             '탭' = '\t', '공백' = ' '),
                 selected = ',',
                 inline = TRUE),
    checkboxInput(inputId = 'header',
                  label = '첫 번째 행은 헤더입니다.',
                  value = TRUE),
    selectInput(inputId = 'x',
                label = 'x축에 놓을 숫자형 변수를 선택하세요.',
                choices = NULL),
    selectInput(inputId = 'y',
                label = 'y축에 놓을 숫자형 변수를 선택하세요.',
                choices = NULL),
    selectInput(inputId = 'z',
                label = '색상을 참고할 범주형 변수를 선택하세요.',
                choices = NULL),
    submitButton(text = '변경사항을 적용합니다.',
                 icon = icon(name = 'sync'))
  ),
  mainPanel(
    uiOutput(outputId = 'mainUI')
  )
)

server <- function(input, output, session) {
  df <- reactive({
    if(is.null(x = input$file)) return()
    read.csv(file = input$file$datapath, header = input$header,
             sep = input$sep, stringsAsFactors = TRUE)
  })
  
  observe({
    cols <- colnames(x = df())
    updateSelectInput(session = session, inputId = 'x', choices = cols)
    updateSelectInput(session = session, inputId = 'y', choices = cols)
    updateSelectInput(session = session, inputId = 'z', choices = cols)
  })
  
  output$table <- renderTable({
    if(is.null(x = df())) return() else df()[1:4, ]
  })
  output$glimpse <- renderPrint({
    if(is.null(x = df())) return() else glimpse(x = df())
  })
 
  output$p1 <- renderTable({  #심박수 최대값 최소값 테이블
    df2 <- data.frame(min=min(data=df()[, input$y]), max=max(data=df()[, input$y]), mean=mean(df()[, input$y]), std=sd(df()[, input$y]))
  })
  
  output$ggplot <- renderPlot({     #line탭 선그래프 그리기
    ggplot(data = df(), aes(x = df()[, input$x],
                         y = df()[, input$y],
                         group = 1)) +
      geom_line(color='red') +
      ggtitle("HeartRate")+
      coord_cartesian(ylim = c(50,100)) + 
      xlab("sec")+ylab("심박수측정")+theme_bw()+
      theme(plot.title=element_text(size=25,color="red",hjust=0.3),
            axis.title.x= element_text(size=20,hjust=0.3),
            axis.title.y=element_text(angle=0,vjust=0.5,size=20))
    
  })
  
  
  output$plot <- renderPlot({
    ggplot(data = df(),
           mapping = aes(x = df()[, input$x],
                         y = df()[, input$y],
                         color = df()[, input$z])) + 
      geom_point(shape = 19, size = 3) + 
      labs(title = 'Scatter plot', x = input$x, y = input$y) +
      theme_bw() + 
      theme(plot.title = element_text(face = 'bold', hjust = 0.5),
            legend.title = element_blank(),
            legend.position = 'bottom')
  })
  
  output$mainUI <- renderUI({
    if(is.null(x = df())) h4('아직 표시할 내용이 없습니다.')
    else tabsetPanel(
      tabPanel(title = 'Data',
               tableOutput(outputId = 'table'),
               verbatimTextOutput(outputId = 'glimpse')),
      tabPanel(title = 'Plot',
               plotOutput(outputId = 'plot')),
      tabPanel(title = 'statistics',
               tableOutput(outputId = 'p1')),
      tabPanel(title = 'line',
               plotOutput(outputId = 'ggplot'))
    )
  })
}

shinyApp(ui = ui, server = server)
