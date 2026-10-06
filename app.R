library(shiny)
library(tidyverse)
library(lubridate)

# Keep app.R in the same folder as city_day.csv
raw <- read_csv("city_day.csv", show_col_types = FALSE)

df <- raw %>%
  filter(!is.na(AQI)) %>%
  mutate(
    Date = as.Date(Date),
    AQI_Bucket = factor(
      AQI_Bucket,
      levels = c("Good","Satisfactory","Moderate","Poor","Very Poor","Severe")
    )
  )

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#f5f7fb;font-family:Arial,sans-serif}
    .title{background:#182230;color:white;padding:22px;margin:-15px -15px 20px}
    .title h2{margin:0}.title p{color:#cbd5e1;margin:6px 0 0}
    .card{background:white;border-radius:10px;padding:18px;margin-bottom:18px;
          box-shadow:0 2px 8px rgba(0,0,0,.07)}
    .label{color:#64748b;font-size:13px}.value{font-size:27px;font-weight:700}
  "))),

  div(class="title",
      h2("Air Quality Analysis Dashboard"),
      p("BDA Mini Project • India Air Quality Dataset • 2015–2020")),

  sidebarLayout(
    sidebarPanel(
      div(class="card",
          h4("Filters"),
          selectInput("city","City",
                      c("All Cities",sort(unique(df$City)))),
          dateRangeInput("date","Date Range",
                         start=min(df$Date),end=max(df$Date),
                         min=min(df$Date),max=max(df$Date)),
          selectInput("bucket","AQI Category",
                      c("All Categories",levels(df$AQI_Bucket))),
          actionButton("reset","Reset Filters",width="100%")
      ),
      div(class="card",
          h4("Project"),
          p("Interactive exploration of AQI, pollutants, cities and monthly trends."),
          p(strong("Original records: "),nrow(raw)),
          p(strong("AQI records: "),nrow(df)))
    ),

    mainPanel(
      fluidRow(
        lapply(c("Average AQI","Maximum AQI","Records","Cities"), function(x)
          column(3,div(class="card",
            div(class="label",x),
            div(class="value",textOutput(
              switch(x,
                     "Average AQI"="avg",
                     "Maximum AQI"="max",
                     "Records"="records",
                     "Cities"="cities"),inline=TRUE)))))
      ),

      tabsetPanel(
        tabPanel("Overview",
          fluidRow(
            column(6,div(class="card",plotOutput("cat",height="340px"))),
            column(6,div(class="card",plotOutput("hist",height="340px")))
          ),
          div(class="card",plotOutput("trend",height="420px"))
        ),

        tabPanel("City Analysis",
          div(class="card",plotOutput("cityplot",height="620px")),
          div(class="card",tableOutput("citytable"))
        ),

        tabPanel("Pollutants",
          div(class="card",plotOutput("cor",height="520px")),
          div(class="card",tableOutput("pollutanttable"))
        ),

        tabPanel("Data",
          div(class="card",
              downloadButton("download","Download Filtered CSV"),
              br(),br(),tableOutput("datatable"))
        )
      )
    )
  )
)

server <- function(input, output, session){

  observeEvent(input$reset,{
    updateSelectInput(session,"city",selected="All Cities")
    updateSelectInput(session,"bucket",selected="All Categories")
    updateDateRangeInput(session,"date",start=min(df$Date),end=max(df$Date))
  })

  filtered <- reactive({
    x <- df %>% filter(Date >= input$date[1], Date <= input$date[2])
    if(input$city != "All Cities") x <- x %>% filter(City == input$city)
    if(input$bucket != "All Categories") x <- x %>% filter(AQI_Bucket == input$bucket)
    x
  })

  output$avg <- renderText({
    x <- filtered(); if(!nrow(x)) return("—"); round(mean(x$AQI),1)
  })
  output$max <- renderText({
    x <- filtered(); if(!nrow(x)) return("—"); round(max(x$AQI),0)
  })
  output$records <- renderText({format(nrow(filtered()),big.mark=",")})
  output$cities <- renderText({n_distinct(filtered()$City)})

  output$cat <- renderPlot({
    ggplot(filtered(),aes(AQI_Bucket))+geom_bar()+
      labs(title="AQI Category Distribution",x="Category",y="Records")+
      theme_minimal(base_size=13)
  })

  output$hist <- renderPlot({
    ggplot(filtered(),aes(AQI))+geom_histogram(bins=35)+
      labs(title="AQI Distribution",x="AQI",y="Frequency")+
      theme_minimal(base_size=13)
  })

  output$trend <- renderPlot({
    m <- filtered()%>%mutate(Month=floor_date(Date,"month"))%>%
      group_by(Month)%>%summarise(Average_AQI=mean(AQI),.groups="drop")
    ggplot(m,aes(Month,Average_AQI))+geom_line(linewidth=.8)+
      labs(title="Monthly Average AQI",x="Month",y="Average AQI")+
      theme_minimal(base_size=13)
  })

  output$cityplot <- renderPlot({
    x <- filtered()%>%group_by(City)%>%
      summarise(Average_AQI=mean(AQI),.groups="drop")
    ggplot(x,aes(reorder(City,Average_AQI),Average_AQI))+
      geom_col()+coord_flip()+
      labs(title="Average AQI by City",x="City",y="Average AQI")+
      theme_minimal(base_size=13)
  })

  output$citytable <- renderTable({
    filtered()%>%group_by(City)%>%
      summarise(Records=n(),Average_AQI=round(mean(AQI),1),
                Maximum_AQI=round(max(AQI)),.groups="drop")%>%
      arrange(desc(Average_AQI))
  })

  pollutants <- c("PM2.5","PM10","NO","NO2","NOx","NH3",
                   "CO","SO2","O3","Benzene","Toluene","Xylene")

  output$cor <- renderPlot({
    x <- filtered()
    vals <- sapply(pollutants,function(p)
      cor(x$AQI,x[[p]],use="pairwise.complete.obs"))
    z <- data.frame(Pollutant=pollutants,Correlation=as.numeric(vals))
    ggplot(z,aes(reorder(Pollutant,Correlation),Correlation))+
      geom_col()+coord_flip()+
      labs(title="Correlation of Pollutants with AQI",
           x="Pollutant",y="Correlation")+
      theme_minimal(base_size=13)
  })

  output$pollutanttable <- renderTable({
    x <- filtered()
    data.frame(Pollutant=pollutants,
               Average_Level=round(sapply(pollutants,
                 function(p)mean(x[[p]],na.rm=TRUE)),2))
  })

  output$datatable <- renderTable({
    filtered()%>%select(City,Date,PM2.5,PM10,NO,NO2,NOx,NH3,
                        CO,SO2,O3,AQI,AQI_Bucket)%>%head(200)
  })

  output$download <- downloadHandler(
    filename=function()paste0("air_quality_filtered_",Sys.Date(),".csv"),
    content=function(file)write_csv(filtered(),file)
  )
}

shinyApp(ui,server)
