require(ggplot2)
library(ggmap)
library(dplyr)
library(shiny)
library(shinythemes)
# 讀取csv
air<-read.csv('air1.csv',header=TRUE)

# 將數據裡的NA轉成0
air <- mutate_if(air, is.numeric, ~replace(., is.na(.), 0))
#class(air)
#str(air)
max(air$Longitude)# 121.7929
min(air$Longitude)# 118.3123
max(air$Latitude)# 26.16047
min(air$Latitude)# 21.95807
head(air)
air$Longitude<-as.numeric(air$Longitude)
air$Latitude<-as.numeric(air$Latitude)
air$CO<-as.numeric(air$CO)
air$PM2.5<-as.numeric(air$PM2.5)
# 繪製地圖
TaiwanMap = get_map(location = c(118.2,21.8,121.9,26.2), zoom = 7, maptype = 'satellite')
# 以CO標點
TaiwanMapO = ggmap(TaiwanMap)+ geom_point(data=subset(air,CO>=0), aes(x=Longitude, y=Latitude,color=CO,size=5))+
  scale_color_continuous(low = "yellow",high = "red")+ guides(size = "none")
# 以PM2.5標點
TaiwanMap1 = ggmap(TaiwanMap)+ geom_point(data=subset(air,PM2.5>=0), aes(x=Longitude, y=Latitude,color=PM2.5,size=5))+
  scale_color_continuous(low = "green",high = "red")+ guides(size = "none")

# 輸出圖片
TaiwanMapO
TaiwanMap1



#設定UI介面
ui <- fluidPage( # 頁面設定函數
  themeSelector(),# 樣式選擇器
  titlePanel("Air pollution"), # 標題版面(panel)
  sidebarLayout( #版型(layout)
    sidebarPanel( #側邊欄位版面(panel)
      selectInput( #輸入元件
        inputId="CountyID",
        label='城市',
        choices = air$County,
        selected = air$County,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      selectInput( #輸入元件
        inputId="Air",
        label='空氣',
        choices = names(air)[6:15],
        selected = 6:15,
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      )
    ), 
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("dataTable")
    ) #主頁版面(panel)
  )
)
# 做輸入與輸出
server <- function (input,output){
  output$plot<-renderPlot( {
    choose=air[grep(input$CountyID,air$County),]
    choose$PM2.5<-as.numeric(choose$PM2.5)
    TaiwanMap = get_map(location = c(min(choose$Longitude)-0.1,min(choose$Latitude)-0.1,max(choose$Longitude)+0.1,max(choose$Latitude)+0.1), zoom = 10, maptype = "satellite")
    TaiwanMap2 = ggmap(TaiwanMap)+ 
      geom_point(data=subset(choose,PM2.5>=0), 
                 aes(x=Longitude, y=Latitude,color=PM2.5,size=1))+
      scale_color_continuous(low = "green",high = "red")+ guides(size = "none")
    TaiwanMap2
  })
  output$dataTable<-renderDataTable({
    choose=air[grep(input$CountyID,air$County),]
    choose
  })
  
}
# 開啟頁面
shinyApp(ui = ui, server = server)

