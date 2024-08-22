#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)

options(warn=-1)
Generate_selectize<-function(data,column,label){
  
  
  
  selectizeInput(column, label,choices = c(sort(unique(data[,column]))),selected = "All",multiple = T,options = list(
    plugins = list("remove_button"),
    create = TRUE,
    persist = TRUE # keep created choices in dropdown
  ))
  
}


filter_selectize_data<-function(session,data,column,input_list){
  
  if(!is.null(input_list)){
    data<-data[data[,column]%in%input_list,]
  }
  return(data)
}

histogram <- function(data,column,bin_width,seq){
  #data<-dataset
  
  plot <- ggplot(data ,aes(x=get(column),text=stat(count)))+geom_histogram(binwidth  =bin_width ,color="white", boundary = 0,size=1,fill="#25EAC9")+
    scale_x_continuous(breaks = seq)+
    theme(panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white",colour = "white"),
          axis.text.y = element_text(colour="black",size=14,margin=margin(r=-25)),
          axis.text.x= element_text(colour="black",size=14,vjust=0.8),
          axis.title.x =  element_text(colour="black",size=16,face="bold"),
          axis.title.y =  element_text(colour="black",size=16,face="bold"),
          plot.caption = element_text(colour="black",size=14,hjust=c(0,1),margin=margin(t=1,unit="cm")),
          axis.ticks.y = element_blank(),
          plot.title =  element_text(colour="black",size=34,face="bold",hjust=0),
          text = element_text(family = "Light"),legend.key = element_rect(fill="white",color="white"),
          legend.text = element_text(colour="black",size=16),legend.position = 'bottom',legend.justification = "left",
          legend.title = element_text(colour="black",size=16),
          plot.subtitle =element_text(family="Light",size=24,margin=margin(b=10),hjust=0),
          legend.key.height = unit(0.5, "inch"),
          strip.background = element_rect(fill="white"),strip.text = element_text(size=14),plot.title.position = "plot"
    )+scale_y_continuous(expand  =c(0,0.2))+xlab(label = column)
  
  
  max_y <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range[2]
  
  plot<- plot +coord_cartesian(ylim=c(0,max_y+10))

  return(  ggplotly(plot,tooltip = "text"))
}


# Define server logic required to draw a histogram
function(input, output, session) {

  dataset <<- read.csv("www/eq_mag5plus_2014_2024.csv") 
  dataset$year <- substr(dataset$time,1,4)
  
  output$Selectyear <- renderUI({Generate_selectize(dataset,"year","Year")})
  
  filtered_Map_Data <-reactive({ filter_selectize_data(session,dataset,"year",input$year) })
  
   
  
  output$myplot <- renderLeaflet({
    
    df_map <<- filtered_Map_Data()
    

   pal <- colorNumeric(
    palette = colorRampPalette(c('#25EAC9', 'blue'))(length(unique(df_map$mag))),domain =as.numeric(df_map$mag),na.color = NA)
  
  
  leaflet(df_map) %>% addProviderTiles(providers$CartoDB.Positron, options=providerTileOptions(noWrap = TRUE))  %>% 
    leaflet::addCircleMarkers( radius = ~ 3,lng = ~longitude, lat = ~latitude,color = ~pal(mag),
                               popup = paste0( "Magnitude:"
                                               , df_map$mag
                                               , "<br>"
                                               , "Location:",df_map$place
                                               , "<br>"
                               )) %>% addLegend(position = "topright",pal=pal,values=df_map$mag,title = "Magnitude",opacity = 1)
  })
  
  
    output$hist <- renderPlotly({
      df_hist <- filtered_Map_Data()

      histogram(df_hist,"mag",bin_width = 0.2,seq =seq(floor(min(df_hist$mag)),ceiling(max(df_hist$mag)),0.2))

    })

    output$hist2 <- renderPlotly({
      df_hist <- filtered_Map_Data()
      
      histogram(df_hist,"depth",bin_width = 50,seq = seq(floor(min(df_hist$depth)),ceiling(max(df_hist$depth)),50 ))
      
    })
    
}
