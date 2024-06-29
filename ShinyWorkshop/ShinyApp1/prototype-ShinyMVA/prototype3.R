

pacman::p_load(shiny,shinydashboard,shinythemes,
               scaterPlotMatrix,parallelPlot,cluster,
               factoextra,tidyverse)

whitewine <- read_csv("data/wine_quality.csv")%>%
  filter(type=="white")%>%
  select(c(1:11))

u1 <- fluidPage()


  
  
  
  
  
  
}

shinyApp (ui=ui, server = server)


  
