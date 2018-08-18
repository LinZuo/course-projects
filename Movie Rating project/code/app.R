library(shiny)
library(ggplot2)
library(dplyr)

load ("movies_update.RData")

num_var <- movies%>%select(runtime, thtr_rel_year, thtr_rel_month, thtr_rel_day, dvd_rel_year, dvd_rel_month, dvd_rel_day, imdb_rating, imdb_num_votes, critics_score, audience_score)
cat_var <-movies%>%select(title_type, genre, mpaa_rating, critics_rating, audience_rating, best_pic_nom, best_pic_win, best_actor_win, best_actress_win, best_dir_win, top200_box)


app = shinyApp(
  ui = fluidPage(
    titlePanel("Fantastic Movies - Where to find them?"),
    sidebarLayout(
      sidebarPanel(
        selectInput("var1", "Choose a criterion: ", choices =c("Public's ratings" = "audience_score", "Oscar awards" = "award", "Profit" = "profit"), selected = "audience_score"),
        selectInput("var2","Choose a numerical predictor: ", choices = names(num_var), selected = "runtime"),
        selectInput("cat_var","Choose a categorical predictor: ", choices =  names(cat_var), selected = "title_type")
),
      mainPanel(
        h4("Graph:"),
        plotOutput("main_plot"),
        hr(),
        h4("Thanks for watching!")
      )
)),
  server = function(input, output)
  {
    output$main_plot = renderPlot({
      if (input$var1=="award")
        {ggplot(movies, aes_string(y=input$var2, x=input$var1, col=input$var1))+geom_boxplot()}
      else
      {ggplot(movies, aes_string(y=input$var1, x=input$var2, col=input$cat_var)) +geom_point()+geom_smooth(method=lm, se=FALSE)}
   })
  }
)