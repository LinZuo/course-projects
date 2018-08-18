library(ggvis)
library(dplyr)

# Set up handles to database tables on app start
db <- movies


function(input, output, session) {
  
  # Filter the movies, returning a data frame
  movies <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    imdb_num_votes <- input$imdb_num_votes
    
    # Apply filters
    m <- all_movies %>%
      filter(
        imdb_num_votes >= imdb_num_votes
      )
    
    # Optional: filter by genre
    if (input$genre != "All") {
      genre <- paste0("%", input$genre, "%")
      m <- m %>% filter(Genre %like% genre)
    }
    # Optional: filter by director
    if (!is.null(input$director) && input$director != "") {
      director <- paste0("%", input$director, "%")
      m <- m %>% filter(Director %like% director)
    }
    
    
    m <- as.data.frame(m)
  
  })
  
  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)
    
    # Pick out the movie with this ID
    all_movies <- isolate(movies())
    movie <- all_movies[all_movies$ID == x$ID, ]
    
    paste0("<b>", movie$Title, "</b><br>",
           movie$Year, "<br>",
           "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
    )
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~imdb_num_votes),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    movies %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~has_oscar, key := ~ID) %>%
      add_tooltip(movie_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("orange", "#aaa")) %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_movies <- renderText({ nrow(movies()) })
}