# Plot functions

# Section 1 Plots --------------------------------------------------------------
plot_1 <- function(datainput,
                   product, # "[Product Code] - [Produce Name]"
                   years, 
                   percentage = FALSE
) {
  
  data <- datainput %>%
    filter(Product == product,
           Year %in% years[1]:years[2])
  
  fig <- plot_ly(data, type = "scatter", mode = "lines", color = ~Trade, colors = "Set2",
                 line = list(width = 4)) %>%
    add_trace(x = ~Year,
              y = ~Value,
              text = ~Trade,
              hovertemplate = "<b>%{text}</b> <br>Year: %{x} </br>Value: %{y}<extra></extra>") %>% # <extra></extra> removes the extra bit (trace 0)
    layout(showlegend = T)
  
  fig <- fig %>%
    layout(xaxis = list(title = "Year",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           
           legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5, # put legend in center of x-axis
                         y = -0.2), 
           
           # Update margins to prevent title from overlapping
           margin = list(autoexpand = TRUE, b = 75, l = 50, r = 100, t = 10) # Change the T to 100 if using titles here
    )
  
  # Use % if Value is in percent terms
  if (percentage) {
    fig <- fig %>%
      layout(yaxis = list(title = "Percentage (%)",
                          tickformat = ".0%",
                          hoverformat = ".2%",
                          range = c(0, suppressWarnings(max(data$Value))+0.01)))
  } else {
    fig <- fig %>%
      layout(yaxis = list(title = "USD '000",
                          range = c(-1000, suppressWarnings(max(data$Value))+25000)))}
  
  return(fig)
}

# Section 2 Plots --------------------------------------------------------------

plot_2 <- function(data, exporter, product, num_countries, years) {
  
  data <- data %>%
    filter(Exporter == exporter,
           Product == product,
           Rank <= num_countries,
           Year %in% years[1]:years[2])
  
  # colourpal <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231',
  #                '#911eb4', '#42d4f4', '#f032e6', '#bfef45', '#fabed4',
  #                '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000',
  #                '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9')
  
  colourpal <- c("#f44336","#e81e63","#9c27b0","#673ab7","#3f51b5",	
                          "#2196f3","#03a9f4","#00bcd4","#009688","#4caf50",	
                          "#8bc34a","#cddc39","#ffeb3b","#ffc107","#ff9800",	
                          "#ff5722","#795548","#9e9e9e","#607d8b","#000000")
                          
  fig <- plot_ly(data, type = "scatter", mode = "lines", color = ~Importers, colors = colourpal,
                 line = list(width = 4)) %>%
    add_trace(x = ~Year,
              y = ~Value,
              text = ~Importers,
              hovertemplate = "<b>%{text}</b> <br>Year: %{x} </br>Value: %{y}<extra></extra>") %>%
    layout(showlegend = T)
  
  fig <- fig %>%
    layout(xaxis = list(title = "Year",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(title = "USD '000",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           
           legend = list(orientation = "l"), 
           
           # Update margins to prevent title from overlapping
           margin = list(autoexpand = TRUE, b = 75, l = 50, r = 100, t = 10) # Change the T to 100 if using titles here
    )
  
  return(fig)
}


# Section 3 Plots --------------------------------------------------------------

plot_3 <- function(data, product = NULL, years) {
  
  if (!is.null(product)) {
    data <- data %>%
      filter(substr(Product, 1, 2) %in% substr(product, 1, 2))
  }
  
  data <- data %>%
    filter(Year %in% years[1]:years[2]) %>%
    mutate(Product = gsub("\n", "<br>", str_wrap(Product, 40))) # Insert <br> to split up legend text
  
  
  colourpal <- c("#FBF8CC", "#FDE4CF", "#FFCFD2", "#f08080", "#F1C0E8",
                          "#CFBAF0", "#A3C4F3", "#90DBF4", "#8EECF5", "#98F5E1", "#B9FBC0")
                          
  fig <- plot_ly(data, type = "scatter", mode = "lines", color = ~Product, colors = colourpal,
                 line = list(width = 4)) %>%
    add_trace(x = ~Year,
              y = ~Value,
              text = ~Product,
              hovertemplate = "<b>%{text}</b> <br>Year: %{x} </br>Value: %{y}<extra></extra>") %>%
    layout(showlegend = T)
  
  fig <- fig %>%
    layout(xaxis = list(title = "Year",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(title = "USD '000",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           
           legend = list(orientation = "l"), 
           
           # Update margins to prevent title from overlapping
           margin = list(autoexpand = TRUE, b = 75, l = 50, r = 100, t = 10) # Change the T to 100 if using titles here
    )
  
  return(fig)
}

# Section 4 Plots --------------------------------------------------------------

# Compare countries 
plot_4 <- function(data, reporter, partner, flow, item_code, years){
  
  data <- data %>%
    filter(Reporter == reporter,
           Partner %in% partner,
           Flow == flow,
           Item_code == item_code,
           Year %in% years[1]:years[2])
  
  
  colourpal <- c("#FBF8CC", "#FDE4CF", "#FFCFD2", "#f08080", "#F1C0E8",
                          "#CFBAF0", "#A3C4F3", "#90DBF4", "#8EECF5", "#98F5E1", "#B9FBC0")
                          
  fig <- plot_ly(data, type = "scatter", mode = "lines", color = ~Partner, colors = colourpal,
                 line = list(width = 4)) %>%
    add_trace(x = ~Year,
              y = ~Final_value,
              text = ~Partner,
              hovertemplate = "<b>%{text}</b> <br>Year: %{x} </br>Value: %{y}<extra></extra>") %>%
    layout(showlegend = T)
  
  fig <- fig %>%
    layout(xaxis = list(title = "Year",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(title = "Millions of dollars ($)",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           
           legend = list(orientation = "l"), 
           
           # Update margins to prevent title from overlapping
           margin = list(autoexpand = TRUE, b = 75, l = 50, r = 100, t = 10) # Change the T to 100 if using titles here
    )
  
  return(fig)
  
}

# Section 5 Plots --------------------------------------------------------------

# Compare products 
plot_5 <- function(data, reporter, partner, flow, item_code, years){
  
  data <- data %>%
    filter(Reporter == reporter,
           Partner == partner,
           Flow == flow,
           Item_code %in% item_code,
           Year %in% years[1]:years[2])
  
  
  colourpal <- c("#FBF8CC", "#FDE4CF", "#FFCFD2", "#f08080", "#F1C0E8",
                          "#CFBAF0", "#A3C4F3", "#90DBF4", "#8EECF5", "#98F5E1", "#B9FBC0")
                          
  fig <- plot_ly(data, type = "scatter", mode = "lines", color = ~Item_code, colors = colourpal,
                 line = list(width = 4)) %>%
    add_trace(x = ~Year,
              y = ~Final_value,
              text = ~Item_code,
              hovertemplate = "<b>%{text}</b> <br>Year: %{x} </br>Value: %{y}<extra></extra>") %>%
    layout(showlegend = T)
  
  fig <- fig %>%
    layout(xaxis = list(title = "Year",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           yaxis = list(title = "Millions of dollars ($)",
                        ticks = "outside", ticklen = 5, tickwidth = 2,
                        tickcolor = 'black', showticklabels = TRUE,
                        zeroline = TRUE, showline = TRUE, zerolinewidth = 2,
                        gridcolor = 'ffff'),
           
           legend = list(orientation = "l"), 
           
           # Update margins to prevent title from overlapping
           margin = list(autoexpand = TRUE, b = 75, l = 50, r = 100, t = 10) # Change the T to 100 if using titles here
    )
  
  return(fig)
  
}



# 1st, 2nd, 3rd, 4th Suffix ----------------------------------------------------
ordinal_suffix <- function(i) {
  if (identical(i, integer(0))) {return("unranked")}
  
  j <-  i %% 10
  k <-  i %% 100
  if (j == 1 & k != 11) {
    return(paste0(i, "st"))
  }
  if (j == 2 & k != 12) {
    return(paste0(i, "nd"))
  }
  if (j == 3 & k != 13) {
    return(paste0(i, "rd"))
  } else {
  return(paste0(i, "th"))
    }
}

