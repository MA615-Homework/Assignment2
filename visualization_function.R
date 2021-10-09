
#' @param t input data set
#' @param x variable related to time in the x axis
#' @param y variable that I want to show the time to time dynamics
#' @param group group variable  
#' @param idx y axis label
#' @return a plot show the time to time dynamics of y

time_plot <- function(t, x, y, group, func, idx){
  t %>% 
   group_by(!!group,!!x) %>%
   mutate(index = mean(!!y)) %>%
   select(c(!!x, index, !!group)) %>%
   ggplot(aes(x = !!x, y = index, color = !!group)) +
   geom_line(size=1) + 
   geom_point(size=0.5) + 
   ylab(idx) 
}


#' @param t input data set
#' @param x a variable in x axis 
#' @param y a continuous variable in y axis
#' @param facet a facet variable  
#' @param facet_value a vector contains value in facet that I want to filter
#' @return a plot show box plot facet by a specific variable

box_plot <- function(t, x, y, facet, facet_value){
  t %>% 
    filter(!!facet %in% facet_value) %>%
    ggplot(mapping = aes(x = !!x, y = !!y))+ 
    geom_boxplot(aes(color = !!x)) +
    facet_grid(expr(`~`(`.`,!!facet)))
}
  