
# ploting ----

### one var barplot
one_var_barplot <- function(dt, x, y, label = TRUE){
  if(label){
    ggplot(dt) +
      aes_string(
        x = x,
        y = y
      ) +
      geom_bar(stat = "identity") +
      geom_text(aes_string(label=y), vjust=-0.5, fontface='bold') +
      theme_classic() + 
      scale_y_continuous(
        expand = c(0, NA)
      ) +
      coord_cartesian(clip = 'off')
  }
  else{
    ggplot(dt) +
      aes_string(
        x = x,
        y = y
      ) +
      geom_bar(stat = "identity") +
      theme_classic() + 
      scale_y_continuous(
        expand = c(0, NA)
      ) +
      coord_cartesian(clip = 'off')
  }
}

two_var_barplot <- function(dt, x, y, f, stack = TRUE){
  if(stack){
    ggplot(dt) +
      aes_string(
        x = x,
        y = y,
        fill = f
      ) +
      geom_bar(stat = "identity",
               width = 0.8,
               position = position_dodge2(preserve = "single")) +
      geom_text(aes_string(label=y), 
                position = position_dodge(width = .8),
                vjust=-0.5, fontface='bold') +
      theme_classic() + 
      scale_y_continuous(
        expand = c(0, NA)
      ) +
      coord_cartesian(clip = 'off') +
      theme(
        legend.position = "top",
        legend.background=element_blank()
      )
    
  }
  else{
    ggplot(dt) +
      aes_string(
        x = x,
        y = y,
        fill = f
      ) +
      geom_bar(stat = "identity", width = 0.7) +
      theme_classic() + 
      coord_cartesian(clip = 'off') +
      theme(
        legend.position = "top",
        legend.background=element_blank()
      )
    
  }
}

# data ----
had_affairs_rate <- function(by_col){
  res <- 
    affairs[, .N, by = c("had_affairs", by_col)][
      ,sum:= sum(N), by = by_col][
        ,had_affairs_rate:=N/sum][had_affairs=="yes"][,had_affairs:=NULL][,sum:=NULL]
  
  return(res)
}
