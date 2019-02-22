map_scale <- function(value, min1, max1, min2, max2) {
  return  (min2+(max2-min2)*((value-min1)/(max1-min1)))
}

build_region_scatter <- function(local_data, x_var, y_var, region) {
  local_data <- local_data %>%
    group_by(!!region) %>%
    mutate(
      x_var=!!x_var,
      y_var=!!y_var,
      x_var = map_scale(x_var, min(x_var), max(x_var), 0,1)) %>%
    ungroup() 
  
  region_data <- local_data %>%
    gather(c(x_var),key = "var", value="value") %>%
    group_by(!!region, var) %>%
    summarise(correlation=round(stats::cor(y_var, value),2),
              gridX=first(gridX), gridY=first(gridY)) %>%
    group_by(var) %>%
    ungroup() %>%
    mutate(abs_max=max(abs(correlation), na.rm=TRUE)) %>%
    spread(var, correlation)
  
  extent <- local_data %>%
    mutate(ymin=min(y_var), ymax=max(y_var)) %>%
    group_by(!!region) %>%
    summarise(xmin=min(x_var), xmax=max(x_var), ymin=first(ymin), ymax=first(ymax))
  labels <- region_data %>%
    left_join(extent) %>%
    mutate(x_var=x_var, abs_max=max(abs(x_var), na.rm=TRUE))
  
  plot <-
    ggplot()+
    geom_rect(data=labels,aes(xmin=xmin, xmax=xmax,ymin=ymin,ymax=ymax, fill= x_var), alpha=0.3)+
    scale_fill_distiller(palette="PRGn", type="div", direction=1, guide="none", limits=c(-first(labels$abs_max),first(labels$abs_max)))+
    geom_point(data=local_data, aes(x=x_var, y=y_var), colour="#525252",pch=21, alpha=0.2) +
    stat_smooth(data=local_data, aes(x=x_var, y=y_var), method=lm, se=FALSE, size=0.6, colour="#525252")+
    facet_grid(gridY~gridX, shrink=FALSE, scales = "free_x")+
    theme(
      plot.title = element_text(face="plain", size=16),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid=element_blank(),
      panel.background = element_blank(),
      strip.text=element_blank(),
    )+
    labs(title= gsub("\\_", " ",quo_name(x_var)))
  return(plot)
}