# Generate bar chart of the result (act as map legend).

# Find the min, max and IQR counties for net_trump to label bar chart.
x_labels <- trump %>%
  mutate(rank=row_number(net_trump)) %>%
  filter(rank %in% c(min(rank), round(quantile(rank, 0.25)), round(median(rank)), round(quantile(rank, 0.75)), max(rank))) %>%
  mutate(net_trump=round(net_trump*100),
         xlab=paste0(abs(net_trump),"%", ifelse(net_trump>0, "\nTrump", "\nClinton"))) %>%
  select(rank, xlab)
# Remove geometry field.
st_geometry(x_labels) <- NULL

# Create bar chart legend and store ggplot2 object.
trump_legend <-
  ggplot()+
  geom_col(data=trump %>% mutate(rank=row_number(net_trump)),aes(x=rank, y=net_trump, fill=net_trump), width=1)+
  geom_text(data=x_labels, aes(x=rank, y=-0.7, label=xlab), size=2)+
  scale_fill_distiller(palette = "RdBu", direction=-1, guide="none")+
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank())+
  coord_flip()+
  labs(title="Net vote share by US county in 2016 Presidential Election",
       subtitle="Trump:Red - Red:Blue")

# Generate a reference map of US states.
# To do this, dissolve county data by state to generate state boundaries and centroids.
# sf package is well-integrated with tidyverse syntax and so we can summarise
# over sf list-cols in the same way as we might a 'regular' column.
# First dissolve to state.
state_data <- trump %>%
  mutate(is_trump=ifelse(net_trump>0,1,0)) %>%
  group_by(state_abbr) %>%
  summarise(total_pop=sum(total_pop))
# Then calculate centroids for map labelling. st_coords() generates a matrix. Re-cast to
# tibble and add state_abbr variable for joining
state_centroids <-
  st_centroid(state_data) %>%
  st_coordinates() %>%
  as_tibble %>%
  rename("lon"="X", "lat"="Y") %>%
  add_column(state_abbr=state_data %>% pull(state_abbr))
# Join state_data and state_centroids.
state_data <- state_data %>% inner_join(state_centroids)
# Clean workspace by removing unnecessary tibble.
rm(state_centroids)

# Create choropleth and store ggplot2 object. Notice that we have two calls to geom_sf(),
# supplying the county and state data separately.
trump_choropleth <-
  ggplot()+
  geom_sf(data=trump, aes(fill=net_trump), colour="#bdbdbd", size=0.05)+
  geom_sf(data=state_data, fill="transparent", colour="#636363", size=0.3)+
  coord_sf(crs=st_crs(trump), datum=NA)+
  scale_fill_distiller(palette="RdBu", direction=-1, guide=FALSE, limits=c(-0.95,0.95), name="")

# Create state reference map and store ggplot2 object.
# Notice that we affect the state labels according to population size.
state_map <-
  state_data %>%
  ggplot()+
  geom_sf(fill="transparent", colour="#636363", size=0.08)+
  geom_text(data=state_data, aes(x=lon, y=lat, label=state_abbr, size=total_pop), colour="#252525", alpha=0.4, show.legend=FALSE)+
  coord_sf(crs=st_crs(state_data), datum=NA)+
  scale_size_continuous(range = c(1.5,4))+
  theme(axis.title=element_blank())+
  labs(
    caption="Data collated by github.com/tonmcg/")

# We need to assemble these ggplot2 objects in a single view.
# The gridExtra package supports this view composition.
library(gridExtra)
out <- grid.arrange(trump_choropleth, trump_legend, state_map,
                    widths = c(0.15, 0.6, 0.25), heights=c(0.15, 0.3, 0.45, 0.1),
                    layout_matrix = rbind(c(2,1,NA), c(2,1,NA), c(2,1,3), c(NA,1,3)))

# Save output as a .png to your local directory
ggsave("./choropleth_production.png",plot = out, width=14, height=7)
