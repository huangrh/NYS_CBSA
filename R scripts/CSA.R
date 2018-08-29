
library(tidyverse)
library(tigris)
library(sf)
library(readxl)

metro <- core_based_statistical_areas(cb=TRUE)

NYS <- st_as_sf(metro) %>% separate(NAME, into = c("CBSA","State"), sep = ",") %>%
  filter(State == " NY") 

CSA <-  NYS %>% filter(CSAFP > 0)


#######
CSA2 <- st_join(MapNY, CSA, join = st_within, 
                   left = FALSE) 
### CBSA NDI
ggplot() + 
  geom_sf(data = CSA2,aes(fill = PC1, color = PC1)) +
  geom_sf(data = NY_st, fill = NA) +
  geom_sf(data = CSA, fill = NA, color = "#ffffff",  size = .5) +
  scale_fill_viridis_c(option = "inferno") + 
  scale_color_viridis_c(option = "inferno") + theme_minimal() +
  theme(axis.text = element_blank()) + 
  labs(fill = "", color = "",
       caption = "Higher index scores represent higher tract-level deprivation \nrelative to all census-tracts in NYS.") +
  ggtitle("New York's Core-based Statistical Areas", 
          subtitle = "excluding Tri-State Area")

##### city halls

hall <- read_excel("halls.xlsx", col_types = c("numeric", 
                                               "numeric", "text", "text", "text"))%>%
  st_as_sf(coords = c("X","Y"), crs = 4269)

### distance from tract centroid

dist <- map(c("45060" ,"40380", "36460" ,"28740", "24020" ,"15380", "15380", "12180" ,
  "11220","24100" ,"42900" ,"12860" ,"10580", "10580", "10580", "18500",
  "27060", "18660", "21300" ,"26460"), function (x){
    ct <- CSA2 %>%
      filter(CBSAFP == x)
    halls <- filter(hall, CBSAFP == x)
    dist <- st_distance(
      st_centroid(ct), halls
    )
    mindist <- apply(dist, 1, min)
    out <- mutate(ct, dist = mindist)
    return(out)
  }) %>%
  reduce(rbind) %>%
  distinct()

#### 

dist %>% 
ggplot(aes(x =dist/1000, y = PC1)) +
  geom_smooth(aes(color = CBSA), se = FALSE) +
  geom_hline(yintercept = 0, lty = 1) +
  scale_color_viridis_d() +
 ggtitle("Neighborhood deprivation decreases \nwith distance from urban center")+
  labs(x = "Distance from Principal City Hall (km)", 
       y = "Deprivation Index") +
  theme_ipsum()

### facet wrap CBSA

dist %>% filter(CBSA %in% c("Albany-Schenectady-Troy", "Syracuse", "Buffalo-Cheektowaga-Niagara Falls",
                "Rochester")) %>%
  ggplot(aes(x =dist/1000, y = PC1)) + geom_point() +
  geom_smooth(se = FALSE) +
  theme_minimal() + geom_hline(yintercept = 0, lty = 2) +
  scale_color_viridis_d(option = "plasma") +
  facet_wrap(.~CBSA, nrow = 2)

### color counties

dist %>% filter(CBSA == "Syracuse") %>%
  ggplot(aes(x =dist/1000, y = PC1)) + geom_point(aes(color = County)) +
  geom_smooth() +
  theme_minimal() + geom_hline(yintercept = 0, lty = 2) +
  scale_color_viridis_d(option = "plasma")

## plot function 
plot <- function(x) {dist %>% filter(CBSA == x) %>%
  ggplot(aes(x =dist/1000, y = PC1)) + geom_point(aes(color = County)) +
  geom_smooth() +
  geom_hline(yintercept = 0, lty = 2) +
  scale_color_viridis_d(option = "plasma") +
    ggtitle("Neighborhood deprivation decreases \nwith distance from urban center" ,
            subtitle = paste(x, "Core-based Statistical Area")) +
    labs(x = "Distance from Principal City Hall (km)", 
         y = "Deprivation Index")}


plot("Syracuse") +  theme_ipsum()
plot("Buffalo-Cheektowaga-Niagara Falls") + theme_ipsum()
plot("Albany-Schenectady-Troy") + theme_ipsum() 
plot("Rochester") + theme_ipsum()


### function for loess - all areas

areas <- unique(dist$CBSA)[1:17]

resdf <- map(areas, function(x){
  dat <- dist %>% filter(CBSA == x)
  reg <- loess(PC1~dist, data = dat, span = .5)
  res <- abs(reg$residuals)
  out <- cbind(dat,res)
  return(out)
}) %>%
  reduce(rbind)

####
ggplot() +  geom_sf(data =NY_st, fill = "#e5e5e5") +
  geom_sf(data = resdf, aes(fill = res, color = res)) +
  geom_sf(data = CSA, fill = NA, color  = "#7f7f7f") +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  theme_minimal() + theme(axis.text = element_blank()) +
  labs(fill = "", color = "") +
  ggtitle("Residuals Map", subtitle = "absolute values of LOESS model residuals") 


