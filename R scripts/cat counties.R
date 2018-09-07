library(gtools)
library(gridExtra)
library(gtable)


## plot function 
plot2 <- function(x) {dist %>% filter(CBSA == x) %>%
    ggplot(aes(x =dist/1000, y = PC1)) + geom_point(aes(color = County)) +
    geom_smooth() +
    geom_hline(yintercept = 0, lty = 2) +
    scale_color_viridis_d(option = "plasma") +
    ggtitle("" ,
            subtitle = paste(x, "CBSA")) +
    labs(x = "", 
         y = "")}

## plot function II
plotmap <- function(x) {dist %>% filter(CBSA == x) %>%
ggplot() + 
  geom_sf(aes(fill = PC1, color = PC1)) +
  scale_fill_viridis_c(option = "inferno") + 
  scale_color_viridis_c(option = "inferno") + theme_minimal() +
  theme(axis.text = element_blank()) + 
  labs(fill = "", color = "") + guides(color = FALSE,fill = FALSE) +
  ggtitle("", subtitle = paste(x))}


a <- plot("Syracuse") + theme_ipsum()
b  <- plot("Buffalo-Cheektowaga-Niagara Falls") + theme_ipsum()
c <- plot("Albany-Schenectady-Troy") + theme_ipsum() 
d <- plot("Rochester") + theme_ipsum()


grid.arrange(a,b,c,d, ncol = 2,
             left = "Desprivation Index Score",
             bottom = "Distance from Principal City Hall (km)")

syr <- plotmap("Syracuse")
ros <- plotmap("Rochester")
alb <- plotmap("Albany-Schenectady-Troy")
buf <- plotmap("Buffalo-Cheektowaga-Niagara Falls")

grid.arrange(syr,ros,alb,buf, ncol = 2)

#### 
dist %>% filter(CBSA %in% c("Albany-Schenectady-Troy", "Syracuse", "Buffalo-Cheektowaga-Niagara Falls",
                            "Rochester")) %>% mutate(quant = quantcut(PC1, q=10)) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot()

dist %>%  mutate(quant = quantcut(PC1)) %>% 
  filter(CBSA %in% c("Albany-Schenectady-Troy", "Syracuse", "Buffalo-Cheektowaga-Niagara Falls",
                     "Rochester")) %>%
  ggplot(aes(quant,dist/1000)) + geom_boxplot()


dist %>% filter(CBSA %in% c("Albany-Schenectady-Troy", "Syracuse", "Buffalo-Cheektowaga-Niagara Falls",
                            "Rochester")) %>% mutate(quant = quantcut(PC1, q=5)) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot(aes(color = CBSA))


#####
dist %>% filter(CBSA == "Albany-Schenectady-Troy") %>%
  mutate(quant = quantcut(PC1, q=4),
        Type = ifelse(County %in% c(" Albany County"," Schenectady County"," Rensselaer County"),"Core County","Commuting County")) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot(aes(color = Type))


dist %>% filter(CBSA == "Syracuse") %>%
  mutate(quant = quantcut(PC1, q=4),
         Type = ifelse(County == " Onondaga County","Core County","Commuting County")) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot(aes(color = Type))



dist %>% filter(CBSA == "Buffalo-Cheektowaga-Niagara Falls") %>%
  mutate(quant = quantcut(PC1, q=4)) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot()


dist %>% filter(CBSA == "Rochester") %>%
  mutate(quant = quantcut(PC1, q=4),
         Type = ifelse(County == " Monroe County","Core County","Commuting County")) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot(aes(color = Type))

#### Core / Comm Counties boxplot ######

dist %>% filter(CBSA %in% c("Albany-Schenectady-Troy", "Syracuse", "Buffalo-Cheektowaga-Niagara Falls",
                            "Rochester")) %>% 
  mutate(quant = quantcut(PC1, q=4),
         Type = ifelse(County %in% c(" Monroe County"," Onondaga County",
                                     " Albany County"," Schenectady County"," Rensselaer County",
                                     " Erie County", " Niagara County"), "Core County", "Commuting County")) %>% 
  ggplot(aes(quant,dist/1000)) + geom_boxplot(aes(color = Type)) +
  scale_x_discrete(labels = c("Least Deprived", "", "", "Most Deprived")) +
  labs(color = "", x = "", y = "Distance from Prinicpal City Hall (km)") +
  scale_color_viridis_d(option = "viridis")


  

