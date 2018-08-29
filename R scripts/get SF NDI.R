library(viridis)
library(psych)
library(tidycensus)


Sys.getenv("CENSUS_API_KEY")

### Deprivation variables 
vars <- c("B17001_002", "B17001_001", "B06009_002" , "B06009_001",
          "B09008_011", "B09008_001","B08124_002", "B08124_001", "B25014_005", 
          "B25014_006",  "B25014_007","B25014_011", "B25014_012", "B25014_013",  
          "B25014_001", "B19058_002", "B19058_001","C23002C_021", "C23002D_008", 
          "C23002C_017", "C23002D_003","B19001_002", "B19001_003", "B19001_004", 
          "B19001_005", "B19001_006", "B19001_001")


### get estimates for all US 
acs_us <- get_acs(geography = "tract",state = "NY", variables = vars,
                  output = "wide") %>%
  mutate(pct_poverty = B17001_002E/B17001_001E,
         pct_noHS = B06009_002E / B06009_001E,
         pct_FHH = B09008_011E / B09008_001E,
         pct_mgmt = B08124_002E /  B08124_001E, 
         pct_crowd =  (B25014_005E +B25014_006E+ B25014_007E + 
                         B25014_011E + B25014_012E + B25014_013E) / B25014_001E,
         pct_pubassist = B19058_002E/B19058_001E,
         pct_unempl = (C23002C_021E + C23002D_008E)  / (C23002C_017E + C23002D_003E),
         pct_under30K =( B19001_002E+B19001_003E+B19001_004E+B19001_005E +
                           B19001_006E) / B19001_001E)

## select transformed variables
values  <-  acs_us %>% select(pct_poverty,pct_noHS,pct_FHH,pct_mgmt,pct_crowd,
                              pct_pubassist, pct_unempl,pct_under30K) %>% as.matrix()
values[is.nan(values)] <- 0
## PCA
ND <- principal(values,nfactors = 1)          
NDI_us <- cbind(acs_us,ND$scores) 

## 
NDI_us <- NDI_us %>% select(NAME,GEOID,PC1) %>% 
  separate(NAME, into = c( "Tract","County","State"), sep = ",")


### counties sf
NY_counties <- get_acs(geography = "tract",state = "NY", variables = c("B01001_001"), 
                       output = "wide", geometry = TRUE)

### merged NDI sf
MapNY <- geo_join(NY_counties,NDI_us, by_sp = "GEOID", by_df = "GEOID")


### state sf
NY_st<- get_acs(geography = "state",state = "NY", variables = c("B01001_001"), 
                       output = "wide", geometry = TRUE)
