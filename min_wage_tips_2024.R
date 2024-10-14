#==============================================================================
#   Minimum wage project: data wrangling tips
#   ECON 173
#   Bill Sundstrom
#==============================================================================

### Load the Current Population Survey Outgoing Rotation Group data (from where you have saved it)

    load("org_2000_2024.RData")

  # merge the state names using the "statefip" variable
    url <- "https://github.com/wsundstrom/econ173/raw/data/statefips.dta"
    states <- read_dta(url) %>% 
      select(statefip = statefips, state = statename, statecd)
    org <- org %>% 
      left_join(states)

### BLS state minimum wage historical tables

  # This is sample code for a few years... you will have to add more to get all the data you need

    library(rvest)
    
  # Scrape the HTML tables on the following website using rvest package:
    url <- "https://www.dol.gov/agencies/whd/state/minimum-wage/history"

  # Get the tables - this results in a list because there are multiple tables
    minwage <- url %>%
      read_html() %>%
      html_nodes("table") 
    
  # the tables with the years we want (2000-) are the 3rd-6th 
    minwage3 <- minwage %>%     
      .[[3]] %>%                  # 3rd table
      html_table() 
    colnames(minwage3)[1] <- "state"
    minwage4 <- minwage %>%     
      .[[4]] %>%                  
      html_table() 
    colnames(minwage4)[1] <- "state"
    minwage5 <- minwage %>%     
      .[[5]] %>%                  
      html_table() 
    colnames(minwage5)[1] <- "state"
    minwage6 <- minwage %>%     
      .[[6]] %>%                  
      html_table() 
    colnames(minwage6)[1] <- "state"
    
  # minwageL and minwageU are the upper and lower values for states with a range
    mintab <- minwage3 %>% 
      left_join(minwage4) %>% 
      left_join(minwage5) %>% 
      left_join(minwage6) %>% 
      pivot_longer(-state, names_to = "year", values_to = "minwage") %>% 
      separate(minwage, into = c("minwageL", "minwageU"), sep = "-", remove = F) %>% 
      mutate(year = as.numeric(gsub("[^0-9]+", "", year)),
             minwageL = as.numeric(gsub("[^0-9.]+", "", minwageL, perl = TRUE)),
             minwageU = ifelse(is.na(minwageU), minwageL, as.numeric(gsub("[^0-9.]+", "", minwageU, perl = TRUE))),
             minwageL = ifelse(state=="Nevada" & year==2022, 9.50, minwageL),
             minwageU = ifelse(state=="Nevada" & year==2022, 10.50, minwageU)) %>% 
      filter(!(state %in% c("Guam", "Puerto Rico", "U.S. Virgin Islands")))
    
  # assign federal min if min is missing or fed > state min 
    fedmin <- mintab %>% 
      filter(state=="Federal (FLSA)") %>% 
      mutate(fedmin = minwageL) %>% 
      select(year, fedmin)
    mintab <- mintab %>% 
      left_join(fedmin) %>% 
      mutate(minwageL = ifelse(is.na(minwageL) | fedmin>minwageL, fedmin, minwageL),
             minwageU = ifelse(is.na(minwageU) | fedmin>minwageU, fedmin, minwageU)) %>% 
      filter(state!="Federal (FLSA)") %>% 
      select(-minwage) 
    
  # plot example
    min_plot <- mintab %>% 
      filter(state %in% c("California", "Georgia", "New York", "Texas")) %>% 
      ggplot(aes(x = year, y = minwageU, color = state) ) +
      geom_line()
    min_plot    
    
### When you have ALL the minimum wage data, you can merge it to the ORG data
