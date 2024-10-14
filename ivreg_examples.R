
#=================================================================================
#   Using ivreg
#=================================================================================

  # Clear the working space
    rm(list = ls())
  
  # The following is the path to your desired working directory:
  # Change just this command line (#24) and then skip down to line #73
  
    MyDir = "/your path/Econ173"
  
### Leave the rest of this chunk alone, unless you need to add a package
  
  # Sundstrom directory (leave this alone!)
    SundstromDir <- "/Users/wsundstrom/Library/CloudStorage/GoogleDrive-wsundstrom@scu.edu/My Drive/Econ173"  

  # Choose correct working directory and proceed
    if (file.exists(SundstromDir)){
      WorkDir = SundstromDir
    } else {
      WorkDir = MyDir
    }
    
  # Set working directory 
    setwd(WorkDir)
    
### Load the packages (all must have been installed)
    library(knitr)
    library(AER)
    library(sandwich)
    library(tidyverse)  
    library(plm)
    library(haven)
    library(readxl)
    library(modelsummary)
    library(lfe)
    library(ivreg)
    
### More settings
  
  # Root directory for knitr
    opts_knit$set(root.dir = WorkDir)
  
  # turn off scientific notation except for big numbers
    options(scipen = 9)
  # set font size for qplot (default is 12)
    theme_set(theme_gray(base_size = 12))

### Read the data 

  fert <- "https://github.com/wsundstrom/econ173/raw/data/fert_80.RData"
  load(url(fert))
  fert <- fert_80 %>% 
    mutate(samesex = as.numeric(female1==female2),
           morekids = as.numeric(numkids>=3),
           work = as.numeric(weeks>0))

### Run regressions
  
  # iv_robust syntax
  # the general set-up is ivreg(Y ~ X + W | Z + W, ... )
  # where Y = outcome variable, X = endogenous var(s)
  # W = any exogenous var(s) not including instruments, and Z = the instrument(s)
  
  ols <- lm(work ~ morekids, data=fert)
  reg1st <- lm(morekids ~ samesex, data=fert)    # 1st stage regression
  iv1 <- ivreg(work ~ morekids | samesex, data=fert)
  iv2 <- ivreg(work ~ morekids + age | samesex + age, data=fert)
  
  models1 <- list("1st stage" = reg1st, "OLS" = ols, "IV1" = iv1, "IV2" = iv2)
  modelsummary(models1,
               vcov = "robust",
               stars=T, gof_map = c("nobs", "r.squared"),
               title = "Regressions",
               output="default")

### IV diagnostics
  # In the following summary command, several diagnostic tests are performed:
  # Weak instruments: Test on instruments in first stage. We hope to reject.
  # Wu-Hausman: tests null that OLS is consistent. If we do not reject, 
    # 2SLS is not necessary, OLS is probably better.   
  # Overidentifying restrictions test: uses overidentifying restrictions to test instrument exogeneity.
    # This requires that we have more instruments (Z) than endogenous variables (X)
  # vcov=sandwich gives heteroskedastic-robust SEs  
  
  summary(iv1)
  