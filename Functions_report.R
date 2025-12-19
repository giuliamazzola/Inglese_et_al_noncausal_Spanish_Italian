#### Libraries ####
library(tidyverse)
library(lme4)
library(effects)
library (Hmisc)
library(kableExtra)
library(report)
library(stringr)

#### Data ####

#### Add significance codes ####  
add_signif<-function(model){
  model %>% 
    mutate(
      sign = case_when( # setting the values of Significance based on p
        p <= 0.001 ~ "***",
        p <= 0.01 ~ "**",
        p <= 0.05 ~ "*",
        p <= 0.1 ~ ".",
        TRUE ~ " "
      )
    )
}


#### Extensive report of fixed effects #### 

interaction_pattern <- "^([^\\s]+) +\\[([^\\]]+)\\] +\\* +([^\\s]+) +\\[([^\\]]+)\\]"
interaction_pattern2 <- "^([^\\s]+) +\\* +([^\\s]+) +\\[([^\\]]+)\\]"

make_fixreport<-function(model){
  model %>% 
  report::report(include_effectsize = FALSE) %>% 
    as_tibble() %>% 
    filter(!is.na(p)) %>%
    dplyr::select(-c(Group, Effects, CI, Fit)) %>% 
    mutate(
      SE= arm::se.coef(model)$fixef,
      Coefficient = exp(Coefficient) # the same as modelreport["Coefficient"]....
    )%>%
    add_signif() %>% 
    dplyr::select(Variable = Parameter, OR = Coefficient, SE, CI.low = CI_low, CI.high = CI_high, p, sign) %>% 
    mutate(
      Variable = case_when(
        Variable == "(Intercept)" ~ " $$(Intercept)",
        str_detect(Variable, "\\] +\\*") ~ str_replace(Variable, interaction_pattern, "\\1 * \\3$$\\2 * \\4"),
        str_detect(Variable, "\\*") ~ str_replace(Variable, interaction_pattern2, "\\1 * \\2$$N words * \\3"),
        str_detect(Variable, "\\[") ~ str_replace(Variable, "^([^\\s]+) +\\[([^\\]]+)\\]", "\\1$$\\2"),
        TRUE ~ paste(Variable, "N words", sep = "$$")
      )) %>%
    separate(Variable, into = c("Variable", "Value"), sep = "\\$\\$") %>%
    mutate(
      CI = sprintf("%.2f — %.2f", CI.low, CI.high),
      p = sprintf("%.2f%s", p, sign),
      Variable = sprintf("\\textsc{%s}", str_to_title(Variable))) %>%
    dplyr::select(Variable, Value, OR, SE, CI, p)
}

#### Short report of fixed effects ####

make_smallreport<-function(model){ # Excellent! But you could do it simpler with tidyverse
  model %>% report::report(include_effectsize = FALSE) %>% 
    as_tibble() %>% 
    dplyr::select(Parameter, Coefficient, p) %>% # the same as your "subset()..."
    filter(!is.na(p)) %>% # the same as subsetting with [] to remove rows with NA
    mutate(
      Coefficient = exp(Coefficient)
      ) %>%
    add_signif() %>% 
    dplyr::select(Variable=Parameter,
      OR = Coefficient,p, sign) %>% 
    mutate(
      Variable = case_when(
        Variable == "(Intercept)" ~ " $$(Intercept)",
        str_detect(Variable, "\\] +\\*") ~ str_replace(Variable, interaction_pattern, "\\1 * \\3$$\\2 * \\4"),
        str_detect(Variable, "\\*") ~ str_replace(Variable, interaction_pattern2, "\\1 * \\2$$N words * \\3"),
        str_detect(Variable, "\\[") ~ str_replace(Variable, "^([^\\s]+) +\\[([^\\]]+)\\]", "\\1$$\\2"),
        TRUE ~ paste(Variable, "N words", sep = "$$")
      )) %>%
    separate(Variable, into = c("Variable", "Value"), sep = "\\$\\$") %>%
    mutate(
      p = sprintf("%.2f%s", p, sign),
      Variable = sprintf("\\textsc{%s}", str_to_title(Variable))) %>% 
    dplyr::select(Variable, Value, OR, p)
}



#### Diagnostics Report #### 

make_diagnostics<-function(model,variable) { #variable=dependent variable of the model
  probs <- binomial()$linkinv(fitted(model))
  
  diagn<-Hmisc::somers2(probs, as.numeric(variable)-1) %>% as_tibble(rownames = "Test")
  diagn<-diagn %>% rename(Fit=value)
  report<-model %>% 
    report::report(include_effectsize = FALSE) %>% 
    as_tibble() %>%
    filter(!is.na(Fit)) %>%
    dplyr::select(Test=Parameter, Fit)
    rbind(report,diagn) %>% 
    mutate(Test = replace(Test, Test == "n", "Observations")) %>% 
    dplyr::filter(Test != "AICc")%>%
    dplyr::filter(Test != "Log_loss") %>% 
    dplyr::filter(Test != "Missing") %>% 
    dplyr::filter(Test != "Sigma")%>% 
      mutate(Test = ifelse(Test == "R2 (conditional)", "R$^2$ (conditional)", Test)) %>% 
      mutate(Test = ifelse(Test == "R2 (marginal)", "R$^2$ (marginal)", Test))
}

#### Random effects report ####
make_ranefreport<-function(model){
    model %>% lme4::VarCorr() %>% as_tibble()%>% 
    dplyr::select(Variable=grp, StdDev=sdcor, Variance=vcov) %>% 
    mutate(
      Variable = sprintf("\\textsc{%s}", str_to_title(Variable)))
}

# I will keep each of the functions separate, in case I want to use them independently.
# but also I want a function that makes a list out of the previous functions, in case I want everything in one object

#### VIF report #### 

make_vif<-function(model){
  myvif<-car::vif(model)
  colnames(myvif)<-c("GVIF","Df","corrected GVIF")
  rownames(myvif)<-str_to_title(rownames(myvif))
  rownames(myvif)<-sprintf("\\textsc{%s}", rownames(myvif))
  myvif
}


#### Make big report ####

list_report<-function(model, variable){
 random <- make_ranefreport(model)
 fixed <- make_fixreport(model)
 fixedsmall <- make_smallreport(model)
  diagnostics<- make_diagnostics(model,variable)
  vifreport<-make_vif(model)
list(fixed = fixed,
     fixedsmall = fixedsmall,
     random = random,
     diagnostics = diagnostics,
     vif = vifreport)
}



kbl_appendix <- function(df, caption, longtable = FALSE, font_size = 7, is_fixed = TRUE, full_width=F) {
  latex_options <- if(longtable) c("repeat_header", "HOLD_position") else "HOLD_position"
  k <- kbl(df, escape = FALSE, booktabs = TRUE, caption = caption, longtable = longtable) %>%
    kable_paper(latex_options=latex_options, font_size = font_size, full_width=full_width) %>% 
    row_spec(0, bold = T)
  if (is_fixed) {
    k %>% 
      #column_spec(2, width = "2cm") %>% 
      collapse_rows(1, latex_hline = "major", valign= "top")
  } else {
    k
  }
}


# kbl_appendix <- function(df, caption, longtable = FALSE, font_size = 9, is_fixed = TRUE, ...) {
#   k <- kbl(df, escape = FALSE, booktabs = TRUE, caption = caption, longtable = longtable) %>%
#     kable_paper(...)
#   if (is_fixed) {
#     k %>% column_spec(2, width = "2cm") %>% 
#       collapse_rows(1, latex_hline = "major", valign= "top")
#   } else {
#     k
#   }
# }
# kbl_append(df, caption, font_size = 3)





 