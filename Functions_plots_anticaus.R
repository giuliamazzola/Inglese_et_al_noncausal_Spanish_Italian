library(tidyverse)


#### Palettes ####

theme_set(theme_minimal(base_size = 14) +
            theme(legend.position = "bottom", 
                  legend.key.width= unit(1, 'cm'), 
                  text = element_text(colour = "black"), 
                  axis.text = element_text(colour = "black"),
                  legend.title=element_text(face = "bold") 
                  )
          )

# 

safe <- c("red", "#f1947a","#c00020", "#9a9a8c", "#f7d176", "#d26675",  "#0165fc",  "#FB8072", "#117733", "#CCEBC5",  "#BEBADA", "#245C73",  "#EEB4B4", "#888888")

#safe12 <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
 #                            "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
#colorBlindness::cvdPlot(scales::show_col(safe12))

#safe_old <- c("#404080", "#69b3a2", "#f4be3b","#CC6677", "#94C973",  "#882255", "#D55E00","#AA4499", "#0165fc", "#EEB4B4", "#EFF2F1", "#888888")
#colorBlindness::cvdPlot(scales::show_col(safe))

#colorBlindness::cvdPlot(scales::show_col(palette.colors(palette = "Set 3")))


##### makeff #### 
# it transforms the effect(s) into a tibble and reorder the effects based on their strength 
makeff<-function(model, var=""){
  ggeffects::ggeffect(model, terms=var) %>% 
    as_tibble()
}

makeff2<-function(model, var=""){
  ggeffects::ggpredict(model, terms=var) %>% 
    as_tibble()
}

#### simpleff ####
#  to plot simple effects 
simpleff <- function(model,var,
                     xtitle= stringr::str_to_upper(var), 
                     ytitle= "Probability of SE", 
                     title= stringr::str_to_upper(var), 
                     color=safe[1]) {
  mydf<-makeff(model=model, var=var) %>% 
    arrange(predicted) %>%
    mutate(x = fct_reorder(x, predicted))
  mydf %>% ggplot(aes(x=x, y=predicted, group=1)) +
    geom_line(size=0.7,color=color)+
    geom_point(size=1.5,color=color)+
    labs(x = xtitle, y = ytitle, title = title)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                       labels = scales::percent,
                       limits = c(0,1))
}


simpleff2 <- function(model,var,
                     xtitle= stringr::str_to_upper(var), 
                     ytitle= "Probability of SE", 
                     title= stringr::str_to_upper(var), 
                     color=safe[1]) {
  mydf<- ggeffects::ggpredict(model, terms=var) %>% 
    as_tibble() %>% 
    arrange(predicted) %>%
    mutate(x = fct_reorder(x, predicted))
  mydf %>% ggplot(aes(x=x, y=predicted, group=1)) +
    geom_line(size=0.7,color=color)+
    geom_point(size=1.5,color=color)+
    labs(x = xtitle, y = ytitle, title = title) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                       labels = scales::percent, 
                       limits = c(0,1))
}

simpleff3 <- function(model,var,
                      xtitle= stringr::str_to_upper(var), 
                      ytitle= "Probability of SE", 
                      title= stringr::str_to_upper(var), 
                      color=safe[1]) {
  mydf<- ggeffects::ggpredict(model, terms=var) %>% 
    as_tibble()
  mydf %>% ggplot(aes(x=x, y=predicted, group=1)) +
    geom_line(size=0.7,color=color)+
    geom_point(size=1.5,color=color)+
    labs(x = xtitle, y = ytitle, title = title) + 
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                       labels = scales::percent, limits = c(0,1))
}



#### diacsmooth ####
# is used to plot diachronic effects (time in interaction with a simple effect) 
# with a smooth line (so you can use it with any interaction between a categorical and a numeric variable) 


diacsmooth <- function(model, var,
                       xtitle= "Real Time", 
                       ytitle="Probability of SE", 
                       grouptitle=stringr::str_to_upper(var[[2]]),
                       point_size = 2, 
                       title="") {
  mydf<-makeff(model=model, var=var)
  mydf %>% ggplot(aes(x=x, y=predicted, group=group)) +
    geom_smooth(aes(color=group, linetype=group), se=F)+
    labs(x = xtitle, y = ytitle, colour=grouptitle, linetype=grouptitle, title = title)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 9)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::percent, limits = c(0,1))+
    scale_color_manual(values = safe)+
    #scale_linetype_manual(values=c("solid", "longdash", "twodash", "dotted", "dashed", "dotdash"))+
    theme(legend.key.width= unit(1.3, 'cm'), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
}

interplot <- function(model, var,
                      xtitle= stringr::str_to_upper(var[[1]]), 
                      ytitle="Probability of SE", 
                      grouptitle=stringr::str_to_upper(var[[2]]),
                      point_size = 2, 
                      title="") {
  mydf<-makeff(model=model, var=var)
  mydf %>% ggplot(aes(x=x, y=predicted, group=group)) +
    geom_line(size=1,aes(color=group, linetype=group))+
    geom_point(size=3,aes(color=group, shape=group))+
    labs(x = xtitle, y = ytitle, colour=grouptitle, linetype=grouptitle, shape=grouptitle, title = title)+
    scale_color_manual(values = safe)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = scales::percent, limits = c(0,1))+
    theme(legend.key.width= unit(1.3, 'cm'), legend.text = element_text(size = 12), legend.title = element_text(size = 12))
}

get_ranef <- function(model, var){
  ranef_data <- lme4::ranef(model)[[var]]
  ranef_data %>% 
    as_tibble(rownames = "Variable") %>% 
    rename(Intercept = `(Intercept)`) %>% 
    ggplot(aes(y = Intercept, x = reorder(Variable, Intercept), fill=Intercept>0 )) + 
    geom_bar(stat="identity",
             position = position_dodge(0.9),
             alpha=0.8,
             colour="#e9ecef") +
    scale_fill_manual(name = '', values = setNames(safe[c(2,1)],c(F, T)), labels = c( "zero", "anticausative"))+
    labs(y = "Log-odds", x=stringr::str_to_upper(var)) +
    coord_flip() +
    theme_minimal(base_size = 9)
    
}


percplot<-function(var, altern){#var=variable, altern=alternation (values in the legend to be compared)
  df<-table(var, altern) %>% as.data.frame()
  colnames(df)<-c("var", "altern", "freq")
  ggplot(df, aes(y=freq, x=var, fill=altern)) + 
    geom_bar(position="fill", stat="identity", alpha=0.8)+ 
    scale_y_continuous(labels = scales::percent)+
    theme(legend.title = element_blank())+
    labs (x="", y= "")+ coord_flip()+
    scale_fill_manual(values=safe[c(2,1)]) 
}



