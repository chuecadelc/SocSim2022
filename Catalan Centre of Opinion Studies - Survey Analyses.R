#################################################

########### SURVEY DATA FOR SIMPE ###############

#################################################

## Required libraries ##

require(haven)
require(sjlabelled)
require(reshape2)
require(tidyverse)



## Importing the various data sets from https://ceo.gencat.cat/ca/barometre/ ##
# All data sets are 1a onada for each of the years.

data11 <- read_sav("~/CatOpSurveys/Encuesta 2011.sav")
data12 <- read_sav("~/CatOpSurveys/Encuesta 2012.sav")
data13 <- read_sav("~/CatOpSurveys/Encuesta 2013.sav")
data14 <- read_sav("~/CatOpSurveys/Encuesta 2014.sav")
data15 <- read_sav("~/CatOpSurveys/Encuesta 2015.sav")
data16 <- read_sav("~/CatOpSurveys/Encuesta 2016.sav")
data17 <- read_sav("~/CatOpSurveys/Encuesta 2017.sav")
data18 <- read_sav("~/CatOpSurveys/Encuesta 2018.sav")
data19 <- read_sav("~/CatOpSurveys/Encuesta 2019.sav")
data20 <- read_sav("~/CatOpSurveys/Encuesta 2020.sav")
data21 <- read_sav("~/CatOpSurveys/Encuesta 2021.sav")
data22 <- read_sav("~/CatOpSurveys/Encuesta 2022.sav")



## Extracting the relevant collumns regarding National Identity in Catalonia ##

data <- cbind(data11$P25, data12$P25,data13$P25,data14$C700,data15$C700, data16$C700,
              data17$C700, data18$C700,data19$P68, data20$P66, data21$SENTIMENT_PERTINENCA, data22$SENTIMENT_PERTINENCA)
data <- as.data.frame(data)

# Assigning them names
colnames(data)<- c("2011","2012","2013","2014","2015","2016","2017","2018","2019",
                    "2020","2021","2022")
str(data)

## Creating frequencies per year fir each of the 5 categories ##

data1 <- data %>%
pivot_longer(cols = starts_with("20"), names_to = "Year",
             values_to = "value", values_drop_na = FALSE) %>%
  filter(value < 95, value > 0) %>% 
  mutate(value = as.factor(value)) %>% 
  group_by(Year,value)%>%
  summarize(n= n())%>%
  mutate(freq = n / sum(n)*100)

## Plotting the outcome ## 

labels <- c("Solely Spanish","More Spanish than Catalan","Equally Spanish and Catalan","More Catalan than Spanish","Solely Catalan")

## Stacked bar-chart 

data1 %>%
  ggplot(aes(
    x = Year,
    freq,
    group = value,
    color = value
  )) +
  geom_line() +
  scale_color_brewer(palette = "Dark2",
                     name = "I feel...",
                     labels = labels) +
  theme_classic() +
  labs(title = "Changes in National Identification in Catalonia (2011-2022)",
       y = "Prop. of individiduals",
       x = "Years") +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +  guides(color = guide_legend(nrow = 2, byrow = TRUE))



## Time series 

data1 %>%
  ggplot(aes(
    x = Year,
    freq,
    group = value,
    color = value
  )) +
  geom_line() +
  scale_color_brewer(palette = "Dark2",
                     name = "I feel...",
                     labels = labels) +
  theme_classic() +
  labs(title = "Changes in National Identification in Catalonia (2011-2022)",
       y = "Prop. of individiduals",
       x = "Years") +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +  guides(color = guide_legend(nrow = 2, byrow = TRUE))


##########################################################################################
##########################################################################################

## Extracting the relevant collumns regarding Secessionist Support in Catalonia ##

dataS <- cbind(data11$P28, data12$P28,data13$P28,data14$P30,data15$P30, data16$P30,
               data17$P30, data18$P30,data19$P33, data20$P33, data21$RELACIONS_CAT_ESP, data22$RELACIONS_CAT_ESP)
               
dataS <- as.data.frame(dataS)

# Assigning them names

colnames(dataS)<- c("2011","2012","2013","2014","2015","2016","2017","2018","2019",
                    "2020","2021","2022")
str(dataS)

## Creating frequencies per year fir each of the 4 categories ##

dataS1 <- dataS %>%
pivot_longer(cols = starts_with("20"), names_to = "Year",
             values_to = "value", values_drop_na = FALSE) %>%
  filter(value < 95, value > 0) %>% 
  mutate(value = as.factor(value)) %>% 
  group_by(Year,value)%>%
  summarize(n= n())%>%
  mutate(freq = n / sum(n)*100)

## Plotting the outcome ## 

labels1 <- c("A region of Spain", "An Autonomous Community of Spain", "A state within a federal Spain", "An independent state")

## Stacked bar-chart 

dataS1 %>%
  ggplot(aes(
    x = Year,
    freq,
    group = value,
    color = value
  )) +
  geom_line() +
  scale_color_brewer(palette = "Dark2",
                     name = "Catalonia should be ",
                     labels = labels1) +
  theme_classic() +
  labs(title = "Changes in Secessionist support in Catalonia (2011-2022)",
       y = "Prop. of individiduals",
       x = "Years") +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +  guides(color = guide_legend(nrow = 2, byrow = TRUE))



## Time series 

dataS1 %>%
  ggplot(aes(
    x = Year,
    freq,
    group = value,
    color = value
  )) +
  geom_line() +
  scale_color_brewer(palette = "Dark2",
                     name = "Catalonia should be",
                     labels = labels1) +
  theme_classic() +
  labs(title = "Changes in Secessionist support in Catalonia (2011-2022)",
       y = "Prop. of individiduals",
       x = "Years") +
  theme(
    legend.key.size = unit(1, 'cm'),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  ) +  
  guides(color = guide_legend(nrow = 2, byrow = TRUE))



