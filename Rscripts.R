##### Part 1. Import data

##### Install required package for data analysis

##### Check if recquired packages for data analysis are installed and install if not installed

##WDI package to download World Bank development indicators

if (!require("WDI",character.only = TRUE)) {
  install.packages("WDI",dep=TRUE)
}

#Tidyverse package for data manaipulation and data visualization

if (!require("tidyverse",character.only = TRUE)) {
  install.packages("tidyverse",dep=TRUE)
}


# package to generate PDF output
if (!require("tinytex",character.only = TRUE)) {
  tinytex::install_tinytex()
}

##### Download the data set (rquire internet!)
data<-WDI(country = "all",indicator = "SP.DYN.LE00.IN",start = 1960)

##### Part 2. Clean data

##### Check for variables types
str(data)

#Summarize data
summary(data)

# rename life expectency at birth

names(data)[3]<-"exp"

#Check for missing values

sum(is.na(data$exp))

# Add missing values per country to data set

data<-data %>% 
  group_by(country) %>% 
  mutate(nb_c = sum(is.na(exp),na.rm=TRUE))

# visualize number of missing values per country
data %>% 
  distinct(country, .keep_all=TRUE)%>%
  filter(nb_c>2)%>%
  ggplot(aes(x=reorder(country,nb_c), y=nb_c))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="Country", y="Number of missing values", fill="")

# More details on missing values
# Israel
data %>% 
  filter(country=="Israel" & is.na(exp))%>% 
  print(n = nrow(.))


# Greenland
data %>% 
  filter(country=="Greenland" & is.na(exp))%>% 
  print(n = nrow(.))


# Kosovo
data %>% 
  filter(country=="Kosovo" & is.na(exp))%>% 
  print(n = nrow(.))

# St. Martin (French part)
data %>% 
  filter(country=="St. Martin (French part)" & is.na(exp))%>% 
  print(n = nrow(.))

# Faroe Islands
data %>% 
  filter(country=="Faroe Islands" & is.na(exp))%>% 
  print(n = nrow(.))

# West Bank and Gaza
data %>% 
  filter(country=="West Bank and Gaza" & is.na(exp))%>% 
  print(n = nrow(.))

# Liechtenstein
data %>% 
  filter(country=="Liechtenstein" & is.na(exp))%>% 
  print(n = nrow(.))

# Bermuda
data %>% 
  filter(country=="Bermuda" & is.na(exp))%>% 
  print(n = nrow(.))

# Serbia
data %>% 
  filter(country=="Serbia" & is.na(exp))%>% 
  print(n = nrow(.))

# Curacao ****
data %>% 
  filter(country=="Curacao" & is.na(exp))%>% 
  print(n = nrow(.))

# Dominica ****
data %>% 
  filter(country=="Dominica" & is.na(exp))%>% 
  print(n = nrow(.))

# select countries that have no more than 40 missing values (Serbia value)
life<-data %>% 
  filter(nb_c<=40)


# Checking for outliers using quantile function

q<-quantile(life$exp,probs = c(0.025,0.975),na.rm=TRUE)
print(q)

# Identify country out of range

#Examine outliers (lowest values)
life %>% 
  filter(exp<q[1])%>% 
  arrange(exp)%>% 
  print(n = nrow(.))

#Examine outliers (higest values)
life %>% 
  filter(exp>q[2])%>% 
  arrange(-exp)%>% 
  print(n = nrow(.))

##### ##### Part 3. Data modeling for SUB-SAHARAN AFRICA

data %>%
  select(country,exp,year)%>%
  filter((country=="Sub-Saharan Africa"|country=="Rwanda")& year<max(year)-1)%>%
  ggplot()+
  geom_line(aes(x = year, y = exp,color=country),size=1)+
  ggtitle("Figure 1. Life expectancy at birth in Sub Saharan Africa since 1960")+
  labs(x="year", y="Life expectancy (years)")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(plot.title = element_text(face = "bold"))


