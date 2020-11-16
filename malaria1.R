

require(tidyr)
require(dplyr)
require(ggplot2)
require(plyr) #for count function
require(plotly)
require(viridis)

setwd('C:/Users/Anika/Documents/GradSchool/Portfolio/Malaria/')

data <- read.csv('Data/IHME_MALARIA_2000_2019_MORT_ADMIN0_Y2020M08D31.csv')

head(data)
glimpse(data)

#only include countries that have mean mortality above 1e-5
#plot countries that increased in one group and those that decreased in another group


# mini data to play around with
data_mini <- data %>% dplyr::filter(ADM0_Name %in% c("Rwanda","Mali"))

# remove rows where don't have mortality data
data_nona <- data[!is.na(data$mean),]

# remove countries that don't have mortality at 1e-5 at some point in time
countries_keep <- data_nona %>% 
  dplyr::group_by(ADM0_Name) %>% 
  dplyr::summarize(max_mort = max(mean,na.rm=T)) %>% 
  dplyr::mutate(keep = case_when(max_mort > 1e-4 ~ 1,
                          max_mort <= 1e-4 ~ 0)) %>%
  dplyr::filter(keep == 1)
countries_keep <- countries_keep$ADM0_Name

data_filtered <- data_nona %>% 
  dplyr::filter(ADM0_Name %in% countries_keep)

# calculate whether rate has increased or decreased (for 0 to 4 year olds)
countries_rate <- data_filtered %>% 
  dplyr::filter(age_group_id == "0_to_4") %>%
  dplyr::group_by(ADM0_Name) %>%
  dplyr::mutate(first_rate = first(mean),
         last_rate = last(mean)) %>%
  dplyr::mutate(rate_change = last_rate - first_rate) %>%
  mutate(change = case_when(rate_change > 0 ~ 1,
                            rate_change <= 0 ~ -1))
countries_rate <- countries_rate %>% 
  group_by(ADM0_Name) %>%
  slice(1) %>%
  select(ADM0_Name, rate_change, change)
countries_rate$change <- as.factor(countries_rate$change)
  
plyr::count(countries_rate, 'change')

data_fil <- merge(data_filtered, countries_rate, by="ADM0_Name", all.x=T)

# separate data by continent
library(countrycode)
data_fil$continent <- countrycode(sourcevar = data_fil[, "ADM0_Name"],
                            origin = "country.name",
                            destination = "continent")

# factor level order
data_fil$age_group_name <- factor(data_fil$age_group_name, 
                                  levels = c("0 to 4 years", "5 to 14 years", "15 plus"))

data_fil <- data_fil %>% dplyr::rename(Country = ADM0_Name) 
country_order <- countries_rate %>% dplyr::arrange(rate_change) %>% select(ADM0_Name)
country_order <- as.vector(country_order$ADM0_Name)
data_fil$Country <- factor(data_fil$Country,
                           levels = country_order)


#Asia
data_fil_asia <- data_fil %>% dplyr::filter(continent == "Asia")
asia <- ggplot(data_fil_asia, aes(x = year, y = mean)) +
  geom_point(aes(color = Country, order = rate_change)) +
  geom_line(aes(color = Country, order = rate_change)) +
  facet_wrap(~age_group_name) +
  scale_color_viridis(discrete=TRUE) +
 # facet_grid(rows = vars(ADM0_Name), 
  #           cols = vars(age_group_name)) + 
             #  scales="free",
             #labeller = label_both) +
  labs(colour = "Country",
       x = "Year",
       y = "Mortality Rate",
       title = " ",
       subtitle = 'Mortality due to malaria, Asia') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))
asia
asaia_p <- ggplotly(asia, tooltip = "Country")

library(htmlwidgets)
saveWidget(asaia_p, file = "malaria_asia.html", selfcontained = TRUE)


#Africa
data_fil_africa <- data_fil %>% dplyr::filter(continent == "Africa")
africa <- ggplot(data_fil_africa, aes(x = year, y = mean)) +
  geom_point(aes(color = Country, order = rate_change)) +
  geom_line(aes(color = Country, order = rate_change)) +
  facet_wrap(~age_group_name) +
  scale_color_viridis(discrete=TRUE) +
  # facet_grid(rows = vars(ADM0_Name), 
  #           cols = vars(age_group_name)) + 
  #  scales="free",
  #labeller = label_both) +
  labs(colour = "Country",
       x = "Year",
       y = "Mortality Rate",
       title = " ",
       subtitle = 'Mortality due to malaria, Africa') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))
africa
africa_p <- ggplotly(africa, tooltip = "Country")

saveWidget(africa_p, file = "malaria_africa.html", selfcontained = TRUE)


##### add GDP data -----------------------
# https://cengel.github.io/gearup2016/worldbank.html
library(wbstats)
new_wb_cache <- wbcache() 

#query data
wbsearch("gdp.*capita.*US\\$", cache = new_wb_cache)
wbsearch("life expectancy at birth.*total", cache = new_wb_cache)
wbsearch("^mortality.*rate.*infant", cache = new_wb_cache)

wb_dat <- wb(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN")) 
names(wb_dat)

#clean up
wb_countries <- wbcountries() 
names(wb_countries)

wb_dat <- merge(wb_dat, y = wb_countries[c("iso2c", "region")], by = "iso2c", all.x = TRUE)

wb_dat <- subset(wb_dat, region != "Aggregates") # this also removes NA

wb_dat$indicatorID[wb_dat$indicatorID == "NY.GDP.PCAP.KD"] <- "GDP"
wb_dat$indicatorID[wb_dat$indicatorID == "SP.DYN.LE00.IN"] <- "life_expectancy"
wb_dat$indicatorID[wb_dat$indicatorID == "SP.DYN.IMRT.IN"] <- "infant_mortality"

library(reshape2)
wb_dat <- dcast(wb_dat, iso2c + country + date + region ~ indicatorID,  value.var = 'value')

ggplot(subset(wb_dat, date == "2008"), aes(x = GDP, y = infant_mortality)) + geom_point()


data_fil_gdp <- merge(data_fil, wb_dat, by.x=c("Country", "year"), by.y=c("country", "date"), all.x=T, all.y=F)
data_fil_gdp <- data_fil_gdp[!is.na(data_fil_gdp$continent),]


### plotting
#malaria death rate vs life expectancy
library(bbplot) #not working right now argh!
ggplot(data_fil_gdp, 
       aes(x = life_expectancy, y = mean)) +
  geom_point(aes(color = region, size = GDP), alpha = 0.5) +
  geom_smooth(method="loess", se=F, color='grey') +
  labs(subtitle="Malaria death rate vs life expectancy", 
       y="Malaria Death Rate", 
       x="Life Expectancy", 
       title="Scatterplot", 
       caption = "Source: IHME & World Bank",
       color = "Continent:",
       size = "GDP (in USD):") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust = -0.35),
        axis.title.y = element_text(size = 15, vjust = 0.35),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)))


#how much above or below the regional average is each country's malaria death rate
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `car name`, 
                   yend = mpg_z, 
                   xend = `car name`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip() +
  theme_bw()


