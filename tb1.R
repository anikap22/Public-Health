


require(tidyr)
require(dplyr)
require(ggplot2)
require(plyr) #for count function
require(plotly)
require(viridis)
library(RColorBrewer)
require(gridExtra)
require(grid)
require(ggpubr)


setwd('C:/Users/Anika/Documents/GradSchool/Portfolio/TB/')

data <- read.csv('Data/IHME_TB_SPENDING_2000_2017_0/IHME_TB_SPENDING_2000_2017_Y2020M05D05.csv')

head(data)
glimpse(data)

data_country <- data %>% dplyr::filter(level == "Country")

#convert spending to millions of USD
data_country$the_total_mean <- data_country$the_total_mean / 1e6

# separate data by continent
library(countrycode)
data_country$continent <- countrycode(sourcevar = data_country[, "location_name"],
                                  origin = "country.name",
                                  destination = "continent")

data_africa <- data_country[data_country$continent == "Africa",]
data_af <- data_africa %>% 
  group_by(location_name) %>% 
  dplyr::summarise(max_spending = max(the_total_mean)) %>% 
  slice_max(max_spending, n = 10)
data_africa <- data_africa %>% dplyr::filter(location_name %in% data_af$location_name)

data_america <- data_country[data_country$continent == "Americas",]
data_af <- data_america %>% 
  group_by(location_name) %>% 
  dplyr::summarise(max_spending = max(the_total_mean)) %>% 
  slice_max(max_spending, n = 10)
data_america <- data_america %>% dplyr::filter(location_name %in% data_af$location_name)

data_asia <- data_country[data_country$continent == "Asia",]
data_af <- data_asia %>% 
  group_by(location_name) %>% 
  dplyr::summarise(max_spending = max(the_total_mean)) %>% 
  slice_max(max_spending, n = 10)
data_asia <- data_asia %>% dplyr::filter(location_name %in% data_af$location_name)

data_europe <- data_country[data_country$continent == "Europe",]
data_af <- data_europe %>% 
  group_by(location_name) %>% 
  dplyr::summarise(max_spending = max(the_total_mean)) %>% 
  slice_max(max_spending, n = 10)
data_europe <- data_europe %>% dplyr::filter(location_name %in% data_af$location_name)

data_oceania <- data_country[data_country$continent == "Oceania",]
data_af <- data_oceania %>% 
  group_by(location_name) %>% 
  dplyr::summarise(max_spending = max(the_total_mean)) %>% 
  slice_max(max_spending, n = 10)
data_oceania <- data_oceania %>% dplyr::filter(location_name %in% data_af$location_name)

p1 <- ggplot(data_africa, aes(x = year, y = the_total_mean, color=location_name)) +
  geom_point(size = 3, alpha=0.5) +
  geom_line(size = 2, alpha=0.5) +
  scale_color_viridis(discrete=TRUE) +
  labs(x = " ",
       y = " ",
       title = " ",
       subtitle = "Africa",
       col = "Country") +
  theme_bw() +
  geom_label(data=data_africa %>% 
               filter(year == 2012) %>% 
               top_n(the_total_mean, n=5), # Filter data first
    aes(label=location_name, color=location_name),
    show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))

p2 <- ggplot(data_america, aes(x = year, y = the_total_mean)) +
  geom_point(alpha = 0.5, size = 3, aes(col=location_name)) +
  geom_line(alpha = 0.5, size = 2, aes(col=location_name)) +
  scale_color_viridis(discrete=TRUE) +
  labs(x = " ",
       y = " ",
       subtitle = "America",
       col = "Country") +
  theme_bw() +
  geom_label( 
    data=data_america %>% filter(year == 2012) %>% top_n(the_total_mean, n=5), # Filter data first
    aes(label=location_name, color=location_name),
    show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))

p3 <- ggplot(data_asia, aes(x = year, y = the_total_mean)) +
  geom_point(alpha = 0.5, size = 3, aes(col=location_name)) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(alpha = 0.5, size = 2, aes(col=location_name)) +
  labs(x = " ",
       y = "Total spending (millions USD)",
       subtitle = "Asia",
       col = " ") +
  theme_bw() +
  geom_label( 
    data=data_asia %>% filter(year == 2012) %>% top_n(the_total_mean, n=5), # Filter data first
    aes(label=location_name, color=location_name),
    show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))

p4  <- ggplot(data_europe, aes(x = year, y = the_total_mean)) +
  geom_point(alpha = 0.5, size = 3, aes(col=location_name)) +
  geom_line(alpha = 0.5, size = 2, aes(col=location_name)) +
  scale_color_viridis(discrete=TRUE) +
  labs(x = "Year",
       y = " ",
       subtitle = "Europe",
       col = " ") +
  theme_bw() +
  geom_label( 
    data=data_europe %>% filter(year == 2012) %>% top_n(the_total_mean, n=5), # Filter data first
    aes(label=location_name, color=location_name),
    show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))

p5 <- ggplot(data_oceania, aes(x = year, y = the_total_mean)) +
  geom_point(alpha = 0.5, size = 3, aes(col=location_name)) +
  geom_line(alpha = 0.5, size = 2, aes(col=location_name)) +
  scale_color_viridis(discrete=TRUE) +
  labs(x = "Year",
       y = " ",
       subtitle = "Oceania",
       col = " ") +
  theme_bw() +
  geom_label( 
    data=data_oceania %>% filter(year == 2012) %>% top_n(the_total_mean, n=5), # Filter data first
    aes(label=location_name, color=location_name),
    show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        axis.line = element_line(size=1, colour = "black"))

# plot layout: http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

figure <- ggarrange(p1, p2, p3, p4, p5, ncol=2, nrow=3, align='v')
annotate_figure(figure,
                top = text_grob("Tuberculosis spending by country over time", color = "black", size = 24))

png("tb_top10_time.png", 
    width = 50, 
    height = 24, 
    units = 'cm', 
    res = 300)
figure <- ggarrange(p1, p2, p3, p4, p5, ncol=2, nrow=3, align='v')
annotate_figure(figure,
                top = text_grob("Tuberculosis spending by country over time", color = "black", size = 24))
dev.off()



#########
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


data_gdp <- merge(data_country, wb_dat, by.x=c("location_name", "year"), by.y=c("country", "date"), all.x=T, all.y=F)
data_gdp <- data_gdp[!is.na(data_gdp$continent),]

plot(data_country$the_total_mean, data_country$ghes_notified_per_cap_mean, col=as.factor(data_country$continent))

p1 <- ggplot(data_gdp, aes(x = log(the_total_mean+0.001), y = log(ghes_notified_per_cap_mean+0.001), color = GDP)) +
  geom_point(aes(text=location_name))

require(plotly)
ggplotly(p1, tooltip = "text")

#ggplot(subset(wb_dat, date == "2008"), aes(...))

ggplot(wb_dat, aes(x = GDP, y = infant_mortality)) + 
  geom_point() +
  facet_wrap(~date) +
  geom_smooth(method="auto") +
  theme_classic() +
  labs(x = "GDP",
       y = "Infant morality (per 1,000 live births)",
       title = "Infant Morality vs GDP") 
  theme(axis.title.x = element_text(size = 15, vjust = -0.35),
        axis.title.y = element_text(size = 15, vjust = 0.35),
        text = element_text(size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 20),
        plot.caption=element_text(size=12, hjust=0, margin=margin(15,0,0,0)),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change axis line
        axis.line = element_line(colour = "black"))


wb_dat$ratio <- wb_dat$infant_mortality/wb_dat$GDP
wb_dat$date <- as.numeric(wb_dat$date)
plot(wb_dat$date, wb_dat$ratio)  
lm1 <- lm(wb_dat$ratio ~ wb_dat$date)
abline(a = summary(lm1)$coefficients[1], b = summary(lm1)$coefficients[2], col='blue')

p1 <- ggplot(wb_dat, aes(x=date, y=ratio)) +
  geom_point(shape=1, aes(text=country)) +    # Use hollow circles
  geom_smooth(data=subset(wb_dat, country %in% c("Mozambique","Nepal","")), method='lm',formula=y~x) +   # Add linear regression line 
  theme_bw() +
  labs(x = "Year",
       y = "Infant mortality rate per GDP")
ggplotly(p1, tooltip="text")




wb_dat <- wb_dat %>% mutate(decade = case_when(date >= 1960 & date < 1970 ~ 1960,
                                               date >= 1970 & date < 1980 ~ 1970,
                                               date >= 1980 & date < 1990 ~ 1980,
                                               date >= 1990 & date < 2000 ~ 1990,
                                               date >= 2000 & date < 2010 ~ 2000,
                                               date >= 2010 & date < 2020 ~ 2010))
p2 <- ggplot(wb_dat, aes(x = as.factor(decade), y = ratio)) +
  geom_boxplot() +
  theme_bw()
ggplotly(p2)


ggarrange(p1, p2)
