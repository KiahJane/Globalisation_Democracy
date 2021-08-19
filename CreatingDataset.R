## PACKAGES
install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)

### VARIABLES

## independent variable
# V-Dem (179 countries, 1900-2019)
vdem <- read_csv("Kiah/Glo.Dem.2020/V-Dem-CY-Core-v10.csv") %>%
  filter(year == 2015) %>%
  rename(cname = country_name, code = country_text_id) %>%
  mutate(cname = recode(cname, "United States of America" = "United States")) %>%
  select(cname, code, v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem) %>%
  mutate(vdem.mean = rowMeans(select(., v2x_polyarchy:v2x_egaldem)))
# Revised Combined Polity Score (165 countries, 1946-2019)
RCPS <- read_csv("~/Kiah/Glo.Dem.2020/qogdata2021-RevisedCombinedPolity.csv") %>%
  filter(year == 2015, !is.na(p_polity2)) %>%
  rename(code = ccodealp) %>%
  mutate(cname = recode(cname, "TimorLeste" = "Timor-Leste",
                        "Korea South" = "South Korea",
                        "Korea North" = "North Korea",
                        "GuineaBissau" = "Guinea-Bissau",
                        "France " = "France",
                        "Ethiopia " = "Ethiopia",
                        "Eswatini former Swaziland" = "Eswatini",
                        "Congo Democratic Republic" = "Democratic Republic of the Congo",
                        "Cote dIvoire" = "Ivory Coast",
                        "Congo" = "Republic of the Congo")) %>%
  select(cname, code, p_polity2)
# Freedom House (205 countries, 1972-2019)
FHI <- read.csv("~/Kiah/Glo.Dem.2020/qogdata2021-FreedomHouse.csv") %>%
  filter(year == 2015) %>%
  drop_na() %>%
  rename(code = ccodealp, fhi = fh_status) %>%
  mutate(cname = recode(cname, "TimorLeste" = "Timor-Leste", 
                        "Pakistan " = "Pakistan",  
                        "Malaysia " = "Malaysia",
                        "Korea South" = "South Korea",
                        "Korea North" = "North Korea",
                        "GuineaBissau" = "Guinea-Bissau",
                        "France " = "France",
                        "Ethiopia " = "Ethiopia",
                        "Eswatini former Swaziland" = "Eswatini",
                        "Cyprus " = "Cyprus",
                        "Congo Democratic Republic" = "Democratic Republic of the Congo",
                        "Cote dIvoire" = "Ivory Coast",
                        "Congo" = "Republic of the Congo")) %>%
  select(cname, code, fhi)
# Freedom House/Imputed Polity 2
fhi.pol2 <- read.csv("~/Kiah/Glo.Dem.2020/qogdata2021-FHI-Polity2.csv") %>%
  filter(year == 2015) %>%
  drop_na() %>%
  rename(code = ccodealp, fhi_polity2 = fh_ipolity2) %>%
  mutate(cname = recode(cname, "TimorLeste" = "Timor-Leste", 
                        "Pakistan " = "Pakistan",
                        "Malaysia " = "Malaysia",
                        "Korea South" = "South Korea",
                        "Korea North" = "North Korea",
                        "GuineaBissau" = "Guinea-Bissau",
                        "France " = "France",
                        "Ethiopia " = "Ethiopia",
                        "Eswatini former Swaziland" = "Eswatini",
                        "Cyprus " = "Cyprus",
                        "Congo Democratic Republic" = "Democratic Republic of the Congo",
                        "Cote dIvoire" = "Ivory Coast",
                        "Congo" = "Republic of the Congo")) %>%
  select(cname, year, fhi_polity2)

## dependent variable
# Green Political Seats in government (36 countries, 1960-2017)
green.seat <- read_csv("~/Kiah/Glo.Dem.2020/qogdata2020-greenseats.csv") %>%
  filter(year == 2015, !is.na(cpds_lg)) %>%  
  rename(grn.seat = cpds_lg) %>%
  select(cname, grn.seat) 
# Recycling Rate % from total waste -OECD- (25 countries, 2014-2017 -data not from 1 single year-)
recycle.rate <- read_csv("~/Kiah/Glo.Dem.2020/Recyling Rate - OECD.csv") %>%
  rename(year = Year, cname = Country, r.rate = RecyclingRate) %>%
  select(cname, r.rate)
# Tax Gap in % of GDP -various countries- (61 countries, 2014)
tax.gap <- read_csv("~/Kiah/Glo.Dem.2020/TaxGap.csv") %>%
  rename(cname = country) %>%
  mutate(cname = recode(cname, "Czech Rep" = "Czech Republic",
                        "Bosnia&Herz" = "Bosnia and Herzegovina",
                        "Korea, Rep." = "South Korea",
                        "Kyrgyz Rep" = "Kyrgyzstan",
                        "Russian Fed" = "Russia",
                        "Slovak Rep" = "Slovakia",
                        "UK" = "United Kingdom"))
# Quality of Government 0-1 (166 countries, 1984-2018)
QoG <- read_csv("~/Kiah/Glo.Dem.2020/qogdata2021-QoG-ICRG.csv") %>%
  filter(year == 2015, !is.na(icrg_qog)) %>%
  rename(qog = icrg_qog) %>%
  mutate(cname = recode(cname, "Congo Democratic Republic" = "Democratic Republic of the Congo",
                        "Cote dIvoire" = "Ivory Coast",
                        "Congo" = "Republic of the Congo",
                        "GuineaBissau" = "Guinea-Bissau",
                        "Korea North" = "North Korea",
                        "Korea South" = "South Korea")) %>%
  select(cname, qog)


## control variable
# Average Total Years of Schooling (189 countries, 1870-2017)
mean.school <- read_csv("~/Kiah/Glo.Dem.2020/mean-years-of-schooling.csv") %>%
  filter(Year == 2015) %>%
  rename(year = Year, cname = Entity, mean.schl = c(4)) %>%
  mutate(cname = recode(cname, "Cote d'Ivoire" = "Ivory Coast",
                        "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                        "Congo" = "Republic of the Congo",
                        "Micronesia (country)" = "Micronesia",
                        "Czechia" = "Czech Republic",
                        "Timor" = "Timor-Leste",
                        "Saint Kitts and Nevis" = "St Kitts and Nevis",
                        "Saint Lucia" = "St Lucia",
                        "Saint Vincent and the Grenadines" = "St Vincent and the Grenadines")) %>%
  select(cname, mean.schl)
# Literacy Rates over 15 given in % (208 couontries, 1475-2018)
region <- c('Africa (Northern)',
            'Africa (Sub-Saharan)',
            'Africa (Sub-Saharan) ',
            'Arab States',
            'Asia (Central and Southern)',
            'Asia (Central)',
            'Asia (Eastern and South-eastern)',
            'Asia (Eastern)',
            'Asia (South-eastern)',
            'Asia (Southern)',
            'Asia (Western)',
            'Central African Republic',
            'Central and Eastern Europe',
            'Central Asia',
            'East Asia and the Pacific',
            'Landlocked Developing Countries',
            'Latin America and the Caribbean',
            'Least Developed Countries',
            'Low income countries',
            'Lower middle income countries',
            'Middle income countries',
            'Small Island Developing States',
            'South and West Asia',
            'Sub-Saharan Africa',
            'Upper middle income countries',
            'Western Asia and Northern Africa',
            'World')    
lit.var <- merge( merge(read_csv("~/Kiah/Glo.Dem.2020/cross-country-literacy-rates (lit1).csv") %>%
                          filter(Year == 2015) %>%
                          rename(cname = Entity, lit.rate = c(4)) %>%
                          mutate(cname = recode(cname, "Cote d'Ivoire" = "Ivory Coast",
                                                "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                                                "Congo" = "Republic of the Congo",
                                                "Timor" = "Timor-Leste")) %>%
                          select(cname, lit.rate),
                        read.csv("~/Kiah/Glo.Dem.2020/literacy-rates (lit2).csv") %>%
                          filter(Time == 2018, 
                                 Indicator == 'Adult literacy rate, population 15+ years, both sexes (%)',
                                 !(Country %in% region)) %>%
                          rename(cname = Country, 
                                 Code = LOCATION, 
                                 lit.rate = Value) %>%
                          mutate(cname = recode(cname, "Côte d'Ivoire" = "Ivory Coast", 
                                                "Brunei Darussalam" = "Brunei",
                                                "Congo" = "Republic of the Congo",
                                                "Viet Nam" = "Vietnam",
                                                "Russian Federation" = "Russia")) %>%
                          select(cname, lit.rate), all = TRUE) %>%
                    aggregate(.~cname, data = ., mean),
                  read.csv("~/Kiah/Glo.Dem.2020/literacy-rates (lit2).csv") %>%
                    filter(Time == 2018, 
                           Indicator == 'Adult literacy rate, population 15+ years, both sexes (%)',
                           Country %in% region) %>%
                    rename(region = Country, 
                           lit.rate = Value) %>%
                    select(region, lit.rate) %>%
                    rbind(., c("Developed countries", 99.0), 
                          c("North America and Western Europe", 99.0)), all = TRUE)

# Liberal Democracy Index 0-1 (173 countries, 1946-2018)
ldi <- read_csv("~/Kiah/Glo.Dem.2020/qogdata2020-liberalDemoIndex.csv") %>%
  filter(year == 2015, !is.na(vdem_libdem)) %>%
  rename(ldi = vdem_libdem) %>%
  mutate(cname = recode(cname, "Congo Democratic Republic" = "Democratic Republic of the Congo",
                        "Cote dIvoire" = "Ivory Coast",
                        "Congo" = "Republic of the Congo",
                        "Eswatini former Swaziland" = "Eswatini",
                        "GuineaBissau" = "Guinea-Bissau",
                        "Korea North" = "North Korea",
                        "Korea South" = "South Korea",
                        "TimorLeste" = "Timor-Leste")) %>%
  select(cname, ldi) %>%
  rbind(., c("Andorra", 0.95), 
        c("Antigua and Barbuda", 0.85),
        c("Bahamas", 0.91),
        c("Belize", 0.86),
        c("Brunei", 0.29),
        c("Dominica", 0.95),
        c("Grenada", 0.89),
        c("Hong Kong", 0.61),
        c("Kiribati", 0.92),
        c("Liechtenstein", 0.91),
        c("Marshall Islands", 0.92),
        c("Micronesia", 0.93),
        c("Monaco", 0.84),
        c("Nauru", 0.77),
        c("Palau", 0.92),
        c("Samoa", 0.80),
        c("San Marino", 0.97),
        c("St Kitts and Nevis", 0.89),
        c("St Lucia", 0.92),
        c("St Vincent and the Grenadines", 0.91),
        c("Tuvalu", 0.94))
# Human Freedom Index (159 countries, 2008-2016)
hfi <- read_csv("~/Kiah/Glo.Dem.2020/human-freedom-index-2020.csv") %>%
  filter(year == 2015, !is.na(hf_score)) %>%
  rename(cname = countries, hfi = hf_score) %>%
  mutate(cname = recode(cname, "Bahamas, The" = "Bahamas",
                        "Brunei Darussalam" = "Brunei",
                        "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                        "Cote d'Ivoire" = "Ivory Coast",
                        "Congo, Rep." = "Republic of the Congo",
                        "Egypt, Arab Rep." = "Egypt",
                        "Gambia, The" = "Gambia",
                        "Hong Kong SAR, China" = "Hong Kong",
                        "Iran, Islamic Rep." = "Iran",
                        "Korea, Rep." = "South Korea",
                        "Kyrgyz Republic" = "Kyrgyzstan",
                        "Lao PDR" = "Laos",
                        "Russian Federation" = "Russia",
                        "Slovak Republic" = "Slovakia",
                        "Syrian Arab Republic" = "Syria",
                        "Venezuela, RB" = "Venezuela",
                        "Yemen, Rep." = "Yemen",
                        "Cabo Verde" = "Cape Verde")) %>%
  select(cname, hfi)
# Environmental Policy Stringency 0=not & 6=very -OECD & BRICS- (16 countries, 1990-2015)
eps <- read_csv("~/Kiah/Glo.Dem.2020/EnvPoliStringency.csv") %>%
  filter(YEA == 2015) %>%
  rename(year = YEA, cname = Country) %>%
  mutate(cname = recode(cname, "China (People's Republic of)" = "China",
                        "Korea" = "South Korea")) %>%
  select(cname, VAR, year, Value) %>%
  spread(key = VAR, value = Value) %>%
  transform(ELV_DIESELSO = as.numeric(ELV_DIESELSO),
            ELV_PM = as.numeric(ELV_PM),
            ELV_SOX = as.numeric(ELV_SOX),
            EPS = as.numeric(EPS),
            EPS_MKT = as.numeric(EPS_MKT),
            EPS_NMKT = as.numeric(EPS_NMKT),
            FIT = as.numeric(FIT),
            FIT_SOLAR = as.numeric(FIT_SOLAR),
            FIT_WIND = as.numeric(FIT_WIND),
            RD_RE = as.numeric(RD_RE),
            RD_SUB = as.numeric(RD_SUB),
            STD = as.numeric(STD),
            TAXCO2 = as.numeric(TAXCO2),
            TAXDIESEL = as.numeric(TAXDIESEL),
            TAXES = as.numeric(TAXES),
            TAXNOX = as.numeric(TAXNOX),
            TAXSOX = as.numeric(TAXSOX),
            TRADESCH = as.numeric(TRADESCH),
            TRADESCH_CO2 = as.numeric(TRADESCH_CO2),
            TRADESCH_EEF = as.numeric(TRADESCH_EEF),
            TRADESCH_REC = as.numeric(TRADESCH_REC)) %>%
  transmute(cname,
            eps_mean = rowMeans(.[ , c(3,24)], na.rm=TRUE)) 

# GDP per capita in US$ (193 countries, 1880-2011)
GDPpc <- read_csv("~/Kiah/Glo.Dem.2020/qogdata2020-GDPpercapita.csv") %>%
  filter(year == 2011, !is.na(gle_cgdpc)) %>%
  rename(gdp.pc = gle_cgdpc) %>%
  mutate(cname = recode(cname, "Congo Democratic Republic" = "Democratic Republic of the Congo",
                        "Cote dIvoire" = "Ivory Coast",
                        "Congo" = "Republic of the Congo",
                        "Eswatini former Swaziland" = "Eswatini",
                        "GuineaBissau" = "Guinea-Bissau",
                        "TimorLeste" = "Timor-Leste",
                        "Korea North" = "North Korea",
                        "Korea South" = "South Korea")) %>%
  select(cname, gdp.pc) %>%
  rbind(., c("Aruba", 24985.99), 
        c("Guam", 30859.40),
        c("Hong Kong", 35142.49),
        c("Macao", 66644.39),
        c("New Caledonia", 13009.03),
        c("Palestine", 2695.19),
        c("Puerto Rico", 27278.88),
        c("South Sudan", 1516.40))

# merge all variables
dataset.a <- merge(merge(
  merge(merge(merge(vdem, 
                    RCPS, all = TRUE), 
              FHI, all = TRUE), 
        fhi.pol2, all = TRUE),
  merge(merge(merge(green.seat,
                    recycle.rate, all = TRUE),
              tax.gap, all = TRUE),
        QoG, all = TRUE), all = TRUE),
  merge(merge(merge(merge(merge(mean.school,
                                lit.var, all = TRUE),
                          ldi, all = TRUE),
                    hfi, all = TRUE),
              eps, all = TRUE),
        GDPpc, all = TRUE)) %>%
  mutate_at(c("p_polity2", 
              "fhi", 
              "fhi_polity2",
              "hfi"), ~(rescale(., to = c(0, 1)) %>% as.vector)) #%>%
dataset.b <- dataset.a %>%
  mutate(reg.mean = rowMeans(select(., vdem.mean:fhi_polity2))) %>%
  select(-(year)) 
dataset.c <- dataset.b %>%
  mutate(region = case_when(cname == "Andorra" ~ "North America and Western Europe",
                            cname == "Australia" ~ "North America and Western Europe",
                            cname == "Austria" ~ "North America and Western Europe",
                            cname == "Belgium" ~ "North America and Western Europe",
                            cname == "Canada" ~ "North America and Western Europe",
                            cname == "Denmark" ~ "North America and Western Europe",
                            cname == "Finland" ~ "North America and Western Europe",
                            cname == "France" ~ "North America and Western Europe",
                            cname == "Germany" ~ "North America and Western Europe",
                            cname == "Iceland" ~ "North America and Western Europe",
                            cname == "Ireland" ~ "North America and Western Europe",
                            cname == "Israel" ~ "North America and Western Europe",
                            cname == "Luxembourg" ~ "North America and Western Europe",
                            cname == "Monaco" ~ "North America and Western Europe",
                            cname == "Netherlands" ~ "North America and Western Europe",
                            cname == "Norway" ~ "North America and Western Europe",
                            cname == "Sweden" ~ "North America and Western Europe",
                            cname == "Switzerland" ~ "North America and Western Europe",
                            cname == "United Kingdom" ~ "North America and Western Europe",
                            cname == "United States" ~ "North America and Western Europe",
                            cname == "Bermuda" ~ "Developed Countries",
                            cname == "Czech Republic" ~ "Developed Countries",
                            cname == "Japan" ~ "Developed Countries",
                            cname == "Liechtenstein" ~ "Developed Countries",
                            cname == "New Zealand" ~ "Developed Countries",
                            cname == "Slovakia" ~ "Developed Countries",
                            cname == "Tuvalu" ~ "Small Island Developing States",
                            cname == "St Vincent and the Grenadines" ~ "Small Island Developing States",
                            cname == "St Lucia" ~ "Latin America and the Caribbean",
                            cname == "St Kitts and Nevis" ~ "Latin America and the Caribbean",
                            cname == "South Korea" ~ "Asia (Eastern and South-eastern)",
                            cname == "Solomon Islands" ~ "East Asia and the Pacific",
                            cname == "Somalia" ~ "Africa (Sub-Saharan)",
                            cname == "Antigua and Barbuda" ~ "Latin America and the Caribbean",
                            cname == "Bahamas" ~ "Latin America and the Caribbean",
                            cname == "Barbados" ~ "Latin America and the Caribbean",
                            cname == "Djibouti" ~ "Sub-Saharan Africa",
                            cname == "Dominica" ~ "Latin America and the Caribbean",
                            cname == "Fiji" ~ "Oceania",
                            cname == "Grenada" ~ "Latin America and the Caribbean",
                            cname == "Hong Kong" ~ "Asia (Eastern)",
                            cname == "Kiribati" ~ "Oceania",
                            cname == "North Korea" ~ "Asia (Eastern)",
                            cname == "South Korea" ~ "Asia (Eastern)",
                            cname == "Micronesia" ~ "Oceania",
                            cname == "Nauru" ~ "East Asia and the Pacific"))
dataset.d <- dataset.c %>%
  mutate(lit.rate = case_when(region == "North America and Western Europe" ~ 99.0,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Developed Countries" ~ 99.0,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Small Island Developing States" ~ 84.47694,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Latin America and the Caribbean" ~ 93.86869,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Asia (Eastern and South-eastern)" ~ 95.80694,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "East Asia and the Pacific" ~ 95.62198,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Africa (Sub-Saharan)" ~ 65.23965,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Oceania" ~ 93.9,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(region == "Asia (Eastern)" ~ 96.52701,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) %>%
  mutate(lit.rate = case_when(cname == "Taiwan" ~ 98.4,
                              TRUE ~ as.numeric(as.character(.$lit.rate)))) 
dataset <- dataset.d %>%
  select(-(region))

write.csv(dataset,"~/Kiah/Glo.Dem.2020/dataset.csv", row.names = TRUE)

dataset.1 <- read.csv("~/Kiah/Glo.Dem.2020/dataset.csv")

