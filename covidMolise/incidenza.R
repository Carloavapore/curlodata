
dati_prov<- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"

dati_prov <- read.csv(dati_prov)
dati_prov <- dati_prov %>% filter(denominazione_regione=="Sicilia")

  

### nuovi casi al giorno per provincia
nuovi_casi <- function(x, yesterday, last_day){
  
  x$data <- as.Date(x$data)
    #x <- x %>% filter(denominazione_provincia==inserire_provincia)
  
  dati_prov_ieri    <- x %>% filter( data==yesterday)
  dati_prov_ieri <- data.frame(dati_prov_ieri$denominazione_provincia, dati_prov_ieri$totale_casi)
  nomi_province <- arrange(dati_prov_ieri, dati_prov_ieri.denominazione_provincia) %>% select(dati_prov_ieri.denominazione_provincia)
  dati_prov_ieri <- arrange(dati_prov_ieri, dati_prov_ieri.denominazione_provincia) %>% select(dati_prov_ieri.totale_casi)
  
  dati_prov_oggi <- x %>% filter( data==last_day)
  dati_prov_oggi <- data.frame(dati_prov_oggi$denominazione_provincia, dati_prov_oggi$totale_casi)
  dati_prov_oggi <- arrange(dati_prov_oggi, dati_prov_oggi.denominazione_provincia) %>% select(dati_prov_oggi.totale_casi)
  
  dati_prov_last2 <- data.frame(dati_prov_ieri, dati_prov_oggi)
  
  ieri<- as.numeric(dati_prov_last2$dati_prov_ieri.totale_casi)
  oggi<- as.numeric(dati_prov_last2$dati_prov_oggi.totale_casi)
  nuovi_casi <- oggi-ieri
  
  
}
#applica_nuovi_casi <- function(x, provincia, start, end){

 # yesterday
 # last_day
  
#  for(i in 1:nc) {
 #   means[i] <- nuovi_casi(y[,i], na.rm= removeNA)
#  }
  
#}
calcolo <- function(x){

"2020-03-23"<- nuovi_casi(dati_prov, "2020-03-22", "2020-03-23")
"2020-03-22"<- nuovi_casi(dati_prov, "2020-03-21", "2020-03-22")
"2020-03-21"<- nuovi_casi(dati_prov, "2020-03-20", "2020-03-21")
"2020-03-20"<- nuovi_casi(dati_prov, "2020-03-19", "2020-03-20")
"2020-03-19"<- nuovi_casi(dati_prov, "2020-03-18", "2020-03-19")
"2020-03-18"<- nuovi_casi(dati_prov, "2020-03-17", "2020-03-18")
"2020-03-17"<- nuovi_casi(dati_prov, "2020-03-16", "2020-03-17")
"2020-03-16"<- nuovi_casi(dati_prov, "2020-03-15", "2020-03-16")
"2020-03-15"<- nuovi_casi(dati_prov, "2020-03-14", "2020-03-15")
"2020-03-14"<- nuovi_casi(dati_prov, "2020-03-13", "2020-03-14")
"2020-03-13"<- nuovi_casi(dati_prov, "2020-03-12", "2020-03-13")
"2020-03-12"<- nuovi_casi(dati_prov, "2020-03-11", "2020-03-12")
"2020-03-11"<- nuovi_casi(dati_prov, "2020-03-10", "2020-03-11")
"2020-03-10"<- nuovi_casi(dati_prov, "2020-03-09", "2020-03-10")
"2020-03-09"<- nuovi_casi(dati_prov, "2020-03-08", "2020-03-09")
"2020-03-08"<- nuovi_casi(dati_prov, "2020-03-07", "2020-03-08")
"2020-03-07"<- nuovi_casi(dati_prov, "2020-03-06", "2020-03-07")
"2020-03-06"<- nuovi_casi(dati_prov, "2020-03-05", "2020-03-06")
"2020-03-05"<- nuovi_casi(dati_prov, "2020-03-04", "2020-03-05")
"2020-03-04"<- nuovi_casi(dati_prov, "2020-03-03", "2020-03-04")
"2020-03-03"<- nuovi_casi(dati_prov, "2020-03-02", "2020-03-03")
"2020-03-02"<- nuovi_casi(dati_prov, "2020-03-01", "2020-03-02")
"2020-03-01"<- nuovi_casi(dati_prov, "2020-02-29", "2020-03-01")
"2020-02-29"<- nuovi_casi(dati_prov, "2020-02-28", "2020-02-29")
"2020-02-28"<- nuovi_casi(dati_prov, "2020-02-27", "2020-02-28")
"2020-02-27"<- nuovi_casi(dati_prov, "2020-02-26", "2020-02-27")
"2020-02-26"<- nuovi_casi(dati_prov, "2020-02-25", "2020-02-26")
"2020-02-25"<- nuovi_casi(dati_prov, "2020-02-24", "2020-02-25")



incidenza_prov <- data.frame(
  `2020-03-23`,
  `2020-03-22`,
  `2020-03-21`,
  `2020-03-20`,
  `2020-03-19`,
  `2020-03-18`,
  `2020-03-17`,
  `2020-03-16`,
  `2020-03-15`,
  `2020-03-14`,
  `2020-03-13`,
  `2020-03-12`,
  `2020-03-11`,
  `2020-03-10`,
  `2020-03-09`,
  `2020-03-08`,
  `2020-03-07`,
  `2020-03-06`,
  `2020-03-05`,
  `2020-03-04`,
  `2020-03-03` )



date_inc_prov <- c(
  "2020-03-23",
  "2020-03-22",
  "2020-03-21",
  "2020-03-20",
  "2020-03-19",
  "2020-03-18",
  "2020-03-17",
  "2020-03-16",
  "2020-03-15",
  "2020-03-14",
  "2020-03-13",
  "2020-03-12",
  "2020-03-11",
  "2020-03-10",
  "2020-03-09",
  "2020-03-08",
  "2020-03-07",
  "2020-03-06",
  "2020-03-05",
  "2020-03-04",
  "2020-03-03" 
)


incidenza_prov <- t(incidenza_prov)
incidenza_prov <- as.numeric(incidenza_prov)
date_inc_prov <- as.Date(date_inc_prov)
df_inc_prov <- data.frame(date_inc_prov,incidenza_prov)
df_inc_prov <- filter(df_inc_prov, incidenza_prov>=0)


date_prov <- uncount(df_inc_prov, weights = incidenza_prov, .remove = FALSE)

date_incidenza_prov <- date_prov$date_inc_prov
inc_obj <- incidence(date_incidenza_prov)
early.fit <- fit(inc_obj)

#print(early.fit)
}
