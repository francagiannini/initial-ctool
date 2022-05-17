#i <- tbl_fill[1,]

#### scenarios data and input --------

data_build <- function(i,
                       #init_yr,
                       start_yr=1996,end_yr=2020#, #time
                       # Yield_init,Yield_croprot, #crop
                       # res_init, res_croprot, 
                       # cc_init,cc_croprot, 
                       # root_init ,root_croprot,
                       # hi_init, hi_croprot,
                       # ep_init, ep_croprot,
                       # manure_init, manure_croprot
){
  #browser()
  i <- as.data.frame(t(i))
  y <- rep(0:(end_yr-as.numeric(i$init_yr)), 1)
  
  mainyieldC <- c(rep(as.numeric(i$Yield_init)*.45,length(as.numeric(i$init_yr):(start_yr - 1))),
                  rep(eval(parse(text =i$Yield_croprot))*.45, times = 5))
  
  residues <- 
    c(rep(as.numeric(i$res_init), length(as.numeric(i$init_yr):(start_yr - 1))),
      rep(eval(parse(text =i$res_croprot)),5))
  
  Ccovercrop <-  
    c(rep(as.numeric(i$cc_init), length(as.numeric(i$init_yr):(start_yr - 1))),
      rep(eval(parse(text =i$cc_croprot)),5)) # data some years with crop
  
  root <- c(rep(as.numeric(i$root_init), length(as.numeric(i$init_yr):(start_yr - 1))),
            rep(eval(parse(text =i$root_croprot)),5))
  
  HI <- c(rep(as.numeric(i$hi_init), length(as.numeric(i$init_yr):(start_yr - 1))),
          rep(eval(parse(text =i$hi_croprot)),5))
  
  below <- ((1/((1-root)*HI))*mainyieldC)*root # above DM multiplied by harvest index 
  
  ep <- c(rep(as.numeric(i$ep_init), length(as.numeric(i$init_yr):(start_yr - 1))),
          rep(eval(parse(text =i$ep_croprot)),5)) # data some years with crop
  
  Cres <- ((1/HI-1)*mainyieldC)-(mainyieldC * residues)
  
  #"Carbon deposited in the topsoil (t/ha)"
  Ctop <- Cres + Ccovercrop + below * ep
  
  #"C deposited in the subsoil (t/ha)"
  Csub <- below * (1 - ep)
  
  #"C deposited in the topsoil from manure (tC ha-1)"
  Cman <- c(rep(as.numeric(i$manure_init), length(as.numeric(i$init_yr):(start_yr - 1))),
            rep(eval(parse(text =i$manure_croprot)),5)) # came from CN relation and N measurments
  
  data_tbl <- cbind("Year"=(y),
                    "Carbon deposited in the topsoil (t/ha)"=round(Ctop,9),
                    "C deposited in the subsoil (t/ha)"=round(Csub,9),
                    "C deposited in the topsoil from manure (tC ha-1)"=round(Cman,9),
                    "Cor-Atmospheric 14C  pM Plant"=0,
                    "Cor-Atmospheric 14C  pM Manure"=0)
########## data writing input------  
  my_input <- c(
    c(  "[Parameters]	
PLoweLayer",	paste0(i$PLoweLayer)),
    "
offset",	paste(i$offset),
    "
depth", paste(i$depth),
    "
PupperLayer",	paste(i$PupperLayer),
    "
Initial pMC(%)", paste(i$'Initial pMC(%)'),
    "
Initial C(t/ha)", paste(i$'Initial C(t/ha)'),
    "
C/N", 	paste(i$'C/N'),
    "
Amended C", paste(i$'Amended C'),
    "
Crop
[HUM]
HUMdecompositionrate", 	paste(i$Crop_HUMdecompositionrate),
    "
[FOM]
FOMdecompositionrate", paste(i$Crop_FOMdecompositionrate),
    "
clayfraction", paste(i$Crop_clayfraction),
    "
tF", paste(i$Crop_tF),
    "
[ROM]
ROMfraction", paste(i$Crop_ROMfraction),
    "
ROMdecompositionrate", 	paste(i$Crop_ROMdecompositionrate),
    "
Manure
[HUM]	
HUMdecompositionrate", 	paste(i$Manure_HUMdecompositionrate),
"
HumFraction", paste(i$Manure_HumFraction),
    "
[FOM]
FOMdecompositionrate", paste(i$Manure_FOMdecompositionrate),
    "
clayfraction", paste(i$Manure_clayfraction),
    "
tF", paste(i$Manure_tF),
    "
[ROM]
ROMfraction", paste(i$Manure_ROMfraction),
    "
ROMdecompositionrate", 	paste(i$Manure_ROMdecompositionrate),
    "
CropC14
[HUM]	
HUMdecompositionrate", 	paste(i$CropC14_HUMdecompositionrate),
    "
[FOM]
FOMdecompositionrate", paste(i$CropC14_FOMdecompositionrate),
    "
clayfraction", paste(i$CropC14_clayfraction),
    "
tF", paste(i$CropC14_tF),
    "
[ROM]
ROMfraction", paste(i$CropC14_ROMfraction),
    "
ROMdecompositionrate", 	paste(i$CropC14_ROMdecompositionrate),
    "
decay rate", 	paste(i$'CropC14_decay rate'),
    "
ManureC14
[HUM]	
HUMdecompositionrate", 	paste(i$ManureC14_HUMdecompositionrate),
"
HumFraction", paste(i$ManureC14_HumFraction),
    "
[FOM]
FOMdecompositionrate", paste(i$ManureC14_FOMdecompositionrate),
    "
clayfraction", paste(i$ManureC14_clayfraction),
    "
tF", paste(i$ManureC14_tF),
    "
[ROM]
ROMfraction", paste(i$ManureC14_ROMfraction),
    "
ROMdecompositionrate", 	paste(i$ManureC14_ROMdecompositionrate),
    "
decay rate", 	paste(i$'ManureC14_decay rate'),
    "
[FOM]
FOMfractionPlantTopLayer",paste(i$FOMfractionPlantLowerLayer)	,
    "
FOMfractionPlantLowerLayer",	paste(i$FOMfractionPlantLowerLayer),
    "
FOMfractionPlantTopLayerC14",	paste(i$FOMfractionPlantTopLayerC14),
    "
FOMfractionPlantLowerLayerC14",	paste(i$FOMfractionPlantLowerLayerC14),
    "
[end] ")
  
############ func output----  
  scn_name <-paste(i$id,i$soil,i$hal_crop, #change by croprot initial
                   i$inittime,i$initC, sep = "_") 
  
  data <- as.data.frame(data_tbl)
  
  input <- my_input
  #writeLines(my_input, sep = "\t")
  
  return(list(scn_name,data,input))
  
}
#aver <-readRDS("aver.RDS")

aver_hal <- apply(tbl_fill_hal,1,data_build)

saveRDS(aver_hal,"aver_hal.RDS")


#### temperature -----


daily_Fou_to2013 <-
  read.csv("temperature/Daily_Foulum_1987_01_01_to_2013_12_31.csv",
           header = TRUE) %>% mutate(date = lubridate::ymd(date))


daily_Fou_from2014 <-read.csv(
  "temperature/Daily_Foulum_1_1_2014_to_1_1_2022.csv", 
  header = TRUE) %>% mutate(date = lubridate::ymd(lubridate::dmy(date)))


monthly_Fou_1987_2022 <-
  bind_rows(daily_Fou_to2013, daily_Fou_from2014)  %>% mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) %>%
  group_by(year, month) %>%
  summarise(fou_tem = mean(temp)) %>% 
  mutate(date=lubridate::ym(paste(year,month, sep = "-")))

# data_jo <- data_jo %>% mutate(
#   date=seq(from=lubridate::my("01-1966"),
#            to=lubridate::my("12/2020"),
#            by="month"))

monthly_Fou_1987_2022 <-
  bind_rows(daily_Fou_to2013, daily_Fou_from2014)  %>% mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) %>%
  group_by(year, month) %>%
  summarise(fou_tem = mean(temp)) %>% 
  mutate(date=lubridate::ym(paste(year,month, sep = "-")))

monthly_DK_1874_2010 <- 
  read.csv("temperature/Monthly_Denmark_1_1874_to_12_2010.csv",
  header = TRUE) %>% mutate(date = lubridate::my(Date),
                            dk_tem=temp)

all_temp <-
  full_join(monthly_DK_1874_2010, monthly_Fou_1987_2022, by = 'date')


train_temp <-
  inner_join(monthly_DK_1874_2010, monthly_Fou_1987_2022, by = 'date')

cor(train_temp$dk_tem,
    train_temp$fou_tem)

plot(train_temp$dk_tem,
    train_temp$fou_tem)

reg_t <- lm(fou_tem~dk_tem,train_temp);reg_t

summary(reg_t)

all_temp <- all_temp %>% mutate(
  tem_ok=if_else(is.na(fou_tem),(-0.4714+0.9875*dk_tem),fou_tem))

temp_yr <- all_temp %>% group_by(year) %>% 
  summarise(yr.mean=mean(fou_tem),
            yr.sd=sd(fou_tem),
            yr.min=min(fou_tem),
            yr.max=max(fou_tem),
            yr.amp=max(fou_tem)-min(fou_tem)
) %>% filter(between(year,1996,2020))

saveRDS(temp_yr, "temperature/temp_yr25.RDS")

temp30 <- all_temp %>%  filter(
  between(date, as.Date("1966-01-01"), as.Date("2020-12-31")))

temp100 <- all_temp %>%  filter(
  between(date, as.Date("1896-01-01"), as.Date("2020-12-31")))
 
write.table(
  temp30$tem_ok,
  "temperature\\temp55years.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)

write.table(
  temp100$tem_ok,
  "temperature\\temp125years.txt",
  col.names = FALSE,
  sep = "\t",
  row.names = FALSE
)

# all_temp %>% pivot_longer(cols=c(dk_tem,fou_tem),names_to = "source") %>% 
#   ggplot(aes(date, temp, group=source, fill=source, col=source)) +
#   geom_point() 


