library(tidyverse)

soiltype <- c("JB1", "JB4", "JB6")

soilCinit <- c("sC0.9", "sC1.5", "sC2.5")

hal <- c("st", "mod.unfert","mod.fert")

croprot <- c(
  "LowF1","LowF2","LowF3",
  "HighF1","HighF2","HighF3",
  "OrganicF1", "OrganicF2", "OrganicF3"
)

inittime <- c("30", "100")

initC <- c("springbar", "wwheat", "grass")

tbl_scn <- expand_grid(soiltype, soilCinit, croprot, hal ,inittime, initC) %>% 
  unite("soil", c('soiltype', 'soilCinit'), sep = "_", remove = F) %>% 
  unite("hal_crop", c('hal', 'croprot'), sep = "_", remove = F) %>% 
  unite("hal_init", c('hal', 'initC'), sep = "_", remove = F) 

inp <-
  read.table(
    "runs/input.txt",
    header = T,
    sep = "\t",
    check.names = F
  )

# Parameters list----
#inp$`[Parameters]`

param <- c(
  "PLoweLayer",
  "offset",
  "depth",
  "PupperLayer",
  "Initial pMC(%)",
  "Initial C(t/ha)",
  "C/N",
  "Amended C",
  # Crop HUM
  "Crop_HUMdecompositionrate",
  #Crop FOM
  "Crop_FOMdecompositionrate",
  "Crop_clayfraction",
  "Crop_tF",
  # Crop ROM
  "Crop_ROMfraction",
  "Crop_ROMdecompositionrate",
  # Manure HUM
  "Manure_HUMdecompositionrate",
  "Manure_HumFraction",
  # Manure FOM
  "Manure_FOMdecompositionrate",
  "Manure_clayfraction",
  "Manure_tF",
  # Manure ROM
  "Manure_ROMfraction",
  "Manure_ROMdecompositionrate",
  # CropC14 HUM
  "CropC14_HUMdecompositionrate",
  # CropC14 FOM
  "CropC14_FOMdecompositionrate",
  "CropC14_clayfraction",
  "CropC14_tF",
  # CropC14 ROM
  "CropC14_ROMfraction",
  "CropC14_ROMdecompositionrate",
  "CropC14_decay rate",
  # ManureC14 HUM
  "ManureC14_HUMdecompositionrate",
  "ManureC14_HumFraction",
  # ManureC14 FOM
  "ManureC14_FOMdecompositionrate",
  "ManureC14_clayfraction",
  "ManureC14_tF",
  # ManureC14 ROM
  "ManureC14_ROMfraction",
  "ManureC14_ROMdecompositionrate",
  "ManureC14_decay rate",
  # Plant
  "FOMfractionPlantTopLayer",
  "FOMfractionPlantLowerLayer",
  # Plant C14
  "FOMfractionPlantTopLayerC14",
  "FOMfractionPlantLowerLayerC14"
)

inp_params <- data.frame(matrix(ncol = length(param),
                                nrow = nrow(tbl_scn)))
colnames(inp_params) <- param

#Table Scenarios and parameters -----
tbl_scn <-
  bind_cols(tbl_scn, inp_params) %>% 
  mutate(
    id = row_number(),
    init_yr = recode(inittime, "30" = 1966, "100" = 1896),
    
    Yield_init = recode(
      initC,
      "springbar" = 4.505,#springbarley
      "wwheat" = 5, #winter wheat
      "grass" = 9.499),#grass
    Yield_croprot = recode(
      croprot,
      "LowF1" = "c(9.499, 9.499, 9.499, 4.505, 4.505)", #Grass, Grass, Grass, Grain_cc, Grain
      "LowF2" = "c(9.499, 9.499, 4.505, 11.21, 4.505)",#Grass, Grass, Grain_cc, Maize, Grain
      "LowF3" = "c(11.211, 11.211, 11.211, 4.505, 5)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "OrganicF1" = "c(7.919, 7.919, 3.485, 2.750, 3.485)",#Grass, Grass, Grain, Legumes, Grain
      "OrganicF2" = "c(7.919, 7.919, 7.919, 9.603, 3.485)",#Grass, Grass, Grass, Maize_cc, Grain
      "OrganicF3" = "c(7.919, 7.919, 7.919, 3.485, 3.485)",#Grass, Grass, Grass, Grain, Grain
      "HighF1" = "c(9.499, 9.499, 9.499, 4.505, 4.505)",#Grass, Grass, Grass, Grain_cc, Grain
      "HighF2" = "c(9.499, 9.499, 11.211, 11.211, 4.505)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "HighF3" = "c(9.499, 11.211, 11.211, 11.211, 4.505)"),#Grass, Maize_cc, Maize, Maize_cc, Grain
    res_init = recode(
      initC,#F_s
      "springbar" = 0.55,#springbarley
      "wwheat" = 0.55,#winter wheat
      "grass" = 0),#grass
    res_croprot = recode(
      croprot,#F_s
      "LowF1" = "c(0, 0, 0, 0.55, 0.55)",#Grass, Grass, Grass, Grain_cc, Grain
      "LowF2" = "c(0, 0, 0.55, 0, 0.55)",#Grass, Grass, Grain_cc, Maize, Grain
      "LowF3" = "c(0, 0, 0, 0.55, 0.8)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "OrganicF1" = "c(0, 0, 0.55, 0, 0.55)",#Grass, Grass, Grain, Legumes, Grain
      "OrganicF2" = "c(0, 0, 0, 0, 0.55)",#Grass, Grass, Grass, Maize_cc, Grain
      "OrganicF3" = "c(0, 0, 0, 0.55, 0.55)",#Grass, Grass, Grass, Grain, Grain
      "HighF1" = "c(0, 0, 0, 0.55, 0.55)",#Grass, Grass, Grass, Grain_cc, Grain
      "HighF2" = "c(0, 0, 0, 0, 0.55)", #Grass, Grass, Maize_cc, Maize_cc, Grain
      "HighF3" = "c(0, 0, 0, 0, 0.55)"),#Grass, Maize_cc, Maize, Maize_cc, Grain
    cc_init = 0,
    cc_croprot = recode(
      croprot,#F_s
      "LowF1" = "c(0, 0, 0, 1.49, 0)",#Grass, Grass, Grass, Grain_cc, Grain
      "LowF2" = "c(0, 0, 1.49, 0, 0)",#Grass, Grass, Grain_cc, Maize, Grain
      "LowF3" = "c(1.49, 1.49, 0, 0, 0)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "OrganicF1" = "c(0, 0, 0, 0, 0)", #Grass, Grass, Grain, Legumes, Grain
      "OrganicF2" = "c(0, 0, 0, 1.49, 0)",#Grass, Grass, Grass, Maize_cc, Grain
      "OrganicF3" = "c(0, 0, 0, 0, 0)", #Grass, Grass, Grass, Grain, Grain
      "HighF1" = "c(0, 0, 0, 1.49, 0)", #Grass, Grass, Grass, Grain_cc, Grain
      "HighF2" = "c(0, 0, 1.49, 1.49, 0)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "HighF3" = "c(0, 1.49, 0, 1.49, 0)"),#Grass, Maize_cc, Maize, Maize_cc, Grain
    root_init = recode(
      hal_init,#F_re
      "mod.fert_grass"=0.41,
      "mod.fert_springbar" = 0.17,
      "mod.fert_wwheat" = 0.25,
      "mod.unfert_grass" = 0.43,
      "mod.unfert_springbar"=0.17,
      "mod.unfert_wwheat"= 0.25,
      "st_springbar" = 0.17, #springbarley
      "st_wwheat" = 0.25,#winter wheat
      "st_grass" = 0.45 ),#grass),
    root_croprot = recode(
      hal_crop,#F_s
      "mod.fert_LowF1" = "c(0.41, 0.41, 0.41, 0.17, 0.17)",#Grass, Grass, Grass, Grain_cc, Grain
      "mod.fert_LowF2" = "c(0.41, 0.41, 0.17, 0.15, 0.17)",#Grass, Grass, Grain_cc, Maize, Grain
      "mod.fert_LowF3" = "c(0.15, 0.15, 0.15, 0.17, 0.7)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "mod.fert_OrganicF1" = "c(0.41, 0.41, 0.17, 0.41, 0.17)",#Grass, Grass, Grain, Legumes, Grain
      "mod.fert_OrganicF2" = "c(0.41, 0.41, 0.41, 0.15, 0.17)",#Grass, Grass, Grass, Maize_cc, Grain
      "mod.fert_OrganicF3" = "c(0.41, 0.41, 0.41, 0.17, 0.17)",#Grass, Grass, Grass, Grain, Grain
      "mod.fert_HighF1" = "c(0.41, 0.41, 0.41, 0.17, 0.17)",#Grass, Grass, Grass, Grain_cc, Grain
      "mod.fert_HighF2" = "c(0.41, 0.41, 0.15, 0.15, 0.17)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "mod.fert_HighF3" = "c(0.41, 0.15, 0.15, 0.15, 0.17)",#Grass, Maize_cc, Maize, Maize_cc, Grain
      
      "mod.unfert_LowF1" = "c(0.43, 0.43, 0.43, 0.17, 0.17)",#Grass, Grass, Grass, Grain_cc, Grain
      "mod.unfert_LowF2" = "c(0.43, 0.43, 0.17, 0.15, 0.17)",#Grass, Grass, Grain_cc, Maize, Grain
      "mod.unfert_LowF3" = "c(0.15, 0.15, 0.15, 0.17, 0.7)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "mod.unfert_OrganicF1" = "c(0.43, 0.43, 0.17, 0.43, 0.17)",#Grass, Grass, Grain, Legumes, Grain
      "mod.unfert_OrganicF2" = "c(0.43, 0.43, 0.43, 0.15, 0.17)",#Grass, Grass, Grass, Maize_cc, Grain
      "mod.unfert_OrganicF3" = "c(0.43, 0.43, 0.43, 0.17, 0.17)",#Grass, Grass, Grass, Grain, Grain
      "mod.unfert_HighF1" = "c(0.43, 0.43, 0.43, 0.17, 0.17)",#Grass, Grass, Grass, Grain_cc, Grain
      "mod.unfert_HighF2" = "c(0.43, 0.43, 0.15, 0.15, 0.17)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "mod.unfert_HighF3" = "c(0.43, 0.15, 0.15, 0.15, 0.17)",#Grass, Maize_cc, Maize, Maize_cc, Grain
       
      "st_LowF1" = "c(0.45, 0.45, 0.45, 0.17, 0.17)",#Grass, Grass, Grass, Grain_cc, Grain
      "st_LowF2" = "c(0.45, 0.45, 0.17, 0.15, 0.17)",#Grass, Grass, Grain_cc, Maize, Grain
      "st_LowF3" = "c(0.15, 0.15, 0.15, 0.17, 0.7)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "st_OrganicF1" = "c(0.45, 0.45, 0.17, 0.45, 0.17)",#Grass, Grass, Grain, Legumes, Grain
      "st_OrganicF2" = "c(0.45, 0.45, 0.45, 0.15, 0.17)",#Grass, Grass, Grass, Maize_cc, Grain
      "st_OrganicF3" = "c(0.45, 0.45, 0.45, 0.17, 0.17)",#Grass, Grass, Grass, Grain, Grain
      "st_HighF1" = "c(0.45, 0.45, 0.45, 0.17, 0.17)",#Grass, Grass, Grass, Grain_cc, Grain
      "st_HighF2" = "c(0.45, 0.45, 0.15, 0.15, 0.17)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "st_HighF3" = "c(0.45, 0.15, 0.15, 0.15, 0.17)"),#Grass, Maize_cc, Maize, Maize_cc, Grain
    hi_init = recode(
      hal_init,#HI
      "mod.fert_grass"=0.91,
      "mod.fert_springbar" = 0.45,
      "mod.fert_wwheat" = 0.45,
      "mod.unfert_grass" = 0.87,
      "mod.unfert_springbar"=0.45,
      "mod.unfert_wwheat"= 0.45,
      "st_springbar" = 0.45,#springbarley
      "st_wwheat" = 0.45,#winter wheat
      "st_grass" = 0.7),#grass
    hi_croprot = recode(
      hal_crop,#HI
      "mod.fert_LowF1" = "c(0.91, 0.91, 0.91, 0.45, 0.45)", #Grass, Grass, Grass, Grain_cc, Grain
      "mod.fert_LowF2" = "c(0.91, 0.91, 0.45, 0.85, 0.45)",#Grass, Grass, Grain_cc, Maize, Grain
      "mod.fert_LowF3" = "c(0.85, 0.85, 0.85, 0.45, 0.45)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "mod.fert_OrganicF1" = "c(0.91, 0.91, 0.45, 0.91, 0.45)",#Grass, Grass, Grain, Legumes, Grain
      "mod.fert_OrganicF2" = "c(0.91, 0.91, 0.91, 0.85, 0.45)",#Grass, Grass, Grass, Maize_cc, Grain
      "mod.fert_OrganicF3" = "c(0.91, 0.91, 0.91, 0.45, 0.45)",#Grass, Grass, Grass, Grain, Grain
      "mod.fert_HighF1" = "c(0.91, 0.91, 0.91, 0.45, 0.45)",#Grass, Grass, Grass, Grain_cc, Grain
      "mod.fert_HighF2" = "c(0.91, 0.91, 0.85, 0.85, 0.45)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "mod.fert_HighF3" = "c(0.91, 0.85, 0.85, 0.85, 0.45)",#Grass, Maize_cc, Maize, Maize_cc, Grain,
      
      "mod.unfert_LowF1" = "c(0.87, 0.87, 0.87, 0.45, 0.45)", #Grass, Grass, Grass, Grain_cc, Grain
      "mod.unfert_LowF2" = "c(0.87, 0.87, 0.45, 0.85, 0.45)",#Grass, Grass, Grain_cc, Maize, Grain
      "mod.unfert_LowF3" = "c(0.85, 0.85, 0.85, 0.45, 0.45)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "mod.unfert_OrganicF1" = "c(0.87, 0.87, 0.45, 0.87, 0.45)",#Grass, Grass, Grain, Legumes, Grain
      "mod.unfert_OrganicF2" = "c(0.87, 0.87, 0.87, 0.85, 0.45)",#Grass, Grass, Grass, Maize_cc, Grain
      "mod.unfert_OrganicF3" = "c(0.87, 0.87, 0.87, 0.45, 0.45)",#Grass, Grass, Grass, Grain, Grain
      "mod.unfert_HighF1" = "c(0.87, 0.87, 0.87, 0.45, 0.45)",#Grass, Grass, Grass, Grain_cc, Grain
      "mod.unfert_HighF2" = "c(0.87, 0.87, 0.85, 0.85, 0.45)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "mod.unfert_HighF3" = "c(0.87, 0.85, 0.85, 0.85, 0.45)",#Grass, Maize_cc, Maize, Maize_cc, Grain,
      
      "st_LowF1" = "c(0.7, 0.7, 0.7, 0.45, 0.45)", #Grass, Grass, Grass, Grain_cc, Grain
      "st_LowF2" = "c(0.7, 0.7, 0.45, 0.85, 0.45)",#Grass, Grass, Grain_cc, Maize, Grain
      "st_LowF3" = "c(0.85, 0.85, 0.85, 0.45, 0.45)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "st_OrganicF1" = "c(0.7, 0.7, 0.45, 0.7, 0.45)",#Grass, Grass, Grain, Legumes, Grain
      "st_OrganicF2" = "c(0.7, 0.7, 0.7, 0.85, 0.45)",#Grass, Grass, Grass, Maize_cc, Grain
      "st_OrganicF3" = "c(0.7, 0.7, 0.7, 0.45, 0.45)",#Grass, Grass, Grass, Grain, Grain
      "st_HighF1" = "c(0.7, 0.7, 0.7, 0.45, 0.45)",#Grass, Grass, Grass, Grain_cc, Grain
      "st_HighF2" = "c(0.7, 0.7, 0.85, 0.85, 0.45)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "st_HighF3" = "c(0.7, 0.85, 0.85, 0.85, 0.45)"),#Grass, Maize_cc, Maize, Maize_cc, Grain,
    ep_init
    = recode(
      initC,#HI
      "springbar" = 0.8,#springbarley
      "wwheat" = 0.7,#winter wheat
      "grass" = 0.9),#grass),,
    ep_croprot = recode(
      croprot, #HI
      "LowF1" = "c(0.9, 0.9, 0.9, 0.8, 0.8)",#Grass, Grass, Grass, Grain_cc, Grain
      "LowF2" = "c(0.9, 0.9, 0.8, 0.8, 0.8)",#Grass, Grass, Grain_cc, Maize, Grain
      "LowF3" = "c(0.8, 0.8, 0.8, 0.8, 0.7)",#Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "OrganicF1" = "c(0.9, 0.9, 0.8, 0.8, 0.8)", #Grass, Grass, Grain, Legumes, Grain
      "OrganicF2" = "c(0.9, 0.9, 0.9, 0.8, 0.8)",#Grass, Grass, Grass, Maize_cc, Grain
      "OrganicF3" = "c(0.9, 0.9, 0.9, 0.8, 0.8)",#Grass, Grass, Grass, Grain, Grain
      "HighF1" = "c(0.9, 0.9, 0.9, 0.8, 0.8)",#Grass, Grass, Grass, Grain_cc, Grain
      "HighF2" = "c(0.9, 0.9, 0.8, 0.8, 0.8)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "HighF3" = "c(0.9, 0.8, 0.8, 0.8, 0.8)"), #Grass, Maize_cc, Maize, Maize_cc, Grain
    manure_init = recode(
      initC,#HI
      "springbar" = 0,#springbarley
      "wwheat" = 0,#winter wheat
      "grass" = 1.7),#grass
    manure_croprot = recode(
      croprot,#HI
      "LowF1" = "c(3.33, 3.33, 3.33, 0.88, 1.31)",#Grass, Grass, Grass, Grain_cc, Grain
      "LowF2" = "c(3.33, 3.33, 0.88, 1.42, 1.59)",#Grass, Grass, Grain_cc, Maize, Grain
      "LowF3" = "c(1.7, 1.42, 1.42, 1.59, 1.59)", #Maize_cc, Maize_cc, Maize,Grain, Grain wheat
      "OrganicF1" = "c(1.83, 1.83, 0.48, 0, 0.76)", #Grass, Grass, Grain, Legumes, Grain
      "OrganicF2" = "c(1.83, 1.83, 1.83, 0.54, 0.72)",#Grass, Grass, Grass, Maize_cc, Grain
      "OrganicF3" = "c(1.83, 1.83, 1.83, 0.48, 0.87)",#Grass, Grass, Grass, Grain, Grain
      "HighF1" = "c(4.1, 4.1, 4.1, 1.08, 1.61)",#Grass, Grass, Grass, Grain_cc, Grain
      "HighF2" = "c(4.1, 4.1, 1.22, 1.75, 1.61)",#Grass, Grass, Maize_cc, Maize_cc, Grain
      "HighF3" = "c(4.1, 1.22, 1.75, 2.09, 1.61)")#Grass, Maize_cc, Maize, Maize_cc, Grain
  ) %>%  select(id, everything()) 



#write_xlsx(tbl_scn, "tbl_scn.xlsx")


# Table 

tbl_fill_hal <-
  tbl_scn  %>%
  mutate(
    "PLoweLayer" = 0.312,
    "offset" = 0,
    "depth" = 100,
    "PupperLayer" = 0.48,
    "Initial pMC(%)" = 0,
    # initial condition of C  
    "Initial C(t/ha)" = recode(
      soil,
      "JB1_sC0.9" = 70.4,
      "JB1_sC1.5" = 114.9,
      "JB1_sC2.5" = 173.4,
      
      "JB4_sC0.9" = 78.4,
      "JB4_sC1.5" = 114.8,
      "JB4_sC2.5" = 174.9,
      
      "JB6_sC0.9" = 70.2,
      "JB6_sC1.5" = 119.6,
      "JB6_sC2.5" = 174.2,
    ),
    # CN balance
    `C/N` = recode(
      soil,
      "JB1_sC0.9" = 0.86 / 0.12,
      "JB1_sC1.5" = 1.49 / 0.21,
      "JB1_sC2.5" = 2.47 / 0.35,
      
      "JB4_sC0.9" = 0.97 / 0.11,
      "JB4_sC1.5" = 1.53 / 0.18,
      "JB4_sC2.5" = 2.61 / 0.30,
      
      "JB6_sC0.9" = 0.81 / 0.09,
      "JB6_sC1.5" = 1.47 / 0.16,
      "JB6_sC2.5" = 2.39 / 0.26,
    ),
    "Amended C"= 0,
    # Crop HUM
    "Crop_HUMdecompositionrate"= 0.0028,
    #Crop FOM
    "Crop_FOMdecompositionrate"= 0.12,
    "Crop_clayfraction"= recode (
      soiltype,
      "JB1"=0.0405,
      "JB4"=0.0809,
      "JB6"=0.1272
    ),
    "Crop_tF"=0.003,
    # Crop ROM
    "Crop_ROMfraction"=0.012,
    "Crop_ROMdecompositionrate"=0.00003858,
    # Manure HUM
    "Manure_HUMdecompositionrate"= Crop_HUMdecompositionrate,
    "Manure_HumFraction"= recode (
      soiltype,
      "JB1"=0.192,
      "JB4"=0.176,
      "JB6"=0.161
    ),
    # Manure FOM
    "Manure_FOMdecompositionrate"=0.12,
    "Manure_clayfraction"= Crop_clayfraction,
    "Manure_tF"=0,
    # Manure ROM
    "Manure_ROMfraction"=0,
    "Manure_ROMdecompositionrate"=0,
    # CropC14 HUM
    "CropC14_HUMdecompositionrate"= Manure_HUMdecompositionrate,
    # CropC14 FOM
    "CropC14_FOMdecompositionrate"= 0,
    "CropC14_clayfraction"= Crop_clayfraction,
    "CropC14_tF"=0,
    # CropC14 ROM
    "CropC14_ROMfraction" = 0,
    "CropC14_ROMdecompositionrate"=0,
    "CropC14_decay rate"=0,
    # ManureC14 HUM
    "ManureC14_HUMdecompositionrate"= Manure_HUMdecompositionrate,
    "ManureC14_HumFraction"= Manure_HumFraction,
    # ManureC14 FOM
    "ManureC14_FOMdecompositionrate"= 0,
    "ManureC14_clayfraction"= Crop_clayfraction,
    "ManureC14_tF"= 0,
    # ManureC14 ROM
    "ManureC14_ROMfraction"= 0,
    "ManureC14_ROMdecompositionrate"= 0,
    "ManureC14_decay rate" = 0,
    # Plant
    "FOMfractionPlantTopLayer" = 0.032,
    "FOMfractionPlantLowerLayer" = 0.03,
    # Plant C14
    "FOMfractionPlantTopLayerC14"= FOMfractionPlantTopLayer,
    "FOMfractionPlantLowerLayerC14"= FOMfractionPlantLowerLayer
  )

saveRDS(tbl_fill_hal, "tbl_fill_hal.RDS")

write.table(
  tbl_fill_hal,
  "C:\\Users\\au710823\\OneDrive - Aarhus universitet\\ctool1st2022\\tbl_fill_hal.txt",
  sep = "\t",
  dec = "."
)

