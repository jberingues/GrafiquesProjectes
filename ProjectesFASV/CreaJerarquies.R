#--------------------------LLIBRERIES---------------------------------
library(tidyverse)
library(stringr)

#-------------------CÀRREGA DE DADES i NETEJA-------------------------
#Diari de vendes del FASV
DiariFASV <- read_csv("Dades/VendesFASV120101151231m.csv",
    locale = locale(encoding = "Latin1", grouping_mark = ".", decimal_mark = ","),
    col_types = cols(
      TEMPS = col_character(),
      DATA = col_date("%Y/%m/%d"),
      VATCOD = col_character(),
      ARCARF = col_character(),
      IMPORT = col_number(),
      MARGE = col_number(),
      QTT = col_character(),
      VPADES = col_character(),
      CLNOMB = col_character(),
      ARDES1 = col_character(),
      VENOMB = col_character(),
      COST = col_number()
    )
)

DiariFASV <- rename(DiariFASV,
    Data = DATA,
    TipusProducte = VATCOD,
    Producte = ARCARF,
    Import = IMPORT,
    Marge = MARGE,
    Unitats = QTT,
    Descripcio = ARDES1,
    Pais = VPADES,
    Client = CLNOMB,
    Cost = COST
)

#Fitxer ambn tots els escandalls del FASV
Escandall <- read_tsv("ProjectesFASV/escandall.txt",
        locale = locale(encoding = "Latin1", grouping_mark = ".", decimal_mark = ","),
        col_types = cols(
            EACANT = col_number()
        )
)
Escandall <- rename(Escandall,
        QuantitatRef = EACANT,
        Producte = ARCARF,
        Semifabricat = EACARF
)

#Fitxer de tots els materials de SAP
Materials <- read_csv("ProjectesFASV/Materials.csv")

#Fitxer de conversió de semifabricats a Projectes
XToProj <- read_csv("ProjectesFASV/XToProj.csv")
 
#Fitxer de conversió de semifabricats a Projectes
NomsJerarquies <- read_csv("ProjectesFASV/jerarquies.csv",
   col_types = cols(
       Jerarquia = col_integer()
   )
)

#-------------------CREACIÓ DEL CONVERSOR DEL CLASSIFICADOR DE MATERIALS FASV-------------------------
#Creació de ProdDiariFASV
Productes = unique(DiariFASV$Producte)

Materials <- Materials %>%
    drop_na(Producte) %>%
    distinct(Producte, .keep_all = TRUE) %>%
    select(-Quantitat)  #No tinc clar què indica Quantitat, hi ha packs de 20 que val 10
ProdDiariFASV <- 
    tibble(Producte = Productes) %>%
    left_join(Materials, by = "Producte")
#Associem cada P amb un semifabricat
EscandallX <- Escandall %>%
    group_by(Producte) %>%
    filter((EATCOD == "X") | (EATCOD == "P") | ((EATCOD == "T") & (!any(EATCOD == "X")))) %>% #Triem les X, les P i les T que no tenen X
    filter(!(str_detect(Producte, "PACK TEST"))) %>%
    select(Producte, Semifabricat, QuantitatRef)
Producte_Repetits = unique(EscandallX$Producte[which(duplicated(EscandallX$Producte))])
Targes <- c("S-TMB-PP1-SMD", "S-MEM-500", "S-TMR-CMA", "S-TMR-CM0", "S-TMR-CP0", 
              "S-MEM-1000", "S-MEM-2000", "S-RTDS418", "S-RTD221-5V", "S-T.SEC", 
              "S-RTDP-SMD", "S-PQCOM-KPBL", "S-RTDS433", "S-TMR-R-SMD", "S-MAXIC-FR",
              "S-BASIC4-ELEC", "S-BASIC4-PILO", "S-RQD", "BOB-NEOPROX 66", "S-PQCOM-BL", 
              "S-PQCOM-KP-M", "S-PQCOM-SW", "S-MANA-NOU-FR", "S-TMB-PP1-SMD", "S-LLAVE JCM",
              "S-PQCOM-LCD", "S-RSEC3", "S-PQCOM-KP-RM", "S-TMR-500", "S-T.LOGICA", "S-MDLX", 
              "S-RTD-A", "S-RTD-B","S-RTDP-SMD", "S-DN3-A", "S-CABLE PINZA", "S-GND", "S-RQDP",
              "S-LLAVE PUJ", "S-PQCOM-KP-MB", "S-PNT-P", "S-MDA2", "S-LLAVE MAT", "S-GND3P", "S-SENSE-R",
              "S-ROLL-IND", "S-BASIC4-ELECP", "S-GTC", "S-TRP", "S-LLAVE SM", "S-PQCOM-APRG",
              "S-A.RADIO CT2-R", "S-AGBD", "S-PQCOM-BL-L", "S-PQCOM-KP-RMP", "S-RTD52BS868", 
              "S-CPA-D", "S-PQCOM-SECTRI", "S-TWIN-R-ST1", "S-PQCOM-SEC", "S-LLAVE FOR", "S-CPA-D",
              "S-NEO2-X", "X S-RTC-B", "S-RTD221-5V-916", "S-AFC-SMD", "S-T-EXP-INPUTS",
              "S-GOKEY_24PSU", "S-NEO1", "S-LLAVE TEL", "S-GOBIOB433", "S-DS3-A", "S-SONAR",
              "S-CMAIN1E-AT", "S-RTC-B", "S-PMGZN2", "S-GO2-RFPIC", "S-GO4-RFPIC", "S-RTC-PUJ")
#Traiem les referencies que estan duplicades i el duplicat és un tarja
EscandallX <- EscandallX %>% 
    filter( !((Producte %in% Producte_Repetits) & (Semifabricat %in% Targes)))

#Traiem les referencies que estan duplicades i el duplicat és un equip
Producte_Repetits = unique(EscandallX$Producte[which(duplicated(EscandallX$Producte))])
EquipsKit <- c("S-RBAND/UMM", "S-RBAND/TBX-B", "S-ROLLER-QZ")
EscandallX <- EscandallX %>% 
    filter( !((Producte %in% Producte_Repetits) & (Semifabricat %in% EquipsKit))) 

#Afegim columna amb el semifabricat
ProdDiariFASV <- ProdDiariFASV %>%
    left_join(EscandallX, by = "Producte")

#Busquem tots els semifabricats que tenen una Jeraquia assignada
XToJerarquia <- ProdDiariFASV %>%
    drop_na(Jerarquia) %>%
    drop_na(Semifabricat) %>%
    distinct(Semifabricat, .keep_all = TRUE) %>%
    select(-Producte, -Descripcio)

#Omplim els que queden perquè són X que ja no estan a SAP
NouX <- read_csv("ProjectesFASV/XToJerarquia.csv",
    col_types = cols(
        Gama = col_character(),
        NomGama = col_character(),
        Jerarquia = col_integer(),
        GrupPr = col_integer(),
        NomGrupPr = col_character(),
        Jprod = col_character(),
        NomJprod = col_character(),
        GrupArt = col_character(),
        NomGrupArt = col_character(),
        Projecte = col_character(),
        Semifabricat = col_character(),
        QuantitatRef = col_double()
    )
)
XToJerarquia <- XToJerarquia %>%
    bind_rows(NouX)

#Assignem Jerarquia a tot els Productes
EscandallXJerarquia <- EscandallX %>%
    left_join(XToJerarquia, by = "Semifabricat")

#Busquem les Jerarquies de les P per assignar-los als packs
PToJerarquia <- EscandallXJerarquia %>%
    drop_na(Jerarquia) %>%
    drop_na(Producte) %>%
    distinct(Producte, .keep_all = TRUE) %>%
    select(-Semifabricat)
    
PToJerarquia <- rename(PToJerarquia, Semifabricat = Producte)

ProdFASV <- ProdDiariFASV %>%
    left_join(XToJerarquia, by = "Semifabricat") %>%
    transmute(
        Producte, 
        Jerarquia = ifelse(is.na(Jerarquia.x), Jerarquia.y, Jerarquia.x),
        NomGama = ifelse(is.na(NomGama.x), NomGama.y, NomGama.x),
        Projecte = ifelse(is.na(Projecte.x), Projecte.y, Projecte.x),
        Semifabricat
    )

ProdFASV <- ProdFASV %>%
    left_join(PToJerarquia, by = "Semifabricat") %>%
    transmute(
        Producte, 
        Jerarquia = ifelse(is.na(Jerarquia.x), Jerarquia.y, Jerarquia.x),
        NomGama = ifelse(is.na(NomGama.x), NomGama.y, NomGama.x),
        Projecte = ifelse(is.na(Projecte.x), Projecte.y, Projecte.x),
        Semifabricat
    )

#Assignem projectes segons el semifabricat
ProdFASV <- ProdFASV %>%
    left_join(XToProj, by = "Semifabricat") %>%
    transmute(
        Producte,
        Jerarquia,
        NomGama,
        Projecte = ifelse(is.na(Projecte.x), Projecte.y, Projecte.x),
        Semifabricat
    )

#Posem noms a les jerarquies
ProdFASV <- ProdFASV %>%
    left_join(NomsJerarquies, by = "Jerarquia") 

write_csv(ProdFASV, "ProjectesFASV/ProdFASV.csv")

