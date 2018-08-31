#--------------------------LLIBRERIES---------------------------------
library(tidyverse)
library(stringr)
Sys.setenv(TZ="Europe/Madrid")
library(lubridate)

#----------------------------FUNCIONS----------------------------
#************************CalculaROI***********************
#Calcula el ROI dels projectes segons una data
#DatFrame: Ha de ser el fitxer Resum
#DatX: Data per calcular ROI 
#Grafica: TRUE=Genera una gràfica del ROI
#Fitxer: TRUE=Genera un fitxer del ROI
#*********************************************************
CalculaROI = function (DatFrame, DatX, Grafica, Fitxer){
    #DatFrame <- ResumROI  #Per test de la funció
    #DatX = ymd("2018-01-01")  #Per test de la funció
    DatFrame <- DatFrame %>%
        filter(!(Projecte %in% c("VARISONE", "VARIS", "VARISG5", "SAP", "MANUALSCA", "SJD", "CENTRE_BULGARIA", "GO", "OFTECNICA", "PROJMARKETING1", "VERSUS"))) %>%
        filter(!str_detect(Projecte, "^_E_")) %>%
        filter(!str_detect(Projecte, "FIRA")) %>%
        group_by(Projecte) %>%
        mutate(DataInici = min(Data))
    
    DatFrame$Suma <- DatFrame$Suma + DatFrame$Previsio     #Posem el que es previsio com si fos real
    DatFrame$Despesa <- DatFrame$Despesa + DatFrame$PrevisioDespesa
    DatFrame$TotalMarge <- DatFrame$TotalMarge + DatFrame$PrevisioMarge
    DatFrame$DataInici[which(DatFrame$Projecte == "ASSISTCLOU")] = ymd("2015-12-01") #Ladata calculada del ASSISTCLOU estava malament perunaPre-inception
    DatFrame$DataInici[which(DatFrame$Projecte == "FTHIRTY")] = ymd("2015-08-26") #Ladata calculada del FTHIRTY la comptem a partir de que torna a arrancar per 2n cop
    DatFrame$DataInici[which(DatFrame$Projecte == "ACCESSVK")] = ymd("2014-02-04") #La data calculada no quadrava amb la del fitxer de Seguiment d'objectius
    DatFrame$DataInici[which(DatFrame$Projecte == "BASEMN")] = ymd("2015-04-16") #La data calculada no quadrava amb la del fitxer de Seguiment d'objectius
    DatFrame$DataInici[which(DatFrame$Projecte == "BELROLL")] = ymd("2014-07-25") #La data calculada no quadrava amb la del fitxer de Seguiment d'objectius
    DatFrame$DataInici[which(DatFrame$Projecte == "FCCULRB")] = ymd("2015-04-27") #La data calculada no quadrava amb la del fitxer de Seguiment d'objectius
    DatFrame$DataInici[which(DatFrame$Projecte == "GOBIOK")] = ymd("2017-01-01") #La data calculada no quadrava amb la del fitxer de Seguiment d'objectius
    DatFrame$DataInici[which(DatFrame$Projecte == "DMRIN")] = ymd("2015-07-01") #La data calculada no quadrava amb la del fitxer de Seguiment d'objectius

    DatFrame <- DatFrame %>%  #Eliminem aquests projectes perquè no conten pel ROI
        filter(Projecte != "Racionalitzacio Quadres") %>%
        filter(Projecte != "Control")
    
    DatesIniciProjecte <- DatFrame %>%
        select(Projecte, DataInici) %>%
        distinct(Projecte, .keep_all = TRUE) %>%
        arrange(DataInici)
    DatesIniciProjecte$Antiguitat = 0
    DatesIniciProjecte$Antiguitat[which(DatesIniciProjecte$DataInici >= (DatX - dyears(2)))] = 2
    DatesIniciProjecte$Antiguitat[which(DatesIniciProjecte$DataInici >= (DatX - dyears(1)))] = 1

    DatFrame <- DatFrame %>%
        filter(Data <= (DataInici + dyears(3))) %>%
        filter(Data >= DataInici) %>%
        filter(Data >= (DatX - dyears(3))) %>%
        filter(Data <= DatX)

    ResumID <- DatFrame %>%
        group_by(Projecte) %>%
        summarise(Marge = sum(TotalMarge), Inversio = sum(Despesa))
    
    DatesIniciProjecte <- left_join(DatesIniciProjecte, ResumID, by = "Projecte")
    DatesIniciProjecte[is.na(DatesIniciProjecte)] = 0
    
    write_csv(DatesIniciProjecte, "Fitxers/DatesInici.csv")

    if(Fitxer){
        Fitxer <-DatFrame %>%
            mutate(AnyMes = format(Data,"%Y%m")) %>%
            group_by(Projecte, AnyMes) %>%
            summarise(Total = sum(Suma), Marge =sum(TotalMarge), Inversio = sum(Despesa))
        Export <- Fitxer %>% #Exportem ROI
            select(-Marge,-Inversio) %>% #De moment només exportem Total 
            spread(key = AnyMes, value = Total)
        Export[is.na(Export)] <- 0
        write_csv(Export, paste("Fitxers/ROI", format(DatX, "%Y%m%d"),".csv"))
        Export <- Fitxer %>% #Exportem Marge
            select(-Total,-Inversio) %>% #De moment només exportem Total 
            spread(key = AnyMes, value = Marge)
        Export[is.na(Export)] <- 0
        write_csv(Export, paste("Fitxers/ROIMarge", format(DatX, "%Y%m%d"),".csv"))
        Export <- Fitxer %>% #Exportem Inversio
            select(-Total,-Marge) %>% #De moment només exportem Total 
            spread(key = AnyMes, value = Inversio)
        Export[is.na(Export)] <- 0
        write_csv(Export, paste("Fitxers/ROIInversio", format(DatX, "%Y%m%d"),".csv"))
        Export <- Fitxer %>%
            group_by(Projecte) %>%
            summarise(Total = sum(Total)) %>%
            arrange(desc(Total))
        write_csv(Export, paste("Fitxers/ROIRanking", format(DatX, "%Y%m%d"),".csv"))
    }

    Resultat <- c(sum(DatFrame$TotalMarge), sum(DatFrame$Despesa), sum(DatFrame$Suma))
    return(Resultat)
}    

#************************Grafica9***********************
#Treu la gràfica Marge/Despesa per dies de 9 projectes
#Proj: Array de projectes
#*********************************************************
Grafica9 = function (ProjList){
    for (i in 1:9){
        NomProjecte = ProjList[i]
        Grafica = subset(Grafiques, Grafiques$Projecte == NomProjecte)
        g = ggplot(arrange(Grafica,Data), aes(x = Data)) +
            ggtitle(paste("Margin(Black) + Invest(Lblue) + Marg-Inv(Blue) + Forec(Red)-", NomProjecte)) + 
            theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
            theme(axis.text = element_text(size = 8)) + 
            labs(y = "Margin / Invest", x = "Time") +
            geom_line(aes(y = cumsum(Suma)), colour = "Blue") +  #Per fer cum sum Data ha d'estar ordenat
            geom_bar(aes(y = TotalMarge),stat = "identity") +
            geom_line(aes(y = cumsum(-Despesa)), colour = "Blue3", alpha = 0.4) +
            geom_line(aes(y = cumsum(Previsio)), colour = "Red", alpha = 0.4) +
            ylim(-170000, 100000)
        print(g)
        ggsave(paste("Grafiques/FluxeCaixa", NomProjecte, ".pdf"), width = 18, height = 18, units = "cm")
    }
}

#************************GraficaSumaProjectes***********************
#Fem gràfica de varis projectes sobreposats
#Proj: Array de projectes
#*********************************************************
GraficaSumaPropjectes = function (ProjList, Fitxer){
    GrupProjectes <- Grafiques %>%
        filter(Data < DataMaxima) %>%
        filter(
                Projecte == ProjList[1] | 
                Projecte == ProjList[2] | 
                Projecte == ProjList[3] | 
                Projecte == ProjList[4] | 
                Projecte == ProjList[5] | 
                Projecte == ProjList[6] | 
                Projecte == ProjList[7] | 
                Projecte == ProjList[8] | 
                Projecte == ProjList[9]
        ) %>% arrange(Data)

    GrupProjectes$Acumulat = 0
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[1])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[1])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[2])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[2])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[3])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[3])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[4])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[4])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[5])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[5])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[6])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[6])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[7])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[7])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[8])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[8])])
    GrupProjectes$Acumulat[which(GrupProjectes$Projecte == ProjList[9])] = cumsum (GrupProjectes$Suma[which(GrupProjectes$Projecte == ProjList[9])])
    g = ggplot(GrupProjectes) +
        geom_line(aes(x=Data, y=Acumulat, color=Projecte, group=Projecte)) + 
        ylab("Margin - Invest") +  #Per fer cum sum Data ha d'estar ordenat
        ggtitle("Projects cashflow") +
        ylim(-160000, 60000) +
        theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
        theme(axis.text = element_text(size = 8)) 
    print(g)
    ggsave(paste("Grafiques/ResumProjectes", Fitxer, ".pdf"), width = 18, height = 18, units = "cm")
}
#-------------------CÀRREGA DE DADES i NETEJA-------------------------
#--------------------------Diari--------------------------------------
#Càrrega del fitxer Diari de facturació. Primer de SAP
Diari_old <- read_csv(
	"Dades/Diari160101171231.csv", 
	locale = locale(encoding = "UTF-8", grouping_mark = ",", decimal_mark = "."),
	col_types = cols(
		Material = col_integer(),
		Sector = col_integer(),
  		`Fecha factura` = col_date("%d/%m/%Y"),
  		`Clase de factura` = col_character(),
  		Factura = col_double(),
  		`Nombre 1` = col_character(),
  		`Clave de país` = col_character(),
  		`Número de personal` = col_integer(),
  		`Nombre del empleado o candidato` = col_character(),
  		`Grupo de clientes` = col_integer(),
  		`Nombre Comercial` = col_character(),
  		`Cantidad facturada` = col_number(),
  		`Un.medida venta` = col_character(),
  		Neto. = col_number(),
  		`Valor neto` = col_number(),
  		`Costes internos` = col_number(),
  		`Importe del impuesto` = col_number(),
  		`Moneda del documento` = col_character(),
  		`Margen %` = col_number(),
  		Margen = col_number(),
  		`Grupo principal` = col_integer(),
  		`Denominación` = col_character(),
  		`País receptor` = col_character(),
  		`Jerarquía productos` = col_integer(),
  		`Denominación_1` = col_character(),
  		`Elemento PEP` = col_character(),
  		`Tipo material` = col_character(),
  		`Denominación_2` = col_character()
	)
)

Diari <- read_csv(
    "Dades/Diari180101180731.csv", 
    locale = locale(encoding = "UTF-8", grouping_mark = ",", decimal_mark = "."),
    col_types = cols(
        Material = col_integer(),
        Sector = col_integer(),
        `Fecha factura` = col_date("%d/%m/%Y"),
        `Clase de factura` = col_character(),
        Factura = col_double(),
        `Nombre 1` = col_character(),
        `Clave de país` = col_character(),
        `Número de personal` = col_integer(),
        `Nombre del empleado o candidato` = col_character(),
        `Grupo de clientes` = col_integer(),
        `Nombre Comercial` = col_character(),
        `Cantidad facturada` = col_number(),
        `Un.medida venta` = col_character(),
        Neto. = col_number(),
        `Valor neto` = col_number(),
        `Costes internos` = col_number(),
        `Importe del impuesto` = col_number(),
        `Moneda del documento` = col_character(),
        `Margen %` = col_number(),
        Margen = col_number(),
        `Grupo principal` = col_integer(),
        `Denominación` = col_character(),
        `País receptor` = col_character(),
        `Jerarquía productos` = col_integer(),
        `Denominación_1` = col_character(),
        `Elemento PEP` = col_character(),
        `Tipo material` = col_character(),
        `Denominación_2` = col_character()
    )
)

Diari <- bind_rows(Diari_old, Diari)

#Neteja de les dades: Eliminar les que tenen un NA al número de factura(els totals)
Diari <- Diari %>% drop_na(Factura)

#Canviem noms de columnes per fer-ho més entenedor
Diari <- rename(Diari,
	Data = `Fecha factura`,
  	ClaseFact = `Clase de factura`,
  	Client = `Nombre 1`,
  	Pais = `Clave de país`,
  	NumVenedor = `Número de personal`,
  	Venedor = `Nombre del empleado o candidato`,
  	GrupClient = `Grupo de clientes`,
  	NomProducteAb = `Nombre Comercial`,
  	Unitats = `Cantidad facturada`,
  	Mesura = `Un.medida venta`,
  	Import = Neto.,
  	Import2 = `Valor neto`,  #Import de la línea de factura + la part de transports proporcional
  	Costos = `Costes internos`,
  	Impostos = `Importe del impuesto`,
  	Moneda = `Moneda del documento`,
  	MargePerc = `Margen %`,
  	Marge = Margen,
  	Grup = `Grupo principal`,
  	NomGrup = `Denominación`,
  	PaisR = `País receptor`,
  	Jerarquia = `Jerarquía productos`,
  	NomJerarquia = `Denominación_1`,
  	Projecte = `Elemento PEP`,
  	TipusMat = `Tipo material`,
  	NomProducte = `Denominación_2`)

Diari <- Diari %>%
    select(
        Data, 
        Client,
        Pais,
        Unitats,
        Import,
        Costos,
        MargePerc,
        Marge,
        NomGrup,
        NomJerarquia,
        Projecte,
        NomProducte
    )

#--------------------------DiariFASV---------------------------------------
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
        QTT = col_number(),
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
    Costos = COST
)

DiariFASV <- DiariFASV %>%
    select(
        Data,
        Client,
        Pais,
        Unitats,
        Import,
        Costos,
        Marge,
        Producte
    )
DiariFASV <- subset(DiariFASV, Data < "2016-01-01")

#--------------------------Despeses-----------------------------------
#Càrega del fitxer de Despeses
DespesesPRESAP <- read_csv("Dades/DespesesPRESAP.csv", 
	locale = locale(encoding = "Latin1", grouping_mark = ",", decimal_mark = "."),
	col_types = cols(
  		Data = col_date("%d-%b-%y"),
  		TipusDespesa = col_character(),
  		CodiProjecte = col_integer(),
  		NomProjecte = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
  		Concepte = col_character(),
  		Import = col_number(),
  		Comentari = col_character(),
  		Persona = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
  		InversioHores = col_integer(),
  		ImportHores = col_number(),
  		ImportInversio = col_number()
  	)
)
	
DespesesSAP_old <- read_tsv("Dades/DespesesSAP160101171231.csv", 
                        locale = locale(encoding = "Latin1", grouping_mark = ".", decimal_mark = ","),
                        skip = 5,
                        col_types = cols(
                            Elem.PEP = col_character(),
                            `DenominaciÃ³n` = col_character(),
                            Fe.contab. = col_date("%d.%m.%Y"),
                            `AÃ±o` = col_integer(),
                            `DescripciÃ³n` = col_character(),
                            `Val/MScCO` = col_number(),
                            MSoCO = col_character()
                        )
)

DespesesSAP <- read_tsv("Dades/DespesesSAP180101180731.csv", 
                            locale = locale(encoding = "Latin1", grouping_mark = ".", decimal_mark = ","),
                            skip = 5,
                            col_types = cols(
                                Elem.PEP = col_character(),
                                `DenominaciÃ³n` = col_character(),
                                Fe.contab. = col_date("%d.%m.%Y"),
                                `AÃ±o` = col_integer(),
                                `DescripciÃ³n` = col_character(),
                                `Val/MScCO` = col_number(),
                                MSoCO = col_character()
                            )
)

DespesesSAP <- bind_rows(DespesesSAP, DespesesSAP_old)

DespesesSAP <- rename(DespesesSAP,
	Projecte = Elem.PEP,
	DescProjecte = `DenominaciÃ³n`,
	Data = Fe.contab.,
	Any = `AÃ±o`,
	Descripcio = `DescripciÃ³n`,
	Import = `Val/MScCO`,
	Moneda = MSoCO
)
#Neteja de les dades: Eliminar les que tenen un NA al número de factura(els totals)
DespesesSAP <- DespesesSAP %>% drop_na(Projecte)

DespesesSAP <- DespesesSAP %>%
	replace_na(list(Descripcio = ""))

DespesesSAP$ImportHores = 0
DespesesSAP$ImportHores[which(DespesesSAP$Descripcio=="Mano de obra proyectos")] = DespesesSAP$Import[which(DespesesSAP$Descripcio == "Mano de obra proyectos")]
DespesesSAP$ImportHores[is.na(DespesesSAP$ImportHores)] = 0

DespesesSAP$ImportInversio = 0
DespesesSAP$ImportInversio[which(DespesesSAP$Descripcio != "Mano de obra proyectos")] = DespesesSAP$Import[which(DespesesSAP$Descripcio != "Mano de obra proyectos")]
DespesesSAP$ImportInversio[is.na(DespesesSAP$ImportInversio)] = 0

#Creació d'un dataframe únic de Despeses
Despeses <- tibble(
	Projecte = as.character(DespesesSAP$Projecte), 
	Data = DespesesSAP$Data, 
	Import = DespesesSAP$Import, 
	ImportHores = DespesesSAP$ImportHores, 
	ImportInversio = DespesesSAP$ImportInversio
)
Despeses <- bind_rows(Despeses, 
	tibble(
		Projecte = as.character(DespesesPRESAP$NomProjecte), 
		Data = DespesesPRESAP$Data, 
		Import = DespesesPRESAP$Import, 
		ImportHores = DespesesPRESAP$ImportHores, 
		ImportInversio = DespesesPRESAP$ImportInversio
	)
)
Despeses$Projecte <- factor(Despeses$Projecte)

#Les desepeses d'ECLOUD les assignem a EBASE
Despeses$Projecte[which(Despeses$Projecte == "ECLOUD")] = "EBASE"

#--------------------------Queixes---------------------------
#Carreguem queixes
QueixesPRESAP <- read_csv("Dades/Queixes_2012-2015.csv",
	locale = locale(encoding = "Latin1", grouping_mark = ".", decimal_mark = ","),
	col_types = cols(
		`ÿ"N¼ Queixa"` = col_character(),
  		`Data Recepci` = col_date("%d/%m/%y"),
  		Client = col_character(),
  		`Grup Responsable` = col_character(),
  		Equip = col_character(),
  		`Situaci` = col_character()
  	)
)

QueixesPRESAP <- rename(QueixesPRESAP,
	NrQueixa = `ÿ"N¼ Queixa"`,
	Data = `Data Recepci`,
	Responsable = `Grup Responsable`,
	Situacio = `Situaci`
)

ConvQueixaProj <- read_csv("Dades/ConversorQueixaProjecte.csv",
	locale = locale(encoding = "Latin1", grouping_mark = ".", decimal_mark = ","),
	col_types = cols(
  		Equip = col_character(),
  		Projecte = col_character()
	)
)

QueixesPRESAP <- QueixesPRESAP %>%
	left_join(ConvQueixaProj, by = "Equip")

QueixesSAP <- read_csv("Dades/QueixesSAP180228.csv",
	locale = locale(encoding = "Latin1", grouping_mark = ",", decimal_mark = "."),
	col_types = cols(
  		Aviso = col_integer(),
  		STATUS = col_character(),
  		`Descripcin` = col_character(),
  		`Fecha de aviso` = col_date("%d/%m/%Y"),
  		`Cierre por fecha` = col_character(),
  		Cliente = col_integer(),
  		`Nombre 1` = col_character(),
  		`Nombre Comercial` = col_character(),
  		`Grupo de artculos` = col_character(),
  		`Denom.gr-artculos` = col_character(),
  		`Jerarqua productos` = col_integer(),
  		`Denominacin` = col_character(),
  		`Elemento PEP` = col_character(),
  		`T.SINTOMA AVERIA` = col_character()
  	)
)

QueixesSAP <- rename(QueixesSAP,
		Data = `Fecha de aviso`,
		Responsable = `T.SINTOMA AVERIA`,
		Projecte = `Elemento PEP`
)

Queixes <- QueixesPRESAP %>%
	select(
		Data,
		Responsable,
		Projecte
	) %>% mutate(
		Queixa = -100 #100 = Queixa
	)

QueixesS <- QueixesSAP %>%
	select(
		Data,
		Responsable,
		Projecte
	) %>% mutate(
		Queixa = -100 #100 = Queixa
	)

Queixes <- bind_rows(Queixes, QueixesS)

#--------------------------Previsions---------------------------
Previsions <- read_csv("Dades/Previsions180308.csv",
                        locale = locale(encoding = "Latin1", grouping_mark = ",", decimal_mark = "."),
                        skip = 5,
                        col_types = cols(
                            PRE_ROI = col_character(),
                            Projecte = col_character(),
                            `M_11/17` = col_number(),
                            `C_11/17` = col_number(),
                            `M_12/17` = col_number(),
                            `C_12/17` = col_number(),
                            `M_01/18` = col_number(),
                            `C_01/18` = col_number(),
                            `M_02/18` = col_number(),
                            `C_02/18` = col_number(),
                            `M_03/18` = col_number(),
                            `C_03/18` = col_number(),
                            `M_04/18` = col_number(),
                            `C_04/18` = col_number(),
                            `M_05/18` = col_number(),
                            `C_05/18` = col_number(),
                            `M_06/18` = col_number(),
                            `C_06/18` = col_number(),
                            `M_07/18` = col_number(),
                            `C_07/18` = col_number(),
                            `M_08/18` = col_number(),
                            `C_08/18` = col_number(),
                            `M_09/18` = col_number(),
                            `C_09/18` = col_number(),
                            `M_10/18` = col_number(),
                            `C_10/18` = col_number(),
                            `M_11/18` = col_number(),
                            `C_11/18` = col_number(),
                            `M_12/18` = col_number(),
                            `C_12/18` = col_number(),
                            X31 = col_character(),
                            X32 = col_character(),
                            X33 = col_character(),
                            X34 = col_character(),
                            X35 = col_character(),
                            X36 = col_character(),
                            X37 = col_character(),
                            X38 = col_character()
                        )
)

Previsions <- Previsions %>% 
    select(-starts_with("X")) %>%
    filter(!is.na(PRE_ROI))

PrevisionsMarge <- Previsions %>% 
    select(starts_with("M"), Projecte) %>% 
    gather(starts_with("M"),key="Data",value="PrevisioMarge")

PrevisionsMarge$Data <- as.Date(strptime(gsub("M_", "15/", PrevisionsMarge$Data), "%d/%m/%y"))
PrevisionsMarge$PrevisioMarge[is.na(PrevisionsMarge$PrevisioMarge)] <- 0
PrevisionsMarge$PrevisioDespesa <- 0

PrevisionsInversio <- Previsions %>% 
    select(starts_with("C"), Projecte) %>% 
    gather(starts_with("C"),key="Data",value="PrevisioDespesa")

PrevisionsInversio$Data <- as.Date(strptime(gsub("C_", "15/", PrevisionsInversio$Data), "%d/%m/%y"))
PrevisionsInversio$PrevisioDespesa[is.na(PrevisionsInversio$PrevisioDespesa)] <- 0
PrevisionsInversio$PrevisioMarge <- 0

Previsions <- bind_rows(PrevisionsInversio, PrevisionsMarge)
Previsions$Responsable <- as.character(" ")
Previsions$Queixa <- as.double(NA)
Previsions$Previsio <- Previsions$PrevisioMarge - Previsions$PrevisioDespesa

#-----------------UNIÓ DELS DOS DIARIS-----------------------------
DiariFASV <- DiariFASV %>%
    mutate(MargePerc = Marge/Import*100)

#Carreguem taula que assigna projecte i jerarquia a cada producte
ProdFASV <- read_csv("ProjectesFASV/ProdFASV.csv",
    col_types = cols(
        Producte = col_character(),
        Jerarquia = col_integer(),
        NomGama = col_character(),
        Projecte = col_character(),
        Semifabricat = col_character(),
        NomJerarquia = col_character(),
        Nivell = col_integer()
    )
)

ProdFASV <- ProdFASV %>%
    select(NomJerarquia, Projecte, Producte)

DiariFASV <- DiariFASV %>%
    left_join(ProdFASV, by = "Producte") %>%
    rename(NomProducte = Producte)

Diari$Projecte <- as.character(Diari$Projecte)
Diari$NomJerarquia <- as.character(Diari$NomJerarquia)
Diari$Client <- as.character(Diari$Client)
Diari$Pais <- as.character(Diari$Pais)

#No tenim informació de grup a ProdFASV
DiariGlobal <- Diari %>%  
    select(-NomGrup) %>%
    bind_rows(DiariFASV) 

#Canviem els marges dels projectes MOTIONFCC i CRISTALLGO
DiariGlobal <- DiariGlobal %>%
    filter ((Projecte != "CRISTALLGO")|(is.na(Projecte))) %>%
    filter ((Projecte != "MOTIONFCC")|(is.na(Projecte)))

DiariGlobal <- DiariGlobal %>%   #MargeIncorrecte
    add_row (Data = c("2016-07-15", "2016-08-15", "2016-09-16", "2016-10-16", "2016-11-16", "2016-12-15", "2017-01-15", "2017-02-15", "2017-03-15", "2017-04-15", "2017-05-15", "2017-06-15", "2017-07-15", "2017-08-15", "2017-09-15", "2017-10-15", "2017-11-15", "2017-12-15","2018-01-15","2018-02-15","2018-03-15","2018-04-15","2018-05-15","2018-06-15","2018-07-15","2018-08-15","2018-09-15","2018-10-15","2018-11-15","2018-12-15","2019-01-15"),
             Client = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""),
             Pais = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""),
             Unitats = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
             Import = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             Costos = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             MargePerc = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
             Marge = c(12,5,0,203,763,935,1112,1258,1854,1289,1718,1849,2173,998,2117,2367,2264,1684,1999,2079,1701,0,0,0,0,0,0,0,0,0,0),
             NomJerarquia = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""),
             Projecte = c("CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO","CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO", "CRISTALLGO"),
             NomProducte = c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
    ) %>%
    add_row (Data = c("2017-10-15", "2017-11-15", "2017-12-15"),
             Client = c("","",""),
             Pais = c("","",""),
             Unitats = c(1,1,1),
             Import = c(0,0,0),
             Costos = c(0,0,0),
             MargePerc = c(0,0,0),
             Marge = c(4175,0,0),
             NomJerarquia = c("","",""),
             Projecte = c("MOTIONFCC","MOTIONFCC", "MOTIONFCC"),
             NomProducte = c("","","")
    ) %>%
    add_row (Data = c("2018-03-15"), #Pagament Teckentrup
             Client = c(""),
             Pais = c(""),
             Unitats = c(1),
             Import = c(0),
             Costos = c(0),
             MargePerc = c(0),
             Marge = c(10000),
             NomJerarquia = c(""),
             Projecte = c("KEYBEL"),
             NomProducte = c("")
    )


#-----------------CREAR TIBBLE RESUM (Diari, Despesa, Queixes)-----------------------
#Acumulem a cada dia la facturació i les despeses per cada projecte
DiariSumat <- DiariGlobal %>%
	group_by(Data, Projecte) %>%
	summarize(TotalMarge = sum(Marge))
DiariSumat$Despesa = 0
DiariSumat$Projecte <- as.character(DiariSumat$Projecte)

DespesesSumat <- Despeses %>%
	group_by(Data, Projecte) %>%
	summarize(Despesa = sum(Import))
DespesesSumat$TotalMarge = 0
DespesesSumat$Projecte <- as.character(DespesesSumat$Projecte)
	
#Ajuntem Diari, Despeses i queixes en un tibble
Resum <- bind_rows(DespesesSumat, DiariSumat)
Resum$Responsable = " "
Resum$Queixa = NA

Queixes$Despesa = as.double(0)
Queixes$TotalMarge = as.double(0)

Resum <- bind_rows(Resum, Queixes)
Resum$Suma = Resum$TotalMarge - Resum$Despesa

Resum$Projecte <- factor(Resum$Projecte)

ResumNoPrevisio <- Resum  #Es el Resum, pero sense cap dada de previsió, Resum real, tant de vendes com despeses

#------------------------------AFEGIM PREVISIO a RESUM------------------------------
Previsions$Suma <- as.double(0)
Previsions$Despesa <- as.double(0)
Previsions$TotalMarge <- as.double(0)
Resum$Previsio <- as.double(0)
Resum$PrevisioDespesa <- as.double(0)
Resum$PrevisioMarge <- as.double(0)

Resum$Projecte <- as.character(Resum$Projecte)

Resum <- Resum %>%   
    bind_rows(Previsions)
# A Resum hi tenim el real i les previsions. Ull que hi pot haver mesos amb dades reals i previsions. Les traurem més endavant
#------------------------------PREPARAR Previsio per graficar i ROI---------------------
#Inicialitzem Previsio amb el darrer resultat acumulat
Acumulat <- Resum %>%
    group_by(Projecte) %>%
    summarise(Previsio = sum(Suma), PrevisioDespesa = sum(Despesa), PrevisioMarge = sum(TotalMarge))

AcumulatData <- ResumNoPrevisio %>%  #Busquem la darrera data amb un apunt real de Diari o Despesa
    group_by(Projecte) %>%
    filter(Data == max(Data)) %>%
    filter(!duplicated(Data)) 

AcumulatData$Despesa <- 0
AcumulatData$TotalMarge <- 0
AcumulatData$Suma <- 0
AcumulatData$Responsable <- " "
AcumulatData$Queixa <- NA

AcumulatData$Projecte <- as.character(AcumulatData$Projecte)

AcumulatData <- AcumulatData %>%  #Fem llista amb projecte, data màxima d'apunt i total acumulat
    left_join(Acumulat, by = "Projecte") %>%
    filter(Projecte %in% Previsions$Projecte)

#Traiem les previsions que ja hi hagi dades reals i afegim acumulat a una nova taula Grafiques
DataMaxima <- max(AcumulatData$Data)
ResumROI <- Resum %>%  #Ara ResumROI té dades reals i quan s'acaben les dades reals enllacen amb previsions
    filter(!((Data < DataMaxima) & (Previsio !=0)))

Grafiques <- ResumROI %>%
    bind_rows(AcumulatData)

ResumPressupostROI <- Resum %>%  #Eliminem totes les dades reals del 2018 ResumPressupostROI té dades reals fins 31/12/17 i pressupost per 2018
    filter(!((Data >= "2018-01-01") & (Previsio == 0))) %>%
    filter(!((Data < "2018-01-01") & (Previsio != 0)))

#------------------------------GRAFICAR------------------------------

NomsProjectes = c("INDSPD", "FCCULRB", "CAPACTIVE", "CAPT868RB3", "DMRIN", "RB3MILLER", "NFCERT", "BELROLL", "FTHIRTY") #G1
Grafica9(NomsProjectes)
GraficaSumaPropjectes(NomsProjectes,"G1")

NomsProjectes = c("KEELOCK", "ACCESSVK", "BASEMN", "SOLARRB1", "MOTIONFCC", "KEYBEL", "MOTION433", "RXMAGNET", "FREETV2") #G2
Grafica9(NomsProjectes)
GraficaSumaPropjectes(NomsProjectes,"G2G3")

NomsProjectes = c("FREETV2", "CAPT868RB3", "MOTION433", "KEYBEL", "BELROLL", "FTHIRTY", "NFCERT", "KEELOCK", "ASSISTCLOU") #PRE
Grafica9(NomsProjectes)
GraficaSumaPropjectes(NomsProjectes,"1")

NomsProjectes = c("AssistCloud v2", "F30_V2", "EBASE", "PROXUSA", "CAPT868CND", "RXCONNECT", "Sesame", "RB3DITEC", "RB3PERIMET") #ROI
Grafica9(NomsProjectes)
GraficaSumaPropjectes(NomsProjectes,"2")

NomsProjectes = c("CAPMOD", "RB3 TALLAFOC", "GOBIOK", "ADAPMEMDC", "CRISTALLGO", "MOTIONFCC", "TEMOTION", "RADIOEYE", "RB3GFA") #ROI / PRE
Grafica9(NomsProjectes)
GraficaSumaPropjectes(NomsProjectes,"3")

NomsProjectes = c("RMARLC", "M8", "VERSUS - PQCOM", "aa", "aa", "aa", "aa", "aa", "aa") #ROI / PRE
Grafica9(NomsProjectes)



#Grafica del temps de protos i vendes del grup de projectes
DiesVendes = data.frame( Projecte = factor(rep("", 36)), 
	Dada = factor(rep("", 36)),
	Temps = rep(0, 36),
	stringsAsFactors=FALSE)
levels(DiesVendes$Projecte) = unique(NomsProjectes)
levels(DiesVendes$Dada) = c("Temps venda 100/mes", "Temps venda 10", "Temps 1a venda", "Temps primera mostra")
Diari$Mes = format(Diari$Data,"%Y-%m")
DiariMesProducte = ddply(Diari, .(Mes, Elemento.PEP), summarise, Unitats=sum(Cantidad.facturada))
DiariMesProducte$Mes = as.Date(strptime(paste(DiariMesProducte$Mes, "-15"), "%Y-%m -%d"))
for (i in 1:9){
	DataInici = min(Despeses$Data[which(Despeses$Projecte == NomsProjectes[i] & Despeses$Import > 0)]); 
	Data1Mostra = min(Diari$Data[which(Diari$Elemento.PEP == NomsProjectes[i] & Diari$Margen != 0)]); 
	Data1Venda = min(Diari$Data[which(Diari$Elemento.PEP == NomsProjectes[i] & Diari$Margen > 0)]);
	DataVenda10 = min(Diari$Data[which(Diari$Elemento.PEP == NomsProjectes[i] & Diari$Cantidad.facturada >= 10 & Diari$Margen > 0)]);
	DataVenda100Mes = min(DiariMesProducte$Mes[which(DiariMesProducte$Elemento.PEP == NomsProjectes[i] & DiariMesProducte$Unitats >= 100)]);
	DiesVendes$Projecte[4*i] = NomsProjectes[i]
	DiesVendes$Dada[4*i] = "Temps primera mostra"
	DiesVendes$Temps[4*i] = Data1Mostra - DataInici;
	DiesVendes$Projecte[4*i-1] = NomsProjectes[i]
	DiesVendes$Dada[4*i-1] = "Temps 1a venda"
	DiesVendes$Temps[4*i-1] = Data1Venda - DataInici;
	DiesVendes$Projecte[4*i-2] = NomsProjectes[i]
	DiesVendes$Dada[4*i-2] = "Temps venda 10"
	DiesVendes$Temps[4*i-2] = DataVenda10 - DataInici	
	DiesVendes$Projecte[4*i-3] = NomsProjectes[i]
	DiesVendes$Dada[4*i-3] = "Temps venda 100/mes"
	DiesVendes$Temps[4*i-3] = DataVenda100Mes - DataInici	
}
DiesVendes$Temps[which(is.na(DiesVendes$Temps))] = 0
DiesVendes$Temps[which(is.infinite(DiesVendes$Temps))] = 0
g = ggplot(DiesVendes, aes(x = Projecte)) +
	geom_bar(aes(y = Temps, fill = Dada), stat = "identity", position = "dodge", color = "Black") +
	coord_flip() +	
	xlab("Dies") +  
	ggtitle("Dies d'entrada de projectes") +
	theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
	scale_fill_brewer(palette = "Pastel1") +
	guides(fill = guide_legend(reverse = TRUE))
print(g)
ggsave("Grafiques/AgilitatProjectes.pdf", width = 18, height = 18, units = "cm")

#------------------------------ROI--------------------------------
ResultatROI <- CalculaROI(ResumROI, ymd("2018-06-01"), TRUE, TRUE) #ROI a dia d'avui
ResultatPressupostROI <- CalculaROI(ResumPressupostROI, ymd("2018-06-01"), TRUE, TRUE) #ROI segons pressupost

ResultatROI <- CalculaROI(ResumROI, ymd("2019-01-01"), TRUE, TRUE) #ROI a 1 any
ROI <- tibble(
    Data = seq(ymd('2017-01-01'),ymd('2019-01-01'), by = '1 month'),
    Marge = as.double(0),
    Inversio = as.double(0),
    ROI = as.double(0)
)
for(i in 1:25){
    Resultat <- CalculaROI(ResumROI, ROI$Data[i], FALSE, FALSE)
    ROI$Marge[i] <- Resultat[1]
    ROI$Inversio[i] <- Resultat[2]
    ROI$ROI[i] <- Resultat[3]
}
ROI <- ROI %>%
    gather(Marge, Inversio, ROI, key = "Tipus", value = "Dada")

g = ggplot(ROI, aes(x = Data, y = Dada, color = Tipus)) +
    geom_line() + 
    ylab("€") +  
    ggtitle("ROI") +
    theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
    theme(axis.text = element_text(size = 8)) 
print(g)
ggsave("Grafiques/ROI20172018.pdf", width = 18, height = 18, units = "cm")

#------------------------------ALTRES--------------------------------

#Comparativa vendes BASE vs BASEMN
DiariBASE = subset(Diari, (NomJerarquia == "BASE" | NomJerarquia == "BASE MN"))
DiariBASE$VendesBASE = 0
DiariBASE$VendesBASEMN = 0
DiariBASE$MargeBASE = 0
DiariBASE$MargeBASEMN = 0
DiariBASE$VendesBASEMN[which(DiariBASE$Projecte == "BASEMN")] = DiariBASE$Import[which(DiariBASE$Projecte == "BASEMN")]
DiariBASE$VendesBASE[which(DiariBASE$Projecte != "BASEMN")] = DiariBASE$Import[which(DiariBASE$Projecte != "BASEMN")]
DiariBASE$VendesBASE[is.na(DiariBASE$Projecte)] = DiariBASE$Import[is.na(DiariBASE$Projecte)]
DiariBASE$MargeBASEMN[which(DiariBASE$Projecte == "BASEMN")] = DiariBASE$Marge[which(DiariBASE$Projecte == "BASEMN")]
DiariBASE$MargeBASE[which(DiariBASE$Projecte != "BASEMN")] = DiariBASE$Marge[which(DiariBASE$Projecte != "BASEMN")]
DiariBASE$MargeBASE[is.na(DiariBASE$Projecte)] = DiariBASE$Marge[is.na(DiariBASE$Projecte)]

DiariSumatBASE = DiariBASE %>%
    group_by(Data) %>%
    summarise(VendaB = sum(VendesBASE), VendaBMN = sum(VendesBASEMN), MargeB = sum(MargeBASE), MargeBMN = sum(MargeBASEMN))

g = ggplot(arrange(DiariSumatBASE, Data), aes(x = Data)) +
	ggtitle("Comparativa evolució BASE vs BASEMN") + 
	theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
	theme(axis.text = element_text(size = 8)) + 
	labs(y = "Marge / Vendes", x = "Temps") +
	geom_line(aes(y = cumsum(VendaB)), colour = "Blue") +  
	geom_line(aes(y = cumsum(VendaBMN)), colour = "Green") +
	geom_line(aes(y = cumsum(MargeB)), colour = "Blue4") +  
	geom_line(aes(y = cumsum(MargeBMN)), colour = "Green4")
	
print(g)
ggsave("Grafiques/ComparativaBASEvsBASEMN.pdf", width = 18, height = 18, units = "cm")

#Comparativa vendes BASE vs BASEMN - No tinc clar que estigui bé!!!!
DiariBASE = subset(Diari, Denominaci.n.1 == "BASE")
DiariBASE$Projecte = factor("BASE")
levels(DiariBASE$Projecte) = c("BASE", "BASEMN")
DiariBASE$Projecte[which(DiariBASE$Elemento.PEP == "BASEMN")] = "BASEMN"

DiariBASE$Week = week(DiariBASE$Data)
DiariSumatBASE = ddply(DiariBASE, .(Week, Projecte), summarise, VendaB = sum(Neto.), MargeB = sum(Margen))

g = ggplot(arrange(DiariSumatBASE, Week), aes(x = Week, y = MargeB, color = Projecte)) +
	ggtitle("Comparativa evolució BASE vs BASEMN") + 
	theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
	theme(axis.text = element_text(size = 8)) + 
	labs(y = "Marge / Vendes", x = "Temps") +
	geom_bar(aes(y = cumsum(VendaB), fill = Projecte), position = position_stack(reverse =TRUE), stat="identity")
print(g)
ggsave("Grafiques/ComparativaBASEvsBASEMN.pdf", width = 18, height = 18, units = "cm")


#-------------------------CLASSIFICAR DESPESA SEGONS MERCAT------------------------
ProjectesXMercat <- tribble(
    ~Projecte, ~Mercat,
    "KEYBEL", "Unifamiliar",
    "BASEMN", "Plurifamiliar",
    "BELROLL", "Unifamiliar",
    "INDSPD", "Porta rapida",
    "FTHIRTY", "Porta rapida",
    "VERSUS", "Not assigned",
    "DMRIN", "Enrotllable industrial",
    "ACCESSVK", "Plurifamiliar",
    "SOLARRB1", "Corredera industrial",
    "KEECAP", "Corredera industrial",
    "CAPACTIVE", "Corredera industrial",
    "RBAND3", "Not assigned",
    "FCCULRB", "Corredera industrial",
    "NFCERT", "Unifamiliar",
    "GO", "Not assigned",
    "KEELOCK", "Unifamiliar",
    "CRISTALLGO", "Not assigned",
    "RMARLC", "Not assigned",
    "RSENS3", "Not assigned",
    "CAPT868RB3", "Corredera industrial",
    "RB3GFA", "Not assigned",
    "MOTION433", "Plurifamiliar",
    "GOBIOK", "Not assigned",
    "ASSISTCLOU", "Plurifamiliar",
    "FREETV2", "Porta rapida",
    "MOTIONFCC", "Not assigned",
    "RADIOEYE", "Not assigned",
    "RB3MILLER", "Corredera industrial",
    "TEMOTION", "Plurifamiliar",
    "RXMAGNET", "Corredera industrial",
    "CAPMOD", "Not assigned",
    "CAPT868CND", "Plurifamiliar",    
    "ADAPMEMDC", "Not assigned",
    "EBASE", "Plurifamiliar",
    "ECLOUD", "Plurifamiliar"
)

DespesesP <- Despeses

DespesesP$Projecte <- as.character(DespesesP$Projecte)
DespesesP$Mes = (as.numeric(format(DespesesP$Data,"%m"))-1) * (1/12) + as.numeric(format(DespesesP$Data,"%Y"))#Distriuim els mesos equitativament

DespesesXMercat <- DespesesP %>%
    filter(Data >= "2017-01-01") %>%
    filter((Projecte != "FIRA FIPA17") & (Projecte != "CENTRE_BULGARIA") & (Projecte != "PROJMARKETING1")) %>%
    left_join(ProjectesXMercat, by = "Projecte") %>%
    group_by(Mes, Mercat) %>%
    summarize(DespesaTotal = sum(Import), DespesaHores = sum(ImportHores), DespesaInversio = sum(ImportInversio))
    
g = ggplot(DespesesXMercat, aes(x = Mes , y= DespesaTotal, fill = Mercat)) + 	
    geom_area(colour="black", size=.2, alpha=.4) +
    ggtitle("Despesa projectes per Mercat") + 
    theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
    theme(axis.text = element_text(size = 8)) + 
    labs(y = "Despesa", x = "Temps") 
print(g)
ggsave("Grafiques/DespesesXMercat.pdf", width = 18, height = 18, units = "cm")

#-------------------------GRAFICA GENERAL DE PROJECTES--------------------------
#Acumulem a cada dia la facturació i les despeses per cada projecte
DiariGlobalSumat <- DiariGlobal %>%
    group_by(Data, Projecte) %>%
    summarize(TotalMarge = sum(Marge))
DiariGlobalSumat$Despesa = 0
DiariGlobalSumat$Projecte <- as.character(DiariGlobalSumat$Projecte)

DespesesSumat <- Despeses %>%
    group_by(Data, Projecte) %>%
    summarize(Despesa = sum(Import))
DespesesSumat$TotalMarge = 0
DespesesSumat$Projecte <- as.character(DespesesSumat$Projecte)

#Ajuntem Diari, Despeses i queixes en un tibble
ResumGlobal <- bind_rows(DespesesSumat, DiariGlobalSumat)
ResumGlobal$Suma = ResumGlobal$TotalMarge - ResumGlobal$Despesa

#Traiem els que no són projectes
ResumGlobal<- ResumGlobal %>%
    filter(!str_detect(Projecte, "^_E")) %>%
    group_by(Data, Projecte) %>%
    summarize(Total = sum(Suma)) %>%
    arrange(Data)

ResumGlobal$Projecte <- factor(ResumGlobal$Projecte)

ResumGlobal$Acumulat <- 0

for(i in 1:(length(levels(ResumGlobal$Projecte)))){
    ResumGlobal$Acumulat[which(ResumGlobal$Projecte == levels(ResumGlobal$Projecte)[i])] = cumsum (ResumGlobal$Total[which(ResumGlobal$Projecte == levels(ResumGlobal$Projecte)[i])])
}

Proj = levels(ResumGlobal$Projecte)[40:50]

ResumGlobal2 <- ResumGlobal %>%
    filter(Projecte %in% Proj)

g = ggplot(ResumGlobal2) +
    geom_line(aes(x=Data, y=Acumulat, color=Projecte, group=Projecte)) + 
    ylab("Margin - Invest(€)") +  
    ggtitle("Projects cashflow") +
    theme(plot.title = element_text(colour = "Blue", family = "Times", lineheight = .9, face = "bold.italic", hjust=0.5)) +
    theme(axis.text = element_text(size = 8)) 
print(g)

ggsave("Grafiques/TotalProjectes.pdf", width = 18, height = 18, units = "cm")
