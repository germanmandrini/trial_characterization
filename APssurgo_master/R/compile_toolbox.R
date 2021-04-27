source("R/latlong2county.R")


compile_toolbox <- function(data_soils,badge_name,crops) {

  # <folder version="37" creator="Apsim 7.9-r4044" name="Soils">
  folder <- newXMLNode("folder", attrs = list(version="DIFM",
                                              creator="Apsim 7.10-r4171",
                                              name= badge_name))

  counter_n = 1
  for(site_n in unique(data_soils$SiteName)){
    # i=1
    # site_n = unique(data_soils$SiteName)[1]

    site <- newXMLNode("folder", attrs = list(version="36",
                                                creator="Apsim 7.6-r3376",
                                                name=site_n), parent = folder)
    data_site <- data_soils[SiteName == site_n]

    if(is.null(data_site)) next

    #for(n in 1:length(data_site)){
      # n=1

      print(paste0(site_n,": ",
                   round(mean(data_site$area_pct),2)*100,
                   "% of AOI"))

      ## <Soil name="Default">
      Soil <- newXMLNode("Soil", attrs = list(name=paste0(site_n,"-",round(mean(data_site$area_pct),2)*100,"% of AOI")),
                         parent = site)

      ### <RecordNumber>0</RecordNumber>
      RecordNumber <- newXMLNode("RecordNumber",parent = Soil)
      xmlValue(RecordNumber) <-  counter_n #i

      counter_n = counter_n + 1

      ### <SoilType>Nicollet</SoilType>
      SoilType <- newXMLNode("SoilType",parent = Soil)
      xmlValue(SoilType) <- site_n#names(data_site)[1]

      ### <Region>Story</Region>
      coords <- data.table(lat = mean(data_site$Y),
                           long = mean(data_site$X))

      county <- capitalize(latlong2county(lat=coords$lat, long=coords$long))
      Region <- newXMLNode("Region",parent = Soil)
      xmlValue(Region) <- county[2]

      ### <State>Iowa</State>
      State <- newXMLNode("State",parent = Soil)
      xmlValue(State) <- county[1]

      ### <Country>US</Country>
      Country <- newXMLNode("Country",parent = Soil)
      xmlValue(Country) <- "USA"

      ### <ApsoilNumber>1</ApsoilNumber>
      ApsoilNumber <- newXMLNode("ApsoilNumber",parent = Soil)
      xmlValue(ApsoilNumber) <- 1

      ### <Latitude>0</Latitude>
      Latitude <- newXMLNode("Latitude",parent = Soil)
      xmlValue(Latitude) <- round(coords$lat,2)

      ### <Longitude>-0</Longitude>
      Longitude <- newXMLNode("Longitude",parent = Soil)
      xmlValue(Longitude) <- round(coords$long,2)

      ### <YearOfSampling>0</YearOfSampling>
      YearOfSampling <- newXMLNode("YearOfSampling",parent = Soil)
      xmlValue(YearOfSampling) <- as.character(year(Sys.Date()))

      ### <DataSource>ssurgo2apsim</DataSource>
      DataSource <- newXMLNode("DataSource",parent = Soil)
      xmlValue(DataSource) <- "ssurgo2apsim"

      ### <Comments></Comments>
      Comments <- newXMLNode("Comments",parent = Soil)
      xmlValue(Comments) <- paste0("This soil was created with data from SSURGO Database, downloaded in R via the FedData package and parameters set using the approach described by Archontoulis et al. (2014, Agron. J. 106(3):1025-1040). This soil type represents ",
                                   round(mean(data_site$area_pct),2)*100,"% of AOI.")
      ### <Water>
      Water <- newXMLNode("Water",parent = Soil)

      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_site$thick[j]
      }

      #### <BD>
      BD <- newXMLNode("BD",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = BD)
        xmlValue(double) <- round(data_site$bd[j],2)
      }

      #### <AirDry>
      AirDry <- newXMLNode("AirDry",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = AirDry)
        xmlValue(double) <- round(data_site$AirDry[j],2)
      }

      #### <LL15>
      LL15 <- newXMLNode("LL15",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = LL15)
        xmlValue(double) <- round(data_site$ll[j],2)
      }

      #### <DUL>
      DUL <- newXMLNode("DUL",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = DUL)
        xmlValue(double) <- round(data_site$dul[j],2)
      }

      #### <SAT>
      SAT <- newXMLNode("SAT",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = SAT)
        xmlValue(double) <- round(data_site$sat[j],2)
      }

      #### <KS>
      KS <- newXMLNode("KS",parent = Water)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = KS)
        xmlValue(double) <- round(data_site$ksat[j],2)
      }

      #### <SoilCrop>
      for(k in 1:length(crops)){
        SoilCrop <- newXMLNode("SoilCrop", attrs = list(name = crops[k]),parent = Water)

        ##### <Thickness>
        Thickness <- newXMLNode("Thickness",parent = SoilCrop)
        for(j in 1:length(data_site$thick)){
          double <- newXMLNode("double",parent = Thickness)
          xmlValue(double) <- data_site$thick[j]
        }

        ##### <LL>
        LL <- newXMLNode("LL",parent = SoilCrop)
        for(j in 1:length(data_site$thick)){
          double <- newXMLNode("double",parent = LL)
          xmlValue(double) <- round(data_site$ll[j],2)
        }

        ##### <KL>
        KL <- newXMLNode("KL",parent = SoilCrop)
        for(j in 1:length(data_site$thick)){
          double <- newXMLNode("double",parent = KL)
          xmlValue(double) <- round(data_site$KL_maize[j],3)
        }

        ##### <XF>
        XF <- newXMLNode("XF",parent = SoilCrop)
        for(j in 1:length(data_site$thick)){
          double <- newXMLNode("double",parent = XF)
          xmlValue(double) <- round(data_site$XF_maize[j],3)
        }
      }

      ### <SoilWater>
      SoilWater <- newXMLNode("SoilWater",parent = Soil)

      #### <SummerCona>
      SummerCona <- newXMLNode("SummerCona",parent = SoilWater)
      xmlValue(SummerCona) <-(round(data_site$cona,2)[1])

      #### <SummerU>
      SummerU <- newXMLNode("SummerU",parent = SoilWater)
      xmlValue(SummerU) <- (round(data_site$U,2)[1])

      #### <SummerDate>
      SummerDate <- newXMLNode("SummerDate",parent = SoilWater)
      xmlValue(SummerDate) <- "1-jun"

      #### <WinterCona>
      WinterCona <- newXMLNode("WinterCona",parent = SoilWater)
      xmlValue(WinterCona) <-as.character(round(data_site$cona,2)[1])

      #### <WinterU>
      WinterU <- newXMLNode("WinterU",parent = SoilWater)
      xmlValue(WinterU) <- as.character(round(data_site$U,2)[1])

      #### <WinterDate>
      WinterDate <- newXMLNode("WinterDate",parent = SoilWater)
      xmlValue(WinterDate) <- "1-nov"

      #### <DiffusConst>
      DiffusConst <- newXMLNode("DiffusConst",parent = SoilWater)
      xmlValue(DiffusConst) <-  unique(data_site$DiffusConst)

      #### <DiffusSlope>
      DiffusSlope <- newXMLNode("DiffusSlope",parent = SoilWater)
      xmlValue(DiffusSlope) <-  unique(data_site$DiffusSlope)

      #### <Salb>
      Salb <- newXMLNode("Salb",parent = SoilWater)
      xmlValue(Salb) <- unique(data_site$Salb)

      #### <CN2Bare>
      CN2Bare <- newXMLNode("CN2Bare",parent = SoilWater)
      xmlValue(CN2Bare) <- (round(unique(data_site$CN2),0))

      #### <CNRed>
      CNRed <- newXMLNode("CNRed",parent = SoilWater)
      xmlValue(CNRed) <- (unique(data_site$CNRed))

      #### <CNCov>
      CNCov <- newXMLNode("CNCov",parent = SoilWater)
      xmlValue(CNCov) <- (unique(data_site$CNCov))

      #### <Slope>NaN</Slope>
      #### <DischargeWidth>NaN</DischargeWidth>
      #### <CatchmentArea>NaN</CatchmentArea>
      #### <MaxPond>NaN</MaxPond>

      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = SoilWater)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_site$thick[j]
      }

      #### <SWCON>
      SWCON <- newXMLNode("SWCON",parent = SoilWater)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = SWCON)
        xmlValue(double) <- round(data_site$SWCON[j],3)
      }


      ### SoilOrganicMatter
      SoilOrganicMatter <- newXMLNode("SoilOrganicMatter",parent = Soil)

      #### <RootCN>
      RootCN <- newXMLNode("RootCN",parent = SoilOrganicMatter)
      xmlValue(RootCN) <- unique(round(data_site$RootCN))

      #### <RootWt>
      RootWt <- newXMLNode("RootWt",parent = SoilOrganicMatter)
      xmlValue(RootWt) <- unique(round(data_site$RootWt))

      #### <SoilCN>13</SoilCN>
      SoilCN <- newXMLNode("SoilCN",parent = SoilOrganicMatter)
      xmlValue(SoilCN) <- unique(round(data_site$SoilCN))

      #### <EnrACoeff>
      EnrACoeff <- newXMLNode("EnrACoeff",parent = SoilOrganicMatter)
      xmlValue(EnrACoeff) <- unique(round(data_site$EnrAcoeff,2))

      #### <EnrBCoeff>
      EnrBCoeff <- newXMLNode("EnrBCoeff",parent = SoilOrganicMatter)
      xmlValue(EnrBCoeff) <- unique(round(data_site$EnrBcoeff,2))

      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = SoilOrganicMatter)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_site$thick[j]
      }

      #### <OC>
      OC <- newXMLNode("OC",parent = SoilOrganicMatter)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = OC)
        xmlValue(double) <- round(data_site$OC[j],2)
      }

      #### <FBiom>
      FBiom <- newXMLNode("FBiom",parent = SoilOrganicMatter)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = FBiom)
        xmlValue(double) <- round(data_site$FBiom[j],4)
      }

      #### <FInert>
      FInert <- newXMLNode("FInert",parent = SoilOrganicMatter)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = FInert)
        xmlValue(double) <- round(data_site$FInert[j],4)
      }

      #### <OCUnits>
      OCUnits <- newXMLNode("OCUnits",parent = SoilOrganicMatter)
      xmlValue(OCUnits) <- "Total"

      ### <Analysis>
      Analysis <- newXMLNode("Analysis",parent = Soil)

      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = Analysis)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_site$thick[j]
      }

      #### <PH>
      PH <- newXMLNode("PH",parent = Analysis)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = PH)
        xmlValue(double) <- round(data_site$ph[j],2)
      }

      #### <ParticleSizeSand>
      ParticleSizeSand <- newXMLNode("ParticleSizeSand",parent = Analysis)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = ParticleSizeSand)
        xmlValue(double) <- round(data_site$sand[j],1)
      }

      #### <ParticleSizeClay>
      ParticleSizeClay <- newXMLNode("ParticleSizeClay",parent = Analysis)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = ParticleSizeClay)
        xmlValue(double) <- round(data_site$clay[j],1)
      }

      #### <ParticleSizeSilt>
      ParticleSizeSilt <- newXMLNode("ParticleSizeSilt",parent = Analysis)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = ParticleSizeSilt)
        xmlValue(double) <- round(100 - data_site$sand[j] - data_site$clay[j],1)
      }

      ### Sample
      Sample <- newXMLNode("Sample",parent = Soil, attrs = list(name="Intial conditions"))

      #### <Thickness>
      Thickness <- newXMLNode("Thickness",parent = Sample)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = Thickness)
        xmlValue(double) <- data_site$thick[j]
      }

      #### <NO3>
      NO3 <- newXMLNode("NO3",parent = Sample)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = NO3)
        xmlValue(double) <- round(data_site$no3ppm[j],2) # same as OC but in ppm
      }

      #### <NH4>
      NH4 <- newXMLNode("NH4",parent = Sample)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = NH4)
        xmlValue(double) <- round(data_site$nh4ppm[j],2) # same as 1/2 oc but ppm
      }

      #### <SW>
      SW <- newXMLNode("SW",parent = Sample)
      for(j in 1:length(data_site$thick)){
        double <- newXMLNode("double",parent = SW)
        xmlValue(double) <- round(data_site$sw[j],2)
      }

      #### <NO3Units>
      NO3Units <- newXMLNode("NO3Units",parent = Sample)
      xmlValue(NO3Units) <- "ppm"

      #### <NH4Units>
      NH4Units <- newXMLNode("NH4Units",parent = Sample)
      xmlValue(NH4Units) <- "ppm"

      #### <SWUnits>
      SWUnits <- newXMLNode("SWUnits",parent = Sample)
      xmlValue(SWUnits) <- "Volumetric"

    }#end of data_site loop


  return(folder)

}
