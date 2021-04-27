source("R/SaxtonRawls.R")


test = FALSE
if(test){
  horizon <- x  
  by_soil = TRUE
  soilLayer_breaks = c(20,40,60,150,200)
}

calcSSURGO <- function(x, by_soil = TRUE, soilLayer_breaks = c(20,40,60,150,200)){

  soils <- vector("list", length(unique(h$name)))

    # Calculate new variables -------------------------------------------------------------------

    # Soil physical properties

    horizon$bd <- ifelse( horizon$wetbd < 0.9, 0.9, ifelse(horizon$wetbd > 1.8, 1.8,horizon$wetbd))
    #horizon$bd <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$BD
    #horizon$ll.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$LL15
    #horizon$dul.sr <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$DUL

    horizon$ksat <- pmin(horizon$ksat*100/1.157,SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$KSAT*24) # mm/day

    #horizon$ksat <- ifelse(horizon$ksat > 500, 500, horizon$ksat) # mm/day

    horizon$sat <- SaxtonRawls(pSand=horizon$sand ,pClay=horizon$clay, pOM=horizon$om)$SAT/100

    horizon$PO <- 1-horizon$bd/2.65

    horizon$Salb <- 0.15 # Bare soil albedo

    horizon$MWCON <- 1 #(0-1)

    horizon$dul <- horizon$dul/100

    horizon$ll <- horizon$ll/100

    horizon$SWCON <- (horizon$PO-horizon$dul)/horizon$PO

    horizon$AirDry <- ifelse(horizon$center<=15,0.9,ifelse(horizon$center<=30,0.95,1))*horizon$ll

    horizon$U <- ifelse(horizon$clay<=20,5+0.175*horizon$clay,
                        ifelse(horizon$clay<=40,7.5+0.05*horizon$clay,
                               ifelse(horizon$clay<=50,11.5-0.05*horizon$clay,
                                      ifelse(horizon$clay<=70,12.75-0.075*horizon$clay,
                                             ifelse(horizon$clay<=80,11-0.05*horizon$clay,0))))) # mm

    horizon$cona <- ifelse(horizon$clay<=30,0.025*horizon$clay+3.25,
                           ifelse(horizon$clay<=50,4,
                                  ifelse(horizon$clay<=70,-0.025*horizon$clay+5.25,
                                         ifelse(horizon$clay<=80,3.5,0)))) # mm/d^5

    horizon$DiffusConst <- 40

    horizon$DiffusSlope <- 16

    horizon$CN2 <- ifelse(is.na(horizon$CN2),80,horizon$CN2)

    horizon$CNRed <- 20 #residue runoff

    horizon$CNCov	 <- 0.8

    horizon$EnrAcoeff	<- 7.4

    horizon$EnrBcoeff	<- 0.2

    horizon$XF_maize <- ifelse(horizon$center<=150,1,0)

    horizon$KL_maize <-	ifelse(horizon$center<=20,0.08,0.09*exp(-0.007*horizon$center))

    horizon$e	<- 0.5  #ifelse(F4=$BC$3,0.07,IF(F4=$BC$4,0.03,0.05))

    # Soil chemical properties

    horizon$ph <- 0.52+1.06*horizon$ph #pH 1:5

    horizon$OC <- horizon$om/1.72 # %

    horizon$OC <- c(horizon$OC[1],
                    ifelse(horizon$center[-1] >= 100 & diff(horizon$OC) == 0,
                           horizon$OC[1]*exp(horizon$center[-1]*-0.035),
                           horizon$OC)) # exponential decay below 100 cm if data is missing


    horizon$FInert <- ifelse(horizon$center<=1,0.4,
                             ifelse(horizon$center<=10,0.4,
                                    ifelse(horizon$center<60,0.008*horizon$center+0.32,
                                           ifelse(horizon$center<=120,0.8,
                                                  ifelse(horizon$center<180,0.0032*horizon$center+0.42,
                                                         ifelse(horizon$center<=300,0.99,0)))))) #(0-1)

    horizon$FBiom <- ifelse(horizon$center<=10,0.04,
                            ifelse(horizon$center<=20,0.055-0.0015*horizon$center,
                                   ifelse(horizon$center<=30,0.03-0.0005*horizon$center,
                                          ifelse(horizon$center<60,0.0216-0.0002*horizon$center,
                                                 ifelse(horizon$center<=300,0.01,0))))) #(0-1)

    horizon$RootCN <- 45

    horizon$SoilCN <- 13

    horizon$RootWt <- 1000

    horizon$sw <- horizon$dul

    horizon$no3ppm <- horizon$OC

    horizon$nh4ppm <- horizon$OC*0.5
    
    # Bin by layer
    horizon[,layer := .bincode(center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))]
    
    horizon_grouped <- horizon[,-c('hydrogroup')] %>% .[, lapply(.SD,mean), by=.(name, layer)]
    
    average_soil <- horizon_grouped[,-'name'] %>% .[, lapply(.SD, weighted.mean, w = area_pct), by=.(layer)]
    
    weighted.mean(x = c(20,30),
                  w = c(0.42,0.2))
    
    soilLayer_breaks_tmp <- c(soilLayer_breaks[soilLayer_breaks < unique(average_soil$restriction)], unique(average_soil$restriction))
    
    
    correction_dt <- data.table(layer = sort(unique(average_soil$layer)),
                                top = c(0, soilLayer_breaks_tmp[-length(soilLayer_breaks_tmp)]),
                                bottom = soilLayer_breaks_tmp)
               
    average_soil <- merge(average_soil[,-c('top', 'bottom', 'thick', 'center')],
                              correction_dt, by = 'layer') %>% .[order(layer)]
    
    average_soil[,thick := bottom-top]
    average_soil[,center := top+thick/2]
    
    
    
    for(i in 1:length(unique(h$name))){

    #Consider restrictions
    # horizon[,restriction := NA]
    # horizon[,restriction := 158]
    are_restrictions <- !is.na(unique(horizon[,restriction]))
    if(are_restrictions){
      print("Soil has restrictions")
      horizon <- horizon[center <= restriction]
      soilLayer_breaks_tmp <- c(soilLayer_breaks[soilLayer_breaks < max(horizon[,restriction])], max(horizon[,restriction]))
     }else{
      soilLayer_breaks_tmp <- soilLayer_breaks
     }
    
    

    #horizon$layer <- .bincode(horizon$center, breaks=c(-1,soilLayer_breaks[soilLayer_breaks>0]))

    


    # Save df into list ###########

    soils[[i]] <- list(area = unique(horizon$area_pct),
                       horizon = horizon_grouped[, -c('top', 'bottom','center','slope_code','slope')])

  } #end name loop

  names(soils) <- (unique(h$name))

  ### Return average soil

  if(by_soil) {

    soils <- list(Average_Soil =
                    list(area = 1,
                         horizon = do.call(rbind, sapply(soils, rbind)[2,])%>%
                           group_by(layer) %>%
                           summarise_all(weighted.mean,unlist(sapply(soils, rbind)[1,]))  %>%
                           mutate(thick = round(thick)) %>%
                           as.data.frame()))
    
    sum(soils$Average_Soil$horizon$thick)
  }

  return(soils)

}

#calcSSURGO(x2,by_soil = TRUE)
