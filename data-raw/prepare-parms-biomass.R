## code to prepare `parms.biomass` dataset goes here



library(data.table)

lambert_D <-  readxl::read_xls("P:/Jing 2/Pare/Reftables/Lambert 2005.xls", sheet= 1, skip = 4, na = "")
lambert_DH <-  readxl::read_xls("P:/Jing 2/Pare/Reftables/Lambert 2005.xls", sheet= 2, skip = 4, na = "")

ung_D <-  readxl::read_xls("P:/Jing 2/Pare/Reftables/Ung 2008.xls", sheet= 1, skip = 4, na = "")
ung_DH <-  readxl::read_xls("P:/Jing 2/Pare/Reftables/Ung 2008.xls", sheet= 2, skip = 4, na = "---")
# setDT(ung_D)[Species == "Red alder and", Species:=paste0(Species, " Black cottonwood" )]
# ung_D[Species == "Black cottonwood", Species:=NA_character_]

setdiff(unique(ung_D$Species), unique(lambert_D$Species))


df.list <- list(lambert2005_D = lambert_D,lambert2005_DH = lambert_DH, ung2008_D = ung_D, ung2008_DH = ung_DH)
res <- lapply( seq_along(df.list), function(i) {
  x <- setDT(df.list[[i]])
  x$label <- names(df.list)[i]

  if (tolower(substr(names(df.list)[i], 1,3)) == "ung"){
    x[Species %in% c("Red alder and", "Red alder"), Species:=paste0("Red alder and Black cottonwood" )]
    x[Species == "Black cottonwood", Species:=NA_character_]
    x[Species == "All hardwoods", Species:= "Hardwood"]
    x[Species == "All softwoods", Species:= "Softwood"]
    x[Species == "All species", Species:= "All"]
  }
  x[Species == "Western redcedar", Species:= "Western red cedar"]
  x[Species == "Grey birch", Species:= "Gray birch"]
  x$Species <- zoo::na.locf(x$Species)
  setnafill(x,  cols=c("Estimate"), fill = 0)
  x <- dcast(x, label + Species ~ Parameter, value.var = "Estimate")
  if (tolower(substr(names(df.list)[i], 1,3)) == "ung"){
    x.bkcotton <- x[Species == "Red alder and Black cottonwood",]
    x.bkcotton[, Species := "Black cottonwood"]
    x[Species == "Red alder and Black cottonwood", Species := "Red alder"]
    x <- rbind(x, x.bkcotton)
    rm(x.bkcotton)
  }
  x
})

parms.all <- rbindlist(res, fill = T)
parms.all[, c("model.year", "model.DorH") := tstrsplit(label, "_", fixed=TRUE)]

setdiff(unique(parms.all[label == "ung2008_D"]$Species), unique(parms.all[label == "lambert2005_D"]$Species))
intersect(unique(parms.all[label == "ung2008_D"]$Species), unique(parms.all[label == "lambert2005_D"]$Species))
setorder(parms.all, model.DorH, Species, model.year)
parms.all[, nameU:=toupper(Species)]
spc.all <- parms.all[, .N, by = .(Species, nameU)]
setnames(spc.all, "Species", "ENGLISH_NAME")
spc.test <- merge(Species.List, spc.all, by = "ENGLISH_NAME", all.y = T)


spc.ref <- readxl::read_xls('P:/Jing/2006-04 ---/_Project/Huor/GY age/Ref tables/intolerant species Ung.xls')




library(stringr)
spc.ms <- spc.test[is.na(species_code)]
spc.ms[, .N, by = .(ENGLISH_NAME)]
spc.ms[,nameU:=toupper(ENGLISH_NAME)]
spc.ms[, .N, by = .(nameU)]

spc.mc <- readxl::read_xls('U:/Centre_Fibre/Jing/MCLambert/Lecteur_C/MC Lambert/MCLambert archieve/autres/ENFOR_Canada/autres/biomasse/tableaux rapport.xls',
                           range ="P3:S36" , col_names = F)



names(spc.mc) <- c("CANFI_CODE",  "FRENCH_NAME",  "SCIENTIFIC_NAME" ,  "ENGLISH_NAME" )
setDT(spc.mc)[ENGLISH_NAME == "Grey Birch",]
setDT(spc.mc)[ENGLISH_NAME == "Grey Birch", ENGLISH_NAME := "Gray Birch"]
spc.mc[,nameU:=toupper(ENGLISH_NAME)]



xc <- matrix(c("Black cottonwood", " peuplier de l’Ouest", " Populus trichocarpa Torr. & A. Gray",
               "black spruce"," épinette noire", " Picea mariana (Mill.) BSP" ,
               "Douglas-fir"," Douglas vert"," Pseudotsuga menziesii (Mirb.) Franco",
               "Engelmann spruce"," épinette d’Engelmann"," Picea engelmannii Parry ex. Engelm.",
               "lodgepole pine"," pin tordu latifolié"," Pinus contorta Dougl. ex Loud. var. latifolia Engelm.",
               "Pacific silver fir"," sapin gracieux"," Abies amabilis (Dougl. ex Loud.) Dougl. ex J. Forbes",
               "red alder"," aulne rouge"," Alnus rubra Bong.",
               "Sitka spruce"," épinette de Sitka"," Picea sitchensis (Bong.) Carrière",
               "subalpine fir"," sapin subalpin"," Abies lasiocarpa (Hook.) Nutt.",
               "trembling aspen"," peuplier faux-tremble"," Populus tremuloides Michx.",
               "western hemlock"," pruche de l’Ouest"," Tsuga heterophylla (Raf.) Sarg.",
               "western red cedar"," thuya géant"," Thuja plicata Donn ex D. Don",
               "white birch"," bouleau à papier"," Betula papyrifera Marsh.",
               "white spruce"," épinette blanche"," Picea glauca (Moench) Voss."),
             ncol = 3, byrow = T)
spc.ung <- as.data.frame(xc)
names(spc.ung) <- c("ENGLISH_NAME", "FRENCH_NAME",  "SCIENTIFIC_NAME" )
setDT(spc.ung)[, nameU:=toupper(ENGLISH_NAME)]

spc.ms.ung <- merge(spc.ms[,c("nameU")], spc.ung, by = "nameU", all.x = T)[,source:= "ung"]
spc.ms.mc <- merge(spc.ms[,c("nameU")], spc.mc, by = "nameU", all.x = T)[,source:= "lambert"]
# name.d <-intersect(spc.ms.mc[!is.na(FRENCH_NAME)]$nameU, spc.ms.ung[!is.na(FRENCH_NAME)]$nameU)
# setdiff(spc.ms.mc[!is.na(FRENCH_NAME)]$nameU, spc.ms.ung[!is.na(FRENCH_NAME)]$nameU)

# spc.ms.ung <- spc.ms.ung[!(nameU %in% name.d)]
spc.ms.corr <- rbind(spc.ms.ung[!is.na(FRENCH_NAME)], spc.ms.mc[!is.na(FRENCH_NAME)], fill = T)
setorder(spc.ms.corr, nameU)
spc.ms.corr[, .N, by = .(nameU)][N>1]
# take Lambert name version

names(spc.ref)
spc.ms.corr[, .N, by = .(nameU)][N>1]

setdiff(spc.ms$nameU, spc.ms.corr$nameU)
spc.ref2 <- setDT(spc.ref)[, .N, by = c("ENGLISH_NAME", "FRENCH_NAME", "SCIENTIFIC_NAME", "NFI_CODE")]
spc.ref2[,nameU:=toupper(ENGLISH_NAME)]

spc.code <- merge(spc.ms.corr, spc.ref2[ ,c("nameU", "NFI_CODE")], by = "nameU", all.x = T)
spc.code[, .N, by = .(nameU)][N>1]



# chk.a <- as.data.frame(sapply(spc.ms.corr$nameU, grepl, toupper(spc.ref2$nameU)))
to.search <- spc.ms.corr$nameU

find.all <- data.table()
for (i in seq_along(to.search)){
  tmp <- spc.ref2[grep(to.search[i], spc.ref2$nameU)][,name.pub := to.search[i]]
  find.all <- rbind(find.all, tmp)
}

find.all[, nameU:=name.pub][,name.pub:=NULL]
spc.ms.corr <- spc.ms.corr[find.all[,c("nameU", "NFI_CODE")], on = .(nameU)]

spc.ms.corr[, species_code:= paste0(substr(NFI_CODE, 1,4),substr(NFI_CODE, 6,8))]
cols.sel <- c("species_code", "ENGLISH_NAME",  "FRENCH_NAME",     "SCIENTIFIC_NAME" , "nameU"      )
spc.all.code <- rbind(spc.test[!is.na(species_code), cols.sel, with = F][,source:="species.lst"], spc.ms.corr[,cols.sel, with = F][,source:="ref file"])

spc.all.code[, .N, by = .(ENGLISH_NAME)][N>1]
spc.all.code[, .N, by = .(species_code)][N>1]
setorder(spc.all.code, species_code)

spc.all.test <- merge(spc.all, spc.all.code[, -"ENGLISH_NAME"], by = "nameU", all = T)
spc.all.test[is.na(species_code)]
spc.all.test[is.na(species_code), species_code := nameU]

names(spc.all.test)
spc.all.test[ENGLISH_NAME.x !=ENGLISH_NAME.y]
names(spc.all)
names(spc.all.code)
spc.all

names(spc.all.test)
species_list_biomass <- spc.all.test[, c("species_code",  "ENGLISH_NAME",  "FRENCH_NAME", "SCIENTIFIC_NAME")]

species_list_biomass[species_code== "ABIELAS"]
species_list_biomass[species_code== "ABIELAS" & ENGLISH_NAME == "Subalpine fir", ENGLISH_NAME := "Alpine/Subalpine fir" ]
species_list_biomass <- species_list_biomass[!(species_code== "ABIELAS" & ENGLISH_NAME == "Alpine fir") ]

setorder(species_list_biomass, species_code)
species_list_biomass<- species.list.biomass
parms.all.test <- merge(parms.all, spc.all.test[, c("ENGLISH_NAME", "species_code")], by.x = "Species", by.y = "ENGLISH_NAME", all = T)
parms.all.test[is.na(species_code)]

names(parms.all.test)
spc.rep <- intersect(parms.all.test[model.year == "lambert2005"]$species_code, parms.all.test[model.year == "ung2008"]$species_code)
parms.all.test[species_code %in% spc.rep][, .N, by = .(species_code)]
parms.all.test[species_code %in% spc.rep, model.ver:= toupper(substr(model.year,1,1))]

parms.all.test[!(species_code %in% spc.rep)][, .N, by = .(species_code)][N!= 2]
parms.all.test[!(species_code %in% spc.rep)][, .N, by = .(species_code, model.year)]
parms.all.test[!(species_code %in% spc.rep), model.ver :="LU"]
parms.all.test[, .N, by = .(species_code, model.ver, model.DorH)][N>1]
parms.all.test[str_detect(model.ver, "L"),][, .N, by = .(species_code)][N!=2]
parms.all.test[str_detect(model.ver, "L"),][,.N]
parms.all.test[str_detect(model.ver, "U"),][, .N, by = .(species_code)][,.N]
parms.all.test[, .N, by = .(species_code)][,.N]


setDF(parms.all.test)
parms.all.test[is.na(parms.all.test)] <- 0
setDT(parms.all.test)


setdiff(spc.all.test$nameU, unique(parms.all.test$nameU))

setdiff(unique(parms.all.test$nameU),spc.all.test$nameU)


spc.all.test[, .N, by = .(nameU)][N>1]
names(parms.all.test)
setcolorder(parms.all.test, c("species_code", "label", "model.ver", "model.DorH"))
parms_biomass <- parms.all.test[, -c("Species", "model.year", "nameU")]
setcolorder(parms_biomass, c("species_code", "label", "model.ver", "model.DorH", "bbark1",  "bbark2",  "bbark3",
                             "bbranches1",   "bbranches2" ,  "bbranches3", "bfoliage1", "bfoliage2",  "bfoliage3",
                             "bwood1", "bwood2", "bwood3" ))

setnames(parms_biomass, "model.DorH", "model.DorDH")
usethis::use_data(parms_biomass, overwrite = TRUE)
usethis::use_data(species_list_biomass, overwrite = TRUE)
names(spc.ref)

# add MRNQ_CODE in species list
str(spc.ref)
data(species_list_biomass)
setDT(spc.ref)[, species_code:=paste0(substr(NFI_CODE,1,4), substr(NFI_CODE,6,8))]
spc.ref[!is.na((MRNQ_CODE)), .N, by = .(species_code)][N>1]
species_list_biomass<- merge(species_list_biomass, spc.ref[!is.na(MRNQ_CODE),c("species_code", "MRNQ_CODE")], by = "species_code", all.x = T)
usethis::use_data(species_list_biomass, overwrite = TRUE)
