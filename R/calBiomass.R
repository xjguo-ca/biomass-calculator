#' above ground biomass calculation

#' @description above ground biomass calculation using Ung2008 and Lambert2005

#' @details For both Ung2008 and Lambert2005, We have developed D-model and DH-model depending on the availability of total height.
#' If species_code was missing or doesn't exist in the parameter sets of the models, use one of the species group ("ALL", "SOFTWOOD", "HARDWOOD") for the approx. estimation.
#'

#' @param data a data frame that includes at minimum the columns: species_code, dbh_cm, Htot_m(if using DH-model). unit of dbh_cm in centimeter and Htot_m in meter to have the estimated biomass in kg.
#' @param model.Ver parameter sets from 2 publications, values: "Ung2008" (default), "Lambert2005"
#' @param model.D logi, T: D-model(default), F: DH-model
#'
#' @returns Above ground biomass (in Kg) of 4 compartments (wood, bark, branches, foliage) and tree biomass
#' as the sum of the 4 compartments.
#'
#' @import stringr
#' @import data.table
#'

#' @export
#'
#' @examples
#' sample_data <- data.table::data.table(
#'   species_code = c("PICEMAR","PICEGLA"),
#'   treeID = c(1,2),
#'   dbh_cm = c(30,30),
#'   Htot_m = c(18,18)
#' )
#'
#' bm <- calbiomass(data = sample_data, model.Ver = "Ung2008", model.D = TRUE)

#'
#' @author Xiao Jing Guo \email{xiaojing.guo@NRCan-RNCan.gc.ca}
#' @references Lambert, M.-C.; Ung, C.-H.; Raulier, F. 2005. Canadian national tree aboveground biomass equations. Can. J. For. Res. 35:1996-2018.
#' @references Ung, C.-H.; Bernier, P.; Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.

calbiomass <- function(data , model.Ver = "Ung2008", model.D = TRUE){
  BM_Tree_kg <- BM_bark_kg <- BM_branch_kg <- BM_fol_kg <- BM_wood_kg <- N <- NULL
  # parameter set selection
  if (!(tolower(model.Ver) %in% c("ung2008", "lambert2005"))) stop("please check model.ver,it can only take one of the 2 values: 'Ung2008' or 'Lambert2005'")
  parms_biomass <- data.table::data.table(parms_biomass)
  parms.sel <- parms_biomass[str_detect(model.ver,toupper(substr(model.Ver,1,1)))][str_detect(model.DorDH, "H") != model.D]
  if (nrow(parms.sel) == 0 | nrow(parms.sel[, .N, by = .(species_code)][N!=1]) >0) stop("check the input model.Ver and/or model.D")

  # check species_code
  spc.na1 <- setdiff(data$species_code, parms.sel$species_code)
  spc.na2 <- nrow(data[species_code == "" | is.na(species_code)])
  if (length(spc.na1) > 0) stop(paste0("no parameters for '",  spc.na1 , "', please verify species_code or use 'ALL'/'SOFTWOOD'/'HARDWOOD' for the approx. estimation instead"))
  if (spc.na2 >0) warnings("missing species_code, biomass won't be estimated")
  # check dbh
  if (!("dbh_cm" %in% names(data))) stop("dbh_cm is missing, please verify input data...")
  if (!is.numeric(data$dbh_cm)) stop("dbh_cm must be numeric, please verify input data...")
  if ( nrow(data[dbh_cm <= 0 | is.na(dbh_cm)]) > 0 ) stop("dbh_cm must have values and positive")
  if (model.D==T) {
    while ("Htot_m" %in% names(data)) {

      H.tmp <- paste0("Htot_m",sample.int(100,1))
      if (!(H.tmp %in% names(data))) setnames(data, "Htot_m", H.tmp)
    }
    data[,Htot_m:= 1]

  } else{
    # check Htot
    if (!("Htot_m" %in% names(data))) stop("Htot_m is missing, please verify input data...")
    if (!is.numeric(data$Htot_m)) stop("Htot_m must be numeric, please verify input data...")
    if ( nrow(data[Htot_m <= 0 | is.na(Htot_m)]) > 0 ) stop("Htot_m must have values and positive")
  }
  mass <- merge(data, parms.sel, by = "species_code", all.x = T)

  mass[, c("BM_wood_kg", "BM_bark_kg", "BM_branch_kg","BM_fol_kg") := list(
    bwood1*dbh_cm^bwood2*Htot_m^bwood3, bbark1*dbh_cm^bbark2*Htot_m^bbark3,
    bbranches1*dbh_cm^bbranches2*Htot_m^bbranches3,bfoliage1*dbh_cm^bfoliage2*Htot_m^bfoliage3)]


  # /*kg/tree*/
  mass[,BM_Tree_kg := BM_wood_kg + BM_bark_kg + BM_branch_kg + BM_fol_kg]
  mass <- mass[,-(setdiff(names(parms.sel), "species_code")), with = F]
  if (model.D == T) {
    mass[,Htot_m := NULL]
    data[,Htot_m := NULL]
    if (H.tmp %in% names(mass)) {
      setnames(mass, H.tmp, "Htot_m")
      setnames(data, H.tmp, "Htot_m")}
  }
  mass
}

# abvG<- calbiomass(data = sample_data, model.Ver = "Ung2008", model.D = T)



