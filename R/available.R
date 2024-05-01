.allowedClimateTypes <- c("historical", "historical_normals", "future", "future_normals")

# dir("~/data/climate/ClimateNA_data/historical/normals/yearly/16/Normal_1991_2020Y") |>
#   tools::file_path_sans_ext() |>
#   dput()
.allowedClimateVars_nrm <- c(
  "AHM", "bFFP", "CMD", "CMI", "DD_0", "DD_18", "DD1040", "DD18",
  "DD5", "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT",
  "MCMT", "MSP", "MWMT", "NFFD", "PAS", "RH", "SHM", "TD"
)

# dir("~/data/climate/ClimateNA_data/historical/monthly/16/Year_2000M") |>
#   tools::file_path_sans_ext() |>
#   dput()
.allowedClimateVars_M <- c(
  "CMD01", "CMD02", "CMD03", "CMD04", "CMD05", "CMD06", "CMD07",
  "CMD08", "CMD09", "CMD10", "CMD11", "CMD12", "CMI01", "CMI02",
  "CMI03", "CMI04", "CMI05", "CMI06", "CMI07", "CMI08", "CMI09",
  "CMI10", "CMI11", "CMI12", "DD_0_01", "DD_0_02", "DD_0_03", "DD_0_04",
  "DD_0_05", "DD_0_06", "DD_0_07", "DD_0_08", "DD_0_09", "DD_0_10",
  "DD_0_11", "DD_0_12", "DD_18_01", "DD_18_02", "DD_18_03", "DD_18_04",
  "DD_18_05", "DD_18_06", "DD_18_07", "DD_18_08", "DD_18_09", "DD_18_10",
  "DD_18_11", "DD_18_12", "DD18_01", "DD18_02", "DD18_03", "DD18_04",
  "DD18_05", "DD18_06", "DD18_07", "DD18_08", "DD18_09", "DD18_10",
  "DD18_11", "DD18_12", "DD5_01", "DD5_02", "DD5_03", "DD5_04",
  "DD5_05", "DD5_06", "DD5_07", "DD5_08", "DD5_09", "DD5_10", "DD5_11",
  "DD5_12", "Eref01", "Eref02", "Eref03", "Eref04", "Eref05", "Eref06",
  "Eref07", "Eref08", "Eref09", "Eref10", "Eref11", "Eref12", "NFFD01",
  "NFFD02", "NFFD03", "NFFD04", "NFFD05", "NFFD06", "NFFD07", "NFFD08",
  "NFFD09", "NFFD10", "NFFD11", "NFFD12", "PAS01", "PAS02", "PAS03",
  "PAS04", "PAS05", "PAS06", "PAS07", "PAS08", "PAS09", "PAS10",
  "PAS11", "PAS12", "PPT01", "PPT02", "PPT03", "PPT04", "PPT05",
  "PPT06", "PPT07", "PPT08", "PPT09", "PPT10", "PPT11", "PPT12",
  "Rad01", "Rad02", "Rad03", "Rad04", "Rad05", "Rad06", "Rad07",
  "Rad08", "Rad09", "Rad10", "Rad11", "Rad12", "RH01", "RH02",
  "RH03", "RH04", "RH05", "RH06", "RH07", "RH08", "RH09", "RH10",
  "RH11", "RH12", "Tave01", "Tave02", "Tave03", "Tave04", "Tave05",
  "Tave06", "Tave07", "Tave08", "Tave09", "Tave10", "Tave11", "Tave12",
  "Tmax01", "Tmax02", "Tmax03", "Tmax04", "Tmax05", "Tmax06", "Tmax07",
  "Tmax08", "Tmax09", "Tmax10", "Tmax11", "Tmax12", "Tmin01", "Tmin02",
  "Tmin03", "Tmin04", "Tmin05", "Tmin06", "Tmin07", "Tmin08", "Tmin09",
  "Tmin10", "Tmin11", "Tmin12"
)

# dir("~/data/climate/ClimateNA_data/historical/all/16/Year_2000MSY") |>
#   tools::file_path_sans_ext() |>
#   grep("_(at|sm|sp|wt)$", x = _, value = TRUE) |>
#   dput()
.allowedClimateVars_S <- c(
  "CMD_at", "CMD_sm", "CMD_sp", "CMD_wt", "CMI_at", "CMI_sm",
  "CMI_sp", "CMI_wt", "DD_0_at", "DD_0_sm", "DD_0_sp", "DD_0_wt",
  "DD_18_at", "DD_18_sm", "DD_18_sp", "DD_18_wt", "DD18_at", "DD18_sm",
  "DD18_sp", "DD18_wt", "DD5_at", "DD5_sm", "DD5_sp", "DD5_wt",
  "Eref_at", "Eref_sm", "Eref_sp", "Eref_wt", "NFFD_at", "NFFD_sm",
  "NFFD_sp", "NFFD_wt", "PAS_at", "PAS_sm", "PAS_sp", "PAS_wt",
  "PPT_at", "PPT_sm", "PPT_sp", "PPT_wt", "Rad_at", "Rad_sm", "Rad_sp",
  "Rad_wt", "RH_at", "RH_sm", "RH_sp", "RH_wt", "Tave_at", "Tave_sm",
  "Tave_sp", "Tave_wt", "Tmax_at", "Tmax_sm", "Tmax_sp", "Tmax_wt",
  "Tmin_at", "Tmin_sm", "Tmin_sp", "Tmin_wt"
)

# dir("~/data/climate/ClimateNA_data/historical/yearly/16/Year_2000Y") |>
#   tools::file_path_sans_ext() |>
#   dput()
.allowedClimateVars_Y <- c(
  "AHM", "bFFP", "CMD", "CMI", "DD_0", "DD_18", "DD1040", "DD18",
  "DD5", "eFFP", "EMT", "Eref", "EXT", "FFP", "MAP", "MAR", "MAT",
  "MCMT", "MSP", "MWMT", "NFFD", "PAS", "RH", "SHM", "TD"
)

.allowedClimateVars_MSY <- c(.allowedClimateVars_M, .allowedClimateVars_S, .allowedClimateVars_Y)
.allowedClimateVars <- c(.allowedClimateVars_nrm, .allowedClimateVars_MSY)

#' Identify ClimateNA data available via this package
#'
#' @template ClimateNA_type
#'
#' @export
#'
#' @examples
#' historical_years <- available("historical")[["years"]]
#' hist_nrm_prds <- available("historical_normals")[["periods"]]
#'
#' future_gcms <- available("future")[["gcms"]]
#' future_ssps <- available("future")[["ssps"]]
#' future_years <- available("future")[["years"]]
#' future_nrm_prds <- available("future_normals")[["periods"]]
available <- function(type) {
  stopifnot(type %in% .allowedClimateTypes)

  future_gcms <- c(
    # "8GCMs_ensemble", ## see http://climatena.ca/downloads/ClimateNA_8ModelRationale_Mahony_07May2022.pdf
    "CanESM5",
    "CNRM-ESM2-1"
  )
  future_ssps <- c(
    # "126",
    "245",
    "370",
    "585"
  )

  switch(type,
    historical = list(
      years = 1901L:2022L
    ),
    historical_normals = list(
      periods = c(
        "1901_1930", "1911_1940", "1921_1950",
        "1931_1960", "1941_1970", "1951_1980",
        "1971_2000", "1981_2010", "1991_2020"
      )
    ),
    future = list(
      years = 2011L:2100L,
      gcms = future_gcms,
      ssps = future_ssps
    ),
    future_normals = list(
      periods = c("2011_2040", "2041_2070", "2071_2100"),
      gcms = future_gcms,
      ssps = future_ssps
    )
  )
}
