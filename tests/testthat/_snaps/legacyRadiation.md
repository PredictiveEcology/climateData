# pre-v7.60 radiation names stop with the new name

    Code
      .stopIfLegacyRadiation(c("historical_MAR", "future_Rad07", "historical_Rad_sm",
        "historical_MAR_normal"))
    Condition
      Error:
      ! Solar radiation variables were renamed in ClimateNA v7.60 (see NEWS.md):
        historical_MAR is now historical_rsds
        future_Rad07 is now future_rsds07
        historical_Rad_sm is now historical_rsds_sm
        historical_MAR_normal is now historical_rsds_normal

# prepClimateLayers stops on pre-v7.60 radiation names before downloading

    Code
      prepClimateLayers(climateVarsList = list(historical_MAR = list(vars = "historical_MAR",
        fun = quote(calcAsIs), .dots = list(historical_years = 2000))), srcdir = d,
      dstdir = d, tile = 39)
    Condition
      Error:
      ! Solar radiation variables were renamed in ClimateNA v7.60 (see NEWS.md):
        historical_MAR is now historical_rsds

