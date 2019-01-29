library(RSQLite)
library(xlsx)
library(dplyr)
library(DT)
# library(ggplot2)
library(lubridate)
library(highcharter)


# app/shiny/
dataloc = "data/Perpustakaan.sqlite"

format.money  <- function(x, digits = 0, ...) {
  paste0("Rp.", formatC(as.numeric(x), format = "f", digits = digits, big.mark = ".", decimal.mark = ","))
}

getTable = function(tblnm, file.loc = "data/Perpustakaan.sqlite")
{
  dbcon = RSQLite::dbConnect(RSQLite::SQLite(), file.loc)
  tbl_dt = dbReadTable(dbcon, tblnm)
  dbDisconnect(dbcon)
  return(tbl_dt)
}

thm = c("cerulean", "cosmo", "cyborg", "darkly", "flatly", "journal", "lumen", "paper", "readable", "sandstone", "simplex", "slate", "spacelab", "superhero", "united", "yeti")

tbl_setting = getTable("PENGATURAN", dataloc)
panel_colr_disp = tbl_setting$PANEL_COLR_DISP
panel_colr_val = tbl_setting$PANEL_COLR_VAL
theme_colr_disp = tbl_setting$THEME




