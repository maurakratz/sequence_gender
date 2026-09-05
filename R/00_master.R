# MASTER

# packages -----------------

pacman::p_load(gert, installr, haven, labelled,
               dailyr, summarytools,
               dplyr, tidyr,
               lubridate, ggplot2, janitor)

# installr::check.for.updates.R()
# installr::updateR()


# functions and vectors ---------------------

remove_negative_val_labels <- function(x) {
   if (haven::is.labelled(x)) {
      labs <- attr(x, "labels")
      attr(x, "labels") <- labs[labs >= 0]
   }
   x
}


party_colors <- c(
   "CDU/CSU" = "#000000",
   "CDU" = "#000000",
   "SPD" = "#E3000F",
   "Bündnis 90/Die Grünen" = "#00A651",
   "Grünen" = "#00A651",
   "Grüne" = "#00A651",
   "FDP" = "#FFED00",
   "Die Linke" = "#BE3075",
   "DieLinke" = "#BE3075",
   "Linke" = "#BE3075",
   "AfD" = "#009EE0",
   "BSW" = "#6B1F7C",
   "Sonstige" = "#999999"
)

party_colors <- c(
   "CDU" = "#000000",
   "CSU" = "#000000",
   "SPD" = "#E3000F",
   "GRUENE" = "#00A651",
   "FDP" = "#FFED00",
   "DIE LINKE" = "#BE3075",
   "AfD" = "#009EE0",
   "BSW" = "#6B1F7C"
)



# scripts ----------------------
# source(01_import_and_cleaning)

# to dos ----------------------------
# alter code in import doc löschen
# datensatz zur kandi nominierung genauer verstehen
# ggf. listen aufstellungsdatum hinzufügen
