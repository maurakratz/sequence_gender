
# 01 packages -------------------
pacman::p_load(haven, dailyr, summarytools, dplyr, installr, labelled)

# installr::check.for.updates.R()
# installr::updateR()



# 02 import -----------------------

# Read in the nomination data
nom_2025_raw <- haven::read_dta(r"(.\data\ZA10104_GLES_25_Nominierung_v1-0-0\ZA10104_v1-0-0.dta)")

# read in candidate data
candi_2025_raw <- haven::read_dta(r"(.\data\ZA10102_GLES25_Kandidierende_v1-0-0/ZA10102_v1-0-0.dta)")
# Grundgesamtheit laut CB: "Die Grundgesamtheit bestand aus den Kandidierenden
## aller Parteien, die vor der Bundestagswahl 2025 mit Fraktions- oder
## Gruppenstatus im Bundestag vertreten waren: SPD, CDU, CSU, BÜNDNIS
## 90/DIE GRÜNEN, FDP, AfD, DIE LINKE und BSW.
# Auswahlgesamtheit laut CB:
## 2665 Kandidat*innen,
## davon 1378 Doppelkandidaturen (knapp 52%),
## 844 ausschließl. Listenkandidaturen (knapp 32%)
## und 443 ausschließl. Wahlkreiskandidaturen (knapp 17%)
# Davon ahben 651 geantwortet davon 513 valide Beantwortungen
## das entspricht etwa 19% Ausschöpfungsquote

# Lables sichern --------------
nom_2025_var_labels <- labelled::var_label(nom_2025_raw)
candi_2025_var_labels <- labelled::var_label(candi_2025_raw)




# 03 cleaning ---------------

# selecting nomination data
nom_2025 <- nom_2025_raw %>%
   select (-1,-2,-3,-4,-5,-6,-7) # remove first variables (metadata)

# selecting candidate data
candi_2025 <- candi_2025_raw %>%
   select(-1,-2,-3,-4,-5,-6) # remove first variables (metadata)

candi_2025 <- candi_2025 %>% # remove col 2 - col 15
   select(-2:-15)
# hier bei zeiten nochmal nachbessern: was sagen mir die meta und paradaten
# und sollte ich darauf basierend beobachtungen ausschließen ?




# 04 Missings -------------------

# Tipp fürs nächste Mal: nom_2025_clean <- nom_2025_clean %>% haven::zap_missing()

candi_2025 %>%
   dailyr::var_overview()
# Was bedeutetn die Minuswerte?

# bei candi: -71 bis -99 missings (siehe cdb)
candi_2025_clean <- candi_2025 %>%
   mutate(across(where(is.numeric), ~ case_when(
      .x < -70 ~ NA_real_,
      TRUE ~ .x
   )))

# label wieder anfügen (nur vorhandene)
candi_2025_clean <- candi_2025_clean %>%
   labelled::set_variable_labels(.labels = candi_2025_var_labels,
                                 .strict = FALSE) # Re-attach labels loosely

candi_2025_clean_overview <- candi_2025_clean %>%
   dailyr::var_overview()



# bei nominierung: -97 trifft nicht zu und -99 keine Angabe (siehe cdb)

nom_2025 %>%
   dailyr::var_overview()

nom_2025_clean <- nom_2025 %>%
   mutate(across(where(is.numeric), ~ case_when(
      .x < -96 ~ NA_real_,       # Convert GLES missing codes to NA
      TRUE ~ .x                  # Keep valid numerical values
   )))

nom_2025_clean <- nom_2025_clean %>%
   mutate(across(where(is.character), ~ case_when(
      .x == "-99 keine Angabe"     ~ NA_character_,
      .x == "-97 trifft nicht zu"  ~ NA_character_,
      .default = .x
   )))


nom_2025_clean_overview <- nom_2025_clean %>%
   dailyr::var_overview()

# reapply labels
nom_2025_clean <- nom_2025_clean %>%
   labelled::set_variable_labels(.labels = nom_2025_var_labels,
                                 .strict = FALSE) # Re-attach labels loosely



# look for n30
nom_2025_clean %>%
   count (n03)

nom_2025_clean %>%
   labelled::look_for("n03")
# das ist nicht schlimm, weil ich bei etwaigen ABbildungen ja erst missings
# filtern und dann as_factor verwenden werde



# ALTER CODE, DER MISSINGS EINZELN UMCODIERT
if(FALSE){
# checking nomination variables one by one
nom_2025 %>%
   count(n30)

nom_2025_clean %>%
   count(n30)

# und mache die gefundenen missings explizit
nom_2025 <- nom_2025 %>%
   dplyr::mutate(
      # -99 only
      n02 = dplyr::case_when(n02 == -99 ~ NA_real_, .default = n02),
      n04 = dplyr::case_when(n04 == -99 ~ NA_real_, .default = n04),
      n08 = dplyr::case_when(n08 == -99 ~ NA_real_, .default = n08),
      n09 = dplyr::case_when(n09 == -99 ~ NA_real_, .default = n09),
      n10 = dplyr::case_when(n10 == -99 ~ NA_real_, .default = n10),
      n11 = dplyr::case_when(n11 == -99 ~ NA_real_, .default = n11),
      n12 = dplyr::case_when(n12 == -99 ~ NA_real_, .default = n12),
      n17 = dplyr::case_when(n17 == -99 ~ NA_real_, .default = n17),
      n18 = dplyr::case_when(n18 == -99 ~ NA_real_, .default = n18),
      n19 = dplyr::case_when(n19 == -99 ~ NA_real_, .default = n19),
      n20 = dplyr::case_when(n20 == -99 ~ NA_real_, .default = n20),
      n25 = dplyr::case_when(n25 == -99 ~ NA_real_, .default = n25),
      n26 = dplyr::case_when(n26 == -99 ~ NA_real_, .default = n26),
      # -99 and -97
      n03 = dplyr::case_when(n03 %in% c(-99, -97) ~ NA_real_, .default = n03),
      n05 = dplyr::case_when(n05 %in% c(-99, -97) ~ NA_real_, .default = n05),
      n13 = dplyr::case_when(n13 %in% c(-99, -97) ~ NA_real_, .default = n13),
      n14 = dplyr::case_when(n14 %in% c(-99, -97) ~ NA_real_, .default = n14),
      n15 = dplyr::case_when(n15 %in% c(-99, -97) ~ NA_real_, .default = n15),
      n16 = dplyr::case_when(n16 %in% c(-99, -97) ~ NA_real_, .default = n16),
      n23 = dplyr::case_when(n23 %in% c(-99, -97) ~ NA_real_, .default = n23),
      n24 = dplyr::case_when(n24 %in% c(-99, -97) ~ NA_real_, .default = n24),
      n27 = dplyr::case_when(n27 %in% c(-99, -97) ~ NA_real_, .default = n27),
      n28 = dplyr::case_when(n28 %in% c(-99, -97) ~ NA_real_, .default = n28),
      n29 = dplyr::case_when(n29 %in% c(-99, -97) ~ NA_real_, .default = n29),
      n30 = dplyr::case_when(n30 %in% c(-99, -97) ~ NA_real_, .default = n30),
      # character missings
      n06 = dplyr::case_when(n06 == "-99 keine Angabe" ~ NA_character_, .default = n06),
      n20s = dplyr::case_when(n20s == "-99 keine Angabe" ~ NA_character_,
                              n20s == "-97 trifft nicht zu" ~ NA_character_,
                              .default = n20s),
      n21s = dplyr::case_when(n21s == "-99 keine Angabe" ~ NA_character_, .default = n21s),
      n22s = dplyr::case_when(n22s == "-99 keine Angabe" ~ NA_character_, .default = n22s)
   )
# check codebook for that


nom_2025 %>%
   count(n02)

# labels wieder anfügen
labelled::var_label(nom_2025) <- var_labels




# check candidate variables
candi_2025 %>%
   count(a9a) # umkämpfte WK-Kandidatur?

candi_2025 %>%
   count(a9b) # umkämpfte Landeslistenkandidatur?
}




# 05 Variable Classes ----------------------------


## Nominierungsstudie ------------

# Schritt 1 Überblick

# alle var classes checken
  # View(nom_2025_clean_overview)

# Schritt 2 Missing Value Labels weg
remove_negative_val_labels <- function(x) {
   if (haven::is.labelled(x)) {
      labs <- attr(x, "labels")
      attr(x, "labels") <- labs[labs >= 0]
   }
   x
}

nom_2025_clean <- nom_2025_clean %>%
   mutate(across(everything(), remove_negative_val_labels))


# test zap:
nom_2025_clean %>%
labelled::look_for("n03")

# manche von denen kann ich bei Bedarf ad hoc ändern.
# Andere sollte ich vorab ein für alle mal ändern:

# Schritt 3 gezielte Umwandlung zu Faktoren und Datum
# vorab check: geschlecht
nom_2025_clean %>%
   count(n19)

nom_2025_clean <- nom_2025_clean %>%
   mutate(
      bula   = as_factor(bula),
      partei = as_factor(partei),
      n19    = as_factor(n19),  # Geschlecht
      n20    = as_factor(n20),  # Bildung: Schule
      n24    = as_factor(n24),  # Amt: aktuelles
      n06    = as.Date(n06)     # Nominierungsdatum
   )

# test nachher
nom_2025_clean %>%
   count(n19)

# Overview updaten
nom_2025_clean_overview <- nom_2025_clean %>%
   dailyr::var_overview()

# als csv exportieren
nom_2025_clean_overview %>%
   write.csv2(r"(.\results\nom_2025_clean_overview.csv)")


## Kandidierendenstudie -------------

# Schritt 1 Überblick
# alle var classes checken
  # View(candi_2025_clean_overview)

# candi_2025_clean_overview als csv exportieren
candi_2025_clean_overview %>%
   write.csv2(r"(.\results\candi_2025_clean_overview.csv)")

# Schritt 2 Missing Value Labels weg
candi_2025_clean <- candi_2025_clean %>%
   mutate(across(everything(), remove_negative_val_labels))

# Schritt 3 gezielte Umwandlung zu Faktoren und Datum
# vorab check
candi_2025_clean %>%
   count(geschlecht)

candi_2025_clean <- candi_2025_clean %>%
   mutate(
      bula = as_factor(bula),
      partei = as_factor(partei),
      geschlecht = as_factor(geschlecht),
      kandidaturtyp = as_factor(kandidaturtyp)
   )

# nachher check
candi_2025_clean %>%
   count(geschlecht)

# nachher overview updaten
candi_2025_clean_overview %>%
   write.csv2(r"(.\results\candi_2025_clean_overview.csv)")


# 05 dfSummary -----------------------

nom_2025_clean %>%
   summarytools::dfSummary(varnumbers = FALSE,
                           valid.col  = FALSE,
                           max.distinct.values = 3) %>%
   summarytools::view(file = "results/nom_2025_clean_summary.html")


candi_2025_clean %>%
   summarytools::dfSummary(varnumbers = FALSE,
                           valid.col = FALSE,
                           max.distinct.values = 3) %>%
   summarytools::view(file = "results/candi_2025_clean_summary.html")


# 06 save dataset --------------------
save(nom_2025_clean, candi_2025_clean,
     file = r"(.\data\btw2025_clean.RData)")
