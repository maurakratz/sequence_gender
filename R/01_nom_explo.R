# NOMINIERUNGSSTUDIE ---------------
# RUN IMPORT AND CLEANING SCRIPT FIRST!

# setup ------------
pacman::p_load(dplyr, tidyr, haven, lubridate, ggplot2, janitor)

load(r"(.\data\btw2025_clean.RData)")
# enthält beide cleaned datasets nominierungsstudie und kandidierendenstudie, die ich in 00_import_and_cleaning erstellt habe


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


# 01 Innerparteilicher Wettbewerb in Wahlkreisen ----------------

nom_2025_clean %>%
   count(partei)
6*299 # = 1794 Parteiwahlkreise (eigentlich 1786 siehe unten)
# CDU und CSU quasi einander erg?nzend weil sie nicht in allen 299 wk aufstellen

# Parteiwahlkreise gesamt
partei_wk_gesamt <- nom_2025_clean %>%
   filter(n01 == 1) %>%
   group_by(wknr, partei) %>%
   count() %>%
   ungroup() %>%
   count(partei, name = "wk_gesamt")

# Parteiwahlkreise mit Wettbewerb (mehr als ein Aspirant)
# Wieviel innerparteilichen Wettbewerb gab es in den Wahlkreisen ?
partei_wk_wettbewerb <- nom_2025_clean %>%
   group_by(wknr, partei) %>%
   count() %>%
   filter(n > 1) %>%
   ungroup() %>%
   count(partei, name = "wk_mit_wettbewerb")

nom_2025_clean %>%
   group_by(wknr, partei) %>%
   count() %>%
   filter(n > 1) %>%
   ungroup() %>%
   count(as_factor(partei)) %>%
   adorn_totals("row")

214/1821
# In wieveieln Wahlkreisen*Partei gab es mehr als eine Kandidatur?
# von den 1.821  Parteiwahlkreisen gab es in 214 mehr als eine Kandidatur
# das sind 0.1175178, also 11.75% der Parteiwahlkreise


# Tabelle zusammenführen
partei_wk_gesamt %>%
   left_join(partei_wk_wettbewerb, by = "partei") %>%
   mutate(
      partei             = as_factor(partei),
      wk_mit_wettbewerb  = coalesce(wk_mit_wettbewerb, 0L),
      min_anteil_wettbewerb  = round(wk_mit_wettbewerb / wk_gesamt * 100, 2)
   ) %>%
   adorn_totals("row")






nom_2025_clean %>%
   distinct(wknr)

nom_2025_clean %>%
   count(name_flag)


# saubere Tabelle mit den 7 Parteien, Anzahl der WK in denen sie Kandidaten
# aufgestellt haben, Anzahl der WK in denen sie mehr als einen Kandidaten
# aufgestellt haben, prozentualer Anteil der Parteiwahlkreise in denne es Wettbewerb gab
nom_2025_clean %>%
   group_by(wknr, partei) %>%
   count() %>%
   filter(n > 1) %>%
   ungroup() %>%
   count(as_factor(partei)) %>% # spalte mit wahlkreisen in denen die partei ?berhaupt aufgestelt hat
   left_join(
      nom_2025_clean %>%
         group_by(wknr, partei) %>%
         count() %>%
         ungroup() %>%
         count(as_factor(partei)),
      by = "as_factor(partei)"
   ) %>%
   rename(
      partei = `as_factor(partei)`,
      wk_mit_wettbewerb = n.x,
      wk_gesamt = n.y
   ) %>%
   adorn_totals("row") %>% # spalte mit anteil dahinter
   mutate(anteil_wettbewerb = round((wk_mit_wettbewerb / wk_gesamt)*100, 2))






# 02 Aufstellungsdatum ----------------

# inspect n06
nom_2025_clean %>%
   count(n06) %>%
   print(n=200)


nom_2025_clean %>%
   dplyr::count(partei)

# übersicht missings
nom_2025_clean %>%
   filter(n01 == 1) %>%
   mutate(partei = as_factor(partei)) %>%
   group_by(partei) %>%
   summarise(
      wk_gesamt      = n_distinct(wknr),
      wk_mit_datum   = n_distinct(wknr[!is.na(n06)]),
      wk_ohne_datum  = n_distinct(wknr[is.na(n06)]),
      anteil_mit_datum = round(wk_mit_datum / wk_gesamt * 100, 2)
   ) %>%
   adorn_totals("row")


# wieviele kandidierende wurden pro monat aufgestellt von den parteien?
nom_2025_clean %>%
   group_by(partei, n06) %>%
   summarise(n = n()) %>%
   print(n=200)

# kumulierte anzahl
nom_2025_clean %>%
   filter(n01 == 1, !is.na(n06)) %>%
   mutate(partei = as_factor(partei)) %>%
   group_by(partei, n06) %>%
   summarise(n = n(), .groups = "drop") %>%
   arrange(partei, n06) %>%
   group_by(partei) %>%
   mutate(kumuliert = cumsum(n)) %>%
   ggplot(aes(x = n06, y = kumuliert, group = partei, color = partei)) +
   geom_line() +
   scale_color_manual(values = party_colors) +
   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
   theme_minimal() +
   labs(
      title = "Kumulierte Nominierungen WK-Kandidierende nach Partei",
      x = "Datum", y = "Kumulierte Anzahl", color = "Partei"
   ) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#kumulierte anteiel
nom_2025_clean %>%
   filter(n01 == 1, !is.na(n06)) %>%
   mutate(partei = as_factor(partei)) %>%
   group_by(partei, n06) %>%
   summarise(n = n(), .groups = "drop") %>%
   arrange(partei, n06) %>%
   group_by(partei) %>%
   mutate(
      kumuliert = cumsum(n),
      anteil    = kumuliert / sum(n) * 100
   ) %>%
   ggplot(aes(x = n06, y = anteil, group = partei, color = partei)) +
   geom_line() +
   scale_color_manual(values = party_colors) +
   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
   theme_minimal() +
   labs(
      title = "Kumulierte Nominierungen WK-Kandidierende nach Partei (Anteile)",
      x = "Datum", y = "Kumulierter Anteil (%)", color = "Partei"
   ) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))


# auf monat aggregiert (eiegntlich nicht notwendig, siehe hier drüber)
nom_2025_clean %>%
   dplyr::mutate(monat = lubridate::floor_date(n06, "month")) %>%
   dplyr::group_by(haven::as_factor(partei), monat) %>%
   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
   print(n = 200)

nom_2025_clean %>%
   dplyr::mutate(monat = format(n06, "%Y-%m")) %>%
   dplyr::group_by(haven::as_factor(partei), monat) %>%
   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
   print(n = 200)

# nun darstellen
nom_2025_clean %>%
   filter(n01 == 1, !is.na(n06)) %>%
   mutate(
      monat  = floor_date(n06, "month"),
      partei = as_factor(partei)
   ) %>%
   group_by(partei, monat) %>%
   summarise(n = n(), .groups = "drop") %>%
   ggplot(aes(x = monat, y = n, group = partei, color = partei)) +
   geom_line() +
   scale_color_manual(values = party_colors) +
   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
   theme_minimal() +
   labs(title = "Anzahl Nominierungsveranstaltungen pro Monat und Partei",
        x = "Monat", y = "Anzahl Kandidierende", color = "Partei") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# facettiertes Balkendiagramm
nom_2025_clean %>%
   filter(n01 == 1, !is.na(n06)) %>%
   mutate(
      monat  = floor_date(n06, "month"),
      partei = as_factor(partei)
   ) %>%
   group_by(partei, monat) %>%
   summarise(n = n(), .groups = "drop") %>%
   ggplot(aes(x = monat, y = n, fill = partei)) +
   geom_col() +
   scale_fill_manual(values = party_colors) +
   scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
   facet_wrap(~ partei, ncol = 2) +
   theme_minimal() +
   labs(title = "Nominierungen WK-Kandidierende pro Monat und Partei",
        x = "Monat", y = "Anzahl", fill = "Partei") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "none")
