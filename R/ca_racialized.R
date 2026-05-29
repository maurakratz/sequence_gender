# racialized? -------------------------

pacman::p_load(rethnicity)


pred_ca_candi <- rethnicity::predict_ethnicity(
  firstnames = ca_candi$given_nm,
  lastnames  = ca_candi$surname,
  method     = "fullname"
)

ca_candi <- ca_candi %>%
  dplyr::bind_cols(pred_ca_candi %>%
                     dplyr::select(prob_asian,
                                   prob_black,
                                   prob_hispanic,
                                   prob_white))

summary(ca_candi$prob_white)
hist(ca_candi$prob_white, breaks = 30)

threshold <- 0.5

ca_candi <- ca_candi %>%
  dplyr::mutate(racialized = dplyr::case_when(
    prob_white < threshold ~ 1L,
    TRUE                   ~ 0L
  ))

table(ca_candi$racialized)
