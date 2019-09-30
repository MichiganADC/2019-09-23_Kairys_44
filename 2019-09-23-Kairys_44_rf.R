# 2019-09-23_Kairys_44_NM.R

# library(profvis)
# profvis(expr = {

# SETUP ----
# Cmd+Shift+O (not Cmd+Shift+0) to show the handy Document Outline

# _ Useful packages ----
library(dplyr)
library(readr)
library(purrr)
library(stringr)

# _ Source personal API tokens ----
source("~/Box/Documents/R_helpers/config.R")
# _ Source helper functions ----
source("./helper_fxns.R")


# ........................................................ ----
# EXTRACT ----

# _ Get Data ----

# _ _ Define fields ----

# _ _ _ MiNDSet ----
fields_ms_blooddr <- c("subject_id", "agree_apoe")    # raw 1 = Yes
fields_ms_mrielig <- c("subject_id", "mri_elig_yn")   # raw 1 = Yes
fields_ms_mriimag <- c("subject_id", "mri_completed") # raw 2 = complete
fields_ms_rvf     <- c("subject_id", "mri_scan")      # raw 2 = Yes

fields_ms_concat <-
  c(
    fields_ms_blooddr
    , fields_ms_mrielig
    , fields_ms_mriimag
    , fields_ms_rvf
  )
fields_ms <- fields_ms_concat %>% unique() %>% paste(collapse = ",")

# _ _ _ UMMAP UDS 2 ----
fields_u2_e1 <- c("subject_id", "apoe") # raw 1 = Yes?

fields_u2_concat <- c(fields_u2_e1)
fields_u2 <- fields_u2_concat %>% unique() %>% paste(collapse = ",")

# _ _ _ UMMAP UDS 3 ----
fields_u3_e2f <- c("ptid", "ftdsmri")  # raw 1 = Yes
fields_u3_e2l <- c("ptid", "lbismri")  # raw 1 = Yes
fields_u3_e3f <- c("ptid", "ftdidiag") # raw 1 = Yes
fields_u3_e3l <- c("ptid", "lboeeg")   # raw 1 = Yes

fields_u3_concat <-
  c(
    fields_u3_e2f
    , fields_u3_e2l
    , fields_u3_e3f
    , fields_u3_e3l
  )
fields_u3 <- fields_u3_concat %>% unique() %>% paste(collapse = ",")

# _ _ _ Studies DB ----
fields_st_entry <- c("subject_id", "study") # raw 10 | 71 = EEG | CUES (resp.)

fields_st_concat <- c(fields_st_entry)
fields_st <- fields_st_concat %>% unique() %>% paste(collapse = ",")


# _ _ Retrieve Data ----

# _ _ _ REDCap Data ----

# Create lists to map over for API call
lst_uris <- # optional
  list(
    REDCAP_API_URI
    , REDCAP_API_URI
    , REDCAP_API_URI
    , REDCAP_API_URI
  )

lst_tokens <-
  list(
    REDCAP_API_TOKEN_MINDSET
    , REDCAP_API_TOKEN_UDS2
    , REDCAP_API_TOKEN_UDS3n
    , REDCAP_API_TOKEN_STUDIES
  )

lst_fieldses <-
  list(
    fields_ms
    , fields_u2
    , fields_u3
    , fields_st
  )

# Map over URI/token/fields lists and feed to `export_redcap_records`
lst_json_objs <-
  pmap(list(lst_uris, lst_tokens, lst_fieldses),
       function(uri, token, fields) {
         export_redcap_records(uri = uri, token = token, fields = fields) 
       })

# Map over JSON objects and convert them to tibbles
lst_tbls <- 
  map(lst_json_objs, json_to_tbl)
# names(lst_tbls) <- c("ms", "u2", "u3", "st")

# Pull out each tibble
df_ms <- lst_tbls[[1]]
df_u2 <- lst_tbls[[2]]
df_u3 <- lst_tbls[[3]] %>% filter(str_detect(ptid, "^UM\\d{8}$"))
df_st <- lst_tbls[[4]]

# _ _ _ CUES IDs Data ----
df_cues <- 
  read_csv("./CUES_IDs.csv", col_types = cols(.default = col_character())) 



# ........................................................ ----
# TRANSFORM ----

# _ APOE Available? ----
df_apoe <-
  full_join(
    x = df_ms %>% 
      select(subject_id, fields_ms_blooddr) %>% 
      filter(agree_apoe == 1),
    y = df_u2 %>% 
      select(subject_id, fields_u2_e1) %>% 
      filter(apoe == 1),
    by = c("subject_id" = "subject_id")
  )
ids_apoe <-
  df_apoe %>% pull(subject_id) %>% unique()


# _ MRI Available? ----
df_mri <- 
  full_join(
    x = df_ms %>% 
      select(subject_id, mri_elig_yn, mri_completed, mri_scan) %>% 
      filter(mri_elig_yn == 1 | mri_completed == 2 | mri_scan == 2),
    y = df_u3 %>% 
      select(ptid, ftdsmri, lbismri, ftdidiag) %>% 
      filter(ftdsmri == 1 | lbismri == 1 | ftdidiag == 1),
    by = c("subject_id" = "ptid")
  )
ids_mri <- 
  df_mri %>% pull(subject_id) %>% unique()


# _ EEG Available? ----
df_eeg <- df_u3 %>% filter(lboeeg == 1)
ids_eeg <- df_eeg %>% pull(ptid) %>% unique()


# _ Has CUES ID? ----
ids_cues <- df_cues %>% pull(`CUES ID (if CUES study)`) %>% unique()



# ........................................................ ----
# LOAD ----

# _ Results ----
ids_apoe_mri <- ids_apoe[ids_apoe %in% ids_mri]
ids_eeg_apoe_mri <- ids_eeg[ids_eeg %in% ids_apoe_mri]
ids_result <- ids_cues[ids_cues %in% ids_eeg_apoe_mri]

ids_cues_apoe_mri <- ids_cues[ids_cues %in% ids_apoe_mri]

# }, interval = 0.005, prof_output = "rf_output")

