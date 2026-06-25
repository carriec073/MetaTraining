bio <- read_csv(file = here("Bio_CleanStacked_Feb26.csv")) |>
  select(
    "ID",
    "Timepoint",
    "question",
    "click" = "response",
    "accurate",
    "TrialRT",
    "ConfResp",
    "ConfRT",
    "Group",
    "S1",
    "CorResp"
  )
bio$task <- "bio"

#On Feb 26, 2026, I changed the column title to CorResp so the rbind function works
dots <- read_csv(file = here("Dots_CleanStacked_Feb26.csv")) |>
  select(
    "ID",
    "Timepoint",
    "dots_num_left", # need to rm
    "dots_num_right", # need to rm
    "accurate" = "discrimination_is_correct",
    "TrialRT" = "DotTrialRT",
    "ConfResp" = "confidence_key",
    "ConfRT" = "DotConfRT",
    "Group",
    "S1",
    "CorResp" = "Dots_Accurate"
  )
dots <- dots |>
  mutate(
    task = "dots",
    question = dots_num_left - dots_num_right,
    click = case_when(
      question < 0 & accurate == "TRUE" ~ "arrowright",
      question < 0 & accurate == "FALSE" ~ "arrowleft",
      question > 0 & accurate == "FALSE" ~ "arrowright",
      question > 0 & accurate == "TRUE" ~ "arrowleft"
    )
  ) |>
  select(-dots_num_left, -dots_num_right)


df <- rbind(bio, dots)

# remove missing responses in the bio task (need to check)
df <- df |> filter(!is.na(click))


df_pre <- df |>
  filter(Timepoint == "1") |>
  mutate(
    feedback = case_when(
      Group == "NoFB" ~ "none",
      str_detect(Group, "Type1") ~ "T1",
      str_detect(Group, "Type2") ~ "T2"
    ),
    video = case_when(
      Group == "NoFB" ~ "attention",
      str_detect(Group, "High") ~ "meta",
      str_detect(Group, "Low") ~ "attention"
    )
  )

df_pre <- df_pre |>
  mutate(
    acc = as.numeric(accurate),
    stim = case_when(
      acc == 1 & click == "arrowright" ~ "right",
      acc == 0 & click == "arrowright" ~ "left",
      acc == 1 & click == "arrowleft" ~ "left",
      acc == 0 & click == "arrowleft" ~ "right"
    )
  )



#create a separate dataframe for the post session (timepoint 2)
df_post <- df |>
  filter(Timepoint == "2") |>
  mutate(
    feedback = case_when(
      Group == "NoFB" ~ "none",
      str_detect(Group, "Type1") ~ "T1",
      str_detect(Group, "Type2") ~ "T2"
    ),
    video = case_when(
      Group == "NoFB" ~ "attention",
      str_detect(Group, "High") ~ "meta",
      str_detect(Group, "Low") ~ "attention"
    )
  )

df_post <- df_post |>
  mutate(
    acc = as.numeric(accurate),
    stim = case_when(
      acc == 1 & click == "arrowright" ~ "right",
      acc == 0 & click == "arrowright" ~ "left",
      acc == 1 & click == "arrowleft" ~ "left",
      acc == 0 & click == "arrowleft" ~ "right"
    )
  )


