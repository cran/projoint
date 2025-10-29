## ----setup, include=FALSE-----------------------------------------------------
library(projoint)
library(dplyr)
library(ggplot2)
library(patchwork)

## ----echo = FALSE-------------------------------------------------------------
data("exampleData1")
outcomes <- paste0("choice", 1:8)
outcomes <- c(outcomes, "choice1_repeated_flipped")
out1 <- reshape_projoint(exampleData1, outcomes)

## -----------------------------------------------------------------------------
mm0 <- projoint(out1, .structure = "profile_level", .estimand = "mm")
print(mm0)
summary(mm0)

## -----------------------------------------------------------------------------
qoi_1 <- set_qoi(
  .structure = "profile_level",
  .estimand = "mm",
  .att_choose = "att1",
  .lev_choose = "level1"
)

mm1 <- projoint(out1, .qoi = qoi_1)
print(mm1)
summary(mm1)

## -----------------------------------------------------------------------------
mm1b <- projoint(out1, .qoi = qoi_1, .irr = 0.75)
print(mm1b)
summary(mm1b)

## -----------------------------------------------------------------------------
amce0 <- projoint(out1, .structure = "profile_level", .estimand = "amce")
print(amce0)
summary(amce0)

## -----------------------------------------------------------------------------
qoi_3 <- set_qoi(
  .structure = "profile_level",
  .estimand = "amce",
  .att_choose = "att1",
  .lev_choose = "level3",
  .att_choose_b = "att1",
  .lev_choose_b = "level1"
)

amce1 <- projoint(out1, .qoi = qoi_3)
print(amce1)
summary(amce1)

## -----------------------------------------------------------------------------
amce1b <- projoint(out1, .qoi = qoi_3, .irr = 0.75)
print(amce1b)
summary(amce1b)

## -----------------------------------------------------------------------------
data("out1_arranged")
mm   <- projoint(out1_arranged, .structure = "profile_level") 
amce <- projoint(out1_arranged, .structure = "profile_level", .estimand = "amce")

## ----fig.width = 6, fig.height = 6--------------------------------------------
plot(mm)

## ----fig.width = 6, fig.height = 6--------------------------------------------
plot(amce)

## ----fig.height = 6.5, fig.width = 7------------------------------------------
outcomes <- c(paste0("choice", 1:8), "choice1_repeated_flipped")

df <- exampleData1 |> mutate(white = ifelse(race == "White", 1, 0))

df_0 <- df |> filter(white == 0) |> reshape_projoint(outcomes)
df_1 <- df |> filter(white == 1) |> reshape_projoint(outcomes)
df_d <- df |> reshape_projoint(outcomes, .covariates = "white")

data_file <- system.file("extdata", "labels_arranged.csv", package = "projoint")
if (data_file == "") stop("File not found!")

df_0 <- read_labels(df_0, data_file)
df_1 <- read_labels(df_1, data_file)
df_d <- read_labels(df_d, data_file)

out_0 <- projoint(df_0, .structure = "profile_level")
out_1 <- projoint(df_1, .structure = "profile_level")
out_d <- projoint(df_d, .structure = "profile_level", .by_var = "white")

plot_0 <- plot(out_0)
plot_1 <- plot(out_1)
plot_d <- plot(out_d, .by_var = TRUE)

plot_0 +
  coord_cartesian(xlim = c(0.2, 0.8)) +
  labs(title = "Non-white", x = "AMCE") +
  theme(plot.title = element_text(hjust = 0.5)) +

plot_1 +
  coord_cartesian(xlim = c(0.2, 0.8)) +
  labs(title = "White", x = "AMCE") +
  theme(axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5)) +

plot_d +
  coord_cartesian(xlim = c(-0.4, 0.4)) +
  labs(title = "Difference", x = "Difference") +
  theme(axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))

## ----echo=FALSE, warning = F--------------------------------------------------
library(downloadthis)
download_link(
  link = "https://raw.githubusercontent.com/yhoriuchi/projoint/master/data-raw/CHKKK_Mummolo_and_Nall_full_replication_W1_Lucid_-_August_2021.qsf",
  button_label = "Download QSF file",
  button_type = "danger",
  has_icon = TRUE,
  icon = "fa fa-save",
  self_contained = FALSE
)

