## ----setup, include=FALSE-----------------------------------------------------
library(projoint)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(tidyverse)
library(projoint)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
data <- read_Qualtrics(
  system.file("extdata", "mummolo_nall_replication.csv", package = "projoint")
)

## -----------------------------------------------------------------------------
# Inspect the imported data:
data

## ----fig-setup, include=FALSE-------------------------------------------------
# Global default settings for all figures
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 5,
  fig.align = "center",
  dpi = 300  # Optional: high-resolution plots
)

# Helper functions for special figure sizes
narrow_fig <- function() list(fig.width = 5, fig.height = 4)
wide_fig <- function() list(fig.width = 8, fig.height = 5)
tall_fig <- function() list(fig.width = 6, fig.height = 7)

# Load libraries
library(projoint)
data(exampleData1, package = "projoint")
data(exampleData2, package = "projoint")
data(exampleData3, package = "projoint")
data(exampleData1_labelled_tibble, package = "projoint")
data(out1_arranged, package = "projoint")


## ----error=TRUE---------------------------------------------------------------
try({
outcomes <- paste0("choice", 1:8)
outcomes1 <- c(outcomes, "choice1_repeated_flipped")

out1 <- reshape_projoint(
  .dataframe = exampleData1,
  .outcomes = outcomes1,
  .choice_labels = c("A", "B"),
  .alphabet = "K",
  .idvar = "ResponseId",
  .repeated = TRUE,
  .flipped = TRUE
)
})

## -----------------------------------------------------------------------------
outcomes <- paste0("choice", 1:8)
outcomes2 <- c(outcomes, "choice1_repeated_notflipped")
out2 <- reshape_projoint(
  .dataframe = exampleData2,
  .outcomes = outcomes2,
  .repeated = TRUE,
  .flipped = FALSE
)

## -----------------------------------------------------------------------------
outcomes <- paste0("choice", 1:8)
out3 <- reshape_projoint(
  .dataframe = exampleData3,
  .outcomes = outcomes,
  .repeated = FALSE
)

## -----------------------------------------------------------------------------
fill_FALSE <- reshape_projoint(
  .dataframe = exampleData1,
  .outcomes = outcomes1,
  .fill = FALSE
)

fill_TRUE <- reshape_projoint(
  .dataframe = exampleData1,
  .outcomes = outcomes1,
  .fill = TRUE
)

## -----------------------------------------------------------------------------
selected_vars <- c("id", "task", "profile", "selected", "selected_repeated", "agree")
fill_FALSE$data[selected_vars]
fill_TRUE$data[selected_vars]

## -----------------------------------------------------------------------------
out4 <- make_projoint_data(
  .dataframe = exampleData1_labelled_tibble,
  .attribute_vars = c(
    "School Quality", "Violent Crime Rate (Vs National Rate)",
    "Racial Composition", "Housing Cost",
    "Presidential Vote (2020)", "Total Daily Driving Time for Commuting and Errands",
    "Type of Place"
  ),
  .id_var = "id",
  .task_var = "task",
  .profile_var = "profile",
  .selected_var = "selected",
  .selected_repeated_var = "selected_repeated",
  .fill = TRUE
)

## -----------------------------------------------------------------------------
out4

## ----eval=FALSE---------------------------------------------------------------
# save_labels(out1, "temp/labels_original.csv")

## ----eval=FALSE---------------------------------------------------------------
# data(out1_arranged, package = "projoint")

## -----------------------------------------------------------------------------
mm <- projoint(out1, .structure = "profile_level", .estimand = "mm")
plot(mm)

## -----------------------------------------------------------------------------
mm <- projoint(out1_arranged, .structure = "profile_level", .estimand = "mm")
plot(mm)

