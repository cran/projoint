## ----setup, include=FALSE-----------------------------------------------------
library(projoint)
library(dplyr)
library(ggplot2)
library(patchwork)

## -----------------------------------------------------------------------------
# Standard order; repeated = task 1
data("exampleData1")
outcomes <- c(paste0("choice", 1:8), "choice1_repeated_flipped")
out1 <- reshape_projoint(exampleData1, outcomes)

## -----------------------------------------------------------------------------
out1$labels

## -----------------------------------------------------------------------------
qoi <- set_qoi(
  .structure = "choice_level",
  .att_choose = "att1",
  .lev_choose = "level3",
  .att_notchoose = "att1",
  .lev_notchoose = "level1"
)

mm2 <- projoint(out1, qoi)
print(mm2)
summary(mm2)

## -----------------------------------------------------------------------------
qoi <- set_qoi(
  .structure = "choice_level",
  .estimand = "amce",
  .att_choose = "att1",
  .lev_choose = "level3",
  .att_notchoose = "att1",
  .lev_notchoose = "level1",
  .att_choose_b = "att1",
  .lev_choose_b = "level2",
  .att_notchoose_b = "att1",
  .lev_notchoose_b = "level1"
)

amce2 <- projoint(out1, qoi)
print(amce2)
summary(amce2)

## -----------------------------------------------------------------------------
data(out1_arranged)
predicted_irr <- predict_tau(out1_arranged)

print(predicted_irr)
summary(predicted_irr)
plot(predicted_irr)

## ----fig-setup, include=FALSE-------------------------------------------------
# Global default settings for all figures
knitr::opts_chunk$set(
  fig.width = 6,
  fig.height = 3,
  fig.align = "center",
  dpi = 300  # Optional: high-resolution plots
)

# Helper functions for special figure sizes
narrow_fig <- function() list(fig.width = 5, fig.height = 4)
wide_fig <- function() list(fig.width = 8, fig.height = 5)
tall_fig <- function() list(fig.width = 6, fig.height = 7)


## ----echo=FALSE---------------------------------------------------------------
library(projoint)
library(ggplot2)

data(out1_arranged, package = "projoint")

## -----------------------------------------------------------------------------
qoi_mm <- set_qoi(
  .structure = "choice_level", # default
  .att_choose = "att1", 
  .lev_choose = "level1", 
  .att_notchoose = "att1", 
  .lev_notchoose = "level3"
)

## -----------------------------------------------------------------------------
choice_mm <- projoint(
  .data = out1_arranged, 
  .qoi = qoi_mm, 
  .ignore_position = TRUE
)

## ----fig.width = 6, fig.height = 3--------------------------------------------
plot(choice_mm)

## ----fig.width = 8, fig.height = 2--------------------------------------------
plot(choice_mm, .type = "pointrange")

