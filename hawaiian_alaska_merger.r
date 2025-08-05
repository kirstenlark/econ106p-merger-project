---
title: "Final Project: Hawaiian-Alaska Airlines Merger Analysis"
author: "Mariana Barkhordar, Kirsten Morris, and Lynna Sooc"
date: "`r Sys.Date()`"
output:
  pdf_document:
    number_sections: true
    toc: true
    toc_depth: 2
    latex_engine: xelatex
    keep_tex: true
    fig_caption: true
    highlight: tango
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

library(tidyverse)
library(kableExtra)
library(wesanderson)
library(scales)
library(AER)
```

# Industry Overview

## Financial Position of Alaskan and Hawaiian Airlines in 2024

```{r financial-table, results='asis'}
financials <- data.frame(
  Metric = c(
    "Adjusted EBITDA",
    "Net Income / Loss",
    "Total Revenue",
    "Unrestricted Cash & Investments",
    "Debt / Capitalization Ratio",
    "Outstanding Debt & Lease Obligations",
    "Employees"
  ),
  `Alaska Airlines` = c(
    "$95 million",
    "–$132 million",
    "$2.2 billion",
    "$2.3 billion",
    "47%",
    "N/A",
    "22,500"
  ),
  `Hawaiian Airlines` = c(
    "–$116 million",
    "–$2.65 per share",
    "$661 million",
    "$1.2 billion",
    "91%",
    "$1.5 billion",
    "7,000"
  )
)

kable(financials, caption = "Financial Overview of Alaska and Hawaiian Airlines (2024)") %>%
  kable_styling(full_width = FALSE, position = "center")
```

# HHI Analysis

## Load and Inspect Market Data

```{r read-data}
data <- read_csv("db1bmarket.csv")

# Filter for routes where both carriers are present
routes_with_both <- data %>%
  group_by(mkt) %>%
  summarise(carriers = n_distinct(rpcarrier)) %>%
  filter(carriers > 1)

nrow(routes_with_both)
```

## Compute Pre-Merger HHI

```{r hhi-pre-post}
data <- data %>%
  group_by(mkt, rpcarrier) %>%
  mutate(s_j = q / sum(q)) %>%
  ungroup()

hhi_pre <- data %>%
  group_by(mkt) %>%
  summarise(hhi_pre = sum(10000 * s_j^2))

summary(hhi_pre$hhi_pre)
```

## Compute Post-Merger HHI

```{r hhi-post}
data_post <- data %>%
  mutate(rpcarrier_merged = ifelse(rpcarrier %in% c("AS", "HA"), "Merged", rpcarrier))

hhi_post <- data_post %>%
  group_by(mkt, rpcarrier_merged) %>%
  summarise(q = sum(q)) %>%
  mutate(s_j = q / sum(q)) %>%
  summarise(hhi_post = sum(10000 * s_j^2))

hhi_combined <- left_join(hhi_pre, hhi_post, by = "mkt") %>%
  mutate(hhi_delta = hhi_post - hhi_pre)

summary(hhi_combined$hhi_delta)
```

# Marginal Costs, Markups, and Lerner Index

```{r mc-lerner}
data <- data %>%
  mutate(ms = q / sum(q))

beta_iv <- -1.984  # from DC3

data <- data %>%
  mutate(
    elasticity_iv = beta_iv * (1 - ms),
    lerner_iv = -1 / elasticity_iv,
    markup_ratio = 1 / (1 - lerner_iv),
    mc = mktfare * (1 + 1 / elasticity_iv)
  )

summary(data$mc)
summary(data$lerner_iv)
summary(data$markup_ratio)
```

# Post-Merger Marginal Cost Assumption

```{r mc-assumption}
# Example: Weighted average based on number of tickets sold
mc_assumed <- data %>%
  filter(rpcarrier %in% c("AS", "HA")) %>%
  group_by(rpcarrier) %>%
  summarise(mean_mc = mean(mc, na.rm = TRUE), total_q = sum(q)) %>%
  mutate(weight = total_q / sum(total_q))

mc_post_merged <- sum(mc_assumed$mean_mc * mc_assumed$weight)
mc_post_merged
```

# Diversion Ratio and Indicative Price Rise (IPR)

```{r ipr-ce}
data_div <- data %>%
  filter(rpcarrier %in% c("AS", "HA")) %>%
  group_by(mkt) %>%
  summarise(
    diversion = prod(ms),
    L = mean(lerner_iv)
  ) %>%
  mutate(ipr = (diversion * L) / (1 - diversion - L))

summary(data_div$ipr)

# Compensating Efficiency
ce_required <- data_div %>%
  mutate(ce = ipr / (1 + ipr))

summary(ce_required$ce)
```

# Conclusion

We find that under the assumptions used, the merger leads to X% average price increase, and Y% CE is required to offset it. Based on this, we recommend [...]
