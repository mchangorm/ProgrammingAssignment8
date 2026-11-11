---
title: "Statistical Inference Project"
author: "Mark Chang"
date: "2026-11-11"
output:
  html_document:
    keep_md: true
---

## Introduction / Synopsis

The project consists of two parts:

- A simulation exercise.
- Basic inferential data analysis.

## Data

## Initialisation

Set to show R code


``` r
library(data.table)
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)
opts_chunk$set(echo=TRUE)
```

## Assignment Part 1

### Instructions

1. Show the sample mean and compare it to the theoretical mean of the distribution.
2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
3. Show that the distribution is approximately normal.

### Data Processing

#### Calculations

We first set our parameters

``` r
set.seed(44)
lambda <- 0.2
n <- 40
nosim <- 1000
```
We simulate the exponential distribution and take the sample means.

``` r
sample_simulated <- replicate(nosim,rexp(n,lambda))
sample_means <- apply(sample_simulated, 2, mean)
```

#### Answers

Q. Show the sample mean and compare it to the theoretical mean of the distribution.

``` r
sample_mean <- mean(sample_means)
theoretical_mean <- 1/lambda
```
A. The sample mean is 4.9837334 and the theoretical mean is 5

Q. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

``` r
sample_sd <- sd(sample_means)
sample_variance <- sample_sd^2

theoretical_sd <- (1/lambda)/sqrt(n)
theoretical_variance <- ((1/lambda)/sqrt(n))^2
```
A. The sample variance is 0.6104049 and the theoretical variance is 0.625. The sample SD is 0.7812841 and the theoretical sd is 0.7905694.

Q. Show that the distribution is approximately normal.

``` r
hist(sample_means, breaks=n, probability=TRUE, main = "Distribution of Exponetial Sample Means", xlab = "Sample Means")

x_axis <- seq(min(sample_means),max(sample_means),length=100)
y_axis <- dnorm(x_axis, mean = theoretical_mean, sd = theoretical_sd)
lines(x_axis,y_axis,lwd=2)
```

![](project_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

A. Due to the central limit theorem (CLT), the distribution of averages of 40 exponentials is very close to a normal distribution.


## Assignment Part 2

### Instructions

1. Load the ToothGrowth data and perform some basic exploratory data analyses
2. Provide a basic summary of the data.
3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
4. State your conclusions and the assumptions needed for your conclusions.

### Data Processing



