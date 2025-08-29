# Decathlon Simulation

## Overview

Implementation of a Bayesian models for modeling and simulating the decathlon. The original data is from https://github.com/Battles186/DecathlonCareerBest. Key documents include the wrangling and scaling of the original decathlon data, functions to simulate decathlon performances under models with varying levels of granularity, examples of overall and inter-event decathlon analysis, and simulations of age curves for individual athletes.

## Stan Mods

The *stan_mods* folder contains stan code for each model combination discussed in the paper: baseline, simple, and compositional forms with cubic spline and polynomial basis expansions. Note that the baseline models are a subgroup of the simple models, only with total points as the target variable, rather than individual events. 

## Study

The *study* folder contains scripts necessary for model evaluation. Importantly, *decathlon_funs.R* contains functions to wrangle data and implement the stan models to produce draws from the posterior predictive for each model. Note that the experiments were run using a high performance computing cluster. To perform individual runs of a particular experiment, you will need to edit a single line in the *study.R* script (see Examples folder).

## Scripts

The *scripts* folder contains code relevant to the analysis of the experiments and simulations. We study the coverage and the predictive performance on held-out test sets of each model. We also analyze the optimal ages for each decathlon event. This folder also contains code to visualize these results.
