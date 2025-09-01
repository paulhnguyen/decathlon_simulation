# Decathlon Simulation

## Overview

Implementation of a Bayesian models for modeling and simulating the decathlon. The original data is from https://github.com/Battles186/DecathlonCareerBest. Key documents include the wrangling and scaling of the original decathlon data, functions to simulate decathlon performances under models with varying levels of granularity, examples of overall and inter-event decathlon analysis, and simulations of age curves for individual athletes.

## Stan Mods

The *stan_mods* folder contains stan code for each model combination discussed in the paper: baseline, simple, and compositional forms with cubic spline and polynomial basis expansions. Note that the baseline models are a subgroup of the simple models, only with total points as the target variable, rather than individual events. 

### Models

Below, we briefly describe the baseline, simple, and compositional models. 

#### Baseline

Let $i$ denote the athlete, and $j$ the decathlon observation. For each decathlon $j$, we observe the age of the athlete, vector of events, $\bm{X}_{i,j}$, and total number of points, $P_{i,j}$. We standardize each variable to have zero mean and standard deviation of one. We can now directly model total decathlon points with our baseline model, using only age as our sole covariate:

```{=latex}
\begin{equation}
    \label{eq:gen_mod}
    \begin{gathered}
        P_{i,j} = \alpha_i + \sum_d \beta_d \cdot \phi_d(X_{{age}_{i,j}})  + \epsilon_{i,j}, \quad \epsilon_{i,j} \sim \normaldist{0}{\sigma^2}
    \end{gathered}
\end{equation}
```

with $\alpha_i$ a random intercept for each athlete. We let $\phi$ be a fixed basis. In our experiments, we test both a cubic spline basis with interior knots placed at age deciles and a simple cubic polynomial basis.

#### Simple

In our simple models, we predict each event separately and compute total points from the individual predictions. 
Again, let $i$ denote the athlete, $j$ the decathlon observation, and $e$ the individual event.
For each of the ten decathlon events, we model individual event scores using $X_{age}$ as a covariate, independent of the other decathlon events.
We then calculate the number of points earned for a given event score with $G_e(\cdot)$, whose formula is described later. We sum the points earned for each event to determine the overall decathlon score. 

```{=latex}
\begin{equation}
    \label{eq:simple_mod}
    \begin{gathered}
        Y_{i, j, e} = \alpha_{i,e} + \sum_d \beta_{d,e} \cdot \phi_{d}(X_{{age}_{i,j}})  + \epsilon_{i,j,e}, \quad \epsilon_{i,j,e} \sim \normaldist{0}{\sigma_e^2} \\
        P_{i,j} = \sum_{e = 1}^{10} G_e(Y_{i,j,e})
    \end{gathered}
\end{equation}
```

Now, $\alpha_{i,e}$ represents a random intercept for each athlete specific to event $e$. Again, we use the cubic spline and cubic polynomial basis expansion for $\phi$.

#### Compositional

In the compositional models, we take advantage of the fact that the order of events in the decathlon is fixed and predetermined. 
Unlike the simple models, where we model individual events independently, in our compositional model, we model individual events using age \emph{and all previous events}. 

```{=latex}
\begin{equation}
    \label{eq:compositional_mod}
    \begin{gathered}
        Y_{i, j, e} = \alpha_{i,e} + \sum_d \beta_{d,e} \cdot \phi_{d}(X_{{age}_{i,j}}) + \sum_{m = 1}^{e-1} \gamma_{m,e} \cdot Y_{i,m}  + \epsilon_{i,j,e}, \quad \epsilon_{i,j,e} \sim \normaldist{0}{\sigma_e^2} \\
        P_{i,j} = \sum_{e = 1}^{10} G_e(Y_{i,j,e})
    \end{gathered}
\end{equation}
```

#### Point Calculation.

We use the following table and point formulae, given by World Athletics. https://worldathletics.org/about-iaaf/documents/technical-information

The formula for calculating points for each individual event is given below:

```{=latex}
\begin{equation}
    \label{eq:point_calc}
    \text{Points} = 
    \begin{cases}

        a \cdot (b-x)^c& \text{if track event} \\
    
        a \cdot (x-b)^c & \text{if field event}
    
    \end{cases}
\end{equation}
```

where $a$, $b$, and $c$ are given by \cref{tab:point_params}, and $x$ is the athlete's time, distance, or height.

```{=latex}
\begin{table}
    \caption{\label{tab:point_params}Parameter values by event for point calculation }
    \centering
    \begin{tabular}[t]{lrrr}
    \toprule
    Event & a & b & c\\
    \midrule
    100 m & 25.435 & 18.0 & 1.81\\
    Long jump & 0.144 & 220.0 & 1.40\\
    Shot put & 51.390 & 1.5 & 1.05\\
    High jump & 0.847 & 75.0 & 1.42\\
    400 m & 1.538 & 82.0 & 1.81\\
    \addlinespace
    110 m hurdles & 5.744 & 28.5 & 1.92\\
    Discus throw & 12.910 & 4.0 & 1.10\\
    Pole vault & 0.280 & 100.0 & 1.35\\
    Javelin throw & 10.140 & 7.0 & 1.08\\
    1500 m & 0.038 & 480.0 & 1.85\\
    \bottomrule
    \end{tabular}
\end{table}
```

## Study

The *study* folder contains scripts necessary for model evaluation. Importantly, *decathlon_funs.R* contains functions to wrangle data and implement the stan models to produce draws from the posterior predictive for each model. Note that the experiments were run using a high performance computing cluster. To perform individual runs of a particular experiment, you will need to edit a single line in the *study.R* script (see Examples folder).

## Scripts

The *scripts* folder contains code relevant to the analysis of the experiments and simulations. We study the coverage and the predictive performance on held-out test sets of each model. We also analyze the optimal ages for each decathlon event. This folder also contains code to visualize these results.
