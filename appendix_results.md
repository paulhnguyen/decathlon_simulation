## Appendix

These results were obtained from running the files located in the scripts folder on a high throughput computing center.

### Coverage Experiment

The following table contains the coverage results for the experiment with data simulated with known structure. Most linear relationships are well captured, with credible intervals containing the true parameter upwards of 90\% of the time. 

Coverage Table: Proportion of 95\% credible intervals containing the true parameter associated with corresponding predictor over 200 simulations. Entries are rounded to the second digit.

| predictor      | 100m | LJ   | SP   | HJ   | 400m | 110mH | DT   | PV   | JT   | 1500m |
|----------------|------|------|------|------|------|-------|------|------|------|-------|
| age            | 0.95 | 0.96 | 0.96 | 0.96 | 0.94 | 0.95  | 0.96 | 0.96 | 0.92 | 0.95  |
| $\text{age}^2$ | 0.94 | 0.94 | 0.96 | 0.92 | 0.96 | 0.96  | 0.95 | 0.96 | 0.96 | 0.98  |
| $\text{age}^3$ | 0.92 | 0.96 | 0.94 | 0.94 | 0.96 | 0.94  | 0.95 | 0.96 | 0.93 | 0.97  |
| 100m           | -    | 0.97 | 0.94 | 0.98 | 0.97 | 0.93  | 0.96 | 0.98 | 0.95 | 0.96  |
| LJ             | -    | -    | 0.97 | 0.94 | 0.96 | 0.93  | 0.94 | 0.95 | 0.94 | 0.96  |
| SP             | -    | -    | -    | 0.98 | 0.96 | 0.94  | 0.98 | 0.96 | 0.96 | 0.94  |
| HJ             | -    | -    | -    | -    | 0.97 | 0.94  | 0.96 | 0.96 | 0.96 | 0.98  |
| 400m           | -    | -    | -    | -    | -    | 0.93  | 0.95 | 0.96 | 0.96 | 0.96  |
| 110mH          | -    | -    | -    | -    | -    | -     | 0.95 | 0.93 | 0.92 | 0.96  |
| DT             | -    | -    | -    | -    | -    | -     | -    | 0.96 | 0.95 | 0.96  |
| PV             | -    | -    | -    | -    | -    | -     | -    | -    | 0.90 | 0.94  |
| JT             | -    | -    | -    | -    | -    | -     | -    | -    | -    | 0.94  |


### Real Data Bakeoff
The next two tables contain the results from the real decathlon data bakeoff experiments with randomly removed observations and tail observations.

#### General

general table: Mean standardized MSE across 10 cross validations in predicting decathlon performance with randomly removed observations.

| model and prior       | f(age) | 100m  | LJ    | SP    | HJ    | 400m  | 110mH | DT    | PV    | JV    | 1500m | points |
|-----------------------|--------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|--------|
| baseline default      | cubic  | -     | -     | -     | -     | -     | -     | -     | -     | -     | -     | 0.234  |
| simple default        | cubic  | 0.309 | 0.414 | 0.188 | 0.343 | 0.329 | 0.311 | 0.262 | 0.294 | 0.295 | 0.413 | 0.235  |
| simple moment         | cubic  | 0.309 | 0.414 | 0.188 | 0.343 | 0.329 | 0.312 | 0.262 | 0.294 | 0.294 | 0.413 | 0.235  |
| compositional default | cubic  | 0.309 | 0.414 | 0.188 | 0.344 | 0.329 | 0.312 | 0.262 | 0.294 | 0.294 | 0.414 | 0.235  |
| compositional moment  | cubic  | 0.309 | 0.413 | 0.188 | 0.344 | 0.329 | 0.312 | 0.262 | 0.294 | 0.294 | 0.413 | 0.235  |
| baseline default      | spline | -     | -     | -     | -     | -     | -     | -     | -     | -     | -     | 0.234  |
| simple default        | spline | 0.309 | 0.414 | 0.188 | 0.344 | 0.329 | 0.311 | 0.263 | 0.294 | 0.295 | 0.414 | 0.235  |
| simple moment         | spline | 0.312 | 0.420 | 0.190 | 0.347 | 0.353 | 0.312 | 0.263 | 0.295 | 0.295 | 0.415 | 0.236  |
| compositional default | spline | 0.309 | 0.414 | 0.188 | 0.344 | 0.329 | 0.312 | 0.262 | 0.294 | 0.295 | 0.414 | 0.235  |
| compositional moment  | spline | 0.312 | 0.419 | 0.189 | 0.344 | 0.345 | 0.313 | 0.263 | 0.295 | 0.295 | 0.417 | 0.238  |



#### Tail

tail table: Mean standardized MSE across 10 cross validations tail-removed observations for decathletes.


| model and prior       | f(age) | 100m  | LJ    | SP    | HJ    | 400m  | 110mH | DT    | PV    | JV    | 1500m | points |
|-----------------------|--------|-------|-------|-------|-------|-------|-------|-------|-------|-------|-------|--------|
| baseline default      | cubic  | -     | -     | -     | -     | -     | -     | -     | -     | -     | -     | 0.358  |
| simple default        | cubic  | 0.337 | 0.471 | 0.238 | 0.414 | 0.406 | 0.447 | 0.301 | 0.372 | 0.315 | 0.482 | 0.362  |
| simple moment         | cubic  | 0.337 | 0.471 | 0.238 | 0.414 | 0.406 | 0.446 | 0.301 | 0.372 | 0.315 | 0.482 | 0.362  |
| compositional default | cubic  | 0.337 | 0.469 | 0.239 | 0.415 | 0.407 | 0.445 | 0.300 | 0.371 | 0.315 | 0.482 | 0.362  |
| compositional moment  | cubic  | 0.337 | 0.469 | 0.239 | 0.415 | 0.407 | 0.445 | 0.300 | 0.371 | 0.315 | 0.482 | 0.362  |
| baseline default      | spline | -     | -     | -     | -     | -     | -     | -     | -     | -     | -     | 0.359  |
| simple default        | spline | 0.338 | 0.470 | 0.238 | 0.415 | 0.409 | 0.448 | 0.301 | 0.371 | 0.317 | 0.484 | 0.363  |
| simple moment         | spline | 0.344 | 0.476 | 0.238 | 0.433 | 0.499 | 0.451 | 0.301 | 0.370 | 0.316 | 0.481 | 0.366  |
| compositional default | spline | 0.337 | 0.469 | 0.239 | 0.415 | 0.408 | 0.445 | 0.300 | 0.369 | 0.315 | 0.482 | 0.363  |
| compositional moment  | spline | 0.344 | 0.474 | 0.238 | 0.420 | 0.477 | 0.457 | 0.300 | 0.370 | 0.316 | 0.492 | 0.373  |


### Emperical and posterior predictive correlations between decathlon events

The empirical correlation table contains the emperical correlations between the decathlon events from the observed data. The experimental simple and compositional correlation tables contain the 2.5\% and 97.5\% quantiles from the posterior predictive correlations from the simulated datasets generated by the simple and compositional models respectively.

Empirical Correlations between events, rounded to two digits:

| Event | 100m | LJ    | SP    | HJ    | 400m  | 110mH | DT    | PV    | JT    | 1500m |
|-------|------|-------|-------|-------|-------|-------|-------|-------|-------|-------|
| 100m  | 1.00 | -0.54 | -0.20 | -0.20 | 0.66  | 0.52  | -0.18 | -0.25 | -0.13 | 0.10  |
| LJ    | -    | 1.00  | 0.33  | 0.43  | -0.45 | -0.47 | 0.29  | 0.34  | 0.24  | -0.14 |
| SP    | -    | -     | 1.00  | 0.31  | -0.15 | -0.36 | 0.73  | 0.38  | 0.51  | -0.01 |
| HJ    | -    | -     | -     | 1.00  | -0.21 | -0.34 | 0.28  | 0.29  | 0.22  | -0.10 |
| 400m  | -    | -     | -     | -     | 1.00  | 0.46  | -0.13 | -0.25 | -0.11 | 0.46  |
| 110mH | -    | -     | -     | -     | -     | 1.00  | -0.32 | -0.37 | -0.24 | 0.12  |
| DT    | -    | -     | -     | -     | -     | -     | 1.00  | 0.40  | 0.48  | -0.02 |
| PV    | -    | -     | -     | -     | -     | -     | -     | 1.00  | 0.31  | -0.20 |
| JT    | -    | -     | -     | -     | -     | -     | -     | -     | 1.00  | -0.09 |
| 1500m | -    | -     | -     | -     | -     | -     | -     | -     | -     | 1.00  |


#### Simple Model posterior correlations

2.5\% and 97.5\% quantiles for posterior predictive correlation between decathlon events from 2000 simulated datasets from the simple model, rounded to 2 digits.

| Event | LJ           | SP           | HJ           | 400m         | 110mH        | DT           | PV           | JT           | 1500m        |
|-------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|
| 100m  | -0.41, -0.37 | -0.16, -0.13 | -0.16, -0.12 | 0.49, 0.52   | 0.37, 0.41   | -0.15, -0.12 | -0.19, -0.16 | -0.11, -0.07 | 0.03, 0.07   |
| LJ    | 1, 1         | 0.24, 0.28   | 0.29, 0.33   | -0.35, -0.31 | -0.37, -0.33 | 0.23, 0.26   | 0.24, 0.28   | 0.16, 0.2    | -0.1, -0.06  |
| SP    | -            | 1, 1         | 0.25, 0.28   | -0.12, -0.08 | -0.33, -0.29 | 0.64, 0.67   | 0.32, 0.35   | 0.44, 0.47   | -0.01, 0.03  |
| HJ    | -            | -            | 1, 1         | -0.17, -0.13 | -0.28, -0.25 | 0.23, 0.26   | 0.22, 0.26   | 0.17, 0.2    | -0.07, -0.03 |
| 400m  | -            | -            | -            | 1, 1         | 0.31, 0.35   | -0.11, -0.07 | -0.2, -0.16  | -0.09, -0.05 | 0.28, 0.32   |
| 110mH | -            | -            | -            | -            | 1, 1         | -0.29, -0.25 | -0.31, -0.27 | -0.21, -0.18 | 0.03, 0.08   |
| DT    | -            | -            | -            | -            | -            | 1, 1         | 0.32, 0.36   | 0.41, 0.44   | -0.02, 0.02  |
| PV    | -            | -            | -            | -            | -            | -            | 1, 1         | 0.24, 0.28   | -0.16, -0.12 |
| JT    | -            | -            | -            | -            | -            | -            | -            | 1, 1         | -0.06, -0.02 |
| 1500m | -            | -            | -            | -            | -            | -            | -            | -            | 1, 1         |


#### Compositional Model posterior correlations

2.5\% and 97.5\% quantiles for posterior predictive correlation between decathlon events from 2000 simulated datasets from the compositional model, rounded to 2 digits.

| Event | LJ           | SP           | HJ           | 400m         | 110mH        | DT           | PV           | JT           | 1500m        |
|-------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|--------------|
| 100m  | -0.56, -0.52 | -0.22, -0.18 | -0.22, -0.18 | 0.64, 0.67   | 0.5, 0.53    | -0.19, -0.16 | -0.26, -0.23 | -0.15, -0.11 | 0.09, 0.13   |
| LJ    | 1, 1         | 0.3, 0.34    | 0.39, 0.43   | -0.47, -0.43 | -0.48, -0.45 | 0.26, 0.3    | 0.31, 0.35   | 0.21, 0.26   | -0.17, -0.12 |
| SP    | -            | 1, 1         | 0.29, 0.32   | -0.16, -0.13 | -0.37, -0.34 | 0.71, 0.74   | 0.36, 0.39   | 0.49, 0.52   | -0.03, 0.01  |
| HJ    | -            | -            | 1, 1         | -0.23, -0.19 | -0.35, -0.31 | 0.25, 0.29   | 0.27, 0.3    | 0.2, 0.24    | -0.13, -0.08 |
| 400m  | -            | -            | -            | 1, 1         | 0.44, 0.47   | -0.15, -0.11 | -0.27, -0.23 | -0.14, -0.1  | 0.43, 0.47   |
| 110mH | -            | -            | -            | -            | 1, 1         | -0.34, -0.3  | -0.38, -0.35 | -0.26, -0.22 | 0.1, 0.14    |
| DT    | -            | -            | -            | -            | -            | 1, 1         | 0.37, 0.41   | 0.46, 0.49   | -0.04, 0     |
| PV    | -            | -            | -            | -            | -            | -            | 1, 1         | 0.29, 0.32   | -0.22, -0.17 |
| JT    | -            | -            | -            | -            | -            | -            | -            | 1, 1         | -0.12, -0.08 |
| 1500m | -            | -            | -            | -            | -            | -            | -            | -            | 1, 1         |


