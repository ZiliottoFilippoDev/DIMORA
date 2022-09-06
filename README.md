[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![GPL][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]

# DIMORA
R library for diffusion model analysis. (**DI**ffusion **MO**dels **R** **A**nalysis) is a statistical package that allows the analysis of diffusion data using different models, among the most used and useful. The need for this package arises from the lack of a similar tool in R, the idea is therefore to provide a platform that allows the most in-depth analysis of the diffusion data.

## Getting started

The implemented methods are: `Standard Bass model`, `Generalized Bass model` (with rectangular shock, exponential shock, and mixed shock. You can choose to add from 1 to 3 shocks), `Guseo-Guidolin model` and Variable Potential Market model, and `UCRCD model`. The Bass model consists of a simple differential equation that describes the process of how new products get adopted in a population, the Generalized Bass model is a generalization of the Bass model in which there is a "carrier" function x(t) that allows to change the speed of time sliding. In some real processes the reachable potential of the resource available in a temporal instant may appear to be not constant over time, because of this we use Variable Potential Market model, in which the Guseo-Guidolin has a particular specification for the market function. The UCRCD model (Unbalanced Competition and Regime Change Diachronic) is a diffusion model used to capture the dynamics of the competitive or collaborative transition.

## Install
From Rstudio or Jupyter notebook
```R
install.packages('DIMORA') 
library(DIMORA)
```

Or cloning the Github repository to access files

```bash
$ git clone https://github.com/ZiliottoFilippoDev/DIMORA.git
$ cd R
```

## Usage
Bass model usage example

```R
data(DBdimora)
data <- DBdimora$iPhone[7:52]
model <- BM(data, display=T)
plot(model, type = 'all', oos = 20)
```

![Plot example](Rplot.png)

## References
Guidolin, M. (2023). Innovation Diffusion Models: Theory and Practice, First Edition. John Wiley & Sons Ltd.

[CRAN package](https://CRAN.R-project.org/package=DIMORA)
