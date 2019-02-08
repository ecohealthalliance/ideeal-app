# IDEEAL: Infectious Disease Emergence and Economics of Altered Landscapes

## How to run this Shiny App
1. git clone this repo
2. open server.R and ui.R in RStudio
3. click "Run App" OR run the below code in your console
```{r}
shiny::runApp()
```
4. Make sure you have all packages downloaded

### Project Goal:
1. Understand the benefits of forest conservation to regulate diseases
2. Estimate the economic costs from infecitous diseases due to land degradation

[IDEEAL website](https://www.ecohealthalliance.org/program/ideeal)

[EcoHealth Alliance](https://ecohealthalliance.org)

---

### Docker deployment

Build and run the app with

    docker build -t ideeal-app . && docker run -d -p 3838:3838 ideeal-app

And visit the app at `localhost:3838`