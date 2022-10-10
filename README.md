# A Shiny app for visualizing how the one-sample t-test works

**Author: Markus Konrad <post@mkonrad.net>**

This is still a prototype that needs some fine-tuning, documentation, explanation and tests.

## Dependencies

### Installing dependencies using *renv*

This project employs [renv](https://blog.rstudio.com/2019/11/06/renv-project-environments-for-r/) for managing dependencies. If you haven't installed renv yet, please do so by running `install.packages('renv')`. After that, open this RStudio project and run:

```
renv::restore()
```

This will install all required packages in an isolated package environment for the project, i.e. it will not mess with your existing R package versions. Every time you open this RStudio project, this package environment will be used.
