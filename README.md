
# vvtermtime

<!-- badges: start -->
[![R-CMD-check](https://github.com/vusaverse/vvtermtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vusaverse/vvtermtime/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/vvtermtime)](https://CRAN.R-project.org/package=vvtermtime/)
<a href="https://diffify.com/R/vvtermtime" target="_blank"><img src="https://diffify.com/diffify-badge.svg" alt="The diffify page for the R package vvtermtime" style="width: 100px; max-width: 100%;"></a>
[![CRAN last month downloads](https://cranlogs.r-pkg.org/badges/last-month/vvtermtime?color=green/)](https://cran.r-project.org/package=vvtermtime/)
[![CRAN last month downloads](https://cranlogs.r-pkg.org/badges/grand-total/vvtermtime?color=green/)](https://cran.r-project.org/package=vvtermtime/)

<!-- badges: end -->

The goal of `vvtermtime` is to provide functionalities to interact with the Semestry Timetabling Software API. The Semestry Timetabling Software is used by universities and educational institutions in creating efficient and effective schedules for their academic activities.

`vvtermtime` allows you to leverage the capabilities of the Semestry Timetabling Software directly from your R workflow.


# Getting Started:

Install the vvtermtime package from GitHub using the following command:

```
install.packages("devtools")
devtools::install_github("vusaverse/vvtermtime")
```

Load the vvtermtime package in your R script:

```
library(vvtermtime)
```

Obtain your Semestry API key and base URL from the Semestry Timetabling Software provider at your institution.

Create a Semestry object using the `authenticate()` function. Replace your_api_key with your actual API key and https://api.semestry.com with the appropriate base URL:

```
semestry <- authenticate(base_url = "https://api.semestry.com", api_key = "your_api_key")
```

Now you are able to use the methods in this package. For example to retrieve the bookings for your institution, pass the `semestry` object to the `get_roombookings()` function, as shown below:

```
Bookings <- get_roombookings(semestry)

```

Please refer to the package documentation for more detailed information on how to use the vvtermtime package and its functions.


For more information about Semestry and their timetabling software, please visit their website: https://semestry.com/solutions/termtime/
