# wafun

<!-- badges: start -->
[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/aswansyahputra/wafun/master?urlpath=rstudio)
<!-- badges: end -->

This repository stores materials for DQLab Data Mentoring webinar held by **DQLab X Komunitas R Indonesia X R-Ladies Jakarta**. You can participate by simply click `launch binder` badge above. No need to install anything.

You can also try the following method to replicate the repository into your local machine. Please run these codes in your R session:

```
install.packages(c("usethis", "devtools"))
usethis::use_course("aswansyahputra/wafun")

# after the new session is opened, run the following code to install all the dependencies
devtools::install_deps()
```

The WhatsApp group chats data is provided by Sarthak Nautiyal at Kaggle and can be downloaded from [here](https://www.kaggle.com/sarthaknautiyal/whatsappsample).

Note that you can also play with codes under `data-raw` directory. That directory contains raw data, mostly dirty, and codes that were used to clean them. Good luck!
