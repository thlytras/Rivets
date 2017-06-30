### Rivets - A collection of useful standalone R functions

This package contains a loose collection of functions that I use to make my life a
tiny bit easier. I found it easier to maintain them in a single package than in scattered R
scripts here and there. Some of these functions are intended for epidemiologists like me,
but others are dual- or general-purpose and can be used by anyone.

For this reason the functions are standalone, i.e. if you want to use just a single function
you can just copy/paste or include() the respective .R script file from the package source, 
instead of installing the whole package.

**Installation** (of the whole package)

Open R, and give:

      devtools::install_git("https://github.com/thlytras/Rivets.git")

If you do not have the package "devtools", first install it from CRAN with:

      install.packages("devtools")

