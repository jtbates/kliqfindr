# kliqfindr - KliqueFinder Algorithm in R

[![Build Status](https://travis-ci.org/jtbates/kliqfindr.svg?branch=master)](https://travis-ci.org/jtbates/kliqfindr)

## WARNING

This is pre-release software under development and is not intended for general use at this time.

## Installation

### General install instructions

1. Install the command line tools for your operating system according to [the instructions below](#operating-system-specific-install-instructions).
2. Install the [devtools](https://github.com/hadley/devtools) package. In R
```
> install.packages('devtools')
```
3. Install the current development version of kliqfindr from GitHub with devtools. In R
```
> devtools::install_github("jtbates/kliqfindr")
```
 
### Operating system-specific install instructions

#### Windows
1. Install Rtools from https://cran.r-project.org/bin/windows/Rtools/.

#### Mac OS X
1. 
    1. Install Xcode Command Line Tools. In the terminal
    ```
    $ xcode-select --install
    ```
    2. Install the latest version of GNU Fortran (6.1 as of Aug 5, 2019) from https://cran.r-project.org/bin/macosx/tools/.

#### Debian-based Linux
1. Install the r-base-dev package. In the terminal
```
$ sudo apt-get install r-base-dev
```
