# kliqfindr - KliqueFinder Algorithm in R

[![Build Status](https://travis-ci.org/jtbates/kliqfindr.svg?branch=master)](https://travis-ci.org/jtbates/kliqfindr)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## WARNING

This is pre-release software under development and is not intended for general use at this time.

## Installation

### General install instructions

This package is not on CRAN and does not yet have pre-compiled binaries. You will need to compile the Fortran code it depends on which requires installing command line tools outside of R if you do not already have them. You will need R 3.3.x or later.

1. Install the command line tools for your operating system.
    1. Windows
        1. Install the [Windows command line tools](https://cran.r-project.org/doc/manuals/R-admin.html#The-command-line-tools). Use the [Rtools35.exe](https://cran.r-project.org/bin/windows/Rtools/Rtools35.exe) installer from [Rtools downloads](https://cran.r-project.org/bin/windows/Rtools/).
    1. Mac OS X
        1. Install Xcode Command Line Tools. Use the following install command in the terminal
            ```
            $ xcode-select --install
            ```
            Only the Command Line Tools (220MB) are needed. A full install of Xcode (7.1GB) is not necessary.
        2. Install GNU Fortran for MacOS X from https://github.com/fxcoudert/gfortran-for-macOS/releases.
    1. Debian-based Linux
        1. Install the r-base-dev package. Use the following install command in the terminal
            ```
            $ sudo apt-get install r-base-dev
            ```
2. Install the [devtools](https://github.com/hadley/devtools) package. In R
    ```
    > install.packages('devtools')
    ```
3. Install the current development version of kliqfindr from GitHub with devtools. In R
    ```
    > devtools::install_github("jtbates/kliqfindr")
    ```
 
