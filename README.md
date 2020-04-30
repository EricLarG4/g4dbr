# g4dbr
_G4 biophysics database visualization and management_

  + [Installation](#Installation)
  + [Use](#Use)
  + [License](#License)

## Installation

### Re-installation

If a previous version has been installed, the previous can be removed using `remove.packages('g4dbr')`, although this is not necessary.

It is also not necessary to reinstall devtools.

### Download-less installation

For the first installation, [create a private access token](https://github.com/settings/tokens).

In R (e.g. the console of RStudio), run:

```{r install_online}
install.packages("devtools")
devtools::install_github('g4db-team/g4dbr', auth_token = 'XXX', build_vignettes = T, build_manual = T)
```
Where `XXX` is your token (40 characters).

### Zip-download installation

To install, download the zip archive.

![Download g4dbr](man/ressources/readme.PNG)

In R (e.g. the console of RStudio), run:

```{r install}
install.packages("devtools")
devtools::install_local("XXX/g4dbr-master.zip")
```

Where `XXX` is the filepath of the zip archive. Use slashes `/` rather than backslashes `\` even on Windows.

You may update some or all of the packages that were already installed or skip this step.

![Package updates](man/ressources/readme2.PNG)

## Use

### g4db

To use g4db, run:

```{r use}
library(g4dbr)
g4db()
```

#### Download-less installation

An example database (db.xlsx) a demo input file (demo_input.xlsx) are located in the extdata subfolder of your package installation path, e.g. `C:\Users\username\Documents\R\win-library\4.0\g4dbr`. **These files will be overridden if the package is re-installed, and removed is the package in uninstalled.** 

#### Zip-based installation

The zip file contains the database (db.xlsx) and a demo input file (demo_input.xlsx) in the inst/extdata subfolder. To use these files extract them from the archives then load into the g4db interface, in the _database_ and _ImportR_ tabs respectively.

### Standalone extinction coefficient calculation

To use epsilon.calculator, run:

```{r use}
library(g4dbr)
epsilon.calculator("SEQUENCE")
```
where `SEQUENCE` is the DNA sequence of choice.

## License

GPL-3 [Eric Largy](figures/https://github.com/EricLarG4)
