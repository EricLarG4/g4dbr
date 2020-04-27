# g4dbr
_G4 biophysics database visualization and management_

---

  - [Installation](#Installation)
  - [Use](#Use)
  - [License](#License)

## Installation

To install, download the zip archive then in R run:

![Download g4dbr](readme.PNG)

```{r install}
install.packages("devtools")

devtools::install_local("XXX/g4dbr-master.zip")
```

Where `XXX` is the filepath of the zip archive. Use slashes `/` rather than backslashes `\` even on Windows.

You may update some or all of the packages that were already installed or skip this step.

![Package updates](readme2.PNG)

## Use

To use g4dbr, run:

```{r use}
library(g4dbr)

g4db()
```

## License

GPL-3 [Eric Largy](https://github.com/EricLarG4)
