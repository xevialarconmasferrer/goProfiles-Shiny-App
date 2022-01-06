

Packages required:

goProfiles
shiny
shinythemes
dplyr
BiocManager
org.Hs.eg.db
org.Mm.eg.db
org.Gg.eg.db
org.EcK12.eg.db
tidyverse
readr

!! IMPORTANT: the shiny theme used sometimes hide the browser button at the left limit of the browser widget on the "DATA" page. You need to click the left limit of the browser.
In case you can't find it, copy the code without the www file or the bootsrap file and run the aplication on basic shiny.

Shiny theme:

I used a different theme (bootstrap) to stilish the aplication from the "shinythemes" package. In order to upgrade the appereance of the app with this bootsrap you just need to include in the folder where you have the app code, the folder "www" from the repository. Or create a new folder cold "www" in the same folder as you have the app, and include the file bootstrap.min3.css in this folder. You can download the file bootstrap.min3.css from the repository.

I included to lists of gene identificators for you to compare as an example of use.
