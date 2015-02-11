# Performance testing SS3 with autocorrelated recruitment
This repository is designed to house and allow for group editing of all files related to this project.

# General requests for editing etiquette
Please upload any edited files to this reposity when your edits are complete.

Include your initials in the comments section of the code along with date of your edit and a brief summary of the changes you made as well as including this information on your "commit" update descriptions.

# Word documents
GitHub does not show line-by-line editorial changes for Word Documents, so please have "track changes" feature enabled when you edit the report/paper file.

# Instructions for installing the correct version of ss3sim
You will need to install a new version of ss3sim in order to properly run the code that produces the plots and output files.  To do this, use 

  devtools::install.github("ss3sim/ss3sim","master")

in R GUI.  Note that this will overwrite the current install of ss3sim, so you should move any user-generated files stored in the ss3sim library folder to another location before reinstalling ss3sim.

You will need to download two files located here: https://www.dropbox.com/sh/zg0sec6j20sfyyz/AACQiuk787qW882U2euVKoPna

You will also need to change your file path.  To do this, consult the vignette for ss3sim available here: http://cran.r-project.org/web/packages/ss3sim/vignettes/ss3sim-vignette.pdf

You should be able to run the code files at this point.  If you have any difficulty doing so, please let me, Elizabeth, or Kelli know.
