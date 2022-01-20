# 0-data

This folder contains all the data associated to the project.

- The raw folder contains the raw data sent by colleagues to make the usms, or directly pre-calibrated usms (but some needed some work).
- The usms folder is a folder where all workspaces for the simulations are listed. They were optimized manually using the sole crops only.
- The usms-optim-beer folder is a folder to optimize the parameters using the beer-lambert law of light extinction. The light extinction coefficient optimized in this folder is then re-used for the radiative transfert option, in the case of the fall-back to the beer-lambert when plants height are close (see `hauteur_threshold`)
- the usms-optim-radiative folder is the same as the one before except it uses the radiative transfer to compute the light interception
- the `calibration.csv` file is used to define the calibration steps, which parameters are calibrated using which variable.
