# 0-data

This folder contains all the data associated to the project.

- The `usms` folder contains all workspaces for the simulations, without any calibration (first guess from bibliography, observations or expertise).
- The usms-optim-beer folder contains the same USMs but optimized using the beer-lambert law of light extinction. The light extinction coefficient optimized in this folder is then re-used for the radiative transfert option, in the case of the fall-back to the Beer-Lambert option when plants height are close (see `hauteur_threshold`)
- the usms-optim-radiative folder is the same as the one before but calibrated using the radiative transfer option for the computation of the light interception
- the `calibration.csv` file is used to define the calibration steps, which parameters are calibrated using which variable, and their boundary values.
