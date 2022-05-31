# FCRE_LER-forecast-code

Run FLARE forecasts using [LakeEnsemblR](https://github.com/tadhg-moore/LakeEnsemblR/tree/flare) for Falling Creek Reservoir, VA, USA.
<img src="https://raw.githubusercontent.com/tadhg-moore/LakeEnsemblR/flare/images/logo.png" alt="LakeEnsemblR logo" align="right" height="220" width="220">

To run these forecasts and analyses on your computer you will need to clone this repository onto your computer.

After the project has cloned, then you will need to install the necessary R packages, pull in the raw data from the sensors at Falling Creek Reservoir, process the data, and then run the forecasts. The scripts to run this forecasting workflow are located in `workflows/ler-ms`.

To run the forecast for GLM you will need to execute the following scripts:
- install_packages.R
- 01_combined_paper_workflow_GLM.R
