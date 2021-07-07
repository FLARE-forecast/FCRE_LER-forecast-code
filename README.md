# LAKE-forecast-code

To run this example it is recommended to git clone this project using RStudio's "New Project..." feature. Click "File > New Project... > Version Control > Git " and input the url from this repository in "Repository URL".

After the project has cloned, then you will need to pull in the raw data which is streaming in near real-time from the sensors at Falling Creek Reservoir, VA, USA to GitHub.

To do this you will need to run the "01_get_data.R" which will download the data from GitHub.

This will clone in the data which is required to run the forecast. Then you can follow through each step of the forecast:
- 02_process_data.R
- 03_run_inflow_forecast.R
- 04_run_flarer_forecast.R
- 05_visualize.R
