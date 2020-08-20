# CH4cast

Welcome to CH4cast! Below, you will find instructions how to generate CH4 ebulliiton rate forecasts in Falling Creek Reservoir, USA. 

First - a few things to note. 

If you use these forecasts as an example for a paper, poster, presentation, teaching, etc. please acknowledge this work using the DOI. 
The DOI for this script is:

Because the forecasts are designed to be probabilistic and randomly pull from different parameter estimates and driver data values, you WILL obtain different results if you run CH4cast multiple times. 

THESE FORECASTS ARE ONLY APPLICABLE TO THE UPSTREAM TRANSECT IN FALLING CREEK RESERVOIR! See McClure et al., 2020 for more details about the transect. 

Again, CH4cast currently cannot be run for another freshwater ecosystem. However, we are currently working on adapting a mechanisitic model (HIMELLI) to be run through a similar forecast framework so it can be scalable to more systems and run on shorter time scales. The goal of CH4cast is to show you that CH4 ebullition, a highly uncertatin efflux from freshwater ecosystems, can in fact be predicted really well when it is integrated into a forecasting workflow! 

Here are the basic requirements to run CH4cast
 - R (newest version if possible (4.0.0 Arbor Day))
 - RStudio

OK - Let's run CH4cast! 

Step 1 - DOWNLOAD CH4cast code via terminal in R

0: Make a FOLDER in your Documents (or Desktop) that is called -->     Forecast

1: Open a fresh R Studio window

2: Locate the terminal tab and select it
      
3: In terminal, move to the new Folder you made using the command "cd ".

      For example:
      cd /Users/Owner/Desktop/Forecast
      
4: In terminal, run the following command to download the CH4cast code from Github. 

      For example:
      git clone https://github.com/ryanmclake/CH4cast.git
      
5: After the cloning finishes, there should be a new folder in your working directory named "CH4cast"



Step - 2 Execute the CH4cast scripts in order and make CH4 ebullition rate forecasts

6: Open up the CH4cast R Project by selecting CH4cast.Rproj in the CH4cast folder

7: In 0_package_load.R, find the "Source" key on the top right --> Click "Source" and then let the script run until it is finished

Note --> this may take a while if this is the first time using R. 

8: When the script is complete, select the tab 1_rjags_temp_scale_model_w_DA.R

9: In 1_rjags_temp_scale_model_w_DA.R, select all of the script with your cursor (Control + A) and then select "Run" in the top right. 

Note --> You cannot source this file because of the way it is currently set up with rjags. This will also take a while to execute. 

10: When 1_rjags_temp_scale_model_w_DA.R is complete, select the tab 2_rjags_AR_forecast_model_w_DA.R

11: In 2_rjags_AR_forecast_model_w_DA.R, select all of the script with your cursor (Control + A) and then select "Run" in the top right.

Note --> this is like the temperature scaling model and will take a while to execute!

12: When 2_rjags_AR_forecast_model_w_DA.R is complete, select the tab 3_generate_forecasts_wDA.R

13: The 3_generate_forecasts_wDA.R is set up to execute using source. Simply click "Source" in the top right and the forecasts will begin running. This code is exectuing and running the forecasts in an iterative framweork and updating via data assimilation.

14: When 3_generate_forecasts_wDA.R is complete, select the tab 4_generate_forecasts_nDA.R

15: The 4_generate_forecasts_nDA.R is set up to execute using source. Simply click "Source" in the top right and the forecasts will begin running. This code is executing the forecasts only from the parameter estimates generated on 24 June but using forecasted water temperature data from FLARE. Meaning, it is still generating an actual forecast by using forecasted data but the model is not improving as data become available. 

16: When 4_generate_forecasts_nDA.R is complete, select the tab 5_rjags_null_persistence.R

17: The 5_rjags_null_persistence.R is set up to execute using run. Select all of the script with your cursor (Control + A) and then select "Run" in the top right.

18: When 5_rjags_null_persistence.R is complete, select the tab 6_null_persistence_forecacst.R

19: The 6_null_persistence_forecacst.R is set up to execute using source. Simply click "Source" in the top right and the forecasts will begin running. This code is executing a null persistence model that is simply estimating future ebullition with the observed ebullition rate and random process noise. There are no parameters or updates to the parameters via data assimilation in this forecasting workflow. This is the baseline that the forecasts with and without data assimilation are evaluated against. 

Step - 3 Forecast Verification (i.e. how did we do?)

20: Select the tab 7_figure_output.R and click "Source". Figures will appear for both the SWI temp scaling model and CH4 ebullition forecasts within the "figures" folder.

Congrats! You have run CH4cast. Please keep an eye on the repo for more updates as we work to develop a mechanistic model that forecasts ebullition using Meterological drivers!

For more information please visit ryanmclake.weebly.com or email me at ryan333@vt.edu.

Also - I am happy to chat further if you have any additional ideas or thoughts. This is a very early version and the entire workflow will hopefully be updated to run more smoothly soon-ish ;). 
