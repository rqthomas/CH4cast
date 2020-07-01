# CH4cast

Welcome to CH4cast! Below, you will find instructions how to generate CH4 ebulliiton rate forecasts in Falling Creek Reservoir, USA. 

First - a few things to note. 

THESE FORECASTS ARE ONLY APPLICABLE TO THE UPSTREAM TRANSECT IN FALLING CREEK RESERVOIR! See McClure et al., 2020 for more details about the transect. 
Again, CH4cast currently cannot be run for another freshwater ecosystem. However, we are currently working on adapting a mechanisitic model (HIMELLI) to be run through a similar forecast framework so it can be scalable to more systems and run on shorter time scales. The goal of CH4cast is to show you that CH4 ebullition, a highly uncertatin efflux from freshwater ecosystems, can in fact be predicted really well when it is integrated into a forecasting workflow! 

Here are the basic requirements to run CH4cast
 - R (newest version if possible (4.0.0 Arbor Day))
 - RStudio

OK - Let's run CH4cast! 

Step 1 - DOWNLOAD CH4cast code via terminal in R

1: Open R Studio

2: Locate the terminal tab

3: In terminal, create a location on your PC where you want all of the files to be stored for this tutorial
      
      For example: 
      mkdir /Users/Owner/Desktop/
      
4: In terminal, move to the new directory using the command "cd ".

      For example:
      cd /Users/Owner/Desktop/
      
5: In terminal, run the following command to download the CH4cast code from Github. 

      For example:
      git clone https://github.com/ryanmclake/CH4cast.git
      
6: After the cloning finishes, there should be a new folder in your working directory named "CH4cast"



Step - 2 Execute the CH4cast scripts in order and make CH4 ebullition rate forecasts

7: Go to the "code" folder and open all six scripts starting with 0_ going to 5_.

8: In 0_package_load.R, find the "Source" key on the top right --> Click "Source" and then let the script run until it is finished

Note --> this may take a while if this is the first time using R. 

9: When the script is complete, select the tab 1_rjags_temp_scale_model_w_DA.R

10: In 1_rjags_temp_scale_model_w_DA.R, select all of the script with your cursor and then select "Run" in the top right. 

Note --> You cannot source this file because of the way it is currently set up with rjags. This will also take a while to execute. 

11: When 1_rjags_temp_scale_model_w_DA.R is complete, select the tab 2_rjags_AR_forecast_model_w_DA.R

12: In 2_rjags_AR_forecast_model_w_DA.R, select all of the script with your cursor and then select "Run" in the top right.

Note --> this is like the temperature scaling model

13: When 2_rjags_AR_forecast_model_w_DA.R is complete, select the tab 3_generate_forecasts.R

14: The 3_generate_forecasts.R is set up to execute using source. Simply click "Source" in the top right and the forecasts will begin running. 

15: Congrats, if there were no errors when 3_generate_forecasts.R was sourced then you have successfully run CH4cast! If you go to the output folder you can find the forecasts for both a SWI temperature scaling model and the CH4 ebullition rate forecasts. 



Step - 3 Forecast Verification (i.e. how did we do?)

16: Select the tab 4_figure_output.R and click "Source". Figures will appear for both the SWI temp scaling model and CH4 ebullition forecasts within the "figures" folder.
    
    For the SWI temp model forecasts:
    CH4cast/figures/SWI_scaling_model_forecast/weekly_output
    
    For the CH4 ebullition forecast:
    CH4cast/figures/ebullition_forecast/weekly_output

17: Finally, select the tab 5_forecast_evaluation.R and then click "Source". A table will generate that reports the Nash-Sutcliffe Efficiancy

    For the NSE table, go to:
    CH4cast/figures/ebullition_forecast/weekly_output

Congrats! You have run CH4cast. Please keep an eye on the repo for more updates as we work to develop a mechanistic model that forecasts ebullition using Meterological drivers!

For more information please visit ryanmclake.weebly.com
