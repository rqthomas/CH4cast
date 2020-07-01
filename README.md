# CH4cast
Methane ebullition forecasts using FLARE water temperature forecasts in Falling Creek Reservoir VA, USA

Welcome to CH4cast! Below, you will find instructions how to execute CH4cast and generate CH4 ebulliiton rate forecasts in Falling Creek Reservoir, USA. 

First - a few things to note. 

THESE FORECASTS ARE ONLY APPLICABLE TO THE UPSTREAM TRANSECT IN FALLING CREEK RESERVOIR!
CH4cast currently cannot be scaled and run in another freshwater ecosystem. However, we are currently working on adapting a mechanisitic model (HIMELLI) to be run through a similar forecast framework so it can be scalable to more systems and run on shorter time scales. Our goal is to inspire all fellow bubble catchers to use this code as a baseline to start ebullition rate forecasts of their own. 

Here are the basic requirements to run CH4cast
 - R (newest version if possible)
 - RStudio

OK - How to run CH4cast! 

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

7: 
      
