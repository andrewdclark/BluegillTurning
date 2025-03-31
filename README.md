# BluegillTurning
 Code and data associated with the paper: "Fast turns are not sped up slow turns: how bluegill sunfish change their kinematics with turn speed"

 ## Code

 In the code folder, you will find all main R files and associated functions along with the code used to triangulate the videos.

 ### Formatting.qmd
 This file is used to format triangulated data for analysis.


 ### TurningAnalysis.qmd
 This file contains all analysis for the paper


 ### Triangulation
 Code associated with triangulating the videos


 #### calibrate_Axes.ipynb
 This file triangulates predefined axes so that the directions make sense. To use it, digitized x and y coordinates need to be manually entered so that they can be triangulated.


 #### triangulate_sleap.ipynb
 This file calibrates the experimental space using a ChArUco board. It then uses that calibration to triangulate all SLEAP digitized points.


 ### Functions
 All custom functions used in our analysis



## RawData
This folder contains all raw data used in the analysis.


### IndividualFiles
All SLEAP digitized CSVs


### AxesRotate
The triangulated axes points for each individual


### Mass
Mass data for each individual


### PecFins
CSVs for manually digitized pectoral fins


### TurnFrames
Frames that denote the start and end of turning for each trial



## ProcessedData
This is where the formatting code will output formatted data. The formatted file already exists here.

### Figures
Where figures are saved
