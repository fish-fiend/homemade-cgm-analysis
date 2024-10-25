<br></br>
### MOST PRESSING (and hardest to solve) ISSUE:
The graph's utility is currently limited by the fact that each individual excel sheet a user uploads can only contain at most 90 days worth of data because 
Dexcom only allows data to be downloaded in 90 day chunks. While 90 days is a decently long time span, I want it to be possible for users to upload up to a 
year's worth of data and be able to view it simultaneously instead of in four different groups. I want to figure out how to make it so that each file upload is
stored as a variable and automatically joined with previous uploads. 

In order to accomplish that, I need to learn more about the way fileInput stores data, specifically the "datapath" variable. Once I can get a handle on storing the different 
uploads as seperate files, the problem is just figuring out how to join them. The full_join function I use in process_and_plot seems like the best way to do that but 
I don't know yet if I can itterate it in the way I want to. I also need to figure out how to turn the indexing/substitution step into a function. Just for my own sanity.

<br></br>

### Upcoming Features: 
  - option to change units between mmol/L and mg/dL (currently mg/dL is the only choice)
  - make the font more readable
  - dark mode option for the graph (maybe)
  - extend compatability beyond dexcom to other cgm brands
  - percent of time in range (pie chart perhaps?)
