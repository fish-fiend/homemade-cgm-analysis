### TEST DATA
  Here's a link to the website >>  gfournier.shinyapps.io/cgm-analysis-longterm/ 
  
  And you need to upload data for the graphics to render so here's some >> 
  [clarity_1023_0120.xlsx](https://github.com/user-attachments/files/17535653/clarity_1023_0120.xlsx) ...
  [clarity_export_90.xlsx](https://github.com/user-attachments/files/17535655/clarity_export_90.xlsx)
  
<br></br>

### MOST PRESSING (and hardest to solve) ISSUE — FIXED ☑️
I finally made it possible for multiple files to be uploaded and analyzed as one dataset!!! ANNNNNDDDDD figured out
how to turn the indexing/substitution step into a function in order to replace the out-of-range "High" and "Low" values with "400" and "40" <strong>without</strong> 
sacrificing the ability to wrangle multiple files at once!

Only new issue is that the graph kinda glitches if you upload two datasets that are not consecutive, but it's a fairly innocent problem so I'm going to worry about it later.

<br></br>

### UPCOMING FEATURES
  - option to change units between mmol/L and mg/dL (currently mg/dL is the only choice)
  - make the font more readable
  - dark mode option for the graph (maybe)
  - extend compatability beyond dexcom to other cgm brands
  - <s>percent of time in range (pie chart perhaps?)</s> ☑️
