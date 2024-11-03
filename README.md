### TESTING
  Here's a link to the website >>  [gfournier.shinyapps.io/cgm-analysis-longterm/](gfournier.shinyapps.io/cgm-analysis-longterm/) 
  
  And you need to upload data for the graphics to render so here's some of my own >> 
  [clarity_1023_0120.xlsx](https://github.com/user-attachments/files/17535653/clarity_1023_0120.xlsx) ...
  [clarity_export_90.xlsx](https://github.com/user-attachments/files/17535655/clarity_export_90.xlsx)
  
  I also uploaded a [demo video](https://youtu.be/vgO8JMuLfws) to youtube for more information about using the app and interpreting results. It's not going to be completely up to date, but it's probably close enough.

<br>

### The website hosted by shiny.io is currently having a couple minor issues——the date range seems to be lagging in respect to the violin plot and the first letter of the title of the bar chart on the left is cut off. For some reason these problems are only present on the web version though, not when I run it from RStudio??? I'm working on it. 

<br>

### UPCOMING FEATURES
  - faster upload processing
    - should improve gradually as I refine the code, eliminating redundancy in data processing and graphic rendering
  - option to change units between mmol/L and mg/dL (currently mg/dL is the only choice)
    - isn't too hard conceptually, but will be very boring and time consuming
  - little buttons that display tips/info when pressed
  - aesthetic changes: dark mode option for the averages graph (maybe), better colors for the violin plot
  - <s>make the font more readable</s> ☑️
  - <s>percent of time in range (pie chart perhaps?)</s> ☑️
