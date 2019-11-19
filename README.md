# mailmerge-reports
This repository includes example files that auto-generate a series of individualized reports based on a single data set.

## In this folder, subfolders include:
- The "data" subfolder contains dataset(s)
- The "pres" subfolder contains scripts, additional content files (e.g., images), and outputs (HTML presentation file) for a summary presentation
- The "reports" subfolder contains the PDF reports generated using the Report Generator script

## Additionally, this main folder contains:
- The R Project file (mailmerge-reports.Rproj), which should always be opened before any other files in this folder
- This README.md file
- The R Markdown template for the PDF reports to be generated (ReportTemplate.Rmd)
- The R script that, when run, calculates summary statistics and plots, and then generates individualized reports (RUNME_ReportGenerator.R)

## To generate individualized reports:
1. After all participants complete the questionnaire, export the raw dataset from Qualtrics as a CSV file with the *numeric values* (NOT *choice text*) option selected.

2. Then, save this CSV file in the "data" subfolder in Google Drive with the name "data.csv".

3. Open the R Project file (mailmerge-reports.Rproj).

4. Open the report generation script (RUNME_ReportGenerator.R).

5. Select all the text in this script by typing command-A and then click the "Run" button to run this script to generate the participant reports. (After running the script, the completed report PDFs will be saved to the "reports" subfolder labeled with each participant's UNI.)

## To generate a summary presentation:
1. If you haven't already done so, after all participants complete the questionnaire, export the raw dataset from Qualtrics as a CSV file with the *numeric values* (NOT *choice text*) option selected.

2. If you haven't already done so, save this CSV file in the "data" subfolder in Google Drive with the name "data.csv".

3. Open the R Project file (mailmerge-reports.Rproj).

4. Open the presentation generation script (summarypres.Rmd) in the "pres" subfolder.

5. Click "Knit" at the top of the Rmd file to generate the presentation.
