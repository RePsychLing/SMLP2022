Download the whole folder and safe it as is in a location of your choice
(You might have to unzip before you work with it)

First, open the "Analysis_RHD_LHD.Rproj" file in RStudio

Second, under the "Files" tab in RStudio you will find all files and subfolders:

markdown files (analysis scripts for both experiments)
	- Analysis_Comprehension.Rmd
		- every part of our analysis of the comprehension experiment
	- Analysis_Production.Rmd
		- every part of our analysis of the production experiment
	- open them from the "Files" tab
	- you can run each code chunk and test it
	- importantly start from the top and work your way down each chunk
	- every step is commented

html-files (html version of analysis scripts)
	- Analysis_Comprehension.html
		- this is a html version of the markdown file Analysis_Comprehension.Rmd
		- when you click on it it opens in RStudio, you can also click it outside of RStudio and it will open in your browser
	- Analysis_Production.html
		- this is a html version of the markdown file Analysis_Production.Rmd
		- when you click on it it opens in RStudio, you can also click it outside of RStudio and it will open in your browser

subfolder "data" (csv files with all of our data)
	- includes 3 csv files that will be automatically loaded when you run the markdown (*.Rmd) files

subfolder "info" (extra information to reproduce and understand data and analysis)
	- includes two pdfs
	- data_files_info.pdf
		- herein you find everything you need to know about the data / csv files for our analysis
	- post_hoc_stats.pdf
		- overview of all post hoc results from emmeans mentioned in the paper
