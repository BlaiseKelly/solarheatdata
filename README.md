# solarheatdata

This repository contains an analysis and article about Danish solar thermal district heating.

The folder R has scripts for processing data abd generating plots.

01_solar_heat_data.R scrapes the hourly heat data from the solarheatdata.eu website.

02_voroni_example.R has the locations of the sites manually defined and generates a voroni shape file (which represents the area around a plant that could theoretically supply energy) and also bins the sites into municipality polygons.

03_data_analysis.R combines the data from solarheatdata.eu with the voronoi and municipality polygons and generates a tmap gif or png.

Additionally danish_electricity_API.R downloadeds historic electricity data from the Danish national grid equivelent.

uk_consumption.R creates a plot of domestic energy consumption from UK government data.

eu_solar.R downloads data from the eu database of solar thermal plants.

The folder Rmd has the article on district heating (https://rpubs.com/Blaise/solar_district_denmark) and a folder with original images used in it.
