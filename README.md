# ModisAnalyzR
Create daily mosaics from Moderate Resolution Imaging Spectro-Radiometer (Modis) and calculates various statistics.

This script assumes that MODIS tiles have been downloaded in HDF format to a directory ('DATA') with various sub-directories for each year. 
In this example, 5 tiles per day were downloaded for a span of roughly 20 years. The script lets you create daily mosaics from those tiles (including adding empty raster cells in case of missing tiles). 
Mosaics are then exported as GEOTIFFs to increase computing speed for pixelwise calculation of various statistics (snow cover, cloud cover, missing data values). 

Lastly, some maps are created from seasonal statistics, including raster stack animations, maps with DEM overlay, and trend analyses. 



