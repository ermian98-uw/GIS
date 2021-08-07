import os
import arcpy
from arcpy import env
from arcpy.ia import *
from arcpy.sa import *

# Eric Anderson #
# University of Washington - Tacoma #
# August 13th, 2021 #
# Automation Code, Python portion only #

##################
# Step ONE: Use NASA's python code (very slightly modified for my workspace) to georeference the ECOSTRESS cloud and surface temperature data.

# Let me reiterate, most of this code is NOT MINE, but was needed to complete the project.
# This code was run via command prompt in the R automation script (step 3 of automation_r.r file).

###
# See the "ECOSTRESS_swath2grid.py" file, I don't want to copy all of it here to detract from MY code
###


##################
# Step TWO: Obtain the stations underneath each ECOSTRESS flyby with arcpy

# This code was run via command prompt in the R automation script (step 3 of automation_r.r file).

lst_file = os.listdir("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output")[1]
cloud_file = os.listdir("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output")[0]

DST_adjust = 700 ################
the_time = str(int(lst_file[37:-22])-DST_adjust)
the_month = int(lst_file[28:-27])
if int(the_time) < 0:
    the_time = str(int(the_time) + 2400)
    the_month = int(lst_file[28:-27]) - 1
    
the_date = str(the_month) + "_" + the_time

args = sys.argv
altitude = abs(float(args[2]))
if altitude > 90:
    altitude = 0
else:
   altitude = abs(float(args[2]))
   
azimuth = float(args[1])
if azimuth < 0:
    azimuth = azimuth + 360
else:
    azimuth = float(args[1])

try:
    # Unit Conversion
    units_raster = UnitConversion("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/" + lst_file, "Kelvin", "Fahrenheit")
    units_raster.save("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/LST_converted" + the_date + ".tif")

    # Transpose Bits
    transpose_raster = arcpy.ia.TransposeBits("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/" + cloud_file, [1, 2], [0, 1], 0, None)
    transpose_raster.save("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/CLOUD_trans" + the_date + ".tif")

    # Raster Calculator (this output was kept in the geodatabase, everything afterwards was removed)
    # Ultimately, I manually combine these into mosaics corresponding to different variables, like Winter Beneath Horizon (these appear on my html page)
    raster_calc_raster = Con(Raster("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/CLOUD_trans" + the_date + ".tif") <= 1, Raster("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/LST_converted" + the_date + ".tif"))
    raster_calc_raster.save("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/merged_" + the_date + ".tif")
    
    # Hillshade
    hillshade_raster = Hillshade("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/WA_State_DEM.tif", azimuth, altitude, "NO_SHADOWS", 1)
    hillshade_raster.save("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/hillshade_" + the_date + ".tif")

    # Mask Hillshade with Cloudless LST pixels
    masked_hillshade_raster = Con(Raster("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/merged_" + the_date + ".tif"), Raster("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/hillshade_" + the_date + ".tif"))
    masked_hillshade_raster.save("C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/masked_hillshade_" + the_date + ".tif")

    # Extract Values to Table (surface temp)
    arcpy.ExtractValuesToTable_ga("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/all_stations_online_XYTableT2/all_stations_online_XYTableT2.shp",
                                  "C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/merged_" + the_date + ".tif", 
                                  "C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/" + the_date + ".txt", "", "")

    # Extract Values to Table (masked hillshade status)
    arcpy.ExtractValuesToTable_ga("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/all_stations_online_XYTableT2/all_stations_online_XYTableT2.shp",
                                  "C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/masked_hillshade_" + the_date + ".tif", 
                                  "C:/Users/eric-/Desktop/MGST_Final/ECOSTRESS_processing/GeoRef/output/" + the_date + "_shadestatus.txt", "", "")
except:
    pass


##################
# Step THREE: Establish the "hot" and "cold" spots for eight groups (see 'ex' variable in code block)

# Spatial Autocorrelation
# The data were divided by county for localization (so that the correlation wasn't state-wide, otherwise the dry east of the state would always be the "hot spot").
# The highest/lowest z-scores (within the 1st/99th percentiles) were extracted by county and merged into a statewide data set.
# These data were intersected with land cover pixels for analysis I cover in the paper.


# Manually mask all pixels above 1000 meters in Washington State using "Con" and "Mask" raster tools.

# Convert raster to point for spatial autocorrelation
ex = ["Summer_AIR_Points", "Summer_BH_AIR_Points", "Spring_AIR_Points", "Spring_BH_AIR_Points",
      "Winter_AIR_Points", "Winter_BH_AIR_Points", "Autumn_AIR_Points", "Autumn_BH_AIR_Points"]

for e in ex:
    # Split points by county
    arcpy.Split_analysis("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/MGST_Final.gdb/" + e,
                         "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/MGST_Final.gdb/WA_State_Boundary", "COUNTY",
                         "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/___split")
    
    print("Split for " + str(e) + " is done.")
    
    # Hot Spot (Getis-Ord Gi*)
    for n in range(1,40):
        myfile = os.listdir("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/___split")[5+((n-1)*8)]
        
        arcpy.stats.HotSpots("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/___split/" + myfile, "grid_code",
                             "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__hotspot/" + myfile, "INVERSE_DISTANCE", "EUCLIDEAN_DISTANCE",
                             "NONE", 160, "#", "#", "NO_FDR")
    
    print("Hot Spots are done.")

    # Merge z-scores into one point file
    myfiles = []
    for n in range(1,40):
        myfiles.append("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__hotspot/" + os.listdir("C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__hotspot")[5+((n-1)*8)])
 
        arcpy.management.Merge(myfiles, "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/MGST_Final.gdb/" + e + "_ZScore")
    
    print("Merging is done.")

    direc = "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/___split"
    for f in os.listdir(direc):
       os.remove(os.path.join(direc, f))

    direc = "C:/Users/eric-/Desktop/MGST_Final/MGST_Final/__hotspot"
    for f in os.listdir(direc):
        os.remove(os.path.join(direc, f))

    # Manual Point to Raster in ArcGIS Pro
    # Reclassify to get highest/lowest outliers, land cover of outliers
    

### END ###
