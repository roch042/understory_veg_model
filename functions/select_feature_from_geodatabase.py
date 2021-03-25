import arcpy
import datetime
import csv
import os

print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

set_workspace = 'U:/GIS_IDFG.gdb'  # location of shapefiles to merge
outws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU.gdb'
name_poly = 'Hunt_GameUnit' # name of target feature
field = 'NAME'
fcs = "('6','10A','1')"
title = "Units_1_6_10A"

arcpy.env.overwriteOutput = True

print 'set workspace'
arcpy.env.workspace = outws
    
print 'loading target shapefile'
arcpy.MakeFeatureLayer_management(set_workspace+'/'+name_poly, "choose_selection")

print 'selecting attribute'
#arcpy.SelectLayerByAttribute_management("choose_selection", "NEW_SELECTION", "NAME IN ('6','10A','32A')")
arcpy.SelectLayerByAttribute_management("choose_selection", "NEW_SELECTION", field + " IN " + fcs)

print 'aggregate polygons'
arcpy.AggregatePolygons_cartography("choose_selection", outws+'/'+title, 1)

print 'exporting'
arcpy.CopyFeatures_management("choose_selection", outws+'/'+"choose_selection")

print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

