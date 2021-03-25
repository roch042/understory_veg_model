# Name: FeatureClassToGeodatabase_Example2.py
# Description: Use FeatureClassToGeodatabase to copy feature classes
#  to geodatabase format
 
# Import modules
print("import modules")
import arcpy
 
# Set environment settings
#arcpy.env.workspace = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/'
arcpy.env.workspace = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_output_tables/' # location of shapefile to add
 
# Set local variables
in_features = ['polygon_for_clipping_IDTM.shp'] # this is the shapefile to add to the geodatabase
out_location = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU.gdb'
 
# Execute FeatureClassToGeodatabase
print("add shapefile to geodatabase")
arcpy.FeatureClassToGeodatabase_conversion(in_features, out_location)

print("inspect to see if feature class added")
fc=arcpy.ListFeatureClasses()
print(fc)

print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
    
