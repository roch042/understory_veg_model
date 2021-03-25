import arcpy
print("arcpy imported")
import time
print("time imported")

start_time = time.time()
print(start_time)

#arcpy.env.workspace = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output'  # location of shapefiles to merge
arcpy.env.workspace = '//hqwildstat/D$/Fine scale vegetation analysis/idahoveg_polygon/GMU_select_polygons.gdb'  # location of shapefiles to merge
outws = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/Result.gdb'
name_poly = 'Unit_43_48' # name of target feature
#fcs = arcpy.ListFeatureClasses()
fcs = ["q43114e1_100blk_select_gmu","q43115e1_100blk_select_gmu","q43114a1_100blk_select_gmu","q43115a1_100blk_select_gmu"]
print(fcs)
#arcpy.Merge_management(fcs, name_poly+'_Merged.shp')
arcpy.Merge_management(fcs, outws+'/'+name_poly+'_Merged')
print(" ---- %s seconds ---" % (time.time() - start_time))
