import arcpy
import datetime
import csv
import os

try:
    print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

    # Variables # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    layers = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/prediction_maps/prediction_maps.gdb' #geodatabase containing field points with non-detections added by Roche and DIMA (lpi) coordinates fixed by Oates
    layers_name = 'Unit_43_48_selection' # name of intercept feature
    polyws = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/Result.gdb' # geodatabase containing finished polygons with corrected TAPR and location data
    polyws_name = "Unit_43_48_Merged"
    outws = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/Result.gdb' # output location for polygons containing field plots
    tblws = '//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/' # output location for tables

    arcpy.env.overwriteOutput = True
    arcpy.env.workspace = polyws
    fc = polyws_name
    
    # load field points
    print 'loading target shapefile'
    arcpy.MakeFeatureLayer_management(layers+'/'+layers_name, 'gmu_poly')

    print 'loading feature '+fc
    arcpy.MakeFeatureLayer_management(polyws+'/'+fc, 'tmp_fc')

    # select polygons matching field points
    print 'selecting ecog polygons that intersect with target shapefile'
    arcpy.SelectLayerByLocation_management('tmp_fc', "INTERSECT", 'gmu_poly', '', "NEW_SELECTION") # point intercept plots

    # exporting polygons
    print 'exporting'
    arcpy.CopyFeatures_management('tmp_fc', outws+'/'+fc+'_select_poly')
    print fc+'_select_poly exported at '+datetime.datetime.now().time().isoformat()[0:8]
    # spatial join
    arcpy.MakeFeatureLayer_management(outws+'/'+fc+'_select_poly', 'tmp_fc_sel')
    arcpy.SpatialJoin_analysis('tmp_fc_sel', 'gmu_poly', outws+'/'+fc+'_join_gmu', "JOIN_ONE_TO_MANY")
    print fc+' joined with target shapefile at '+datetime.datetime.now().time().isoformat()[0:8]
     # export dbf
##            arcpy.TableToExcel_conversion(outws+'/'+fc+'_join_gmu', tblws+'/'+fc+'_tbl_gmu.xls')
    arcpy.TableToTable_conversion(outws+'/'+fc+'_select_poly', tblws, fc+'_tbl_poly.dbf')
    print fc+' dbf file exported at '+datetime.datetime.now().time().isoformat()[0:8]

    print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

except Exception as e:
    print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'
    print e
