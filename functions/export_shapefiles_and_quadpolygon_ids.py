import arcpy
import datetime
import csv
import os

try:
    print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

    # Variables # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    plotws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU.gdb' #geodatabase with target feature layer
    gmu_poly = 'polygon_for_clipping_IDTM' # name of intercept feature in target feature layer
    polyws = '//hqwildstat/D$/Fine scale vegetation analysis/dbases_4modeling/blank_polys100k.gdb' # geodatabase containing finished polygons with only QuadPoly_ID
    outws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_select_polygons.gdb' # output location for polygons containing field plots
    tblws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/output' # output location for shapefiles
    sel = ['q43112e1_100blk', 'q44113a1_100blk', 'q44112a1_100blk', 'q44115a1_100blk', 'q44116a1_100blk', 'q44114a1_100blk', 'q44111e1_100blk', 'q44113e1_100blk', 'q44112e1_100blk', 'q44116e1_100blk', 'q44114e1_100blk', 'q44115e1_100blk', 'q45113a1_100blk', 'q45116a1_100blk', 'q45113e1_100blk', 'q45114a1_100blk', 'q45115a1_100blk', 'q45116e1_100blk', 'q45114e1_100blk', 'q45115e1_100blk', 'q46116a1_100blk', 'q46114a1_100blk', 'q46116e1_100blk', 'q46115a1_100blk', 'q46114e1_100blk', 'q47116a1_100blk', 'q46115e1_100blk', 'q47116e1_100blk', 'q47115a1_100blk', 'q48116a1_100blk', 'q47115e1_100blk', 'q48116e1_100blk']
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    arcpy.env.overwriteOutput = True
    arcpy.env.workspace = polyws
    
    print 'identifying needed polygons'
    # get list of all polygons by 24k quad
##    fc_list = [fc for fc in arcpy.ListFeatureClasses()] # adjust this to limit the quads you sort through
    fc_list = [fc for fc in sel] # adjust this to limit the quads you sort through

    # load field points
    print 'loading target shapefile'
    arcpy.MakeFeatureLayer_management(plotws+'/'+gmu_poly, 'gmu_poly')

    # loop through features
    for fc in fc_list:
        print 'loading feature '+fc
        arcpy.MakeFeatureLayer_management(polyws+'/'+fc, 'tmp_fc')

        # select polygons matching field points
        print 'selecting ecog polygons that intersect with target shapefile'
        arcpy.SelectLayerByLocation_management('tmp_fc', "INTERSECT", 'gmu_poly', '', "NEW_SELECTION") # point intercept plots
        # see if anything was selected
        matchcount = int(arcpy.GetCount_management('tmp_fc')[0])
        # if so save that fc
        if matchcount != 0:
            # exporting polygons
            print 'exporting'
            arcpy.CopyFeatures_management('tmp_fc', outws+'/'+fc+'_select_gmu')
            print fc+'_select_gmu exported at '+datetime.datetime.now().time().isoformat()[0:8]
            # spatial join
            arcpy.MakeFeatureLayer_management(outws+'/'+fc+'_select_gmu', 'tmp_fc_sel')
            arcpy.SpatialJoin_analysis('tmp_fc_sel', 'gmu_poly', outws+'/'+fc+'_join_gmu', "JOIN_ONE_TO_MANY")
            print fc+' joined with target shapefile at '+datetime.datetime.now().time().isoformat()[0:8]
            # export shapefile
            arcpy.FeatureClassToFeatureClass_conversion(outws+'/'+fc+'_join_gmu', tblws, gmu_poly+'_intersect_'+fc+'.shp')
            print fc+' shapefile file exported at '+datetime.datetime.now().time().isoformat()[0:8]
        else:
            pass

    print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

except Exception as e:
    print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'
    print e

# # # # select log entries from last run # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
