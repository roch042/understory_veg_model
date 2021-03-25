import arcpy
import datetime
import csv
import os

try:
    print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

    # Variables # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    plotws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU.gdb' #geodatabase containing field points with non-detections added by Roche and DIMA (lpi) coordinates fixed by Oates
    gmu_poly = 'Units_1_6_10A' # name of intercept feature
    polyws = '//hqwildstat/D$/Fine scale vegetation analysis/dbases_4modeling/poly_merges.gdb' # geodatabase containing finished polygons with corrected TAPR and location data
    outws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_select_polygons.gdb' # output location for polygons containing field plots
    tblws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_output_tables' # output location for tables
##    sel = 'q44111b4'
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    arcpy.env.overwriteOutput = True
    arcpy.env.workspace = polyws

##    print 'export feature codes'
##    fcodes = arcpy.ListFeatureClasses()
##    with open('F:/idahoveg_polygon/codes.csv', 'a') as f:
##         writer = csv.writer(f)
##         writer.writerows([fcodes])

##    print 'slice string'
##    fcodes = arcpy.ListFeatureClasses()
##    fcodes_list = [fcodes]
##    print fcodes_list[1:2]
    
    print 'identifying needed polygons'
    # get list of all polygons by 24k quad
    fc_list = [fc for fc in arcpy.ListFeatureClasses()] # adjust this to limit the quads you sort through
##    fc_list = [fc for fc in arcpy.ListFeatureClasses(wild_card = sel)] # select specific quad

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
            # export dbf
##            arcpy.TableToExcel_conversion(outws+'/'+fc+'_join_gmu', tblws+'/'+fc+'_tbl_gmu.xls')
            arcpy.TableToTable_conversion(outws+'/'+fc+'_join_gmu', tblws, fc+'_tbl_gmu.dbf')
            print fc+' dbf file exported at '+datetime.datetime.now().time().isoformat()[0:8]
        else:
            pass

    print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

except Exception as e:
    print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'
    print e

# # # # select log entries from last run # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
