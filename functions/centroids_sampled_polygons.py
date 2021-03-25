import arcpy
import datetime
import csv
import os

try:
    print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
   
    # Variables # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    plotws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_select_polygons.gdb' #geodatabase with target feature layer
    tblws = '//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/centroids'
    dropFields = ["Mean_blue","Mean_green","Mean_red","Mean_nir","Standard_deviation_blue","Standard_deviation_green","Standard_deviation_red","Standard_deviation_nir","ele","slp","casp","sasp","twi","lcv","sri","tpi","minpr","maxpr","tapr","mintp","maxtp","aws","clay","sand","silt","cec","d2r","ph","om","caco3","tsf","ff","tc","sc","nass","dev","water_m2","shadow_m2","bareground_m2","mgrass_m2","xgrass_m2","mshrub_m2","xshrub_m2","conifer_m2","decid_m2","agriculture_m2","developed_m2","Shape_Length_1","Shape_Area_1"]
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    arcpy.env.overwriteOutput = True
    arcpy.env.workspace = plotws
    
    print 'identifying needed polygons'
##    fc_list = [fc for fc in arcpy.ListFeatureClasses()] # adjust this to limit the quads you sort through
    fc_list = ['q44116a1_100k_join_gmu','q46115e1_100k_join_gmu','q46115a1_100k_join_gmu','q47116a1_100k_join_gmu','q44116e1_100k_join_gmu','q46116a1_100k_join_gmu','q46116e1_100k_join_gmu','q47115a1_100k_join_gmu'] # adjust this to limit the quads you sort through

    for fc in fc_list:
        print 'begin loop through polygons in quad ' + fc

        print 'make feature'
        arcpy.MakeFeatureLayer_management(plotws+'/'+fc, 'fclyr')
        
        print 'exporting feature point'
        arcpy.FeatureToPoint_management(in_features='fclyr',out_feature_class=plotws+'/'+fc+'_centroid', point_location='INSIDE')

        print 'adding X and Y coordinates'
        arcpy.AddXY_management(in_features=plotws+'/'+fc+'_centroid')

##        print 'deleting unneeded attribute data columns'
##        arcpy.DeleteField_management(in_table=plotws+'/'+fc+'_centroid', drop_field=dropFields)
    
    print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

except Exception as e:
    print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'
    print e

# # # # select log entries from last run # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
