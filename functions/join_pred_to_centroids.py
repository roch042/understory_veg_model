import arcpy
import datetime
import csv
import os

try:
    print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
    
    #Change to match your data
    arcpy.env.workspace='//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_select_polygons.gdb'
    outws='//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/spatial/GMU_select_polygons.gdb'
    csvfile ='//hqwildstat/D$/Fine scale vegetation analysis/understory_veg_model/results/species/presence/text/Units_6_10A_32A_5shrubspecies_presentonly.csv'

    arcpy.env.overwriteOutput = True
    
##   fc_list = ['q44116a1_100k_join_gmu_centroid','q46115e1_100k_join_gmu_centroid','q46115a1_100k_join_gmu_centroid','q47116a1_100k_join_gmu_centroid','q44116e1_100k_join_gmu_centroid','q46116a1_100k_join_gmu_centroid','q46116e1_100k_join_gmu_centroid','q47115a1_100k_join_gmu_centroid'] # adjust this to limit the quads you sort through
    fc_list = ['q44116a1_100k_join_gmu_centroid']
   
    for fc in fc_list:
        print 'begin loop through polygons in quad ' + fc

        print("make feature layer")
        arcpy.MakeFeatureLayer_management(in_features=fc, out_layer='fclyr')
        
        print("make table view")
        arcpy.MakeTableView_management(in_table=csvfile, out_view='csvview')
        
        print("join")
        arcpy.AddJoin_management(in_layer_or_view='fclyr', in_field='QuadPoly_ID', join_table='csvview', join_field='QuadPoly_ID', join_type = 'KEEP_COMMON')
        
        print("create new feature")
        arcpy.CopyFeatures_management('fclyr',outws+'/'+'ShrubPresence_'+fc)

    print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

except Exception as e:
    print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'
    print e
