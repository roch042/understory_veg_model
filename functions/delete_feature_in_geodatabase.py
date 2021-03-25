import arcpy

#Change to match your data
arcpy.env.workspace='//hqwildstat/D$/Fine scale vegetation analysis/2020 Field Season/2020_Field_Season_Sample_Points.gdb'
#arcpy.env.workspace='//hqwildstat/D$/Fine scale vegetation analysis/idahoveg_database/GMU.gdb'
#delete_feature='Unit_43_48_Merged'
#delete_feature='Unit_43_48_Merged_join_gmu'
##delete_feature=['alp_r1_dft','alp_r1_dft_polygon','alp_r1_dft_polygon_invert']
delete_feature=['alp_r1_dft_polygon','alp_r2_dft_polygon','alp_r3_dft_polygon','alp_r4567_dft_polygon','alp_r1_dft_polygon_selection','alp_r2_dft_polygon_selection','alp_r3_dft_polygon_selection','alp_r4567_dft','alp_r4567_dft_polygon_selection','int1_selection_clip','int2_selection_clip','int3_selection_clip','int5_selection_clip','Merge1','Region_1','Region_2','Region_3','Region_4','Region_5','Region_6','Region_7','region_selection_omit']
##delete_feature=['Region_1_selection','Region_2_selection','Region_3_selection','Region_4_selection','Region_5_selection','Region_6_selection','Region_1_selection_pairwise','Region_7_selection']


print("list feature classes")
fc=arcpy.ListFeatureClasses()
print(fc)

print("delete target feature class")
##arcpy.Delete_management(delete_feature)
for objFeatureClass in delete_feature:  
    arcpy.Delete_management(objFeatureClass)
    
print("inspect to see if feature class removed")
fc=arcpy.ListFeatureClasses()
print(fc)

print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
    
