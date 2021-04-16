#--------------Python Function for Extracting QuadPoly ID-----------------------#
## Updated to include functionality for creating a file geodatabase if missing ##
#-------------------------------------------------------------------------------#
# EXPLANATION: Function contains nested for loops for dealing with two optional
#              input features, 'intercept_feature' & 'gdb_name'
#
# 'intercept_feature' defaults to 'None'
#       This argument takes the file name of a geodatabase (only use if field data contained in file geodatabase)
#       If 'None', then the field data file path is assumed to be to a shapefile
#
# 'gdb_name' defaults to 'None'
#       This argument takes the input '[name].gdb' and creates an empty file geodatabase with that name, then updates 'path_to_output_gdb'
#           to include it
#       If 'None', then 'path_to_output_gdb' assumed to be to a valid .gdb
#
# REQUIRED ARGUMENTS:
# 'path_to_pts' --> file path to location of field points (if path to a gdb, provide an argument for 'intercept_feature')
# 'path_to_quad --> file path to geodatabase containing eCognition quadpolygons
# 'path_to_output_gdb' --> file path to a geodatabase which will contain output shapefiles;
#                          to create an empty gdb, provide a file path and an argument for 'gdb_name'
# 'path_to_output_folder' --> file path to a folder which will contain individual output dbfs
# 'mergeTable_dbf' --> file name of final merged output dbf table (must include '.dbf')
#-------------------------------------------------------------------------------#
#Import modules
import arcpy
import datetime
import csv
import os

#Define function
def extract_quadpolyID(path_to_pts, path_to_quad_gdb, path_to_output_gdb,path_to_output_folder, mergeTable_dbf, intercept_feature = None,gdb_name = None):
    #Begin Script
    try:
        print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
        plotws = path_to_pts #file location of field points/plots
        polyws = path_to_quad_gdb #gdb of quadpolygons 
        tblws =  path_to_output_folder #Output location for tables
        #Set workspace environment
        arcpy.env.overwriteOutput = True
        arcpy.env.workspace = polyws
        if gdb_name != None:  #if gdb_name is provided, create file geodatabase for output features
            #Create gdb for to contain output features
            out_folder_path = path_to_output_gdb
            out_name = gdb_name #name of gdb to create for output features
            arcpy.CreateFileGDB_management(out_folder_path=out_folder_path,out_name=out_name)
            outws =  path_to_output_gdb+'/'+gdb_name
            print 'listing blank 100k quadpolygons'
            fc_list = [fc for fc in arcpy.ListFeatureClasses()] # adjust this to limit the quads you sort through
            # load field points
            print 'loading target shapefile'
            arcpy.MakeFeatureLayer_management(plotws,'pts_lyr') #Field data shapefile
            # loop through features
            for fc in fc_list:
                print 'loading feature '+fc
                arcpy.MakeFeatureLayer_management(polyws+'/'+fc, 'tmp_fc')
                # select polygons matching field points
                print 'selecting ecog polygons that intersect with target shapefile'
                arcpy.SelectLayerByLocation_management('tmp_fc', "INTERSECT", 'pts_lyr','',"NEW_SELECTION") # point intercept plots
                # see if anything was selected
                matchcount = int(arcpy.GetCount_management('tmp_fc')[0])
                # if so save that fc
                if matchcount != 0:
                    # exporting polygons
                    print 'exporting'
                    arcpy.CopyFeatures_management('tmp_fc', outws+'/'+fc+'_select_pts')
                    print fc+'_select_pts exported at '+datetime.datetime.now().time().isoformat()[0:8]
                    # spatial join
                    arcpy.MakeFeatureLayer_management(outws+'/'+fc+'_select_pts', 'tmp_fc_sel')
                    arcpy.SpatialJoin_analysis('tmp_fc_sel', 'pts_lyr', outws+'/'+fc+'_join_pts', "JOIN_ONE_TO_MANY")
                    print fc+' joined with target shapefile at '+datetime.datetime.now().time().isoformat()[0:8]
                    # export dbf
                    arcpy.TableToTable_conversion(outws+'/'+fc+'_join_pts', tblws, fc+'_tbl_pts.dbf')
                    print fc+' dbf file exported at '+datetime.datetime.now().time().isoformat()[0:8]
                else:
                    pass

            # Combine Output .dbfs into one table
            arcpy.env.workspace = tblws
            dbf_list=arcpy.ListTables()
            dbf_merge=arcpy.Merge_management(dbf_list,mergeTable_dbf)
            return(dbf_merge)
            print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
                
        else:
            outws =  path_to_output_gdb #Output location for polygons containing points
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

            # Combine Output .dbfs into one table
                arcpy.env.workspace = tblws
                dbf_list=arcpy.ListTables()
                dbf_merge=arcpy.Merge_management(dbf_list,mergeTable_dbf)
                return(dbf_merge)
                print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
            
    except Exception as e:
        print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'

# # # # # END OF FUNCTION # # # # # # #

