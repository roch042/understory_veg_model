#--------------Python Function for Extracting QuadPoly IDs---------------------------------------------------------------------#
#--------------Author: Robert Ritson, Research Associate (WMI)-----------------------------------------------------------------#
#--------------Last Updated: 4/19/2021-----------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------#
## OPTIONAL ARGUMENTS:
### (1) 'intercept_feature'
###         Defaults to 'None'. This argument allows for specifying a specific layer
###         containing a target layer within a geodatabase (for field data stored in a .gdb
###         'None' assumes 'path_to_pts' leads to a shapefile
#####
### (2) 'gdb_name'
###         Defaults to 'None'. This argument names and creates a new geodatabase for storing output
###         'None' assumes 'path_to_output_gdb' leads to a valid geodatabase
#####
### (3) 'sel'
###         Defaults to "all". This argument provides a name or names of specific quadpolygons to sort through
###         Can be a string containing a single quadpolygon name (ex. "q41117e1_100blk") or
###         a list of polygons (ex: ["q41117e1_100blk","q41117a1_100blk","q42117e1_100blk"]
###         "all" sorts through all quadpolygons in the geodatabase specified by 'path_to_quad'
#------------------------------------------------------------------------------------------------------------------------------#
## REQUIRED ARGUMENTS:
### (1) 'path_to_pts' --> file path to location of field points (if path to a gdb, provide an argument for 'intercept_feature')
### (2) 'path_to_quad --> file path to geodatabase containing eCognition quadpolygons
### (3) 'path_to_output_gdb' --> file path to a geodatabase which will contain output shapefiles;
##       to create an empty gdb, provide a file path and an argument for 'gdb_name'
### (4) 'path_to_output_folder' --> file path to a folder which will contain individual output dbfs
### (5) 'mergeTable_dbf' --> file name of final merged output dbf table (must include '.dbf')
#------------------------------------------------------------------------------------------------------------------------------#
#Import modules
import arcpy
import datetime
import csv
import os

#Define function
def extract_quadpolyID(path_to_pts, path_to_quad_gdb, path_to_output_gdb,path_to_output_folder, mergeTable_dbf, intercept_feature = "None",gdb_name = "None",sel= "None"):
    try:
        print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

        #Set paths to folders and workspace#
        plotws = path_to_pts #file location of field points/plots
        polyws = path_to_quad_gdb #gdb of quadpolygons 
        tblws =  path_to_output_folder #Output location for tables
        arcpy.env.overwriteOutput = True
        arcpy.env.workspace = polyws

        #Set and/or create an output workspace#
        if gdb_name == "None":  
            outws = path_to_output_gdb
        else:
            arcpy.CreateFileGDB_management(out_folder_path=path_to_output_gdb,out_name=gdb_name)
            outws =  path_to_output_gdb+'/'+gdb_name

        #Create QuadPolygon List (w/selection, if prompted)#
        try:            
            if sel == "None":
                print 'listing blank 100k quadpolygons'
                fc_list = [fc for fc in arcpy.ListFeatureClasses()] 
            elif type(sel) is str:
                print 'listing blank 100k quadpolygons'
                fc_list = [fc for fc in arcpy.ListFeatureClasses(sel)]
            elif type(sel) is list:
                print 'listing blank 100k quadpolygons'
                fc_list =[fc for fc in sel]
            else:
                raise ValueError("Invalid Selection")
        except ValueError as ve:
            print(ve)
            print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'

        #Execute QuadPolygonID extraction for field pts not w/in a GDB#
        else:
            #Code block for specific field data shapefile
            if intercept_feature == "None":
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
                    
        #Execute QuadPolyID extraction for field pts w/in a GDB#
            #Code block for field data located w/in a GDB  
            else:
                ifc = intercept_feature # name of intercept feature in target feature layer (aka if plotws of field data is a .gdb)
                # load field points
                print 'loading target shapefile'
                arcpy.MakeFeatureLayer_management(plotws+'/'+ifc, 'ifc') #field point shapefile from within a .gdb
                # loop through features
                for fc in fc_list:
                    print 'loading feature '+fc
                    arcpy.MakeFeatureLayer_management(polyws+'/'+fc, 'tmp_fc')
                    # select polygons matching field points
                    print 'selecting ecog polygons that intersect with target shapefile'
                    arcpy.SelectLayerByLocation_management('tmp_fc', "INTERSECT", 'ifc', '', "NEW_SELECTION") # point intercept plots
                    # see if anything was selected
                    matchcount = int(arcpy.GetCount_management('tmp_fc')[0])
                    # if so save that fc
                    if matchcount != 0:
                        # exporting polygons
                        print 'exporting'
                        arcpy.CopyFeatures_management('tmp_fc', outws+'/'+fc+'_select_ifc')
                        print fc+'_select_ifc exported at '+datetime.datetime.now().time().isoformat()[0:8]
                        # spatial join
                        arcpy.MakeFeatureLayer_management(outws+'/'+fc+'_select_ifc', 'tmp_fc_sel')
                        arcpy.SpatialJoin_analysis('tmp_fc_sel', 'ifc', outws+'/'+fc+'_join_ifc', "JOIN_ONE_TO_MANY")
                        print fc+' joined with target shapefile at '+datetime.datetime.now().time().isoformat()[0:8]
                        # export dbf
                        arcpy.TableToTable_conversion(outws+'/'+fc+'_join_ifc', tblws, fc+'_tbl_ifc.dbf')
                        print fc+' dbf file exported at '+datetime.datetime.now().time().isoformat()[0:8]
                    else:
                        pass

        #Merge Output DBF Tables into One File#
            arcpy.env.workspace = tblws
            dbf_list=arcpy.ListTables()
            dbf_merge=arcpy.Merge_management(dbf_list,mergeTable_dbf)
            return(dbf_merge)
            print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]
        
    except Exception as e:
        print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'

# # # # # END OF FUNCTION # # # # # # #
