import arcpy
import datetime
import csv
import os

try:
    print 'begin script on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

    # Variables # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    set_workspace = '//hqwildstat/D$/Fine scale vegetation analysis/2020 Field Season/2020_Field_Season_Sample_Points.gdb' #geodatabase containing field points with non-detections added by Roche and DIMA (lpi) coordinates fixed by Oates
    featureclass = 'Region_Admin' # name of intercept feature
    field = "REGION_ADMIN_ID"
    
    arcpy.env.overwriteOutput = True

    print 'set workspace'
    arcpy.env.workspace = set_workspace

    valueList = []
    rows = arcpy.SearchCursor(featureclass)
    for row in rows:
        valueList.append(row.getValue(field))

    uniqueSet = set(valueList)
    uniqueList = list(uniqueSet)
    uniqueList.sort()

    del rows
    del row

    print uniqueList

    for number in uniqueList:

        layers_sel_name = 'Region_'+`number`
        
        print layers_sel_name
        
        print 'loading target shapefile'
        arcpy.MakeFeatureLayer_management(set_workspace+'/'+featureclass, "region_selection")


        print 'selecting attribute'
        arcpy.SelectLayerByAttribute_management("region_selection", "NEW_SELECTION", "REGION_ADMIN_ID = "+`number`)

        # exporting polygons
        print 'exporting'
        arcpy.CopyFeatures_management("region_selection", set_workspace+'/'+layers_sel_name)

        print layers_sel_name + ' at '+datetime.datetime.now().time().isoformat()[0:8]
    
        print 'script complete on '+datetime.datetime.now().date().isoformat()+' at '+datetime.datetime.now().time().isoformat()[0:8]

except Exception as e:
    print '\n\n!!!---> SCRIPT CRASHED ON '+datetime.datetime.now().date().isoformat()+' AT '+datetime.datetime.now().time().isoformat()[0:8]+' <---!!!\n'
    print e
