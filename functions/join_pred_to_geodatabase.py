import arcpy, os, csv
arcpy.env.overwriteOutput=1

#Change to match your data
arcpy.env.workspace='//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/Result.gdb'
outws='//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/spatial/output/Result.gdb'
inFeatures='Unit_56_73A_Merged'
csvfolder='//hqwildstat/D$/Fine scale vegetation analysis/Erin Rcode/veg_model/results/species/presence/text'
filename='56_73A_Pseudoroegneria_spicata_NA.csv'
species='Pseudoroegneria_spicata_NA'

#Find all csv files in csvfolder and add to csvlist
#csvlist=[]
#for file in os.listdir(csvfolder):
#    if file.endswith(".csv"):
#        csvlist.append(os.path.join(csvfolder, file))

csvlist=os.path.join(csvfolder, filename)
print("loaded file")

print("make feature layer")
arcpy.MakeFeatureLayer_management(in_features=inFeatures, out_layer='fclyr')
print("make table view")
arcpy.MakeTableView_management(in_table=csvlist, out_view='csvview')
print("join")
arcpy.AddJoin_management(in_layer_or_view='fclyr', in_field='QdPl_ID', join_table='csvview', join_field='QuadPoly_ID')
print("create new feature")
arcpy.CopyFeatures_management('fclyr',outws+'/'+species)

#For each csv in csvlist join and export
#for csv in csvlist:
#    print("make feature layer")
#    arcpy.MakeFeatureLayer_management(in_features=inFeatures, out_layer='fclyr')
#    print("make table view")
#    arcpy.MakeTableView_management(in_table=csv, out_view='csvview')
#    print("join")
#    arcpy.AddJoin_management(in_layer_or_view='fclyr', in_field='QdPl_ID', join_table='csvview', join_field='QuadPoly_ID')
#    print("create new feature")
#    arcpy.CopyFeatures_management(in_features='fclyr',out_feature_class=os.path.splitext(os.path.basename(csv))[0])
