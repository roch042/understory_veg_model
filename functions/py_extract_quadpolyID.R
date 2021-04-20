##### Experimental R Function calling Python ######
# Author: Robert Ritson, Research Associate (WMI) #
# Note: This function must be run with 32-bit R!! #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Description:
#  Includes menu options for extracting "FieldDataPoints_QuadPolyID" or "Covariates_QuadPolyID", 
#  Functionality allows for flexibility in handling field data path type (*.gdb or *.shp; intercept.feature default "None" assumes field data is a *.shp), 
#  creating or specifiying an output geodatabase when newgdb.name is specified (defaults to "None"),
#  and accepts either a string or list of quadpolygons to select from quadpolygon file path (defaults to all; sel="None")
#  output.RData default of "None" autopopulates a name ("FieldDataPoints_QuadPolyID" or "Covariates_QuadPolyID") based on the quadpolygon .gdb selected
#  python path default is "C:/Python27/ArcGIS10.6/python.exe", arcpy module in python script relies on ArcGIS installed version of Python
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
py_extract_quadpolyID <-function (py.path="C:/Python27/ArcGIS10.6/python.exe",py.function,fielddata.path,
                                  quad.path="default",
                                  output.gdb.path,output.folder.path,output.dbf,
                                  output.RData="None",intercept.feature = "None",newgdb.name = "None",quad.sel = "None"){
lapply(c('stringr','reticulate','foreign','reshape2'),require,character.only=T)  
if (quad.path=="default"){
  x<-list("ID Only"="A://Fine scale vegetation analysis/dbases_4modeling/blank_polys100k.gdb",
          "Covariates"="A://Fine scale vegetation analysis/dbases_4modeling/poly_merges.gdb")  
  quad.path<-melt(x[menu(c("ID Only","Covariates"),title="Select QuadPolygon Type:")])
} else{next}
output.RData<-ifelse(output.RData=="None",ifelse(stringr::str_detect(quad.path,"blank"),"FieldDataPoints_QuadPolyID.RData","Covariates_QuadPolyID.RData"),
                     output.RData)
print(paste(output.RData,"will be stored in",output.folder.path))
print('Initializing Python and Loading Function')
use_python(py.path, required = T) #Geoprocessing requires ArcGIS installed Python (MUST USE 32-bit v. of R!!!)
py_available(initialize = T) #initialize python
source_python(py.function)
print('Reticulating QuadPolyID Extraction...')
extract_quadpolyID(path_to_pts=fielddata.path, 
                   path_to_quad_gdb= quad.path,
                   path_to_output_gdb= output.gdb.path,
                   path_to_output_folder= output.folder.path,
                   mergeTable_dbf=output.dbf,
                   sel=quad.sel)

##Combine and Export Output
print('Formatting and Exporting Output')
output<-read.dbf(paste0(output.folder.path,"/",output.dbf),as.is = F)
output$QuadPoly_ID<-output$QuadPoly_I;output$Join_Count<-NULL;output$TARGET_FID<-NULL;output$JOIN_FID<-NULL;output$QuadPoly_1<-NULL;output$QuadPoly_I<-NULL
output$quad<-transpose(lapply(output$QuadPoly_ID,FUN=str_split,pattern="_",n=2,simplify=T))[[1]]
output<-output[,c(8,1:7)]
save(output,file = paste0(output.folder.path,output.RData))
return(output.RData)
}
