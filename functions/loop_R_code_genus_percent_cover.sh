  fold='genus'
  type='percent_cover'
  var=( $( ls ./models/$fold/$type ) )
	len=${#var[@]}
	#len=1
	for ((i=1;i<=len;i++))
    do
      echo "Rscript Predict_models_template.r $i" #check call to r script is as expected
      Rscript Predict_models_template.r $i $fold $type --save 
    done