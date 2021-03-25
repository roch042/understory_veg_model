    var=( $( ls ./models/species ) )
	len=${#var[@]}
	for ((i=1;i<=len;i++))
    do
      echo "Rscript myRcode.r $i" #check call to r script is as expected
      Rscript myRcode.r $i
    done