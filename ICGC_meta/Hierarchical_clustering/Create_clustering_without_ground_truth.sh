#!/bin/bash
path_to_analysis=/path/to/analysis/
R=/path/to/R
FILES=$(ls ../SNVCall*)
for call in $FILES;do
	echo $call
	sparename=$(echo $call | sed "s|\.\.\/||g" | sed "s|\.txt||g")
	echo $sparename
	echo "cd $path_to_analysis;$R < ${path_to_analysis}Create_clustering.R $call $sparename --no-save" |  qsub -q batch -o $path_to_analysis -j oe -N $sparename -l nodes=1:ppn=1,mem=1Gb,walltime=0:30:00
done
