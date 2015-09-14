#!/bin/shell
path=/path/to/analysis/
R=/path/to/R
FILES=$(ls ../../SNVCalls_IS*)
for call in $FILES;do
	echo $call
	sparename=$(echo $call | sed "s|\.\.\/||g" | sed "s|\.txt||g")
	echo $sparename
	if [ $sparename = SNVCalls_IS1 ]; then
		vcf=synthetic.challenge.set1.tumor.all.truth.vcf
		echo $vcf
	elif [ $sparename = SNVCalls_IS2 ]; then
		vcf=synthetic.challenge.set2.tumor.all.truth.vcf
		echo $vcf
	elif [ $sparename = SNVCalls_IS3 ]; then
		vcf=synthetic.challenge.set3.tumor.20pctmasked.truth.vcf
		echo $vcf
	elif [ $sparename = SNVCalls_IS4 ]; then

		vcf=synthetic.challenge.set4.tumour.25pctmasked.truth.vcf
		echo $vcf
	fi
	filename=$(echo $call | sed "s|\.\.\/||g")
	echo "$path$filename"
	awk '{FS="\t";OFS="\t";if($0 !~ "#" && $0 !~ "SVTYPE" ){print $1,$2}}' ../../$vcf > ${sparename}.truth_pos.bed 


	echo "cd $path;$R < ${path}Create_clustering.R $path${filename} $sparename --no-save" |  qsub -q batch -o $path -j oe -N $sparename -l nodes=1:ppn=1,mem=1Gb,walltime=0:30:00
done
