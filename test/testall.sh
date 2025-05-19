script_dir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
exec=$script_dir/../bin/faust.exe
report_file=$script_dir/report.txt

total_tests=0
total_score=0

test() {
	score=0
	total=0
	echo "---" $1 tests "----"
	for file in $script_dir/$1/* ; do
		filename=`basename $file`
		$exec -p $file &> /dev/null
		res=$?
		if [[ $res == $2 ]]
		then
			echo $filename: OK | tee -a $report_file
			res=1
		else
			echo $filename: FAILED with $res instead of $2 | tee -a $report_file
			res=0
		fi
		score=$(($score+$res))
		total=$(($total+1))
	done
	echo score: $score / $total | tee -a $report_file
	echo | tee -a $report_file
	total_tests=$(($total_tests+$total))
	total_score=$(($total_score+$score))
}

# truncate the old file
echo -e "Tests report\n" | tee $report_file

test pos 0
test neg 1

echo total score: $total_score / $total_tests | tee -a $report_file
