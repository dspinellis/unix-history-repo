for i in $*
do
	echo icheck $i
	icheck $i
	echo dcheck $i
	dcheck $i
done
