NEWSDIR=/usr/news
cd $NEWSDIR
if [ $# = 0 ]
then
	echo News catalog :
	ls -t
else
	for i in $*
	do
		if [ -f $i ]
		then
			echo  ; echo $i news :
			cat $i
		else 
			echo  ; echo \< News item \"$i\" not found \>
		fi
	done
fi
