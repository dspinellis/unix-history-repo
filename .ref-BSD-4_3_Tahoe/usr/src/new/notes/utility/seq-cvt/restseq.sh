: This shell script executes the program to convert a sequencer
:	file with ascii dates into a file with binary dates

for i
do

case $i in

all)	old = `pwd`
	cd /tmp/seq
	for j in `ls [a-z]*`
	do
	/usr/local/seqtobinary /usr/notes/.sequencer/$j < /tmp/seq/$j
	done
	rm -f /tmp/seq/*
	cd $old
	echo "Sequencer file(s) restored."
	;;

*)	/usr/local/seqtobinary /usr/notes/.sequencer/$i < /tmp/seq/$i
	rm -f /tmp/seq/$i
	echo "Sequencer file(s) restored."
	;;

esac

done
