: This shell script executes the program which converts the
:	binary dates in the named sequencer file to ascii

for i
do

case $i in

all)	old = `pwd`
	cd /usr/notes/.sequencer
	for j in `ls [a-z]*`
	do
	/usr/local/seqtoascii /usr/notes/.sequencer/$j > /tmp/seq/$j
	done
	echo "All intermediate sequencer files are in /tmp/seq."
	echo "    Transfer these files to /tmp/seq on the target machine,"
	echo "    log in to the target machine, then execute 'restseq all'."
	cd $old
	;;
*)
	/usr/local/seqtoascii /usr/notes/.sequencer/$i > /tmp/seq/$i
	echo "Your intermediate file is /tmp/seq/$i."
	echo "    Transfer this file to /tmp/seq on the target machine,"
	echo "    log in to the target machine, then execute 'restseq $i'."
	;;

esac

done
