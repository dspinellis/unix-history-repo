#!/bin/sh
#	dumpdev.sh	1.4	%G%
#	shell script to dump a single device
#	the script is called with a single
#	parameter - which is the device to be dumped
#
#	A second parameter (if set) sets the debug switch which will
#	simply print the parameters to dump and not alter any state
PATH=:/etc:/bin:/usr/bin:
#
dumpdata=/etc/dumpdata
dumpcycle=${dumpdata}/dumpcycle
dump=/etc/dump
labelchk="t"
#
dev=$1
if [ "$1" = "" ]
then
	echo	'Usage: doadump device-name'
	exit 1
fi
chkflg=no
if [ "$2" != "" ]
then
	chkflg=yes
fi
#	device can be /dev/<name>
#	so we'll look for that and split it off
dev=`echo $dev|sed -e 's/\/dev\///'`
#	Now lets see if the device exists in the dumpcycle file
gstr=`grep "^$dev" $dumpcycle` 2> /dev/null
if [ "$gstr" = "" ]
then
#	it might be a raw device
	altdev=`expr $dev : 'r\(.*\)'`
	if [ "$altdev" = "" ]
	then
		echo Sorry, cannot find device $1 in $dumpcycle
		exit 1
	fi
	dev=$altdev
	gstr=`grep "^$dev" $dumpcycle` 2> /dev/null
	if [ "$gstr" = "" ]
	then
		echo Sorry, cannot find device $1 in $dumpcycle
		exit 1
	fi
fi
#	Now we look for existing dump state
#	stored in a file called devicename.state on /etc/dumpdata
statefile=${dumpdata}/${dev}.state
if [ ! -s $statefile ]
then
#	we ain't got one
	STATE="0"
else
	STATE=`cat $statefile`
fi
#
#	Get the next state from the cycle file
#
awkprog="/^$dev/ { if ($STATE == \$2) print \$3	}"
NEXTSTATE=`awk "$awkprog" < $dumpcycle`
if [ "$NEXTSTATE" = "" ]
then
	echo "Dump state problem"
	echo "State file $statefile contents giving current state = $STATE"
	echo "cannot be found in $dumpcycle"
	exit 1
fi
#
#	Now we need the dump information from the cycle file
#
awkprog="/^$dev/ { if ($NEXTSTATE == \$2) print \$4,\$5	}"
decodethis=`awk "$awkprog" < $dumpcycle`
if [ "$decodethis" = "" ]
then
	echo "Dump state problem"
	echo "Cannot find state $NEXTSTATE in $dumpcycle"
	exit 1
fi
#
#	This is really nasty - but
#	now finally set the dump level and the tape range
LEVEL=`expr "$decodethis" : '^\(.*\) '`
TAPE=`expr "$decodethis" : '^.* \(.*\)$'`
if [ "$chkflg" = yes ]
then
	echo "DUMP of /dev/$dev at level ${LEVEL} to tapes $TAPE"
	exit 1
fi
$dump ou${labelchk}${LEVEL} $TAPE /dev/$dev
#
#	dump returns 1 on a successful dump
#
if [ $? = 1 ]
then
	echo $NEXTSTATE > $statefile
fi
exit 0
