: run this script through /bin/sh

x=`fgrep logpath @(ETCDIR)isotailor 2>/dev/null | awk '{ print $2 }'`
if [ "x$x" != x ]; then
    cd $x
else
    cd @(LOGDIR)
fi

rm -f iso.*.log ros.*.log [0-9]*.log

for A in [a-z]*.log
do
    x=2
    while [ $x -gt 0 ];
    do
	y=`expr $x - 1`
	mv $A-$y $A-$x >/dev/null 2>&1
	x=$y
    done

    if [ ! -f $A ];
    then
	continue
    fi

    if [ -s $A ];
    then
	mv $A $A-0 >/dev/null 2>&1
    fi

    > $A
    chmod 666 $A
done

exit 0
