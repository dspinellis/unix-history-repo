: run this script through /bin/sh
:	nightly maintenance script for Level-1 DSA
:	contact c=@(country)@cn=Manager to get a time slot for cron to run this script

W="@(dsa)"
D=@(etcdir)quipu/@(wildlife)

: make sure dish is in the search path
PATH=${PATH-:/bin:/usr/ucb:/usr/bin}:@(bindir)
export PATH

if [ ! -d $D ]; then
   echo "unable to find database directory for $W: $D" 1>&2
   exit 1
fi

cd $D

: first, mail the logs to the pilot project sponsors

(echo "To: wpp-stats@nisc.psi.net" ; \
    echo "Subject: $W DSA activity" ; \
    echo "" ; \
    cat dsap.log) | /bin/mail wpp-stats@nisc.psi.net

(echo "To: wpp-stats@nisc.psi.net" ; \
    echo "Subject: $W DSA stats" ; \
    echo "" ; \
    cat stats.log) | /bin/mail wpp-stats@nisc.psi.net


: second, cycle the logs

rm -f iso.*.log ros.*.log [0-9]*.log dish.log xquipu.log

for A in [a-z]*.log
do
    x=2
    while [ $x -gt 0 ];
    do
	y=`expr $x - 1`
	mv $A-$y $A-$x >/dev/null 2>&1
	x=$y
    done

    mv $A $A-0 >/dev/null 2>&1

    > $A
    chmod 666 $A
done

exit 
