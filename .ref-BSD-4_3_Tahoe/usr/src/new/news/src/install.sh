: '@(#)install.sh	1.18	9/24/87'

if test "$#" != 6
then
	echo "usage: $0 spooldir libdir bindir nuser ngroup ostype"
	exit 1
fi
SPOOLDIR=$1
LIBDIR=$2
BINDIR=$3
NEWSUSR=$4
NEWSGRP=$5
OSTYPE=$6

: Get name of local system
case $OSTYPE in
	usg)	SYSNAME=`uname -n`
		if test ! -d $LIBDIR/history.d
		then
			mkdir $LIBDIR/history.d
			chown $NEWSUSR $LIBDIR/history.d
			chgrp $NEWSGRP $LIBDIR/history.d
		fi
		for i in 0 1 2 3 4 5 6 7 8 9
		do
			touch $LIBDIR/history.d/$i
			chown $NEWSUSR $LIBDIR/history.d/$i
			chgrp $NEWSGRP $LIBDIR/history.d/$i
		done
		;;
	v7)	SYSNAME=`uuname -l`
		touch $LIBDIR/history.pag $LIBDIR/history.dir;;
	*)	echo "$0: Unknown Ostype"
		exit 1;;
esac

if test "$SYSNAME" = ""
then
	echo "$0: Cannot get system name"
	exit 1
fi

: Ensure SPOOLDIR exists
for i in $SPOOLDIR $SPOOLDIR/.rnews
do
	if test ! -d $i
	then
		mkdir $i
	fi
	chmod 777 $i
	chown $NEWSUSR $i
	chgrp $NEWSGRP $i
done

chown $NEWSUSR $LIBDIR
chgrp $NEWSGRP $LIBDIR

: Ensure certain files in LIBDIR exist
touch $LIBDIR/history $LIBDIR/active $LIBDIR/log $LIBDIR/errlog $LIBDIR/users
chmod 666 $LIBDIR/users

: If no sys file, make one.
if test ! -f $LIBDIR/sys
then
echo
echo Making a $LIBDIR/sys file to link you to oopsvax.
echo You must change oopsvax to your news feed.
echo If you are not in the USA, remove '"usa"' from your line in the sys file.
echo If you are not in North America, remove '"na"' from your line in the sys file.
	cat > $LIBDIR/sys << EOF
$SYSNAME:world,comp,sci,news,rec,soc,talk,misc,na,usa,to::
oopsvax:world,comp,sci,news,rec,soc,talk,misc,na,usa,to.oopsvax::
EOF
fi

: If no seq file, make one.
if test ! -s $LIBDIR/seq
then
	echo '100' >$LIBDIR/seq
fi

: If no mailpaths, make one.
if test ! -s $LIBDIR/mailpaths
then
	cat <<E_O_F >$LIBDIR/mailpaths
backbone	%s
internet	%s
E_O_F
echo "I have created $LIBDIR/mailpaths for you. The paths are certainly wrong."
echo "You must correct them manually to be able to post to moderated groups."
fi

sh makeactive.sh $LIBDIR $SPOOLDIR $NEWSUSR $NEWSGRP

for i in $LIBDIR/ngfile $BINDIR/inews $LIBDIR/localgroups $LIBDIR/moderators \
	$LIBDIR/cunbatch $LIBDIR/c7unbatch
do
	if test -f $i
	then
		echo "$i is no longer used. You should remove it."
	fi
done

for i in $LIBDIR/csendbatch $LIBDIR/c7sendbatch
do
	if test -f $i
	then
		echo "$i is no longer used. You should remove it after"
		echo "changing your crontab entry to use sendbatch [flags]"
	fi
done

if test -f $BINDIR/cunbatch
then
	echo "$BINDIR/cunbatch is not used by the new batching scheme."
	echo "You should remove it when all of your neighbors have upgraded."
fi

cat >$LIBDIR/aliases.new <<EOF
comp.os.fidonet		comp.org.fidonet
net.sources	comp.sources.misc
misc.jobs		misc.jobs.misc
na.forsale		misc.forsale
rec.skydive		rec.skydiving
talk.philosophy.tech		sci.philosophy.tech 
talk.religion		talk.religion.misc
talk.rumor		talk.rumors
EOF
: if no aliases file, make one
if test ! -f $LIBDIR/aliases
then
	mv $LIBDIR/aliases.new $LIBDIR/aliases
else
	: see whats missing
	sort $LIBDIR/aliases | sed -e 's/  */	/g'  -e 's/		*/	/g' >/tmp/$$aliases
	sort $LIBDIR/aliases.new | sed -e 's/  */	/g'  -e 's/		*/	/g' >/tmp/$$aliases.new
	comm -23 /tmp/$$aliases.new /tmp/$$aliases >/tmp/$$comm
	if test -s /tmp/$$comm
	then
		echo "The following suggested aliases are missing or incorrect in your"
		echo "$LIBDIR/aliases file. It is suggested you add them."
		echo ""
		cat /tmp/$$comm
		echo ""
		echo "A suggested aliases file has been left in $LIBDIR/aliases.new"
		echo "for your convenience."
		rm /tmp/$$comm /tmp/$$aliases
	else
		rm /tmp/$$comm /tmp/$$aliases $LIBDIR/aliases.new
	fi
fi

: if no distributions file, make one
if test ! -f $LIBDIR/distributions
then
	cat >$LIBDIR/distributions <<EOF
local		Local to this site
regional	Everywhere in this general area
usa		Everywhere in the USA
na		Everywhere in North America
world		Everywhere on Usenet in the world
EOF
echo
echo You may want to add distributions to $LIBDIR/distributions if your
echo site particpates in a regional distribution such as '"ba"' or '"dc"'.
fi

chown $NEWSUSR $LIBDIR/[a-z]*
chgrp $NEWSGRP $LIBDIR/[a-z]*

echo
echo Reminder: uux must permit rnews if running over uucp.
rm -f /tmp/$$*
