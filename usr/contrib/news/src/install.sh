: '@(#)install.sh	1.12	3/19/86'

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
		fi;;
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
if test ! -d $SPOOLDIR
then
	mkdir $SPOOLDIR
fi
chmod 777 $SPOOLDIR
chown $NEWSUSR $SPOOLDIR
chgrp $NEWSGRP $SPOOLDIR

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
$SYSNAME:net,mod,na,usa,to::
oopsvax:net,mod,na,usa,to.oopsvax::
EOF
fi

: If no seq file, make one.
if test ! -s $LIBDIR/seq
then
	echo '100' >$LIBDIR/seq
fi

: if no moderators file, make one.
if test ! -f $LIBDIR/moderators
then
	cat > $LIBDIR/moderators << EOF
mod.ai				ailist@sri-ai.arpa
mod.compilers			compilers@ima.uucp
mod.computers.apollo		apollo@yale.arpa
mod.computers.ibm-pc		info-ibmpc@usc-isib.arpa
mod.computers.laser-printers	laser-lovers@washington.arpa
mod.computers.pyramid		info-pyramid@mimsy.umd.edu
mod.computers.ridge		info-ridge@hopkins-eecs-bravo.arpa
mod.computers.sequent		info-sequent@sally.utexas.edu
mod.computers.sun		sun-spots@rice.arpa
mod.computers.vax		info-vax@sri-kl.arpa
mod.computers.workstations	works@red.rutgers.edu
mod.graphics			info-graphics@aids-unix.arpa
mod.human-nets			human-nets@red.rutgers.edu
mod.legal			info-law@sri-csl.arpa
mod.map				uucpmap@cbosgd.uucp
mod.mac				info-mac@sumex-aim.arpa
mod.mac.binaries		macintosh@felix.uucp
mod.mac.sources			macintosh@felix.uucp
mod.motss			motss@spdcc.uucp
mod.music			gds@eddie.mit.edu
mod.newprod			newprod@cbosgd.uucp
mod.newslists			usenet@gatech.uucp,rick@seismo.css.gov
mod.os				mod-os@lll-crg.uucp
mod.os.os9			os9@nyit.uucp
mod.os.unix			unix@cbosgd.uucp
mod.politics			poli-sci@red.rutgers.edu
mod.politics			politics@abnji.uucp
mod.politics.arms-d		arms-d@mit-mc.arpa
mod.protocols			protocols@red.rutgers.edu
mod.protocols.appletalk		info-applebus@c.cs.cmu.edu
mod.protocols.kermit		info-kermit@cu20b.columbia.edu
mod.protocols.tcp-ip		tcp-ip@sri-nic.arpa
mod.rec.guns			jkh@ucbopal.berkeley.edu
mod.recipes			mod-recipes@glacier.stanford.edu
mod.risks			risks@sri-csl.arpa
mod.sources			sources@panda.uucp
mod.sources.doc			archive@brl.arpa
mod.std				mark@cbosgd.uucp
mod.std.c			std-c@cbosgd.uucp
mod.std.mumps			std-mumps@plus5.uucp
mod.std.unix			std-unix@sally.utexas.edu
mod.techreports			trlist@smu.uucp
mod.telecom			telecom@xx.lcs.mit.edu
mod.vlsi			info-vlsi@sandia-cad.arpa
net.announce			announce@cbosgd.uucp
net.announce.arpa-internet	arpanet-bboards@mit-mc.arpa
net.announce.newusers		usenet@gatech.uucp
EOF
echo
echo Make sure the uucp paths in $LIBDIR/moderators are correct for your site.
fi

sh makeactive.sh $LIBDIR $SPOOLDIR $NEWSUSR $NEWSGRP

for i in $LIBDIR/ngfile $BINDIR/inews $LIBDIR/localgroups
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

: if no aliases file, make one
if test ! -f $LIBDIR/aliases
then
	cat >$LIBDIR/aliases <<EOF
net.chess	net.games.chess
net.forsale	na.forsale
net.joke	net.jokes
net.music.gdea	net.music.gdead
net.notes	net.news.notes
net.periph	net.periphs
net.puzzles	net.puzzle
net.sources.wanted	net.wanted.sources
net.stat	net.math.stat
net.term	net.info-terms
net.trivia	net.games.trivia
net.unix.wizards	net.unix-wizards
net.vlsi	net.lsi
net.works.apollo	net.works
EOF
fi

: if no distributions file, make one
if test ! -f $LIBDIR/distributions
then
	cat >$LIBDIR/distributions <<EOF
local		Local to this site
regional	Everywhere in this general area
usa		Everywhere in the USA
na		Everywhere in North America
world		Everywhere on Usenet in the world (same as net)
EOF
echo
echo You may want to add distributions to $LIBDIR/distributions if your
echo site particpates in a regional distribution such as '"ba"' or '"dc"'.
fi

chown $NEWSUSR $LIBDIR/[a-z]*
chgrp $NEWSGRP $LIBDIR/[a-z]*

echo
echo Reminder: uux must permit rnews if running over uucp.
