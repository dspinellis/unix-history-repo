: check active file for missing or extra newsgroups
: '@(#)checkgroups	1.23	9/24/87'

if  test  ! -s LIBDIR/newsgroups
then
	cp /dev/null LIBDIR/newsgroups
fi
echo "" >/tmp/$$out
# Read first line of stdin.  If of the form "-n group", then only check
# for the specified group.  Otherwise, assume doing standard groups
sed -e "/^From: /w /tmp/$$out" -e '/^[a-zA-Z-]*: /d' -e '/^$/d' -e '/^[#:]/d' | (
read line
case "${line}" in
-n*)
	# Doing specific group.  extract group name and preserve
	# all of current newsgroups file except for that group.
	# Then append entries for this group.
	group=`echo "x${line}" | sed -e 's/x-n /^/' -e 's/$/[. 	]/'`
	egrep -v "${group}" LIBDIR/newsgroups > /tmp/$$a
	cat /tmp/$$a - > LIBDIR/newsgroups
	;;
*)
	# Get the distributions from the checkgroups message itself
	# This allows sites to append their local groups to the distributed
	# checkgroups message and prevents stray checkgroups from other sites
	# from showing all the local groups as being bad groups.
	#
	echo "${line}" > /tmp/$$msg
	cat >> /tmp/$$msg
	cp /dev/null /tmp/$$b
	sed -e "s;[ 	].*;;" -e "s;\..*;;" -e "s;^!;;" /tmp/$$msg | sort -u |
		while read dist
		do
			group=`cat /tmp/$$b`
			group="${group}|^$dist[. 	]"
			echo "${group}" > /tmp/$$b
		done
	group=`cat /tmp/$$b`
	egrep -v "${group}" LIBDIR/newsgroups > /tmp/$$a
	cat /tmp/$$a > LIBDIR/newsgroups
	sed -e "/^!/d" /tmp/$$msg >> LIBDIR/newsgroups
	rm -f /tmp/$$b /tmp/$$msg
	;;
esac

egrep "${group}" LIBDIR/active | sed 's/ .*//' | sort >/tmp/$$active
egrep "${group}" LIBDIR/newsgroups | sed 's/[ 	].*//' | sort >/tmp/$$newsgrps

comm -13 /tmp/$$active /tmp/$$newsgrps >/tmp/$$missing
comm -23 /tmp/$$active /tmp/$$newsgrps >/tmp/$$remove

egrep "${group}" LIBDIR/active | sed -n "/m\$/s/ .*//p" |
	sort > /tmp/$$amod.all
egrep "${group}" LIBDIR/newsgroups |
sed -n "/Moderated/s/[ 	][ 	]*.*//p" | sort > /tmp/$$ng.mod

comm -12 /tmp/$$missing /tmp/$$ng.mod >/tmp/$$add.mod
comm -23 /tmp/$$missing /tmp/$$ng.mod >/tmp/$$add.unmod
cat /tmp/$$add.mod /tmp/$$add.unmod >>/tmp/$$add

comm -23 /tmp/$$amod.all /tmp/$$remove >/tmp/$$amod
comm -13 /tmp/$$ng.mod /tmp/$$amod >/tmp/$$ismod
comm -23 /tmp/$$ng.mod /tmp/$$amod >/tmp/$$nm.all
comm -23 /tmp/$$nm.all /tmp/$$add >/tmp/$$notmod

echo "" >>/tmp/$$out
if test -s /tmp/$$remove
then
	(
	echo "The following newsgroups are non-standard."
	sed "s/^/	/" /tmp/$$remove
	echo ""
	echo "You can remove them by executing the commands:"
	echo \	LIBDIR/rmgroup `cat /tmp/$$remove`
	echo ""
	) 2>&1 >>/tmp/$$out
fi

if test -s /tmp/$$add
then
	(
	echo "The following newsgroups were missing and should be added."
	sed "s/^/	/" /tmp/$$add
	echo ""
	echo "You can do this by executing the command(s):"
	for i in `cat /tmp/$$add.unmod`
	do
		echo 'LIBDIR/inews -C '$i' </dev/null'
	done
	for i in `cat /tmp/$$add.mod`
	do
		echo 'LIBDIR/inews -C '$i' moderated </dev/null'
	done
	echo ""
	) 2>&1 >>/tmp/$$out
fi

if test -s /tmp/$$ismod
then
	(
	echo "The following newsgroups are not moderated and are marked moderated."
	sed "s/^/	/" /tmp/$$ismod
	echo ""
	echo "You can correct this by executing the command(s):"
	for i in `cat /tmp/$$ismod`
	do
		echo 'LIBDIR/inews -C '$i' </dev/null'
	done
	echo ""
	) 2>&1 >>/tmp/$$out
fi

if test -s /tmp/$$notmod
then
	(
	echo "The following newsgroups are moderated and not marked so."
	sed "s/^/	/" /tmp/$$notmod
	echo ""
	echo "You can correct this by executing the command(s):"
	for i in `cat /tmp/$$notmod`
	do
		echo 'LIBDIR/inews -C '$i' moderated </dev/null'
	done
	echo ""
	) 2>&1 >>/tmp/$$out
fi

if test -s /tmp/$$out
then
	(echo	"Subject: Problems with your active file"
	echo ""
	cat /tmp/$$out
	) | if test $# -gt 0
		then
			mail $1
		else
			cat
		fi	
fi
)

rm -f /tmp/$$*

