: check active file for missing or extra newsgroups
: '@(#)checkgroups	1.12	1/17/86'

# Read first line of stdin.  If of the form "-n group", then only check
# for the specified group.  Otherwise, assume doing net and fa.
sed -e '/^[a-zA-Z-]*: /d' -e '/^$/d' -e '/^[#:]/d' | (
read line
case "${line}" in
-n*)
	# Doing specific group.  extract group name and preserve
	# all of current newsgroups file except for that group.
	# Then append entries for this group.
	group=`echo ${line} | sed -e 's/-n /^/' -e 's/$/\\\\./'`
	egrep -v "${group}" LIBDIR/newsgroups > /tmp/$$a
	cat /tmp/$$a - > LIBDIR/newsgroups
	;;
*)
	group="^net\\.|^fa\\.|^mod\\."
	egrep -v "${group}" LIBDIR/newsgroups > /tmp/$$a
	cat /tmp/$$a > LIBDIR/newsgroups
	echo "${line}" >> LIBDIR/newsgroups
	cat >> LIBDIR/newsgroups
	;;
esac
echo junk >/tmp/$$a
echo control >>/tmp/$$a
sed 's/[ \	].*//' LIBDIR/newsgroups |
	egrep "${group}|^general" >>/tmp/$$a
sort -u /tmp/$$a -o /tmp/$$a
egrep "${group}|^general|^junk|^control" LIBDIR/active | sed 's/ .*//' | sort  -u >/tmp/$$b

comm -13 /tmp/$$a /tmp/$$b >/tmp/$$remove
comm -23 /tmp/$$a /tmp/$$b >/tmp/$$add

if test -s /tmp/$$remove
then
	(
	echo "The following newsgroups are not valid and should be removed."
	sed "s/^/	/" /tmp/$$remove
	echo ""
	echo "You can do this by executing the command:"
	echo \	LIBDIR/rmgroup `cat /tmp/$$remove`
	echo ""
	) 2>&1 >/tmp/$$out
fi

if test -s /tmp/$$add
then
	(
	echo "The following newsgroups were missing and should be added."
	sed "s/^/	/" /tmp/$$add
	echo ""
	echo "You can do this by executing the command(s):"
	for i in `cat /tmp/$$add`
	do
		echo 'LIBDIR/inews -n control -d local -t "cmsg newgroup '$i'" </dev/null'
	done
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
