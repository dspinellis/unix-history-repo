#! /bin/sh
# subst - substitute strings into files, carefully

PATH=/bin:/usr/bin ; export PATH
sed=sed

case "$1"
in
	-e)
	sed=$2
	shift ; shift
	;;
esac

case "$1"
in
	-f)
	substs=$2
	shift ; shift
	;;
	*)
	echo "$0: no substitutions file given" >&2
	exit 2
	;;
esac

them="`${sed}	-e '/^#/d' \
		-e '/^[ 	]*$/d' \
		-e '/&/s/&/\\\\&/g' \
		-e 's/^\\([^	]*\\)		*\\([^	]*\\)$/s#@<\\1>@#\\2#g/' \
	$substs`"


for f
do
	# first, figure out temporary names
	case "$f"
	in
		*/*)
		file="`expr \"$f\" : '.*/\\([^/]*\\)'`"
		dir="`expr \"$f\" : '\\(.*\\)/[^/]*'`"
		new="$dir/substtmp.new"
		old="$dir/substtmp.old"
		;;

		*)
		new="substtmp.new"
		old="substtmp.old"
		;;
	esac
	echo "$f: " | tr -d '\012'

	# test existences
	if test ! -f $f
	then
		echo "$0: cannot find \`$f'" >&2
		continue				# NOTE CONTINUE
	fi
	if test -r $new
	then
		echo "$0: $new exists, cannot proceed" >&2
		exit 1
	fi
	if test -r $old
	then
		echo "$0: $old exists, cannot proceed" >&2
		exit 1
	fi
	( >$old >$new ) 2>/dev/null
	if test ! -w "$old" -o ! -w "$new"
	then
		rm -f $old $new
		echo "$0: cannot create temporaries $old $new" >&2
		exit 1
	fi

	# generate the new version
	trap "rm -f $new; exit" 1 2 15
	${sed} "/=()<.*>()=/{
		h
		n
		g
		s/.*=()<//
		s/>()=.*//
		$them
	}" $f >$new

	# substitute new for old, if necessary
	if cmp -s $new $f
	then
		rm -f $new $old
		echo "unchanged"
	else
		trap "mv $old $f; exit" 1 2 15
		mv $f $old
		mv $new $f
		trap "rm -f $old; exit" 1 2 15
		rm -f $old
		echo "updated"
	fi
done
