#
# shprompt -- give a prompt and get an answer satisfying certain criteria
#
# shprompt [-dDfFsy] prompt
#	s = prompt for string
#	f = prompt for filename
#	F = prompt for full pathname to a file or directory
#	d = prompt for a directory name
#	D = prompt for a full pathname to a directory
#	y = prompt for y or n answer
#
# Chet Ramey
# chet@ins.CWRU.Edu

type=file

OPTS=dDfFsy

succeed()
{
	echo "$1"
	exit 0
}

while getopts "$OPTS" c
do
	case "$c" in
	s)	type=string
		;;
	f)	type=file
		;;
	F)	type=path
		;;
	d)	type=dir
		;;
	D)	type=dirpath
		;;
	y)	type=yesno
		;;
	?)	echo "usage: $0 [-$OPTS] prompt" 1>&2
		exit 2
		;;
	esac
done

if [ "$OPTIND" -gt 1 ] ; then
	shift $[$OPTIND - 1]
fi

while :
do
	case "$type" in
	string)
		echo -n "$1" 1>&2
		read ans || exit 1
		if [ -n "$ans" ] ; then
			succeed "$ans"
		fi
		;;
	file|path)
		echo -n "$1" 1>&2
		read ans || exit 1
		#
		# use `fn' and eval so that bash will do tilde expansion for
		# me
		#
		eval fn="$ans"
		case "$fn" in
		/*)	if test -e "$fn" ; then
				succeed "$fn"
			else
				echo "$0: '$fn' does not exist" 1>&2
			fi
			;;
		*)	if [ "$type" = "path" ] ; then
				echo "$0: must give full pathname to file" 1>&2
			else
				if test -e "$fn" ; then
					succeed "$fn"
				else
					echo "$0: '$fn' does not exist" 1>&2
				fi
			fi
			;;
		esac
		;;
	dir|dirpath)
		echo -n "$1" 1>&2
		read ans || exit 1
		#
		# use `fn' and eval so that bash will do tilde expansion for
		# me
		#
		eval fn="$ans"
		case "$fn" in
		/*)	if test -d "$fn" ; then
				succeed "$fn"
			elif test -e "$fn" ; then
				echo "$0 '$fn' is not a directory" 1>&2
			else
				echo "$0: '$fn' does not exist" 1>&2
			fi
			;;
		*)	if [ "$type" = "dirpath" ] ; then
				echo "$0: must give full pathname to directory" 1>&2
			else
				if test -d "$fn" ; then
					succeed "$fn"
				elif test -e "$fn" ; then
					echo "$0 '$fn' is not a directory" 1>&2
				else
					echo "$0: '$fn' does not exist" 1>&2
				fi
			fi
			;;
		esac
		;;
	yesno)
		echo -n "$1" 1>&2
		read ans || exit 1
		case "$ans" in
		y|Y|[yY][eE][sS])
			succeed "yes"
			;;
		n|N|[nN][oO])
			succeed "no"
			exit 0
			;;
		*)
			echo "$0: yes or no required" 1>&2
			;;
		esac
		;;
	esac
done

exit 1
