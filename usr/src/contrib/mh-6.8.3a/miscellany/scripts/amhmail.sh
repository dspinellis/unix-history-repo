#! /bin/sh 
#
#    amhmail - aliased version of mhmail(1)
#
#	This program has the same syntax as does mhmail(1).  The differences
#	are that amhmail will do aliasing of addresses by using ali(1) which
#	mhmail(1) does not do.
#
#	The other difference is that amhmail will prompt you if the body of
#	the message is not specified on the command line and stdin is a
#	terminal.
#
#	WARNING:  DON'T CALL THIS FILE 'mhmail' or 'ali'!
#
#	Andy Crump (andyc@inteloc)
#	phone: 681-4697, MS: JF1-70
#

PGM="`basename $0`"
USAGE="syntax: $PGM [addrs ... [-body text] [-cc addrs ...] [-from addr] [-s su
bject] [-(help)]]"

if [ $# -eq 0 ]; then	# If no arguments, do an inc(1)
	inc
	exit 0
fi

BODY="NO BODY"
CC=
FROM=
SUBJECT=
ADDRS=

while [ $# -ne 0 ]
do 
	case $1 in 
	-help)			# Help message only
		echo "$USAGE"
		exit 0
		;;
	-b*)			# -body option, next argument is the text
		shift
		BODY="$1"
		if [ $# -ne 0 ]; then
			shift
		fi
		;;
	-s*)			# -subject option, next argument is the text
		shift
		SUBJECT="$1"
		if [ $# -ne 0 ]; then
			shift
		fi
		;;
	-c*)			# -cc option, all non (-) arguments are taken 
				#  to be cc's.
		shift;
		BREAK=0;
		while [ $BREAK -eq 0 ]
		do
			case "$1" in
			-*)
				BREAK=1;
				CC="`ali $CC`";
				continue;
				;;
			*)
				CC="$CC $1"
				if [ $# -ne 0 ]; then
					shift;
				else
					BREAK=1;
					CC="`ali $CC`";
					continue;
				fi
				;;
			esac
		done
		;;
	-f*)			# -from option, next argument is the text
		shift
		FROM="$1"
		if [ $# -ne 0 ]; then
			shift
		fi
		;;
	-*)			# what is this??
		echo "$PGM : $1 unknown option."
		echo "$USAGE"
		exit 1
		;;
	*) 			# the addresses or garbage
		if [ ! -z "$ADDRS" ]; then	# if we already have the 
						# addresses, this must be 
						# garbage
			echo "$USAGE"
			exit 1
		fi

		BREAK=0
		while [ $BREAK -eq 0 ]
		do
			case "$1" in
			-*)
				BREAK=1
				ADDRS="`ali $ADDRS`"
				continue
				;;
			*)
				ADDRS="$ADDRS $1"
				if [ $# -ne 0 ]; then
					shift
				else
					BREAK=1
					ADDRS="`ali $ADDRS`"
					continue
				fi
				;;
			esac
		done
		;;
	esac
done

#
# Build the command line
#

CMDLINE="mhmail $ADDRS"

if [ "$BODY" != "NO BODY" ]; then
	CMDLINE="$CMDLINE -b \"$BODY\""
else
	if [ -t 0 ]; then		# if stdin is a terminal
		echo "$PGM : Body of message is being read from stdin."
		echo "	Please enter your message and then type ctrl-D."
		echo ""
	fi
fi
if [ ! -z "$CC" ]; then
	CMDLINE="$CMDLINE -cc $CC"
fi
if [ ! -z "$FROM" ]; then
	CMDLINE="$CMDLINE -from $FROM"
fi
if [ ! -z "$SUBJECT" ]; then
	CMDLINE="$CMDLINE -subject \"$SUBJECT\""
fi

eval "$CMDLINE"		# do it!
exit 0
