#! /bin/sh
# Copyright 1985,1988 Massacusetts Institute of Technology.
# $XConsortium: xdpr.script,v 1.9 91/06/30 19:20:16 rws Exp $
# origin: William Kucharski, Solbourne Computer, Inc. 3/24/90
#         translated from csh script xdpr.script "paul 4/12/88"

# initialize variables

display="$DISPLAY"
header=
bsdlprv=
lprv=
out=
svlprv=
trailer=
xprv=
xwdv=

usage="Usage: xdpr [filename] [-out filename ] \
[-display host:display] [[-Pprinter] | [-dprinter]] [-device devtype] \
[{-root | -id <id> | -name <name>}] [-nobdrs] [-xy] \
[-scale scale] [-height inches] [-width inches] [-left inches] \
[-top inches] [-split n] [-header string] [-trailer string] \
[-landscape] [-portrait] [-rv] [-compact] [-noff] [-frame] \
[-plane number] [-gray number] [-psfig] [-density dpi] \
[-cutoff level] [-noposition] [-gamma correction] [-render algorithm] \
[-slide] [-add value] [-help]"

# Guess if we are BSD or System V

if [ -x /usr/ucb/lpr -o -x /usr/bin/lpr -o -x /bin/lpr -o -x /usr/bsd/lpr ]
then
	LP=lpr
	BSD=1
elif [ -x /usr/bin/lp -o -x /bin/lp ]
then
	LP=lp
	BSD=0
else
	LP=lpr
	BSD=1
fi

# parse arguments...

while [ $1 ]; do
	case "$1" in

# ...arguments interpreted by xdpr itself...

	-help)
		echo $usage;
		exit 0;;

# ...arguments to xwd...

	-nobdrs|-root|-xy|-frame)
		xwdv="$xwdv $1";;
	-display)
		display=$2
		xwdv="$xwdv $1 $2";
		shift;;
	-id|-name)
		xwdv="$xwdv $1 $2";
		shift;;
	-out|-add)
		out=true
		xwdv="$xwdv $1 $2";
		shift;;

# ...arguments to xpr...

	-scale|-height|-width|-left|-top|-split|-device)
		xprv="$xprv $1 $2";
		shift;;
	-plane|-gray|-density|-cutoff|-gamma|-render)
		xprv="$xprv $1 $2";
		shift;;
	-header)
		shift;
		header="$1";;
	-trailer)
		shift;
		trailer="$1";;
	-landscape|-portrait|-rv|-compact|-noff|-psfig|-noposition|-slide)
		xprv="$xprv $1";;

# ...arguments to lp[r]...

	-P*|-#?*|-C?*|-J?*|-h|-m)
		bsdlprv="$lprv $1";;

	-d*|-H*|-q*|-n*|-o*|-w)
		svlprv="$svlprv $1";;

# ...disallow other arguments; print usage message

	-*)
		echo "xdpr: Unknown option $1";
		echo $usage;
		exit 1;;

# ...input filename...

	*)
		if [ ! "$infile" ]; then
			infile=true
			xprv="$xprv $1"
		else
			echo "xdpr: Invalid argument "$1""
			echo $usage			
			exit 1
		fi
	esac
	shift
done

# quit if there is no DISPLAY specified

if [ ! "$display" ]; then
	echo "xdpr: DISPLAY variable must be set or a display specified."
	exit
fi

# Command lines:

# Set up lp[r] options...

if [ $BSD -eq 0 ]
then
	lprv=$svlprv
else
	lprv=$bsdlprv
fi

# disallow concurrent input and  -out arguments
if [ "$out" -a "$infile" ]; then
	echo "xdpr: -out <filename> cannot be used if an input file is also specified."
	exit 0
fi

# dump only
if [ "$out" ]; then
	if [ "$xprv" -o "$lprv" ]; then
		echo "xdpr: The following arguments will be ignored:"
		echo $xprv $lprv
	fi
	/usr/bin/X11/xwd $xwdv
	exit 0
fi

# print only 
if [ "$infile" ]; then
	if [ "$xwdv" ]; then
		echo "xdpr: The following arguments will be ignored:"
		echo $xwdv
	fi
	/usr/bin/X11/xpr -header "$header" -trailer "$trailer" $xprv | $LP $lprv
	exit 0
fi

# dump & print (default)
/usr/bin/X11/xwd $xwdv | /usr/bin/X11/xpr -header "$header" -trailer "$trailer" $xprv | $LP $lprv
exit 0	

# EOF
