#!/bin/sh
#Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.
#     Written by James Clark (jjc@jclark.uucp)
#
#This file is part of groff.
#
#groff is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 1, or (at your option) any later
#version.
#
#groff is distributed in the hope that it will be useful, but WITHOUT ANY
#WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.
#
#You should have received a copy of the GNU General Public License along
#with groff; see the file LICENSE.  If not, write to the Free Software
#Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

# Add new devices below, where it says `Add local devices here.'

prog=`basename $0`
optF=
optP=
optL=
trflags=
tflag=
pflag=
eflag=
sflag=
lflag=
zflag=
vflag=
iflag=
Cflag=
Nflag=
Vflag=
dev=${GROFF_TYPESETTER:-@DEVICE@}

synopsis="\
usage: $prog [-abehilpstvzCENVZ] [-Hfile] [-Fdir] [-mname] [-Tdev] [-ffam]
       [-wname] [-Wname] [ -Mdir] [-dcs] [-rcn] [-nnum] [-olist] [-Parg]
       [-Larg] [files...]"

devices="Available devices are:
X100	X11 previewer at 100dpi
X75	X11 previewer at 75dpi
X100-12	X11 previewer at 100dpi (optimized for 12 point text)
X75-12	X11 previewer at 75dpi (optimized for 12 point text)
ps	PostScript
dvi	TeX dvi format
latin1	ISO Latin-1
ascii	ASCII"

help="$synopsis
-h	print this message
-t	preprocess with tbl
-p	preprocess with pic
-e	preprocess with eqn
-s	preprocess with soelim
-Tdev	use device dev
-mname	read macros tmac.name
-dcs	define a string c as s
-rcn	define a number register c as n
-nnum	number first page n
-olist	output only pages in list
-ffam	use fam as the default font family
-Fdir	search directory dir for device directories
-Mdir	search dir for macro files
-Hfile	read hyphenation patterns from file
-v	print version number
-z	suppress formatted output
-Z	don't postprocess
-a	produce ASCII description of output
-i	read standard input after named input files
-wname	enable warning name
-Wname	inhibit warning name
-E	inhibit all errors
-b	print backtraces with errors or warnings
-l	spool the output (@PSPRINT@ or @DVIPRINT@)
-C	enable compatibility mode
-V	print the pipeline on stdout instead of executing it
-Parg	pass arg to the postprocessor	
-Larg	pass arg to the spooler	
-N	don't allow newlines within eqn delimiters

$devices"

usage="$synopsis
$prog -h gives more help"

while test $# -gt 0
do
	case $1 in
	-h*)
		echo "$help" >&2
		exit 0
		;;
	-[az])
		trflags="$trflags $1"
		zflag=1
		;;
	-Z)
		zflag=1
		;;
	-i)
		iflag=1
		;;
	-V)
		Vflag=1
		;;
	-t)
		tflag=1
		;;
	-p)
		pflag=1
		;;
	-e)
		eflag=1
		;;
	-s)
		sflag=1
		;;
	-l)
		lflag=1
		;;
	-v)
		vflag=-v
		;;
	-C)
		Cflag=-C
		;;
	-N)
		Nflag=-N
		;;
	-[bE])
		trflags="$trflags $1"
		;;
	-[aiztpeslvbECNVZ]*)
		first=`expr "$1" : '\(-.\)'`
		rest=`expr "$1" : '-.\(.*\)$'`
		shift
		set "" "$first" "-$rest" "$@"
		;;
	-F)
		if test $# -lt 2
		then
			echo "$prog: option -F requires an argument" >&2
			exit 1
		else
			optF="$optF -F$2"
			shift
		fi
		;;
	-F*)
		optF="$optF $1"
		;;
	-T)
		if test $# -lt 2
		then
			echo "$prog: option -T requires an argument" >&2
			exit 1
		else
			dev="$2"
			shift
		fi
		;;

	-T*)
		dev=`expr "$1" : '-T\(.*\)$'`
		;;
	-[fomrMHdnwW])
		if test $# -lt 2
		then
			echo "$prog: option $1 requires an argument" >&2
			exit 1
		else
			trflags="$trflags $1$2"
			shift
		fi
		;;
	-[fomrMHdnwW]*)
		trflags="$trflags $1"
		;;
	-P)
		if test $# -lt 2
		then
			echo "$prog: option -P requires an argument" >&2
			exit 1
		else
			optP="$optP $2"
			shift
		fi
		;;
	-P*)
		optP="$optP `expr "$1" : '-.\(.*\)$'`"
		;;
	-L)
		if test $# -lt 2
		then
			echo "$prog: option -L requires an argument" >&2
			exit 1
		else
			optL="$optL $2"
			shift
		fi
		;;
	-L*)
		optL="$optL `expr "$1" : '-.\(.*\)$'`"
		;;
	--)
		shift
		break
		;;
	-)
		break
		;;
	-*)
		echo "$prog: unrecognized option $1" >&2
		echo "$usage" >&2
		exit 1
		;;
	*)
		break
		;;
	esac
	shift
done

if test $# -gt 0
then
	files="$@"
	if test "$iflag"
	then
		files="$files -"
	fi
else
	files=-
fi

eqnchar=
eqnflag=
picflag=
postpro=

case $dev in
ps)
	trflags="$trflags -mps"
	eqnchar=@FONTDIR@/devps/eqnchar
	postpro="| grops $vflag $optF $optP"
	picflag="-x -p"
	eqnflag=-D
	if test "$lflag"
	then
		postpro="$postpro | @PSPRINT@ $optL"
	fi
	;;

X100|X75|X100-12|X75-12)
	trflags="$trflags -mX"
	picflag=-x
	eqnflag=-D
	postpro="| gxditview $optP -"
	eqnchar=@FONTDIR@/dev$dev/eqnchar
	;;

ascii|latin1)
	trflags="$trflags -mtty"
	postpro="| grotty $vflag $optF $optP"
	;;

dvi)
	trflags="$trflags -mdvi"
	eqnchar=@FONTDIR@/devdvi/eqnchar
	picflag=-x
	postpro="| grodvi $vflag $optF $optP"
	if test "$lflag"
	then
		postpro="$postpro | @DVIPRINT@ $optL"
	fi
	;;
	

# Add local devices here.

*)
	echo "$prog: unknown device \`$dev'" >&2
	echo "$devices" >&2
	exit 1
	;;
esac

prepro=

if test "$sflag"
then
	prepro="$prepro gsoelim $vflag $Cflag $files |"
	files=-
fi

if test "$pflag"
then
	prepro="$prepro gpic $vflag $Cflag $picflag $files |"
	files=-
fi

if test "$tflag"
then
	prepro="$prepro gtbl $vflag $Cflag $files |"
	files=-
fi

if test "$eflag"
then
	prepro="$prepro geqn $eqnflag $vflag $Cflag $Nflag -T$dev -- $eqnchar $files |"
	files=-
fi

if test "$zflag"
then
	postpro=
fi

pipe="$prepro gtroff -T$dev $vflag $Cflag $trflags $optF -- $files $postpro"

if test "$Vflag"
then
	echo $pipe
else
	eval $pipe
fi
exit 0
