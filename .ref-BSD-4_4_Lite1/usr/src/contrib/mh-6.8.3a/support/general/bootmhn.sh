: run this script through /bin/sh
: '$Id'

if [ -z "$1" ]; then
    echo "usage: bootmhn.sh MH-library-area" 1>&2
    exit 1
fi
LIB="$1"
MHN="$LIB/mhn_defaults"

if [ -s $MHN ]; then
    echo "%s: already exists." 1>&2
    exit 1
fi
TMP=/tmp/mhn$$
trap "rm -f $TMP" 0 1 2 3 13 15

echo "mhn-store-text: %m%P.txt" >> $TMP
echo "mhn-store-text/richtext: %m%P.rt" >> $TMP
echo "mhn-store-video/mpeg: %m%P.mpg" >> $TMP
echo "mhn-store-application/PostScript: %m%P.ps" >> $TMP

PGM="`./bootmhn.findit xwud $LIB`"
if [ ! -z "$PGM" ]; then
    XWUD="$PGM" X11DIR="`echo $PGM | awk -F/ '{ for(i=2;i<NF;i++)printf "/%s", $i;}'`"/
else
    XWUD= X11DIR=
fi

PGM="`./bootmhn.findit pbmtoxwd $LIB`"
if [ ! -z "$PGM" ]; then
    PBM="$PGM" PBMDIR="`echo $PGM | awk -F/ '{ for(i=2;i<NF;i++)printf "/%s", $i;}'`"/
else
    PBM= PBMDIR=
fi

PGM="`./bootmhn.findit xv $LIB`"
if [ ! -z "$PGM" ]; then
    echo "\
mhn-show-image: %p$PGM -geometry =-0+0 '%f'" >> $TMP
elif [ ! -z $"PBM" -a ! -z "$XWUD" ]; then
    echo "\
mhn-show-image/gif: %p${PBMDIR}giftoppm | ${PBMDIR}ppmtopgm | ${PBMDIR}pgmtopbm | ${PBMDIR}pbmtoxwd | $XWUD -geometry =-0+0
mhn-show-image/x-pbm: %p${PBMDIR}pbmtoxwd | $XWUD -geometry =-0+0
mhn-show-image/x-pgm: %p${PBMDIR}pgmtopbm | ${PBMDIR}pbmtoxwd | $XWUD -geometry =-0+0
mhn-show-image/x-ppm: %p${PBMDIR}ppmtopgm | ${PBMDIR}pgmtopbm | ${PBMDIR}pbmtoxwd | $XWUD -geometry =-0+0
mhn-show-image/x-xwd: %p$XWUD -geometry =-0+0" >> $TMP

    PGM="`./bootmhn.findit djpeg $LIB`"
    if [ ! -z "$PGM" ]; then
	echo "\
mhn-show-image/jpeg: %p$PGM -Pg | ${PBMDIR}ppmtopgm | ${PBMDIR}pgmtopbm | ${PBMDIR}pbmtoxwd | $XWUD -geometry =-0+0" >> $TMP
    fi
fi

if [ -f "/dev/audioIU" ]; then
    PGM="`./bootmhn.findit recorder $LIB`"
    if [ ! -z "$PGM" ]; then
	echo "\
mhn-store-audio/basic: %m%P.au
mhn-compose-audio/basic: ${AUDIODIR}recorder '%f' -au -pause > /dev/tty
mhn-show-audio/basic: %p${AUDIODIR}splayer -au" >> $TMP
    fi
elif [ -f "/dev/audio" ]; then
    PGM="`./bootmhn.findit raw2audio $LIB`"
    if [ ! -z "$PGM" ]; then
	AUDIODIR="`echo $PGM | awk -F/ '{ for(i=2;i<NF;i++)printf "/%s", $i;}'`"/
	echo "\
mhn-store-audio/basic: | ${AUDIODIR}raw2audio -e ulaw -s 8000 -c 1 > %m%P.au
mhn-store-audio/x-next: %m%P.au" >> $TMP
	AUDIOTOOL="`./bootmhn.findit audiotool $LIB`"
	if [ ! -z "$AUDIOTOOL" ]; then
	    echo "\
mhn-compose-audio/basic: $AUDIOTOOL %f && ${AUDIODIR}raw2audio -F < %f" >> $TMP
	else
	    echo "\
mhn-compose-audio/basic: trap \"exit 0\" 2 && ${AUDIODIR}record | ${AUDIODIR}raw2audio -F" >> $TMP
	fi
	echo "\
mhn-show-audio/basic: %p${AUDIODIR}raw2audio 2>/dev/null | ${AUDIODIR}play" >> $TMP

	PGM="`./bootmhn.findit adpcm_enc $LIB`"
	if [ ! -z "$PGM" ]; then
	    DIR="`echo $PGM | awk -F/ '{ for(i=2;i<NF;i++)printf "/%s", $i;}'`"/
	    if [ ! -z "$AUDIOTOOL" ]; then
		echo "\
mhn-compose-audio/x-next: $AUDIOTOOL %f && ${DIR}adpcm_enc < %f" >> $TMP
	    else
		echo "\
mhn-compose-audio/x-next: ${AUDIODIR}record | ${DIR}adpcm_enc" >> $TMP
	    fi
	    echo "\
mhn-show-audio/x-next: %p${DIR}adpcm_dec | ${AUDIODIR}play" >> $TMP
	else
	    if [ ! -z "$AUDIOTOOL" ]; then
		echo "\
mhn-compose-audio/x-next: $AUDIOTOOL %f" >> $TMP
	    else
		echo "\
mhn-compose-audio/x-next: ${AUDIODIR}record" >> $TMP
	    fi
	    echo "\
mhn-show-audio/x-next: %p${AUDIODIR}play" >> $TMP
	fi
    else
	echo "\
mhn-compose-audio/basic: cat < /dev/audio
mhn-show-audio/basic: %pcat > /dev/audio" >> $TMP
    fi
fi

PGM="`./bootmhn.findit mpeg_play $LIB`"
if [ ! -z "$PGM" ]; then
	echo "\
mhn-show-video/mpeg: %p$PGM '%f'" >> $TMP
fi

PGM="`./bootmhn.findit lpr $LIB`"
if [ ! -z "$PGM" ]; then
	echo "\
mhn-show-application/PostScript: %plpr -Pps" >> $TMP    
else
    PGM="`./bootmhn.findit lp $LIB`"
    if [ ! -z "$PGM" ]; then    
	echo "\
mhn-show-application/PostScript: %plp -dps" >> $TMP    
    fi
fi

PGM="`./bootmhn.findit ivs_replay $LIB`"
if [ ! -z "$PGM" ]; then
	echo "\
mhn-show-application/x-ivs: %p$PGM -o '%F'" >> $TMP
fi

PGM="`./bootmhn.findit richtext $LIB`"
if [ ! -z "$PGM" ]; then
	echo "\
mhn-show-text/richtext: %p$PGM -p '%F'" >> $TMP
else
    PGM="`./bootmhn.findit rt2raw $LIB`"
    if [ ! -z "$PGM" ]; then
	echo "\
mhn-show-text/richtext: %p$PGM < '%f' | fmt -78 | more" >> $TMP
    fi
fi

PGM="`./bootmhn.findit xterm $LIB`"
if [ ! -z "$PGM" ]; then
	echo "\
mhn-charset-iso-8859-1: xterm -fn '-*-*-medium-r-normal-*-*-120-*-*-c-*-iso8859-*' -e %s" >> $TMP
fi

sort < $TMP > $MHN
chmod 644 $MHN

exit 0

: not until we get a "safe" postscript environment...

PGM="`./bootmhn.findit pageview $LIB`"
if [ "$DISPLAY" = "unix:0.0" -a ! -z "$PGM" ]; then
    echo "mhn-show-application/PostScript: %p$PGM -" >> $TMP    
else
    PGM="`./bootmhn.findit gs $LIB`"
    if [ ! -z "$PGM" ]; then
	echo "mhn-show-application/PostScript: %p$PGM -- '%F'" >> $TMP
    fi
fi

: have to experiment more with this

PGM="`./bootmhn.findit ivs_record $LIB`"
if [ ! -z "$PGM" ]; then
	echo "\
mhn-compose-application/x-ivs: $PGM -u localhost '%F'" >> $TMP
fi
