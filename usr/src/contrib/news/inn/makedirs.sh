#! /bin/sh
##  $Revision: 1.6 $
##  Script to make INN directories.

##  Some systems don't support -x, so we have to use -f.
CHOWN=chown
if [ ${CHOWN} = chown ] ; then
    if [ -f /etc/chown ] ; then
	CHOWN=/etc/chown
    else
	if [ -f /usr/etc/chown ] ; then
	    CHOWN=/usr/etc/chown
	fi
    fi
fi

##  =()<MAN1=@<MAN1>@>()=
MAN1=/usr/contrib/man/man1
##  =()<MAN3=@<MAN3>@>()=
MAN3=/usr/contrib/man/man3
##  =()<MAN5=@<MAN5>@>()=
MAN5=/usr/contrib/man/man5
##  =()<MAN8=@<MAN8>@>()=
MAN8=/usr/contrib/man/man8

##  =()<SPOOL=@<_PATH_SPOOL>@>()=
SPOOL=/var/spool/news/spool
##  =()<ARCHIVEDIR=@<_PATH_ARCHIVEDIR>@>()=
ARCHIVEDIR=/var/spool/news/news.archive
##  =()<BATCHDIR=@<_PATH_BATCHDIR>@>()=
BATCHDIR=/var/spool/news/out.going
##  =()<MOST_LOGS=@<_PATH_MOST_LOGS>@>()=
MOST_LOGS=/var/spool/news/data
##  =()<SPOOLNEWS=@<_PATH_SPOOLNEWS>@>()=
SPOOLNEWS=/var/spool/news/spool/in.coming
##  =()<BADNEWS=@<_PATH_BADNEWS>@>()=
BADNEWS=/var/spool/news/spool/in.coming/bad
##  =()<SPOOLTEMP=@<_PATH_SPOOLTEMP>@>()=
SPOOLTEMP=/var/spool/news/spool/in.coming/tmp

##  =()<NEWSLIB=@<_PATH_NEWSLIB>@>()=
NEWSLIB=/var/spool/news/data
##  =()<NEWSBIN=@<_PATH_NEWSBIN>@>()=
NEWSBIN=/usr/contrib/news
##  =()<CONTROLPROGS=@<_PATH_CONTROLPROGS>@>()=
CONTROLPROGS=/var/spool/news/data/ctlbin
##  =()<RNEWSPROGS=@<_PATH_RNEWSPROGS>@>()=
RNEWSPROGS=/usr/contrib/rnews
##  =()<INNDDIR=@<_PATH_INNDDIR>@>()=
INNDDIR=/var/spool/news/data/innd
## =()<MOST_LOGS=@<_PATH_MOST_LOGS>@>()=
MOST_LOGS=/var/spool/news/data

##  =()<NEWSUSER=@<NEWSUSER>@>()=
NEWSUSER=news
##  =()<NEWSGROUP=@<NEWSGROUP>@>()=
NEWSGROUP=news

PFLAG="$1"
umask 0
set -x

for F in \
${MAN1} ${MAN3} ${MAN5} ${MAN8} \
${SPOOL} ${ARCHIVEDIR} ${BATCHDIR} ${MOST_LOGS} ${MOST_LOGS}/OLD \
${SPOOLNEWS} ${BADNEWS} ${SPOOLTEMP} \
${NEWSLIB} ${INNDDIR} \
${NEWSBIN} ${CONTROLPROGS} ${RNEWSPROGS} ; do

    if [ ! -d ${F} ] ; then
	mkdir ${PFLAG} ${F} || exit 1
	${CHOWN} ${NEWSUSER} ${F} || exit 1
	chgrp ${NEWSGROUP} ${F} || exit 1
	case ${F} in
	${INNDDIR})
	    chmod 0770 ${F} || exit 1
	    ;;
	*)
	    chmod 0775 ${F} || exit 1
	    ;;
	esac
    fi

done

exit 0
