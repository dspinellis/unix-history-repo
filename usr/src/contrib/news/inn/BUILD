#! /bin/sh
##  $Revision: 1.9 $
##  An interactive script to configure, build, and install InterNetNews.
##  Installation documentation can be found in the "Install.ms*" files;
##  please see the README!

UMASK=${INN_UMASK-027}

if [ ! -f config/config.data ] ; then
    X=`basename ${EDITOR-vi}`
    echo "You don't have a config.data file.  Do the following:" 1>&2
    echo '    cd config' 1>&2
    echo '    cp config.dist config.data' 1>&2
    echo "    $X config.data" 1>&2
    echo '    cd ..' 1>&2
    echo 'And start again.' 1>&2
    exit 1
fi

echo 'NOTE:  If you just type [RETURN] to any question, this script will'
echo '       assume you are being cautious, and default to whatever answer'
echo '       is "safer."  This usually means recompiling.'
echo ''
echo '       You cannot install over a running system; this script will'
echo '       remind you about this later.'

echo ''
if [ -f config/subst ] ; then
    if cmp -s config/subst config/subst.sh ; then
	TYPE="sh"
    else
	TYPE="c"
    fi
    echo "A ${TYPE} version of subst exists;"
    file config/subst
    echo '-- do you want to use it [y or n]?  ' | tr -d '\012'
    read reply
    reply=`echo "${reply}" | tr -d ' \011'`
    case "X${reply}" in
    X[yY]*)
	;;
    *)
	rm config/subst
	if [ -f config/subst ] ; then
	    echo 'Please remove config/subst and start again.' 1>&2
	    exit 1
	fi
	;;
    esac
fi

if [ ! -f config/subst ] ; then
    echo ''
    echo 'Use the /bin/sh version of subst, or the C version?'
    echo "Type sh, c, or x if you don't care:  " | tr -d '\012'
    read style
    reply=`echo "${reply}" | tr -d ' \011'`
    case "X${style}" in
    X[Cc]*)	WHAT=c		;;
    X[Ss][Hh]*)	WHAT=sh		;;
    *)		WHAT=subst	;;
    esac

    echo 'Building subst...'
    cd config ; make ${WHAT} ; cd ..
    if [ ! -f config/subst ] ; then
	echo "Can't build subst -- please investigate and fix." 1>&2
	exit 1
    fi
fi

echo ''
echo 'Have you already built the executables [y or n]?  ' | tr -d '\012'
read reply
reply=`echo "${reply}" | tr -d ' \011'`
case "X${reply}" in
X[yY]*)
    MUSTBUILD=false
    ;;
*)
    MUSTBUILD=true
    ;;
esac

if ${MUSTBUILD} ; then
    echo ''
    echo 'Configuring INN'
    cd config ; make quiet ; cd ..

    echo ''
    echo 'Building and linting the INN library.'
    echo 'Please examine the lint output.'
    cd lib ; make lint ; cd ..
    ${PAGER-more} lib/lint || cat lib/lint

    echo ''
    echo 'Continue with the build [y or n]?  ' | tr -d '\012'
    read reply
    reply=`echo "${reply}" | tr -d ' \011'`
    case "X${reply}" in
    X[yY]*)
	;;
    *)
	echo 'Fix any problems and start again.' 1>&2
	exit 1
	;;
    esac

    echo ''
    echo 'Run lint after each compile [y or n]?  ' | tr -d '\012'
    read reply
    reply=`echo "${reply}" | tr -d ' \011'`
    case "X${reply}" in
    X[yY]*)
	WHAT=lint
	echo ''
	echo 'Compiling and linting the INN programs.'
	;;
    *)
	WHAT=all
	echo ''
	echo 'Compiling the INN programs.'
	;;
    esac
    make ${WHAT} || {
	echo 'Could not build INN.  Fix any problems and start again.' 1>&2
	exit 1
    }

    if [ ${WHAT} = lint ] ; then
	echo ''
	echo 'Do you want to see the lint output [y or n]?  ' | tr -d '\012'
	read reply
	reply=`echo "${reply}" | tr -d ' \011'`
	case "X${reply}" in
	X[yY]*)
	    ${PAGER-more} */lint || cat */lint
	    ;;
	esac
    fi
fi

umask ${UMASK}
echo ''
echo 'Setting umask to' `umask`

echo ''
echo 'Do the spool, etc., directories exist [y or n]?  ' | tr -d '\012'
read reply
reply=`echo "${reply}" | tr -d ' \011'`
case "X${reply}" in
X[nN]*)
    echo 'Please su to root and run the "makedirs.sh" script'
    su
    ;;
esac

echo ''
echo 'You are now ready to install the INN programs and config files.'
echo 'You CANNOT DO THIS if INN is running now -- you must shut down'
echo 'your news system first.'
echo 'Do you want to continue with the installation [y or n]?  ' | tr -d '\012'
read reply
reply=`echo "${reply}" | tr -d ' \011'`
case "X${reply}" in
X[yY]*)
    ;;
*)
    echo 'Fix any problems and start again.' 1>&2
    exit 1
esac

echo ''
echo 'Installing the INN programs.'
echo 'Part of this may fail if you do not have have write permission in'
echo 'all the destination directories.  This is often the case for innd'
echo 'and inndstart, for example.  If the following "make" fails, do this:'
echo '	echo make update | su'
echo 'and rerun this script.'
make update || {
    echo 'Could not install INN.  Fix any problems and start again.' 1>&2
    exit 1
}

echo ''
echo 'Building the data files.'
cd site
make all || {
    echo 'Fix any problems and start again.' 1>&2
    exit 1
}
cd ..

MORETODO=true
SUB='a subshell'
while ${MORETODO} ; do
    echo ''
    echo "Start ${SUB} to edit the config files [y or n]?  " | tr -d '\012'
    read reply
    reply=`echo "${reply}" | tr -d ' \011'`
    case "X${reply}" in
    X[yY]*)
	cd site
	${SHELL-/bin/sh}
	cd ..
	SUB='another shell'
	;;
    X[nN]*)
	MORETODO=false
	;;
    esac
done

echo ''
echo 'Installing the data files.'
cd site
make install || {
    echo 'Could not install INN.  Fix any problems and start again.' 1>&2
    exit 1
}
cd ..

echo ''
echo 'Checking other data files.'
TEMP=${TMPDIR-/tmp}/inn$$
cat >${TEMP} <<\EOF
# =()<ACTIVEFILE=@<_PATH_ACTIVE>@>()=
ACTIVEFILE=/usr/lib/news/active
# =()<HISTORY=@<_PATH_HISTORY>@>()=
HISTORY=/usr/lib/news/history
# =()<NEWSBIN=@<_PATH_NEWSBIN>@>()=
NEWSBIN=/usr/lib/newsbin
# =()<NEWSLIB=@<_PATH_NEWSLIB>@>()=
NEWSLIB=/usr/lib/news
# =()<OWNERS="-o @<NEWSUSER>@ -g @<NEWSGROUP>@">()=
OWNERS="-o news -g news"
EOF
./config/subst -f config/config.data ${TEMP} >/dev/null
. ${TEMP}
rm ${TEMP}
PATH=${PATH}:${NEWSBIN} ; export PATH

##  Active file.
if [ ! -f ${ACTIVEFILE} ] ; then
    echo ''
    echo 'You have no active file; making one.'
    echo 'Make sure to create the new groups you want!'
    echo 'control 0000000000 0000000001 y' >${ACTIVEFILE}
    echo 'junk 0000000000 0000000001 y' >>${ACTIVEFILE}
fi

##  History file.
if [ ! -f ${HISTORY} ] ; then
    touch ${HISTORY}
    FLAGS="-r -i"
else
    echo ''
    echo 'Did you use DBZ in your old news system [y or n]?  ' | tr -d '\012'
    read reply
    reply=`echo "${reply}" | tr -d ' \011'`
    case "X${reply}" in
    X[yY]*)
	;;
    *)
	FLAGS="-i"
	echo 'Will rebuild full history file'
	cp /dev/null ${HISTORY}
	;;
    esac
fi

echo ''
echo 'Setting correct ownership of history file -- double-check when done.'
sh ./installit.sh ${OWNERS} -m 0664 ${HISTORY} ${HISTORY}

echo ''
echo 'Rebuilding history.  This may take some time, so it is the last thing'
echo 'in the installation.'
HERE=`/bin/pwd`
HISTORYDIR=`echo ${HISTORY} | sed -e 's@\(.*\)/.*@\1@'`
cd ${HISTORYDIR}
${NEWSBIN}/makehistory ${FLAGS} || {
    echo 'Could not build history.' 1>&2
    echo "Please run ${NEWSBIN}/makehistory by hand." 1>&2
    exit 1
}
mv history.n.dir history.dir
mv history.n.pag history.pag
cd ${HERE}
sh ./installit.sh ${OWNERS} -m 0664 ${HISTORY}.dir ${HISTORY}.dir
sh ./installit.sh ${OWNERS} -m 0664 ${HISTORY}.pag ${HISTORY}.pag

exit 0
