#! /bin/sh
PATH=/usr/local/bin:/bin:/usr/bin:/usr/ucb
#       'rcsfreeze' has the purpose of assigning a symbolic revision
#       number to a set of RCS files, which form a valid configuration.
#
#       The idea is to run rcsfreeze each time a new version is checked
#       in. A unique symbolic revision number (C_[number], where number
#       is increased each time rcsfreeze is run) is then assigned to the most
#       recent revision of each RCS file of the main trunk.
#
#       If the command is invoked with an argument, then this
#       argument is used as the symbolic name to freeze a configuration.
#       The unique identifier is still generated
#       and is listed in the log file but it will not appear as
#       part of the symbolic revision name in the actual RCS file.
#
#       A log message is requested from the user which is saved for future
#       references.
#
#       The shell script works only on all RCS files at one time.
#       It is important that all changed files are checked in (there are
#       no precautions against any error in this respect).
#       file names:
#       {RCS/}rcsfreeze.version         for the version number
#       {RCS/}rscfreeze.log             for the log messages, most recent
#                                       logmessage first.

progname=`basename $0`
DATE=`date`
# Check whether we have an RCS subdirectory, so we can have the right
# prefix for our paths.
if [ -d RCS ] ; then
	RCSDIR=RCS
else
	RCSDIR=.
fi

# Version number stuff, log message file
VERSIONFILE=$RCSDIR/.rcsfreeze.version
LOGFILE=$RCSDIR/.rcsfreeze.log
if [ ! -r $VERSIONFILE ] ; then
# Initialize, rcsfreeze never run before in the current directory
    cat << EOF > $VERSIONFILE
0
EOF
    touch       $LOGFILE
fi

# Get Version number, increase it, write back to file.
VERSIONNUMBER=`cat $VERSIONFILE`
VERSIONNUMBER=`expr $VERSIONNUMBER + 1`
    cat << EOF > $VERSIONFILE
$VERSIONNUMBER
EOF

# Symbolic Revision Number
SYMREV=C_$VERSIONNUMBER
# Allow the user to give a meaningful symbolic name to the revision.
SYMREVNAME=${1-$SYMREV}
echo    "$progname: symbolic revision number computed: \"$SYMREV\""
echo    "$progname: symbolic revision number used:     \"$SYMREVNAME\""
echo    "$progname: the two differ only when $progname invoked with argument"

# Stamp the logfile. Because we order the logfile the most recent
# first we will have to save everything right now in a temporary file.
TMPLOG=/tmp/rcsfreeze.$$.log.tmp
echo "Version: $SYMREVNAME($SYMREV), Date: $DATE"     > $TMPLOG
echo "-----------"                      >> $TMPLOG
# Now ask for a log message, continously add to the log file
echo    "$progname: give log message, summarizing changes"
echo    "       (terminate with ^D or single '.')"
while read MESS ; do
    if [ "$MESS" = '.' ] ; then break ; fi
    echo "  $MESS"      >> $TMPLOG
done
echo "-----------"                      >> $TMPLOG
echo                                    >> $TMPLOG

# combine old and new logfiles
TMPLOG2=$TMPLOG.2
cat $TMPLOG $LOGFILE >  $TMPLOG2
cp $TMPLOG2     $LOGFILE
rm -f  $TMPLOG $TMPLOG2

# Now the real work begins by assigning a symbolic revision number
# to each rcs file. Take the most recent version of the main trunk.

for FILE in $RCSDIR/* ; do
#   get the revision number of the most recent revision
    REV=`rlog -h -d"$DATE" $FILE | fgrep 'head:' | awk ' { print $2 } ' `
    echo        "$progname: file name: \"$FILE\", Revision Number: $REV"
#   assign symbolic name to it.
    rcs -q -n$SYMREVNAME:$REV $FILE
done
