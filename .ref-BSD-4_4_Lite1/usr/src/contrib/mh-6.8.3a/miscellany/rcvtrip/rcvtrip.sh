: run this script through /bin/sh
:	usage:	rcvtrip address

if [ "x$1" = x ]; then
    echo "usage: rcvstrip address" 1>&2
    exit ${RP_MECH}
fi

db=rcvtrip draftf=drafts field=advised form=tripcomps
: RP_MECH=200 RP_MOK=32 RP_OK=9
RP_MECH=1 RP_MOK=0 RP_OK=0

prf=/tmp/prf$$ ctx=/tmp/ctx$$ tmp=/tmp/rcvtrip$$
trap "rm -f $prf $ctx $tmp" 0 1 2 3 13 15

rm -f $prf
echo "MH-Sequences:" > $prf
cat ${MH-$HOME/.mh_profile} >> $prf
MH="$prf" ; export MH

rm -f $ctx
cp ${MHCONTEXT-`mhpath +`/context} $ctx
MHCONTEXT="$ctx" ; export MHCONTEXT

cat <&3 > $tmp
chmod 0600 $tmp

from=`ap -format "%<error%|%mbox%<host@%host%>%>" "$1"`

if pick --$field "$from" +$db first; then exit ${RP_MOK}; fi

cat <&3 > $tmp
chmod 0600 $tmp

if repl -cc all -whatnowproc send -file $tmp -form $form -draftfolder +$draftf;
    then anno -component $field -text "$from" +$db first;
    else exit ${RP_MECH};
fi

exit ${RP_MOK}
