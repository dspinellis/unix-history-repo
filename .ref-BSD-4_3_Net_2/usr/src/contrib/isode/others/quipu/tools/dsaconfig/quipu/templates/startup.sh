: run this script through /bin/sh
:	startup script for Level-1 DSA

W="@(dsa)"
D=@(etcdir)quipu/@(wildlife)

if [ ! -d $D ]; then
   echo "unable to find database directory for $W: $D" 1>&2
   exit 1
fi

cd $D

rm -f iso.*.log ros.*.log [0-9]*.log dish.log xquipu.log

exec @(sbindir)ros.quipu -t $D/quiputailor >/dev/null 2>&1
