#!/bin/sh

ROOT=terp.umd.edu.
TMPFILE=/tmp/ns.$$
CACHETMP=/tmp/db.cache.tmp

#
# Look up root server information and store it in a temporary file.
#
nslookup >$TMPFILE <<-EOF
server $ROOT
set type=ns
.
EOF

#
# Generate the the db.cache file from the nslookup output.  Store
# it in a temporary file.
#
cat $TMPFILE | awk '
  /(root)/ {printf "%-20s 99999999 IN  NS %s.\n", ".", $NF}
  /address/ {printf "%-20s 99999999 IN  A  %s\n", $1 ".", $NF}
  ' > $CACHETMP

#
# Move the new db.cache in place if it is not empty.
#
if test -s $CACHETMP
then
  mv $CACHETMP db.cache
else
  rm $CACHETMP
fi

#
# Clean up.
#
rm $TMPFILE
