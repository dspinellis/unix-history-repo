case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting makedepend (with variable substitutions)"
$spitshell >makedepend <<!GROK!THIS!
$startsh
# $Id: makedepend.SH,v 3.0 1991/09/09 20:23:31 davison Trn $
# 
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 
export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

$cat /dev/null >.deptmp
$echo "(Note: this is going to take a while.)"
$rm -f X*.[cy]
for file in *.[cy]; do
    case "\$file" in
    *.c) filebase=\`basename \$file .c\`;;
    *.y) filebase=\`basename \$file .y\`;;
    esac
    $echo "Finding dependencies for \$filebase.o."
    $sed -n <\$file >X\$file \\
	-e "/^\${filebase}_init(/q" \\
	-e '/^#/{' \\
	-e 's|/\*.*$||' \\
	-e 's/\\\\[ 	]*$//' \\
	-e p \\
	-e '}'
    $cpp -I/usr/local/include X\$file | $sed  \\
	-e '/^# *line/s/line//' \
	-e '/^# *[0-9]/!d' \\
	-e 's/^.*"\(.*\)".*\$/'\$filebase'.o: \1/' \\
	-e 's|: \./|: |' \\
	-e 's|: X|: |' | \\
	$uniq | $sort | $uniq >> .deptmp
done

for file in *.SH; do
    $echo \`basename \$file .SH\`: \$file config.sh \; /bin/sh \$file >> .deptmp
done

$sed <Makefile >Makefile.new -e '1,/^# AUTOMATICALLY/!d'

if $test -s .deptmp; then
    echo "Updating Makefile..."
    echo "# If this runs make out of memory, delete /usr/include lines." >>Makefile.new
    $sed -e 's/\\\$/\$\$/g' .deptmp >>Makefile.new
else
    $echo "You don't seem to have a proper C preprocessor.  Using grep instead."
    $egrep '^#include ' *.[cyh] >.deptmp
    echo "Updating Makefile..."
    <.deptmp $sed -n 's|c:#include "\(.*\)".*\$\$|o: \1|p' >> Makefile.new
    <.deptmp $sed -n 's|c:#include <\(.*\)>.*\$\$|o: /usr/include/\1|p' >> Makefile.new
    <.deptmp $sed -n 's|y:#include "\(.*\)".*\$\$|o: \1|p' >> Makefile.new
    <.deptmp $sed -n 's|y:#include <\(.*\)>.*\$\$|o: /usr/include/\1|p' >> Makefile.new
    <.deptmp $sed -n 's|h:#include "\(.*\)".*\$\$|h: \1|p' >> Makefile.new
    <.deptmp $sed -n 's|h:#include <\(.*\)>.*\$\$|h: /usr/include/\1|p' >> Makefile.new
fi
$mv Makefile Makefile.old
$mv Makefile.new Makefile
$echo "# WARNING: Put nothing here or make depend will gobble it up!" >> Makefile
rm .deptmp X*.[cy]

!GROK!THIS!
$eunicefix makedepend
chmod 755 makedepend
