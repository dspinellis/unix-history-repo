case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting makedepend (with variable substitutions)"
$spitshell >makedepend <<!GROK!THIS!
$startsh
# $Header: makedepend.SH,v 4.3.1.2 85/05/13 15:53:42 lwall Exp $
#
# $Log:	makedepend.SH,v $
# Revision 4.3.1.2  85/05/13  15:53:42  lwall
# Made cpp look in /usr/local/include too.
# 
# Revision 4.3.1.1  85/05/10  11:35:10  lwall
# Branch for patches.
# 
# Revision 4.3  85/05/01  11:42:26  lwall
# Baseline for release with 4.3bsd.
# 

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

$cat /dev/null >.deptmp
$echo "(Note: this is going to take a while.)"
$rm -f X*.c
for file in *.c; do
    filebase=\`basename \$file .c\`
    $echo "Finding dependencies for \$filebase.o."
    $sed -n <\$file >X\$file \\
	-e "/^\${filebase}_init(/q" \\
	-e '/^#/{' \\
	-e 's|/\*.*$||' \\
	-e p \\
	-e '}'
    $cpp -I/usr/local/include X\$file | $sed  \\
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
    $cat .deptmp >>Makefile.new
else
    $echo "You don't seem to have a proper C preprocessor.  Using grep instead."
    $egrep '^#include ' *.c *.h >.deptmp
    echo "Updating Makefile..."
    <.deptmp $sed -n 's|c:#include "\(.*\)".*\$\$|o: \1|p' >> Makefile.new
    <.deptmp $sed -n 's|c:#include <\(.*\)>.*\$\$|o: /usr/include/\1|p' >> Makefile.new
    <.deptmp $sed -n 's|h:#include "\(.*\)".*\$\$|h: \1|p' >> Makefile.new
    <.deptmp $sed -n 's|h:#include <\(.*\)>.*\$\$|h: /usr/include/\1|p' >> Makefile.new
fi
$mv Makefile Makefile.old
$mv Makefile.new Makefile
$echo "# WARNING: Put nothing here or make depend will gobble it up!" >> Makefile
rm .deptmp X*.c

!GROK!THIS!
$eunicefix makedepend
chmod 755 makedepend
