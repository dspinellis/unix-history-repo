case $CONFIG in
'')
    if test ! -f config.sh; then
	ln ../config.sh . || \
	ln ../../config.sh . || \
	ln ../../../config.sh . || \
	(echo "Can't find config.sh."; exit 1)
    fi 2>/dev/null
    . ./config.sh
    ;;
esac
case "$0" in
*/*) cd `expr X$0 : 'X\(.*\)/'` ;;
esac
echo "Extracting makedepend (with variable substitutions)"
rm -f makedepend
$spitshell >makedepend <<!GROK!THIS!
$startsh
# $RCSfile: makedepend.SH,v $$Revision: 4.0.1.4 $$Date: 92/06/08 13:51:24 $
#
# $Log:	makedepend.SH,v $
# Revision 4.0.1.4  92/06/08  13:51:24  lwall
# patch20: various and sundry fixes
# 
# Revision 4.0.1.3  91/11/05  17:56:33  lwall
# patch11: various portability fixes
# 
# Revision 4.0.1.2  91/06/07  15:40:06  lwall
# patch4: fixed cppstdin to run in the right directory
# 
# Revision 4.0.1.1  91/06/07  11:20:06  lwall
# patch4: Makefile is no longer self-modifying code under makedepend
# 
# Revision 4.0  91/03/20  01:27:04  lwall
# 4.0 baseline.
# 
# 

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

cat='$cat'
cppflags='$cppflags'
cp='$cp'
cppstdin='$cppstdin'
cppminus='$cppminus'
echo='$echo'
egrep='$egrep'
expr='$expr'
mv='$mv'
rm='$rm'
sed='$sed'
sort='$sort'
test='$test'
tr='$tr'
uniq='$uniq'
!GROK!THIS!

$spitshell >>makedepend <<'!NO!SUBS!'

PATH="$PATH:."
export PATH

$cat /dev/null >.deptmp
$rm -f *.c.c c/*.c.c
if test -f Makefile; then
    cp Makefile makefile
fi
mf=makefile
if test -f $mf; then
    defrule=`<$mf sed -n		\
	-e '/^\.c\.o:.*;/{'		\
	-e    's/\$\*\.c//'		\
	-e    's/^[^;]*;[	 ]*//p'	\
	-e    q				\
	-e '}'				\
	-e '/^\.c\.o: *$/{'		\
	-e    N				\
	-e    's/\$\*\.c//'		\
	-e    's/^.*\n[	 ]*//p'		\
	-e    q				\
	-e '}'`
fi
case "$defrule" in
'') defrule='$(CC) -c $(CFLAGS)' ;;
esac

make clist || ($echo "Searching for .c files..."; \
	$echo *.c | $tr ' ' '\012' | $egrep -v '\*' >.clist)
for file in `$cat .clist`; do
# for file in `cat /dev/null`; do
    case "$file" in
    *.c) filebase=`basename $file .c` ;;
    *.y) filebase=`basename $file .y` ;;
    esac
    case "$file" in
    */*) finc="-I`echo $file | sed 's#/[^/]*$##'`" ;;
    *)   finc= ;;
    esac
    $echo "Finding dependencies for $filebase.o."
    ( $echo "#line 1 \"$file\""; \
      $sed -n <$file \
	-e "/^${filebase}_init(/q" \
	-e '/^#/{' \
	-e 's|/\*.*$||' \
	-e 's|\\$||' \
	-e p \
	-e '}' ) >$file.c
    $cppstdin $finc -I/usr/local/include -I. $cppflags $cppminus <$file.c |
    $sed \
	-e 's#\.[0-9][0-9]*\.c#'"$file.c#" \
	-e 's/^[	 ]*#[	 ]*line/#/' \
	-e '/^# *[0-9][0-9]* *[".\/]/!d' \
	-e 's/^.*"\(.*\)".*$/'$filebase'.o: \1/' \
	-e 's/^# *[0-9][0-9]* \(.*\)$/'$filebase'.o: \1/' \
	-e 's|: \./|: |' \
	-e 's|\.c\.c|.c|' | \
    $uniq | $sort | $uniq >> .deptmp
done

$sed <$mf >$mf.new -e '1,/^# AUTOMATICALLY/!d'

make shlist || ($echo "Searching for .SH files..."; \
	$echo *.SH | $tr ' ' '\012' | $egrep -v '\*' >.shlist)
if $test -s .deptmp; then
    for file in `cat .shlist`; do
	$echo `$expr X$file : 'X\(.*\).SH'`: $file config.sh \; \
	    /bin/sh $file >> .deptmp
    done
    $echo "Updating $mf..."
    $echo "# If this runs make out of memory, delete /usr/include lines." \
	>> $mf.new
    $sed 's|^\(.*\.o:\) *\(.*/.*\.c\) *$|\1 \2; '"$defrule \2|" .deptmp \
       >>$mf.new
else
    make hlist || ($echo "Searching for .h files..."; \
	$echo *.h | $tr ' ' '\012' | $egrep -v '\*' >.hlist)
    $echo "You don't seem to have a proper C preprocessor.  Using grep instead."
    $egrep '^#include ' `cat .clist` `cat .hlist`  >.deptmp
    $echo "Updating $mf..."
    <.clist $sed -n							\
	-e '/\//{'							\
	-e   's|^\(.*\)/\(.*\)\.c|\2.o: \1/\2.c; '"$defrule \1/\2.c|p"	\
	-e   d								\
	-e '}'								\
	-e 's|^\(.*\)\.c|\1.o: \1.c|p' >> $mf.new
    <.hlist $sed -n 's|\(.*/\)\(.*\)|s= \2= \1\2=|p' >.hsed
    <.deptmp $sed -n 's|c:#include "\(.*\)".*$|o: \1|p' | \
       $sed 's|^[^;]*/||' | \
       $sed -f .hsed >> $mf.new
    <.deptmp $sed -n 's|c:#include <\(.*\)>.*$|o: /usr/include/\1|p' \
       >> $mf.new
    <.deptmp $sed -n 's|h:#include "\(.*\)".*$|h: \1|p' | \
       $sed -f .hsed >> $mf.new
    <.deptmp $sed -n 's|h:#include <\(.*\)>.*$|h: /usr/include/\1|p' \
       >> $mf.new
    for file in `$cat .shlist`; do
	$echo `$expr X$file : 'X\(.*\).SH'`: $file config.sh \; \
	    /bin/sh $file >> $mf.new
    done
fi
$rm -f $mf.old
$cp $mf $mf.old
$cp $mf.new $mf
$rm $mf.new
$echo "# WARNING: Put nothing here or make depend will gobble it up!" >> $mf
$rm -f .deptmp `sed 's/\.c/.c.c/' .clist` .shlist .clist .hlist .hsed

!NO!SUBS!
$eunicefix makedepend
chmod +x makedepend
case `pwd` in
*SH)
    $rm -f ../makedepend
    ln makedepend ../makedepend
    ;;
esac
