#! /bin/sh
#
# If these # comments don't work, trim them.  Don't worry about any other
# shell scripts, Configure will trim # comments from them for you.
#
# (If you are trying to port this package to a machine without sh, I would
# suggest you cut out the prototypical config.h from the end of Configure
# and edit it to reflect your system.  Some packages may include samples
# of config.h for certain machines, so you might look for one of those.)
#
# $Header: Head.U,v 1.0 87/05/22 12:28:10 lwall Exp $
#
# Yes, you may rip this off to use in other distribution packages.
# (Note: this Configure script was generated automatically.  Rather than
# working with this copy of Configure, you may wish to get metaconfig.)

: sanity checks
: the user has a better chance than we do of setting a reasonable PATH
: but add some directories we need that are probably not there
PATH=.:${PATH}:/etc:/usr/lib:/lib
export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh $0; kill $$)

if test ! -t 0; then
    echo "Say 'sh Configure', not 'sh <Configure'"
    exit 1
fi

(alias) >/dev/null 2>&1 && \
    echo "(I see you are using the Korn shell.  Some ksh's blow up on Configure," && \
    echo "especially on exotic machines.  If yours does, try the Bourne shell instead.)"

if test ! -d ../UU; then
    if test ! -d UU; then
	mkdir UU
    fi
    cd UU
fi

case "$1" in
-d) shift; fastread='yes';;
esac

d_eunice=''
eunicefix=''
define=''
loclist=''
expr=''
sed=''
echo=''
cat=''
rm=''
mv=''
cp=''
tail=''
tr=''
mkdir=''
sort=''
uniq=''
grep=''
trylist=''
test=''
inews=''
egrep=''
more=''
pg=''
Mcc=''
vi=''
mailx=''
mail=''
cpp=''
Log=''
Header=''
bin=''
cc=''
contains=''
cppstdin=''
cppminus=''
d_charsprf=''
d_flexfnam=''
d_index=''
d_voidsig=''
d_dirheader=''
d_unistd=''
libc=''
mansrc=''
manext=''
models=''
split=''
small=''
medium=''
large=''
huge=''
ccflags=''
ldflags=''
n=''
c=''
package=''
registers=''
reg1=''
reg2=''
reg3=''
reg4=''
reg5=''
reg6=''
reg7=''
reg8=''
reg9=''
reg10=''
reg11=''
reg12=''
reg13=''
reg14=''
reg15=''
reg16=''
spitshell=''
shsharp=''
sharpbang=''
startsh=''
voidflags=''
defvoidused=''
CONFIG=''
: set package name
package=patch

echo " "
echo "Beginning of configuration questions for $package kit."
: Eunice requires " " instead of "", can you believe it
echo " "

define='define'
undef='/*undef'
libpth='/usr/lib /usr/local/lib /lib /usr/ccs/lib'
smallmach='pdp11 i8086 z8000 i80286 iAPX286'
rmlist='kit[1-9]isdone kit[1-9][0-9]isdone'
trap 'echo " "; rm -f $rmlist; exit 1' 1 2 3

: We must find out about Eunice early
eunicefix=':'
if test -f /etc/unixtovms; then
    eunicefix=/etc/unixtovms
fi
if test -f /etc/unixtovms.exe; then
    eunicefix=/etc/unixtovms.exe
fi

attrlist="mc68000 sun gcos unix ibm gimpel interdata tss os mert pyr"
attrlist="$attrlist vax pdp11 i8086 z8000 u3b2 u3b5 u3b20 u3b200"
attrlist="$attrlist ns32000 ns16000 iAPX286 mc300 mc500 mc700 sparc"
attrlist="$attrlist nsc32000 sinix xenix venix posix ansi M_XENIX"
attrlist="$attrlist mc68k m68k __STDC__"
pth=`echo $PATH | tr : ' '`
d_newshome="../../NeWS"
defvoidused=7

: some greps do not return status, grrr.
echo "grimblepritz" >grimble
if grep blurfldyick grimble >/dev/null 2>&1 ; then
    contains=contains
elif grep grimblepritz grimble >/dev/null 2>&1 ; then
    contains=grep
else
    contains=contains
fi
rm -f grimble
: the following should work in any shell
case "$contains" in
contains*)
    echo " "
    echo "AGH!  Grep doesn't return a status.  Attempting remedial action."
    cat >contains <<'EOSS'
grep "$1" "$2" >.greptmp && cat .greptmp && test -s .greptmp
EOSS
chmod +x contains
esac

: first determine how to suppress newline on echo command
echo "Checking echo to see how to suppress newlines..."
(echo "hi there\c" ; echo " ") >.echotmp
if $contains c .echotmp >/dev/null 2>&1 ; then
    echo "...using -n."
    n='-n'
    c=''
else
    cat <<'EOM'
...using \c
EOM
    n=''
    c='\c'
fi
echo $n "Type carriage return to continue.  Your cursor should be here-->$c"
read ans
rm -f .echotmp

: now set up to do reads with possible shell escape and default assignment
cat <<EOSC >myread
case "\$fastread" in
yes) ans=''; echo " " ;;
*) ans='!';;
esac
while expr "X\$ans" : "X!" >/dev/null; do
    read ans
    case "\$ans" in
    !)
	sh
	echo " "
	echo $n "\$rp $c"
	;;
    !*)
	set \`expr "X\$ans" : "X!\(.*\)\$"\`
	sh -c "\$*"
	echo " "
	echo $n "\$rp $c"
	;;
    esac
done
rp='Your answer:'
case "\$ans" in
'') ans="\$dflt";;
esac
EOSC

: general instructions
cat <<EOH
 
This installation shell script will examine your system and ask you questions
to determine how the $package package should be installed.  If you get stuck
on a question, you may use a ! shell escape to start a subshell or execute
a command.  Many of the questions will have default answers in square
brackets--typing carriage return will give you the default.

On some of the questions which ask for file or directory names you are
allowed to use the ~name construct to specify the login directory belonging
to "name", even if you don't have a shell which knows about that.  Questions
where this is allowed will be marked "(~name ok)".

EOH
rp="[Type carriage return to continue]"
echo $n "$rp $c"
. myread
cat <<EOH

Much effort has been expended to ensure that this shell script will run
on any Unix system.  If despite that it blows up on you, your best bet is
to edit Configure and run it again. Also, let me (lwall@netlabs.com)
know how I blew it.  If you can't run Configure for some reason, you'll have
to generate a config.sh file by hand.

This installation script affects things in two ways: 1) it may do direct
variable substitutions on some of the files included in this kit, and
2) it builds a config.h file for inclusion in C programs.  You may edit
any of these files as the need arises after running this script.

If you make a mistake on a question, there is no easy way to back up to it
currently.  The easiest thing to do is to edit config.sh and rerun all the
SH files.  Configure will offer to let you do this before it runs the SH files.

EOH
rp="[Type carriage return to continue]"
echo $n "$rp $c"
. myread

: get old answers, if there is a config file out there
if test -f ../config.sh; then
    echo " "
    dflt=y
    rp="I see a config.sh file.  Did Configure make it on THIS system? [$dflt]"
    echo $n "$rp $c"
    . myread
    case "$ans" in
    n*) echo "OK, I'll ignore it.";;
    *)  echo "Fetching default answers from your old config.sh file..."
	tmp="$n"
	ans="$c"
        . ../config.sh
	n="$tmp"
	c="$ans"
	;;
    esac
fi

: find out where common programs are
echo " "
echo "Locating common programs..."
cat <<EOSC >loc
$startsh
case \$# in
0) exit 1;;
esac
thing=\$1
shift
dflt=\$1
shift
for dir in \$*; do
    case "\$thing" in
    .)
	if test -d \$dir/\$thing; then
	    echo \$dir
	    exit 0
	fi
	;;
    *)
	if test -f \$dir/\$thing; then
	    echo \$dir/\$thing
	    exit 0
	elif test -f \$dir/\$thing.exe; then
	    : on Eunice apparently
	    echo \$dir/\$thing
	    exit 0
	fi
	;;
    esac
done
echo \$dflt
exit 1
EOSC
chmod +x loc
$eunicefix loc
loclist="
expr
sed
echo
cat
rm
tr
grep
"
trylist="
test
Mcc
cpp
"
for file in $loclist; do
    xxx=`loc $file $file $pth`
    eval $file=$xxx
    eval _$file=$xxx
    case "$xxx" in
    /*)
	echo $file is in $xxx.
	;;
    *)
	echo "I don't know where $file is.  I hope it's in everyone's PATH."
	;;
    esac
done
echo " "
echo "Don't worry if any of the following aren't found..."
ans=offhand
for file in $trylist; do
    xxx=`loc $file $file $pth`
    eval $file=$xxx
    eval _$file=$xxx
    case "$xxx" in
    /*)
	echo $file is in $xxx.
	;;
    *)
	echo "I don't see $file out there, $ans."
	ans=either
	;;
    esac
done
case "$egrep" in
egrep)
    echo "Substituting grep for egrep."
    egrep=$grep
    ;;
esac
case "$test" in
test)
    echo "Hopefully test is built into your sh."
    ;;
/bin/test)
    echo " "
    dflt=n
    rp="Is your "'"'"test"'"'" built into sh? [$dflt] (OK to guess)"
    echo $n "$rp $c"
    . myread
    case "$ans" in
    y*) test=test ;;
    esac
    ;;
*)
    test=test
    ;;
esac
case "$echo" in
echo)
    echo "Hopefully echo is built into your sh."
    ;;
/bin/echo)
    echo " "
    echo "Checking compatibility between /bin/echo and builtin echo (if any)..."
    $echo $n "hi there$c" >foo1
    echo $n "hi there$c" >foo2
    if cmp foo1 foo2 >/dev/null 2>&1; then
	echo "They are compatible.  In fact, they may be identical."
    else
	case "$n" in
	'-n') n='' c='\c' ans='\c' ;;
	*) n='-n' c='' ans='-n' ;;
	esac
	cat <<FOO
They are not compatible!  You are probably running ksh on a non-USG system.
I'll have to use /bin/echo instead of the builtin, since Bourne shell doesn't
have echo built in and we may have to run some Bourne shell scripts.  That
means I'll have to use $ans to suppress newlines now.  Life is ridiculous.

FOO
	rp="Your cursor should be here-->"
	$echo $n "$rp$c"
	. myread
    fi
    $rm -f foo1 foo2
    ;;
*)
    : cross your fingers
    echo=echo
    ;;
esac
rmlist="$rmlist loc"

: get list of predefined functions in a handy place
echo " "
if test -f /lib/libc.a; then
    echo "Your C library is in /lib/libc.a.  You're normal."
    libc=/lib/libc.a
else
    if test -f /lib/clib -a -f /lib/libc; then
	echo "Your standard C library is in /lib/libc.  Must be Domain/OS."
	libc=/lib/libc
    elif test -f /lib/libsys_s.a; then
	echo "Your C library is in /lib/libsys_s.a.  Must be a NeXT."
	libc=/lib/libsys_s.a
    else
        ans=`loc libc.a blurfl/dyick $libpth`
        if test ! -f $ans; then
	    ans=`loc clib blurfl/dyick $libpth`
        fi
        if test ! -f $ans; then
	     ans=`loc libc blurfl/dyick $libpth`
        fi
        if test -f $ans; then
	    echo "Your C library is in $ans, of all places."
	    libc=$ans
        else
	    if test -f "$libc"; then
	        echo "Your C library is in $libc, like you said before."
	    else
	        cat <<EOM
 
I can't seem to find your C library.  I've looked in the following places:

	$libpth

None of these seems to contain your C library.  What is the full name
EOM
	        dflt=None
	        $echo $n "of your C library? $c"
	        rp='C library full name?'
	        . myread
	        libc="$ans"
	    fi
        fi
    fi
fi
echo " "
$echo $n "Extracting names from $libc for later perusal...$c"
nm $libc 2>/dev/null | sed -n -e 's/^.* T _//p' -e 's/^.* T //p' > libc.list
if $contains '^printf$' libc.list >/dev/null 2>&1; then
    echo "done"
else
    nm $libc 2>/dev/null | sed -n -e 's/^.* D _//p' -e 's/^.* D //p' > libc.list
    if $contains '^printf$' libc.list >/dev/null 2>&1; then
	echo "done"
    else
	echo " "
	echo "nm didn't seem to work right."
	echo "Trying ar instead..."
	if ar t $libc | sed -e 's/\.o$//' > libc.list; then
	    echo "Ok."
	else
	    echo "That didn't work either.  Giving up."
	    exit 1
	fi
    fi
fi
rmlist="$rmlist libc.list"

: make some quick guesses about what we are up against
echo " "
$echo $n "Hmm...  $c"
if $contains SIGTSTP /usr/include/signal.h >/dev/null 2>&1 ; then
    echo "Looks kind of like a BSD system, but we'll see..."
    echo exit 0 >bsd
    echo exit 1 >usg
    echo exit 1 >v7
elif $contains '^fcntl$' libc.list >/dev/null 2>&1 ; then
    echo "Looks kind of like a USG system, but we'll see..."
    echo exit 1 >bsd
    echo exit 0 >usg
    echo exit 1 >v7
else
    echo "Looks kind of like a version 7 system, but we'll see..."
    echo exit 1 >bsd
    echo exit 1 >usg
    echo exit 0 >v7
fi
if $contains '^vmssystem$' libc.list >/dev/null 2>&1 ; then
    cat <<'EOI'
There is, however, a strange, musty smell in the air that reminds me of
something...hmm...yes...I've got it...there's a VMS nearby, or I'm a Blit.
EOI
    echo "exit 0" >eunice
    eunicefix=unixtovms
    d_eunice="$define"
: it so happens the Eunice I know will not run shell scripts in Unix format
else
    echo " "
    echo "Congratulations.  You aren't running Eunice."
    eunicefix=':'
    d_eunice="$undef"
    echo "exit 1" >eunice
fi
if test -f /xenix; then
    echo "Actually, this looks more like a XENIX system..."
    echo "exit 0" >xenix
else
    echo " "
    echo "It's not Xenix..."
    echo "exit 1" >xenix
fi
chmod +x xenix
$eunicefix xenix
if test -f /venix; then
    echo "Actually, this looks more like a VENIX system..."
    echo "exit 0" >venix
else
    echo " "
    if xenix; then
	: null
    else
	echo "Nor is it Venix..."
    fi
    echo "exit 1" >venix
fi
chmod +x bsd usg v7 eunice venix
$eunicefix bsd usg v7 eunice venix
rmlist="$rmlist bsd usg v7 eunice venix xenix"

: see if sh knows # comments
echo " "
echo "Checking your sh to see if it knows about # comments..."
if sh -c '#' >/dev/null 2>&1 ; then
    echo "Your sh handles # comments correctly."
    shsharp=true
    spitshell=cat
    echo " "
    echo "Okay, let's see if #! works on this system..."
    echo "#!/bin/echo hi" > try
    $eunicefix try
    chmod +x try
    try > today
    if $contains hi today >/dev/null 2>&1; then
	echo "It does."
	sharpbang='#!'
    else
	echo "#! /bin/echo hi" > try
	$eunicefix try
	chmod +x try
	try > today
	if test -s today; then
	    echo "It does."
	    sharpbang='#! '
	else
	    echo "It doesn't."
	    sharpbang=': use '
	fi
    fi
else
    echo "Your sh doesn't grok # comments--I will strip them later on."
    shsharp=false
    echo "exec grep -v '^#'" >spitshell
    chmod +x spitshell
    $eunicefix spitshell
    spitshell=`pwd`/spitshell
    echo "I presume that if # doesn't work, #! won't work either!"
    sharpbang=': use '
fi

: figure out how to guarantee sh startup
echo " "
echo "Checking out how to guarantee sh startup..."
startsh=$sharpbang'/bin/sh'
echo "Let's see if '$startsh' works..."
cat >try <<EOSS
$startsh
set abc
test "$?abc" != 1
EOSS

chmod +x try
$eunicefix try
if try; then
    echo "Yup, it does."
else
    echo "Nope.  You may have to fix up the shell scripts to make sure sh runs them."
fi
rm -f try today

#: see if sprintf is declared as int or pointer to char
#echo " "
#cat >.ucbsprf.c <<'EOF'
#main() { char buf[10]; exit((unsigned long)sprintf(buf,"%s","foo") > 10L); }
#EOF
#if cc .ucbsprf.c -o .ucbsprf >/dev/null 2>&1 && .ucbsprf; then
#    echo "Your sprintf() returns (int)."
#    d_charsprf="$undef"
#else
#    echo "Your sprintf() returns (char*)."
#    d_charsprf="$define"
#fi
#/bin/rm -f .ucbsprf.c .ucbsprf

: see if we can have long filenames
echo " "
rm -f 123456789abcde
if (echo hi >123456789abcdef) 2>/dev/null; then
    : not version 8
    if test -f 123456789abcde; then
	echo 'You cannot have filenames longer than 14 characters.  Sigh.'
	d_flexfnam="$undef"
    else
	echo 'You can have filenames longer than 14 characters.'
	d_flexfnam="$define"
    fi
else
    : version 8 probably
    echo "You can't have filenames longer than 14 chars.  V8 can't even think about them!"
    d_flexfnam="$undef"
fi 
: index or strcpy
echo " "
case "$d_index" in
n) dflt=n;;
*) dflt=y;;
esac
if $contains '^index$' libc.list >/dev/null 2>&1 ; then
    if $contains '^strchr$' libc.list >/dev/null 2>&1 ; then
	echo "Your system has both index() and strchr().  Shall I use"
	rp="index() rather than strchr()? [$dflt]"
	$echo $n "$rp $c"
	. myread
	case "$ans" in
	    n*) d_index="$define" ;;
	    *)  d_index="$undef" ;;
	esac
    else
	d_index="$undef"
	echo "index() found."
    fi
else
    if $contains '^strchr$' libc.list >/dev/null 2>&1 ; then
	d_index="$define"
	echo "strchr() found."
    else
	echo "No index() or strchr() found!"
	d_index="$undef"
    fi
fi

: see if signal is declared as pointer to function returning int or void
echo " "
if $contains 'void.*signal' /usr/include/signal.h >/dev/null 2>&1 ||
$contains 'void.*signal' /usr/include/sys/signal.h >/dev/null 2>&1 ; then
    echo "You have void (*signal())() instead of int."
    d_voidsig="$define"
else
    echo "You have int (*signal())() instead of void."
    d_voidsig="$undef"
fi

: check for directory library
echo " "
if test -f /usr/include/dirent.h; then
    echo "You have dirent.h."
    d_dirheader="#define DIRENT"
elif test -f /usr/include/ndir.h; then
    echo "You have ndir.h."
    d_dirheader="#define USG"
elif test -f /usr/include/sys/ndir.h; then
    echo "You have sys/ndir.h."
    d_dirheader="#define SYSNDIR
#define USG"
elif test -f /usr/include/sys/dir.h; then
    echo "You have sys/dir.h; I hope that's the BSD version."
    d_dirheader="#define SYSDIR"
else
    echo "I can't find a directory library header file.
That means you won't have numbered backups available."
    d_dirheader="#define NODIR"
fi

: check for unistd.h
echo " "
if test -f /usr/include/unistd.h; then
    echo "You have unistd.h."
    d_unistd="$define"
else
    echo "I don't see unistd.h, but that's OK."
    d_unistd="$undef"
fi

: check for void type
echo " "
$cat <<EOM
Checking to see how well your C compiler groks the void type...

  Support flag bits are:
    1: basic void declarations.
    2: arrays of pointers to functions returning void.
    4: operations between pointers to and addresses of void functions.

EOM
case "$voidflags" in
'')
    $cat >try.c <<'EOCP'
#if TRY & 1
void main() {
#else
main() {
#endif
	extern void *moo();
	void *(*goo)();
#if TRY & 2
	void (*foo[10])();
#endif

#if TRY & 4
	if(*goo == moo) {
		exit(0);
	}
#endif
	exit(0);
}
EOCP
    : Argh -- AIX 3.2 does not have cc -S!
    if cc -c -DTRY=7 try.c >.out 2>&1 ; then
	voidflags=7
	echo "It appears to support void fully."
	if $contains warning .out >/dev/null 2>&1; then
	    echo "However, you might get some warnings that look like this:"
	    $cat .out
	fi
    else
	echo "Hmm, you compiler has some difficulty with void.  Checking further..."
	if cc -c -DTRY=1 try.c >/dev/null 2>&1 ; then
	    echo "It supports 1..."
	    if cc -c -DTRY=3 try.c >/dev/null 2>&1 ; then
		voidflags=3
		echo "And it supports 2 but not 4."
	    else
		echo "It doesn't support 2..."
		if cc -c -DTRY=3 try.c >/dev/null 2>&1 ; then
		    voidflags=5
		    echo "But it supports 4."
		else
		    voidflags=1
		    echo "And it doesn't support 4."
		fi
	    fi
	else
	    echo "There is no support at all for void."
	    voidflags=0
	fi
    fi
esac
dflt="$voidflags";
rp="Your void support flags add up to what? [$dflt]"
$echo $n "$rp $c"
. myread
voidflags="$ans"
$rm -f try.* .out

: see how we invoke the C preprocessor
echo " "
echo "Now, how can we feed standard input to your C preprocessor..."
cat <<'EOT' >testcpp.c
#define ABC abc
#define XYZ xyz
ABC.XYZ
EOT
echo 'Maybe "'$cpp'" will work...'
$cpp <testcpp.c >testcpp.out 2>&1
if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
    echo "Yup, it does."
    cppstdin="$cpp"
    cppminus='';
else
    echo 'Nope, maybe "'$cpp' -" will work...'
    $cpp - <testcpp.c >testcpp.out 2>&1
    if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
	echo "Yup, it does."
	cppstdin="$cpp"
	cppminus='-';
    else
	echo 'No such luck...maybe "cc -E" will work...'
	cc -E <testcpp.c >testcpp.out 2>&1
	if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
	    echo "It works!"
	    cppstdin='cc -E'
	    cppminus='';
	else
	    echo 'Nixed again...maybe "cc -E -" will work...'
	    cc -E - <testcpp.c >testcpp.out 2>&1
	    if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
		echo "Hooray, it works!  I was beginning to wonder."
		cppstdin='cc -E'
		cppminus='-';
	    else
		echo 'Nope...maybe "cc -P" will work...'
		cc -P <testcpp.c >testcpp.out 2>&1
		if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
		    echo "Yup, that does."
		    cppstdin='cc -P'
		    cppminus='';
		else
		    echo 'Nope...maybe "cc -P -" will work...'
		    cc -P - <testcpp.c >testcpp.out 2>&1
		    if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
			echo "Yup, that does."
			cppstdin='cc -P'
			cppminus='-';
		    else
			echo 'Hmm...perhaps you already told me...'
			case "$cppstdin" in
			'') ;;
			*) $cppstdin $cppminus <testcpp.c >testcpp.out 2>&1;;
			esac
			if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
			    echo "Hooray, you did!  I was beginning to wonder."
			else
			    echo 'Uh-uh.  Time to get fancy...'
			    echo 'Trying (cat >/tmp/$$.c; cc -E /tmp/$$.c; rm /tmp/$$.c)'
			    cppstdin='(cat >/tmp/$$.c; cc -E /tmp/$$.c; rm /tmp/$$.c)'
			    cppminus='';
			    $cppstdin <testcpp.c >testcpp.out 2>&1
			    if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
				echo "Eureka!."
			    else
				dflt=blurfl
				$echo $n "No dice.  I can't find a C preprocessor.  Name one: $c"
				rp='Name a C preprocessor:'
				. myread
				cppstdin="$ans"
				$cppstdin <testcpp.c >testcpp.out 2>&1
				if $contains 'abc.*xyz' testcpp.out >/dev/null 2>&1 ; then
				    echo "OK, that will do."
				else
				    echo "Sorry, I can't get that to work.  Go find one."
				    exit 1
				fi
			    fi
			fi
		    fi
		fi
	    fi
	fi
    fi
fi
rm -f testcpp.c testcpp.out

: get C preprocessor symbols handy
echo " "
: AIX 3.2 rejects tr '[ - ]' because the range endpoints are the same.
echo $attrlist | $tr ' ' '\012' >Cppsym.know
$cat <<EOSS >Cppsym
$startsh
case "\$1" in
-l) list=true
    shift
    ;;
esac
unknown=''
case "\$list\$#" in
1|2)
    for sym do
	if $contains "^\$1$" Cppsym.true >/dev/null 2>&1; then
	    exit 0
	elif $contains "^\$1$" Cppsym.know >/dev/null 2>&1; then
		:
	else
	    unknown="\$unknown \$sym"
	fi
    done
    set X \$unknown
    shift
    ;;
esac
case \$# in
0) exit 1;;
esac
echo \$* | $tr ' ' '\012' | $sed -e 's/\(.*\)/\\
#ifdef \1\\
exit 0; _ _ _ _\1\\	 \1\\
#endif\\
/' >/tmp/Cppsym\$\$
echo exit 1 >>/tmp/Cppsym\$\$
$cppstdin $cppminus </tmp/Cppsym\$\$ >/tmp/Cppsym2\$\$
case "\$list" in
true) awk 'NF > 5 {print substr(\$6,2,100)}' </tmp/Cppsym2\$\$ ;;
*)
    sh /tmp/Cppsym2\$\$
    status=\$?
    ;;
esac
$rm -f /tmp/Cppsym\$\$ /tmp/Cppsym2\$\$
exit \$status
EOSS
chmod +x Cppsym
$eunicefix Cppsym
echo "Your C preprocessor defines the following symbols:"
Cppsym -l $attrlist >Cppsym.true
cat Cppsym.true
rmlist="$rmlist Cppsym Cppsym.know Cppsym.true"

: see how many register declarations we want to use
case "$registers" in
'')
    if Cppsym vax; then
	dflt=6
    elif Cppsym sun mc68000 mc68k m68k; then
	dflt=10
    elif Cppsym pyr; then
	dflt=14
    elif Cppsym ns32000 ns16000; then
	dflt=5
    elif Cppsym $smallmach; then
	dflt=3
    else
	: if you have any other numbers for me, send them in
	dflt=6
    fi
    ;;
*)  dflt=$registers ;;
esac
cat <<EOM
 
Different C compilers on different machines pay attention to different
numbers of register declarations.  About how many register declarations in
EOM
$echo $n "each routine does your C compiler pay attention to? (OK to guess) [$dflt] $c"
rp="# register declarations used? [$dflt]"
. myread
registers=$ans
reg1=''
awk "BEGIN { for (i=1; i<=16; i++) printf \"reg%d=''\n\", i}" </dev/null >.foo
. .foo
awk "BEGIN { for (i=1; i<=$registers; i++) printf \"reg%d=register\n\", i}" \
	</dev/null >.foo
. .foo
rm -f .foo

: preserve RCS keywords in files with variable substitution, grrr
Log='$Log'
Header='$Header'

: set up shell script to do ~ expansion
cat >filexp <<EOSS
$startsh
: expand filename
case "\$1" in
 ~/*|~)
    echo \$1 | $sed "s|~|\${HOME-\$LOGDIR}|"
    ;;
 ~*)
    if $test -f /bin/csh; then
	/bin/csh -f -c "glob \$1"
	echo ""
    else
	name=\`$expr x\$1 : '..\([^/]*\)'\`
	dir=\`$sed -n -e "/^\${name}:/{s/^[^:]*:[^:]*:[^:]*:[^:]*:[^:]*:\([^:]*\).*"'\$'"/\1/" -e p -e q -e '}' </etc/passwd\`
	if $test ! -d "\$dir"; then
	    me=\`basename \$0\`
	    echo "\$me: can't locate home directory for: \$name" >&2
	    exit 1
	fi
	case "\$1" in
	*/*)
	    echo \$dir/\`$expr x\$1 : '..[^/]*/\(.*\)'\`
	    ;;
	*)
	    echo \$dir
	    ;;
	esac
    fi
    ;;
*)
    echo \$1
    ;;
esac
EOSS
chmod +x filexp
$eunicefix filexp

: determine where public executables go
case "$bin" in
'')
    dflt=`loc . /bin /usr/local/bin /usr/lbin /usr/local /usr/bin`
    ;;
*)  dflt="$bin"
    ;;
esac
cont=true
while $test "$cont" ; do
    echo " "
    rp="Where do you want to put the public executables? [$dflt]"
    $echo $n "$rp $c"
    . myread
    bin="$ans"
    bin=`filexp $bin`
    if test -d $bin; then
	cont=''
    else
	dflt=n
	rp="Directory $bin doesn't exist.  Use that name anyway? [$dflt]"
	$echo $n "$rp $c"
	. myread
	dflt=''
	case "$ans" in
	y*) cont='';;
	esac
    fi
done

: determine where manual pages go
case "$mansrc" in
'')
    dflt=`loc . /usr/man/man1 /usr/local/man/man1 /usr/man/mann /usr/man/local/man1 /usr/man/u_man/man1`
    ;;
*)  dflt="$mansrc"
    ;;
esac
cont=true
while $test "$cont" ; do
    echo " "
    rp="Where do the manual pages (source) go? [$dflt]"
    $echo $n "$rp $c"
    . myread
    mansrc=`filexp "$ans"`
    if test -d $mansrc; then
	cont=''
    else
	dflt=n
	rp="Directory $mansrc doesn't exist.  Use that name anyway? [$dflt]"
	$echo $n "$rp $c"
	. myread
	dflt=''
	case "$ans" in
	y*) cont='';;
	esac
    fi
done
case "$mansrc" in
*l)
    manext=l
    ;;
*n)
    manext=n
    ;;
*C)
    manext=C
    ;;
*)
    manext=1
    ;;
esac

: see what memory models we can support
case "$models" in
'')
    if Cppsym pdp11; then
	dflt='unsplit split'
    else
	ans=`loc . X /lib/small /lib/large /usr/lib/small /usr/lib/large /lib/medium /usr/lib/medium /lib/huge`
	case "$ans" in
	X) dflt='none';;
	*)  if $test -d /lib/small || $test -d /usr/lib/small; then
		dflt='small'
	    else
		dflt=''
	    fi
	    if $test -d /lib/medium || $test -d /usr/lib/medium; then
		dflt="$dflt medium"
	    fi
	    if $test -d /lib/large || $test -d /usr/lib/large; then
		dflt="$dflt large"
	    fi
	    if $test -d /lib/huge || $test -d /usr/lib/huge; then
		dflt="$dflt huge"
	    fi
	esac
    fi
    ;;
*)  dflt="$models" ;;
esac
$cat <<EOM
 
Some systems have different model sizes.  On most systems they are called
small, medium, large, and huge.  On the PDP11 they are called unsplit and
split.  If your system doesn't support different memory models, say "none".
If you wish to force everything to one memory model, say "none" here and
put the appropriate flags later when it asks you for other cc and ld flags.
Venix systems may wish to put "none" and let the compiler figure things out.
(In the following question multiple model names should be space separated.)

EOM
rp="Which models are supported? [$dflt]"
$echo $n "$rp $c"
. myread
models="$ans"

case "$models" in
none)
    small=''
    medium=''
    large=''
    huge=''
    unsplit=''
    split=''
    ;;
*split)
    case "$split" in
    '') 
	if $contains '-i' $mansrc/ld.1 >/dev/null 2>&1 || \
	   $contains '-i' $mansrc/cc.1 >/dev/null 2>&1; then
	    dflt='-i'
	else
	    dflt='none'
	fi
	;;
    *) dflt="$split";;
    esac
    rp="What flag indicates separate I and D space? [$dflt]"
    $echo $n "$rp $c"
    . myread
    case "$ans" in
    none) ans='';;
    esac
    split="$ans"
    unsplit=''
    ;;
*large*|*small*|*medium*|*huge*)
    case "$model" in
    *large*)
	case "$large" in
	'') dflt='-Ml';;
	*) dflt="$large";;
	esac
	rp="What flag indicates large model? [$dflt]"
	$echo $n "$rp $c"
	. myread
	case "$ans" in
	none) ans='';
	esac
	large="$ans"
	;;
    *) large='';;
    esac
    case "$model" in
    *huge*)
	case "$huge" in
	'') dflt='-Mh';;
	*) dflt="$huge";;
	esac
	rp="What flag indicates huge model? [$dflt]"
	$echo $n "$rp $c"
	. myread
	case "$ans" in
	none) ans='';
	esac
	huge="$ans"
	;;
    *) huge="$large";;
    esac
    case "$model" in
    *medium*)
	case "$medium" in
	'') dflt='-Mm';;
	*) dflt="$medium";;
	esac
	rp="What flag indicates medium model? [$dflt]"
	$echo $n "$rp $c"
	. myread
	case "$ans" in
	none) ans='';
	esac
	medium="$ans"
	;;
    *) medium="$large";;
    esac
    case "$model" in
    *small*)
	case "$small" in
	'') dflt='none';;
	*) dflt="$small";;
	esac
	rp="What flag indicates small model? [$dflt]"
	$echo $n "$rp $c"
	. myread
	case "$ans" in
	none) ans='';
	esac
	small="$ans"
	;;
    *) small='';;
    esac
    ;;
*)
    echo "Unrecognized memory models--you may have to edit Makefile.SH"
    ;;
esac

case "$ccflags" in
'') dflt='none';;
*) dflt="$ccflags";;
esac
echo " "
rp="Any additional cc flags? [$dflt]"
$echo $n "$rp $c"
. myread
case "$ans" in
none) ans='';
esac
ccflags="$ans"

case "$ldflags" in
'') if venix; then
	dflt='-i -z'
    elif xenix; then
	if test -f /usr/include/dirent.h; then
	    dflt='-ldir -lx'
	else
	    dflt='-lx'
	fi
    else
	dflt='none'
    fi
    ;;
*) dflt="$ldflags";;
esac
echo " "
rp="Any additional ld flags? [$dflt]"
$echo $n "$rp $c"
. myread
case "$ans" in
none) ans='';
esac
ldflags="$ans"

: see if we need a special compiler
echo " "
if usg; then
    case "$cc" in
    '')
	case "$Mcc" in
	/*) dflt='Mcc'
	    ;;
	*)
	    case "$large" in
	    -M*)
		dflt='cc'
		;;
	    *)
		if $contains '\-M' $mansrc/cc.1 >/dev/null 2>&1 ; then
		    dflt='cc -M'
		else
		    dflt='cc'
		fi
		;;
	    esac
	    ;;
	esac
	;;
    *)  dflt="$cc";;
    esac
    $cat <<'EOM'
 
On some systems the default C compiler will not resolve multiple global
references that happen to have the same name.  On some such systems the
"Mcc" command may be used to force these to be resolved.  On other systems
a "cc -M" command is required.  (Note that the -M flag on other systems
indicates a memory model to use!)  What command will force resolution on
EOM
    $echo $n "this system? [$dflt] $c"
    rp="Command to resolve multiple refs? [$dflt]"
    . myread
    cc="$ans"
else
    echo "Not a USG system--assuming cc can resolve multiple definitions."
    cc=cc
fi

echo " "
echo "End of configuration questions."
echo " "

: create config.sh file
echo " "
if test -d ../UU; then
    cd ..
fi
echo "Creating config.sh..."
$spitshell <<EOT >config.sh
$startsh
# config.sh
# This file was produced by running the Configure script.

d_eunice='$d_eunice'
eunicefix='$eunicefix'
define='$define'
loclist='$loclist'
expr='$expr'
sed='$sed'
echo='$echo'
cat='$cat'
rm='$rm'
mv='$mv'
cp='$cp'
tail='$tail'
tr='$tr'
mkdir='$mkdir'
sort='$sort'
uniq='$uniq'
grep='$grep'
trylist='$trylist'
test='$test'
inews='$inews'
egrep='$egrep'
more='$more'
pg='$pg'
Mcc='$Mcc'
vi='$vi'
mailx='$mailx'
mail='$mail'
cpp='$cpp'
Log='$Log'
Header='$Header'
bin='$bin'
cc='$cc'
contains='$contains'
cppstdin='$cppstdin'
cppminus='$cppminus'
d_charsprf='$d_charsprf'
d_flexfnam='$d_flexfnam'
d_index='$d_index'
d_voidsig='$d_voidsig'
d_dirheader='$d_dirheader'
d_unistd='$d_unistd'
libc='$libc'
mansrc='$mansrc'
manext='$manext'
models='$models'
split='$split'
small='$small'
medium='$medium'
large='$large'
huge='$huge'
ccflags='$ccflags'
ldflags='$ldflags'
n='$n'
c='$c'
package='$package'
registers='$registers'
reg1='$reg1'
reg2='$reg2'
reg3='$reg3'
reg4='$reg4'
reg5='$reg5'
reg6='$reg6'
reg7='$reg7'
reg8='$reg8'
reg9='$reg9'
reg10='$reg10'
reg11='$reg11'
reg12='$reg12'
reg13='$reg13'
reg14='$reg14'
reg15='$reg15'
reg16='$reg16'
spitshell='$spitshell'
shsharp='$shsharp'
sharpbang='$sharpbang'
startsh='$startsh'
voidflags='$voidflags'
defvoidused='$defvoidused'
CONFIG=true
EOT

CONFIG=true

echo " "
dflt=''
fastread=''
echo "If you didn't make any mistakes, then just type a carriage return here."
rp="If you need to edit config.sh, do it as a shell escape here:"
$echo $n "$rp $c"
. UU/myread
case "$ans" in
'') ;;
*) : in case they cannot read
    eval $ans;;
esac
. ./config.sh

echo " "
echo "Doing variable substitutions on .SH files..."
set x `awk '{print $1}' <MANIFEST | $grep '\.SH'`
shift
case $# in
0) set x *.SH; shift;;
esac
if test ! -f $1; then
    shift
fi
for file in $*; do
    case "$file" in
    */*)
	dir=`$expr X$file : 'X\(.*\)/'`
	file=`$expr X$file : 'X.*/\(.*\)'`
	(cd $dir && . $file)
	;;
    *)
	. $file
	;;
    esac
done
if test -f config.h.SH; then
    if test ! -f config.h; then
	: oops, they left it out of MANIFEST, probably, so do it anyway.
	. config.h.SH
    fi
fi

if $contains '^depend:' Makefile >/dev/null 2>&1; then
    dflt=n
    $cat <<EOM

Now you need to generate make dependencies by running "make depend".
You might prefer to run it in background: "make depend > makedepend.out &"
It can take a while, so you might not want to run it right now.

EOM
    rp="Run make depend now? [$dflt]"
    $echo $n "$rp $c"
    . UU/myread
    case "$ans" in
    y*) make depend
	echo "Now you must run a make."
	;;
    *)  echo "You must run 'make depend' then 'make'."
	;;
    esac
elif test -f Makefile; then
    echo " "
    echo "Now you must run a make."
else
    echo "Done."
fi

$rm -f kit*isdone
: the following is currently useless
cd UU && $rm -f $rmlist
: since this removes it all anyway
cd .. && $rm -rf UU
: end of Configure
