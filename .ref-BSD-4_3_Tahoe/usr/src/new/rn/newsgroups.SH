case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting newsgroups (with variable substitutions)"
$spitshell >newsgroups <<!GROK!THIS!
$startsh
# $Header: newsgroups.SH,v 4.3 85/05/01 11:43:27 lwall Exp $
# 
# $Log:	newsgroups.SH,v $
# Revision 4.3  85/05/01  11:43:27  lwall
# Baseline for release with 4.3bsd.
# 

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

: syntax: newsgroups [pattern] [pipeflag]

: System Dependencies

: You might want to change pager to a "make column" program if you have one.
: On the other hand, if your kernel does paging, cat would probably do.
pager="${pager-/usr/ucb/more}"
active="${active-/usr/lib/news/active}"

: End of system dependencies, hopefully

case \$active in
~*) active=\`$filexp \$active\` ;;
esac

if $test \$# -ge 2 ; then
    pager=$cat
else
    $echo "Completely unsubscribed newsgroups:"
fi

dotdir=\${DOTDIR-\${HOME-\$LOGDIR}}

: Throwing .newsrc into the pot twice is a lovely hack to prevent
: bogus newsgroups from showing up as unsubscribed.

$cat \$dotdir/.newsrc \$dotdir/.newsrc \$active | \\
$sed	-e '/^options/d' \\
	-e '/^[	 ]/d' \\
	-e '/^control/d' \\
	-e '/^to\./d' \\
	-e 's/^\([^ !:]*\)[ !:].*\$/\1/' \\
	-e "/.*\$1/p" \\
	-e 'd' | \\
$sort | $uniq -u | \$pager
if $test \$# -ge 2 ; then
    exit
fi
$echo $n "[Type return to continue] $c"
read tmp
$echo ""
$echo "Unsubscribed but mentioned in .newsrc:"
$sed < \$dotdir/.newsrc \\
	-e "/\$1.*!/"'s/^\([^!]*\)!.*\$/\1/p' \\
	-e 'd' | \\
$sort | \$pager
!GROK!THIS!
$eunicefix newsgroups
chmod 755 newsgroups
