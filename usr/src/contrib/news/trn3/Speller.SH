case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting Speller (with variable substitutions)"
$spitshell >Speller <<!GROK!THIS!
$startsh
# $Id: Speller.SH,v 3.0 1992/02/23 21:25:39 davison Trn $
#	Speller - a script to disassemble a posting; use ispell to spell-
#	check the body along with the Subject, Keywords, and Summary lines;
#	and put it all back together again.
#
#	Written by Eric Schnoebelen, (eric@cirr.com)
#					Fri May 14 20:33:48 CDT 1993

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

# temporary, or is it?
tmpdir=/tmp
# what pager you use--if you have kernal paging use cat
pager="\${PAGER-$pager}"
# either the ispell program or "none"
ispell=$ispell_path
ispell_options=$ispell_options
test=${test-test}
sed=${sed-sed}
echo=${echo-echo}
cat=${cat-cat}
awk=${awk-awk}
rm=${rm-rm}
mv=${mv-mv}
diff=${diff-diff}
ed=${ed-ed}
!GROK!THIS!
$spitshell >>Speller <<'!NO!SUBS!'

# get us some temporary files.
hdrs=$tmpdir/sp$$hdr
body=$tmpdir/sp$$body
sig=$tmpdir/sp$$sig
mine=$tmpdir/sp$$mine
quoted=$tmpdir/sp$$quoted

Cmdname=`basename $0`

if $test "$1 " = " "; then
    $echo "$Cmdname: insufficent arguments" >&2
    $echo "$Cmdname: usage: $Cmdname <filename>" >&2 
    exit 1
fi

trap "$rm -f $hdrs $body $body~ $sig $mine $quoted; exit 1" 0 1 2 15

while $test "$1" != ""; do

    # create the files, so that cat is quiet later..
    >$hdrs
    >$body
    >$sig
    >$mine
    >$quoted

    # tear the wanted headers out and toss them at body, leaving the 
    # the remainder to be put back in later.

    $awk 'BEGIN { inhdr = 1; keephdr = 0; insig = 0 } 
	/^$/		{ 
			inhdr = 0;
			print $0 > Body;
			next;
			}
	/^-- $/		{
			insig = 1;
			print $0 > Sig;
			next;
			}
	/^Subject: /	{ 
			if (inhdr) { 
			    keephdr = 1;
			    print $0 > Body;
			    next;
			}  
			}
	/^Keywords: /	{
			if (inhdr) { 
			    keephdr = 1;
			    print $0 > Body 
			}
			next;
			}
	/^Summary: /	{ if (inhdr) { 
			    keephdr = 1;
			    print $0 > Body 
			}
			next;
			}
	/^[ \t]/	{
			    if (keephdr && indhr) {
				print $0 > Body;
			    } else if (inhdr) {
				print $0 > Hdrs;
			    }
			}
	/^.*: /		{ if (inhdr) { 
			    keephdr = 0;
			    print $0 > Hdrs;
			    next;
			} }
	/^.*$/		{ if (!inhdr && !insig) {
			    print $0 > Body;
			    next;
			} 
			if (insig) {
			    print $0 > Sig;
			    next
			} }
    ' Body=$body Hdrs=$hdrs Sig=$sig $1

    # now rip out the quoted text from the article, so we only
    # spell check our own pristine prose..

    if $test "$QUOTECHARS " = " " ; then
	$mv $body $mine
    else
	$sed -e "/^$QUOTECHARS/s/.*//" $body >$mine
	$diff -e $mine $body > $quoted
    fi

    # ok, we've torn everything asunder, now lets spell check
    # the guts of the article..

    if $test "$ispell " = "none "; then
	($echo ---- mispelled words --------------------------------------
	 spell $mine | fmt
	 $echo -----------------------------------------------------------
	) | $pager
    else
	$ispell $ispell_options $mine
    fi

    if $test $? -ne 0; then
	$echo "$Cmdname: error returned, leaving message untouched"

	# don't want to mess with this file again, either
	shift
	continue
    fi

    # resurrect the body of the article..
    if $test -s $quoted ; then
	($cat $quoted; $echo w $body; $echo q) | $ed - $mine
    else
	$mv $mine $body
    fi

    # ..and re-assemble the article.
    $cat $hdrs $body $sig >$1

    # move to the next filename!
    shift

done
!NO!SUBS!
$eunicefix Speller
chmod 755 Speller
