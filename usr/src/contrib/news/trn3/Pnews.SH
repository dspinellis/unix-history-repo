case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting Pnews (with variable substitutions)"
$spitshell >Pnews <<!GROK!THIS!
$startsh
# $Id: Pnews.SH,v 3.0 1992/02/23 21:25:39 davison Trn $
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
#
# syntax: Pnews -h headerfile			or
#	  Pnews -h headerfile oldarticle	or
#         Pnews newsgroup title			or just
#         Pnews

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

# System dependencies

mailer="${mailer-/bin/mail}"
# if you change this to something that does signatures, take out signature code

case $d_portable in
define)
    # where recordings, distributions and moderators are kept
    lib=\`$filexp $newslib\`
    # where important rn things are kept
    rnlib=\`$filexp $privlib\`
    ;;
undef)
    # where recordings, distributions and moderators are kept
    lib="$newslib"
    # where important rn things are kept
    rnlib="$privlib"
    ;;
esac

# your organization name
orgname="$orgname"
# what pager you use--if you have kernal paging use cat
pager="\${PAGER-$pager}"
# how you derive full names, bsd, usg, or other
nametype="$nametype"
# default editor
defeditor="$defeditor"
# how not to echo with newline
n="$n"
c="$c"

# You should also look at the distribution warnings below marked !DIST!
# to make sure any distribution regions you are a member of are included.
# The following are some prototypical distribution groups.  If you do not
# use them all set the unused ones to a non-null string such as 'none'.
loc="$locdist"
org="$orgdist"
multistate="$multistatedist"
city="$citydist"
state="$statedist"
cntry="$cntrydist"
cont="$contdist"

test=${test-test}
sed=${sed-sed}
echo=${echo-echo}
cat=${cat-cat}
egrep=${egrep-egrep}
grep=${grep-grep}
tr=${tr-tr}
inews=${inewsloc-inews}
nidump=${nidump}
ypmatch=${ypmatch}

!GROK!THIS!
case "$ignoreorg" in
define) $spitshell >>Pnews <<'!NO!SUBS!'
orgname=${NEWSORG-$orgname}
!NO!SUBS!
	;;
*)	$spitshell >>Pnews <<'!NO!SUBS!'
orgname=${NEWSORG-${ORGANIZATION-$orgname}}
!NO!SUBS!
	;;
esac
$spitshell >>Pnews <<'!NO!SUBS!'
dotdir=${DOTDIR-${HOME-$LOGDIR}}
tmpart=$dotdir/.article
artcheck=$rnlib/artcheck
speller=$rnlib/Speller

if $test -f $dotdir/.pnewsexpert; then
    expertise=expert
else
    $cat <<'EOM'
I see you've never used this version of Pnews before.  I will give you extra
help this first time through, but then you must remember what you learned.
If you don't understand any question, type h and a CR (carriage return) for
help.

If you've never posted an article to the net before, it is HIGHLY recommended
that you read the netiquette document found in news.announce.newusers so
that you'll know to avoid the commonest blunders.  To do that, interrupt
Pnews, get to the top-level prompt of [t]rn, and use the command
"g news.announce.newusers" to go to that group.

EOM
    expertise=beginner
fi

case $cntry in
  can) Stpr=Province ; stpr=province ;;
  *)   Stpr=State ; stpr=state ;;
esac

case $multistate in
  pnw) multistpr="Pacific NorthWest" ;;
  *)   multistpr="Multi-State Area" ;;
esac

headerfile=""
case $# in
0) ;;
*)  case $1 in
    -h)
	headerfile="$2"
	shift
	shift
	case $# in
	0)
	    oldart=""
	    ;;
	*)
	    oldart="$1"
	    shift
	    ;;
	esac
	;;
    esac
    ;;
esac

case $headerfile in
'')
    . $rnlib/Pnews.header
    ;;
*)
    $cat < $headerfile  > $tmpart
    ;;
esac
    rescue="sleep 1; $cat $tmpart >>${HOME-$LOGDIR}/dead.article ; $echo Article appended to ${HOME-$LOGDIR}/dead.article ; exit"
    trap "$rescue" 1
    trap "$rescue" 2

$echo ""

# extract the newsgroups list and distribution
hdr_newsgroups=`$sed -n -e '/^Newsgroups:/{' -e 's///' -e 's/,/ /g' -e p -e q -e '}' $tmpart`
hdr_distribution=`$sed -n -e '/^Distribution:/{' -e 's///' -e p -e q -e '}' $tmpart`

# check for "poster" magic cookie.  Allow erroneous user@site too.
flag=0
for ng in $hdr_newsgroups ; do
    case "$ng" in
	poster)	flag=1 ;;
	*@*) flag=1 ;;
	*)	;;
    esac
done
case $flag in
1)
    $echo " "
    $echo "The original author has requested that messages be sent back via"
    $echo "mail rather than posting to news.  Do you want to jump out of this"
    $echo $n "and mail your reply instead? [yn] $c"
    read ans
    case $ans in
	n*) ;;
	*)  exit ;;
    esac
    $echo " "
    $echo "OK, but you will have to edit the 'Newsgroups:' line in the message."
    ;;
esac
  
# play recorded message
if $test -s ${lib}/recording ; then
     for ng in $hdr_newsgroups ; do
	_rec1=${lib}/`$sed -n "/^$ng/s/^.*	//p" ${lib}/recording`
	_tmp=`$echo $ng |$sed "s/\..*//"`
	_rec2=${lib}/`$cat -s ${lib}/recording|$grep ${_tmp}.all|$sed "s/^.*	//"`
	if $test -f ${_rec1} ; then
	    $cat -s ${_rec1}
	fi
	if $test -f ${_rec2} ; then
	    $cat -s ${_rec2}
	fi
    done
fi

# determine the distribution of this message
set X $hdr_distribution
shift
if $test $# -gt 0 ; then
    dist=$1.whatever
else
    set X $hdr_newsgroups
    shift
    if $test $# -gt 0 ; then
	dist=$1.whatever
    else
	dist=misc.whatever
    fi
fi
case $dist in
*.*)
    ;;
*)
    dist=$dist.whatever
    ;;
esac

# tell them what we think they are doing... !DIST!
case $dist in
world.*|comp.*|news.*|sci.*|rec.*|misc.*|soc.*|talk.*|alt.*|'')
    $cat <<'EOM'
This program posts news to thousands of machines throughout the entire
civilized world.  Your message will cost the net hundreds if not thousands of
dollars to send everywhere.  Please be sure you know what you are doing.

EOM
    ;;
vmsnet.*)
    $echo 'This program posts news to many machines.'
    ;;
bit.*)
    $echo 'This program posts news to many machines on BITNET.'
    ;;
ddn.*)
    $echo 'This program posts news to many machines throughout the internet.'
    ;;
$cont.*)
    $echo 'This program posts news to many machines throughout the continent.'
    ;;
$cntry.*)
    $echo 'This program posts news to many machines throughout the country.'
    ;;
$multistate.*)
    $echo "This program posts news to many machines throughout the ${multistpr}."
    ;;
$state.*)
    $echo "This program posts news to many machines throughout the ${stpr}."
    ;;
$city.*)
    $echo 'This program posts news to many machines throughout the city.'
    ;;
$org.*)
    $echo 'This program posts news to machines throughout the organization.'
    ;;
$loc.*)
    $echo 'This program posts news to machines throughout the local organization.'
    ;;
*.*)
    $echo 'This program may post news to many machines.'
    ;;
to.*)
    $echo 'This program may post news to a particular machine.'
    ;;
*)
    $echo 'This program posts news to everyone on the machine.'
    ;;
esac
ans=""
while $test "$ans" = "" ; do
    $echo $n "Are you absolutely sure that you want to do this? [ny] $c"
    read ans
    case $ans in
    y*) ;;
    f*) ;;
    h*) $cat <<'EOH'

Type n or CR to exit, y to post.

EOH
	ans="" ;;
    *) exit ;;
    esac
done

file=h
while $test "$file" = h ; do
    $echo ""
    $echo $n "Prepared file to include [none]: $c"
    read file
    case $file in
    h)
	$cat <<'EOH'

If you have already produced the body of your article, type the filename
for it here.  If you just want to proceed directly to the editor, type a
RETURN.  In any event, you will be allowed to edit as many times as you
want before you send off the article.
EOH
	;;
    '')
	$echo "" >> $tmpart
	state=edit
	;;
    *)
	$cat $file >>$tmpart
	state=check
	;;
    esac
done

$echo ""

while true ; do
    case $state in
    edit)
	case $expertise in
	beginner)
	    $cat </dev/null >$dotdir/.pnewsexpert
	    $cat <<'EOMessage'
A temporary file has been created for you to edit.  Be sure to leave at
least one blank line between the header and the body of your message.
(And until a certain bug is fixed all over the net, don't start the body of
your message with any indentation, or it may get eaten.)

Within the header may be fields that you don't understand.  If you don't
understand a field (or even if you do), you can simply leave it blank, and
it will go away when the article is posted.

Type return to get the default editor, or type the name of your favorite
editor.

EOMessage
	    ;;
	esac
	case "${VISUAL-${EDITOR-}}" in
	'')
	    tmp=h
	    ;;
	*)
	    tmp=''
	    ;;
	esac
	while $test "$tmp" = h ; do
	    $echo $n "Editor [${VISUAL-${EDITOR-$defeditor}}]: $c"
	    read tmp
	    case $tmp in
	    h)
		$cat <<'EOH'

Type a return to get the default editor, or type the name of the editor you
prefer.  The default editor depends on the VISUAL and EDITOR environment
variables.

EOH
		;;
	    '')
		;;
	    *)
		VISUAL=$tmp
		export VISUAL
		;;
	    esac
	done
	trap : 2
	${VISUAL-${EDITOR-$defeditor}} $tmpart $oldart
	trap "$rescue" 2
	state=check
	;;
	
    check)
	# warn about long lines, malformed headers, misspelled newsgroups
	$artcheck $tmpart 79 $lib/newsgroups
	state=ask
	;;

    ask)
	$echo ""
	$echo $n "Check spelling, Send, Abort, Edit, or List? $c"
	read ans
	
	case "$ans" in
	a*)
	    state=rescue
	    ;;
	e*)
	    set $ans
	    case $# in
	    2)  VISUAL="$2" ;;
	    esac
	    state=edit
	    ;;
	l*)
	    $pager $tmpart
	    state=ask
	    ;;
	s*)
	    state=send
	    ;;
	c*)
	    $speller $tmpart
	    state=ask
	    ;;
	h*)
	    $cat <<'EOH'

Type c to check the article's spelling, s to send the article, a to abort
and append the article to dead.article, e to edit the article again, or l
to list the article with your pager.

To invoke an alternate editor, type 'e editor'.
EOH
	esac
	;;
    
    send)
	set X `$sed < $tmpart -n -e '/^Newsgroups: /{' -e p -e q -e '}'`
	shift
	case $# in
	2)
	    state=cleanup
	    if $test -f $lib/moderators; then
		tryinews=no
		shift
		case "$1" in
		*,*) set `$echo $1 | tr ',' ' '`;;
		esac
		for newsgroup in $*; do
# the following screwy sed should prevent Eunice from hanging on no match
		    moderator=`$sed <$lib/moderators \
		    -e "/^$newsgroup[ 	]/!s/.*//" \
		    -e "s/^$newsgroup[ 	]//"`
		    case ${moderator}X in
		    X)  tryinews=yes
			;;
		    *)
			$echo Mailing to moderator $moderator
			case "$sign" in
			n*) ;;
			*)
			    if $test -f $dotdir/.signature; then
				$echo $n "Append .signature file? [y] $c"
				read ans
				case $ans in
				''|y*)
				    $echo "-- " >> $tmpart
				    $cat $dotdir/.signature >> $tmpart
				    ;;
				esac
			    fi
			    sign=no
			    ;;
			esac
			case "$mailer" in
			*recmail)
			    $echo To: $moderator | $cat - $tmpart | $mailer
			    ;;
			*)
			    $mailer $moderator < $tmpart
			    ;;
			esac
			case $? in
			0) ;;
			*)
			    $echo Unable to mail to moderator $moderator
			    state=rescue
			    ;;
			esac
			;;
		    esac
		done
	    else
		tryinews=yes
	    fi
	    case "$tryinews" in
	    yes)
		if $sed '1,/^[	 ]*$/{/^[A-Z][-A-Za-z0-9]*:[	 ]*$/d;
			/^X-ORIGINAL-NEWSGROUPS:.*$/d;
			/^[Cc][Cc]:/d;}' $tmpart | $inews -h ; then
		    : null
		else
		    state=rescue
		fi
		cc=`$sed -n '1,/^[	 ]*$/{/^[Cc][Cc]:[	 ][^	 ]/p;}' $tmpart|
		 $sed 's/^[Cc][Cc]:[	 ][	 ]*//'`
		if $test "$cc " != " " ; then
		    set -- $cc
		    case "$mailer" in
		    *recmail)
			$echo To: $cc | $cat - $tmpart | $mailer
			;;
		    *)
			set -- `echo $cc | sed 's/,/ /g'`
			$mailer $@ < $tmpart
			;;
		    esac
		fi
		;;
	    esac
	    ;;
	*)
	    $echo ""
	    $echo "Malformed Newsgroups line."
	    $echo ""
	    sleep 1
	    state=edit
	    ;;
	esac
	;;
    rescue)
	if $test -s $tmpart; then
		$cat $tmpart >> ${HOME-$LOGDIR}/dead.article
		$echo "Article appended to ${HOME-$LOGDIR}/dead.article"
		$echo "A copy may be temporarily found in $tmpart"
	else
		$echo "Null article discarded."
	fi
	exit
	;;
    cleanup)
	case "${AUTHORCOPY-none}" in
	none)
	    ;;
	*)
	    set X ${USER-${LOGNAME-`who am i`}} unknown
	    shift
	    $rnlib/mbox.saver $tmpart "." "." 0 0 Pnews $AUTHORCOPY "From $1 `LANG= date`"
	    if $test $? -eq 0 ; then
		$echo "Article appended to $AUTHORCOPY"
	    else
		$echo "Cannot append to $AUTHORCOPY"
	    fi
	    ;;
	esac
	exit
	;;
    esac
done
!NO!SUBS!
$eunicefix Pnews
chmod 755 Pnews
$spitshell >Pnews.header <<'!NO!SUBS!'
case $# in
0)
    ng=h
    while $test "$ng" = h ; do
	$echo ""
	$echo $n "Newsgroup(s): $c"
	read ng
	case $ng in
	h)
	    $cat <<'EOH'

Type the name of one or more newsgroups to which you wish to post an article.
If you want to post to multiple newsgroups, it is better to do them all at
once than to post to each newsgroup individually, which defeats the news
reading programs' strategies of eliminating duplicates.

Separate multiple newsgroup names with commas.
EOH
	    ;;
	esac
    done
    ;;
*)
    ng=$1
    shift
    ;;
esac
case $ng in
*\ *)
    ng=`$echo "$ng" | $sed 's/[, ] */,/g'`
    ;;
esac
case $ng in
ddn.*)
    defdist=inet
    dist=h
    ;;
*.*)
    defdist=''
    dist=h
    ;;
*)
    defdist=''
    dist=''
    ;;
esac

while $test "$dist" = h ; do
    if $test -f $lib/distributions; then
	$echo " "
	$echo "Your local distribution prefixes are:"
	$cat $lib/distributions
	$echo " "
    else
	$egrep -v '[	 ]none$' <<EOM

Your local distribution prefixes are:
    Local organization:	$loc
    Organization:	$org
    City:		$city
    $Stpr:  		$state
    $multistpr:	$multistate
    Country:		$cntry
    Continent:		$cont
    Everywhere:		<null> (not "world")

EOM
    fi
    $echo $n "Distribution ($defdist): $c"
    read dist
    case $dist in
    '') dist="$defdist" ;;
    esac
    case "$dist" in
    h)
	$cat <<'EOH'

The Distribution line may be used to limit the distribution of an article
to some subset of the systems that would receive the article based only on
the Newsgroups line.  For example, if you want to sell your car in talk.auto,
and you live in New Jersey, you might want to put "nj" on the Distribution
line to avoid advertising in California, which has enough problems of its own.
The actual area designators to use depend on where you are, of course.
EOH
	;;
    world*|comp*|news*|sci*|rec*|misc*|soc*|talk*|alt*)
	dist=''
	;;
    ''|$loc|$org|$city|$state|$multistate|$cntry|$cont|$defdist)
	;;
    *)  
	if $test -f $lib/distributions && \
	  $egrep "^$dist[ 	]" $lib/distributions >$tmpart && \
	  $test -s $tmpart; then
	    : null
	else
	    $echo "Unrecognized distribution prefix--type h for help, CR to use anyway."
	    defdist=$dist
	    dist=h
	fi
	;;
    esac
done

follow=""

# LCP 16-Oct-91 Subject line is required.  Make it a little more
# difficult to omit.  Added "while : ; do", ... "done", and "if"
# at end of while loop.
while :
do
  case $# in
  0)
    title=h
    while $test "$title" = h ; do
	$echo ""
	$echo $n "Title/Subject: $c"
	read title
	case $title in
	h)
	    $cat <<'EOH'

Type the title for your article.  Please make it as informative as possible
(within reason) so that people who aren't interested won't have to read the
article to find out they aren't interested.  This includes marking movie
spoilers as (spoiler), and rotated jokes as (rot 13).
EOH
	;;
	esac
    done
    ;;
  *)
    title="$*"
    # LCP 16-Oct-91 Added "set" and "shift".  Must insure $# is 0
    # in case the title is all white space and we make another
    # pass thru this loop.
    set X
    shift
    ;;
  esac
  if expr "X$title" : "^X[    ]*$" > /dev/null 2>&1
  then
    $cat <<'EOH'

Articles without a "Subject:" line will not be accepted by the News
system.  Please give a Title/Subject line for your article.
EOH
  else
     break
  fi
done


# now build a file with a header for them to edit

set X ${USER-${LOGNAME-`who am i`}}
shift
logname=$1
case $logname in
*!*) logname=`expr "$logname" : '!\(.*\)$'` ;;
esac
case ${NAME-$nametype} in
bsd)
	if $test "$ypmatch" != ""; then
		fullname=`$ypmatch $logname passwd 2>/dev/null | $sed -e "s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^,:;]*\).*"'$'"/\1/"`
	elif $test "$nidump" != ""; then
		fullname=`$nidump passwd / | $sed -e "/^$logname:/{s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^,:;]*\).*"'$'"/\1/" -e "q" -e "}" -e "d"`
	fi
     if $test "$fullname" = ""; then
		fullname=`$sed </etc/passwd -e "/^$logname:/{s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^,:;]*\).*"'$'"/\1/" -e "q" -e "}" -e "d"`
	fi
    case $fullname in
    *'&'*) : GACK
	lname=`$echo $logname | $tr 'a-z' 'A-Z'`
	lname=`$echo $lname $logname | $sed 's/^\(.\)[^ ]* ./\1/'`
	fullname=`$echo "$fullname" | $sed "s/&/${lname}/"`
	;;
    esac
    ;;
usg)
	if $test "$ypmatch" != ""; then
		fullname=`$ypmatch $logname passwd 2>/dev/null | $sed -e "s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^(:]*\).*"'$'"/\1/" -e "s/^.*-//" -e "q"`
	fi
     if $test "$fullname" = ""; then
    fullname=`$sed </etc/passwd -e "/^$logname:/{s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^(:]*\).*"'$'"/\1/" -e "s/^.*-//" -e "q" -e "}" -e "d"`
	fi
    ;;
*)
    fullname=${NAME-`$cat $dotdir/.fullname`}
    ;;
esac

case $orgname in
/*) orgname=`$cat $orgname` ;;
esac

$sed -e '/^Reply-To: $/d' > $tmpart <<EOHeader
Newsgroups: $ng
Subject: $title
Summary: 
Reply-To: $REPLYTO
Followup-To: $follow
Distribution: $dist
Organization: $orgname
Keywords: 
Cc: 

EOHeader

!NO!SUBS!
case "$d_nntp" in
define) sed < Pnews.header -e '/^#NORMAL/d' > Pnews.h.new ;;
*)  sed < Pnews.header -e '/^#NORMAL/s/^#NORMAL//' > Pnews.h.new ;;
esac
mv Pnews.h.new Pnews.header
$eunicefix Pnews.header
