case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting Pnews (with variable substitutions)"
$spitshell >Pnews <<!GROK!THIS!
$startsh
# $Header: Pnews.SH,v 4.3.1.4 86/09/05 15:21:10 lwall Exp $
#
# $Log:	Pnews.SH,v $
# Revision 4.3.1.4  86/09/05  15:21:10  lwall
# Changes for new top-level newsgroup names.
# 
# Revision 4.3.1.3  85/08/01  14:24:06  lwall
# Added AUTHORCOPY.  Temp file is no longer in /tmp.  'e editor' added.
# 
# Revision 4.3.1.2  85/05/17  10:36:46  lwall
# Removed some extra backslashes.
# 
# Revision 4.3.1.1  85/05/10  11:30:21  lwall
# Branch for patches.
# 
# Revision 4.3  85/05/01  12:20:33  lwall
# Baseline for release with 4.3bsd.
# 
#
# syntax: Pnews -h headerfile			or
#	  Pnews -h headerfile oldarticle	or
#         Pnews newsgroup title			or just
#         Pnews

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

# System dependencies

mailer="${mailer-/bin/mail}"
# if you change this to something that does signatures, take out signature code

case $portable in
define)
# your site name
sitename=\`$hostcmd\`
# where recordings, distributions and moderators are kept
lib=\`$filexp $lib\`
# where important rn things are kept
rnlib=\`$filexp $rnlib\`
;;
undef)
# your site name
sitename="$sitename"
# where recordings, distributions and moderators are kept
lib="$lib"
# where important rn things are kept
rnlib="$rnlib"
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
loc="$locpref"
org="$orgpref"
city="$citypref"
state="$statepref"
cntry="$cntrypref"
cont="$contpref"

test=${test-test}
sed=${sed-sed}
echo=${echo-echo}
cat=${cat-cat}
egrep=${egrep-egrep}
grep=${grep-grep}
rm=${rm-rm}
tr=${tr-tr}
inews=${inews-inews}

!GROK!THIS!
$spitshell >>Pnews <<'!NO!SUBS!'
dotdir=${DOTDIR-${HOME-$LOGDIR}}
tmpart=$dotdir/.article

if $test -f $dotdir/.pnewsexpert; then
    expertise=expert
else
    $cat <<'EOM'
I see you've never used this version of Pnews before.  I will give you extra
help this first time through, but then you must remember what you learned.
If you don't understand any question, type h and a CR (carriage return) for
help.

If you've never posted an article to the net before, it is HIGHLY recommended
that you read the netiquette document found in mod.announce.newusers so
that you'll know to avoid the commonest blunders.  To do that, interrupt
Pnews, and get to the top-level prompt of rn.  Say "g mod.announce.newusers"
and you are on your way.

EOM
    expertise=beginner
fi

case $cntry in
  can) stpr=Province ;;
  *)   stpr=State ;;
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
set X `$sed < $tmpart -n -e '/^Distribution: /{' -e p -e q -e '}' -e '/^$/q'`
shift
case $# in
0|1)
    set X `$sed < $tmpart -n -e '/^Newsgroups: /{' -e p -e q -e '}'`
    shift
    case $# in
    0|1)
	set "x misc.whatever"
	;;
    esac
    ;;
*)
    set $1 $2.whatever
    ;;
esac
shift

#: play recorded message
#if $test -s ${lib}/recording ; then
#     ng=`$echo $1 | $sed "s/,.*//"`
#    _rec1=${lib}/`$sed -n "/^$ng/s/^.*	//p" ${lib}/recording`
#    _tmp=`$echo $ng |$sed "s/\..*//"`
#    _rec2=${lib}/`$cat -s ${lib}/recording|$grep ${_tmp}.all|$sed "s/^.*	//"`
#    if $test -f ${_rec1} ; then
#	$cat -s ${_rec1}
#    fi
#    if $test -f ${_rec2} ; then
#	$cat -s ${_rec2}
#    fi
#fi

# tell them what we think they are doing... !DIST!
case $1 in
net.*|world.*|comp.*|news.*|sci.*|rec.*|misc.*|soc.*|talk.*|mod.*)
    $cat <<'EOM'
This program posts news to thousands of machines throughout the entire
civilized world.  You message will cost the net hundreds if not thousands of
dollars to send everywhere.  Please be sure you know what you are doing.

EOM
    ;;
$cont.*)
    $echo 'This program posts news to many machines throughout the continent.'
    ;;
$cntry.*)
    $echo 'This program posts news to many machines throughout the country.'
    ;;
$state.*)
    $echo 'This program posts news to many machines throughout the state.'
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
    f*) suppressmess=y ;;
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
	state=ask
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
	state=ask
	;;
	
    ask)
	$echo ""
	$echo $n "Send, abort, edit, or list? $c"
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
	h*)
	    $cat <<'EOH'

Type s to send the article, a to abort and append the article to dead.article,
e to edit the article again, or l to list the article.

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
				echo $n "Append .signature file? [y] $c"
				read ans
				case $ans in
				''|y*)
				    echo "-- " >> $tmpart
				    cat $dotdir/.signature >> $tmpart
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
		if $inews -h < $tmpart ; then
		    : null
		else
		    state=rescue
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
	$cat $tmpart >> ${HOME-$LOGDIR}/dead.article
	$echo "Article appended to ${HOME-$LOGDIR}/dead.article"
	$echo "A copy may be temporarily found in $tmpart"
	exit
	;;
    cleanup)
	case "${AUTHORCOPY-none}" in
	none)
	    ;;
	*)  if $echo From $USER `date` >> $AUTHORCOPY; then
		$cat $tmpart >> $AUTHORCOPY
		$echo "Article appended to $AUTHORCOPY"
		$echo "" >> $AUTHORCOPY
		$echo "" >> $AUTHORCOPY
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
net.*|mod.*|comp.*|news.*|sci.*|rec.*|misc.*|soc.*|talk.*)
    defdist=world
    dist=h
    ;;
*.*)
    defdist=`expr "X$ng" : 'X\([a-z0-9]*\)'`
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
    Organization:		$org
    City:			$city
    $stpr:  		$state
    Country:		$cntry
    Continent:		$cont
    Everywhere:		world

EOM
    fi
    $echo $n "Distribution ($defdist): $c"
    read dist
    case $dist in
    '') dist=$defdist ;;
    esac
    case $dist in
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
    ''|$loc*|$org*|$city*|$state*|$cntry*|$cont*|$defdist)
	;;
    net*|world*|comp*|news*|sci*|rec*|misc*|soc*|talk*)
	dist=''
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

case $ng in
*net.general*)
    follow=`echo "$ng" | sed 's/net\.general/net.followup/g'`
    ;;
*)
    follow=""
    ;;
esac

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
    ;;
esac

# now build a file with a header for them to edit

set X ${USER-${LOGNAME-`who am i`}}
shift
logname=$1
case $logname in
*!*) logname=`expr "$logname" : '!\(.*\)$'` ;;
esac
case ${NAME-$nametype} in
bsd)
    fullname=`$sed </etc/passwd -e "/^$logname:/{s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^,:;]*\).*"'$'"/\1/" -e "q" -e "}" -e "d"`
    case $fullname in
    *'&'*) : GACK
	lname=`$echo $logname | $tr 'a-z' 'A-Z'`
	lname=`$echo $lname $logname | $sed 's/^\(.\)[^ ]* ./\1/'`
	fullname=`$echo "$fullname" | $sed "s/&/${lname}/"`
	;;
    esac
    ;;
usg)
    fullname=`$sed </etc/passwd -e "/^$logname:/{s/^[^:]*:[^:]*:[^:]*:[^:]*:\([^(:]*\).*"'$'"/\1/" -e "s/^.*-//" -e "q" -e "}" -e "d"`
    ;;
*)
    fullname=${NAME-`$cat $dotdir/.fullname`}
    ;;
esac

orgname=${ORGANIZATION-$orgname}
case $orgname in
/*) orgname=`$cat $orgname` ;;
esac

$cat > $tmpart <<EOHeader
Newsgroups: $ng
Subject: $title
Expires: 
References: 
Sender: 
Reply-To: $logname@$sitename.UUCP ($fullname)
Followup-To: $follow
Distribution: $dist
Organization: $orgname
Keywords: 

EOHeader

!NO!SUBS!
$eunicefix Pnews.header
