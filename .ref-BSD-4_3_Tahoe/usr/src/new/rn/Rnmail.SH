case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting Rnmail (with variable substitutions)"
$spitshell >Rnmail <<!GROK!THIS!
$startsh
# $Header: Rnmail.SH,v 4.3.1.4 85/08/01 14:23:05 lwall Exp $
# 
# $Log:	Rnmail.SH,v $
# Revision 4.3.1.4  85/08/01  14:23:05  lwall
# Added MAILRECORD.  Temp file is no longer in /tmp.  'e editor' added.
# 
# Revision 4.3.1.3  85/05/20  16:25:17  lwall
# Shouldn't ask editor if EDITOR or VISUAL defined.
# 
# Revision 4.3.1.2  85/05/17  10:36:19  lwall
# Added "-- " before .signature.
# 
# Revision 4.3.1.1  85/05/10  11:30:38  lwall
# Branch for patches.
# 
# Revision 4.3  85/05/01  11:34:18  lwall
# Baseline for release with 4.3bsd.
# 
#
# syntax: Rnmail -h headerfile [oldart]		or
#         Rnmail destination-list 		or just
#         Rnmail

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

# System dependencies

mailer="${mailer-/bin/mail}"
# if you change this to something that does signatures, take out signature code

# your site name
case $portable in
define) sitename=\`$hostcmd\` ;;
undef) sitename="$sitename" ;;
esac
# your organization name
orgname="$orgname"
# what pager you use--if you have kernal paging use cat
pager="\${PAGER-$pager}"
# how you derive full names, bsd, usg, or other
nametype="$nametype"
# default editor
defeditor="$defeditor"
# how not to do a newline with echo
n="$n"
c="$c"

test=${test-test}
sed=${sed-sed}
echo=${echo-echo}
cat=${cat-cat}
grep=${grep-grep}
rm=${rm-rm}

!GROK!THIS!
$spitshell >>Rnmail <<'!NO!SUBS!'
dotdir=${DOTDIR-${HOME-$LOGDIR}}
tmpart=$dotdir/.letter

headerfile=""
case $# in
0) ;;
*)  case $1 in
    -h)
	headerfile="$2"
	case $# in
	3) oldart=$3 ;;
	esac
	;;
    esac
    ;;
esac

case $headerfile in
'')
    case $# in
    0)
	to=h
	while $test "$to" = h ; do
	    $echo ""
	    $echo $n "To: $c"
	    read to
	    case $to in
	    h)
		$cat <<'EOH'

Type the net address of those people that you wish the message sent to.
Additional recipients may be added on the Cc: line when you edit.

Separate multiple addresses with spaces.

EOH
		;;
	    esac
	done
	;;
    *)
	to="$*"
	;;
    esac
    to=`$echo "$to" | $sed 's/  */ /g'`

    title=h
    while $test "$title" = h ; do
	$echo ""
	$echo $n "Title/Subject: $c"
	read title
	case $title in
	h)
	    $cat <<'EOH'

Type the title for your message.  
EOH
	    ;;
	esac
    done

# now build a file with a header for them to edit
    
    orgname=${ORGANIZATION-$orgname}
    case $orgname in
    /*) orgname=`$cat $orgname` ;;
    esac

    $cat > $tmpart <<EOHeader
To: $to
Subject: $title
Organization: $orgname
Cc:
Bcc:

EOHeader

    ;;
*)
    $cat < $headerfile  > $tmpart
    ;;
esac


file=h
while $test "$file" = h ; do
    $echo ""
    $echo $n "Prepared file to include [none]: $c"
    read file
    case $file in
    h)
	$cat <<'EOH'

If you have already produced the body of your message, type the filename
for it here.  If you just want to proceed directly to the editor, type a
RETURN.  In any event, you will be allowed to edit as many times as you
want before you send off the message.
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
	rescue="sleep 1; $cat $tmpart >>${HOME-$LOGDIR}/dead.letter ; $echo Message appended to ${HOME-$LOGDIR}/dead.letter ; exit"
	trap "$rescue" 1
	trap : 2
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
	${VISUAL-${EDITOR-$defeditor}} $tmpart $oldart
	trap "$rescue" 2
	state=ask
	;;
	
    ask)
	$echo ""
	$echo $n "Send, abort, edit, or list? $c"
	read ans
	
	case $ans in
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

Type s to send the message, a to abort and append the message to dead.letter,
e to edit the message again, or l to list the message.

To invoke an alternate editor, type 'e editor'.
EOH
	esac
	;;
    
    send)
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
	case $mailer in
	*sendmail)
	    $mailer -t <$tmpart
	    ;;
# but recmail does not know about Bcc, alas
	*recmail)
	    $mailer <$tmpart
	    ;;
	*)
	    set X `$sed <$tmpart -n -e '/^To:/{' -e 's/To: *//p' -e q -e '}'`
	    shift
	    set X "$@" `$sed <$tmpart -n -e '/^Cc:/{' -e 's/Cc: *//p' -e q -e '}'`
	    shift
	    set X "$@" `$sed <$tmpart -n -e '/^Bcc:/{' -e 's/Bcc: *//p' -e q -e '}'`
	    shift
	    $grep -v "^Bcc:"  <$tmpart | $mailer "$@"
	    ;;
	esac
	case $? in
	0)
	    state=cleanup
	    ;;
	*)
	    state=rescue
	    ;;
	esac
	;;
    rescue)
	$cat $tmpart >> ${HOME-$LOGDIR}/dead.letter
	$echo "Message appended to ${HOME-$LOGDIR}/dead.letter"
	$echo "A copy may be temporarily found in $tmpart"
	exit
	;;
    cleanup)
	case "${MAILRECORD-none}" in
	none)
	    ;;
	*)  if $cat $tmpart >> $MAILRECORD; then
		$echo "Article appended to $MAILRECORD"
	    else
		$echo "Cannot append to $MAILRECORD"
	    fi
	    ;;
	esac
	exit
	;;
    esac
done
!NO!SUBS!
$eunicefix Rnmail
chmod 755 Rnmail
