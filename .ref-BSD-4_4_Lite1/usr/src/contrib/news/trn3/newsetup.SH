case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting newsetup (with variable substitutions)"
$spitshell >newsetup <<!GROK!THIS!
$startsh
# $Id: newsetup.SH,v 3.0 1992/02/01 03:09:32 davison Trn $
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

: syntax: newsetup

: System dependencies
: You will want to change the definitions below to reflect the distribution
: areas around you.  If you have more areas than this you will need to modify
: the sed below.

locorg="$locdist"
organization="$orgdist"
city="$citydist"
state="$statedist"
cntry="$cntrydist"
cont="$contdist"
#NORMALactive="${active-/usr/lib/news/active}"
#NNTPactive="/tmp/active.\$\$"

dotdir="\${DOTDIR-\${HOME-\$LOGDIR}}"
newsrc="\${NEWSRC-\$dotdir/.newsrc}"
$rm -f \$newsrc-old
$echo "Creating \$newsrc to be used by news programs."
#NNTPrnlib=$privlib
#NNTPcase \$rnlib in
#NNTP~*) rnlib=\`$filexp \$rnlib\` ;;
#NNTPesac
#NNTPif \$rnlib/getactive ACTIVE \$active; then
#NNTP    true;
#NNTPelse
#NNTP    exit 1;
#NNTPfi
#NORMALcase \$active in
#NORMAL~*) active=\`$filexp \$active\` ;;
#NORMALesac

if $test -s \$newsrc ; then
    $echo "Saving your current \$newsrc as \$newsrc-old..."
    $mv -f \$newsrc \$newsrc-old
fi

: newsrc order determined here

$sed <\$active '
	/^to\./d
	/ [^mny][^ ]*$/d
	s/ .*//
	s/^/ /
	s/^ '\$locorg'\./01&/
	s/^ '\$organization'\./02&/
	s/^ '\$city'\./03&/
	s/^ '\$state'\./04&/
	s/^ '\$cntry'\./05&/
	s/^ '\$cont'\./06&/
	s/^ news\./07&/
	s/^ comp\./08&/
	s/^ sci\./09&/
	s/^ rec\./10&/
	s/^ soc\./11&/
	s/^ talk\./13&/
	s/^ control\$/14&/
	s/^ junk\$/14&/
	s/^ test\$/14&/
	/\.test\$/s/^[0-9]*/14/
	s/^ .*\./12&/
	s/^ /00&/
' |
$sort -u |
$sed '
	/^14 /!s/\$/:/
	/^14 /s/\$/!/
	s/^[0-9][0-9] //
' >\$newsrc

#NNTP$rm -f \$active

$cat <<'EOH'
Done.

If you have never used the news system before, you may find the articles
in news.announce.newusers to be helpful.  There is also a manual entry for rn.

To get rid of newsgroups you aren't interested in, use the 'u' command.
Type h for help at any time while running rn.
EOH
!GROK!THIS!
case "$d_nntp" in
define)  sed < newsetup -e '/^#NNTP/s/^#NNTP//' -e '/^#NORMAL/d' > newsetup.new ;;
*) sed < newsetup -e '/^#NNTP/d' -e '/^#NORMAL/s/^#NORMAL//' > newsetup.new ;;
esac
mv newsetup.new newsetup
$eunicefix newsetup
chmod 755 newsetup
