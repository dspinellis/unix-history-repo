case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting newsetup (with variable substitutions)"
$spitshell >newsetup <<!GROK!THIS!
$startsh
# $Header: newsetup.SH,v 4.3.1.2 86/09/05 15:41:04 lwall Exp $
# 
# $Log:	newsetup.SH,v $
# Revision 4.3.1.2  86/09/05  15:41:04  lwall
# Changes for newsgroup renaming.
# 
# Revision 4.3.1.1  85/05/10  11:35:43  lwall
# Branch for patches.
# 
# Revision 4.3  85/05/01  11:43:05  lwall
# Baseline for release with 4.3bsd.
# 

export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

: syntax: newsetup

: System dependencies
: You will want to change the definitions below to reflect the distribution
: areas around you.  If you have more areas than this you will need to modify
: the sed below.

locorg="$locpref"
organization="$orgpref"
city="$citypref"
state="$statepref"
cntry="$cntrypref"
cont="$contpref"
active="${active-/usr/lib/news/active}"

dotdir="\${DOTDIR-\${HOME-\$LOGDIR}}"
$rm -f \$dotdir/.oldnewsrc
$echo "Creating .newsrc in \$dotdir to be used by news programs."

case \$active in
~*) active=\`$filexp \$active\` ;;
esac

: NOTE: SED WILL NOT TAKE MORE THAN 10 WFILES, SO BEWARE

$sort <\$active | $sed >/tmp/n.tmp\$\$	\\
	-e 's/^\([^ ]*\) .*\$/\1:/'	\\
	-e '/^control:/{'		\\
	-e "  w /tmp/n.test\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e '/^junk:/{'			\\
	-e "  w /tmp/n.test\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e '/test:/{'			\\
	-e "  w /tmp/n.test\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^net\./{"			\\
	-e "  w /tmp/n.net\$\$"		\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^mod\./{"			\\
	-e "  w /tmp/n.mod\$\$"		\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^\$locorg\./{"		\\
	-e "  w /tmp/n.\$locorg\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^\$organization\./{"	\\
	-e "  w /tmp/n.\$organization\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^\$city\./{"		\\
	-e "  w /tmp/n.\$city\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^\$state\./{" 		\\
	-e "  w /tmp/n.\$state\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^fa\./{"			\\
	-e "  w /tmp/n.fa\$\$"		\\
	-e '  d'			\\
	-e '}'

$sed </tmp/n.tmp\$\$ >/tmp/n.local\$\$	\\
	-e "/^\$cntry\./{" 		\\
	-e "  w /tmp/n.\$cntry\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^\$cont\./{" 		\\
	-e "  w /tmp/n.\$cont\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^to\./{"			\\
	-e "  w /tmp/n.to\$\$"		\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^comp\./{"			\\
	-e "  w /tmp/n.comp\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^news\./{"			\\
	-e "  w /tmp/n.news\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^rec\./{"			\\
	-e "  w /tmp/n.rec\$\$"		\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^sci\./{"			\\
	-e "  w /tmp/n.sci\$\$"		\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^soc\./{"			\\
	-e "  w /tmp/n.soc\$\$"		\\
	-e '  d'			\\
	-e '}'				\\
	-e "/^talk\./{"			\\
	-e "  w /tmp/n.talk\$\$"	\\
	-e '  d'			\\
	-e '}'				\\
	-e "/\./{"			\\
	-e "  w /tmp/n.misc\$\$"	\\
	-e '  d'			\\
	-e '}'


if $test -s \$dotdir/.newsrc ; then
    $echo "Saving your current .newsrc as .oldnewsrc..."
    $mv -f \$dotdir/.newsrc \$dotdir/.oldnewsrc
fi

: newsrc order determined here
$cat \\
    /tmp/n.local\$\$ \\
    /tmp/n.\$locorg\$\$ \\
    /tmp/n.\$organization\$\$ \\
    /tmp/n.\$city\$\$ \\
    /tmp/n.\$state\$\$ \\
    /tmp/n.\$cntry\$\$ \\
    /tmp/n.\$cont\$\$ \\
    /tmp/n.mod\$\$ \\
    /tmp/n.news\$\$ \\
    /tmp/n.comp\$\$ \\
    /tmp/n.sci\$\$ \\
    /tmp/n.rec\$\$ \\
    /tmp/n.soc\$\$ \\
    /tmp/n.misc\$\$ \\
    /tmp/n.talk\$\$ \\
    /tmp/n.net\$\$ \\
    /tmp/n.fa\$\$ \\
    /tmp/n.test\$\$ \\
| $uniq >\$dotdir/.newsrc

$rm -f	/tmp/n.to\$\$ \\
	/tmp/n.tmp\$\$ \\
	/tmp/n.local\$\$ \\
	/tmp/n.\$locorg\$\$ \\
	/tmp/n.\$organization\$\$ \\
	/tmp/n.\$city\$\$ \\
	/tmp/n.\$state\$\$ \\
	/tmp/n.\$cntry\$\$ \\
	/tmp/n.\$cont\$\$ \\
	/tmp/n.mod\$\$ \\
	/tmp/n.news\$\$ \\
	/tmp/n.comp\$\$ \\
	/tmp/n.sci\$\$ \\
	/tmp/n.soc\$\$ \\
	/tmp/n.rec\$\$ \\
	/tmp/n.talk\$\$ \\
	/tmp/n.net\$\$ \\
	/tmp/n.fa\$\$ \\
	/tmp/n.misc\$\$ \\
	/tmp/n.test\$\$

$cat <<'EOH'
Done.

If you have never used the news system before, you may find the articles
in mod.announce.newuser to be helpful.  There is also a manual entry for rn.

To get rid of newsgroups you aren't interested in, use the 'u' command.
Type h for help at any time while running rn.
EOH
!GROK!THIS!
$eunicefix newsetup
chmod 755 newsetup
