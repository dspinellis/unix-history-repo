case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting ng.help (with variable substitutions)"
$spitshell >ng.help <<!GROK!THIS!
$startsh
# $Header: ng.help.SH,v 4.3 85/05/01 11:44:34 lwall Exp $
# 
# $Log:	ng.help.SH,v $
# Revision 4.3  85/05/01  11:44:34  lwall
# Baseline for release with 4.3bsd.
# 

$pager <<'EOT'
Newsgroup Selection commands:

y,SP	Do this newsgroup now.
.cmd	Do this newsgroup, executing cmd as first command.
=	Equivalent to .=<carriage return>.
u	Unsubscribe from this newsgroup.
c	Catch up (mark this newsgroup all read).

n	Go to the next newsgroup with unread news.
N	Go to the next newsgroup.
p	Go to the previous newsgroup with unread news.
P	Go to the previous newsgroup.
-	Go to the previously displayed newsgroup.
1	Go to the first newsgroup.
^	Go to the first newsgroup with unread news.
$	Go to the last newsgroup.
g name	Go to the named newsgroup.  Subscribe to new newsgroups this way too.
/pat	Search forward for newsgroup matching pattern.
?pat	Search backward for newsgroup matching pattern.
	(Use * and ? style patterns.  Append r to include read newsgroups.)
l pat	List unsubscribed newsgroups containing pattern.
m name	Move named newsgroup elsewhere (no name moves current newsgroup).
o pat	Only display newsgroups matching pattern.  Omit pat to unrestrict.
a pat	Like o, but also scans for unsubscribed newsgroups matching pattern.
L	List current .newsrc.
&	Print current command-line switch settings.
&switch {switch}
	Set (or unset) more command-line switches.
&&	Print current macro definitions.
&&def	Define a new macro.
!cmd	Shell escape.
q	Quit rn.
^K	Edit the global KILL file.  Use commands like /pattern/j to suppress
	pattern in every newsgroup.
v	Print version.
EOT
!GROK!THIS!
$eunicefix ng.help
chmod 755 ng.help
