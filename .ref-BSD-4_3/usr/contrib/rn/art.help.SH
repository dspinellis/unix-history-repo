case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting art.help (with variable substitutions)"
$spitshell >art.help <<!GROK!THIS!
$startsh
# $Header: art.help.SH,v 4.3 85/05/01 11:35:34 lwall Exp $
# 
# $Log:	art.help.SH,v $
# Revision 4.3  85/05/01  11:35:34  lwall
# Baseline for release with 4.3bsd.
# 
# 
$pager <<'EOT'
Article Selection commands:

n,SP	Scan forward for next unread article.
N	Go to next article.
^N	Scan forward for next unread article with same subject.
p,P,^P	Same as n,N,^N, only going backwards.
-	Go to previously displayed article.
number	Go to specified article.
range{,range} command{:command}
	Apply one or more commands to one or more ranges of articles.
	Ranges are of the form: number | number-number.  You may use . for
	the current article, and $ for the last article.
 	Valid commands are: j, m, M, s, S, and !.
/pattern/modifiers
	Scan forward for article containing pattern in the subject line.
	(Use ?pat? to scan backwards; append h to scan headers, a to scan
	entire articles, r to scan read articles, c to make case sensitive.
/pattern/modifiers:command{:command}
	Apply one or more commands to the set of articles matching pattern.
	Use a K modifier to save entire command to the KILL file for this
	newsgroup.  Commands m and M, if first, imply an r modifier.
 	Valid commands are: j, m, M, s, S, and !.
f,F	Submit a followup article (F = include this article).
r,R	Reply through net mail (R = include this article).
s ...	Save to file or pipe via sh.
S ...	Save via preferred shell.
w,W	Like s and S but save without the header.
| ...	Same as s|...
C	Cancel this article, if yours.
^R,v	Restart article (v=verbose).
^X	Restart article, rot13 mode.
c	Catch up (mark all articles as read).
^B	Back up one page.
^L	Refresh the screen.  You can get back to the pager with this.
X	Refresh screen in rot13 mode.
^	Go to first unread article.  Disables subject search mode.
$	Go to end of newsgroup.  Disables subject search mode.
#       Print last article number.
&	Print current values of command-line switches.
&switch {switch}
	Set or unset more switches.
&&	Print current macro definitions.
&&def	Define a new macro.
j	Junk this article (mark it read).  Stays at end of article.
m	Mark article as still unread.
M	Mark article as still unread upon exiting newsgroup or Y command.
Y	Yank back articles marked temporarily read via M.
k	Mark current SUBJECT as read.
K	Mark current SUBJECT as read, and save command in KILL file.
=	List subjects of unread articles.
u	Unsubscribe to this newsgroup.
^K	Edit local KILL file (the one for this newsgroup).
q	Quit this newsgroup for now.
Q	Quit newsgroup, staying at current newsgroup.
EOT

!GROK!THIS!
$eunicefix art.help
chmod 755 art.help
