/* $Header: help.c,v 4.3.1.2 85/09/10 11:05:39 lwall Exp $
 *
 * $Log:	help.c,v $
 * Revision 4.3.1.2  85/09/10  11:05:39  lwall
 * Improved %m in in_char().
 * 
 * Revision 4.3.1.1  85/05/10  11:33:10  lwall
 * Branch for patches.
 * 
 * Revision 4.3  85/05/01  11:38:59  lwall
 * Baseline for release with 4.3bsd.
 * 
 */

#include "EXTERN.h"
#include "common.h"
#include "rn.h"
#include "term.h"
#include "INTERN.h"
#include "help.h"

void
help_init()
{
    ;
}

int
help_page()
{
    int cmd;

#ifdef PAGERHELP
    doshell(sh,filexp(PAGERHELP));
#else
    page_init();
    if ((cmd = print_lines("\
Paging commands:\n\
",STANDOUT)) ||
    (cmd = print_lines("\n\
SP	Display the next page.\n\
x	Display the next page decrypted (rot13).\n\
d	Display half a page more.\n\
CR	Display one more line.\n\
^R,v,^X	Restart the current article (v=verbose header, ^X=rot13).\n\
",NOMARKING)) ||
    (cmd = print_lines("\
^B	Back up one page.\n\
^L,X	Refresh the screen (X=rot13).\n\
g pat	Go to (search forward within article for) pattern.\n\
G	Search again for current pattern within article.\n\
^G	Search for next line beginning with \"Subject:\".\n\
TAB	Search for next line beginning with a different character.\n\
q	Quit the pager, go to end of article.  Leave article read or unread.\n\
j	Junk this article (mark it read).  Goes to end of article.\n\
\n\
",NOMARKING)) ||
    (cmd = print_lines("\
The following commands skip the rest of the current article, then behave\n\
just as if typed to the 'What next?' prompt at the end of the article:\n\
",STANDOUT)) ||
    (cmd = print_lines("\n\
n	Scan forward for next unread article.\n\
N	Go to next article.\n\
^N	Scan forward for next unread article with same title.\n\
p,P,^P	Same as n,N,^N, only going backwards.\n\
-	Go to previously displayed article.\n\
\n\
",NOMARKING)) ||
    (cmd = print_lines("\
The following commands also take you to the end of the article.\n\
Type h at end of article for a description of these commands:\n\
",STANDOUT)) ||
    (cmd = print_lines("\
	# $ & / = ? c C f F k K ^K m M number r R ^R s S u v w W Y ^ |\n\
\n\
(To return to the middle of the article after one of these commands, type ^L.)\n\
",NOMARKING)) )
	return cmd;
#endif
    return 0;
}

int
help_art()
{
    int cmd;
#ifdef ARTHELP
    doshell(sh,filexp(ARTHELP));
#else
    page_init();
    if ((cmd = print_lines("\
Article Selection commands:\n\
",STANDOUT)) ||
    (cmd = print_lines("\n\
n,SP	Scan forward for next unread article.\n\
N	Go to next article.\n\
^N	Scan forward for next unread article with same subject.\n\
p,P,^P	Same as n,N,^N, only going backwards.\n\
-	Go to previously displayed article.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
number	Go to specified article.\n\
range{,range} command{:command}\n\
	Apply one or more commands to one or more ranges of articles.\n\
	Ranges are of the form: number | number-number.  You may use . for\n\
	the current article, and $ for the last article.\n\
 	Valid commands are: j, m, M, s, S, and !.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
/pattern/modifiers\n\
	Scan forward for article containing pattern in the subject line.\n\
	(Use ?pat? to scan backwards; append h to scan headers, a to scan\n\
	entire articles, r to scan read articles, c to make case sensitive.\n\
/pattern/modifiers:command{:command}\n\
	Apply one or more commands to the set of articles matching pattern.\n\
	Use a K modifier to save entire command to the KILL file for this\n\
	newsgroup.  Commands m and M, if first, imply an r modifier.\n\
 	Valid commands are: j, m, M, s, S, and !.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
f,F	Submit a followup article (F = include this article).\n\
r,R	Reply through net mail (R = include this article).\n\
s ...	Save to file or pipe via sh.\n\
S ...	Save via preferred shell.\n\
w,W	Like s and S but save without the header.\n\
| ...	Same as s|...\n\
C	Cancel this article, if yours.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
^R,v	Restart article (v=verbose).\n\
^X	Restart article, rot13 mode.\n\
c	Catch up (mark all articles as read).\n\
^B	Back up one page.\n\
^L	Refresh the screen.  You can get back to the pager with this.\n\
X	Refresh screen in rot13 mode.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
^	Go to first unread article.  Disables subject search mode.\n\
$	Go to end of newsgroup.  Disables subject search mode.\n\
",NOMARKING)) ||
    (cmd = print_lines("#       Print last article number.\n\
&	Print current values of command-line switches.\n\
&switch {switch}\n\
	Set or unset more switches.\n\
&&	Print current macro definitions.\n\
&&def	Define a new macro.\n\
j	Junk this article (mark it read).  Stays at end of article.\n\
m	Mark article as still unread.\n\
M	Mark article as still unread upon exiting newsgroup or Y command.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
Y	Yank back articles marked temporarily read via M.\n\
k	Mark current SUBJECT as read.\n\
K	Mark current SUBJECT as read, and save command in KILL file.\n\
=	List subjects of unread articles.\n\
u	Unsubscribe to this newsgroup.\n\
^K	Edit local KILL file (the one for this newsgroup).\n\
q	Quit this newsgroup for now.\n\
Q	Quit newsgroup, staying at current newsgroup.\n\
",NOMARKING)) )
	return cmd;
#endif
    return 0;
}

int
help_ng()
{
    int cmd;
#ifdef NGHELP
    doshell(sh,filexp(NGHELP));
#else
    page_init();
    if (cmd = print_lines("\
Newsgroup Selection commands:\n\
",STANDOUT) )
	return cmd;
    if (ng != nextrcline) {
	if (cmd = print_lines("\
\n\
y,SP	Do this newsgroup now.\n\
.cmd	Do this newsgroup, executing cmd as first command.\n\
=	Equivalent to .=<carriage return>.\n\
u	Unsubscribe from this newsgroup.\n\
c	Catch up (mark this newsgroup all read).\n\
",NOMARKING) )
	    return cmd;
    }
    if ((cmd = print_lines("\
\n\
n	Go to the next newsgroup with unread news.\n\
N	Go to the next newsgroup.\n\
p	Go to the previous newsgroup with unread news.\n\
P	Go to the previous newsgroup.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
-	Go to the previously displayed newsgroup.\n\
1	Go to the first newsgroup.\n\
^	Go to the first newsgroup with unread news.\n\
$	Go to the last newsgroup.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
g name	Go to the named newsgroup.  Subscribe to new newsgroups this way too.\n\
/pat	Search forward for newsgroup matching pattern.\n\
?pat	Search backward for newsgroup matching pattern.\n\
	(Use * and ? style patterns.  Append r to include read newsgroups.)\n\
",NOMARKING)) ||
    (cmd = print_lines("\
l pat	List unsubscribed newsgroups containing pattern.\n\
m name	Move named newsgroup elsewhere (no name moves current newsgroup).\n\
o pat	Only display newsgroups matching pattern.  Omit pat to unrestrict.\n\
a pat	Like o, but also scans for unsubscribed newsgroups matching pattern.\n\
L	List current .newsrc.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
&	Print current command-line switch settings.\n\
&switch {switch}\n\
	Set (or unset) more command-line switches.\n\
&&	Print current macro definitions.\n\
&&def	Define a new macro.\n\
!cmd	Shell escape.\n\
",NOMARKING)) ||
    (cmd = print_lines("\
q	Quit rn.\n\
^K	Edit the global KILL file.  Use commands like /pattern/j to suppress\n\
	pattern in every newsgroup.\n\
v	Print version.\n\
",NOMARKING)) )
	return cmd;
#endif
#ifdef PUSHBACK
    if (cmd = get_anything())
	return cmd;
    show_macros();
#endif
    return 0;
}

#ifdef ESCSUBS
int
help_subs()
{
    int cmd;
#ifdef SUBSHELP
    doshell(sh,filexp(SUBSHELP));
#else
    page_init();
    if ((cmd = print_lines("\
Valid substitutions are:\n\
",STANDOUT)) ||
    (cmd = print_lines("\
\n\
a	Current article number\n\
A	Full name of current article (%P/%c/%a)\n\
b	Destination of last save command, often a mailbox\n\
B	Bytes to ignore at beginning of last saved article\n\
",NOMARKING)) ||
    (cmd = print_lines("\
c	Current newsgroup, directory form\n\
C	Current newsgroup, dot form\n\
d	Full name of newsgroup directory (%P/%c)\n\
D	Distribution line from current article\
",NOMARKING)) ||
    (cmd = print_lines("\
f	Who the current article is from\n\
F	Newsgroups to followup to (from Newsgroups and Followup-To)\n\
h	(This help message)\n\
H	Host name (yours)\n\
i	Message-I.D. line from current article, with <>\n\
I	Reference indicator mark (see -F switch)\n\
",NOMARKING)) ||
    (cmd = print_lines("\
l	News administrator's login name, if any\n\
L	Login name (yours)\n\
m	Current mode, first letter of (init, newsgroup, article, pager,\n\
		Add, Catchup, Delete bogus, Mailbox, Resubscribe)\n\
M	Number of article marked with M\n\
n	Newsgroups from current article\n\
N	Full name (yours)\n\
",NOMARKING)) ||
    (cmd = print_lines("\
o	Organization (yours)\n\
O	Original working directory (where you ran rn from)\n\
p	Your private news directory (from -d)\n\
P	Public news spool directory\n\
",NOMARKING)) ||
    (cmd = print_lines("\
r	Last reference (parent article id)\n\
R	References list for followup article\n\
s	Subject, with all Re's and (nf)'s stripped off\n\
S	Subject, with one Re stripped off\
",NOMARKING)) ||
    (cmd = print_lines("\
t	New To line derived from From and Reply-To (Internet format)\n\
T	New To line derived from Path\n\
u	Number of unread articles\n\
U	Number of unread articles not counting current article\n\
x	News library directory\n\
X	Rn library directory\n\
z	Length of current article in bytes\n\
",NOMARKING)) ||
    (cmd = print_lines("\
~	Your home directory\n\
.	Directory containing . files\n\
$	Current process number\n\
/	Last search string\n\
ESC	Run preceding command through % interpretation\n\
",NOMARKING)) )
	return cmd;
#endif
    return 0;
}
#endif

