case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting subs.help (with variable substitutions)"
$spitshell >subs.help <<!GROK!THIS!
$startsh
# $Header: subs.help.SH,v 4.3 85/05/01 11:50:50 lwall Exp $
# 
# $Log:	subs.help.SH,v $
# Revision 4.3  85/05/01  11:50:50  lwall
# Baseline for release with 4.3bsd.
# 

$pager <<'EOT'
Valid substitutions are:

a	Current article number
A	Full name of current article (%P/%c/%a)
b	Destination of last save command, often a mailbox
B	Bytes to ignore at beginning of last saved article
c	Current newsgroup, directory form
C	Current newsgroup, dot form
d	Full name of newsgroup directory (%P/%c)
D	Distribution line from current article\
f	Who the current article is from
F	Newsgroups to followup to (from Newsgroups and Followup-To)
h	(This help message)
H	Host name (yours)
i	Message-I.D. line from current article, with <>
I	Reference indicator mark (see -F switch)
l	News administrator's login name, if any
L	Login name (yours)
m	Current mode, first letter of (init,newsgroup,article,pager,misc)
M	Number of article marked with M
n	Newsgroups from current article
N	Full name (yours)
o	Organization (yours)
O	Original working directory (where you ran rn from)
p	Your private news directory (from -d)
P	Public news spool directory
r	Last reference (parent article id)
R	References list for followup article
s	Subject, with all Re's and (nf)'s stripped off
S	Subject, with one Re stripped off\
t	New To line derived from From and Reply-To (Internet format)
T	New To line derived from Path
u	Number of unread articles
U	Number of unread articles not counting current article
x	News library directory
X	Rn library directory
z	Length of current article in bytes
~	Your home directory
.	Directory containing . files
$	Current process number
/	Last search string
ESC	Run preceding command through % interpretation
EOT
!GROK!THIS!
$eunicefix subs.help
chmod 755 subs.help
