case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting pager.help (with variable substitutions)"
$spitshell >pager.help <<!GROK!THIS!
$startsh
# $Header: pager.help.SH,v 4.3 85/05/01 11:45:31 lwall Exp $
#
# $Log:	pager.help.SH,v $
# Revision 4.3  85/05/01  11:45:31  lwall
# Baseline for release with 4.3bsd.
# 

$pager <<'EOT'
Paging commands:

SP	Display the next page.
x	Display the next page decrypted (rot13).
d	Display half a page more.
CR	Display one more line.
^R,v,^X	Restart the current article (v=verbose header, ^X=rot13).
^B	Back up one page.
^L,X	Refresh the screen (X=rot13).
g pat	Go to (search forward within article for) pattern.
G	Search again for current pattern within article.
^G	Search for next line beginning with \"Subject:\".
TAB	Search for next line beginning with a different character.
q	Quit the pager, go to end of article.  Leave article read or unread.
j	Junk this article (mark it read).  Goes to end of article.

The following commands skip the rest of the current article, then behave
just as if typed to the 'What next?' prompt at the end of the article:

n	Scan forward for next unread article.
N	Go to next article.
^N	Scan forward for next unread article with same title.
p,P,^P	Same as n,N,^N, only going backwards.
-	Go to previously displayed article.

The following commands also take you to the end of the article.
Type h at end of article for a description of these commands:

	# $ & / = ? c C f F k K ^K m M number r R ^R s S u v w W Y ^ |

(To return to the middle of the article after one of these commands, type ^L.)
EOT
!GROK!THIS!
$eunicefix pager.help
chmod 755 pager.help
