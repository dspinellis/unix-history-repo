case $CONFIG in
    '') . config.sh ;;
esac
echo "Extracting norm.saver (with variable substitutions)"
$spitshell >norm.saver <<!GROK!THIS!
$startsh
# $Header: norm.saver.SH,v 4.3.1.2 85/05/20 15:56:24 lwall Exp $
# 
# $Log:	norm.saver.SH,v $
# Revision 4.3.1.2  85/05/20  15:56:24  lwall
# Turned $5 into \$5.
# 
# Revision 4.3.1.1  85/05/10  11:36:52  lwall
# Branch for patches.
# 
# Revision 4.3  85/05/01  11:45:16  lwall
# Baseline for release with 4.3bsd.
# 
# 
#	Arguments:
#	1 Full name of article (%A)
#	2 Public news spool directory (%P)
#	3 Directory of current newsgroup (%c)
#	4 Article number (%a)
#	5 Where in article to start (%B)
#	6 Newsgroup name (%C)
#	7 Save destination (%b)
#
export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

( case "\$5" in
  0) $echo "Article \$4 of \$6:" ;;
  esac
  $tail +\$5c \$1
  $echo ""
  $echo "" ) >> \$7
!GROK!THIS!
$eunicefix norm.saver
chmod 755 norm.saver
