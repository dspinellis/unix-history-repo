#! /bin/csh -f
#
#	sendbug.sh	4.1	83/05/11
# Create a bug report and mail to '4bsd-bugs'.
#

onintr clean
/bin/cp bugformat /tmp/bug$$
/usr/ucb/vi /tmp/bug$$
if ($#argv == 0) then
	/usr/lib/sendmail -t 4bsd-bugs\@BERKELEY < /tmp/bug$$
else
	/usr/lib/sendmail -t $argv[1] < /tmp/bug$$
endif
clean:
/bin/rm -f /tmp/bug$$
