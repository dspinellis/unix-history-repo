#!/bin/sh
#
# Shell script for making manual pages
#
# Created by Stan Barber; Mon Apr  6 13:25:27 CDT 1987
# Modified by Phil Lapsley; Thu Oct 15 17:43:40 PDT 1987
#

#
# ------------- SITE DEPENDENT STUFF ---------------------------
# (modify these lines to suit your system)
#
# the location of inetd on your system (usually /etc/inetd)
MINETD=/etc/inetd
# the location of the inetd configuration file (usually /etc/inetd.conf)
MINETDCONFIG=/etc/inetd.conf
# the location of nntpd following installation (usually /etc/nntpd)
MNNTPD=/etc/nntpd
# the location of the hosts file (usually /etc/hosts)
MHOSTFILE=/etc/hosts
# the location of the services file (usually /etc/services)
MSERVICES=/etc/services
# the location of the networks file (usually /etc/networks)
MNETWORKFILE=/etc/networks
# The location of the news spool directory (usually /usr/spool/news)
MNEWSSPOOL=/usr/spool/news
# The location of the news library directory (usually /usr/lib/news)
MNEWSLIB=/usr/lib/news
# The location of the file containing the name of the nntp server machine
# (usually /usr/local/lib/rn/server)
MSERVERFILE=/usr/local/lib/rn/server
# The location of rn's local library (usually /usr/local/lib/rn)
MRNLIB=/usr/local/lib/rn
#
# ------------ END OF SITE DEPENDENT STUFF ---------------------
# (you should not have to touch anything below).

echo "Extracting $2 from $1 ..."

( 
echo g%NEWSLIB%s%NEWSLIB%$MNEWSLIB%
echo g%NEWSSPOOL%s%NEWSSPOOL%$MNEWSSPOOL%
echo g%INETDCONFIG%s%INETDCONFIG%$MINETDCONFIG%
echo g%LNNTPD%s%LNNTPD%$MNNTPD%
echo g%INETD%s%INETD%$MINETD%
echo g%HOSTFILE%s%HOSTFILE%$MHOSTFILE%
echo g%SERVICES%s%SERVICES%$MSERVICES%
echo g%NETWORKFILE%s%NETWORKFILE%$MNETWORKFILE%
echo g%RNLIB%s%RNLIB%$MRNLIB%
echo w
echo q
) > /tmp/ed.$$

rm -f $2
cp $1 $2
chmod 644 $2
(ed $2 < /tmp/ed.$$) 2>&1 > /dev/null
rm /tmp/ed.$$

echo "Finished."
