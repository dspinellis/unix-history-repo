#!/bin/sh
PATH=:/bin:/usr/bin:/usr/ucb:/usr/local/bin
LOGFILE=/usr/spool/log/named
MAILMSG=/usr/tmp/mailmsg$$
LAMERS=/usr/tmp/lamers$$
MSGFILE=/usr/local/bin/lamer-message
LAMEREPORT=/tmp/.lamereport$$
REPORT=/tmp/.report.$$
WEEKFILE=/usr/tmp/week$$
HOSTMASTER="lame-delegations@terminator.rs.itd.umich.edu"

# -------------------------------------------------------------
#  Copyright (c) 1991 Regents of the University of Michigan.
#  All rights reserved.
#
#  Redistribution and use is permitted provided that this notice 
#  is preserved and that due credit is given to the University of 
#  Michigan. The name of the University may not be used to endorse 
#  or promote products derived from this software without specific 
#  prior written permission. This software is provided "as is" 
#  without express or implied warranty.
#
#  Lame delegation notifier
#  Author:  Bryan Beecher
#  Last Modified:  10/27/92
#
#  To make use of this software, you need to be running the
#  University of Michigan release of BIND 4.8.3, or any version
#  of named that supports the LAME_DELEGATION patches posted to
#  USENET.  The U-M release is available via anonymous ftp from
#  terminator.rs.itd.umich.edu:/dns/bind4.8.3.tar.Z.
#
#  You must also have a copy of query(1) and host(1).  These
#  are also available via anonymous ftp in the aforementioned
#  place.
# -------------------------------------------------------------

# -------------------------------------------------------------
#  handle arguments
# -------------------------------------------------------------
#	-d <day>
#	This flag is used to append a dot-day suffix to the LOGFILE.
#	Handy where log files are kept around for the last week
#	and contain a day suffix.
#
#	-f <logfile>
#	Change the LOGFILE value altogether.
#
#	-w
#	Count up all of the DNS statistics for the whole week.
#
#	-v
#	Be verbose.
#
#	-t
#	Test mode.  Do not send mail to the lame delegation
#	hostmasters.
# -------------------------------------------------------------
VERBOSE=0
TESTMODE=0
while [ $# != 0 ] ; do
	case "$1" in
		-d)
		LOGFILE=$LOGFILE"."$2
		shift
		;;

		-f)
		LOGFILE=$2
		shift
		;;

		-w)
		cat $LOGFILE* > $WEEKFILE
		LOGFILE=$WEEKFILE
		;;

		-v)
		VERBOSE=1
		;;

		-t)
		TESTMODE=1
		;;
	esac
	shift
done

#--------------------------------------------------------------------------
#  Clean up and exit on a HUP, INT or QUIT
#--------------------------------------------------------------------------
trap "rm -f $LAMERS $MAILMSG $LAMEREPORT $WEEKFILE ; exit" 1 2 3

#--------------------------------------------------------------------------
#  See if there are any lamers
#--------------------------------------------------------------------------
grep "Lame" $LOGFILE | tr A-Z a-z | grep -v "*" | awk '{
	print substr($16, 2, length($16) - 3), $12 }' |
	sort | uniq | awk '{
		printf("%s %s\n", $1, $2)
}' > $LAMERS

if [ ! -s $LAMERS ] ; then
	exit 0
fi

if [ $VERBOSE -eq 1 ] ; then
	echo "Found" `awk 'END { print NR }' $LAMERS` "lame delegations"
fi

#--------------------------------------------------------------------------
#  There were lamers; send them mail
#--------------------------------------------------------------------------
touch $LAMEREPORT
NAME=""
while read DOMAIN IPADDR ; do
	#-----------------------------------------------------------
	# Echo args if verbose
	#-----------------------------------------------------------
	if [ $VERBOSE -eq 1 ] ; then
		echo "$IPADDR may be a lame delegation for $DOMAIN"
	fi
	#-----------------------------------------------------------
	# Lookup the SOA record form $DOMAIN.  A really broken name
	# server many have more than one SOA for a domain, so exit
	# after finding the first one.  Send it to the local hostmaster
	# if we cannot find the proper one.
	#-----------------------------------------------------------
	if [ $VERBOSE -eq 1 ] ; then
		echo "Looking up the hostmaster for $DOMAIN"
	fi
	HOSTMASTER=`query -h $DOMAIN -t SOA 2> /dev/null | awk '/mail addr/ { print $4 ; exit }' | sed -e 's/\./@/'`
	NAME=`host $IPADDR 2> /dev/null`
	if [ -z ""$HOSTMASTER ] ; then
		if [ -z ""$NAME ] ; then
			HOSTMASTER="hostmaster"
		else
			HOSTMASTER="postmaster@"$NAME
		fi
	fi
	#-----------------------------------------------------------
	# Find the name associated with IP address $IPADDR.  Query
	# the nameserver at that address:  If it responds listing
	# itself as a domain namserver, then it is lame; if it isn't
	# in the list, then perhaps the lame delegation alert was
	# spurious.
	#-----------------------------------------------------------
	if [ $VERBOSE -eq 1 ] ; then
		echo "Making sure that $IPADDR is listed as a NS for $DOMAIN"
	fi
	if [ -n ""$NAME ] ; then
		query -n $IPADDR -h $DOMAIN 2>&1 | grep "domain name" | grep -i $NAME > /dev/null
		if [ $? -eq 1 -a $VERBOSE -eq 1 ] ; then
			echo $NAME does not seem to be a nameserver for $DOMAIN
			continue
		fi
	fi
	#-----------------------------------------------------------
	# If the delegation is no longer lame, don't send mail.
	# We do the query twice; the first answer could be authori-
	# tative even if the nameserver is not performing service
	# for the domain.  If this is the case, then the second
	# query will come from cached data, and will be exposed
	# on the second query.  If the resolver returns trash, the
	# entire set of flags will be set.  In this case, don't
	# count the answer as authoritative.
	#-----------------------------------------------------------
	if [ $VERBOSE -eq 1 ] ; then
		echo "Making sure that $IPADDR is not providing authoritative data now"
	fi
	query -n $IPADDR -h $DOMAIN > /dev/null 2>&1 
	query -n $IPADDR -h $DOMAIN 2>&1 | grep header | grep aa | grep -v tc > /dev/null
	if [ $? -eq 0 ] ; then
		if [ $VERBOSE -eq 1 ] ; then
			if [ -n ""$NAME ] ; then
				echo $NAME seems to be serving $DOMAIN OK now
			else
				echo $I seems to be serving $DOMAIN OK now
			fi
		fi
		continue
	fi
	#-----------------------------------------------------------
	# Notify the owner of the lame delegation, and also notify
	# the local hostmaster.
	#-----------------------------------------------------------
	if [ $TESTMODE -eq 0 ] ; then
		if [ $VERBOSE -eq 1 ] ; then
			echo "Sending mail to $HOSTMASTER about lame server $IPADDR for domain $DOMAIN"
		fi
		echo "To: " $HOSTMASTER > $MAILMSG
		echo "Subject: $IPADDR appears to be a lame delegation for $DOMAIN" >> $MAILMSG
		cat $MSGFILE >> $MAILMSG
		if [ -z ""$NAME ] ; then
			NAME=" "
		fi
		sed -e "s|%DOMAIN%|$DOMAIN|" -e "s|%SERVER%|$NAME|" -e "s|%IPADDR%|$IPADDR|" $MAILMSG |
			/usr/lib/sendmail -t -fdns-maintenance
	fi
	echo $IPADDR $DOMAIN >> $LAMEREPORT
done < $LAMERS
#--------------------------------------------------------------------------
# No news is good news
#--------------------------------------------------------------------------
if [ -s $LAMEREPORT ] ; then
	rm -f $REPORT
	echo "The following lame delegations were discovered by the U-M namservers" >> $REPORT
	echo "during the past two weeks of operation." >> $REPORT
	echo " " >> $REPORT
	echo "This nameserver  was found to be a lame delegation for this domain" >> $REPORT
	echo "---------------  -------------------------------------------------" >> $REPORT
	awk '{ printf("%-15s  %s\n", $1, $2) }' $LAMEREPORT >> $REPORT
	Mail -s "U-M lame delegation report" $HOSTMASTER < $REPORT
fi

#--------------------------------------------------------------------------
# Tidy up
#--------------------------------------------------------------------------
rm -f $LAMERS $MAILMSG $LAMEREPORT $WEEKFILE $REPORT
