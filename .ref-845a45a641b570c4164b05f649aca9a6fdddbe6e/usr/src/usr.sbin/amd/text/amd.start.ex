#!/bin/sh -
#
# Copyright (c) 1989 Jan-Simon Pendry
# Copyright (c) 1989 Imperial College of Science, Technology & Medicine
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# %sccs.include.redist.sh%
#
#	@(#)amd.start.ex	5.4 (Berkeley) %G%
#
# Start amd
#
# $Id: amd.start.ex,v 5.2.2.1 1992/02/09 15:11:32 jsp beta $
#
PATH=/usr/sbin:/bin:/usr/bin:$PATH export PATH

#
# Either name of logfile or "syslog"
#
#LOGFILE=syslog
LOGFILE=/var/run/amd.log

#
# Figure out whether domain name is in host name
# If the hostname is just the machine name then
# pass in the name of the local domain so that the
# hostnames in the map are domain stripped correctly.
#
case `hostname` in
*.*) dmn= ;;
*) dmn='-d doc.ic.ac.uk'
esac

#
# Zap earlier log file
#
case "$LOGFILE" in
*/*)
	mv "$LOGFILE" "$LOGFILE"-
	> "$LOGFILE"
	;;
syslog)
	: nothing
	;;
esac

cd /usr/sbin
#
# -r 		restart
# -d dmn	local domain
# -w wait	wait between unmount attempts
# -l log	logfile or "syslog"
#
eval nice --4 ./amd -p > /var/run/amd.pid -r $dmn -w 240 -l "$LOGFILE" \
	/homes amd.homes -cache:=inc \
	/home amd.home -cache:=inc \
	/vol amd.vol -cache:=inc
