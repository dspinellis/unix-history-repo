#!/bin/sh -
#
# Start amd
#
# $Id: amd.start.ex,v 5.2 90/06/23 22:21:29 jsp Rel $
#
# Copyright (c) 1989 Jan-Simon Pendry
# Copyright (c) 1989 Imperial College of Science, Technology & Medicine
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# Redistribution and use in source and binary forms are permitted provided
# that: (1) source distributions retain this entire copyright notice and
# comment, and (2) distributions including binaries display the following
# acknowledgement:  ``This product includes software developed by the
# University of California, Berkeley and its contributors'' in the
# documentation or other materials provided with the distribution and in
# all advertising materials mentioning features or use of this software.
# Neither the name of the University nor the names of its contributors may
# be used to endorse or promote products derived from this software without
# specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)amd.start.ex	5.1 (Berkeley) 6/29/90
#
PATH=/usr/local/etc:/bin:/usr/bin:/usr/ucb:$PATH export PATH

#
# Either name of logfile or "syslog"
#
#LOGFILE=syslog
LOGFILE=/var/adm/am.log

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

cd /usr/local/etc
#
# -r 		restart
# -d dmn	local domain
# -w wait	wait between unmount attempts
# -l log	logfile or "syslog"
#
eval nice --4 ./amd -p > /etc/amd.pid -r $dmn -w 240 -l "$LOGFILE" \
	/homes amd.homes -cache=inc \
	/home amd.home -cache=inc \
	/vol amd.vol -cache=inc
