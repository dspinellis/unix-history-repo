#!/bin/sh -
#
#	from named.restart	5.4 (Berkeley) 6/27/89
#	$Id: named.restart.sh,v 4.9.1.4 1993/12/06 00:43:02 vixie Exp $
#

PATH=/bin:/sbin:/usr/sbin:/usr/bin:/etc:/usr/etc:%DESTSBIN%

pid=`cat %PIDDIR%/named.pid`
kill $pid
sleep 5
exec %INDOT%named 
