divert(10)
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)boilerplate.m4	1.2 (Berkeley) 1/3/89
#
divert(0)
######################
#   Special macros   #
######################

# my name
DnMAILER-DAEMON
# UNIX header format
DlFrom $g  $d
# delimiter (operator) characters
Do.:%@!^=/[]
# format of a total name
Dq$g$?x ($x)$.
# SMTP login message
De$j Sendmail $v/$Z ready at $b

###############
#   Options   #
###############

# location of alias file
OA/etc/aliases
# wait up to ten minutes for alias file rebuild
Oa10
# substitution for space (blank) characters
OB.
# (don't) connect to "expensive" mailers
#Oc
# default delivery mode (deliver in background)
Odbackground
# temporary file mode
OF0600
# default GID
Og1
# location of help file
OH/usr/share/misc/sendmail.hf
# log level
OL9
# default network name
ONARPA
# default messages to old style
Oo
# queue directory
OQ/var/spool/mqueue
# read timeout -- violates protocols
Or2h
# status file
OS/var/log/sendmail.st
# queue up everything before starting transmission
Os
# default timeout interval
OT3d
# time zone names (V6 only)
OtPST,PDT
# default UID
Ou1
# wizard's password
OW*
# load average at which we just queue messages
Ox8
# load average at which we refuse connections
OX12

###########################
#   Message precedences   #
###########################

Pfirst-class=0
Pspecial-delivery=100
Pbulk=-60
Pjunk=-100

#####################
#   Trusted users   #
#####################

Troot
Tdaemon
Tuucp

#########################
#   Format of headers   #
#########################

H?P?Return-Path: <$g>
HReceived: $?sfrom $s $.by $j ($v/$Z)
	id $i; $b
H?D?Resent-Date: $a
H?D?Date: $a
H?F?Resent-From: $q
H?F?From: $q
H?x?Full-Name: $x
HSubject:
# HPosted-Date: $a
# H?l?Received-Date: $b
H?M?Resent-Message-Id: <$t.$i@$j>
H?M?Message-Id: <$t.$i@$j>

