divert(10)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)boilerplate.m4	1.3 (Berkeley) 2/15/89
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
OA/usr/lib/aliases
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
OH/usr/lib/sendmail.hf
# log level
OL9
# default network name
ONARPA
# default messages to old style
Oo
# queue directory
OQ/usr/spool/mqueue
# read timeout -- violates protocols
Or2h
# status file
OS/usr/lib/sendmail.st
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

