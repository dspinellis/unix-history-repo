#
# Copyright (c) 1989 The Regents of the University of California.
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
#	@(#)Makefile	5.1 (Berkeley) %G%
#

# not currently working
# pstat, tn3270, w

SUBDIR=	apply awk banner basename bc biff cal calendar checknr \
	chpass cmp col colcrt colrm comm compress cpio cut \
	dc deroff des diction diff diff3 du eqn ex \
	expand false file find finger fold from fstat ftp \
	graph grep groups head hexdump join last lastcomm learn \
	leave ln locate lock \
	logger look machine mail make man mesg more msgs mt netstat \
	nfsstat nice nohup pagesize passwd paste plot pr printenv \
	printf ptx quota rdist refer renice rlogin roff rsh \
	ruptime rwho sccs script sed sendbug soelim sort spell spline \
	split su sum sysline systat tail talk tbl tc tcopy tee telnet \
	tftp time tip touch tput tr true tset tty ul uniq \
	units users uucp uuencode vacation vmstat wall wc what whereis \
	which \
	who whoami whois window write xsend yes

all clean cleandir depend install lint tags: FRC
	@-for i in ${SUBDIR}; do \
		(echo "==> $$i"; cd $$i; \
		    make ${MFLAGS} DESTDIR=${DESTDIR} $@); done

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} DESTDIR=${DESTDIR}

FRC:
