.\" Copyright (c) 1986, 1988 Regents of the University of California.
.\" All rights reserved.
.\"
.\" Redistribution and use in source and binary forms are permitted
.\" provided that this notice is preserved and that due credit is given
.\" to the University of California at Berkeley. The name of the University
.\" may not be used to endorse or promote products derived from this
.\" software without specific prior written permission. This software
.\" is provided ``as is'' without express or implied warranty.
.\"
.\"	@(#)named.boot.primary	6.3 (Berkeley) 2/28/88
.\"
.sh 3 "Boot File"
.sh 4 "Primary Master Server"
.(b L
.TS
l
l s
l.
;
; Boot file for Primary Master Name Server
;
.TE
.TS
l l l
l
l l l.
; type	domain	source file or host
;
directory	/usr/local/domain
primary	Berkeley\fB.\fPEdu	ucbhosts
primary	32\fB.\fP128\fB.\fPin-addr\fB.\fParpa	ucbhosts\fB.\fPrev
primary	0\fB.\fP0\fB.\fP127\fB.\fPin-addr\fB.\fParpa	named\fB.\fPlocal
cache	\fB.\fP	root\fB.\fPcache
.TE
.)b
