#	@(#)Makefile	5.5.1.1 (Berkeley) 10/21/90

# need conversion:	tn3270

SUBDIR=	apply at awk banner basename bc biff cal calendar checknr chpass \
	cmp col colcrt colrm column comm compress cpio cut dc deroff des \
	diction diff dirname du env eqn ex expand false file find finger \
	fmt fold from fstat ftp graph grep groups head hexdump join ktrace \
	last lastcomm learn leave ln locate lock logger login look machine \
	mail make man mesg mkfifo more msgs mt netstat nfsstat nice nohup \
	pagesize passwd paste plot pr printenv printf ptx quota \
	rdist refer renice rlogin roff rsh ruptime rwho sccs script sed \
	shar showmount soelim sort spell spline split su sum sysline \
	tail talk tbl tcopy tee telnet tftp time tip touch tput tr true \
	tset tty ul unexpand uniq units unvis uucp users uudecode uuencode \
	vacation vis vmstat w wall wc what whereis which who whoami whois \
	window write xargs yes

.if   make(clean) || make(cleandir)
SUBDIR+=systat
.elif ${MACHINE} == "tahoe"
SUBDIR+=systat
.elif ${MACHINE} == "vax"
SUBDIR+=systat
.endif

.include <bsd.subdir.mk>
