#	@(#)Makefile	5.19 (Berkeley) %G%

# BROKEN: tn3270
SUBDIR=	apply ar at awk banner basename bc bdes biff cal calendar cap_mkdb \
	checknr chflags chpass cksum col colcrt colrm column comm compress \
	cpio cpp ctags cut dc deroff diction diff dirname du env error ex \
	expand false file find finger fmt fold fpr from fsplit fstat ftp \
	gcore gprof graph grep head hexdump id indent join last \
	lastcomm learn leave lex locate lock logger login logname lorder \
	m4 mail make man mesg mkdep mkfifo mkstr more msgs mt netstat \
	nfsstat nice nm nohup nvi pagesize passwd paste plot pr printenv \
	printf ptx quota ranlib ratfor rdist renice rev rlogin rsh ruptime \
	rwho sccs script sed shar showmount size soelim sort spell spline \
	split strings strip struct su talk tcopy tee telnet tftp \
	time tip touch tput tr true tset tsort tty ul uname unexpand \
	unifdef uniq units unvis users uucp uudecode uuencode vacation \
	vgrind vis vmstat w wall wc what who whois window write xargs \
	xinstall xsend xstr yacc yes

# Cmp, look and tail all use mmap, so new-VM only.
# F77 and pascal are VAX/Tahoe only.
.if	${MACHINE} == "hp300"
SUBDIR+=cmp ktrace ld look systat tail
.elif	${MACHINE} == "i386"
SUBDIR+=cmp ktrace ld look systat tail
.elif	${MACHINE} == "luna68k"
SUBDIR+=cmp ktrace ld look systat tail
.elif	${MACHINE} == "sparc"
SUBDIR+=cmp ktrace ld look tail
.elif	${MACHINE} == "tahoe"
SUBDIR+=pascal
.elif	${MACHINE} == "vax"
SUBDIR+=gas pascal
.endif

.include <bsd.subdir.mk>
