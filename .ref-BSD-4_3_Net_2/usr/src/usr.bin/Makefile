#	@(#)Makefile	5.8.1.1 (Berkeley) 5/8/91

# need conversion:	tn3270
# remove lisp

SUBDIR=	ar banner basename bdes biff cal calendar \
	checknr chpass cksum cmp col colcrt colrm column comm compress \
	cpio ctags cut dirname du env error \
	expand false find finger fmt fold fpr from fsplit fstat ftp \
	g++ gcore gdb gprof groff groups head hexdump id indent \
	ktrace last lastcomm ld leave lex locate lock logger \
	login logname lorder m4 machine mail make man mesg mkdep mkfifo \
	mkstr more msgs mt netstat nfsstat nice nm nohup pagesize \
	passwd paste printenv printf quota ranlib \
	rdist renice rlogin rsh ruptime rwho sccs script \
	shar showmount size soelim split strings \
	strip su symorder talk tcopy tee telnet tftp time \
	tip touch tput tr true tset tsort tty ul unexpand unifdef uniq \
	unvis users uudecode uuencode vacation vgrind vis \
	vmstat w wall wc what whereis which who whoami whois window \
	write xargs xinstall xstr yacc yes

.if	${MACHINE} == "hp300"
SUBDIR+=gas gcc
.elif	${MACHINE} == "i386"
SUBDIR+=gas gcc
.elif	${MACHINE} == "tahoe"
SUBDIR+=pascal
.elif	${MACHINE} == "vax"
SUBDIR+=gas pascal
.endif

.include <bsd.subdir.mk>
