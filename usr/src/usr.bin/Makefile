#	@(#)Makefile	5.8 (Berkeley) %G%

# need conversion:	tn3270
# remove lisp

SUBDIR=	apply ar at awk banner basename bc bdes biff cal calendar \
	checknr chpass cksum cmp col colcrt colrm column comm compress \
	cpio ctags cut dc deroff diction diff dirname du env error ex \
	expand false file find finger fmt fold fpr from fsplit fstat ftp \
	g++ gcore gdb gprof graph grep groff groups head hexdump indent \
	join ktrace last lastcomm ld learn leave lex locate lock logger \
	login look lorder m4 machine mail make man mesg mkdep mkfifo \
	mkstr more msgs mt netstat nfsstat nice nm nohup pagesize \
	passwd paste plot pr printenv printf ptx quota ranlib ratfor \
	rdist refer renice rlogin rsh ruptime rwho sccs script sed \
	shar showmount size soelim sort spell spline split strings \
	strip struct su symorder tail talk tcopy tee telnet tftp time \
	tip touch tput tr true tset tsort tty ul unexpand unifdef uniq \
	units unvis users uucp uudecode uuencode vacation vgrind vis \
	vmstat w wall wc what whereis which who whoami whois window \
	write xargs xinstall xsend xstr yacc yes

.if	${MACHINE} == "hp300"
SUBDIR+=gas gcc
.elif	${MACHINE} == "i386"
SUBDIR+=gas gcc
.elif	${MACHINE} == "tahoe"
SUBDIR+=f77 pascal systat
.elif	${MACHINE} == "vax"
SUBDIR+=f77 gas pascal systat
.endif

.include <bsd.subdir.mk>
