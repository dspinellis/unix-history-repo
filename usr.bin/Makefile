#	@(#)Makefile	5.8.1.1 (Berkeley) 5/8/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         2       00124
# --------------------         -----   ----------------------
#
# 09 Mar 93	Rodney W. Grimes	Added netstat, rwho, and ruptime
#					to SUBDIR
# 04 Apr 93	Rodney W. Grimes	Added 15 programs, disabled tn3270,
#					enable msgs
#

SUBDIR=	ar awk bc biff basename cal calendar \
	checknr chpass cksum cmp col colcrt colrm column comm compress \
	cpio crontab ctags cut diff diff3 dirname du egrep elvis elvisrecover \
	env error expand false fgrep file find finger fmt fold fpr from fsplit \
	fstat ftp \
	gas gcc2 gdb grep gprof groff groups gzip head hexdump id indent join \
	ktrace last lastcomm ld leave lex locate lock logger \
	login logname look lorder m4 machine mail make man mesg mkdep mkfifo \
	mkstr more msgs mt netstat nfsstat nice nm nohup pagesize \
	passwd paste pr printenv printf quota ranlib rcs \
	rdist renice ref rlogin rsh ruptime rwho sccs script sed \
	shar showmount size soelim sort split strings \
	strip su symorder tail talk tar tcopy tee telnet tftp time \
	tip touch tput tr true tset tsort tty ul unexpand unifdef uniq \
	unvis users uudecode uuencode vacation vgrind vis vmstat \
	w wall wc what whereis which who whoami whois \
	window write xargs xinstall xstr yacc yes
	

# disabled tn3270

awk::
	@rm -f awk
	ln -s ../gnu/gawk awk
bc::
	@rm -f bc
	ln -s ../gnu/bc .
gcc1::
	@rm -f gcc1
	ln -s ../gnu/gcc1 .
gcc2::
	@rm -f gcc2
	ln -s ../gnu/gcc2 .
groff::
	@rm -f groff
	ln -s ../gnu/groff .
gzip::
	@rm -f gzip
	ln -s ../gnu/gzip .
tar::
	@rm -f tar
	ln -s ../gnu/tar . 

.include <bsd.subdir.mk>
