VERSION=3.5
#
# Ex skeletal makefile for VAX VM/Unix version 7
#
# NB: This makefile doesn't indicate any dependencies on header files.
#
# Ex is very large - this version will not fit on PDP-11's without overlay
# software.  Things that can be turned off to save
# space include LISPCODE (-l flag, showmatch and lisp options), UCVISUAL
# (visual \ nonsense on upper case only terminals), CHDIR (the undocumented
# chdir command.)  CRYPT includes the code to edit encrypted files (the -x
# option, like ed.)  VMUNIX makes ex considerably larger, raising many limits
# and improving speed and simplicity of maintenance.  It is suitable only
# for a VAX or other large machine, and then probably only in a paged system.
#
# Don't define VFORK unless your system has the VFORK system call,
# which is like fork but the two processes have only one data space until the
# child execs. This speeds up ex by saving the memory copy.
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below
#
BINDIR=	/usr/ucb
NBINDIR=/usr/new
LIBDIR=	/usr/lib
FOLD=	${BINDIR}/fold
CTAGS=	${BINDIR}/ctags
XSTR=	${BINDIR}/xstr
DEBUGFLAGS=	-DTRACE -g
NONDEBUGFLAGS=	-O
DEB=	${NONDEBUGFLAGS}	# or ${DEBUGFLAGS} to to debug
CFLAGS=	-DTABS=8 -DCRYPT -DLISPCODE -DCHDIR -DUCVISUAL -DVFORK -DVMUNIX ${DEB}
LDFLAGS=	-z		# or -i or -n
TERMLIB=	-ltermlib
MKSTR=	${BINDIR}/mkstr
CXREF=	${BINDIR}/cxref
INCLUDE=/usr/include
PR=	pr
OBJS=	ex.o ex_addr.o ex_cmds.o ex_cmds2.o ex_cmdsub.o \
	ex_data.o ex_get.o ex_io.o ex_put.o ex_re.o \
	ex_set.o ex_subr.o ex_temp.o ex_tty.o ex_unix.o \
	ex_v.o ex_vadj.o ex_vget.o ex_vmain.o ex_voperate.o \
	ex_vops.o ex_vops2.o ex_vops3.o ex_vput.o ex_vwind.o \
	printf.o bcopy.o strings.o
HDRS=	ex.h ex_argv.h ex_re.h ex_temp.h ex_tty.h ex_tune.h ex_vars.h ex_vis.h
SRC1=	ex.c ex_addr.c ex_cmds.c ex_cmds2.c ex_cmdsub.c
SRC2=	ex_data.c ex_get.c ex_io.c ex_put.c ex_re.c
SRC3=	ex_set.c ex_subr.c ex_temp.c ex_tty.c ex_unix.c
SRC4=	ex_v.c ex_vadj.c ex_vget.c ex_vmain.c ex_voperate.c
SRC5=	ex_vops.c ex_vops2.c ex_vops3.c ex_vput.c ex_vwind.c
SRC6=	printf.c bcopy.c expreserve.c exrecover.c
MISC=	makefile READ_ME :rofix
VGRIND=	csh /usr/ucb/vgrind
VHDR=	"Ex Version ${VERSION}"

.c.o:
#	${MKSTR} - ex${VERSION}strings x $*.c
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
#	rm -f x$*.c
	${CC} ${CFLAGS} -c x.c 
	mv x.o $*.o

a.out: ${OBJS}
	${CC} ${LDFLAGS} ${OBJS} ${TERMLIB}

all:	a.out exrecover expreserve tags

tags:	/tmp
	${CTAGS} -w ex.[hc] ex_*.[hc]

${OBJS}: ex_vars.h

# ex_vars.h:
# 	csh makeoptions ${CFLAGS}

bcopy.o:	bcopy.c
	${CC} -c ${CFLAGS} bcopy.c

# xstr: hands off!
strings.o: strings
	${XSTR}
	${CC} -c -S xs.c
	ed - <:rofix xs.s
	${AS} -o strings.o xs.s
	rm xs.s
	
exrecover: exrecover.o
	${CC} ${CFLAGS} exrecover.o -o exrecover

exrecover.o: exrecover.c
	${CC} ${CFLAGS} -c -O exrecover.c

expreserve: expreserve.o
	${CC} expreserve.o -o expreserve

expreserve.o:
	${CC} ${CFLAGS} -c -O expreserve.c

clean:
#	If we dont have ex we cant make it so dont rm ex_vars.h
	-rm -f a.out exrecover expreserve strings core errs trace
	-rm -f *.o x*.[cs]

# install a new version for testing in /usr/new
ninstall: a.out
	-rm -f ${DESTDIR}${NBINDIR}/ex ${DESTDIR}${NBINDIR}/vi ${DESTDIR}${NBINDIR}/view
	cp a.out ${DESTDIR}${NBINDIR}/ex
#	-cp ex${VERSION}strings ${LIBDIR}/ex${VERSION}strings
	ln ${DESTDIR}${NBINDIR}/ex ${DESTDIR}${NBINDIR}/vi
	ln ${DESTDIR}${NBINDIR}/ex ${DESTDIR}${NBINDIR}/view
	chmod 1755 ${DESTDIR}${NBINDIR}/ex

# install in standard place (/usr/ucb)
install: a.out exrecover expreserve
	-rm -f ${DESTDIR}${BINDIR}/ex
	-rm -f ${DESTDIR}${BINDIR}/vi
	-rm -f ${DESTDIR}${BINDIR}/view
	-rm -f ${DESTDIR}${BINDIR}/edit
	-rm -f ${DESTDIR}${BINDIR}/e
	-rm -f ${DESTDIR}/usr/bin/ex
	cp a.out ${DESTDIR}${BINDIR}/ex
#	cp ex${VERSION}strings ${DESTDIR}/${LIBDIR}/ex${VERSION}strings
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/edit
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/e
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/vi
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/view
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}/usr/bin/ex
	chmod 1755 ${DESTDIR}${BINDIR}/ex
	cp exrecover ${DESTDIR}${LIBDIR}/ex${VERSION}recover
	cp expreserve ${DESTDIR}${LIBDIR}/ex${VERSION}preserve
	chmod 4755 ${DESTDIR}${LIBDIR}/ex${VERSION}recover ${DESTDIR}${LIBDIR}/ex${VERSION}preserve
# The following line normally fails.  This is OK.
	mkdir ${DESTDIR}/usr/preserve

# move from /usr/new to /usr/ucb
newucb: a.out
	-rm -f ${DESTDIR}${BINDIR}/ex
	-rm -f ${DESTDIR}${BINDIR}/vi
	-rm -f ${DESTDIR}${BINDIR}/edit
	-rm -f ${DESTDIR}${BINDIR}/e
	-rm -f ${DESTDIR}/usr/bin/ex
	mv ${DESTDIR}${NBINDIR}/ex ${DESTDIR}${NBINDIR}/ex
	-rm -f ${DESTDIR}${NBINDIR}/vi
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/edit
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/e
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/vi
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}/usr/bin/ex
	chmod 1755 ${DESTDIR}${BINDIR}/ex

lint:
	lint ${CFLAGS} ex.c ex_?*.c
	lint ${CFLAGS} -u exrecover.c
	lint ${CFLAGS} expreserve.c

print:
	@${PR} READ* BUGS
	@${PR} makefile*
	@${PR} /etc/termcap
	@(size -l a.out ; size *.o) | ${PR} -h sizes
	@${PR} -h errno.h ${INCLUDE}/errno.h
	@${PR} -h setjmp.h ${INCLUDE}/setjmp.h
	@${PR} -h sgtty.h ${INCLUDE}/sgtty.h
	@${PR} -h signal.h ${INCLUDE}/signal.h
	@${PR} -h sys/stat.h ${INCLUDE}/sys/stat.h
	@${PR} -h sys/types.h ${INCLUDE}/sys/types.h
	@ls -ls | ${PR}
	@${CXREF} *.c | ${PR} -h XREF
	@${PR} *.h *.c
vgrind:
	tee index < /dev/null
	${VGRIND} -h ${VHDR} ${HDRS}
	${VGRIND} -h ${VHDR} ${SRC1}
	${VGRIND} -h ${VHDR} ${SRC2}
	${VGRIND} -h ${VHDR} ${SRC3}
	${VGRIND} -h ${VHDR} ${SRC4}
	${VGRIND} -h ${VHDR} ${SRC5}
	${VGRIND} -h ${VHDR} ${SRC6}
	${VGRIND} -n -h ${VHDR} ${MISC}
	${VGRIND} -i -h ${VHDR} index

ex.c: SCCS/s.ex.c ${HDRS}; rm -f ex.c; sccs get ex.c 
ex.h: SCCS/s.ex.h; rm -f ex.h; sccs get ex.h 
ex_addr.c: SCCS/s.ex_addr.c; rm -f ex_addr.c; sccs get ex_addr.c 
ex_argv.h: SCCS/s.ex_argv.h; rm -f ex_argv.h; sccs get ex_argv.h 
ex_cmds.c: SCCS/s.ex_cmds.c; rm -f ex_cmds.c; sccs get ex_cmds.c 
ex_cmds2.c: SCCS/s.ex_cmds2.c; rm -f ex_cmds2.c; sccs get ex_cmds2.c 
ex_cmdsub.c: SCCS/s.ex_cmdsub.c; rm -f ex_cmdsub.c; sccs get ex_cmdsub.c 
ex_data.c: SCCS/s.ex_data.c; rm -f ex_data.c; sccs get ex_data.c 
ex_get.c: SCCS/s.ex_get.c; rm -f ex_get.c; sccs get ex_get.c 
ex_io.c: SCCS/s.ex_io.c; rm -f ex_io.c; sccs get ex_io.c 
ex_put.c: SCCS/s.ex_put.c; rm -f ex_put.c; sccs get ex_put.c 
ex_re.c: SCCS/s.ex_re.c; rm -f ex_re.c; sccs get ex_re.c 
ex_re.h: SCCS/s.ex_re.h; rm -f ex_re.h; sccs get ex_re.h 
ex_set.c: SCCS/s.ex_set.c; rm -f ex_set.c; sccs get ex_set.c 
ex_subr.c: SCCS/s.ex_subr.c; rm -f ex_subr.c; sccs get ex_subr.c 
ex_temp.c: SCCS/s.ex_temp.c; rm -f ex_temp.c; sccs get ex_temp.c 
ex_temp.h: SCCS/s.ex_temp.h; rm -f ex_temp.h; sccs get ex_temp.h 
ex_tty.c: SCCS/s.ex_tty.c; rm -f ex_tty.c; sccs get ex_tty.c 
ex_tty.h: SCCS/s.ex_tty.h; rm -f ex_tty.h; sccs get ex_tty.h 
ex_tune.h: SCCS/s.ex_tune.h; rm -f ex_tune.h; sccs get ex_tune.h 
ex_unix.c: SCCS/s.ex_unix.c; rm -f ex_unix.c; sccs get ex_unix.c
ex_v.c: SCCS/s.ex_v.c; rm -f ex_v.c; sccs get ex_v.c 
ex_vadj.c: SCCS/s.ex_vadj.c; rm -f ex_vadj.c; sccs get ex_vadj.c 
ex_vars.h: SCCS/s.ex_vars.h; rm -f ex_vars.h; sccs get ex_vars.h 
ex_vget.c: SCCS/s.ex_vget.c; rm -f ex_vget.c; sccs get ex_vget.c 
ex_vis.h: SCCS/s.ex_vis.h; rm -f ex_vis.h; sccs get ex_vis.h 
ex_vmain.c: SCCS/s.ex_vmain.c; rm -f ex_vmain.c; sccs get ex_vmain.c 
ex_voperate.c: SCCS/s.ex_voperate.c; rm -f ex_voperate.c; sccs get ex_voperate.c
ex_vops.c: SCCS/s.ex_vops.c; rm -f ex_vops.c; sccs get ex_vops.c 
ex_vops2.c: SCCS/s.ex_vops2.c; rm -f ex_vops2.c; sccs get ex_vops2.c 
ex_vops3.c: SCCS/s.ex_vops3.c; rm -f ex_vops3.c; sccs get ex_vops3.c 
ex_vput.c: SCCS/s.ex_vput.c; rm -f ex_vput.c; sccs get ex_vput.c 
ex_vwind.c: SCCS/s.ex_vwind.c; rm -f ex_vwind.c; sccs get ex_vwind.c 
expreserve.c: SCCS/s.expreserve.c; rm -f expreserve.c; sccs get expreserve.c 
exrecover.c: SCCS/s.exrecover.c; rm -f exrecover.c; sccs get exrecover.c 
makeoptions: SCCS/s.makeoptions; rm -f makeoptions; sccs get makeoptions 
bcopy.c: SCCS/s.bcopy.c; rm -f bcopy.c; sccs get bcopy.c
printf.c: SCCS/s.printf.c; rm -f printf.c; sccs get printf.c 
