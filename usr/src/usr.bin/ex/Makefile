VERSION=3.4
#
# Ex skeletal makefile for VAX VM/Unix version 7
#
# NB: This makefile doesn't indicate any dependencies on header files.
#
# Ex is very large - this version will not fit on PDP-11's without overlay
# software.  Things that can be turned off to save
# space include LISPCODE (-l flag, showmatch and lisp options), UCVISUAL
# (visual \ nonsense on upper case only terminals), CHDIR (the undocumented
# chdir command.)  VMUNIX makes ex considerably larger, raising many limits
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
#
# D O   N O T   D I S T R I B U T E   E X    W I T H   H O R S E   I N   I T!!!
NONDEBUGFLAGS=	-O -DHORSE
DEB=	${NONDEBUGFLAGS}	# or ${DEBUGFLAGS} to to debug
CFLAGS=	-DTABS=8 -DLISPCODE -DCHDIR -DUCVISUAL -DVFORK -DVMUNIX ${DEB}
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
	printf.o bcopy.o horse.o strings.o
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
	cc ${LDFLAGS} ${OBJS} ${TERMLIB}

all:	a.out exrecover expreserve tags

tags:	/tmp
	${CTAGS} -w ex.[hc] ex_*.[hc]

${OBJS}: ex_vars.h

ex_vars.h:
	csh makeoptions ${CFLAGS}

bcopy.o:	bcopy.c
	cc -c ${CFLAGS} bcopy.c

# xstr: hands off!
strings.o: strings
	${XSTR}
	${CC} -c -S xs.c
	ed - <:rofix xs.s
	as -o strings.o xs.s
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
#	${VGRIND} -h ${VHDR} ${HDRS}
#	${VGRIND} -h ${VHDR} ${SRC1}
#	${VGRIND} -h ${VHDR} ${SRC2}
	${VGRIND} -h ${VHDR} ${SRC3}
	${VGRIND} -h ${VHDR} ${SRC4}
	${VGRIND} -h ${VHDR} ${SRC5}
	${VGRIND} -h ${VHDR} ${SRC6}
#	${VGRIND} -n -h ${VHDR} ${MISC}
	${VGRIND} -i -h ${VHDR} index
