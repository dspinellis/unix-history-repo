#	Makefile	4.4	84/03/27
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	apl bib courier cpm dsh icon hyper learn mh news notes rcs sccstorcs \
	spms tools 

# Shell scripts that need only be installed and are never removed.
#
SHSCRIPT= 
CSHSCRIPT= 

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	chmod chown chgrp

# Programs that must run setuid to root
#
SETUID=	

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	for i in ${SHSCRIPT}; do \
		(install -c $$i.sh ${DESTDIR}/usr/ucb/$$i); done
	for i in ${CSHSCRIPT}; do \
		(install -c $$i.csh ${DESTDIR}/usr/ucb/$$i); done
	for i in ${STD} ${NSTD}; do \
		(install -s $$i ${DESTDIR}/usr/ucb/$$i); \
	done
	for i in ${SETUID}; do (chown root ${DESTDIR}/usr/ucb/$$i; \
		chmod 4755 ${DESTDIR}/usr/ucb/$$i); done

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}

FRC:

depend:
	cat </dev/null >x.c
	for i in ${STD} ${NSTD}; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ex - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# Files listed in ${NSTD} have explicit make lines given below.

# DO NOT DELETE THIS LINE -- make depend uses it

chmod: chmod.c
chmod: /usr/include/stdio.h
chmod: /usr/include/sys/types.h
chmod: /usr/include/sys/stat.h
chmod: /usr/include/sys/dir.h
chown: chown.c
chown: /usr/include/stdio.h
chown: /usr/include/ctype.h
chown: /usr/include/sys/types.h
chown: /usr/include/sys/stat.h
chown: /usr/include/pwd.h
chown: /usr/include/sys/dir.h
chown: /usr/include/grp.h
chgrp: chgrp.c
chgrp: /usr/include/stdio.h
chgrp: /usr/include/ctype.h
chgrp: /usr/include/sys/types.h
chgrp: /usr/include/sys/stat.h
chgrp: /usr/include/grp.h
chgrp: /usr/include/pwd.h
chgrp: /usr/include/sys/dir.h
chmod: chmod.c
chmod: /usr/include/stdio.h
chmod: /usr/include/sys/types.h
chmod: /usr/include/sys/stat.h
chmod: /usr/include/sys/dir.h
chown: chown.c
chown: /usr/include/stdio.h
chown: /usr/include/ctype.h
chown: /usr/include/sys/types.h
chown: /usr/include/sys/stat.h
chown: /usr/include/pwd.h
chown: /usr/include/sys/dir.h
chown: /usr/include/grp.h
chgrp: chgrp.c
chgrp: /usr/include/stdio.h
chgrp: /usr/include/ctype.h
chgrp: /usr/include/sys/types.h
chgrp: /usr/include/sys/stat.h
chgrp: /usr/include/grp.h
chgrp: /usr/include/pwd.h
chgrp: /usr/include/sys/dir.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
