#	Makefile	1.5	85/09/05
#
CFLAGS=	-O ${DEFS}
SRCS=	getopt.c memccpy.c memchr.c memcmp.c memcpy.c memset.c \
	strcatn.c strchr.c strcmpn.c strcpyn.c strcspn.c strpbrk.c \
	strrchr.c strspn.c strtok.c tmpnam.c
OBJS=	getopt.o memccpy.o memchr.o memcmp.o memcpy.o memset.o \
	strcatn.o strchr.o strcmpn.o strcpyn.o strcspn.o strpbrk.o \
	strrchr.o strspn.o strtok.o tmpnam.o
TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

compat-sys5lib compat-sys5lib_p: ${OBJS}
	@echo "building profiled compat-sys5lib"
	@cd profiled; ar cru ../compat-sys5lib_p ${OBJS}
	@echo "buiding normal compat-sys5lib"
	@ar cru compat-sys5lib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f compat-sys5lib compat-sys5lib_p *.o profiled/*.o Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

getopt.o: getopt.c /usr/include/stdio.h
memccpy.o: memccpy.c
memchr.o: memchr.c
memcmp.o: memcmp.c
memcpy.o: memcpy.c
memset.o: memset.c
strcatn.o: strcatn.c
strchr.o: strchr.c
strcmpn.o: strcmpn.c
strcpyn.o: strcpyn.c
strcspn.o: strcspn.c
strpbrk.o: strpbrk.c
strrchr.o: strrchr.c
strspn.o: strspn.c
strtok.o: strtok.c
tmpnam.o: tmpnam.c
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
