#	@(#)Makefile	8.1 (Berkeley) 6/9/93

# If you don't have snprintf/vsnprintf(3), add snprintf.o to the MISC line.
# Note, this depends on your having vsprintf(3) -- if you don't, there's
# no workaround other than changing the source code to not use the snprintf
# calls.  If you have to make that change, check to see if your vsprintf
# returns a length or a char *; if it's the latter, set VSPRINTF_CHARSTAR
# in the include/compat.h file.
#
# If you don't have err(3) or warn(3), add err.o to the MISC line.
#
# If you don't have strerror(3), add strerror.o to the MISC line.
#
MISC=
CFLAGS=	-c -g -I.. -I../.. -I../include

nvi.recover: nvi.recover.o ${MISC}
	${CC} -o $@ ${MISC} nvi.recover.o

clean:
	rm -f ${MISC} nvi.recover.o nvi.recover

CL=	${CC} -c -g -I.. -I../include
err.o:
	${CL} ../clib/err.c
snprintf.o:
	${CL} ../clib/snprintf.c
strerror.o:
	${CL} ../clib/strerror.c
