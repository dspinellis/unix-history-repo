DESTDIR=
CONFDIR= /usr/new
INCLUDES= -I../include

# set up for the rare 480 dpi imagen
#CFLAGS = -O -DXWIND -DIMAGEN300 $(INCLUDES)
CFLAGS = -O -DXWIND -DIMAGEN480 $(INCLUDES)
XLIB = ../Xlib/libX.a
SRCS = impv.c decode.c dofile.c
OBJS = impv.o decode.o dofile.o

all: 	ximpv

$(OBS}: $(SRCS)

ximpv:	impv.o dofile.o decode.o site.h ../Xlib/Xlib.h
	cc ${CFLAGS} impv.o dofile.o decode.o -lm ${XLIB} -o ximpv

install: ximpv
	install -c -s -m 755 ximpv ${DESTDIR}${CONFDIR}

ximpv-test:
	ximpv test.man.data

clean:
	rm -f ximpv *.o *.bak *~ \#*

depend:
	cc -M ${CFLAGS} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
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
# DEPENDENCIES MUST END AT END OF FILE

impv.o: impv.c ../include/X/Xlib.h ../include/X/mit-copyright.h
impv.o: /usr/include/sys/types.h ../include/X/X.h ../include/X/mit-copyright.h
impv.o: ../include/X/Xkeyboard.h ../include/X/mit-copyright.h
impv.o: /usr/include/sys/types.h /usr/include/strings.h /usr/include/sys/stat.h
impv.o: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/signal.h
impv.o: /usr/include/netdb.h /usr/include/netinet/in.h /usr/include/sgtty.h
impv.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
impv.o: /usr/include/sys/ttydev.h ./site.h ./imPdefs.h ./imPcodes.h ./impv.h
decode.o: decode.c ./imPdefs.h ./imPcodes.h
dofile.o: dofile.c ./site.h ./imPdefs.h ./imPcodes.h ./impv.h
dofile.o: /usr/include/stdio.h

