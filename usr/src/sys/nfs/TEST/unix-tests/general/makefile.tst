#
#       @(#)makefile.tst	1.1 90/01/03 NFS Rev 2 Testsuite
#
HDRS = /usr/include/stdio.h /usr/include/ctype.h \
       /usr/include/a.out.h /usr/include/pwd.h \
       /usr/include/signal.h /usr/include/setjmp.h
XOBJ = dummy1.o  dummy2.o  dummy3.o  dummy4.o  dummy5.o \
       dummy6.o  dummy7.o  dummy8.o  dummy9.o  dummy10.o \
       dummy11.o dummy12.o dummy13.o dummy14.o dummy15.o \
       dummy16.o dummy17.o dummy18.o dummy19.o dummy20.o \
       dummy21.o dummy22.o dummy23.o dummy24.o dummy25.o
.c.o:	; echo $@ > /dev/null

x:	$(XOBJ)
	echo done > /dev/null

dummy1.o:	dummy1.c $(HDRS)
dummy2.o:	dummy2.c $(HDRS)
dummy3.o:	dummy3.c $(HDRS)
dummy4.o:	dummy4.c $(HDRS)
dummy5.o:	dummy5.c $(HDRS)
dummy6.o:	dummy6.c $(HDRS)
dummy7.o:	dummy7.c $(HDRS)
dummy8.o:	dummy8.c $(HDRS)
dummy9.o:	dummy9.c $(HDRS)
dummy10.o:	dummy10.c $(HDRS)
dummy11.o:	dummy11.c $(HDRS)
dummy12.o:	dummy12.c $(HDRS)
dummy13.o:	dummy13.c $(HDRS)
dummy14.o:	dummy14.c $(HDRS)
dummy15.o:	dummy15.c $(HDRS)
dummy16.o:	dummy16.c $(HDRS)
dummy17.o:	dummy17.c $(HDRS)
dummy18.o:	dummy18.c $(HDRS)
dummy19.o:	dummy19.c $(HDRS)
dummy20.o:	dummy20.c $(HDRS)
dummy21.o:	dummy21.c $(HDRS)
dummy22.o:	dummy22.c $(HDRS)
dummy23.o:	dummy23.c $(HDRS)
dummy24.o:	dummy24.c $(HDRS)
dummy25.o:	dummy25.c $(HDRS)
