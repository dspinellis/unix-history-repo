CFLAGS = -O

ratfor:	r0.o r1.o r2.o rio.o rlook.o rlex.o y.tab.o
	cc r*.o y.tab.o -o ratfor

r0.o:	r.h y.tab.h r0.c
r1.o:	r.h y.tab.h r1.c
r2.o:	r.h y.tab.h r2.c
rio.o:	r.h y.tab.h rio.c
rlook.o:	r.h y.tab.h rlook.c
rlex.o:	r.h y.tab.h rlex.c
y.tab.c:	r.g
	yacc -d r.g
y.tab.h:	r.g
	yacc -d r.g
install: ratfor
	install -s ratfor $(DESTDIR)/usr/bin
clean:
	rm -f y.tab.c *.o ratfor y.tab.h

