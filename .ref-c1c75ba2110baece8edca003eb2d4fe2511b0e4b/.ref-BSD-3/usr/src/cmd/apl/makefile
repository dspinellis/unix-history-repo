CFLAGS = -O -w
AS = -as
OBJS = \
a0.o a1.o a2.o a3.o a4.o a5.o a6.o a7.o a8.o a9.o aa.o ab.o ac.o ad.o\
ae.o af.o ag.o ah.o ai.o aj.o ak.o al.o an.o ao.o aq.o aw.o az.o apl_s.o\
gamma.o apl.o asciiterm.o write2.o write3.o

apl: $(OBJS)
	cc $(OBJS) -lm -o apl
apl.o: apl.c
apl.c:	 apl.y
	yacc apl.y
	mv y.tab.c apl.c
clean:
	rm -f apl *.o apl.c
install:
	install -s apl ${DESTDIR}/usr/ucb
