sdb:	a.out

decode.o:	head.h decode.c
docomm.o:	head.h docomm.c
main.o:		head.h main.c
sub.o:		head.h sub.c
re.o:		head.h re.c
fio.o:		head.h fio.c
bio.o:		bio.h bio.c
access.o:	defs.h mac.h machine.h mode.h access.c
pcs.o:		head.h defs.h mac.h machine.h mode.h pcs.c
runpcs.o:	head.h defs.h mac.h machine.h mode.h runpcs.c
xeq.o:		head.h xeq.c
setup.o:	head.h defs.h mac.h machine.h mode.h setup.c
message.o:	defs.h mac.h machine.h mode.h message.c
udef.o:		udef.c
symt.o:		bio.h head.h defs.h symt.c
display.o:	bio.h head.h defs.h cdefs.h display.c

.c.o:;	cc -c -g $*.c

a.out:	main.o re.o fio.o bio.o decode.o docomm.o sub.o head.h \
	access.o pcs.o runpcs.o setup.o message.o udef.o \
	symt.o display.o xeq.o
	cc -g -o sdb *.o
	echo DONE

install :
	install -s sdb $(DESTDIR)/usr/bin

clean :
	rm *.o  sdb

print:
	pr *.h *.c
