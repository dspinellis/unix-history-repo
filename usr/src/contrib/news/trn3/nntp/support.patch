If you apply this patch to NNTP's support directory and move the
acttimes.c file there also, it will compile the acttimes program
rather than the specious mkgrdates.

Wayne Davison                                        davison@borland.com
---8<------8<------8<------8<---cut here--->8------>8------>8------>8---
Index: Makefile
@@ -3,7 +3,7 @@
 #
 
-OBJS	=	mkgrdates.o
+OBJS	=	acttimes.o
 
-SRCS	=	mkgrdates.c
+SRCS	=	acttimes.c
 
 HFILES	=	../common/conf.h
@@ -15,18 +15,18 @@
 DESTDIR	= /usr/lib/news
 
-all: mkgrdates
+all: acttimes
 
-mkgrdates: mkgrdates.o
-	$(CC) ${CFLAGS} -o mkgrdates mkgrdates.o
+acttimes: acttimes.o
+	$(CC) ${CFLAGS} -o acttimes acttimes.o
 
-install: mkgrdates
-	cp mkgrdates ${DESTDIR}/mkgrdates
-	chmod 755 ${DESTDIR}/mkgrdates
+install: acttimes
+	cp acttimes ${DESTDIR}/acttimes
+	chmod 755 ${DESTDIR}/acttimes
 
 lint:
-	lint mkgrdates.c
+	lint acttimes.c
 
 clean:
-	-rm -f *.o mkgrdates
+	-rm -f *.o acttimes
 
 distrib: clean
---8<------8<------8<------8<---cut here--->8------>8------>8------>8---
