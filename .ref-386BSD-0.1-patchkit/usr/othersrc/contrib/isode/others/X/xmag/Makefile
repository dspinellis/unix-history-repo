
X = /cs/sys/src/package/X11/R3/clients/xmag
I= /cs/research/dsa/hubris/incads/sun4.0/isode-5.8uk

OBJSXI = ../client/XlibInt.o ../client/XConnDis.o

ISODELIBS = ../tsap/tsapmisc.o $I/libtsap.a $I/libisode.a $I/libcompat.a

L = /usr/ucl/lib/libXaw.a /usr/ucl/lib/libXt.a  \
	/usr/ucl/lib/libXmu.a /usr/ucl/lib/libX11.a \
	$(ISODELIBS) \
	-ltermlib -lm

CFLAGS = -DISOCONN

OBJS1 = $X/xmag.o

xmag: $(OBJS1) 
	$(CC) -DISOCONN -o $T$@ $(OBJS1) $(OBJSXI) $L

