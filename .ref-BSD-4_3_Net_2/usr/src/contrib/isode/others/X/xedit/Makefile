
X = /cs/sys/src/package/X11/R3/clients/xedit
I = /cs/research/dsa/hubris/incads/sun4.0/isode-5.8uk

OBJS1 = $X/*.o

OBJSXI = ../client/XlibInt.o ../client/XConnDis.o

ISODELIBS = $I/libtsap.a $I/libisode.a $I/libcompat.a

L = /usr/ucl/lib/libXaw.a /usr/ucl/lib/libXt.a  \
	/usr/ucl/lib/libXmu.a /usr/ucl/lib/libX11.a \
	$(ISODELIBS) \
	-ltermlib -lm

xedit: $(OBJS1) 
	$(CC) -o $T$@ $(OBJS1) $(OBJSXI) $L

