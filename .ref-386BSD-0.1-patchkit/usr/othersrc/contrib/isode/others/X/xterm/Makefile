
X = /cs/sys/src/package/X11/R3/clients/xterm
I = /cs/research/dsa/hubris/incads/sun4.0/isode-5.8uk

OBJS1 = $X/main.o $X/input.o $X/charproc.o $X/cursor.o $X/util.o $X/tabs.o \
		  $X/screen.o $X/scrollbar.o $X/button.o $X/Tekproc.o \
		  $X/misc.o \
		  $X/VTPrsTbl.o $X/TekPrsTbl.o $X/data.o $X/menu.o

OBJSXI = ../client/XlibInt.o ../client/XConnDis.o

ISODELIBS = ../tsap/tsapmisc.o $I/libtsap.a $I/libisode.a $I/libcompat.a

L = /usr/ucl/lib/libXaw.a /usr/ucl/lib/libXt.a  \
	/usr/ucl/lib/libXmu.a /usr/ucl/lib/libX11.a \
	$(ISODELIBS) \
	-ltermlib -lm

xterm: $(OBJS1) 
	$(CC) -o $T$@ $(OBJS1) $(OBJSXI) $L


