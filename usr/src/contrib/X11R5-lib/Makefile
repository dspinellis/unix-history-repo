# Minimal Makefile to just build libX11.a
# This Makefile should be replaced with the real X11R5 Makefile
# when the rest of X11R5 is installed.

all::
	(cd lib/X; make)

install::
	-mkdir -p /usr/X11R5/include
	cp -rp X11 /usr/X11R5/include
	-mkdir -p /usr/X11R5/lib
	cp -p lib/X/libX11.a /usr/X11R5/lib
	ranlib -t /usr/X11R5/lib/libX11.a

clean::
	(cd lib/X; make clean)
