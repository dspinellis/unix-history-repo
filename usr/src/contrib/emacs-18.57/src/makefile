#Build for 4.4BSD

all:
# Currently support 'hp300', 'sparc'
	rm -f config.h
	ln -s config.${MACHINE}bsd.h config.h
	make -f Makefile

clean:
	make -f Makefile clean

install:
	make -f Makefile install
