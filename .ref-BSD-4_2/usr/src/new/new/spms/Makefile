# Makefile to bootstrap SPMS

all:;		@cd bin; pmake

clean:;		@cd bin; pclean

depend:;	@cd bin; pdepend

install:;	cd bin; pinstall

update:;	@pexec -P~root\^usr\^spms -Tupdate make DESTDIR=$(DESTDIR) update
