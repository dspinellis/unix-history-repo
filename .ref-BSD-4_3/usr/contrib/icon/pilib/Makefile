all:
		rm -f *.c *.h rtlib linklib
		cp ../h/*.h .
		cp ../link/ilink.h .
		cp ../link/builtin.c .
		cp ../iconx/init.c .
		cp ../rt/strprc.c .
		cp ../bin/icont.c .
		Pilib

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile Pilib`
