CFLAGS =

progs = getenv.o iscope.o math.o seek.o trig.o ttyctl.o 

what:
		@echo "what do you want to make?"

All:		$(progs)
		ls $(progs)

Clean:
		rm -f *.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -rf `gcomp Makefile *.c`
