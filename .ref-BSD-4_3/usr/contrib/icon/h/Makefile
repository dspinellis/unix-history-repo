what:
		@echo "what do you want to make?"

Listall:
		@pr *.[hs]
		@date >List

List:		config.h ctype.h defs.s err.h gc.h keyword.h pdef.h pnames.h \
		   record.h rt.h
		@pr $?
		@date >List

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.h *.s *.gen `
