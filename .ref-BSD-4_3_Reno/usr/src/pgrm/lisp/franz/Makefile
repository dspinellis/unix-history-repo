# 
# $Header: Makefile,v 1.12 87/12/17 12:56:59 root Exp $
#
#  Franz Lisp C coded kernel, Machine independent part.
#
#  The directions for modifying this file are found in the machine
# dependent Makefiles (subdirectories vax/ or 68k/)


Includes = h/config.h 	h/global.h 	h/aout.h 	h/vaxframe.h \
	h/catchfram.h 	h/dfuncs.h 	h/gtabs.h 	h/sigtab.h \
	h/chars.h 	h/frame.h 	h/lfuncs.h 	h/structs.h \
	h/chkrtab.h 	h/gc.h 		h/lispo.h 	h/types.h \
	h/lconf.h	h/ltypes.h	h/68kframe.h	h/hpagsiz.h\
	h/duallispo.h	h/dualaout.h	h/tahoeframe.h

AllSrc = Makefile ${Includes}

clean:	FRC
	rm -f *.o

#--- copysource : copy source files to another directory
#  called via   make CopyTo=/xx/yyy/zz copysource
# 
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@(cd .. ; scriptcat franz franz ${AllSrc})

FRC:
