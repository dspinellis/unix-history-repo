# 
# $Header: Makefile 1.10 83/07/06 02:24:04 layer Exp $
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
	h/duallispo.h	h/dualaout.h

AllSrc = Makefile ${Includes}

clean:
	rm -f *.o

#--- copysource : copy source files to another directory
#  called via   make CopyTo=/xx/yyy/zz copysource
# 
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@(cd .. ; scriptcat franz franz ${AllSrc})
