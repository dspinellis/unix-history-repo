# Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984.

DOCS	      = bproject \
		bugreport \
		description \
		quickref \
		usersguide

PRINT	      = vtroff

all:		print

clean:;

install:;

print: 		$(DOCS)
		@touch print

bproject:	Bproject
		print Bproject
bugreport:
		print BugReport
description:	Descr.macs Description1 Description2 bfont.def
		$(PRINT) Descr.macs Description1 Description2
quickref:	QuickRef bfont.def
		$(PRINT) QuickRef
usersguide:	UsersGuide.ms bfont.def
		$(PRINT) -ms UsersGuide.ms
