# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1 (Berkeley) %G%
#
# Makefile to run off revised 4.3 ipc primer
#

TROFF= ditroff
PKG= -ms
TBL= dtbl
TBLARGS =
PRINTER = -Pdp

ipc: 0.t 1.t 2.t 3.t 4.t 5.t
	$(TBL) $(TBLARGS) *.t | $(TROFF) $(PKG) $(PRINTER)

clean:
	rm *.imP *.bak *.CKP

