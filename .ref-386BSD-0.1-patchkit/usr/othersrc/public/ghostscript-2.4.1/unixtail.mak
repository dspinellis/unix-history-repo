#    Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Partial makefile for Ghostscript, common to all Unix configurations.

# This is the last part of the makefile for Unix configurations.
# Since Unix make doesn't have an 'include' facility, we concatenate
# the various parts of the makefile together by brute force (in tar_cat).

# The following prevents GNU make from constructing argument lists that
# include all environment variables, which can easily be longer than
# brain-damaged system V allows.

.NOEXPORT:

# -------------------------------- Library -------------------------------- #

## The Unix platform

unix__=gp_unix.$(OBJ)
unix_.dev: $(unix__)
	$(SHP)gssetmod unix_ $(unix__)

gp_unix.$(OBJ): gp_unix.c $(memory__h) $(string__h) $(gx_h) $(gp_h) \
 $(stat__h) $(time__h)

# -------------------------- Auxiliary programs --------------------------- #

ansi2knr$(XE):
	$(CC) -o ansi2knr$(XE) $(CFLAGS) ansi2knr.c

# On the RS/6000 (at least), compiling genarch.c with gcc with -O
# produces a buggy executable.
genarch$(XE): genarch.c
	$(CC) -o genarch$(XE) genarch.c

# ----------------------------- Main program ------------------------------ #

# Main program

ALLUNIX=gsmain.$(OBJ) $(LIB)

# Interpreter main program

GSUNIX=gs.$(OBJ) $(INT) $(ALLUNIX)

gs: $(GSUNIX) obj.tr lib.tr
	$(SHP)echoq $(CC) $(LDFLAGS) $(XLIBDIRS) -o gs $(GSUNIX) >_temp_
	cat obj.tr >>_temp_
	cat lib.tr >>_temp_
	echo $(EXTRALIBS) -lm >>_temp_
	$(SH) <_temp_

# Installation

TAGS:
	etags -t *.c *.h

install: gs
	-mkdir $(bindir)
	$(INSTALL_PROGRAM) gs $(bindir)
	$(INSTALL_PROGRAM) gsnd $(bindir)
	$(INSTALL_PROGRAM) bdftops $(bindir)
	$(INSTALL_PROGRAM) font2c $(bindir)
	$(INSTALL_PROGRAM) pfbtogs $(bindir)
	-mkdir $(libdir)
	$(INSTALL_DATA) gs_init.ps $(libdir)
	$(INSTALL_DATA) gs_2asc.ps $(libdir)
	$(INSTALL_DATA) gs_dps1.ps $(libdir)
	$(INSTALL_DATA) gs_fonts.ps $(libdir)
	$(INSTALL_DATA) gs_lev2.ps $(libdir)
	$(INSTALL_DATA) gs_statd.ps $(libdir)
	$(INSTALL_DATA) sym__enc.ps $(libdir)
	$(INSTALL_DATA) quit.ps $(libdir)
	$(INSTALL_DATA) Fontmap $(libdir)
	$(INSTALL_DATA) uglyr.gsf $(libdir)
	$(INSTALL_DATA) chess.ps $(libdir)
	$(INSTALL_DATA) cheq.ps $(libdir)
	$(INSTALL_DATA) golfer.ps $(libdir)
	$(INSTALL_DATA) escher.ps $(libdir)
	$(INSTALL_DATA) decrypt.ps $(libdir)
	$(INSTALL_DATA) bdftops.ps $(libdir)
	$(INSTALL_DATA) font2c.ps $(libdir)
	$(INSTALL_DATA) pfbtogs.ps $(libdir)
	$(INSTALL_DATA) pstoppm.ps $(libdir)
	$(INSTALL_DATA) prfont.ps $(libdir)
	$(INSTALL_DATA) showpbm.ps $(libdir)
