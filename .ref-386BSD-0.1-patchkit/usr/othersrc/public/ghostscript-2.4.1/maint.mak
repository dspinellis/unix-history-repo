#    Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

# Auxiliary MS-DOS makefile for maintenance operations.

# This file pertains to Aladdin Enterprises maintenance operations,
# and is unlikely to be useful to users.

DRIVE=a:
LABEL=041992

# Targets:
#	xmit -- make the .BE files for Kermit transmission
#	xfer -- make a diskette for transferring files to the Sun
#	release -- make the .BE files for a release
#	disk720, disk1.2, disk1.44 -- make a MS-DOS diskette set
# Remember to erase *.zip / *.z / *.be, if needed, before running these.

# ---------------- Make files for Kermit transmission ----------------

TARS=tar1.be tar2.be tar3a.be tar3b.be tar3c.be tar4a.be tar4b.be tar5.be tar6.be tar7.be

xmit: $(TARS)

xfer: tar1.z tar2.z tar3a.z tar3b.z tar3c.z tar4a.z tar4b.z \
  tar5.z tar6.z tar7.z
	@echo ---------------- Insert diskette:
	@command /c pause
	xcopy tar_x a:
	xcopy tar*.z a:

release: $(TARS) tar0.be

# The dependency lists for the .be files should be much longer!

.z.be:
	bed $*.z $*.be
	erase $*.z

tar0.z: gs.exe gs386.exe
	tar -b1 -cf _temp_.t -uexe gs.exe gs386.exe
	compress -i _temp_.t
	if exist tar0.z erase tar0.z
	rename _temp_.txz tar0.z

tar1.z: \cp.bat \rm.bat bdftops.bat font2c.bat pfbtogs.bat
	copy \cp.bat
	copy \rm.bat
	tar -b1 -cf _temp_.t -ubat g*.asm i*.asm gs*.bat cp.bat bdftops.bat font2c.bat pfbtogs.bat showpbm.bat rm.bat
	compress -i _temp_.t
	rm tar1.z
	rename _temp_.txz tar1.z

tar2.z: ansi2knr.c checkall ega.c uniq.c
	tar -b1 -cf _temp_.t ansi2knr.c checkall ega.c uniq.c
	compress -i _temp_.t
	rm tar2.z
	rename _temp_.txz tar2.z

tar3a.z: gs.c
	tar -b1 -cf _temp_.t gs*.c
	compress -i _temp_.t
	rm tar3a.z
	rename _temp_.txz tar3a.z

tar3b.z: gxfill.c
	tar -b1 -cf _temp_.t gx*.c
	compress -i _temp_.t
	rm tar3b.z
	rename _temp_.txz tar3b.z

tar3c.z: gconfig.c genarch.c
	tar -b1 -cf _temp_.t gdev*.c gp_*.c gconfig.c genarch.c
	compress -i _temp_.t
	rm tar3c.z
	rename _temp_.txz tar3c.z

tar4a.z: interp.c stream.c turboc.cfg history.doc
	tar -b1 -cf _temp_.t i*.c s*.c turboc.cfg *.doc
	compress -i _temp_.t
	rm tar4a.z
	rename _temp_.txz tar4a.z

tar4b.z: zarith.c
	tar -b1 -cf _temp_.t z*.c
	compress -i _temp_.t
	rm tar4b.z
	rename _temp_.txz tar4b.z

tar5.z: gs.h gs.mak
	tar -b1 -cf _temp_.t *.h *.mak *.man *.sh *.tr tar_*.
	compress -i _temp_.t
	rm tar5.z
	rename _temp_.txz tar5.z

tar6.z: gs_init.ps uglyr.gsf
	tar -b1 -cf _temp_.t *.ps ugly*.*
	compress -i _temp_.t
	rm tar6.z
	rename _temp_.txz tar6.z

tar7.z: fontmap license readme bdftops font2c pfbtogs ccgs echoq
	tar -b1 -cf _temp_.t fontmap license readme bdftops font2c pfbtogs showpbm gs*. ccgs echoq
	compress -i _temp_.t
	rm tar7.z
	rename _temp_.txz tar7.z

# ---------------- Make MS-DOS diskette sets ----------------
# We have to juggle things carefully to make the files fit.

disk720:
	make disk1dd
	make disk2dd
	make disk3dd
	make disk4dd
	make disk5dd
	make disk6dd
	make disk7dd
	@echo ---------------- Done. ----------------

disk1.2 disk1.44:
	make disk1hd
	make disk2hd
	make disk3hd
	make disk4hd
	@echo ---------------- Done. ----------------

# ------ Here are the ZIP files that go onto the diskettes. ------
# We have to break them up so they will fit onto 720Kb media,
# and so that two of them will fit onto 1.2Mb media.

gsexe.zip: gs.exe gs386.exe
	if exist gsexe.zip erase gsexe.zip
	pkzip gsexe.zip gs.exe gs386.exe \watc\bin\dos4gw.exe

gsfiles.zip: bdftops.bat
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip gs*.bat bdftops.bat font2c.bat pfbtogs.bat showpbm.bat *.doc
	pkzip -a _temp_.zip *.ps fontmap license readme bdftops font2c pfbtogs showpbm
	pkzip -d _temp_.zip q* q*.* t.* comp1.*
	if exist gsfiles.zip erase gsfiles.zip
	rename _temp_.zip gsfiles.zip

gssrc1.zip: ansi2knr.c
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip \rm.bat \cp.bat
	pkzip -a _temp_.zip g*.asm i*.asm
	pkzip -a _temp_.zip ansi2knr.c ega.c uniq.c turboc.cfg
	pkzip -a _temp_.zip *.h *.mak *.man *.sh *.tr tar_*. ugly*.*
	pkzip -a _temp_.zip gs*. ccgs echoq
	pkzip -d _temp_.zip arch.h gconfig.h obj*.tr lib*.tr _temp*.*
	pkzip -d _temp_.zip ugly*.bdf libc*.tr q* q*.* t.* comp1.*
	if exist gssrc1.zip erase gssrc1.zip
	rename _temp_.zip gssrc1.zip

gssrc2.zip: gs.c
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip g*.c i*.c s*.c z*.c
	pkzip -d _temp_.zip zlzw*.c slzw*.c comp1.*
	if exist gssrc2.zip erase gssrc2.zip
	rename _temp_.zip gssrc2.zip

gsfonts1.zip: fonts\bchr.gsf fonts\pagk.gsf fonts\pbkd.gsf fonts\pcrro.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\b*.gsf fonts\pa*.gsf fonts\pb*.gsf fonts\pc*.gsf
	if exist gsfonts1.zip erase gsfonts1.zip
	rename _temp_.zip gsfonts1.zip

gsfonts2.zip: fonts\phvr.gsf fonts\pncr.gsf fonts\pplr.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\ph*.gsf fonts\pn*.gsf fonts\pp*.gsf
	if exist gsfonts2.zip erase gsfonts2.zip
	rename _temp_.zip gsfonts2.zip

gsfonts3.zip: fonts\psyr.gsf fonts\ptmr.gsf fonts\pzdr.gsf fonts\zcr.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip fonts\ps*.gsf fonts\pt*.gsf fonts\pz*.gsf fonts\z*.gsf
	if exist gsfonts3.zip erase gsfonts3.zip
	rename _temp_.zip gsfonts3.zip

gsfonts4.zip: fonts\cour.pfa hershey\hrsy_r.gsf
	if exist _temp_.zip erase _temp_.zip
	pkzip _temp_.zip hershey\*.gsf fonts\*.pfa
	if exist gsfonts4.zip erase gsfonts4.zip
	rename _temp_.zip gsfonts4.zip

# ------ Now the actual contents of the diskettes. ------

# Double density diskettes (720K)

disk1dd: readme.1st gsexe.zip
	@askdisk 1 $(DRIVE) $(LABEL)_1$7
	xcopy readme.1st $(DRIVE) /v
	xcopy \utils\pkunzip.exe $(DRIVE) /v
	xcopy gsexe.zip $(DRIVE) /v
	dir $(DRIVE)

disk2dd: gsfiles.zip gssrc1.zip
	@askdisk 2 $(DRIVE) $(LABEL)_2$7
	xcopy gsfiles.zip $(DRIVE) /v
	xcopy gssrc1.zip $(DRIVE) /v
	dir $(DRIVE)

disk3dd: gssrc2.zip
	@askdisk 3 $(DRIVE) $(LABEL)_3$7
	xcopy gssrc2.zip $(DRIVE) /v
	dir $(DRIVE)

disk4dd: gsfonts1.zip
	@askdisk 4 $(DRIVE) $(LABEL)_4$7
	xcopy gsfonts1.zip $(DRIVE) /v
	dir $(DRIVE)

disk5dd: gsfonts2.zip
	@askdisk 5 $(DRIVE) $(LABEL)_5$7
	xcopy gsfonts2.zip $(DRIVE) /v
	dir $(DRIVE)

disk6dd: gsfonts3.zip
	@askdisk 6 $(DRIVE) $(LABEL)_6$7
	xcopy gsfonts3.zip $(DRIVE) /v
	dir $(DRIVE)

disk7dd: gsfonts4.zip
	@askdisk 7 $(DRIVE) $(LABEL)_7$7
	xcopy gsfonts4.zip $(DRIVE) /v
	dir $(DRIVE)

# High density diskettes (1.2Mb or 1.44Mb)

disk1hd: readme.1st gsexe.zip gsfiles.zip
	@askdisk 1 $(DRIVE) $(LABEL)_1$4
	xcopy readme.1st $(DRIVE) /v
	xcopy \utils\pkunzip.exe $(DRIVE) /v
	xcopy gsexe.zip $(DRIVE) /v
	xcopy gsfiles.zip $(DRIVE) /v
	dir $(DRIVE)

disk2hd: gssrc1.zip gssrc2.zip
	@askdisk 2 $(DRIVE) $(LABEL)_2$4
	xcopy gssrc1.zip $(DRIVE) /v
	xcopy gssrc2.zip $(DRIVE) /v
	dir $(DRIVE)

disk3hd: gsfonts1.zip gsfonts2.zip
	@askdisk 3 $(DRIVE) $(LABEL)_3$4
	xcopy gsfonts1.zip $(DRIVE) /v
	xcopy gsfonts2.zip $(DRIVE) /v
	dir $(DRIVE)

disk4hd: gsfonts3.zip gsfonts4.zip
	@askdisk 4 $(DRIVE) $(LABEL)_4$4
	xcopy gsfonts3.zip $(DRIVE) /v
	xcopy gsfonts4.zip $(DRIVE) /v
	dir $(DRIVE)
