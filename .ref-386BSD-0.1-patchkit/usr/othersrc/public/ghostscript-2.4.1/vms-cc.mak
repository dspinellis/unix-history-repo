$ !    Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
$ !    Distributed by Free Software Foundation, Inc.
$ !
$ ! This file is part of Ghostscript.
$ !
$ ! Ghostscript is distributed in the hope that it will be useful, but
$ ! WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
$ ! to anyone for the consequences of using it or for whether it serves any
$ ! particular purpose or works at all, unless he says so in writing.  Refer
$ ! to the Ghostscript General Public License for full details.
$ !
$ ! Everyone is granted permission to copy, modify and redistribute
$ ! Ghostscript, but only under the conditions described in the Ghostscript
$ ! General Public License.  A copy of this license is supposed to have been
$ ! given to you along with Ghostscript so you can know your rights and
$ ! responsibilities.  It should be in a file named COPYING.  Among other
$ ! things, the copyright notice and this notice must be preserved on all
$ ! copies.
$ !
$ !
$ ! "makefile" for Ghostscript, VMS / VMS C / DECwindows (X11) configuration.
$ !
$ !
$ ! Execute this command file while in the GhostScript directory!
$ !
$ !
$ ! To build a debugging configuration, issue the command:
$ !
$ !          $ @VMS-CC.MAK DEBUG
$ !
$ ! Do not  abbreviate "DEBUG".  To specify an alternate directory for
$ ! GS_LIB_DEFAULT, issue the command:
$ !
$ !          $ @VMS-CC.MAK directory
$ !
$ ! with "directory" a fully qualified directory name.  "directory" cannot
$ ! be DEBUG (otherwise it will be confused with the DEBUG switch).  Both
$ ! the DEBUG switch and a directory name may be specified on the same
$ ! command line and in either order.
$ !
$ CDEF = ""
$ LDEF = ""
$ !
$ GS_INIT = "GS_INIT.PS"
$ GS_LIB_DEFAULT = F$ENVIRONMENT("DEFAULT")
$ IF P1 .NES. "" .AND. P1 .NES. "DEBUG" THEN GS_LIB_DEFAULT = P1
$ IF P2 .NES. "" .AND. P2 .NES. "DEBUG" THEN GS_LIB_DEFAULT = P2
$ !
$ IF P1 .NES. "DEBUG" .AND. P2 .NES. "DEBUG" THEN GOTO NODEBUG
$ CDEF = "/DEFINE=(""DEBUG"")/NOOPTIMIZE/DEBUG"
$ LDEF = "/DEBUG"
$ !
$ NODEBUG:
$ !
$ ! Give ourself a healthy search list for C include files
$ !
$ DEFINE C$INCLUDE 'F$ENVIRONMENT("DEFAULT"),DECW$INCLUDE,SYS$LIBRARY
$ DEFINE VAXC$INCLUDE C$INCLUDE
$ DEFINE SYS SYS$LIBRARY
$ !
$ !
$ ! Build the ANSI C to Kernighan & Ritchie filter
$ !
$ CC/NOLIST/OBJECT=ANSI2KNR.OBJ ANSI2KNR.C
$ LINK/NOMAP/EXE=ANSI2KNR.EXE ANSI2KNR,SYS$INPUT/OPT
SYS$SHARE:DECW$XLIBSHR/SHARE
$ DELETE ANSI2KNR.OBJ.*
$ !
$ ! Now define an ansi2knr symbol
$ !
$ ANSI2KNR = "$" + F$ENVIRONMENT("DEFAULT") + "ANSI2KNR.EXE"
$ !
$ !
$ ! Compile genarch.c and then run it to create the arch.h header file
$ !
$ CC/NOLIST/OBJECT=GENARCH.OBJ GENARCH.C
$ LINK/NOMAP/EXE=GENARCH.EXE GENARCH,SYS$INPUT/OPT
SYS$SHARE:VAXCRTL/SHARE
$ GENARCH = "$" + F$ENVIRONMENT("DEFAULT") + "GENARCH.EXE"
$ GENARCH ARCH.H
$ DELETE GENARCH.EXE.*,GENARCH.OBJ.*
$ PURGE ARCH.H
$ !
$ !
$ ! Create a minimal gconfig.h
$ !
$ CREATE GCONFIG.H
device_(gs_x11_device)
$ !
$ ! Create an empty object library
$ !
$ LIBRARY/CREATE GS.OLB
$ !
$ !
$ ! Create a list of modules to compile and store in the object library
$ !
$ CREATE MODULES.LIS
GDEVMEM1
GDEVMEM2
GDEVMEM3
GP_VMS
GSCHAR
GSCHAR0
GSCOLOR
GSCOORD
GSDEVICE
GSDPS1
GSFILE
GSFONT
GSIM2OUT
GSIMAGE
GSLINE
GSMATRIX
GSMISC
GSPAINT
GSPATH
GSPATH2
GSSTATE
GSTDEV
GSTYPE1
GSUTIL
GXCACHE
GXCLIST
GXCOLOR
GXCPATH
GXDITHER
GXDRAW
GXFILL
GXHT
GXPATH
GXPATH2
GXSTROKE
IALLOC
IBNUM
IBSCAN
ICCFONT
IDEBUG
IDICT
IINIT
INAME
INTERP
ISAVE
ISCAN
IUTIL
SFILTER
SFILTER2
STREAM
ZARITH
ZARRAY
ZBSEQ
ZCHAR
ZCOLOR
ZCONTROL
ZDEVICE
ZDICT
ZDPS1
ZFILE
ZFILEIO
ZFILTER
ZFILTER2
ZFONT
ZFONT0
ZFONT1
ZFONT2
ZGENERIC
ZGSTATE
ZHT
ZMATH
ZMATRIX
ZMISC
ZPACKED
ZPAINT
ZPATH
ZPATH2
ZPROPS
ZRELBIT
ZSTACK
ZSTRING
ZTYPE
ZUPATH
ZVMEM
$ !
$ !
$ ! NOW COMPILE AWAY!
$ !
$ OPEN/READ MODULE_LIST MODULES.LIS
$ !
$ COMPILE_LOOP:
$ READ/END=END_COMPILE MODULE_LIST MODULE
$ ANSI2KNR 'MODULE'.C 'MODULE'.CC
$ CC'CDEF/NOLIST/OBJECT='MODULE'.OBJ 'MODULE'.CC
$ LIBRARY/INSERT GS.OLB 'MODULE'.OBJ
$ DELETE 'MODULE'.OBJ.*
$ IF CDEF .EQS. "" THEN DELETE 'MODULE'.CC.*
$ GOTO COMPILE_LOOP
$ !
$ END_COMPILE:
$ CLOSE MODULE_LIST
$ DELETE MODULES.LIS.*
$ !
$ !
$ ! Is the DECwindows environment about?  Must be installed in order to
$ ! build the executable programs gs.exe, gt.exe, and xlib.exe.
$ !
$ IF F$SEARCH("SYS$SHARE:DECW$XLIBSHR.EXE") .NES. "" THEN GOTO CHECK2
$ WRITE SYS$OUTPUT "DECwindows user environment not installed;"
$ WRITE SYS$OUTPUT "unable to build executable programs."
$ GOTO DONE
$ !
$ CHECK2:
$ IF F$TRNLNM("DECW$INCLUDE") .NES. "" THEN GOTO BUILD_EXES
$ WRITE SYS$OUTPUT "You must invoke @DECW$STARTUP before using this"
$ WRITE SYS$OUTPUT "command procedure to build the executable programs."
$ GOTO DONE
$ !
$ ! Build the executables
$ !
$ BUILD_EXES:
$ !
$ DEFINE X11 DECW$INCLUDE
$ !
$ ANSI2KNR GCONFIG.C GCONFIG.CC
$ LIBDEF = """GS_LIB_DEFAULT=""""" + ''GS_LIB_DEFAULT' + """"""""
$ INIDEF = """GS_INIT=""""" + ''GS_INIT' + """"""""
$ CC'CDEF/NOLIST/DEFINE=('LIBDEF','INIDEF')/OBJECT=GCONFIG.OBJ GCONFIG.CC
$ !
$ ANSI2KNR GDEVXINI.C GDEVXINI.CC
$ CC'CDEF/NOLIST/OBJECT=GDEVXINI.OBJ GDEVXINI.CC
$ !
$ ANSI2KNR GDEVX.C GDEVX.CC
$ CC'CDEF/NOLIST/OBJECT=GDEVX.OBJ GDEVX.CC
$ !
$ ANSI2KNR GSMAIN.C GSMAIN.CC
$ CC'CDEF/NOLIST/OBJECT=GSMAIN.OBJ GSMAIN.CC
$ !
$ ANSI2KNR UTRACE.C UTRACE.CC
$ CC'CDEF/NOLIST/OBJECT=UTRACE.OBJ UTRACE.CC
$ !
$ ! 
$ ! GSMAIN and UTRACE are tossed into the GT link so that when a
$ ! debugging configuration is built, the gs_log_error and gs_exit
$ ! routines will be defined.
$ !
$ ANSI2KNR GS.C GS.CC
$ CC'CDEF/NOLIST/OBJECT=GS.OBJ GS.CC
$ LINK'LDEF/NOMAP/EXE=GS.EXE GS,GSMAIN,UTRACE,GDEVX,GDEVXINI,GCONFIG,-
  GS/LIB/INCLUDE=(GDEVMEM1,GDEVMEM2,GDEVMEM3,GXCLIST,-
  ZARITH,ZARRAY,ZBSEQ,ZCHAR,ZCOLOR,ZCONTROL,ZDEVICE,ZDPS1,-
  ZDICT,ZFILE,ZFILEIO,ZFILTER,ZFILTER2,ZFONT,ZFONT0,ZFONT1,ZFONT2,-
  ZGENERIC,ZGSTATE,ZHT,ZMATH,ZMATRIX,ZMISC,ZPACKED,ZPAINT,ZPATH,ZPATH2,-
  ZPROPS,ZRELBIT,ZSTACK,ZSTRING,ZTYPE,ZUPATH,ZVMEM),SYS$INPUT/OPT
SYS$SHARE:DECW$DWTLIBSHR/SHARE
$ !
$ DELETE GDEVX.OBJ.*,GDEVXINI.OBJ.*,GSMAIN.OBJ.*,UTRACE.OBJ.*,GS.OBJ.*,-
         GCONFIG.OBJ.*
$ IF CDEF .EQS. "" THEN -
  DELETE GDEVX.CC.*,GDEVXINI.CC.*,GSMAIN.CC.*,UTRACE.CC.*,GS.CC.*,GCONFIG.CC.*
$ !
$ DELETE ANSI2KNR.EXE.*
$ !
$ DEASSIGN X11
$ !
$ DONE:
$ !
$ DEASSIGN C$INCLUDE
$ DEASSIGN VAXC$INCLUDE
$ DEASSIGN SYS
$ !
$ ! ALL DONE
