$!
$! provide support for submitting this directly...
$!
$flnm = f$enviroment("PROCEDURE")	! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$! this command procedure is used to build the LIBGXX library for VMS
$! it is assumed that the sources are in a subdirectory [-.SRC], and that
$! the support modules are in this directory [.VMS]
$!
$! this is provided as a seperate file, since there are occasions that you
$! might wish to rebuild just the library, since the other support files have
$! already been built.
$!
$! first do some consistency checking to make sure things are OK.
$say:== write sys$output
$filenm=f$search("[-.src]regex.cc;-1")
$if filenm.eqs."" then goto start
$    say "Use the VMS-INSTALL-LIBGXX.COM file to set some things up before"
$    say "attempting to use this command file.  VMS_INSTALL-LIBGXX will also"
$    say "automatically call this command file to perform the build once"
$    say "things are set up."
$    exit 2	!return error condition
$start:
$!
$! Define a few symbols that we will use
$lib:=lib/nolog
$!
$lib [-]libgxx/create
$!
$! now compile the iostream
$!
$ call compile iostream-objs.list [-.iostream] c "strtoul"
$!
$! now compile the sources
$!
$ call compile src-objs.list [-.src] cc "new,delete,dtoa"
$!
$! We have a conflict with a file by the same name in iostream.
$ gcc/plus/debug [-.src]dtoa.cc/object=dtoaxx.obj
$ lib [-]libgxx dtoaxx.obj
$ delete/nolog dtoaxx.obj;
$!
$! now include the other miscelaneous modules
$!
$macro/nolist gxx_main_shr
$!
$lib gnu_cc:[000000]gcclib -
   /extract=(ALLOCA,BCMP,BCOPY,BZERO,INDEX,RINDEX,L_EPRINTF, -
	L_BUILTIN_NEW,L_CAPS_NEW,L_BUILTIN_DEL, -
	L_LSHRDI3,L_NEGDI2,L_UDIVDI3,L_UMODDI3,L_UDIVMODDI4)/output=t.obj
$lib [-]libgxx t.obj
$delete/nolog t.obj;
$!
$macro/nolist vms-gcclib
$lib [-]libgxx vms-gcclib
$delete/nolog vms-gcclib.obj;
$!
$gcc/debug vms-curses
$lib [-]libgxx vms-curses
$delete/nolog vms-curses.obj;
$!
$write sys$output "Library has been rebuilt."
$!
$!****************************************************************
$write sys$output "Now building sharable image library..."
$!
$! G++ 1.n used GSMAJOR=1
$! G++ 2.0 uses GSMAJOR=2
$!
$! These will be overridden by the values in the file [-]Makefile.in,
$! if it is possible for the following TPU procedure to find it.
$GSMAJOR := 2
$GSMINOR := 1
$!
$edit := edit
$edit/tpu/nosection/nodisplay/command=sys$input
! this is the start of the main procedure
   filename := GET_INFO (COMMAND_LINE, 'file_name') ;
   mainbuffer := CREATE_BUFFER ("Makefile.in", "[-]Makefile.in") ;
   newbuffer := CREATE_BUFFER("outfile");
   position (beginning_of (mainbuffer));
   range1 := search_quietly (LINE_BEGIN & "LIBG++_DIST_VERSION" & 
	((SPAN(" ") & "=") | "="), FORWARD, EXACT) ; 
   if range1 <> 0 then
	position (end_of (range1));
	move_horizontal(1);
	mark1 := MARK(NONE);
	range1 := search_quietly (LINE_END, FORWARD, EXACT) ; 
	position (beginning_of (range1));
	mark2 := MARK(NONE);
	range1 := CREATE_RANGE (mark1, mark2);
	position (beginning_of(newbuffer));
	copy_text (range1);
	position (beginning_of(newbuffer));
	LOOP
	  range1 := search_quietly (".", FORWARD, EXACT) ; 
	  exitif range1 = 0;
	  position (beginning_of (range1));
	  erase_character(1);
	  split_line;
	  ENDLOOP;
	SET(OUTPUT_FILE, newbuffer, "libgxx_version.txt");
        write_file (newbuffer);
	endif;
  quit;
$!
$OPEN IFILE$ LIBGXX_VERSION.TXT
$READ/END=DONE IFILE$ GSMAJOR
$READ/END=DONE IFILE$ GSMINOR
$DONE:CLOSE IFILE$
$if f$search("libgxx_version.txt").nes."" then delete/nolog libgxx_version.txt;
$!
$!
$! Since libgxx_vector.mar is not shipped, fndvec will bump gsmajor.
$gcc/debug fndvec.c/define=("GSMAJOR=''gsmajor'","GSMINOR=''gsminor'")
$link fndvec.obj+GNU_CC:[000000]gcclib/lib+sys$library:vaxcrtl/lib/nomap
$
$! compile the transfer vectors for the sharable library
$LIB/LIST=T.TMP/names [-]libgxx.OLB
$name=f$environment("PROCEDURE")
$fndvec=="$"+f$parse(name,,,"DEVICE")+f$parse(name,,,"DIRECTORY")+"FNDVEC"
$fndvec libgxx_vector.mar t.tmp
$del/nolog t.tmp;
$macro/nolist libgxx_vector
$delete *.obj;*/nolog/excl=(gxx_main_shr.obj,libgxx_vector.obj)
$! 			and clean up a little bit
$delete fndvec.exe;
$!
$! Now generate options file that we need to link the sharable image
$!
$! First generate a list of writable psects.  We do this by linking, which
$! will generate a link map, and then examining the map.
$!
$! Generate the options file without a list of writable psects.
$!
$call buildopt
$!
$! Some modules might not contain any entry points, so we force the linker
$! to use them all.
$!
$lib [-]libgxx/extract=*/output=libgxx.obj
$!
$! And link it.
$!
$!
$LINK/exe=nla0:LIBGXX.EXE/MAP=LIBGXX.MAP LIBGXX_SHR_BLD.OPT/OPTIONS
$search libgxx.map ",  WRT,",",  SHR,"/match=and/output=t.wrt
$osect=" "
$changeflag==0
$if f$search("libgxx.cmn").eqs."" then create libgxx.cmn
$open ofile$ libgxx.cmn/append
$OPEN IFILE$ t.wrt
$loop1:
$read/end=quit ifile$ line
$if f$length(line).eq.0 then goto loop1
$use_sect:
$ijk=f$locate(" ",line)
$sectnam=f$extract(0,ijk,line)
$! the same psect might have appeared on several pages, on the 2nd and
$! following pages the psect name and attributes appear again.
$if osect.eqs.sectnam then goto loop1
$osect=sectnam
$write ofile$ sectnam
$changeflag==1
$GOTO LOOP1
$quit:
$close ifile$
$close ofile$
$del libgxx.map;/nolog
$del t.wrt;/nolog
$WRITE SYS$OUTPUT "List of writable psects written to libgxx.cmn"
$!
$! Now build the actual options file we need to finish the job...
$!
$call buildopt
$!
$	say:=write sys$output
$	say "Linking sharable image library..."
$
$         LINK/NOMAP/SHAREABLE=[-]LIBGXX_SHR.EXE -
			LIBGXX_SHR_BLD.OPT/OPTIONS
$!
$! Now clean up what we do not need.
$!
$purge/nolog *.opt
$delete/nolog libgxx_vector.obj;
$delete/nolog gxx_main_shr.obj;
$delete/nolog libgxx.obj;
$!
$! now we are done building the library - now figure out if we have libgxx
$!already installed.
$installed = 0
$if f$search("gnu_cc:[000000]libgxx_shr.exe").eqs."" then goto cpy
$if f$file_attributes("gnu_cc:[000000]libgxx_shr.exe","KNOWN").eqs."TRUE" -
	then installed = 1
$cpy:
$! copy libgxx_shr.olb gnu_cc:[000000]
$on error then goto noclean
$ copy [-]libgxx_shr.exe gnu_cc:[000000]
$ lnm=f$trnlnm("GNU_CC")-".]"+"]libgxx_shr"
$ assign/system 'lnm' libgxx_shr
$ delete/nolog [-]libgxx_shr.exe;
$type/nopage sys$input
Finished building library.  LIBGXX_SHR.EXE has been copied to gnu_cc:[000000].
$if  installed.eq.0 then goto done_build
$if f$privilege("CMKRNL").eqs."FALSE" then goto noinst
$install:==$install
$if installed.eq.1 then install replace gnu_cc:[000000]libgxx_shr.exe
$type/nopage sys$input
A previous version was installed before, it has been replaced by the new one.
$ goto done_build
$noinst:
$type/nopage sys$input
A previous version of libg++ was installed before, but this account does not
have CMKRNL, so the image has not been replaced.
$goto done_build
$not_inst:
$type/nopage sys$input
You have the option of installing the executable to make it a known image.
$
$!
$noclean:
$type/nopage sys$input
Finished building library.  LIBGXX_SHR.EXE has NOT been copied to gnu_cc:[000000]
since you ignored the instructions about having write access to gnu_cc:[000000].
Not to worry, however, the file has been left in the parent directory of the
vms subdirectory, and you can copy the file by hand. Once you have done this,
you have the option of installing the executable to make it a known image.  
$exit
$done_build:
$on error then exit
$!
$! and clean up a little bit.
$!
$!
$ if f$search("sys$library:libgxx_shr.olb").nes."" then -
	delete/nolog sys$library:libgxx_shr.olb;
$exit
$!
$!
$! Subroutine to generate a linker options file.  It requires as input the
$! file libgxx.cmn, listing the writable psects in the image, and it needs
$! to take a peek at libgxx_vector.mar to find out the gsnumbers.
$! The output is in the files libgxx_shr_bld.opt and options_shr.opt.
$!
$! When building the library we need the LCL modifier, to tell the linker
$! that the psect is local.  When linking to the library we must not use it.
$buildopt:
$subroutine
$gsmajor = 1
$gsminor = 1
$open ofile$ libgxx_shr_bld.opt/WRITE
$open  user_link_opt$ options_shr.opt/WRITE
$write user_link_opt$ "LIBGXX_SHR/SHARE"
$write user_link_opt$ "SYS$SHARE:VAXCRTL/SHARE"
$write user_link_opt$ "gnu_cc:[000000]gcclib/lib
$write user_link_opt$ "psect_attr=curscr,noshr"
$write user_link_opt$ "psect_attr=stdscr,noshr"
$write user_link_opt$ "psect_attr=stdin,noshr"
$write user_link_opt$ "psect_attr=stdout,noshr"
$write user_link_opt$ "psect_attr=stderr,noshr"
$write user_link_opt$ "psect_attr=_ctype_,noshr"
$write ofile$ "!"
$write ofile$ "NAME = libgxx_SHR"
$!
$! first find the gsmatch parameters from the ident field of libgxx_vector.mar
$open ifile$ libgxx_vector.mar
$gsloop:
$read/end=nogs ifile$ line
$ijk=f$locate(".IDENT",line)
$if ijk.eq.f$length(line) then goto gsloop
$ijk=f$locate("/",line)+1
$line=f$extract(ijk,f$length(line)-ijk-1,line)
$ijk=f$locate("-",line)
$gsmajor='f$extract(0,ijk,line)
$gsminor='f$extract(ijk+1,f$length(line)-ijk-1,line)
$close ifile$
$nogs:
$write ofile$ "IDENTIFICATION = """,gsmajor,".",gsminor,"""
$write ofile$ "!
$write ofile$ "!
$write ofile$ "! Increase the second number by one for each new routine added
$!write ofile$ "GSMATCH = LEQUAL,",gsmajor,",",gsminor
$write ofile$ "GSMATCH = EQUAL,",gsmajor,",",gsminor
$write ofile$ "!
$write ofile$ "PSECT_ATTR = $PDATA,PIC,EXE
$write ofile$ "PSECT_ATTR = $MACRO,PIC,EXE
$write ofile$ "PSECT_ATTR = $TOKEN,PIC,EXE
$write ofile$ "!
$write ofile$ "! If new routines add any more writable psects, give them PSECT_ATTR's of LCL
$write ofile$ "! and NOSHR
$if f$search("libgxx.cmn").eqs."" then goto nofile1
$open ifile$ libgxx.cmn
$loop:read ifile$ line/end=done
$write ofile$ "PSECT_ATTR = ''line',LCL,NOSHR"
$write user_link_opt$ "PSECT_ATTR = ''line',NOSHR"
$goto loop
$done:close ifile$
$!
$! Now add the universal symbols
$!
$if f$search("libgxx_vector.mar").eqs."" then goto nouniversal
$search libgxx_vector.mar "UNIVERSAL"/OUTPUT=universal.lst
$open ufile$ universal.lst
$uloop:
$read/end=udone ufile$ line
$write ofile$ f$extract(1,255,line)
$goto uloop
$udone:close ufile$
$delete/nolog universal.lst;
$!
$nouniversal:
$write ofile$ "!"
$write ofile$ "! New writable psect names should be added to CLUSTER3"
$write ofile$ "COLLECT = CLUSTER1,$$$$libgxx_VECTOR"
$open ifile$ libgxx.cmn
$loop1:read ifile$ line/end=done1
$write ofile$ "COLLECT = CLUSTER1,"+LINE
$GOTO LOOP1
$DONE1:CLOSE IFILE$
$nofile1:
$write ofile$ "COLLECT = CLUSTER2,$LOCAL,$BLANK"
$write ofile$ "COLLECT = CLUSTER2,$CODE,$PDATA"
$write ofile$ "!"
$write ofile$ "gxx_main_shr"
$write ofile$ "libgxx_VECTOR.OBJ"
$write ofile$ "libgxx.OBJ"
$write ofile$ "SYS$LIBRARY:VAXCRTL/SHARE"
$write ofile$ "psect_attr=curscr,noshr"
$write ofile$ "psect_attr=stdscr,noshr"
$write ofile$ "psect_attr=stdin,noshr"
$write ofile$ "psect_attr=stdout,noshr"
$write ofile$ "psect_attr=stderr,noshr"
$write ofile$ "psect_attr=_ctype_,noshr"
$close user_link_opt$
$close ofile$
$exit
$endsubroutine
$!
$!
$!  This routine takes parameter p1 to be a linker options file with a list
$!  of object files that are needed.  It extracts the names, and compiles
$!  each source module, one by one.
$!
$!  Parameter P2 is a list of files which will appear in the options file
$!  that should not be compiled.  This allows us to handle special cases.
$!
$compile:
$subroutine
$on error then goto c_err
$on control_y then goto c_err
$open ifile$ 'p1'
$loop: read ifile$ line/end=c_done
$!
$i=0
$loop1:
$flnm=f$element(i,",",line)
$i=i+1
$if flnm.eqs."" then goto loop
$if flnm.eqs."," then goto loop
$if f$locate(flnm,"''p4'").nes.f$length("''p4'") then goto loop1
$!
$name = f$edit("''p2'''flnm'.''p3'","collapse")
$set verify
$ gcc/plus/debug 'name'
$ lib [-]libgxx 'flnm'.obj
$ delete/nolog 'flnm'.obj;
$!'f$verify(0)
$goto loop1
$!
$goto loop
$!
$! In case of error or abort, go here (In order to close file).
$!
$c_err: !'f$verify(0)
$close ifile$
$exit %x2c
$!
$c_done:
$close ifile$
$endsubroutine
