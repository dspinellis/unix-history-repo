
$! this command procedure calls make_libgxx to build libg++, and then installs
$! it on a VMS system.  It is assumed that
$! GNU-C and GNU-C++ have already been installed.  It is also assumed that the
$! sharable library will be used.  In order to run this you will need to have
$! write access to GNU_CC:[000000]
$!
$! this hack lets us submit this file directly to a queue, without having to 
$! specify the default directory - it assumes that the file is in the [.vms]
$! directory of the tree
$flnm = f$enviroment("PROCEDURE")	! get current procedure name
$set default 'f$parse(flnm,,,"DEVICE")''f$parse(flnm,,,"DIRECTORY")'
$!
$! do some simple tests first...
$ if f$trnlnm("GNU_CC").eqs."" then goto errmess1
$ if f$trnlnm("GNU_GXX_INCLUDE").eqs."" then goto errmess2
$on error then exit
$ create gnu_cc:[000000]write_test.tmp
$delete/nolog gnu_cc:[000000]write_test.tmp;
$!
$!
$!  First we use a crude  hack to rename header files in
$!  the case that there exist two different files in a case-sensitive
$!  world, which become one file in a case insensitive world.
$!
$! This will fix things so they look like:
$! #ifdef VMS
$! #include <rregex.h>
$! #else
$! #include <Regex.h>
$! #endif
$!
$call fix_file "[-.src]Regex.cc" "RRegex.cc"
$!
$call fix_file "[-.src]Regex.h" "RRegex.h"
$call fix_file "[-.src]String.h" "SString.h"
$call fix_file "[-.src]Complex.h" "CComplex.h"
$!
$fix_includes:=EDIT/TPU/NODISPLAY/NOJOURNAL/NOSECTION -
	/COMMAND=sys$disk:[]vms_fixincludes.tpu
$!
$fix_includes [-.src]RRegex.cc
$fix_includes [-.src]SString.h
$fix_includes [-.src]String.cc
$fix_includes [-.src]strclass.h
$fix_includes [-.src]complex.h
$fix_includes [-.src]Complex.cc
$fix_includes [-.tests]TComplex.cc
$fix_includes [-.tests]TString.cc
$fix_includes [-.tests]Test_H.cc
$
$!
$! We must rename these so that the g++ compiler implements the classes.
$!
$if f$search("[-.src]Complex.cc").nes."" then -
$	rename [-.src]Complex.cc [-.src]CComplex.cc
$if f$search("[-.src]String.cc").nes."" then -
$	rename [-.src]String.cc [-.src]SString.cc
$!
$!*************************************************************************
$! Next we install the header files in the directory GNU_CC:[GXX_INCLUDE]
$!*************************************************************************
$!
$! this is the easiest way of installing the header files.
$ copy _g_config.h gnu_gxx_include:[000000]
$! backup [-.g__-include...]*.*; gnu_gxx_include:[000000...]*.*;
$ backup [-.iostream]*.h; gnu_gxx_include:[000000]*.*;
$ backup [-.src]*.h; gnu_gxx_include:[000000]*.*;
$ backup [-.src.gen...]*.*; gnu_gxx_include:[gen...]*.*;
$!
$!  There are some header files that are called in libg++ and the test 
$! suite for which an empty file works.  Create these if needed.
$if f$search("gnu_cc_include:[000000]malloc.h").eqs."" then -
$	create gnu_cc_include:[000000]malloc.h
$if f$search("gnu_cc_include:[000000]fcntl.h").eqs."" then -
$	create gnu_cc_include:[000000]fcntl.h
$if f$search("gnu_cc_include:[000000]grp.h").eqs."" then -
$	create gnu_cc_include:[000000]grp.h
$if f$search("gnu_cc_include:[000000]pwd.h").eqs."" then -
$	create gnu_cc_include:[000000]pwd.h
$if f$search("gnu_cc_include:[sys]times.h").eqs."" then -
$	create gnu_cc_include:[sys]times.h
$if f$search("gnu_cc_include:[sys]signal.h").eqs."" then -
$	create gnu_cc_include:[sys]signal.h
$if f$search("gnu_cc_include:[sys]wait.h").eqs."" then -
$	create gnu_cc_include:[sys]wait.h
$if f$search("gnu_cc_include:[sys]resource.h").eqs."" then -
$	create gnu_cc_include:[sys]resource.h
$if f$search("gnu_cc_include:[sys]resourcetime.h").eqs."" then -
$	create gnu_cc_include:[sys]resourcetime.h
$if f$search("gnu_cc_include:[sys]socket.h").eqs."" then -
$	create gnu_cc_include:[sys]socket.h
$!
$get_makefile:=edit/tpu/nojournal/nosection/nodisplay/command=sys$input
$get_makefile
   PROCEDURE makefile_lookup (TAG_NAME, outfile)
        position (beginning_of (newbuffer));
        recursive_fetch_tag (TAG_NAME);
!
! Now fix up a few things in the output buffer
!
	pat_replace (".o ",",");
	pat_replace (".o","");  !appear at end of lines.
!
! Remove trailing commas, if present.
!
	position (beginning_of (newbuffer));
	LOOP
	  range1:=search_quietly("," & ((SPAN(" ") & LINE_END) | LINE_END), FORWARD, EXACT);
	  exitif range1 = 0;
	  position (beginning_of (range1));
	  erase(range1);
	  split_line;		
	  ENDLOOP;
! get rid of leading spaces on lines.
        position (beginning_of (current_buffer)) ;
	LOOP
          range1 := search_quietly ( LINE_BEGIN & " ", FORWARD, EXACT) ;
	  EXITIF range1 = 0;
	  position (end_of (range1));
	  erase_character(1);
	ENDLOOP;       
!
! Now write the output file.
!
	SET(OUTPUT_FILE, newbuffer, outfile);
      write_file (newbuffer);
      erase (newbuffer);
   ENDPROCEDURE;

!
! Looks up a tag, copies it to newbuffer, and then translates any $(...)
! definitions that appear.  The translation is put at the current point.
!
   PROCEDURE recursive_fetch_tag (TAG_N);
   fetch_tag (TAG_N);
!
! substitute any  makefile symbols $(...)
!
        position (beginning_of (current_buffer)) ;
	LOOP
          range1 := search_quietly ("$(" &  
	     SPAN("abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ#~0123456789") & ")", FORWARD, EXACT) ;
	  EXITIF range1 = 0;
	  position (beginning_of (range1));
	  move_horizontal(2);
	  mark_1 := MARK (NONE);
	  position (end_of (range1));
	  move_horizontal(-1);
	  mark_2 := MARK (NONE);
	  tag_range := CREATE_RANGE(MARK_1, MARK_2, NONE);
	  position (end_of (range1));
	  tag_string := STR (tag_range);
	  erase (range1);
	  fetch_tag (LINE_BEGIN & tag_string & ((SPAN(" ") & "=") | "="));
          position (beginning_of (current_buffer)) ;
	ENDLOOP;       
   ENDPROCEDURE;

!
! Looks up the translation of a tag, and inserts it at the current location
! in the buffer
!
   PROCEDURE fetch_tag (TAG_N);
      LOCAL mark1, mark2, mark3, range2;
      mark3 := MARK(NONE) ;
      position (beginning_of (mainbuffer)) ;
      range2 := search_quietly (TAG_N, FORWARD, EXACT) ;
      IF (range2 = 0) then 
	position (mark3);
	return;
	endif;
      position (end_of (range2)) ;
      MOVE_HORIZONTAL(1);
      mark1 := MARK(NONE) ;
      position (beginning_of (range2)) ;
      MOVE_VERTICAL(1);
      MOVE_HORIZONTAL(-2);
      LOOP
	EXITIF CURRENT_CHARACTER <> "\" ;
	ERASE_CHARACTER(1);
	MOVE_HORIZONTAL(1);
	MOVE_VERTICAL(1);
	MOVE_HORIZONTAL(-2);
	ENDLOOP;
      MOVE_HORIZONTAL(1);
      mark2 := MARK(NONE) ;
      range2 := CREATE_RANGE(mark1, mark2, NONE) ;
      position (mark3);
      if (length(range2) = 0) then return; endif;
      copy_text(range2);
   ENDPROCEDURE;

   PROCEDURE pat_replace (
      oldstring, !
      newstring)  !
      LOCAL range2;
      position (beginning_of (current_buffer)) ;
      LOOP
         range2 := search_quietly (oldstring, FORWARD, EXACT) ;
         EXITIF range2 = 0 ;
         position (beginning_of (range2)) ;
         erase (range2) ;
         copy_text (newstring) ;
         ENDLOOP ;
   ENDPROCEDURE ;

!
! this is the start of the main procedure.
!
! First get the iostream makefile, and find out which components we need to 
! install.
!
   filename := GET_INFO (COMMAND_LINE, 'file_name') ;
   mainbuffer := CREATE_BUFFER ("iostream.in", "[-.iostream]Makefile.in") ;
   newbuffer := CREATE_BUFFER("outfile");

   makefile_lookup(LINE_BEGIN & "IOSTREAM_OBS" & ((SPAN(" ") & "=") | 
		"="),"iostream-objs.list");
   erase(mainbuffer);
   delete(mainbuffer);
!
! Now do the same thing with the [-.src]Makefile.in
!
   mainbuffer := CREATE_BUFFER ("src.in", "[-.src]Makefile.in") ;
   position (beginning_of(mainbuffer));
!
! Ooooooo I wish VMS were case sensitive like Unix was.  This a a really 
! gross hack.
!
   pat_replace (" Regex.o"," RRegex.o");
   pat_replace (" Complex.o"," CComplex.o");
   pat_replace (" String.o"," SString.o");
   makefile_lookup(LINE_BEGIN & "OBJS" & ((SPAN(" ") & "=") | "="),
		"src-objs.list");
   quit ;
!
$! next build the actual library.  This will copy the result to gnu_cc:[000000]
$@vms-build-libgxx
$!
$! next install the files for container classes.
$copy genclass.* GNU_CC:[000000]*.*
$!
$! now install the options files required for linking.
$ copy options_shr.opt GNU_CC:[000000]*.*
$! copy options.opt GNU_CC:[000000]*.*   !only required to link non-shared lib
$!
$! now install the command files required to link a user program to the library
$!
$copy cxshare.com GNU_CC:[000000]*.*
$!copy cxlink.com GNU_CC:[000000]*.*  !used to link to non-shared library.
$!
$write sys$output "The installation is complete"
$exit 1
$!
$!
$errmess1:
$say:=write sys$output
$ say "You must install GNU-C and then GNU-C++ before you attempt to build"
$ say "libg++.  After you have installed GNU-C and GNU-C++ you can try this"
$ say "again."
$exit 0
$!
$errmess2:
$ say "You must install GNU-C++ before you attempt to build"
$ say "libg++.  After you have installed GNU-C++ you can try this again."
$exit 0
$!
$type sys$input
You must have write access to GNU_CC to do this installation.  If
you wish, you can create your own private directory, and use a logical
to point to it.  An example would be:
	ASSIGN DUA0:[FRED.GNU.]+F$TRNLNM("GNU_CC") GNU_CC
and any files created by this installation will be put in dua0:[fred.gnu]
$exit 0
$!
$exit
$!
$!
$!
$!*************************************************************************
$! What follows are a series of subroutines used to install libg++
$!*************************************************************************
$!
$!
$fix_file:
$subroutine
$!
$! First locate the file that needs to be renamed.
$!
$epos = f$length(p1) - 1
$fix_loop:
$if f$extract(epos,1,p1).eqs."." then goto fix_loop_done
$epos=epos-1
$if epos.ge.0 then goto fix_loop
$write sys$error "Ooops"
$exit
$!
$fix_loop_done:
$ name = p1
$ extension = ""
$ if epos.ne.f$length(p1) then name = f$extract(0,epos,p1)
$ if epos.ne.f$length(p1) then extension = f$extract(epos,255,p1)
$epos = f$locate("]",name)
$ if epos.ne.f$length(name) then name = f$extract(epos+1,255,name)
$!
$write sys$output name
$target = name+"_h"
$if extension.nes.".h" then target=name+".h"
$assign nla0: sys$output
$assign nla0: sys$error
$fix_loop:
$flnm=f$search("''p1';*")
$if flnm.eqs."" then exit
$search 'flnm' "''target'"/exact
$if $status.ne.1 then goto fix_loop
$!
$! Now rename the header file...
$!
$deassign sys$output
$deassign sys$error
$jpos = f$locate("]",flnm) + 1
$flnm1 = f$extract(0,jpos,flnm) + p2
$write sys$output "rename "+flnm+" "+flnm1
$rename 'flnm' 'flnm1'
$endsubroutine
