$! this command procedure is called from libgxx_install.com
$!
$! this command procedure is used to build the LIBGXX library for VMS
$! it is assumed that the sources are in a subdirectory [-.SRC], and that
$! the support modules are in this directory [.VMS]
$!
$!
$! first do some consistency checking to make sure things are OK.
$say:== write sys$output
$filenm=f$search("[-.src]ffilebuf.cc")
$if filenm.nes."" then goto start
$    say "Before you build the library, you must rename the file that"
$    say "contains the class filebuf to ffilebuf.cc.  Since vms is case insensitive"
$    say "both Filebuf.cc and filebuf.cc appear as two versions of the same file."
$    exit 2	!return error condition
$start:
$lib [-]libgxx/create
$!
$! now compile the sources
$!
$set def [-.src]
$gcc/plus/debug ACG.CC
$gcc/plus/debug ALLOCRING.CC
$gcc/plus/debug BINOMIAL.CC
$gcc/plus/debug BITSET.CC
$gcc/plus/debug BITSTRING.CC
$gcc/plus/debug CHR.CC
$gcc/plus/debug COMPLEX.CC
$gcc/plus/debug CURSES.CC
$gcc/plus/debug DELETE.CC
$gcc/plus/debug DISCUNIF.CC
$gcc/plus/debug DTOA.CC
$gcc/plus/debug EH.CC
$gcc/plus/debug ERLANG.CC
$gcc/plus/debug ERROR.CC
$gcc/plus/debug FILE.CC
$gcc/plus/debug FFILEBUF.CC
$gcc/plus/debug FILEBUF.CC
$gcc/plus/debug FIX.CC
$gcc/plus/debug FIX16.CC
$gcc/plus/debug FIX24.CC
$gcc/plus/debug FORM.CC
$gcc/plus/debug GCD.CC
$gcc/plus/debug GEOMETRIC.CC
$gcc/plus/debug GETOPT.CC
$gcc/plus/debug HASH.CC
$gcc/plus/debug HYPERGEO.CC
$gcc/plus/debug INTEGER.CC
$gcc/plus/debug ISTREAM.CC
$gcc/plus/debug ITOA.CC
$gcc/plus/debug LG.CC
$gcc/plus/debug MLCG.CC
$gcc/plus/debug NEGEXP.CC
$gcc/plus/debug NEW.CC
$gcc/plus/debug NORMAL.CC
$gcc/plus/debug OBSTACK.CC
$gcc/plus/debug OSTREAM.CC
$gcc/plus/debug PLOTFILE.CC
$gcc/plus/debug POISSON.CC
$gcc/plus/debug POW.CC
$gcc/plus/debug RATIONAL.CC
$gcc/plus/debug REGEX.CC
$gcc/plus/debug RNG.CC
$gcc/plus/debug SAMPLE.CC
$gcc/plus/debug SFILE.CC
$gcc/plus/debug SQRT.CC
$gcc/plus/debug STR.CC
$gcc/plus/debug STREAMBUF.CC
$gcc/plus/debug STRING.CC
$gcc/plus/debug TIMER.CC
$gcc/plus/debug UNIFORM.CC
$gcc/plus/debug WEIBELL.CC
$gcc/plus/debug XYZZY.CC
$lib [-]libgxx *.obj
$delete *.obj;*
$set def [-]
$!
$! now include the other miscelaneous modules
$!
$set def [.VMS]
$!
$gcc/debug gnulib
$lib [-]libgxx gnulib
$delete/nolog gnulib.obj;
$!
$gcc/plus/debug gstart.cc
$lib [-]libgxx gstart
$! The obj will be deleted later.
$!
$gcc/plus/debug gshrstart.cc
$lib [-]libgxx gshrstart
$del/nolog gshrstart.obj
$!
$macro/nolist gxx_main_shr
$lib [-]libgxx gxx_main_shr
$del/nolog gxx_main_shr.obj;
$!
$macro/nolist gxx-startup-1
$lib [-]libgxx gxx-startup-1
$del/nolog gxx-startup-1.obj;
$!
$lib gnu_cc:[000000]gcclib -
   /extract=(ALLOCA,BCMP,BCOPY,BZERO,INDEX,RINDEX)/output=t.obj
$lib [-]libgxx t.obj
$delete/nolog t.obj;
$!
$macro/nolist gcclib_vms
$lib [-]libgxx gcclib_vms
$del/nolog gcclib_vms.obj;
$!
$! now create the __main entry point used with the sharable lib.
$ macro/nolist gxx_main
$copy gxx_main.obj+gstart.obj libgxx_shr_main.obj
$del gxx_main.obj;
$del/nolog gstart.obj;
$! 
$write sys$output "Library has been rebuilt."
$!
$!
$! compile the transfer vectors for the sharable library
$macro/nolist libgxx_vector
$rename libgxx_vector.obj [-]
$set def [-]
$!
$	say:=write sys$output
$	say "Linking sharable image library..."
$
$         LINK/NOMAP/SHAREABLE=LIBGXX_SHR.EXE -
			[.vms]LIBGXX_SHR_BLD.OPT/OPTIONS
$	  delete/nolog libgxx_vector.mar;
$         say "Creating shareable image library..."
$         LIBRARY/CREATE/SHAREABLE LIBGXX_SHR.OLB LIBGXX_SHR.EXE
$ copy libgxx_shr.olb sys$library
$ copy libgxx_shr.exe sys$library
$
$ say "Finished building library.  LIBGXX_SHR.EXE and LIBGXX_SHR.OLB have been copied"
$ say "to SYS$LIBRARY.  You have the option of installing the executable to"
$ say "make it a known image."
$!
$! and return to the hole from whence we came.
$set def [.vms]
