	This directory contains the VMS specific files for libg++.  To 
install, you will need to have both gcc and gxx (C++) already running, and 
you must have write access to the directories that GNU_GXX_INCLUDE:[000000], 
GNU_CC:[000000] and GNU_CC_INCLUDE:[000000] point to.  If you have not figured
it out already, you must run the command file "make-l2.com" in the gcc 2.0
distribution (which creates the library gcclib2.olb), and then you should
extract all of the object modules from this library and add them to
GNU_CC:[000000]GCCLIB.OLB. If you have write access to sys$library,  then the
compiled libgxx_shr.exe will be copied there, and if you have  CMKRNL
privilege, the installation procedure will run INSTALL to make it a  known
image.  If you do not have the correct privileges or write access to 
sys$library, the compiled libgxx_shr.exe will be left in the directory  where
it was built, and the installation procedure will continue.
  The installation procedure will automatically copy all of the header
files from the directory where you unpacked the library to the appropriate
header directory (GNU_GXX_INCLUDE).  To start the installation, simply
type: 

	@VMS-INSTALL-LIBGXX

and go get a cup of coffee.  (This can be submitted to a batch queue if you 
wish).

	For one reason or another, the installation procedure may bomb on 
your system when it is running.  For this reason, I will describe the 
various steps that it takes in order to help you figure out what went wrong.

	1) A Unix filesystem is case sensitive with respect to filenames
	while VMS is not case-sensitive.  Thus String.h and string.h appear
	to be two different files under Unix, and would appear the same to 
	VMS.  Unfortunately, there are a couple of cases where two files
	differ only in the case of their names, and when you unpack libg++
	on a VMS system, you are left with two seperate versions of what
	would appear to be the same file.

	The installation procedure knows about these cases and attempts to
	rename the offending files by renaming String.h to sstring.h for 
	example.  Next the installation procedure edits any files in the
	distribution that contain references to these header files, and
	changes the include directives to refer to the new names.

	This procedure may fail if a new file is added to the distribution
	that has a name that needs to be changed, or if a current or new 
	file includes one of the existing files that was renamed.  In this
	case, you can simply hack the file VMS-INSTALL-LIBGXX.COM to handle
	these new cases.

	* Next the install procedure moves all of the header files to 
	GNU_GXX_INCLUDE.  This step is fairly robust, and the only problem
	that could occur is if you run out of disk space/quota.  If there
	are already existing files present in the directory, the install
	procedure copies the new set over the old ones, and you must purge
	the old ones by hand.


	* Finally, the install procedure will compile the source modules and
	build the library.  In addition, it will automatically move the
	support files into the required directories.  Building the library
	should take several hours.  After all of the compilation is complete
	it will	build a sharable image library, will be moved to sys$library,
	if the account you are running this from has write access to
	sys$library.  It will also move the other files that are required by
	libg++ into the GNU_CC:[000000] directory.

After this, you should define the following commands in SYLOGIN.COM:

$gxx:==gcc/plus
$genclass:==@gnu_cc:[000000]genclass
$cxlink:==@gnu_cc:[000000]cxlink	!only if using non-shared libraries.
$cxlink:==@gnu_cc:[000000]cxshare	!only if using shared libraries

Finally, make sure that GCC_INSTALL.COM is in the system startup file, 
if you have already installed C++, then you have probably already done this.

	Once you have built the library itself, you may wish to run the test
suite that is included with libgxx.  To do this you simply need to execute
the command procedure VMS-TEST-LIBGXX.COM, which will compile and link all of the
source modules that are required to run the test.  Once again, you should
expect this to take several hours. It will create a file RUN.COM, which can be
submitted to a batch queue, and you can compare the output to the file
EXPECTED.VMS

      There is one bit of oddness which is that programs running under
batch have spurious line breaks in the output if you use
cout << (anything).  This is due to the
fact that the implementation of write in the VAX-C run time library inserts a
line break whenever a 512 byte buffer fills up.  If you are running
interactively then the output will appear normally.  If you use the $assign
command to redirect sys$output to a file, or if you use sethost/log to
try to capture the output, you will get the spurious line breaks.
At some point in the future, I may add some kind of primative I/O
routines that will only be used internally by libg++, which should work a
lot better than the brain-dead version in the VAX-C run time library.

The files included in this directory are:

AAAREADME.TXT	This file.
CXLINK.COM	Command file to link to the non-shared library.
CXSHARE.COM	Command file to link to the shared version of the library.
EXPECTED.LIST	List of the executables to be run to generate EXPECTED.VMS
		Used by VMS-TEST-LIBGXX.COM to generate RUN.COM
EXPECTED.VMS	Expected output from the test suite.  Execute command file
		VMS-TEST-LIBGXX, and then the file RUN.COM that is generated, and
		compare the output to this file.
FNDVEC.C	Source file to a program that will automatically generate
		LIBGXX_VECTOR.C, the list of all of the transfer vectors.
GENCLASS.COM	Command file to generate container classes.
GENCLASS.TPU	TPU code called by GENCLASS.COM
GXX_MAIN_SHR.MAR
		This does not contain any routines, but it does contain a
		couple of psect definitions that are used by __MAIN to locate
		any global objects in the sharable libg++, and initialize them.
OPTIONS.OPT	Options file used when linking to the non-shared library.
VMS-BUILD-LIBGXX.COM
		Command file that actually does the dirty work of building the
		library.
VMS-CURSES.C	A couple of routines that are needed by the curses module
		in libg++.
VMS-GCCLIB.MAR	Source to the modified version of module GCCLIB in library
		GCCLIB.OLB
VMS-INSTALL-LIBGXX.COM
		This command file builds the actual library, and moves any
		assorted files required by libg++ to the correct directory.
		This is not idiot proof, so beware.  It will delete the obj's
		after it is finished, and leave behind the lib.
VMS-TEST-LIBGXX.COM	Command file to build the test suite and create RUN.COM
VMS_FIXINCLUDES.TPU
		This file is used to fix include directives that refer to files
		such as String.h (when there is also a string.h).

************************************************
A few comments are in order.

	1) I strongly recommend that you use the sharable library if at all
possible. (See note 4 below) This is really quite painless.  This is created by
default when you execute @LIBGXX_INSTALL.  If you want to get the optimum
performance, and you expect a lot of people to use libg++, you have the option
of installing LIBGXX_SHR.EXE. Obviously, there are no special privileges
required for the image.  

	2) If you get warning messages from the assembler about g++ making
references to __vt.* as a routine, ignore them.  There is a bug in the g++ 
compiler, but the assembler catches them and fixes it before anything bad 
happens.

	Also, the assembler, gas, has been patched to automatically do a
globaldef/globalref for all vtables.  These are special variables that are
used internally by g++, and the names always begin with "_vt.".  By doing
this, the object code generated from c++ programs will tend to work better
(although not perfectly) with the VMS librarian and the VMS linker.
See note 4 for more information.

	3) You should have the proper versions of both the compiler and
assembler in order to use this version of libg++.  In addition, there may be
times when there are patches to the compiler and/or the assembler which have
not made it into the distribution version.  In the past, executables have been
made available via anonymous FTP from mango.miami.edu, which have the proper
patches.

	4) Using the non-sharable version of the library is fairly risky.
The problem is that the VMS linker will only pull an object module out of
the library if it needs a procedure from that module.  If the module 
contains an initialized variable, or uses a global constructor to 
initialize a variable, the module might not get pulled in, and you could
get very puzzling results indeed.  I do want to point out that the g++ 
compiler does not use inline functions when compiling with /nooptimize,
and instead generates function calls.  Thus you may find that if you
compile with /nooptimize the program works (because the linker does need
a routine from a module that defines some data), and it fails when compiled
with /optimize (because the compiler has inlined the functions).

	As it stands with the 2.0 release, there are two modules in the
iostream portion of libg++ which do not have any entry points at all.  
Unfortunately, these two modules contain the initialized structures for the
variables cout, cin, and cerr.  If you really insist upon using the non-
sharable library, you will have to pull these out of the library, and
always include them explicitly to the linker.  The modules that you
need are STDSTRBUFS and STDSTREAMS.

	The sharable library does not have any of these sorts of problems
because all modules are automatically linked into the image.

	5) If you create your own libraries, you need to be *very* aware of
the problems outlined in note 4 above.  The suggested remedy is to make
use of the GLOBALREF and GLOBALDEF macros defined in gnu_hacks.h.  All you
need to do is place a GLOBALDEF in any source module that contains 
initialized data or a global object that will be constructed, and place
a GLOBALREF in the header file that you use to declare those objects/
variables as being external.  When you do this, the object modules will
have special symbol references and definitions which the linker will use
to pull the appropriate modules out of the library.

	It would be fairly easy to hack the file VMS-BUILD-LIBGXX.COM to
build a sharable image library for your own applications.  If you do this, 
and you wish to use your library with libg++, it might be easiest to
modify VMS-BUILD-LIBGXX.COM merge your library with libg++ before fndvec
is compiled and run.  The build procedure will automatically pick up any
all of your entry points and do the right thing.  If you want to build
a sharable library from your own library, you can hack VMS_BUILD_LIBGXX.COM
to do the job with a very minimal amount of work.

	6) As far as I can tell the curses module works, however I am no expert
on curses.  If someone finds something that does not seem to work quite right
let me know.  Keep in mind that you are interfacing to VAXCCURSES, and you are
stuck with all of the idiosyncrasies that it has.  Currently the tCurses
program does not run perfectly, since the prompts do not appear on the screen.

	7) In chapter 6 of the libg++ manual there is a description of what
are called the "container class prototypes".  These are classes that can be
made for any type of data object.  The way that this is done now, is to
use a simple text substitution to fill in data types where required.  The
prototype files are in the GNU_GXX_INCLUDE:[GEN] directory, and they
have extensions .HP and .CCP.  The GENCLASS command can be used to generate
the classes, where GENCLASS is defined as:

$genclass:==@gnu_cc:[000000]genclass

The command syntax is identical to that given in the libg++ manual (except that
the data type should be quoted).  If there is one data type, then the syntax is:

$genclass "var" mode class

where var is the data type (quoted to preserve case). mode is either REF or
VAL, and class is the name of the container class.  For a class with two data
types, the syntax is 

$genclass -2 "var1" mode1 "var2" mode2 class

GENCLASS puts the output files in the current default directory.

	You should be aware that the g++ compiler does not use inline 
functions when the /NOOPTIMIZE switch is used.  The principal advatage of 
this is that the compiler runs much faster, making it easier to debug
and develop your programs.


	Any programs that were linked to the old sharable libg++ will not work
with the new libg++, since so many things have changed.  You should 
recompile and relink any such programs.  Also, any programs linked to the 
libg++ 2.0 sharable library will need to be relinked.

	The installation procedure is more robust, and more automatic,
which should in principle mean that there will be less maintainance required
for the VMS  port.  The general idea is that for the time being, you
will always have to relink any program linked to the sharable image library
each time you upgrade libg++.  The reason for this is that it is a lot of 
work trying to keep track of which entry points were used for what and so 
forth  (also that the library has been rapidly changing as bugs are fixed
and new methods added).  The fndvec.c program can keep track of some of
this, but if modules or objects change names, then this can get thrown off.

	You *may* wish to keep the old sharable libgxx libraries, 
especially if you have programs linked to the old library.

								6/2/92
						Eric Youngdale
						youngdale@v6550c.nrl.navy.mil


***************************************************************************
