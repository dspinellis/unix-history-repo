		   Notes on the MS-DOS Perl port

			Diomidis Spinellis
			 (dds@cc.ic.ac.uk)

[0. First copy the files in the msdos directory into the parent
directory--law]

1.  Compiling.

     Perl has been compiled under MS-DOS using the Microsoft
C  compiler  version 5.1.  Before compiling install dir.h as
<sys/dir.h>.  You will need a Unix-like make  program  (e.g.
pdmake) and something like yacc (e.g. bison).  You could get
away by running yacc and dry running make on  a  Unix  host,
but  I  haven't tried it.  Compilation takes 12 minutes on a
20MHz 386 machine (together with formating the  manual),  so
you  will probably need something to do in the meantime. The
executable is 272k and the top level directory needs 1M  for
sources  and  about the same ammount for the object code and
the executables.

     The makefile will compile glob for you which  you  will
need  to  place somewhere in your path so that perl globbing
will work correctly.  I have not tried all the tests or  the
examples,  nor the awk and sed to Perl translators.  You are
on your own with them.  In the eg directory I have  included
an  example  program  that uses ioctl to display the charac-
teristics of the storage devices of the system.

2.  Using MS-DOS Perl

     The MS-DOS version of perl has most of the  functional-
ity of the Unix version.  Functions that can not be provided
under  MS-DOS  like  sockets,  password  and  host  database
access,  fork  and wait have been ommited and will terminate
with a fatal error.  Care has been taken  to  implement  the
rest.   In particular directory access, redirection (includ-
ing pipes, but excluding the pipe function),  system,  ioctl
and sleep have been provided.

[Files currently can be edited in-place provided you are cre-
ating  a  backup.   However, if the backup coincidentally has 
the same name as the original, or  if  the  resulting  backup 
filename  is invalid, then the file will probably be trashed.
For example, don't do

	perl -i~ script makefile
	perl -i.bak script file.dat

because  (1)  MS-DOS treats "makefile~" and "makefile" as the
same filename, and (2) "file.dat.bak" is an invalid filename.
The  files  "makefile"  and  "file.dat" will probably be lost 
forever.  Moral of the story:   Don't  use  in-place  editing 
under MS-DOS. --rjc]

2.1.  Interface to the MS-DOS ioctl system call.

     The function code of the  ioctl  function  (the  second
argument) is encoded as follows:

- The lowest nibble of the function code goes to AL.
- The two middle nibbles go to CL.
- The high nibble goes to CH.

     The return code is -1 in the case of an  error  and  if
successful:

- for functions AL = 00, 09, 0a the value of the register DX
- for functions AL = 02 - 08, 0e the value of the register AX
- for functions AL = 01, 0b - 0f the number 0.

     See the perl manual for instruction on how  to  distin-
guish between the return value and the success of ioctl.

     Some ioctl functions need a number as the  first  argu-
ment.   Provided  that  no  other files have been opened the
number  can  be   obtained   if   ioctl   is   called   with
@fdnum[number]  as  the  first  argument after executing the
following code:

        @fdnum = ("STDIN", "STDOUT", "STDERR");
        $maxdrives = 15;
        for ($i = 3; $i < $maxdrives; $i++) {
                open("FD$i", "nul");
                @fdnum[$i - 1] = "FD$i";
        }

2.2.  Binary file access

     Files are opened in text mode by default.   This  means
that  CR LF pairs are translated to LF.  If binary access is
needed the `binary'  function  should  be  used.   There  is
currently  no  way to reverse the effect of the binary func-
tion.  If that is needed close and reopen the file.

2.3.  Interpreter startup.

     The effect of the Unix #!/bin/perl interpreter  startup
can  be  obtained  under  MS-DOS by giving the script a .bat
extension and using the following lines on its begining:

        @REM=("
        @perl %0.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
        @end ") if 0 ;

(Note that you will probably want an absolute path name in
front of %0.bat).

				March 1990

				Diomidis Spinellis <dds@cc.ic.ac.uk>
				Myrsinis 1
				GR-145 62 Kifissia
				Greece

--------------------------------------------------------------------------

	Revisions to the MS-DOS support in Perl 4.0
	Tom Dinger, 18 March 1991

The DOS compatibility added to Perl sometime in release 3.x was not
maintained, and Perl as distributed could not be built without changes.

Both myself and Len Reed more or less "rediscovered" how to get Perl built
and running reliably for MS-DOS, using the Microsoft C compiler.  He and I
have communicated, and will be putting together additional patches for the
DOS version of Perl.

1. Compiling Perl

    For now, I have not supplied a makefile, as there is no standard for
    make utilities under DOS.  All the files can be compiled with Microsoft
    C 5.1, using the switches "-AL -Ox" for Large memory model, maximum
    optimization (this turned out a few code generation bugs in MSC 5.1).
    The code will also compile with MSC 6.00A, with the optimization
    "-Oacegils /Gs" for all files (regcomp.c has special case code to change
    the aliasing optimizations).

    Generally, you follow the instructions given above to compile and build
    Perl 4.0 for DOS.  I used the output of SunOS yacc run on perly.y,
    without modification, but I expect both Bison and Berkeley-YACC will work
    also.  From inspection of the generated code, however, I believe AT&T
    derived YACC produces the smallest tables, i.e. uses the least memory.
    This is important for a 300K executable file.

2. Editing in-place.

    You will need the file suffix.c from the os2 subdirectory -- it will
    create a backup file with much less danger for DOS.

3. A "Smarter" chdir() function.

    I have added to the DOS version of Perl 4.0 a replacement chdir()
    function.  Unlike the "normal" behavior, it is aware of drive letters
    at the start of paths for DOS.  So for example:

    perl_chdir( "B:" )      changes to the default directory, on drive B:
    perl_chdir( "C:\FOO" )  changes to the specified directory, on drive C:
    perl_chdir( "\BAR" )    changes to the specified directory on the
                            current drive.

4. *.BAT Scripts as Perl scripts

    The strategy described above for turning a Perl script into a *.BAT
    script do not work.  I have been using the following lines at the
    beginning of a Perl *.BAT script:

        @REM=(qq!
        @perl -S %0.bat %1 %2 %3 %4 %5 %6 %7 %8 %9
        @goto end !) if 0 ;

    and the following at the end of the *.BAT script:

        @REM=(qq!
        :end !) if 0 ;

    If you like, with the proper editor you can replace the four '!'
    characters with some untypeable character, such as Ctrl-A.  This will
    allow you to pass any characters, including ".." strings as arguments.

4. Things to Come

     *  Better temporary file handling.
     *  A real Makefile -- Len Reed has one for Dmake 3.6
     *  Swapping code -- swaps most of Perl out of memory (to EMS, XMS or
	disk) before running a sub-program or pipe.
     *	MKS command line support, both into Perl, and to other programs
	spawned by Perl.
     *	Smarter pipe functions, not using COMMAND.COM.


					Tom Dinger
					tdinger@East.Sun.COM
					Martch 18, 1991
