This is version 1.39.0 of libg++, the GNU C++ class library.
Release date Tue Feb 19 06:43:04 1991  Doug Lea  (dl at g.oswego.edu)

* Please skim through this once BEFORE attempting to make and install libg++.

* Contents

    * g++ source files are in the ./src directory
    * Some simple tests and demo programs are in ./tests
    * Header files are in ./g++-include
    * documentation is in ./libg++.texinfo. 
    * Some miscellaneous files of possible interest are in ./etc
      (These files are not officially a part of the libg++ distribution,
      and are subject to arbitrary changes, deletions, etc. from release
      to release.)

* Pre-installation

    * Install a version of g++ with at least as high a version
      number as this version of libg++. You also need gcc installed.

    * If there is a version of the GNU as (gas) assembler that
      works on your machine, get it, and install it as gcc-as,
      in whatever lib directory holds gcc-cc1plus. Otherwise,
      you may have to remake g++ with -DFASCIST_ASSEMBLER in CFLAGS,
      and disable the -PIPE_AS flag in the libg++ Makefile.

    * With only trivial modifications (like changing file extensions,
      etc.) everything (perhaps except for some demos in ./etc)
      should compile and run with any 2.0 C++ compiler. Please tell me
      if this is not so.

* Installation (see libg++.texinfo more more details)

    * For VMS, cd to ./vms, and read AAAREADME.TXT

    * Except for how to set the `PWD' make variable (which you must
      manually change if you do not use GNU make), the Makefiles
      are reported to be compatible with all flavors of `make'.

    * Check the declared indicated pathnames, etc. in the top-level Makefile

        Be sure to use USG-oriented switches if you run SystemV unix.

        If you run into problems, check through system-dependent
        #defines in  g++-include/stdio.h,  g++-include/math.h
        and g++-include/values.h, especially, and report any
        incompatibilities or problems to bug-lib-g++@prep.ai.mit.edu.

    * Choose functionality flags.

        In the libg++ Makefile, `XTRAFLAGS' are selectable. By default,
        none of these are enabled:

        -DEFAULT_filebuf, if included in XTRAFLAGS, causes standard
         cin, cout, and cerr streams to bypass your libc stdio facilities.
         I know of no system for which this is absolutely required. However,
         if there are known bugs with your stdio implementation, or if
         you wish to give up some functionality in order to be conservative,
         then select this. If you use this, expect an innocuous difference
         between the expected and obtained results of tFile in how the
         final status of standard streams is printed.

        -DNO_GNULIB3 suppresses compilation of a dummy `main',
         which calls global constructors and destructors around a
         user main. Select this if you have made alternative arrangements
         for doing this.

        -DCOFF_ENCAPSULATE is required for systems in which all
         executables use GNU COFF encapsulation.

        -DNO_LINE_BUFFER_STREAMBUF suppresses line-buffering (automatic
         flushing of output whenever a newline is encountered). Selecting
         this may result in slightly smaller code, but will require
         manual flushing when lines are expected to be printed right
         when they are written.

        -DUSE_LIBGXX_INLINES forces inlines to be compiled even if
         not compiling with optimization turned on. This is
         required when compiling src files only if -O does not work 
         for you.

        -DNO_LIBGXX_MALLOC suppresses compilation of libg++ malloc
         routines, thus causing all malloc, free, realloc, operator
         new() and operator delete() calls to go through your libc
         version of malloc. Select this if your system requires a
         the system version of malloc (possibly necessary on some
         shared memory multiprocessors, since libg++ malloc does not
         contain any concurrency control), or, perhaps, if your applications
         are specially tuned for or otherwise work better with your
         system malloc.

        -DMALLOC_STATS compiles statistics counting into libg++ malloc
         (but also slows it down a little). Select this if you are
         interested in examining the performance of libg++ malloc.
         (I welcome this, and would be grateful to receive statistics
         (printed via `malloc_stats()') for programs with heavy
         dynamic allocation requirements.)

    * Other compilation flags are tunable, but the default versions
        should normally suffice.

    * type `make all', or, step-by-step:

       `make src'          -- to compile libg++.a
       `make tests'        -- to make some tests/demos of libg++.
                              This will first compile tests, then run them,
                              then diff the results against expected
                              results. 
       `make etc'          -- (optional) to compile various other things 

       `make run_etc'      -- (optional) to run tests on some of the
                                things compiled in etc.
                            
       `make gperf'        -- (optional) to compile Doug Schmidt's
                               perfect hash function generator. 

    * Type `make install'  to install 

        libg++.a          (from src)
        include files     (from g++-include)
        prototype files   (from g++-include/gen)
        etags             (from etc)
        g++dep            (from etc)

        You may also want to install etc/c++-mode.el in your
        emacs/lisp directory, probably in byte-compiled form.

    * Install the documentation

    If you are a systems administrator installing libg++ for others,
    please make the documentation available to users!

    The libg++.texinfo file may be formatted as a paper document by

        * Get a copy of texinfo.tex. 
            This file defines various tex macros used in libg++.texinfo
            One is in the gcc release.
            You can temporarily copy it into the current directory.
        * Run tex on libg++.texinfo
             and do whatever you normally do from there to print it.

    It may be made into an emacs info file by

        * Inside emacs, run M-x texinfo-format-buffer on libg++.texinfo.
        * Save the resulting files:
            libg++
            libg++-1
            libg++-2
            ...

        * Copy these files into your emacs info directory
            (normally somewhere like /usr/gnu/emacs/info).
        * If you have not done so before, edit the emacs/info/dir file
            to add a libg++ node, by inserting a line like
		
            * Libg++: (libg++).	The GNU C++ Library

            to the bottom of the file.

    * (Optional) Install, from ./etc
        etags (version of etags that understands c++)
        g++dep (a version of mkdep that understands c++)
        c++-mode.el (a c++-mode for GNU emacs)

* General compilation notes

By default, everything is compiled with the g++/gcc -Wall flag
enabled. This causes g++ to produce numerous diagnostic warnings and
reminders. This is perfectly normal. Only true error messages, which
cause the `makes' to halt indicate real problems. If you do not like
to look at the warnings, disable the -Wall in the Makefile. -Wall is
enabled because, if there are true errors, warnings leading up to
them may prove helpful.

* Notes on compiling and running libg++/tests

tCurses is not automatically run through `checktests'.
You must run it manually:

tCurses attempts to place one of each curses library primitive (boxes,
strings, numbers...) on your screen, and asks for some input, to test
curses input routines. Since there is no way to describe the output
in a system- and terminal- independent way, you will have to be the 
judge of whether it works acceptably.

tCurses (and the curses-based classes generally) may fail on the
Sequent and perhaps other systems with unusual or old curses library
implementations if you do not end input with a <linefeed> instead of
the normal <carriage-return>.

It is a very good idea to also cd to the test directory and run tests
manually, to see what they do. 

Compiling and running the tests consumes a fair amount of time and
disk space!

Some reported diffs may be perfectly reasonable, owing to things like
floating point precision differences: The expected.out file was created
on a Sun4/110.

    Some tRational and tFix results depend on floating point precision
    and may generate slightly different output on different machines.

    tRandom seeds some random-numbers in a way that also relies on
    floating-point representations -- Your output should be numerically
    similar, but probably not identical.

* changes from libg++-1.37.0

    * All files use the new g++ #pragma interface / #pragma implementation
      convention, which minimies duplication of `outlined' inlines
      and vtables. This also causes no inlines to be used at
      all when not compiling with `-O', which speeds compilation
      and simplifies debugging.

    * Many .h header file names had to be shortened so as to simulaneously
      work with SYSV and with #pragma interface (since .h and .cc file
      base names must match.) Sorry!

    * All genclass-able files have been moved to g++-include/gen.

    * various and sundry bug fixes, minor enhancements, and/or portability 
        improvements as described in the ChangeLog. 

* Known bugs and problems

    * Support for the DecStation3100 and other MIPS-based machines is 
        still uncertain.

    * The file etc/HINTS is an emacs RMAIL file that contains recent
        bug-lib-g++ list mail and related messages that may be useful.

* Lots of other information is in the libg++.texinfo file. It really is
  very important to actually read the documentation before using 
  library classes. Examination of the demo files in the test directory
  may also be useful. (Note however, that the demo files are merely
  designed to test examples of each class capability,
  and are not especially good examples of client functions that might
  use these classes.)

* There is now a gnu libg++ mailing list (bug-lib-g++@prep.ai.mit.edu) and
    associated usenet gnu news group.

* You will be performing a valuable service if you use libg++
   classes and report back any comments, and suggestions, or bugs,
   preferably to the bug-lib-g++ list. Your feedback is extremely 
   helpful in efforts to make libg++ as useful and reliable as possible.

* See file `etc/release.log' for changes from previous versions


* I continue to solicit

  * bug reports.
  * suggestions.
  * comments.
  * questions about installing and using libg++
  * other contributions to be incorporated into libg++.
  * sample programs using libg++.

  Often, the best place to send such things is bug-lib-g++@prep.ai.mit.edu,
  although direct mail to me is also welcome.

* Good luck!

Doug Lea, Computer Science Dept., SUNY Oswego, Oswego, NY, 13126 (315)341-2367
email: dl@g.oswego.edu            or dl@cat.syr.edu
UUCP :...cornell!devvax!oswego!dl or ...rutgers!sunybcs!oswego!dl
