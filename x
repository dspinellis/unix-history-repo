





                Berkeley UNIX Software Tape




Extracting the data from this tape:__________ ___ ____ ____ ____ ____

Most of the data on this tape has been  archived  to  reduce
the  number  of  files so that tp will write this tape.  You
should extract the  data  from  the  libraries.   This  will
require  about  10000  blocks of storage if you don't remove
each "cont.a" after you extract it.

What is on this tape:____ __ __ ____ ____

This tape includes sources and binaries for a quantity of UC
Berkeley  software.   The  major  items on this tape are the
instructional Pascal system and the text editor "ex".  Other
software  here  includes  a modified shell, a new shell, new
commands, and a "star trek" game.  Machine readable documen-
tation  is  included  for  all programs.  The "Pascal User's
Manual" and the "Ex reference manual" need a phototypesetter
if readable copies are to be produced.

Pascal:______

The Pascal system has been used for a year  for  instruction
here.  It has undergone a number of improvements in the last
six months, but is quite stable.  We use it for  undergradu-
ate and graduate instruction.

The Pascal system requires separate I/D space; that  is,  an
11/45 or 11/70 host.  To run the Pascal stuff right away you
will also need floating point hardware -- it is possible  to
run  Pascal  without floating point hardware but it requires
adding a system call to replace the "mfpi" instruction  that
doesn't work on the 11/45's and 11/70's in user mode.

Ex:__

The Ex editor has been in use for about the same  length  of
time  as Pascal, and is used by a majority of our users.  It
has undergone a number  of  improvements  in  the  last  few
months.  We intend to use ex for the text editing classes at
the Computer Center here (for  a  general  campus  audience)
starting  in  January. The Pascal documentation uses "ex" in
its examples.

Installing the software:__________ ___ ________

Compiled  binaries  have  been  included  for  most  of  the
software  here.   (A few of the routines in the directory s6
include system dependent headers and so binaries would be of
no use and are not included.)









                           - 2 -


The major programs "pi", "pxp", "px", and "ex-1.1" have  the
binaries  in  the  directories  with  the same names.  "Pi",
"pxp" and "px" should run as they stand...  if  you  have  a
non-standard  teletype  driver  "ex" may require some system
dependent changes.  The binaries in "ex" will  run  directly
on standard or Berkeley-type version 6 UNIX systems.

Each major directory includes a  file  "READ_ME"  describing
the  software  in  the  directory.   There  is often a shell
script "setup" in the directory to  perform  one  time  only
operations.   The script "install" in these directories will
place the software in its standard home.

For recompilation of these programs you can use the  scripts
"make*",  and  "comp"  and  "load" in the directories.  Most
directories also have "print" scripts,  i.e.  "printpi",  to
make  a program listing with utility files and programs in a
reasonable order.

The suggested way to bring up the software on this  tape  is
to  run the install scripts in "pi", "pxp", and "px", and to
then install (some or all) of the software from "bin".   The
editor  "ex" can also be installed... this requires probably
as much work as all the others combined as it uses some data
bases  which don't exist on standard UNIX relating to termi-
nal types and capabilities.

Note that some of the scripts to make new  versions  of  the
software  on  this  tape use the programs in "bin".  You can
run these scripts easily, without adding all  the  stuff  in
"bin"  to  your  "/usr/bin", by using the shell in "ashell".
This shell has a number of nice features  and  was  used  in
making  all the software here... the files "errs" in each of
the major directories are outputs from the  "make*"  scripts
so  you  can  see  how  this  was  done.   Documentation for
"ashell" is in s6/sh.6.

The trek game in "trek" uses the rewritten portable  library
in  "portlib".   It  (and  the program tset) were written by
Eric Allman whose address is in the trek setup  instructions
in "trek".






















                           - 3 -


Directory contents:_________ ________



        pi           Pascal translator source
        px           Pascal interpreter
        pxp          Pascal execution profiler
        eyacc        Modified yacc for Pascal
        assubs       Assembly stuff for Pascal
        tests        Test programs for Pascal
        pcs          Wirth's Pascal-S
        pxref        Pascal cross-refence program
        opcodes      Definition files for Pascal
        fpterp       Sep ID floating point interpreter using FETCHI sys call
        s?           Command software source
        man?         Documents for s? stuff
        ashell       A new shell with some nice features
        ex-1.1       Ex source
        exrecover    Ex recovery routines (after system crashes)
        trek         Source for a "star trek" game
        portlib      Portable library used by trek
        exrefm       Troff source for "Ex 1.1 Reference Manual"
        puman        Troff source for "UNIX Pascal User's Manual"
        help         Sections from our help command

        lib          Routines for /lib and /usr/lib
        bin          Routines for /usr/bin
        etc          Stuff for /etc



If you don't have floating point:__ ___ ___ _ ____ ________ _____

If you don't have floating point hardware, and wish  to  run
Pascal,  you  will  need  to  add  a system call to fetch an
instruction word when  running  separate  I/D  so  that  the
floating  point  interpreter can work. The system call to be
added is "fetchi"...  if you can make  it  system  call  61.
(decimal)  then  the binaries on this tape will work immedi-
ately.  The code for "fetchi" will reside in sys4.c and look
like

        fetchi()
        {
                  u.u_ar0[R0] = fuiword(u.u_ar0[R0]);
        }

It is used as in:

                mov     $iaddr,r0
                sys     fetchi

to get the contents of location "iaddr", a word in  I-space.
Look  at  the  floating  point  interpreter in the directory









                           - 4 -


fpterp for a sample.  Don't forget that to make  the  system
call  work  you  must  add  an  entry to the sysent array in
sysent.c.

Miscellaneous notes:_____________ _____

A version 7 C compiler and many of the binaries in "bin" are
required  to  make  a  new version of "pascal" or "ex".  For
Pascal the file "nofloat" in this directory should  exist...
it  causes  the  scripts  in  the source directories to work
slightly differently.

Feedback:________

We would like to hear from all users of  the  Pascal  system
and  of  ex.   Reports of problems in bringing this software
up, or of bugs in the programs  or  documentation  would  be
appreciated.   We would also appreciate hearing of any local
improvements you make.

11/34 or 11/40 Pascal:__ __ __ __ __ ______

It is indeed unfortunate that the Pascal system  here  won't
run  on an 11/34 or 11/40... the only reason this is true is
that the translator "pi" is too large.  Pi used  to  be  two
pass  but  was  made  one pass about a year ago.  It is cer-
tainly possible to break pi into two passes or two processes
communicating through a pipe.  If you have an 11/34 or 11/40
and are interested in trying this I will be glad to give you
more details.

                              Bill Joy
                              CS Division
                              Department of EE and CS
                              UC Berkeley
                              Berkeley, California 94704

                              (415) 524-4510          [HOME]
                              (415) 642-4948     [SCHOOL]























Feb  1 12:44 1978  ashell/READ_ME Page 1


Wed Oct 19, 1977

This directory contains the source for a shell.
It requires floating point to do the time command which is built-in
so you will have to cc it -f on machines without floating point.
It also requires a version 7 C compiler.

Accurate documentation is in the file "sh.6" to be nroffed with
/usr/man/man0/naa and a new "version 7" nroff.

This shell requires the "htmp" data base also used by the editor "ex".
If you do not set it up so that the "sethome" command is done by "login"
then you should use the old "osethome" routine in ../s6 rather than "sethome"
and reenable the execl of this sethome in the file "sh.c" (with the correct
pathname).

                                Bill Joy
                                CS Division
                                Department of EE and CS
                                UC Berkeley
                                Berkeley, California  94704

                                (415) 524-4510          [HOME]
                                (415) 642-4948          [SCHOOL]







































Feb  1 12:44 1978  bin/READ_ME Page 1


November 13, 1977

The files in this directory belong in /usr/bin.
The only ones not documented are "lock" which lets you supply
a password to lock up your terminal (so you can go to the bathroom...),
rout which cleans old junk out of /tmp,
and teco which is of unknown origin (its mentioned in the Pascal
document so I threw it in.)

The programs "dates" "public" and "procp" all need to be setuid.
Dates requires the creation of a data file "/usr/lib/dates" and public
a directory "/usr/public"... procp needs to be able to read the memory.
Procp depends on your system configuration and will, most likely,
have to be recompiled.

The programs pi, pxp, px, ex and ashell as well as trek are in their
own directories.
The programs cpall/cptree in this directory can help you install all
this stuff...

                                        Bill Joy
                                        CS Division
                                        Department of EE and CS
                                        UC Berkeley
                                        Berkeley, California 94704

                                        (415) 524-4510          [HOME]
                                        (415) 642-4948          [SCHOOL]

P.S. Note that `l' and `ls' were linked together, and should be
linked again if you wish to use them...
































Feb  1 12:44 1978  etc/READ_ME Page 1


November 17, 1977

This directory contains prototype data files for /etc.
Htmp is here simply to indicate that it must exist... it can be created via
        cp /dev/null /etc/htmp
        chown bin /etc/htmp
        chmod 644 /etc/htmp

The file ttytype should be modified to conform to your configuration,
and new entries should be added to ttycap for the terminals which you
have.  Note that the programs "sethome", "tset" and "ttytype" must be able to
write on htmp so they must either be setuid or /etc/htmp must be mode 666
(safe only if you have a completely friendly community of users.)


















































Feb  1 12:56 1978  ex-1.1/READ_ME Page 1


January 30, 1977

There is a binary for ex1.1 in this directory (a.out) which can be "installed"
if you have a full load of user core and an 11/45 or 11/70.
If you have an 11/34 or 11/40 with 64K bytes of user space, you can
"mv a.outNOID a.out" and then "install".
If you prefer an ex without open and visual modes (for whatever reason)
choose between "a.outNOVISUAL" and "a.outNOVISNOID".
If you have a Berkeley type system (with full word significant user id's)
then you should use the "patchd" program in this directory on the binaries
you wish to use.  This is just a db "!" patch, but db doesn't work on
separate i/d programs.

Thus a typical installation, using just "a.out", on a standard UNIX system
would be:
        sh install

or to install the smallest ex here on an 11/34 or 11/40
        mv a.outNOVISNOID a.out
        sh install

This process will give you a basic editor (without any of its data bases)
which you can try.

Other files (other than /usr/bin/ex and /usr/bin/edit):

The editor keeps its error messages in a file.  This file is
"/usr/lib/ex1.1strings", and must be present or all errors will be
diagnosed as "ERROR".

The data base for the editor "help" command lives in "/usr/lib/how_ex"
(a directory)... the files in how_ex go there.

The programs "expreserve" and "exrecover" (from ../exrecover) should
be setuserid root and go in /usr/lib.  There should be a directory
/usr/preserve owned by root, mode 755 for use by these programs.

If you clean out /tmp in /etc/rc there should be a line
        /usr/lib/expreserve -a
before you do this.

This editor needs a number of hooks into the rest of the system to get
information about teletype types.  The changes needed in the system at
large to support this are described in the file SETUP.

The editor uses the data base "/etc/ttycap" to discern capabilities
of terminals, mapping a two character code it gleaned from the "htmp"
data base (described below) to the characteristics of that terminal.
You can add new terminals to /etc/ttycap quite easily... look
at ../s6/ttycap.5.  Adding a cursor addressible terminal requires
an editor recompilation if you want to use the cursor addressing.
Only a straightforward change to ex_ca.c is necessary.

There is a system data base "/etc/ttytype" which maps terminals
to 2 character type codes... see ../s6/ttytype.5.
Look at ../etc/ttytype for a sample of this data base... you







Feb  1 12:56 1978  ex-1.1/READ_ME Page 2


should change this file to correspond to your system.
You can add new types as necessary to "/etc/ttycap" in this process
or simply leave some terminals "unknown" for now.

The editor uses a data base "/etc/htmp" to determine a user's
home directory and his terminal type.
This is necessary for terminal types to handle dial-ups and for
home directories to allow them to be changed and to avoid
password file searches on systems with large password files.
The best way to implement the maintenance of "/etc/htmp" is to have
the "login" program maintain it.  There is a set of routines
in ../s7 (libX.a) which make this trivial.  The "htmp" routines
can be used to access htmp, the "typeof" routines to extract types
from /etc/ttytype.  If you don't have "login" do this, you will
have to do it by hand every time you login, or the editor may
have the wrong terminal type and will not be able to find your
start_up file. A login and an su program changed to handle the
maintenance of "htmp" are given in ../s1.

If you need to recompile:

This directory contains all of the source for "ex" version 1.1.
To recompile the editor you will need a version 7 C compiler
as well as the following non-standard programs:

        mkstr           create string message file
        lnall           link a number of files to a directory in one blow
        mvall           move all of a number of files
        rmtree          remove a hierarchical subtree
        cxref           a shell script giving a list of routine defn points

All of these programs are on this tape (except the C compiler) and of the
others, only mkstr is truly essential.
(Recompilation should not be necessary unless you wish to change the editor
or have a Version 7 UNIX system.)

System dependencies:

The only major problems here are the format of "/etc/utmp",
the form of teletype names, and the meaning of user/group id's.
Several of the programs in ../s6, notably "ttytype" and "sethome",
assume that "utmp" is accessed as a array indexed by the letter of the
terminal in use, treated as a number.  Other systems have slots
arranged '0', '1', ... '9', 'A', ... .
If you have the latter format you'll have to change these programs.

If you treat a user/id as being significant in all 16 bits
returned from "getuid()" in determining if two people are the
same person (i.e. if you have "newgrp") then should use the
program "patchd" to change the initial value of "mask" to be 0177777, i.e.:
        patchd _mask 177777 a.out

This is trivial, but essential.
Note that you must also change the source for the ttytype and sethome
programs to not mask off these bits and recompile (or use patchd)








Feb  1 12:56 1978  ex-1.1/READ_ME Page 3


Finally the editor (and all other programs here) assume ttynames of the
form "/dev/ttyx" with x a single letter.

If you are having system related problems or have questions
please feel free to give me a call.

Other, less serious, dependencies are:

1. This editor assumes that you have a restricted, 512 byte
argument list.  If your system gives larger lists that is no problem,
but the "next" command will allow at most 512 character lists on
subsequent matchings.

2. The major and minor device numbers of /dev/null and /dev/tty
are used and are given in ex_io.h.  Also the system error codes are mapped
from magic numbers to names here.  If your system has additional codes
these numbers will have to be extended and new cases will have to be
added to the switch on page 6 of ex_io.c.

3. To add a cursor addressible terminal in this version, you must recompile.
A simple change is required to the routines in "ex_ca.c", with a return
code of 1 being supplied for the new known type, and a string doing the
addressing being returned from cgoto.  This information should
be put in /etc/ttycap, but I haven't seen enough terminals to know
a good encoding.

4. This version of the editor needs a printf which prints through putchar.
Such a printf exists in printf.s in this directory.

5. The read routine rop in exr.c knows about special binary files.
Thus if you have any more binary files with different magic numbers
it makes sense to add them here so the editor will give better diagnostics.

I would like to hear of other dependencies/problems you encounter.

Scripts:

The following scripts are in this directory of general interest

        makeex          make a new binary and string file
        comp            recompile and load one or more files
        install         put new ex in /usr/bin... you may want to change
                        the path names

The shell used for these scripts is in the directory ../ashell,
with documentation in ../s6/sh.6.
If you comment out the "set" commands or make a dummy set command
which does nothing then any other shell should do fine.

Note also that the scripts making ex use a "version" shell script
which uses ex.  If you have no ex, the script will fail in a safe way.

In order for the option setting for "edit" to work either the second
or third character of its name must be a 'd'.  Thus "edit" and "nedit"
are fine.  This is naive, but easy to change... look at the first few
lines of ex.c.







Feb  1 12:56 1978  ex-1.1/READ_ME Page 4




To complete the installation of ex you must also install exrecover and
expreserve see the directory ../exrecover.  I would appreciate hearing of
any problems you have with the editor or of any improvements you make.
One thing which would be nice to have is the ability to drive terminals
with more intelligent operations such as add or delete a line on the screen
or insert characters pushing them to the right.
I havent done this because we have only 2 such terminals
both of which are hard-wired at 9600 baud (and private terminals).
The changes needed to do this are almost all localized in the routine
"ex_vadjust.c".


                                        Bill Joy
                                        CS Division
                                        Department of EE and CS
                                        UC Berkeley
                                        Berkeley, California 94704

                                        (415) 524-4510          [HOME]
                                        (415) 642-4948          [SCHOOL]









































Feb  1 12:49 1978  exrecover/READ_ME Page 1


November 17, 1977

To have a fully functioning recovery mechanism you should
place a command of the form
        /usr/lib/expreserve -a
in /etc/rc if you clean out the directory /tmp there.
For full security, exrecover and expreserve should be setuid root
and the directory /usr/preserve should be mode 700.
If you don't need or want this, change "install" and "setup".






















































Feb  1 12:49 1978  eyacc/READ_ME Page 1


August 28, 1977

This directory contains source for a version of yacc needed by the Pascal
parser.  The differences between this yacc and a stadard version 6 yacc
are indicated in a comment in y1.c.

Note that the standard yacc parser will not work on the tables produced
by "eyacc" and also that these changes are really useful only with
a fairly large set of error recovery routines which are part of both
"pi" and "pxp".  The routines are language independent, but the method
will only work on languages which have some redundancy in them... it is
probably ill suited for C, but would work fine in ALGOL-60, ALGOL-W,
EUCLID, LIS, SP/K, PL/1, ALPHARD, CLU, ...

I am working on a short document describing the internals of the error
recovery technique used in "pi"... It is a simple modification of the
Graham/Rhodes technique described in a recent article in the CACM.


                                Bill Joy
                                Computer Science Division
                                EECS Department
                                University of California, Berkeley
                                Berkeley, California  94704

                                Office: (415) 642-4948
                                Home:   (415) 524-4510




































Feb  1 12:49 1978  fpterp/READ_ME Page 1


November 13, 1977

This directory contains the source for a floating point interpreter
modified to work in separate i/d space with a system call "fetchi"
as described in ../SETUP.  The interpreter assumes that the
system call in number 61.

If you have to add this call because you don't have floating point hardware,
and you cannot make it call 61. you don't have to remake all the Pascal
stuff... simply running the "load" scripts in ../pi, ../pxp and ../px
is sufficient... these don't require that you have a version 7 C compiler.

                                        Bill Joy
                                        CS Division
                                        Department of EE and CS
                                        UC Berkeley
                                        Berkeley, California 94704

                                        (415) 524-4510          [HOME]
                                        (415) 642-4948          [SCHOOL]











































Feb  1 12:50 1978  lib/READ_ME Page 1


The routines in libX.a are from ../s7 and are used by ex and ashell.






























































Feb  1 12:50 1978  opcodes/READ_ME Page 1


August 18, 1977

Contents of this directory are as follows:

opc.d           Description of machine opcodes.
OPnames.h       Opcode data for put.c - comes from opc.d via makeopc.
opcode.h        Opcode #defines - made from opc.d by makeopc.
picture         Pretty picture of abstract machine opcodes.
trdata          Data pertaining to the parse tree.
tree.h          Tree defines from trdata out of maketrdata.
TRdata.c        Tree description and printing names of operators.




















































Feb  1 13:00 1978  pcs/READ_ME Page 1


November 13, 1977

This directory contains the source for the Pascal program pascals.
It should be put in /usr/lib/pascals, the C program in in ../s6/pascals.c
should be put in /usr/bin/pascals... this C driver hunts up "px"
and executes the /usr/lib/pascals.

For this program to run at any sort of reasonable speed it should be
compiled with the options "-p" and "-t" and preferably "-b", i.e.:

        pi -pbt pascals.p




















































Feb  1 13:03 1978  pi/READ_ME Page 1


November 9, 1977

This directory contains the source necessary to make a new pi.
Many of the files in this directory were linked to files with the same
name in the directory ../pxp, but the links were broken by tp.
There is a binary of pi here that should work immediately.
If you have floating point or with the simple system change in
../READ_ME.  If you have to give a different number to the system
call, you can simply reassemble the floating point interpreter and
reload pi via
        load -i

To print a copy of pi:          printpi
To make a new pi:               makepi

Read the file ../SETUP before you bother with any of this.

The compiler made by makepi is suitable for running on an 11/45 or 11/70
which has hardware floating point.  If you don't have hardware floating
point, it is not possible to run separate I/D using the standard interpreter.
Standard version 6 UNIX does not have the needed system call to allow
it to fetch the floating point opcode from instruction space, and the
mfpi instruction doesn't work on 11/70's.  A simple system change to add
a "mfpi" system call is explained in ../SETUP;  the floating point interpreter
which uses this system call is in ../fpterp.

This tape does not contain a Pascal system which will run on
11/34's or 11/40's.  Chuck and I were misinformed that
11/34's would have sep I/D so we went to a one pass compiler scheme.
It should be possible to make a smaller "pi" or a two pass "pi" without
a great deal of effort.  Earlier versions of the translator
"pi" were small enough to run without separate I/D, and a still earlier "pc"
was two rather much smaller passes.

The following non-standard programs are needed to compile pi:

        rmtree          Remove a subtree of the directory system
        mvall           Move a named group of files to a specified directory
        cc              Version 7 C compiler
        lnall           Make links to a number of files in a specified
                        target directory
        squash          reduce object file size for library
                        not needed, just speeds loading and reduces
                        library size
        mkstr           Program to process C source putting error messages
                        into an error message file
        eyacc           Modified yacc

In addition, the scripts here run with a shell that has a "set" command
to enable automatic timing of commands.  You can comment out the lines
of the form
        set ...
without any harm, or make a null set shell script.
Other local shell features used here are the alias "cd" for chdir and
the syntax "$*" expanding to all the arguments, i.e. "$1 $2 $3 ...".








Feb  1 13:03 1978  pi/READ_ME Page 2


Note: it is normal for the grammar to have a number of shift/reduce
conflicts; a message of the form

        conflicts: 16 shift/reduce

(or worse) from yacc is to be expected.

Defining the variable DEBUG on the first line of the file 0.h allows
the following debugging options

        c               print generated code
        y               dump namelist
        E               trace basic syntactic error recovery
        F               full trace syn err rec
        A               super full trace ...
        U               prevent unique symbol insertion in error recovery

As this option makes a much larger compiler it is not normally recommended.

The syntactic error recovery relatively new code.
It has not been as thoroughly tested as the rest of the system.

Please let me know of any problems with Pascal, especially with the
error recovery.  I would be glad to hear of any problems, as well as
any local modifications which you find necessary or desirable.

                                        Bill Joy
                                        Computer Science Division
                                        EECS Department
                                        University of California, Berkeley
                                        Berkeley, California  94704

                                        Office: (415) 642-4948
                                        Home:   (415) 524-4510





























Feb  1 13:02 1978  px/READ_ME Page 1


August 28, 1977

This directory contains all the source for the interpreter px.
The script "makepx" will make a new "px", the script "printpx"
will print a copy of all the stuff here.  The binaries here can be installed
immediately with "install".   If you have no floating point use a.outNOFLOAT
rather than a.out; do
        mv a.outNOFLOAT a.out
before
        install

In making a px for a system without floating point, the program "flt40"
is used to massage the interpreter code to make it run a good deal faster.

                                        Bill Joy
                                        Computer Science Division
                                        EECS Department
                                        University of California, Berkeley
                                        Berkeley, California  94704

                                        Office: (415) 642-4948
                                        Home:   (415) 524-4510









































Feb  1 13:02 1978  pxp/READ_ME Page 1


November 16, 1977

This directory contains the source for pxp.
Many of the files here were linked to the same in the directory ../pi.
The links were broken by tp.  The a.out binary here can be installed

        install

This is all that should be necessary.

To make a pxp do                makepxp
To print a listing do           printpxp

The variable DEBUG can be defined allowing tracing of the error
recovery in the parser.
Look at the first line of "0.h" to see if DEBUG is defined.
A smaller pxp results if it is not.

Non-standard programs required to make a pxp

        cc              Version 7 C compiler, version 6 will not work
        eyacc           Need modified yacc distributed with pxp
        set time=3      Causes commands taking more than 3 seconds of
                        processor time to be "timed" in a variant shell.
                        You can comment out this line or do
                                cp /dev/null set; chmod 755 set
                        to avoid a "set: Not found" diagnostic.

I would like to know of any problems with pxp, or of any
local modifications which you find necessary or desirable.

                                Bill Joy
                                Computer Science Division
                                EECS Department
                                University of California, Berkeley
                                Berkeley, California  94704

                                Office: (415) 642-4948
                                Home:   (415) 524-4510
























Feb  1 13:02 1978  pxref/READ_ME Page 1


To make a pxref do
        makepxref
It can be installed using
        install
If you have a shell which recognizes Pascal objects it is not necessary
to use both the Pascal and C programs... the Pascal object can be put in
/usr/bin... but it will have the same status as shell scripts (essentially)
thus
        time pxref pxref.p
would not work then.





















































Feb  1 13:02 1978  s1/READ_ME Page 1


Here are a passwd command which asks you for the password twice so
you dont screw yourself up, and a shell which has a couple nice features
(interruptible waits, redirect unit 2) and which knows about Pascal
objects... the glob (../s8/glob2.c) which goes with this shell
takes arbitrary path names, i.e.
        /*/mbox

This shell has been the standard at Berkeley for over a year.

The login and su in ../s8 maintain the htmp data base, and also
implement ".start_up" files which a shell runs when you log in.
Also implemented are the file ".reminder" which you can place
in your login directory to have catted on your terminal when you
log in.

These features of login are superfluous if you have "../ashell"
but very useful otherwise.  The "su" program also maintains an
unused byte in /etc/utmp so that the "who" here can print out
who you are "su'd" to ... currently we disable su's to anyone but
the root except by the root ... this is easy to take out.











































Feb  1 13:02 1978  s7/READ_ME Page 1


These are the routines for manipulating the editor data bases.
They live in ../s7/libX.a
The program "Ttyn.c" is actually made by the program "makeTtyn"
in ../s6... it produces a hashed version of "ttyn" called "Ttyn"
which is useful because it runs much faster... it is however
dependent on the structure of your /dev/.
If you want the editor and ashell to start faster you can have them
call a version of Ttyn which you can make using the program
makeTtyn for your system.






















































Feb  1 13:02 1978  s8/READ_ME Page 1


The glob2.c goes with the sh.c in s1.
The login and su are here so hyou can see the (simple) changes needed
to support the editor data base /etc/htmp.
They need the routines in ../lib/libX.a

See also ../s1/READ_ME

























































Feb  1 13:03 1978  SETUP Page 1


October 12, 1977

The editor uses the following data bases:

        /etc/ttytype            gives teletype types of hardwire ports
        /etc/ttycap             gives capabilities of teletypes
        /etc/htmp               gives home directories and teletype types

The implications of the absence of these are as follows:

        /etc/ttycap
                Editor will think all terminals are model 33 teletypes,
                essentially, as they will be "unknown."  You'll have
                no way of specifying the capabilities of your terminal.

        /etc/ttytype
                You will have to tell the editor the type of the terminal
                you are on every time you log in, unless you trust
                that the way the last user of your port set your terminal
                type is correct.

        /etc/htmp
                Editor start-up files cannot work;  you won't be able to
                specify your terminal type once per login... you have to
                do it each time you enter the editor.

These data bases are maintained and used in the following ways:

        HTMP data base:
                The file /etc/htmp contains a structure described by htmp (V).
                It contains, for each user, his "home" directory, normally
                the login directory, his user-id, and the type of terminal
                he is on.  The home directory is here because on large
                systems searching the password file is unreasonably slow.
                Its presence in this data base also allows it to be changed.
                The tty type information is necessary here because users
                who dial in on a dialup port need to be able to specify it.

        TTYTYPE data base:
                The file /etc/ttytype is organized similarly to /etc/ttys
                and maps tty names to 2 character codes.  This data base is
                used both by the editor and by the program "tset", and
                can be used by other programs.

        TTYCAP data base:
                The file /etc/ttycap allows programs to map a terminal's
                type code to its characteristics.  This allows addition
                of new terminals to the system without changing any existing
                programs - only the data base needs to be updated.  (Note
                that currently cursor addressing information is not recorded
                here requiring changes to the editor to add new such terminals.)

The following utility programs are included with the editor:

        SETHOME
                Set the home directory entry in /etc/htmp.







Feb  1 13:03 1978  SETUP Page 2



        TTYTYPE
                Set the teletype type entry in /etc/htmp.

The following changes to support the editor are suggested:

        LOGIN
                So that naive users may login on dial up ports and have
                their home directory and terminal-type set to reasonable
                values at initialization without any action on their part,
                the program login should be changed to write the initial
                entry in /etc/htmp.  The work involved is in getting the
                terminal type from the file /etc/ttytype and writing it
                into /etc/htmp.  The overhead should be negligible,
                especially since the operations of reading /etc/passwd
                and looking for mail in /usr/mail are typically much more
                expensive.  This change is simple to make, just calling
                some of the routines in the supplied library htmp (V) ..

        SU
                So that the home directory will be correct after a su
                command, the command should be changed to save and restore
                this entry in /etc/htmp before and after the su.
                This is similar to the saving and restoring of the utmp
                user byte entry to allow who to print out the name of
                the person one is su'd to.  (This latter change has been made
                at Berkeley.)

For more information on formats and programs see the following documents

        Section V:      htmp, ttycap, ttytype
        Section VI:     sethome, ttytype
        Section VII:    htmp, typeof, ttycap


                                        Bill Joy
                                        CS Division
                                        Department of EE and CS
                                        UC Berkeley
                                        Berkeley, California 94704

                                        (415) 524-4510          [home]
                                        (415) 642-4948          [school]




















Feb  1 13:03 1978  SYSDEP Page 1


November 9, 1977

KNOWN SYSTEM DEPENDENCIES / MAGIC NAMES IN EX-1.1

Globally:

1. Needs a printf which prints through putchar.
2. Wants the data bases described in SETUP.
3. Needs a large amount of core; separate i/d preferred.
4. Needs a version 7 C compiler.
5. Assumes (in the library routines and the way it deals with ttyn
   e.g. for the visualmessage option) that teletype names are single
   characters, and that /etc/utmp is indexed by this character.
   (More on this below.)

---------------------------------------------

In the HEADERS:

ex.h
        Defines ECHO and RAW for stty.
        Defines TTYNAM which is initialized in ex_tty.c and there
            assumes that single char teletype names are used.
        Note that ex does not catch the TERMINATE signal of version 7
            UNIX... it should be defined here.

ex_glob.h
        The definitions here limit the argument list size possible with the
            next command.  This does not limit the size of entry argument
            list however... if invoked with a longer list that is ok (although
            it has never been tested of course!)

ex_io.h
        The basic stat buffer structure is assumed here.
        Also the error codes for errno from system error returns on i/o
            are defined here... if you have more error codes you should
            add them... ex does not use "perror" because this way puts
            the messages in the string message file saving space.

ex_vis.h
        The definitions of TUBELINES, TUBECOLS, and TUBESIZE here
            limit the kinds of terminals on which open and visual are possible.
            The areas are allocated, with fixed size, on the stack at entry to
            the routines in exo.c and exv.c.  Variable sized screens would be
            possible if you added an assembly language intermediary here,
            or you can make these numbers larger, at the expense of allocating
            these larger buffers on terminals with small screens.

---

CODE files:

ex.c
        /erpath =+ 9/
                Assumes that the error path contains a prefix like "/usr/lib"
                    so that adding 9 bytes will give the last portion of the







Feb  1 13:03 1978  SYSDEP Page 2


                    error file name for testing "a.out".
        /== 'd'/
                Here derive the properties of "edit".
        /signal/
                New caught signals should be added here, notably
                    the signal TERMINATE for a version 7 system.

ex_ca.c
        To add a cursor addressible terminal you must add it to the
            routine canCA and change cgoto to return a string.
            All terminals which are to be added must have their properties
            recorded in /etc/ttycap.

ex_glob.c
        This glob routine supports constructs in all portions of path names,
            e.g. "/*/bill/a.out";  it also interprets trailing slashes as
            forcing a directory match - eg "*/" matches all subdirectory names.
            This corresponds to the glob which is glob2.c in ../s6
            and also the glob built into ../ashell.

ex_tty.c
        This routine assumes the version 6 structure of teletype names
            in its handling of TTYNAM.  This can be easily changed
            by forming the value of TTYNAM in a slightly different way.

ex_io.c
        The routine ioerror embodies the strings from "perror" which are
            related to input/output.  These should be added to or changed
            as appropriate to your system.  Perror is not used because
            it is desired to have the error messages in the string message
            file (this saves ~~ 200 bytes per editor user in the swap image.)


ex_put.c
        Note the routine "setcol" which fixes UNIX's notion of the tab
            column position after a cursor addressing sequence on an ADM-3A.
            This is hard, in general.  If you wish to perform a similar
            fix for your terminals this is the place to do it.

ex_recover.c
        The routines here assume that you have installed the exrecover
            and expreserve routines from ../exrecover, and that when
            the system crashes you run expreserve in /etc/rc to save the stuff
            from /tmp so that people can continue where they left off.
            If this is not true, no harm is done... as long as you don't ever
            crash!

ex_set.c
        Note that the default directory and shell are initialized here.

ex_tty.c
        Here the capabilities of the terminal which interest us are extracted.
            These are used by the print routines in ex_put.c, and more exten-
            sively by the visual and open mode routines.

            Currently, we are running visual on ADM-3A's (its native terminal)







Feb  1 13:03 1978  SYSDEP Page 3


            and also on HP2645's.  For the latter, the editor makes use only
            of the clear to end-of-line operation.  I have not put in the use
            of the add and delete line operations (although they are read
            here into AL and DL).  If you have a number of intelligent
            terminals you may wish to try to add the intelligent terminal
            driving to visual... it is not hard.  I have not done it since
            we have only 2 HP2645's and both are at 9600 baud where it matters
            little if it knows about the intelligence.

            The routines most likely to be changed in such an addition are
            all in the file ex_vadjust.c... they are mentioned below.

ex_unix.c
        It is here assumed that the shell understands the option "-c",
            and also the option "-i" to give a login type shell.  Both of
            the shells from Berkeley, and also the Version 7 shell from
            Bell have this property.  Note also that substituted file
            names are given high-order bits set so that glob will not
            be a nuisance... it is better of course not to edit files
            with funny names.

ex_vadjust.c
        This is where the work is required to add the AL (add line)
            and DL (delete line) capabilities of the terminal to open
            and visual.  Likely candidates for change are the routines
            "vopen", "vsync", and "vredraw"... with these capabilities
            you may well want to always use "vredraw"... "vsync" is the
            routine which leaves the dirty "@" lines on the screen.
            If you do any work here I would like to hear of it as I
            am not planning to do this but would like to have working
            code for it.

ex_vcurs.c
        Note that the routine "vputchar" assumes that you can simply
            overwrite and have no trouble (that overstrking an "a" with
            a "b" works leaving a "b").  If you have terminals where this
            is not true (i.e. that have OS) you can send a blank to clear
            the position first... rumor has it that there are some
            ITT beasts of this flavor (at UCLA?).

exr.c
        Sensibility detection by decoding the flags bit of a stat buffer
            an by looking at the magic numbers of PLAIN files should
            be changed to reflect the kinds of stuff you have in your
            file system.

exw.c
        Note the explicit checks for /dev/tty and /dev/null by major
            and minor device number here.

--------------------------------------------------

I would be glad to learn of any other problems you have or changes
you make to ex.

I will be glad to answer questions by mail or phone, and would be glad to know







Feb  1 13:03 1978  SYSDEP Page 4


of any fixes and changes.

                                        Bill Joy
                                        CS Division
                                        Department of EE and CS
                                        UC Berkeley
                                        Berkeley, California 94704

                                        (415) 524-4510          [home]
                                        (415) 642-4948          [school]

























































           Update to Ex Version 1.1 Documentation



This sheet is a quick update to Ex version 1.1 documentation  for  features
which were added after the documentation was prepared.

Crash Recovery_____ ________

     The crash recovery mechanism has  been  improved  to  recover  mangled
buffers  and  to  mail  to  users that their files were lost after a crash.
More information is available in ex "help".  It is also possible to recover
a  buffer  which  had  no  current file name; it is saved as though it were
named "LOST".

Smaller Screens in Visual_______ _______ __ ______

     A new, partially implemented feature, is smaller visual screens.   You
can specify a smaller window size for visual by

        set window=10

or on the "visual" command, similarly to the "z" command, i.e.:

        vi.5


Visual features - Unimplemented, 11/34's and 11/40's______ ________   _____________  __ __ _ ___ __ __ _

     The visual operations ``e'', ``E'', ``<'' and ``>'', which were  unim-                                                                      _____
plemented  in  the  documentation  are,  in  fact, available on 11/45's and_________
11/70's but not on 11/34's and 11/40's due to space problems.  In addition,
the  sequences ``^CTRL(d)'' and ``0CTRL(d)'' are not available with autoin-                                                                    _______
dent in visual and open modes on the smaller machines.____    ______     ____

Future visual changes______ ______ _______

     The following additions to visual are contemplated:                                ______

1)   An operation `V' to dynamically respecify the window  size.   With  no
     argument,  `V'  will set the window size to the maximum possible, this
     being rather innocuous.

2)   Operations to deal with LISP S-Expressions.

3)   Handling of intelligent terminals.











                             February 1, 1978








                Ex documentation corrections


Table of Contents_____ __ ________

     On page ii under Summary Tables change Open and visual targets to Open                      _______ ______        ____ ___ ______ _______    ____
and visual operations.___ ______ __________

Crash Recovery_____ ________

     It is no longer true that you must have had a  current  file  name  to
recover after a crash; files with no name are arbitrarily named ``LOST''.

     Also note that if the system crashes you will  receive  mail  when  it
comes  up  telling you of the name of the file saved for you if you were in
the editor.

File command____ _______

     The file command description should begin ``The current filename'' not         ____
``The  current  file''  as  the editor does not have a current file, only a
current filename associated with a buffer.

Visual Command______ _______

     The visual command may take a trailing count indicating the number  of         ______
lines  to  be used (physical) in the window.  The default for this count is
the value of the window option.                 ______

Visual Operations______ __________

     The operations ``e'', ``E'', ``<'' and ``>''  are  implemented  unless
you are on a machine without separate I/D space (Evans "D" system) in which
case the features ``0CTRL(d)'' and ``^CTRL(d)'' with autoindent in open and                                                     __________    ____
visual also don't work.______

Ex manual section__ ______ _______

     Add the following bugs:

     Lines which are changed or joined lose marks; it would be  better  for                     _______    ______
     the marks to be attached to the (first) new line.

     There should be an option for maintaining a perfectly clean screen  in
     visual and open on very fast (or intelligent) terminals.     ______

Ttycap data base______ ____ ____

     In the descripton of the ttycap data base, change ``set and  cleared''
to ``cleared and set'' in the second line of the third paragraph.  Refer to
stty (II) for more information about the flags described in this paragraph.






                             February 1, 1978


