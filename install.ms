.if n .nr LL 7i
.TL
Second\ Distribution\ of\ Berkeley\ Software\ for\ UNIX\(dg
.sp
Installation\ Instructions
.PP
.FS
\(dg UNIX is a trademark of Bell Laboratories.
.FE
By following the directions here you
should be able to bring up the software on the tape in a very short
period of time, ranging from an hour (if you have a standard version 6
system and can use the precompiled binaries), to about 6 hours (if you
have a version 6 or 7 system which requires recompilation.)
.SH
Extracting the tape
.PP
To extract the tape you will need a 800 BPI tape drive and a file system
with 12000 blocks of free space.  If your tape was written with \fItar\fR
or \fIcpio\fR, then extracting the tape is straightforward.
If it was written with \fItp\fR (the default) then some work
will be required to fully unpack the tape.  Unless you will be
using \fItp\fR here skip the next paragraph.
.PP
First extract the file \fBcreate\fR from the tape by doing:
.DS
tp xm ./create
.DE
This is a shell script of \fBmkdir\fR commands.  Run the script and then
do
.DS
tp xm
.DE
This will take several minutes.  When it completes, you will have a skeletal
tape.  In most directories on the tape will be a file \fBcont.a\fR
which is an archive of the contents of that directory.
The best thing to do is to unarchive all the files so you can look
at things easily.  The find command
.DS
find . \-name cont.a \-a \-print
.DE
will print the names of all the \fBcont.a\fR files.  For each such file,
change into its directory and do
.DS
ar x cont.a
rm cont.a
.DE
You can omit the \fBrm\fR if you have tons of space.
If you have a very old \fIar\fR, you may have to use the \fIar\fR in
\fBmisc\fR.
.bp
.SH
Installation Preparation
.PP
The first thing to determine is which version of \s-2UNIX\s0 you are running,
and how much impact modifications you have made to the system will have
on the software here.  
.SH
Version 7
.PP
This is the new version of \s-2UNIX\s0 which has just been released by Bell
Laboratories.  Most of the software here has been running on version 7 for
several months at Berkeley.
.PP
The binaries on the distribution tape will NOT run on version 7, as
they were compiled on a \s-2PDP\s0-11 version 6 system.  Thus you must
recompile from the source for a version 7 system.  This will not be
hard since almost all of the software thinks it is running on version
7.  The one exception is the Pascal system, which has not been run on
version 7 (since we don't have version 7 on our \s-2PDP\s0-11s yet).  
.SH
Version 6
.PP
If you have a standard version 6 \s-2UNIX\s0 system then you can just use
the binaries on the tape and avoid the bother of recompiling.  In fact,
unless you have a late-model C compiler
compilation may be troublesome or impossible.
.PP
On a ``standard'' system the \fIgetuid\fR system call returns
the user id in the low byte of its result word.  If this is
the case on your system, then you should have no trouble installing the
binaries supplied.\(dg
.FS
\(dg I have compiled the code here with a library which uses ``sys sleep''
to implement \fIsleep()\fR, rather than the later, more efficient sleep
implementation using new system calls pause and alarm.  If you have the
latter, you can recompile programs which sleep if you wish.
.FE
.SH
Other Version 6
.PP
If you have a version 6 \s-2UNIX\s0 system which has 16 bit user id's (such as
the systems at Berkeley) then you will have to modify the \fBupgrade/libretro\fR
version 7 simulator library and recompile the programs here.
.PP
If you have a \s-2PWB\s0/\s-2UNIX\s0 system, the binaries supplied here should work
(as far as I know).  If they don't the \fImake\fR and \fIcc\fR from \s-2PWB\s0
should be adequate to recompile to repair any problems.
.bp
.SH
New files to be added.
.PP
The following are the major files and directories
which will be created as you install the tape:
.de BP
.IP \fB\\$1\fR 28
..
.BP /bin/csh
This is the new shell.  It is not placed in
the directory \fB/usr/ucb\fR because it is often linked to \fB/bin/makesh\fR,
which is on a different file system than \fB/usr/ucb\fR on
most systems.
.BP /etc/htmp
For version 6 systems, this forms a data base which simulates
version 7 environments, storing home directories and (most
importantly) terminal types for each terminal.
.BP /etc/termcap
This is a data base describing terminals, and is used by the \fIex\fR editor,
and the \fItset\fR program.
.BP /etc/ttytype
This file maps terminal ports to their types, and indicates which
ports are not hard wired.  The
.I tset
program uses this to initialize the terminal type at login.
.BP /usr/include/retrofit
On version 6 systems, a directory of header files used to simulate version
7 \s-2UNIX\s0.
.BP /usr/lib/Mail.help*
Help files for \fIMail\fR.
.BP /usr/lib/Mail.rc
A startup file for \fIMail\fR.
.BP /usr/lib/ex2.0preserve
Preserve command for \fIex\fR.
.BP /usr/lib/ex2.0recover
Recover command for \fIex\fR.
.BP /usr/lib/ex2.0strings
Error messages for \fIex\fR.
.BP /usr/lib/how_p*
Help files for Pascal.
.BP /usr/lib/libretro.a
Library simulating some version 7 calls on version 6.
.BP /usr/lib/libtermlib.a
Library providing terminal independent functions.
.BP /usr/lib/me
[Directory]
The dynamically loaded parts of the \fI\-me\fR macros are placed here.
.BP /usr/lib/pi1.2strings
Error messages for \fIpi\fR, the Pascal translator.
.BP /usr/lib/pi1:2strings
Messages for two process \fIpi\fR for 34's and 40's.
.BP /usr/lib/pi1
Second pass of two process \fIpi\fR translator.
.BP /usr/lib/*px_header
Header files which \fIpi\fR prepends to \fIobj\fR files.
.BP /usr/lib/tabset
[Directory] Terminal initialization files for \fItset\fR.
.BP /usr/lib/tmac.e
The \fI\-me\fR macros themselves, on version 6 systems.
.BP /usr/lib/tmac/tmac.e
The \fI\-me\fR macros themselves, on version 7 systems.
.BP /usr/msgs
[Directory]
The \fImsgs\fR program places messages here.
.BP /usr/preserve
[Directory]
Editor temporaries are preserved here after system crashes.
.BP /usr/ucb
[Directory]
Most of the binaries on the tape are placed here.
They can be linked elsewhere (i.e. \fB/usr/bin\fR) but
the makefiles which create the tape software expect
them in \fB/usr/ucb\fR so they should be left there also.
.bp
.SH
Installation procedure.
.PP
Now follow the following procedure:
.IP 1.
Run the \fBsetup\fR script in this directory to create needed files
and directories.
.IP 2.
If you have a version 6 system then run the \fBinstall\fR script in the
directory \fBupgrade/include\fR to put a copy of the retrofitting header
files in \fB/usr/include/retrofit\fR.
.IP 3.
If you have a standard version 6 system (with 8 bit user id's) then
run the \fBinstall\fR script in the directory
\fBbin\fR on the tape.  Then skip to step 6.
.IP 4.
If you have a non-standard version 6 system which uses 16 bit user-id's
or has other modifications which would destroy binary compatibility,
then:
.RS
.IP a.
Look at the retrofit library source directory \fBupgrade/libretro\fR and make
needed changes.  Recreate the library and install it.
If you have \fImake\fR you can use the \fBmakefile\fR; otherwise
use your shell with \fBmake.script\fR.
.IP b.
Recompile the termlib library \fBsrc/termlib\fR, using \fBmakefile.v6\fR
and ``make install'', or the shell script \fBmake.script\fR
if you don't have \fImake\fR.
.IP c.
Recompile the programs in \fBupgrade/src\fR using \fBmake.script\fR
or \fBmakefile\fR.  These are versions
of some programs in \fBsrc\fR which are different for version 6.
.IP d.
Follow the rest of the instructions for making a version 7
compilation, using \fBmakefile.v6\fR or \fBmake.script\fR whenever
they exist rather than \fBmakefile\fR.
(You can skip part \fIa\fR since you have done it already.)
.RE
.IP 5.
If you have a version 7 system:
.RS
.IP a.
Run \fImake\fR in \fBsrc/termlib\fR, since this makes an important
library which you will need right away.
.IP b.
Then start in the \fBsrc\fR directory, and run \fImake\fR there and
then in each subdirectory (see below).  Look at the \fBREAD_ME\fR files in
each directory to get an idea of what is going on.  After
creating the binaries ``make install'' will install them in \fB/usr/ucb\fR.
Some makefiles also install things in \fB/usr/lib/\fR or \fB/etc\fR; use
``make \-n'' to see what \fImake\fR will do.
.IP
The following is a reasonable order to do the subdirectories in:
(omitting Pascal for now):
.DS
Mail, csh, ex, me
.DE
.IP c.
Install the Pascal system.  Some of the parts of the Pascal
system will require special treatment on version 7 as they
use the older i/o library of version 6.  See the file \fBmisc/v7pascal\fR
for more details.
.IP
It is not necessary to
compile \fBeyacc\fR or to run \fIeyacc\fR in the \fBpi\fR and
\fBpxp\fR directories; rather just use the supplied \fBy.tab.c\fR files.
(The supplied \fBmakefile\fRs don't run \fIeyacc\fR.)
.IP d.
Now prepare the utilities for the Pascal system in the
directory \fBpascal\fR.  Then prepare the Pascal translator \fBpi\fR,
the interpreter \fBpx\fR and, finally, the profiler \fBpxp\fR.
.IP
If you have a non-separate I/D machine, or if you do not have
hardware floating point, then prepare \fBpi0\fR and
\fBpi1\fR rather than \fBpi\fR, and use the \fBpx34\fR and \fBpxp34\fR
(NOID) versions of \fIpx\fR and \fIpxp\fR.\(dg
.FS
\(dg You can run \fIpi\fR (instead of \fIpi34\fR from \fBpi0\fR and \fBpi1\fR)
on a non-floating point machine with separate
I/D by adding a system call to simulate a mfpi instruction
(see \fBmisc/fetchi.sys\fR).
This \fIpi\fR will run slightly faster, and allow slightly larger programs
to be written.
.FE
You should, on these machines:
.DS
mv /usr/ucb/pi34 /usr/ucb/pi
mv /usr/ucb/px34 /usr/ucb/px
mv /usr/ucb/pxp34 /usr/ucb/pxp
.DE
.RE
.IP 6.
Install the manual sections in \fBman\fR copying them to \fB/usr/man/manu\fR.
If you have version 6, follow the instructions in \fBupgrade/man\fR
on adapting to the different manual macros used.
.IP 7.
Add a line of the form
.DS
/usr/lib/ex2.0preserve \-a
.DE
to the file \fB/etc/rc\fR, before it cleans files out of \fB/tmp\fR.  This
will preserve the editor temporaries from \fB/tmp\fR after system crashes,
and implements the editor crash recovery mechanism.\(dg
.FS
\(dg If \fB/usr\fR is a mounted filesystem, be sure it is mounted before
you try to run \fB/usr/lib/ex2.0preserve\fR.
.FE
.IP 7.
So that the \fImsgs\fR program can receive messages which are sent via
.B mail
change, change your mail program to execute
``/usr/ucb/msgs \-s''
with the message on the standard input whenever mail is sent to ``msgs''.
A version 6
.B mail
program which does this is in
``mail.c''
in the directory \fBmisc\fR.
.IP 8.
Make sure that the programs \fB/usr/lib/ex2.0preserve\fR and
\fB/usr/lib/ex2.0recover\fR can write the directory \fB/usr/preserve\fR.
For security, these programs should be owned by ``root'', mode 4755,
and the directory \fB/usr/preserve\fR should be mode 755.
.IP
The programs \fB/usr/ucb/setenv\fR and \fB/usr/ucb/tset\fR must
be able to write \fB/etc/htmp\fR.  It is wise to have \fB/etc/htmp\fR
mode 644 and \fBsethome\fR and \fBttytype\fR mode 4755 to a user who
owns \fB/etc/htmp\fR (this doesn't have to be ``root'', but it can).
.IP 9.
Initialize the \fB/etc/ttytype\fR data base with the types of the terminals
on your system.  The file contains one line per terminal.  On version
6, each line has the (one character) terminal name, and then a 2 character
code.  On version 7 each line has a two character code, a space, and
then the (arbitrary length) terminal name.  See \fBmisc/ttytype.v6\fR
and \fBmisc/ttytype\fR for samples.  The codes are defined by the file
\fB/etc/termcap\fR.
.IP 10.
Initialize the Mail file \fB/usr/lib/Mail.rc\fR defining any \fIalias\fR
groups for distribution of mail you wish.  A line of the form
.DS
alias staff bill kurt eric
.DE
will cause ``Mail staff'' to send copies to \fIbill\fR, \fIkurt\fR, and
\fIeric\fR.
.SH
Software not installed by the above procedure
.PP
The modifications to the standard i/o library \fBsrc/libNS\fR,
the Berkeley network \fBsrc/net\fR, and the
\fIfinger\fR program \fBsrc/finger.c\fR are not installed by
the above procedure.
.PP
The standard I/O library modifications may require some care to make
as several slightly different versions of this library are extant.
See the \fBREAD_ME\fR file in the \fBsrc/libNS\fR directory.
.PP
If you wish to run the Berkeley network, read the material in the
\fBsrc/net\fR directory.  The network is not hard to set up, but
this will require a bit of preparation.
.PP
The \fIfinger\fR program requires preparation of some data bases,
and perhaps modifications to the \fIlogin\fR program as well as to
\fIfinger\fR itself to work.  See the comments at the beginning
of the program \fBsrc/finger.c\fR and its manual page for details.
.SH
Problems you may encounter (Version 6 only)
.IP 1.
Recompiling the editor will overflow the standard compiler symbol table.
See \fBupgrade/c\fR for instructions on a trivial change to make a C compiler
with a bigger symbol table, which you can make available via the \fB\-t0\fR
flag to \fBcc\fR.  Some scripts on the tape also reference a \fB\-t1\fR version
of the C compiler, which puts \fIswitch\fR statement code out as instructions
rather than as data.  This makes for programs with larger text spaces but
smaller per-user data.  See \fBupgrade/c\fR for the C compiler change which
implements this.
.IP 2.
If you use the binaries on the tape, some will print times in Pacific time.
They will work in your time zone if you recompile them.
.IP 3.
\fICsh\fR uses an \fIaccess\fR system call which is not part of a
bare version 6 system.  Its manual page and C interface are in the
directory \fBmisc\fR, as well as a file \fBaccess.sys\fR containing information on how
to add it to your system.  The \fIaccess\fR call is in later version 6
and version 7 systems.
.IP 4.
If you don't get mail in the file ``.mail'' in your login
directory you'll have to finagle the \fIMail\fR
program to know where you do.  Look at its \fBlocal.c\fR and \fBlocal.h\fR
files.  The \fIfrom\fR program in \fBupgrade/src\fR
will also have to be changed.
.IP 5.
If you have changed the \fItimes\fR system call as per 50
changes, returning long integers for \fIproc_user_time\fR and
\fIproc_system_time\fR, the supplied \fIcsh\fR binary will
dump when it calls \fItimes\fR.  You'll have to change some declarations
in the shell and recompile.
.SH
Maintenance
.PP
.BP /etc/termcap
If you get have or get terminals which aren't described
in this data base, you will have to add entries.
The manual page for \fItermcap\fR explains how to write new entries.
.BP /etc/ttytype
This file tells the types of hardwired ports and which
lines are dialups, and is used with \fItset\fR.  It must
be edited when the system configuration changes.
.BP /usr/preserve
Editor temporaries are saved here after a system crash,
when /usr/lib/ex2.0preserve is run out of /etc/rc.
If no one cleans this directory out, it can get very
large.  You can periodically run a find command of the form
.DS
find /usr/preserve \-mtime +7 \-a \-exec rm \-f {} \e;
.DE
to clean out old junk.  It is usefully run by the daemon \fIcron\fR.
.BP /usr/msgs
Must be cleaned out periodically (every few months).
