.ce
.LG
.B "Bug fixes and changes"
.SM
.sp
.LP
.LP
.LG
.ce
Section 1
.SM
.sp
.PP
.de BP
.IP \fB\\$1\\fR 11n
..
.BP adb
Support has been added for interpreting kernel data structures
on a running system and in post mortem crash dumps created
by savecore.  A
.B \-k
option causes adb to map addresses
according to the system and current process page tables.
A new command, $p, can be used to switch between process contexts.
Many scripts are available for symbolically displaying
kernel data structures, searching for a process' context
by process ID, etc.  A new document, ``Using ADB to
Debug the UNIX Kernel'', supplies hints in the use of adb
with system crash dumps.
.BP addbib
Is a new utility for creating and extending bibliographic
data bases for use with refer.
.BP apply
Is a new program which may be used to apply a command
to a set of arguments.
.BP ar
Has a new key, `o', for preserving a file's modification 
time when it is extracted from an archive.
.BP cc
Supports the additional symbol information used by dbx.
The old symbol information, used by the defunct sdb debugger,
is available by specifying the
.B \-go
flag.  A new flag, \fB\-pg\fP,
creates executable programs which collect profiling information
to be interpreted by the new gprof program.  A bug in the C
preprocessor, which caused line numbers to be incorrect
for macros with formal parameters with embedded newlines has
been fixed.  The C preprocessor now properly handles hexadecimal
constants in ``#if'' constructs and checks for missing ``#endif''
statements.
.BP chfn
Now works interactively in changing a user's information
field in the password file.
.BP chgrp
Is now in section 1 and may be executed by anyone.  Users
other than the super-user may change
group ownership of a file they own to any group in
their group access list.
.BP cp
Now has a
.B \-r
flag to copy recursively down a file system tree.
.BP csh
A bug which caused backquoted commands to wedge
the terminal when interrupted has been fixed.  Job identifiers
are now globbed.  A bug which caused the ``wait'' command
to uninteruptible in certain cases has been fixed.  History
may now be saved and restored across terminal sessions with
the \fIsavehist\fP variable.  The newgrp command has been
deleted due to the new group facilities.
.BP ctags
Now handles C \fBtypedefs\fP.
.BP cu
Exists only in the form of a ``compatible front-end'' to
the new tip program.
.BP dbx
Is a new symbolic debugger replacing sdb.  Dbx handles C
and Fortran programs.
.BP delivermail
.br
Has been replaced by the new sendmail program.
.BP df
Understands the new file system organization and
reports all disk space totals in kilobytes.
.BP du
Now reports disk usage in kilobytes and uses the new
field in the inode structure which contains the actual
number of blocks allocated to a file to increase
accuracy of calculations.
.BP dump
Has been moved to section 8.
.BP error
Has been taught about the error message formats of troff.
.BP eyacc
A bug which caused the generated parser to not recognize valid
null statements has been fixed.
.BP f77
Has undergone major changes.
.IP
The i/o library has been extensively tested and debugged.
Sequential files are now opened at the \s-2\fBBEGINNING\fP\s0
by default; previously they were opened at the end.
.IP
Compilation of data statements has been substantially sped up.
Significant new optimization is optionally available (this
is still a bit buggy and should be used with caution).
Even without optimization, however, single precision computations
execute much faster.
.IP
The new debugger, dbx, has replaced sdb for debugging Fortran
programs; sdb is no longer supported.
.IP
Files with ``.F'' suffixes are preprocessed by the C preprocessor. 
This allows C-style ``#include'' and ``#define'' constructs to be used.
The compiler has been modified to print error messages with sensible
line numbers.  Make also understands the ``.F'' suffix.  Note that
when using the C preprocessor, the 72 column convention is not followed.
.IP
The
.B \-I
option for specifying short integers has been changed
to \fB\-i\fP.  The
.B \-I
option is now used to specify directory search paths
for ``#include'' statements.  A
.B \-pg
option for creating executable
images which collect profiling information for gprof has been added.
.BP fed
Is a font editor of dubious value.
.BP file
Now understands symbolic links.
.BP find
Has a new
.B \-type
value, `l', for finding symbolic links.
.BP fp
Is a new compiler/interpreter for the
Functional Programming language.  A supporting document is
present in Volume 2C of the UNIX Programmer's Manual.
.BP fpr
Is a new program for printing Fortran files with embedded
Fortran carriage controls.
.BP fsplit
Is a new program for splitting a multi-function Fortran file
into individual files.
.BP ftp
Is a new program which supports the ARPA standard File Transfer
Protocol.
.BP gcore
Is a new program which creates a core dump of a running process.
.BP gprof
Is a new profiling tool which displays execution time for
the dynamic call graph of a program.  Gprof works on C, Fortran,
and Pascal programs compiled with the
.B \-pg
option.  Gprof may
also be used in creating a call graph profile for the operating
system.  A supporting document, ``gprof: A Call Graph Execution
Profiler" is included in Volume 2C of the UNIX Programmer's
Manual.
.BP groups
Is a new program which displays a user's group access list.
.BP hostid
Is a new program which displays the system's unique identifier
as returned by the new gethostid system call.  The super-user
uses this program to set the host identifier at boot time.
.BP hostname
Is a new program which displays the system's name as returned
by the new gethostname system call.  The super-user uses this
program to set the host name at boot time.
.BP indent
Is a new program for formatting C program source.
.BP install
Is a shell script used in installing software.
.BP iostat
Now reports kilobytes per second transferred for each
disk.  This is useful as the unit of information transferred
is no longer a constant one kilobytes.
.BP last
Now displays the remote host from which a user logged in
(when accessing a machine across a network).  The pseudo
user ``ftp'' may be specified to find out information about
FTP file transfer sessions.
.BP lastcomm
Now displays flags for each command indicating if the program
dumped core, used PDP-11 mode, executed with a set-user-ID,
or was created as the result of a fork (with no following exec).
.BP learn
Now has lessons for vi (this is user contributed software
which is not part of the standard system).
.BP lint
Has a new
.B \-C
flag
for creating lint libraries from C source code.  Has improved
type checking on static variables.
.BP lisp
Has been ported to several 68000 UNIX systems, the relevant
code is included in the distribution.  A new vector data
type and a form of ``closure'' have been added.
.BP ln
Has a new flag, \fB\-s\fP, for creating symbolic links.
.BP login
Has been extensively modified for use with the rlogind
and telnetd network servers.
.BP lpq
Is totally new, see lpr.
.BP lpr
And its related programs are totally new.  The line printer
system supports multiple printers of many different characteristics.
A master data base, /etc/printcap, describes both local printers
and printers accessable across a network.  A document
describing the line printer system is now part of Volume 2C
of the UNIX Programmer's Manual.
.BP lprm
Is totally new, see lpr.
.BP ls
Has been rewritten for the new directory format.  It understands
symbolic links and uses the new inode field which contains the
actual number of blocks allocated to a file when the 
.B \-s
flag is supplied.  Many rarely used options have been deleted.
.BP m4
A bug which caused m4 to dump core when keywords were 
undefined then redefined has been fixed.
.BP Mail
Now supports mail folders in the style of the Rand MH system.
Has been reworked to cooperate with sendmail in understanding
the new mail address formats.  Allows users to defined message
header fields which are not be displayed when a messages is
viewed.  Many other changes are described in a revised
version of the user manual.
.BP make
Understands not to unlink directories when interrupted.
Understands the new ``.F'' suffix for Fortran source files
which are to be run through the C preprocessor.  Has a
new predefined macro MFLAGS which contains the flags
supplied to make on the command line (useful in creating
hierarchies of makefiles).
.BP mkdir
Now uses the mkdir system call to run faster.
.BP mt
Has a new command, status, which shows the current state
of a tape drive.
.BP mv
Has been rewritten to use the new rename system call.  As
a result, multiple directories may now be moved in a single
command, the restrictions on having ``..'' in a pathname
are no longer present, and everything runs faster.
.BP net
And all related Berknet programs are no longer part
of the standard distribution.  These programs live on
in /usr/src/old for those who can not do without them.
.BP netstat
Is a new program which displays network statistics and
active connections.
.BP oldcsh
No longer exists.
.BP od
Has gobs of new formats options.
.BP pagesize
Is a new program which prints the system page size for use
in constructing portable shell scripts.
.BP passwd
Now reliably interlocks with chsh, chfn, and vipw, in
guarding against concurrent updates to the password file.
.BP pc/pi
\fBFor\fP loops are now done according to the standard.
Files may now be dynamically allocated and disposed.
Records and variant records are now aligned to correspond to 
C structures and unions (this was falsely claimed before).
Several obscure bugs involving formal routines have been
fixed.  Three new library routines support random access
file i/o, see /usr/include/pascal for details.
.BP pc (only)
\fBFor\fP loop variables and \fBwith\fP
pointers are now allocated to registers.
Separate compilation type checking can now be done without reference
to the source file; this permits movement (including distribution)
of .o files and creation of libraries.
Display entries are saved only when needed (a speed optimization).
.BP pdx
Is a new debugger for use with pi.  Pdx is invoked automatically
by the interpreter if a run-time error is encountered. 
Future work is planned to extend the new
dbx debugger to understand code generated by
the Pascal compiler pc.
.BP ps
Has been changed to work with the new kernel and is no longer
dependent on system page size.  All process segment sizes
are now shown in kilobytes.  Understands that the old
``using new signal facilities'' bit in the process structure
now means ``using old 4.1BSD signal facilities''.
.BP pwd
Now simply calls the \fIgetwd\fP\|(3) routine.
.BP rcp
Is a new program for copying files across a network.
The complete syntax of cp is supported, including recursive
directory copying.
.BP refer
Has had many bugs fixed in it and the associated \-ms macro
package support made to work.
.BP reset
Now resets all the special characters to the system defaults
specified in the include file <sys/ttychars.h>.
.BP rlogin
Is a new program for logging in to a machine across a
network.  Rlogin uses the files /etc/hosts.equiv and .rhosts
in the users login directory to allow logins to be performed
without a password.  Rlogin supports proper handling of ^S/^Q
and flushing of output when an interrupt is typed at the 
terminal.  Its `~' escape sequences are reminiscent of the
old cu program (as it is based on the same source code).
.BP rmdir
Now uses the rmdir system call to run more efficiently and
not require root privileges.  Unfortunately, this means
arguments which end in one or more ``/'' characters are no
longer legal.
.BP roffbib
Is a new program for running off bibiliographic databases.
.BP rsh
Is a new program which supports remote command execution
across a network.
.BP ruptime
Is a new program which displays system status information
for clusters of machines attached to a local area network.
.BP rwho
Is a new program which displays users logged in on clusters
of machines attached to a local area network.
.BP script
Has been rewritten to use pseudo-terminals.  This allows
the C shell job control facilities (among other things)
to be used while scripting.  A side effect of this change
is that scripts now contain everything typed at a terminal.
.BP sdb
Has been replaced by dbx; it still lives on in /usr/src/old
for those with a personal attachment.
.BP sendbug
Is a new command for submitting bug reports on 4.2BSD in
a standard format suitable for automatic filing by the
bugfiler program.
.BP sh
No longer has a newgrp command due to the new groups facilities.
.BP sortbib
Is a new command for sorting bibliographic databases.
.BP strip
Has been made blindingly fast by using the new truncate
system call (thereby eliminating the old method of copying
the file).
.BP stty
The default system erase, kill, and interrupt characters have
been made the DEC standard values of DEL (`^?'), `^U', and
\&`^C'.  This is not expected to gain much popularity, but was
done in the interest of compatibility with many other standard
operating systems.
.BP su
Has been changed to do a ``full login'' when starting up
the subshell.  A new flag, \fB\-f\fP, does a ``fast'' su for when
a system is heavily loaded.  Extra arguments supplied to
su are now treated as a command line and executed directly
instead of creating an interactive shell.
.BP sysline
Is a new program for maintaining system status information
on terminals which support a ``status line''; a poor man's
alternative to a window manager (or emacs).
.BP tail
Has a larger buffer so that ``tail \-r'' and similar
show more.
.BP talk
Is a new program which provides a screen-oriented write
facility.  Users may be ``talked to''across a network,
though satellite response times have indicated overseas
conversations are still best done by phone.  Can be very
obnoxious when engaged in important work.
.BP tar
Now allocates its internal buffers dynamically so that the
block size can be specified to be very large for streaming
tape drives.  Also, now avoids many core-core copy operations.
Has a new
.B \-C
option for forcing chdir operations in the middle
of operation (thereby allowing multiple disjoint subtrees to
be easily placed in a single file, each with short relative
pathnames).  Has a new flag, `B', for forcing 20 block records
to be read and written; useful in joining two tar commands
with a remote
shell to transfer large amounts of data across a network.
.BP telnet
Is a new program which supports the ARPA standard Telnet
protocol.
.BP tip
Replaces cu as the standard mechanism for connecting to 
machines across a phone line or through a hardwired connection.  Tip
uses a database of system descriptions, supports many different
auto-call units, and understands many nuances required to talk
to non-UNIX systems.  Files may be transferred
to and from non-UNIX systems in a simple fashion.
.BP ul
A bug which sometimes caused an extra blank line to
be printed after reaching end of file has been fixed.
.BP uucp
And related programs have been extensively enhanced
to support many different auto-call units and multiple spooling
directories (among other things).  A large number of bugs
and performance enhancements have been made.
.BP uusnap
Is a new program which gives a snap-shot of the uucp
spooling area.
.BP vfontinfo
Is a program used to inspect and print information about
fonts.
.BP vgrind
Now uses a regular expression language
to describe formatting.  A
.B \-f
flag forces vgrind to act as
a filter, generating output suitable for inclusion in troff
and/or nroff documents.  Language descriptions exist for C, Pascal, 
Model, C shell, Bourne shell, Ratfor, and Icon programs.
.BP vi
A bug which caused the ^B command to place the cursor on
the wrong line has been fixed.  A bug which caused vi to
believe a file had been modified when an i/o error occurred
has been fixed.  A bug which allowed ``hardtabs'' to be
set to 0 causing a divide by zero fault has been fixed.
.BP vlp
Is a new program for pretty printing Lisp programs.
.BP vmstat
Has had one new piece of information added to
.B \-s
summary, the number of fast page reclaims performed.
The fields related to paging activity are now all
given in kilobytes.
.BP vpr
And associated programs for spooling and printing files
on Varian and Versatec printers are now shell scripts
which use the new line printer support.
.BP vwidth
Is a new program for making troff width tables for a font.
.BP wc
Is once again identical to the version 7 program.  That is,
the \fB\-v\fP, \fB\-t\fP, \fB\-b\fP, \fB\-s\fP, and \fB\-u\fP
flags have been deleted.
.BP whereis
Understands the new directory organization for the source
code.
.BP which
Now understands how to handle aliases.
.BP who
Now displays the remote machine from which a user is
logged in.
