.\" $Revision: 1.13 $
.\" Uses the -ms macro package; e.g., "nroff -ms Install.ms | col >Install.out"
.\" UX - UNIX macro
.nr UX 0
.de UX
.ie \\n(UX \s-1UNIX\s0\\$1
.el \{\
\s-1UNIX\s0\\$1\(dg
.FS
\(dg \s-1UNIX\s0 is a registered trademark of Unix Systems Laboratories.
.FE
.nr UX 1
.\}
..
.DA "February 14, 1992"
.TL
Installing InterNetNews
.AU
Rich $alz
.AI
Open Software Foundation
11 Cambridge Center
Cambridge, MA  02142

\fIOrganization given for identification only;\fP
\fIplease send electronic mail to <rsalz@uunet.uu.net>\fP
.AB
This document discusses how to install and set up InterNetNews.
You should be familiar with Usenet and networks; the first section gives
references to documentation for these topics, and the last appendix gives
a Usenet overview for novices.
.PP
This document also describes what many of the programs do and how they
should be used.
Even if you are a world-class expert at building and maintaining
public software, you should probably read this.
.PP
.de R$
This is revision \\$3, dated \\$4.
..
.R$ $Id: Install.ms.1,v 1.13 1993/03/19 17:49:59 rsalz Exp $
.AE
.NH 1
Things You Should Know Before You Do Anything
.PP
InterNetNews is abbreviated \fIINN\fP, which is pronounced
as the three letters, \fIeye en en\fP.
It is a Usenet transport and expiration system for larger
.UX
systems where NNTP is used for most Usenet traffic.
.PP
This document is not a tutorial on Usenet.
If you do not have much Usenet experience, you should read
\fIUsing UUCP and Usenet\fP, ISBN 0-937175-10-2.
You might also find it useful to read \fIManaging UUCP and Usenet\fP
(get the most recent edition available), ISBN 0-937175-48-X.
Both books are published by O'Reilly & Associates; send inquiries to
to <nuts@ora.com>.
.PP
You should know BSD-derived TCP/IP \(em at least be comfortable with host
names and dotted-quad addresses.
If you have installation problems, you should know about
.UX -domain
stream and datagram sockets and the like.
In addition to any documentation available from your vendor, you might
find it useful to read the two IPC tutorials in \fI
.UX
Programmer's Manual:  Supplementary Documents 1\fP.
Copies can be purchased from the Usenix Association; send inquiries to
<office@usenix.org>.
.PP
There are two RFCs that are important to InterNetNews.
RFC 1036 describes the format of Usenet articles.
It is incomplete and has some errors, but it is the only formal
document available.
RFC 977 defines NNTP, the Network News Transfer Protocol.
RFCs are available from several places, including anonymous FTP
to nnsc.nsf.net, where they can be found in the directory \fIrfc\fP.
Both RFCs are currently being revised.
The 1036 revision is most likely going to be a ``tightening-up''; since
INN already has a strict interpretation of the RFC, this revision will
probably not affect InterNetNews very much.
The 977 revision is adding new features and facilities, and while INN
will not provide all of them, they will have some impact.
.PP
InterNetNews does things differently from other news software.
The most common Usenet systems for
.UX
are B2.11 and C News.
Both of them require a separate NNTP implementation.
The one everyone uses is called ``NNTP.''
Because this is confusing (they don't call \fIsendmail\fP ``SMTP''), I will
refer to it as the ``reference implementation.''
You generally do not need to know anything about these other systems,
but if you are curious, the official sites are as follows:
.DS
.ta \w'B2.11 News   'u +\w'ftp.cs.toronto.edu    'u
Package	Host	Directory
C\ News	ftp.cs.toronto.edu	pub/c-news
B2.11	ftp.uu.net	news/bnews-2.11
nntp	lib.tmc.edu	public
.DE
You might find the files \fIdoc/biblio\fP, \fIdoc/problems\fP, and
\fIdoc/rfcerrata\fP in the C News distribution worthwhile reading.
The first is a bibliography, the second discusses known C News porting
problems (see the DBZ sections in particular, and ignore most of the shell
comments), while the third lists some technical and philosophical errors
in RFC 1036.
.PP
The commands below assume that \fI$inn\fP is an abbreviation for the
top of the InterNetNews source tree.
.PP
INN could not have been written without access to the freely-redistributable
sources of B2.11, C News, and NNTP.
In particular, I want to thank Rick Adams; Geoff Collyer and Henry Spencer;
and Stan Barber, Erik Fair, Brian Kantor, and Phil Lapsley.
The financial support of UUNET Technologies is also greatly appreciated.
The beta-test sites gave invaluable feedback.
.NH 1
If You Are Impatient
.PP
If you don't want to follow these directions, do the following:
.DS
cd $inn/config
cp config.dist config.data
chmod 644 config.data
vi config.data (or emacs, or whatever)
cd ..
make world install
.DE
Then go read the appendixes.
If you have any problems, read the rest of this document.
.NH 1
Distribution Roadmap
.PP
The INN sources are divided into the following directories:
.IP \fIfrontends\fP 15
Programs to feed articles to the central server and control it.
.IP \fIinnd\fP 15
The central NNTP server.
It accepts incoming connections, receives articles, and arranges for
them to be sent to downstream newsfeeds.
.IP \fIbackends\fP 15
Programs to transmit articles to other sites.
.IP \fIexpire\fP 15
Programs to purge the article files and history database.
.IP \fInnrpd\fP 15
An NNTP server for on-campus clients that do newsreading (as opposed to
bulk article transfer).
.IP \fIlib\fP 15
Library routines used by all the above.
.IP \fIinclude\fP 15
Header files used by all the above.
.PP
The distribution also includes these directories:
.IP \fIsamples\fP 15
Prototype scripts and configuration files that might have to be edited
before they are installed.
.IP \fIsite\fP 15
A place to store edited copies of the files in the \fIsamples\fP directory.
.IP \fIdoc\fP 15
Manual pages for all the above.
.IP \fIconfig\fP 15
Tools to configure the release for your site.
.PP
Finally, there are a handful of files in the top-level directory:
.IP \fIREADME\fP 15
A basic introduction.
.IP \fICOPYRIGHT\fP 15
The distribution copyright.
InterNetNews is freely redistributable, provided proper credit is given.
.IP \fIMANIFEST\fP 15
A one-line description of every file in the distribution.
.IP \fIBUILD\fP 15
An interactive script to configure, build, and install INN.
.IP \fImakedirs.sh\fP 15
A script to build INN's directories.
As long as you have write permission to install the programs, this is
the only part of the installation that needs to be done as root.
.IP \fIMakefile\fP 15
Rules to call the other Makefiles and make distributions.
.IP \fIInstall.ms\fP 15
This document.
It requires the ``\-ms'' nroff/troff macro package.
.IP \fIMakeLib\fP 15
Script to build a directory with a replacement of the reference
implementation's ``clientlib'' routines needed by remote \fIrn\fP.
.IP \fIMakeInews\fP 15
Script to build an \fIinews\fP distribution directory.
.IP \fIMakeRnews\fP 15
Script to build an \fIrnews\fP distribution directory.
.IP \fIsedf.xxx\fP 15
Various \fIsed\fP scripts to filter the output of \fIlint\fP.
.NH 1
Building the System
.PP
INN is built in steps.
First, the \fIsubst\fP program is built.
Next, a configuration file containing key/value pairs is created.
\fISubst\fP reads this file and uses it to edit a specific set of files
in the INN distribution.
(Most of the files that get modified are Makefiles or header files.)
The library is then built; \fIlint\fP is usually a good way to see if
some of the basic configuration parameters are set up right.
The next step is to compile (and lint) all the programs.
The programs are then installed, and the INN data files are set up.
.PP
The configuration process is deliberately not interactive.
Configure scripts like the one in \fIrn\fP are fun to watch, but they
spend too much effort on the wrong job, like whether \fIgrep\fP returns
an exit status.
It is also difficult to change one parameter and rebuild the software.
(C News has this same problem.)
.PP
INN's method also has its flaws.
Because almost all configuration data is in one header file, changing
almost anything will force everything to be recompiled.
.NH 2
Building subst
.PP
INN uses the C News \fIsubst\fP program to automate the configuration.
\fISubst\fP is a very clever way of safely editing a file under the control
of a configuration file.
For more details, see the documentation in \fIdoc/subst.1\fP.
Thanks to Henry Spencer and Geoff Collyer for their permission to use and
redistribute \fIsubst\fP.
.PP
We will use \fIconfig.dist\fP as the configuration file to test the
version of \fIsubst\fP that you build.
.\"Using this file on your INN distribution should result in no changes to
.\"any files.
.\"If, while doing the steps below, you see any ``updated'' messages, then
.\"something is wrong with your distribution.
(You can always replace your config file with the distribution file and
do another \fImake\fP to restore the original versions.)
.PP
The C News \fIsubst\fP program is a shell script that uses \fIsed\fP to
do the editing.
The INN configuration file is too large for some versions of \fIsed\fP.
The first step is to see if your \fIsed\fP will work.
To do this, type the following:
.DS
cd $inn/config
cp config.dist config.data
make sedtest
.DE
If you get any error messages from \fIsed\fP such as ``too much command
text'' (or if it dumps core) you have two choices.
(You should also complain to your vendor.)
One choice is to use another version of \fIsed\fP, such as the one
distributed by the Free Software Foundation.
If you do this, edit \fIconfig/Makefile\fP and change the line that
defines the SED variable.
If you want to use the C News script, then do the following:
.DS
cd $inn/config
make sh
.DE
.PP
The other choice is to use the C version of \fIsubst\fP.
You might want to do this anyway, since it can be much faster.
To do this, type the following:
.DS
cd $inn/config
cp config.dist config.data
make c quiet
.DE
If you get any compilation errors, you will have to edit the file
\fIconfig/subst.c\fP.
If you are using an early version of AFS, you will have edit the file
to enable the USE_RENAME macro.
If you have to make any other changes, please let me know.
.PP
Since \fIsubst\fP changes source files, you might want to make a
backup copy of all the files that will be modified.
You can do this by typing ``make backup'' in the \fIconfig\fP
directory.
This will create a local tar file that contains all the files that will
be modified into it.
Doing ``make restore'' will unpacks the tar file.
(Since \fIsubst\fP makes its changes safely, this step is optional.)
.NH 2
Editing config.data
.PP
Once you have \fIsubst\fP working, the next step is to set up your
configuration parameters.
This is the hardest part of installing INN.
\fIDon't panic!\fP
There are many configuration parameters, but it should be very easy
for you to determine the answer for most of them.
To do this, you should copy \fIconfig/config.dist\fP, the distribution
master, to \fIconfig/config.data\fP, your local copy.
INN is distributed to compile and run under SunOS4.1 (without using
<unistd.h> and other POSIX facilities) by default.
.PP
The configuration file is divided into the following sections:
.DS
Make config parameters
Logging levels         
Ownerships and file modes
C library differences
C library omissions
Miscellaneous config data
Paths to common programs
Paths related to the spool directory
Execution paths for innd and rnews
Sockets created by innd or clients
Log and config files
Innwatch configuration
.DE
You should have a copy of \fIconfig.data\fP nearby as you read the next
few sections.
It is probably a good idea to write down your changes on paper before
you edit the file.
.PP
The format of the file is very strict.
A line starting with a poundsign is a comment line.
All other lines must be in this format:
.DS
parameter \fI<one-or-more-tabs>\fP value
.DE
If there is no ``value'' the ``<one-or-more-tabs>'' is still required.
Do not put quote marks around the values \(em if you do, you will usually
get a syntax error while compiling the system.
The discussion below uses quotes only to show where the values start and end.
.NH 3
Make config parameters
.PP
This section is used primarily to identify the path to your C compiler,
and what extra libraries or command-line switches are needed.
For example, you could put \fIgcc \-Wall\fP on the \fICC\fP line.
If you need extra \fI\-I\fP flags put them on the \fIDEFS\fP line.
INN uses the \fIregister\fP declaration a great deal.
If your compiler is very good, you might want to add \fI\-Dregister=\fP
to the \fIDEFS\fP line so that INN's declarations are ignored.
.PP
The DBZ package can be compiled so that the database is memory-mapped.
If you want to do this and have the \fImmap\fP system call, then add
``\-DMMAP'' to the \fIDBZCFLAGS\fP parameter.
.PP
If you need to link in other libraries (e.g., \fI\-lnet\fP) put them
on the \fILIBS\fP line.
.\".PP
.\"INN can be built with Conor Cahill's ``debug_malloc'' package.
.\"Add ``-D_DEBUG_MALLOC_INC\ -Ixxx'' to the \fIDEFS\fP line, where \fIxxx\fP
.\"is the path to the debug package.
.\"You will also have to add the appropriate ``libdbmalloc'' value to the
.\"\fILIBS\fP parameter.
.PP
The Makefiles usually filter all \fIlint\fP output through a \fIsed\fP
script.
If you are very paranoid, set \fILINTFILTER\fP to \fIcat\fP.
If your lint output is in the broken multi-line format:
.DS
value type declared inconsistently
    exit        llib-lc(297) :: test.c(7)
function returns value which is always ignored
    printf
.DE
Then set \fILINTFILTER\fP to be the ``sedf.sysv'' line.
.PP
The \fIlib\fP directory also builds a \fIlint\fP library, so that
you can make sure the other programs are properly using the library
routines.
The \fILINTLIBSTYLE\fP parameter (used in \fIlib/Makefile\fP and
\fIlib/makellib.sh\fP) controls how the \fIlint\fP library is built.
If your \fIlint\fP understands the ``\-C'' flag, then set it to ``BSD''.
If you need the ``\-o'' flag to build a library, then set it to ``SYSV''.
If neither of these work, you can set it to ``NONE''; this will just
create an empty file so that the other Makefiles don't break.
If you come up with a fourth alternative, let me know.
.PP
Unfortunately, on some systems \fIlint\fP is all but useless, so complain
to your vendor and take the output with a grain of salt.
You might get some warnings about ``struct\ _DDHANDLE'' being undefined.
You can ignore them and ask your vendor to support the BSD ``\-z'' lint flag.
If you set \fIHAVE_UNISTD\fP to ``DO'' then you might get warnings
about prototype mismatches for various functions declared in
\fIinclude/clibrary.h\fP.
You can ignore them or remove the lines from the INN header file.
.PP
The \fIMANPAGESTYLE\fP parameter (used in \fIdoc/Makefile\fP and
\fIdoc/putman.sh\fP) controls how manual pages are installed into your
public directory while the \fIMANx\fP parameters specify the directories
where they get installed.
If you do not want to install any manpages, set \fIMANPAGESTYLE\fP to
\fINONE\fP.
.NH 3
Logging levels
.PP
INN uses the modern \fIsyslog\fP that separates messages into both
levels and categories.
Look in your \fI<syslog.h>\fP header file for a ``LOG_NEWS'' macro,
and check your \fIsyslog\fP(3) manpage to make sure that \fIopenlog\fP
takes three arguments.
If it doesn't, then you will have to use the library routine and server
provided in the \fIsyslog\fP directory.
This is described later.
.PP
The different levels that are described in the \fIsyslog\fP(3) manpage
are confusing, so INN uses its own names for the four levels it uses:
.DS
.nf
.ta \w'L_NOTICE  'u
L_FATAL	Fatal error, about to exit
L_ERROR	Error that might require attention
L_NOTICE	Informational notice, no action needed
L_DEBUG	Protocol tracing or other debugging messages
.fi
.DE
Depending on how your \fIsyslog.conf\fP(5) file is set up, you might want
to change the \fIL_xxx\fP parameters in this section.
.PP
The \fIscanlogs\fP script assumes that the first three categories above
are each directed into separate files.
See \fIdoc/newslog.5\fP, \fIdoc/newslog.8\fP, and \fIsyslog/syslog.conf\fP
for details.
Logging is also described in more detail later.
.NH 3
Ownerships and file modes
.PP
The NNTP server needs to open the NNTP port; it is port number 119,
which requires root access.
This is the only part of INN that needs this privilege: all other programs
can run under the distinct user and group id specified by the \fINEWSUSER\fP
and \fINEWSGROUP\fP parameters.
Most news administration tasks must be done as user \fINEWSUSER\fP (see the
explanation of \fIctlinnd\fP below).
In addition, \fIinews\fP will only let the \fINEWSUSER\fP user or members of
the \fINEWSGROUP\fP group post control messages other than cancel.
.PP
Some INN scripts (primarily the control message scripts and the
daily maintenance script) need to send email to the news maintainer.
The \fINEWSMASTER\fP parameter specifies the right address.
This is most often the login name of the account which has
\fINEWSUSER\fP as its user id; use an alias to forward it to the right people.
.PP
Some Usenet sites still use the Path header line to generate their email reply
messages.
Using the Path has never been guaranteed to work, and INN tries to help
stop this practice by refusing to generate valid Path addresses.
The \fIPATHMASTER\fP parameter specifies what \fIinews\fP should put
at the tail end of the Path line.
If your \fINEWSMASTER\fP mailbox is getting cluttered, then you might
want to change this to be an alias that rejects the message or drops it
into the bit-bucket.
The default value is ``not-for-mail'' which usually results in bounced
email.
.PP
The \fIxxx_MODE\fP parameters specify the permissions for articles and
directories created within the spool area, and the active file, all of
which are owned by user id \fINEWSUSER\fP.
.NH 3
C library differences
.PP
Editing the parameters in this section will require you to look
around at the files in your \fI/usr/include\fP directory.
.PP
The \fISIZE_T\fP parameter is the datatype of the ``size'' parameters
in subroutine calls like \fImemchr\fP and \fIfread\fP.
The \fILOCK_STYLE\fP parameter specifies how file-locking should be done.
\fIInnxmit\fP is the only program that locks files; if you use the provided
scripts, this isn't even necessary, so you can set this to ``NONE'' if you
have any compile problems.
.PP
The \fIDIR_STYLE\fP parameter specifies what is returned by your
\fIreaddir\fP(3) routine.
This will be either a ``struct\ direct'' or a ``struct\ dirent'';
set the parameter to ``DIRECT'' or ``DIRENT'' as appropriate.
.PP
If you do not have
.UX -domain
sockets, set \fIHAVE_UNIX_DOMAIN\fP to ``DONT.''
This means that INN will use a named pipe for the communication
between \fIinnd\fP and \fIctlinnd\fP.
It also means that there will be no local ``private'' port for \fIrnews\fP
to use; this should not cause any problems, although it makes it
easier for anyone to use \fIrnews\fP and post fake news articles.
(You might also have to modify the \fIsyslog\fP routines; see the end of
the file \fIsyslog/README\fP for details on this.)
.PP
INN needs to know how many descriptors are available to use for files
and sockets.
There are several ways to get this number; the \fIFDCOUNT_STYLE\fP
parameter specifies which method to use.
On most systems, the \fIgetdtablesize\fP routine will do this, so
leave the default of ``GETDTAB.''
On other systems you need to use the \fIgetrlimit\fP, \fIsysconf\fP or
\fIulimit\fP routine, so set the parameter to ``GETRLIMIT'', ``SYSCONF'',
or ``ULIMIT'', respectively.
If you do not have any of those calls then set the parameter to ``CONSTANT''
and edit the file \fIlib/getdtab.c\fP to return the right number.
To get this number, look for an \fIOPEN_MAX\fP constant in your system header
files, or write a program that repeatedly opens \fI/dev/null\fP until
it gets an error.
.PP
The last few parameters in this section, \fIxxxVAL\fP, are used primarily
to keep \fIlint\fP quiet.
These functions are declared in \fIinclude/clibrary.h\fP, and the return
values are pretty much always ignored.
You can usually determine what these values should be by examining
your manpages or your \fIlint\fP libraries.
.NH 3
C library omissions
.PP
INN uses library routines that might not be present in all
.UX
systems, although they should be.
The \fIlib\fP directory provides versions of some of these routines,
including copies of the freely-redistributable BSD string routines.
The \fIMISSING_SRC\fP and \fIMISSING_MAN\fP parameters can be set to
list those routines that are missing from your C library.
If you don't have \fIstrcasecmp\fP and \fIstrncasecmp\fP then you will
need the \fIstrcasecmp\fP module built into your INN library.
Add the ``.c'' and ``.o'' names to \fIMISSING_SRC\fP and \fIMISSING_OBJ\fP,
respectively.
.PP
The following routines are all found in the file of the same name.
If they are missing from your system, add them the same way:
.DS
.ta 1.5i +1.5i
memchr	strchr	getopt
memcmp	strrchr	mkfifo
memcpy	strspn	strerror
memset	strtok
.DE
.PP
If you are using version 1 of the GNU C compiler on a Sparc running SunOS,
you should add \fIinet_ntoa\fP as a missing function.
This is because the first version of \fIgcc\fP didn't properly pass
structures into routines compiled with the Sun C compiler.
.PP
If you have an older version of \fIsyslog\fP add \fIsyslog.c\fP and
\fIsyslog.o\fP to the appropriate parameters.
.PP
Pyramid machines running OSx have fast assembly-language versions of the
string routines in the ATT library.
To use these routines, add ``$(OSXATTOBJ)'' to the \fIMISSING_OBJS\fP
parameter.
This will cause \fIlib/Makefile\fP to extract the object files from the ATT
library, and add them to the INN library.
.NH 3
Miscellaneous config data
.PP
All the parameters in this section become macros in the file
\fIinclude/configdata.h\fP.
You should at least look through the parameters up to \fIVERIFY_CANCELS\fP.
(If set to ``DO'', then \fIinnd\fP will ignore cancel messages unless
the From or Sender header match those of the original poster.)
In general, however, you can leave this section pretty much alone until you
have some experience running INN.
Nevertheless, here are some comments on some of the more useful parameters.
.PP
\fIInnd\fP can memory-map the \fIactive\fP file if you set
\fIACT_STYLE\fP to ``MMAP''.
On some systems, however, when a mapped file is updated its mtime is
not updated.
Apparently some versions of System V Release 4 have this problem.
This causes problems for programs like \fInnmaster\fP which look at the
\fIst_mtime\fP field of the \fIstat\fP structure in order to determine if
any new news has come in.
(\fINnmaster\fP is part of the \fInn\fP newsreading program.)
The best work-around is probably an hourly \fIcron\fP job that touches the
\fIactive\fP file.
.PP
There are a number of parameters that control the behavior of \fIrnews\fP.
If you set \fIRNEWS_SAVE_BAD\fP to ``DO'' then articles that \fIinnd\fP
rejects for reasons like bad headers will be saved in the \fI_PATH_BADNEWS\fP
directory; you will have to periodically scan this directory and clean
it up.
You can also control how \fIrnews\fP logs duplicates (those aren't saved
regardless of the value of \fIRNEWS_SAVE_BAD\fP), logging them through
\fIsyslog\fP, to a file, or not.
Note that if you set \fIRNEWS_LOG_DUPS\fP to ``FILE'', then you will want
to change \fI_PATH_RNEWS_DUP_LOG\fP, which appears later in the file.
If you receive news from several UUCP feeds, you might want to log duplicates
so that you can cut down your phone bills by optimizing your feeds.
The \fIRNEWSPROGS\fP parameter says whether or not to look in
\fI_PATH_NEWSPROGS\fP for commands named on the incoming ``#!'' line of
news batches.
You probably want to set this to ``DO''.
Make sure that the full pathname of \fIrnews\fP, \fI_PATH_RNEWS\fP,
does not conflict with the directory where your unpackers are put,
\fI_PATH_NEWSPROGS\fP.
.PP
If \fIIPADDR_LOG\fP is set to ``DO'' then the news log will report the IP
address of hosts that send articles, rather then what they put in the Path
line.
This can be useful if you run \fIinnd\fP with the ``\-a'' flag.
(If you do this, you might want to pick up ``hf.tar.Z'' via anonymous
FTP to ee.lbl.gov; it is a filter that turns IP addresses into host names.)
.PP
The \fIxxx_TIMEOUT\fP parameters control various timers within INN;
you might want to change some of these depending on your system load.
.NH 3
Paths to common programs
.PP
INN uses a few standard programs like \fI/bin/sh\fP and \fIsendmail\fP.
If you don't have \fIsendmail\fP then you must have a program that
accepts a full message \(em including headers \(em on its standard input,
and delivers it.
This program is specified by the \fI_PATH_SENDMAIL\fP parameter, and
is normally ``/usr/lib/sendmail\ -t''.
The parameter is actually a \fIsprintf\fP format string that will be
given the destination address as its only argument.
on some sites (e.g., those running MMDF) the ``\-t'' could be replaced
with a ``%s''.
.PP
INN puts most of its executables in the directory specified by the
\fI_PATH_NEWSBIN\fP parameter.
Some programs expect \fIinews\fP and \fIrnews\fP to be in certain places;
for example, UUCP usually wants \fIrnews\fP in \fI/bin\fP.
The default configuration puts these programs in only one spot; if you
need to have multiple links to the same file, you will have to do it
yourself after INN is installed.
If you have additional scripts and programs that you use to maintain your
system, you can put them in whatever directory you want.
You will probably need to add \fI_PATH_NEWSBIN\fP to the PATH of any
such scripts.
.PP
If you have an \fI/etc/rc.local\fP file you should make sure that
it invokes the script named by the \fI_PATH_NEWSBOOT\fP parameter.
On other systems (mostly System V derivatives), the system boot procedure
automatically runs all the scripts in a particular directory, such as
\fI/etc/init.2\fP.
In that case, you should pick a name like \fI/etc/init.2/S99news\fP and
have the news boot script installed there, or install it in the default
\fI/etc/rc.news\fP and make the link yourself.
.PP
The daily maintenance script, \fInews.daily\fP calls \fIscanlogs\fP
to rotate and trim log files, as well as generating summaries using
\fIegrep\fP and \fIawk\fP.
On some systems the log files are too big for these programs so you
might have to complain to your vendor and install the versions from
the Free Software Foundation.
The \fIscanlogs\fP script has a short test you can run to see if your
\fIegrep\fP will work.
.NH 3
Paths related to the spool directory
.PP
By default, all news articles are stored in directories under
\fI/usr/spool/news\fP.
Be careful if you pick a different value \(em many newsreaders know
about this directory name.
.PP
INN uses a trick (which I first saw in C News) that lets it use this same
directory to store its incoming news (spooled by \fIrnews\fP when
\fIinnd\fP is not available), and its outgoing batch files.
Since no newsgroup can ever have a dot in its name, a directory like
\fIout.going\fP can never be a newsgroup name, and it is safe to
put the news batchfiles in there.
This is used by the \fI_PATH_SPOOLNEWS\fP parameter, and the
\fI_PATH_BATCHDIR\fP parameter.
.PP
Do not make \fI_PATH_LOCKS\fP be in the same filesystem as
\fI_PATH_SPOOLNEWS\fP.
If you do this, then INN will not be able to create any lock files when
your spool directory is full.
This will probably mean that \fInews.daily\fP will not be able to run
and that it won't call \fIexpire\fP to free up disk space.
You should also put \fI_PATH_NEWSLIB\fP on a separate partition if you
can, but that is not as important because it tends to fill up less often.
.PP
If you change parameters in this section a great deal, then there is a chance
that the \fImakedirs.sh\fP script will fail because some needed
intermediate directories will not exist.
This should not be a problem, as you can just create the directories
yourself \(em make sure to set the ownership and modes right \(em and
re-run the script.
.NH 3
Execution paths for innd and rnews
.PP
All control messages (other then ``cancel'') are carried out by scripts.
Your system must be able to \fIexec\fP shell scripts directly.
All the scripts distributed with INN start with ``#!\ /bin/sh.''
.PP
The \fI_PATH_CONTROLPROGS\fP parameter specifies the directory where
these scripts exist.
Do not set this to a public directory like \fI/bin\fP because some bozo
might send out an ``rm'' control message.
.PP
The \fI_PATH_RNEWSPROGS\fP directory serves the same purpose for \fIrnews\fP
when it needs to unpack batches.
The \fIRNEWSPROGS\fP parameter specifies if the directory is really used.
.NH 3
Sockets created by innd or clients
.PP
The \fIinnd\fP server and its clients (most notably \fIctlinnd\fP)
create
.UX -domain
sockets or named pipes.
They are created inside a ``firewall'' directory that gives access permission
to a limited set of users.
For example, assume the directory is \fI/usr/local/news/innd\fP and that it
is owned by user news in group news and has mode 0770.
Using these permissions, then only members of the news group can use
\fIctlinnd\fP to create new groups because only they will be able to
send a message to the \fIinnd\fP socket.
.PP
This directory (which is specified by the \fI_PATH_INNDDIR\fP parameter)
is also used to determine the user and group id of all sub-processes spawned
by \fIinnd\fP, as well as the owner of all news articles and files.
The owner of this directory is set at installation time and specified in
the ``Ownerships and file modes'' section, above.
.NH 3
Log and config files
.PP
INN keeps its databases, and some control files their own directory,
typically named \fI/usr/local/news\fP.
Log files are kept in \fI/var/log/news\fP.
There are many parameters in this section that refer to files within
this directory.
Some sites will want to globally replace ``/usr/local/news'' with something
like ``/var/news'', and ``/usr/lib/newsbin'' with ``/var/newsbin''
.PP
There are two files that contain access passwords,
\fI_PATH_NNTPPASS\fP and \fI_PATH_NNRPACCESS\fP.
The default location for these files is in \fI/usr/local/etc\fP, so that it is
generally safe to export \fI/usr/local/news\fP (read-only is probably best).
.PP
INN programs do extensive logging, and the daily maintenance scripts
do extensive summary reports and analysis of them.
It might take you some time to learn your way around the INN logging system;
start by reading the newslog manpages in the \fIdoc\fP directory.
.NH 3
Innwatch configuration
.PP
The INN server, \fIinnd\fP, does not contain any checks to see if there
is enough free space on the disk or if the system load average is low enough
to allow article reception.
There are two reasons for this.
The first reason is philosophical:  it is a mistake to bury this kind of policy
information inside a program.
For example, you don't want to have to recompile the program just
because you moved to a different filesystem.
(Yes, this could be partially answered by moving the information to an
external config file, but any compiled rules are still likely to be
incomplete.)
The second reason is pragmatic:  there is no portable way to get standard
measurements for the information needed.
For example, C News provides three different routines to get the filesystem
statistics (with conditional compilation) while the ``get load average''
file in IDA sendmail has over 700 lines.
.PP
Rather than get tangled up in such a mess of #ifdef's, INN uses an
external program (shell script) that invokes \fIctlinnd\fP to stop and start the
server as necessary.
The program, \fIinnwatch\fP, reads the control file \fIinnwatch.ctl\fP.
\fIInnwatch\fP is documented in \fIdoc/news.daily.8\fP, while \fIinnwatch.ctl\fP
is documented in \fIdoc/innwatch.ctl.5\fP.
.PP
The parameters in this section control when the server should stop accepting
articles, and when it should start again.
You will have to examine \fIsite/innwatch.ctl\fP and probably modify it,
usually to check the amount of free space on the disks.
For example, there is a line in the file that has this fragment in it:
.DS
!!! df . | awk 'NR == 2 { print $4 }' ! ...
.DE
This is looking at the fourth field of the second line to get the amount
of freespace.
You will have to change the ``2'' and ``4'' here, and on other lines, as
appropriate for your system.
(Changing the output of \fIdf\fP seems to be one of the things vendors like
to do most; it is not worth my time to have INN keep track of all of them.)
.PP
The parameter \fIINNWATCH_SLEEPTIME\fP specifies how frequently
\fIinnwatch\fP should check the system \(em the other parameters should
be set with this in mind, eg: there needs to be enough free space on the
filesystem to last the next \fIINNWATCH_SLEEPTIME\fP seconds.
.PP
The \fIINNWATCH_xxxLOAD\fP parameters specify the load average at
which different actions should be taken.
They are integers, representing the load average multipled by 100.
For example, if you want to throttle the server when your load reaches
7.5, set \fIINNWATCH_HILOAD\fP to ``750.''
.PP
The \fIINNWATCH_xxxSPACE\fP parameters specify the minimum amount of disk
space needed for each of INN's three major filesystems.
The numbers are in ``local units,'' equivalent to whatever your \fIdf\fP
uses (512-byte units, 1K blocks, etc).
.PP
The \fIINNWATCH_SPOOLNODES\fP parameter specifies how many inodes must
be available in your spool directory.
.NH 2
Typical config.data changes
.PP
The following sections show some of the changes that need to be made to
\fIconfig.data\fP so that INN will compile.
They are only samples; ``your mileage may vary.''
.PP
Note that if you are using the first release of \fIgcc2\fP, set
\fIUSE_CHAR_CONST\fP to ``DONT''.
.PP
.DS
.UL "AIX"
.ta \w'HAVE_ST_BLKSIZE   'u
DEFS	-I../include -D_NO_PROTO -U__STR__
FORK	fork
FREEVAL	void
FUNCTYPE	int
HAVE_ST_BLKSIZE	DONT
HAVE_TM_GMTOFF	DONT
LDFLAGS	
LINTFILTER	| sed -n -f ../sedf.aix
LINTFLAGS	-wkD -b -h $(DEFS)
LINTLIBSTYLE	SYSV
LOCK_STYLE	FNCTL
MISSING_MAN	
MISSING_SRC	
NEED_TIME	DO
POINTER	void
.DE
Under AIX 3.1, you must also use the \fIsyslog\fP that comes with INN.
This is not necessary for 3.2.
Some versions also need \USE_UNION_WAIT\fP set to ``DONT''.
.PP
.DS
.UL "A/UX"
.ta \w'HAVE_ST_BLKSIZE   'u
LIBS	-lbsd
.DE
Make sure you don't use \fIgcc\fP version 1; it miscompiles the socket calls
in \fIinnd/cc.c\fP.
.PP
.DS
.UL "BSDI"
ABORTVAL	void
ALARMVAL	u_int
EXITVAL	volatile void
_EXITVAL	volatile void
FREEVAL	void
GETPIDVAL	pid_t
GID_T	gid_t
HAVE_UNISTD	DO
HAVE_VFORK	DONT
HAVE_WAITPID	DO
LSEEKVAL	off_t
MISSING_OBJ	
MISSING_SRC	
_PATH_COMPRESS	/usr/bin/compress
_PATH_EGREP	/usr/bin/egrep
_PATH_MAILCMD	/usr/bin/Mail
_PATH_SENDMAIL	/usr/sbin/sendmail -t
PID_T	pid_t
POINTER	void
QSORTVAL	void
SIZE_T	size_t
SLEEPVAL	u_int
UID_T	uid_t
USE_UNION_WAIT	DONT
VAR_STYLE	STDARGS
.DE
Change the \fISHELL\fP variable in \fIconfig/Makefile\fP and
\fIsite/Makefile\fP to point to \fI/usr/contrib/bin/bash\fP.
Edit \fIlib/Makefile\fP so that the \fIinstall\fP target does not
try to make \fI../llib-linn.ln\fP.
You must also use the GNU \fIsed\fP; the version distributed with
BSDI 0.9.4.1 enters an infinite loop processing newgroup messages.
.PP
.DS
.UL "HP-UX 8.0"
.ta \w'HAVE_ST_BLKSIZE   'u
ABORTVAL	void
ALARMVAL	unsigned int
CLX_STYLE	FCNTL
CTYPE	isXXXXX((c))
DEFS	-I../include -DHPUX
FDCOUNT_STYLE	SYSCONF
FREEVAL	void
GETPIDVAL	pid_t
GID_T	gid_t
HAVE_SETBUFFER	DONT
HAVE_ST_BLKSIZE	DONT
HAVE_TM_GMTOFF	DONT
HAVE_UNISTD	DO
HAVE_WAITPID	DO
LINTFILTER	| sed -n -f ../sedf.sysv
LINTFLAGS	-b -h $(DEFS)
LINTLIBSTYLE	SYSV
LOCK_STYLE	LOCKF
LOG_INN_PROG	LOG_LOCAL7
LOG_INN_SERVER	LOG_LOCAL7
LSEEKVAL	off_t
_PATH_MAILCMD		/usr/bin/mailx
NOFILE_LIMIT	200
PID_T	pid_t
POINTER	void
PROF	
QSORTVAL	void
RANLIB	echo
RES_STYLE	TIMES
SIZE_T	size_t
SLEEPVAL	unsigned int
UID_T	uid_t
USE_UNION_WAIT	DONT
_EXITVAL	void
.DE
You will probably also need to use the \fIbdf\fP command instead of \fIdf\fP.
.PP
.DS
.UL "SGI Indigo with IRIX 4.0.1"
.ta \w'HAVE_ST_BLKSIZE   'u
ABORTVAL	void
ALARMVAL	uint
ACT_STYLE	MMAP
CFLAGS	$(DEFS) -g -w
CLX_STYLE	FCNTL
_EXITVAL	void
FORK	fork
FREEVAL	void
GID_T	gid_t
HAVE_ST_BLKSIZE	DONT
HAVE_TM_GMTOFF	DONT
HAVE_UNISTD	DO
LDFLAGS	
LIBS	-lmld
LINTFILTER	| sed -n -f ../sedf.sysv
LINTFLAGS	 $(DEFS)
LINTLIBSTYLE	SYSV
LSEEKVAL	off_t
POINTER	void
QSORTVAL	void
RANLIB	echo
SIZE_T	size_t
SLEEPVAL	uint
UID_T	uid_t
_PATH_COMPRESS	/usr/bsd/compress
.DE
Also, the \fIMISSING_xxx\fP parameters should be empty.
.PP
.DS
.UL "Solaris 2.X/SunOS 5.X, using SPARCompiler C 2.X"
.ta \w'HAVE_ST_BLKSIZE   'u
DEFS	-I../include -DSUNOS5
USE_CHAR_CONST		DO
CFLAGS	-O -Xa $(DEFS)
LDFLAGS
LIBS	-lnsl -lsocket -lelf
LINTLIBSTYLE	SYSV
LINTFLAGS	-b -h $(DEFS)
LINTFILTER	| sed -n -f ../sedf.sysv
RANLIB	echo
VAR_STYLE	STDARGS
SIZE_T	size_t
UID_T	uid_t
GID_T	gid_t
PID_T	pid_t
POINTER	void
ALIGNPTR	long
LOCK_STYLE	LOCKF
HAVE_UNISTD	DO
HAVE_SETSID	DO
HAVE_TM_GMTOFF	DONT
HAVE_WAITPID	DO
USE_UNION_WAIT	DONT
HAVE_VFORK	DONT
HAVE_UNIX_DOMAIN	DONT
CLX_STYLE	FCNTL
RES_STYLE	TIMES
FDCOUNT_STYLE	SYSCONF
ABORTVAL	void
ALARMVAL	unsigned
GETPIDVAL	pid_t
SLEEPVAL	unsigned
QSORTVAL	void
LSEEKVAL	off_t
FREEVAL		void
_EXITVAL	void
MISSING_SRC		
MISSING_OBJ		
PATH_COMPRESS	/bin/compress
.DE
Make sure you use the C version of subst.
.PP
.DS
.UL "System V Release 4"
.ta \w'HAVE_ST_BLKSIZE   'u
FREEVAL	void
GETPIDVAL	long
HAVE_TM_GMTOFF	DONT
HAVE_WAITPID	DO
LDFLAGS
LIBS	-lnsl -lsocket
LINTFILTER	| sed -n -f ../sedf.sysv
LINTFLAGS	-b -h $(DEFS)
LINTLIBSTYLE	NONE
LOCK_STYLE	FCNTL
MANPAGESTYLE	NONE
MISSING_MAN	strcasecmp.3
MISSING_OBJ	strerror.o strcasecmp.o
MISSING_SRC	strerror.c strcasecmp.c
_PATH_MAILCMD		/usr/bin/mailx
POINTER	void
QSORTVAL	void
RANLIB
RES_STYLE	TIMES
SIZE_T	unsigned int
USE_CHAR_CONST	DONT
USE_UNION_WAIT	DONT
.DE
I was never able to get \fIlint\fP to be useful on the machine I used.
Some versions of System V (for example, Esix 4.0.3) need the following
LIBS value:
.DS
.ta \w'HAVE_ST_BLKSIZE   'u
LIBS	-lresolv -lsocket -lnsl -L/usr/ccs/lib -lelf
.DE
On a Dell System V machine, you have to set \fIHAVE_UNIX_DOMAIN\fP to ``DONT.''
.PP
.DS
.UL "Ultrix 4.x (RISC)"
.ta \w'HAVE_ST_BLKSIZE   'u
ALARMVAL	unsigned int
FREEVAL	void
LDFLAGS
LINTFILTER	| sed -n -f ../sedf.sysv
LINTFLAGS	-b -u -x $(DEFS)
LSEEKVAL	off_t
MISSING_MAN	
MISSING_OBJ	syslog.o strerror.o
MISSING_SRC	syslog.c strerror.c
POINTER	void
PROF	-p
QSORTVAL	void
SIZE_T	unsigned int
SLEEPVAL	unsigned int
_EXITVAL	void
.DE
Ultrix also requires the new \fIsyslog\fP.
Some sites have reported problems with using the \fIsyslog\fP that INN includes.
The file \fIjtkohl-syslog-complete.tar.Z\fP in the \fI/pub/DEC\fP
directory on gatekeeper.dec.com has a ``for-Ultrix'' package that handles
both old and new \fIsyslog\fP calls.
While Ultrix has symlinks, it does not have the ``\-follow'' option in
its \fIfind\fP command.
This is used in \fIexpire/makeactive.c\fP; you will have to either install
the GNU \fIfind\fP or edit the source file.
.NH 1
Other Source Preparations
.PP
In addition to setting up the configuration file, it might be necessary
to do some other setups.
.NH 2
Systems with old syslogs
.PP
If you need to install the \fIsyslog\fP that is distributed with INN, go to
the top of the distribution and type ``make syslogfix''.
This will also compile \fIsyslogd\fP, the logging daemon.
You should install this to replace your existing daemon, usually in
\fI/etc/syslog\fP.
You will also need to install the new-style \fIsyslog.conf\fP file.
.PP
If you cannot replace \fIsyslogd\fP on your machine, then see the file
\fIsyslog/README\fP for information on how to set it up as an alternate
daemon.
.PP
Ignore any complaints from \fIlint\fP about the INN sources calling
\fIopenlog\fP with the wrong argument count.
In fact, if you \fBdon't\fP get any complaints, then something is wrong
with the way \fIsyslog\fP, \fI<syslog.h>\fP, or the \fIlint\fP libraries
are set up on your system.
.NH 2
The DBZ package
.PP
INN uses the DBZ database package.
Thanks to Jon Zeeff for his permission to use and redistribute DBZ,
as modified by Henry Spencer. 
INN has its own set of modifications to DBZ.
The changes are made with the \fIpatch\fP program and the context diff
in \fIlib/dbz.pch\fP.
If you don't have \fIpatch\fP installed, then you can make the changes
manually.
(If you don't have Larry Wall's \fIpatch\fP program get it from any
\fIcomp.sources.unix\fP archive as well as many FSF archives and other
places \(em you'll be glad you did.)
.PP
If you are using \fIvfork\fP (specified in the \fIFORK\fP parameter),
or you want to \fImmap\fP the database, then you \fBmust\fP apply the patch.
The Makefile in \fIlib\fP will normally do it for you automatically, anyway.
The beginning of the patch file describes the changes made in more detail.
If you do not apply the patch, then you must add add ``dbzalt.c'' and
\&``dbzalt.o'' to the MISSING_SRC and MISSING_OBJ parameters.
.PP
Apparently the System V 386 compiler can't optimize \fIdbz.c\fP (the
GNU C compiler doesn't have this problem).
If you have ``\-O'' in your \fIDBZCFLAGS\fP configuration parameter, then
take it out.
.NH 2
Using writev
.PP
INN makes extensive use the \fIwritev\fP system call to write several
I/O buffers in a single call.
If you do not have \fIwritev\fP then you must copy \fIinclude/uio.h\fP to
your \fI/usr/include/sys\fP directory.
You must also add ``writev.c'' and ``writev.o'' to the MISSING_SRC and
MISSING_OBJ parameters.
.PP
The ``fake'' \fIwritev\fP found in the \fIlib\fP directory is not highly
efficient.
You might want to write a better one that tries to \fImalloc\fP a new
buffer and join all the elements.
Be careful about doing this because \fIinnd\fP can use very big buffers.
.NH 1
Compiling the System
.PP
Once the INN sources have been configured, they are ready to be compiled.
If you are very confident of your changes, type the following:
.DS
cd $inn
make all
.DE
If you do not get any errors, skip to the section titled ``Installing the
System.''
.PP
If you are confident, but careful, type:
.DS
cd $inn
make world
cat */lint
.DE
This will compile everything, then run \fIlint\fP in all directories.
.PP
Another option is to run the \fIBUILD\fP script found at the top of
the source tree.
This will interactively configure, compile, and install the system.
After running that script, skip to the section titled ``Installing the
System.''
.PP
If you are more cautious, you should type the following:
.DS
cd $inn/config
make quiet
cd ..
.DE
This will use your already-tested \fIsubst\fP program with your new
\fIconfig.data\fP file.
You should then follow the steps in the following sections.
.NH 2
Building the Library
.PP
The next step is to build the INN library.
Do the following
.DS
cd $inn/lib
make libinn.a lint
.DE
.PP
This will build the library and run \fIlint\fP on the sources, putting
the output into a file named \fIlint\fP.
If anything fails to compile, you probably made a configuration error,
most likely in the ``C library differences'' section.
In particular, double-check the \fISIGHANDLER\fP and \fIxxx_STYLE\fP
parameters.
.PP
The \fIlint\fP output should be almost empty, except for a couple of
\&``possible pointer alignment problem'' warnings in \fIdbz.c\fP.
If you get much more than this, then you probably did not define
the \fIPOINTER\fP or \fISIZE_T\fP parameters properly.
The \fINEW\fP and \fIRENEW\fP macros in \fIinclude/macros.h\fP try to
capture all the alignment problems associated with dynamic memory allocation.
Also double-check the \fIALIGNPTR\fP parameter and the \fICAST\fP macro in
\fIinclude/macros.h\fP.
.PP
If \fIlint\fP reports any other problems, you should take the time to
investigate them.
Note that many \fIlint\fP libraries have errors.
Also, you may get some problems in \fIyaccpar\fP in \fIparsedate.y\fP; these
are most likely in the \fIyacc\fP-generated C code.
If you get any of these, complain to your vendor.
.PP
If you find a portability issue that I missed, please let me know.
.PP
Once the library is built, you should install it in the top-level INN
directory.
To do this type ``make\ install'' while still in the \fIlib\fP directory.
This will also compile a \fIlint\fP library for use in linting the programs
in the other directories.
.PP
Note that any time a change is made to the library you must do
\&``make\ install''; it is not enough to type ``make\ libinn.a''.
This is a deliberate decision \(em like a program, compiling a library
is different from making it available for others to use, and installing
a library should make it possible to run \fIlint\fP against it.
.NH 2
Compiling the Programs
.PP
INN's programs are separated into six areas, as detailed in the roadmap.
You'll need to build each one before you can install and use INN.
.NH 3
The Frontend Programs
.PP
Frontends are those programs that talk to the main news server, either
offering it articles or controlling its action.
This includes the following programs:
.IP \fIinews\fP 15
The program that validates and prepares news articles and gives them to
\fIinnd\fP.
This is mostly used by users (usually indirectly, through programs like
\fIPnews\fP), but also through special facilities such as news/mail
gateways.
.IP \fIrnews\fP 15
Unpacks news batches from UUCP sites and offers them to \fIinnd\fP.
.IP \fIctlinnd\fP 15
This program controls \fIinnd\fP, directing it to do most of the
tasks a news administrator will have to do:  create newsgroups,
update newsfeeds, and the like.
.PP
To build these programs, type the following:
.DS
cd $inn/frontends
make all
.DE
.NH 3
Innd
.PP
The next program is the main news server, which includes the following
programs:
.IP \fIinnd\fP 15
\fIInnd\fP accepts all incoming NNTP connections and either processes
their traffic or hands them off to the NNTP ``newsreader'' server.
It accepts articles, files them, and queues them so that they can be
sent to downstream feeds.
\fIInnd\fP listens on the official NNTP port.
On most systems only root can do this.
\fIInnd\fP is careful to set the modes of any files it creates, as well as
the privileges of any processes it spawns.
.IP \fIinndstart\fP 15
Sites that are concerned about large root-access programs may wish
to install \fIinndstart\fP.
This program opens the port, changes its user and group ID to be that
of the news administrator, and then \fIexec\fP's \fIinnd\fP with the
open port.
It also sets up a secure execution environment.
It is a small program (about 100 lines) that is easily understood.
You should use it because \fIinnd\fP will run faster because it won't
have to make any \fIchown\fP system calls.
If you make \fIinndstart\fP setuid root then no news maintenance has to
be done as root.
.PP
To build these, type the following:
.DS
cd $inn/innd
make all
.DE
.PP
Note that \fIinnd\fP handles the filing and distribution of certain messages
differently from other systems.
For example, you can have newsgroups within ``control'' for the
different types of control messages.
See \fIinnd.8\fP, \fInewsfeeds.5\fP, and \fIactive.5\fP in the \fIdoc\fP
directory for details.
.NH 3
The NetNews Reading Daemon
.PP
\fIInnd\fP implements a subset of the NNTP protocol \(em only those commands
that are needed for peer sites to feed news articles.
You must install \fInnrpd\fP to allow users to read news.
If a connection comes in from a host that is not a specified feed,
then an \fInnrpd\fP process is spawned to handle it.
(You can debug \fInnrpd\fP by running it interactively; put an entry for
the host named ``stdin'' in your \fInnrp.access\fP file.)
.PP
Build the newsreader server by doing the following:
.DS
cd $inn/nnrpd
make all
.DE
Note that if users on a peer machine (one that feeds you news) want to
read news from your server, then you have two choices.
You can use \fInntpd\fP from the reference platform (See Appendix II)
and make sure not to list the peer in your \fInntp.access\fP file.
The other choice is to relink the reading software on the other machine
with the INN library so that it uses the ``mode reader'' NNTP command
extension.
.NH 3
The Backend Programs
.PP
The backend programs take articles that \fIinnd\fP received and offer them
to your news neighbors.
This includes the following programs:
.IP \fIarchive\fP 15
A simple program to archive news articles.
.IP \fIbatcher\fP 15
Collects articles into batches for UUCP delivery.
.IP \fIbuffchan\fP 15
A program to split a single \fIinnd\fP stream into separate files.
It can buffer data, flushing files based on command-line switches.
.IP \fIcvtbatch\fP 15
A program to turn a file list into an INN batchfile.
A transition aide that is only documented in the source.
.IP \fIfilechan\fP 15
Another program to split a single \fIinnd\fP stream into separate files.
It is system-call intensive, but requires no locking protocol.
.IP \fIinnxmit\fP 15
A replacement for \fInntpxmit\fP from the reference implementation.
It reads a file containing a list of articles, and sends them to a host.
.IP \fInntpget\fP 15
A program to retrieve articles from a remote site.
.IP \fIshlock\fP 15
A program to provide a locking protocol for shell scripts.
.IP \fIshrinkfile\fP 15
A program to shrink a file by removing lines from the beginning.
It is useful for purging backlogged batchfiles.
.IP \fIsys2nf\fP 15
A program to turn a B or C News \fIsys\fP file into an INN \fInewsfeeds\fP
file.
This is a transition aide that is only documented in the source.
.PP
To build this set of programs, type the following:
.DS
cd $inn/backends
make all
.DE
.NH 3
Expire
.PP
This directory includes programs to modify the history database
as well as some utilities that might be useful in this task.
The database is called the \fIhistory\fP file, and it contains one
line for every article on the system, specifying when it was received
and where it was filed.
This file is indexed by the Message-ID, and the DBZ package provides fast
retrieval from it.
.IP \fIconvdate\fP 15
Converts between user-readable dates and the format used in the history file.
.IP \fIexpire\fP 15
Scans the history database to purge old entries, and remove old articles
from the spool area.
You can specify how long to keep sets of newsgroups.
.IP \fImakeactive\fP 15
This program can be used to rebuild the \fIactive\fP file if it is
lost in a crash.
.IP \fImakehistory\fP 15
This program scans through the spool area and rebuilds the history files.
.IP \fInewsrequeue\fP 15
This program can be used after a crash to resend articles to your neighbors.
.IP \fIprunehistory\fP 15
This is a tool for other programs that expire news.
It reads a list of Message-ID's and filenames, and updates the history
file to mark that the files have been deleted.
.PP
This directory also includes \fIexpire.pch\fP and \fIreap.pch\fP.
The first is a patch to the C News expire program that lets it cooperate
better with \fIinnd\fP, sending it messages when articles have been removed.
The second is a set of patches to the \fIreap\fP program that lets it
cooperate with \fIprunehistory\fP; it also adds some other useful features.
Both patch files have additional information in them.
Both programs are unsupported, provided by members of the beta-test group.
.PP
To build these programs, type the following:
.DS
cd $inn/expire
make all
.DE
.PP
If you are currently running C News, note that it has a directory named
\fIexpire\fP that is often the same pathname as INN's \fIexpire\fP program.
You will have to move, or remove, the directory before you can intall
the INN program.
.NH 3
Script and data files
.PP
In addition to the programs, INN requires several scripts.
For example, one script starts the server when the machine boots
while another prunes the log files and runs \fIexpire\fP every night.
Many of these scripts can be used as-is until you get a feel for
how INN works.
.PP
INN also requires several data files.
One specifies what sites feed you news, another what sites you feed, and
so on.
INN cannot provide these, other than giving sample entries.
You'll probably find that writing these files will be the hardest part of
your installation.
.PP
Prototypes for all these files are provided in the \fIsamples\fP directory.
Your modified copies should be maintained in the \fIsite\fP directory.
By splitting things up this way, official updates will never wipe out
any changes you have made.
.PP
To create the initial set of files, do the following:
.DS
cd $inn/site
make all
.DE
.PP
See below for an explanation of each file.
.NH 2
Manual pages
.PP
INN comes with an extensive set of manual pages.
You might want to edit the Makefile to set up the right ownership of
the installed manual pages.
Or you might want to not bother installing them at all.
.PP
When it comes to reading them, you should start with \fIinnd.8\fP
and \fIctlinnd.8\fP.
From there follow the cross-references as you want.
.NH 1
Installing the System
.PP
Although either \fIinnd\fP or \fIinndstart\fP must be run by root, most
of the installation does not have to be done as root.
The \fI$inn/makedirs.sh\fP script creates all the necessary directories used
by INN, and sets up the right ownerships and modes: owned by \fINEWSUSER\fP
in group \fINEWSGROUP\fP with 0775 permissions (the ``firewall'' directory,
\fI_PATH_INDDDIR\fP, has mode 0770).
You should review this script, then run it.
.PP
The rest of the installation should be done as the news administrator
or as root.
The Makefiles are very strict about setting the modes on the files that
get installed.
To install the programs, do the following:
.DS
cd $inn
make update
.DE
This target does a ``make\ install'' in all program directories.
It installs the programs and manpages, but does not update or install
any configuration files or scripts.
This is important:  in any directory (including the top-level one), a
\&``make\ install'' will install everything in that directory into
the right place.
A ``make\ update'' can only be done in the top-level directory or in the
\fIsite\fP directory, and it only replaces scripts, not configuration files.
When updating to a new INN release, you will probably want to do an ``update''
first, and then review the changed files by doing ``make\ diff'' in the
\fIsite\fP directory, and integrate your local changes as appropriate.
The Makefile also has other targets that you might find useful, so the
comments for entries like ``most'' and ``installed-diff', for example.
.PP
The next, and last, step is to build your INN configuration files and utility
scripts.
If you have not already done so, type the following:
.DS
cd $inn/site
make all
.DE
This will get copies of the scripts and files from the \fIbackends\fP and the
\fIsamples\fP directories and run \fIsubst\fP over them.
Whenever patches are issued, doing a \fImake\fP in this directory will let
you know what files have been updated, without destroying your local changes.
The \fIgetsafe.sh\fP script does this.
If you have either an \fISCCS\fP or an \fIRCS\fP directory then
\fIgetsafe.sh\fP will use the appropriate source control system for the
files in this directory.
.PP
The first set of files are used to carry out the control messages.
You might want to look them over; in particular, look at the table in
\fIcontrol.ctl\fP and the newslog manpages in \fIdoc\fP.
The control files are:
.DS
.ta 1.5i
checkgroups	rmgroup
control.ctl	sendme
default	sendsys
docheckgroups	senduuname
ihave	version
newgroup	writelog
parsecontrol
.DE
.PP
The following scripts are normally invoked by \fIcron\fP or at
system boot time, and should not require many changes:
.DS
.ta 1.5i
innlog.awk	scanlogs
innstat	tally.control
news.daily	tally.unwanted
rc.news
.DE
\fIRc.news\fP starts the server.
\fINews.daily\fP invokes \fIexpire\fP and \fIscanlogs\fP.
\fIScanlogs\fP calls the other scripts to process the logs.
You might want to review these scripts just to see what they do.
Do not get bogged down in the details, just read the comments.
They are documented in the manpages news.daily(8) newslog(5), and newslog(8).
.PP
There are some utility scripts to send news to your news feeds:
.DS
.ta 1.5i
nntpsend	send-nntp
nntpsend.ctl	send-uucp
send-ihave	sendbatch
.DE
They flush and lock the batch file for the specified site(s) and then call
\fIinnxmit\fP to send the articles to your downstream feeds.
\fISend-ihave\fP is used for ``ihave/sendme'' feeds and is described
in an appendix.
\fISendbatch\fP and \fIsend-uucp\fP flush and lock batchfiles and call
\fIbatcher\fP to queue up UUCP jobs.
You might want to modify these files to change the flags given to \fIuux\fP;
the default is to queue jobs up as grade ``d.''
You will almost definitely have to edit them to make sure that they properly
parse the output of \fIdf\fP so that your spool area is not overrun!
\fINntpsend\fP and \fIsend-nntp\fP do the same thing for NNTP feeds.
You must determine how you want to propagate your articles \(em the scripts
give common ways of getting the job done.
.PP
The following files will have to be edited to contain your local information.
They all have manual pages in the \fIdoc\fP directory that describe them:
.DS
.ta 1.5i
expire.ctl	newsfeeds
hosts.nntp	nnrp.access
inn.conf	passwd.nntp
moderators
.DE
.PP
The last group of files are utility scripts you might find useful:
.DS
.ta 1.5i
ctlrun	makegroup
inncheck	scanspool
innwatch
.DE
\fICtlrun\fP reads all the articles filed in your ``control'' newsgroup
and calls the appropriate control message script to parse them.
\fIInncheck\fP is a Perl script to check the syntax and permissions of
an installed INN system.
\fIInnreport\fP is an alternate way of summarizing the server's log file.
It is a Perl script.
\fIInnwatch\fP is a shell script to monitor the system and stop the server
when you are running low on disk space or inodes; it could be run out of your
\fI_PATH_NEWSBOOT\fP script.
You might have to edit it to understand your \fIdf\fP output format.
\fIMakegroup\fP is a front-end to \fIrnews\fP that helps you write
a control message to create a newsgroup.
You should review this script because you might have to change the way the
output of the \fIdate\fP command is parsed, and because you might might
want to change the default distribution.
\fIScanspool\fP is a Perl script to make sure that the active file
and the contents of your spool tree agree.
.PP
Once you have made the necessary modifications (and I admit that some of
this \(em especially the \fInewsfeeds\fP file \(em will be difficult), you
should type the following:
.DS
make install
.DE
Make sure you have \fIrc.news\fP installed in the right place, as explained
in the ``Paths to common programs'' section, above.
You might find it useful to read the ``First-Time Usenet or NNTP Installation''
appendix for help on navigating through the INN configuration files.
.PP
There are now only a couple more things to check.
First, make sure you have an \fIactive\fP file and a \fIhistory\fP database!
The appendices explain how to convert your existing files; the \fIBUILD\fP
script will create new ones for you.
If you have Perl, run \fIinncheck\fP to make sure that you have the
datafiles configured correctly.
The second is make sure that you have correctly updated your \fIsyslog.conf\fP
file to match the filenames and logging levels required by INN.
See \fIsyslog/syslog.conf\fP for an example of what to do.
.PP
Once you have done all of this, InterNetNews is now installed, and ready
to run \(em have fun!
.NH 1
Heterogeneous Client Installations
.PP
The \fIinews\fP program is used by user newsreaders.
Programs such as \fIrn\fP (which call \fIPnews\fP) prepare a news article
and feed it into \fIinews\fP.
\fIInews\fP validates the news headers, adds its own, and feeds the article
to the campus \fIinnd\fP server.
The \fIinews\fP that comes with INN is more useful then the ``mini-inews''
that comes with the reference implementation.
You cannot run the standard B2.11 \fIinews\fP.
You can run the C News \fIinews\fP, but only on client machines (i.e.,
those with a \fI$NEWSCTL/server\fP file).
I recommend that you install INN's \fIinews\fP on all the clients in your
campus.
.PP
INN comes with a \fIMakeInews\fP script to make it easier to build and
install \fIinews\fP on a wide variety of hosts.
This script creates a directory and copies all the necessary files (headers,
sources, configuration files) into it.
The script takes an optional argument, which should name the client machine's
architecture.
For example:
.DS
cd $inn
\&./MakeInews sun3
.DE
will create an \fIinews.sun3\fP directory.
You can then examine the Makefile in that directory, and build and install
\fIinews\fP on your Sun-3 clients.
This is easiest if the client NFS-mounts the source directory \(em that
way you can keep all your \fIinews\fP sources in one place.
.PP
\fIRnews\fP only has to be available on the machine where you run UUCP
(and perhaps a mail-news gateway).
If this is not the same machine as where \fIinnd\fP is running, then the
\fIMakeRnews\fP script can be used in the same manner as the \fIMakeInews\fP
script.
.\" $Revision: 1.10 $
.NH 1
Known Problems
.PP
If you use NIS (formerly Yellow Pages) on SunOS, you will need to add
a ``domainname'' entry to your \fIinn.conf\fP file if your hosts do
not contain fully-qualified domain names.
The most common symptom of this is that \fIinews\fP will fail because
it cannot generate a Message-ID.
Another problem with NIS is that reverse name lookups do not return the
fully-qualified domain name.
If you know that none of your local clients have a period in their name,
you can use a pattern like ``*[^.]*'' in your \fInnrp.access\fP file.
.PP
SunOS4.1.1 has a bug where \fIwrite\fP(2) can return EINTR.
The most common symptom is the following fatal error message from \fIinnd\fP:
.DS
Can't sync history, interrupted system call
.DE
This is Sun bug 1052649.
It is fixed in patch 100293-01.
According to the release manual, it is also fixed in all releases of SunOS
since 4.1.2.
.PP
If you have \fINOFILE_LIMIT\fP set you should know that the standard I/O
library in SunOS4.x has trouble with more than 127 descriptors.
The most common symptom is the following fatal error message from \fIinnd\fP:
.DS
can't fopen /usr/local/news/history, invalid argument
.DE
This occurs after doing a \fIctlinnd\fP ``reload'' command.
For a work-around, reboot your server instead of trying to ``reload.''
Another symptom is that \fIinnd\fP will exit if you do a \fIctlinnd\fP
\&``flush'' command while the server is paused or throttled.
This is Sun bug 1045141.
Sun does not plan to fix it for any 4.x release.
.PP
One site has reported the same error message happens after doing
a sequence of ``throttle'' and ``go'' commands.
It does not appear to be related to the bug mentioned above, although
the symptom is the same.
If you replace the body of INN's \fIxfopena\fP routine with the following,
it will work:
.DS
return fopen(p, "a+");
.DE
This is in the file \fIlib/xfopena.c\fP.
.PP
If you use Sun's unbundled compiler, \fIacc\fP, you must make sure
to use the unbundled assembler, too.
You might also get lots of ``left operand must be modifiable lvalue'' errors.
Setting \fIUSE_CHAR_CONST\fP to ``DONT'' will help.
.PP
There have been reports that the VAX Ultrix 4.2 \fImalloc\fP doesn't work
well with \fIinnd\fP, causing it to slowly fill up all swap space.
I believe that all of the memory leaks in \fIinnd\fP have been fixed,
but you might want to look at using a different \fImalloc\fP package.
The Kingsley/Perl \fImalloc\fP package is provided in the \fIlib\fP directory.
Add ``malloc.c'' and ``malloc.o'' to the MISSING_SRC and MISSING_OBJ lines
in \fIconfig.data\fP and rebuild.
.PP
I have been told that on SunSoft Interactive
UNIX System V Release 3.2 Version 3.0 systems <errno.h> has been
broken up into separate files.
The easiest way to work around this problem is to add
\&``#include\ <net/errno.h>'' to \fIinclude/clibrary.h\fP.
.PP
If you use 386BSD (the Jolitz port, not the BSDI product) you will have to
set \fIACT_STYLE\fP to ``READ''.
If you do not do this then the active file will not get updated.
Another work-around is to insert an ``msync'' call in the ICDwriteactive
routine in \fIinnd/icd.c\fP.
This is not supported because I consider the 386BSD behavior to be buggy.
.PP
The default configuration of some Sequent kernels does not provide enough
descriptors for \fIinnd\fP to run.
You might have to rebuild your kernel with the ``MAXNOFILE=128''
and ``NOFILEEXT=64'' options.
You will also have to had a ``setdtablesize(nnn)'' call in the main routine
of \fIinnd\fP, and a ``setdtablesize(0)'' call in the Spawn routine.
.PP
I have been told that some older versions of the SCO \fIopendir\fP routine
have file descriptor leaks.
The most noticeable symptom is probably that \fIinnd\fP will die while
trying to renumber the \fIactive\fP file.
You might want to use a freely-redistributable ``dirent'' package such
as one distributed by the Free Software Foundation.
.PP
On some SVR4 systems, attempting to set the socket buffer size is either
not supported or, even worse, might result in \fIinnd\fP's data size
growing.
The most noticeable symptom is ``cant setsockopt(SNDBUF)'' messages in
your \fIsyslog\fP output.
To fix this, either comment out the calls to \fIsetsockopt\fP in
\fIinnd/nc.c\fP or add ``\-USO_SNDBUF'' to your \fIDEFS\fP config
parameter.
.PP
I have heard that Sony SVR4 systems have lots of problems.
You must set \fIHAVE_UNIX_DOMAIN\fP to ``DONT''; sockets in general seem
to have problems, including kernel crashes and a blocked \fIinnd\fP.
.PP
If you use the GNU \fIsed\fP in the \fI_PATH_SED\fP configuration parameter,
make sure you get version 1.13; earlier versions have a bug that breaks
the \fIparsecontrol\fP scripts.
The most noticeable symptom is that all ``newgroup'' control messages
result in mail saying that they are unparseable.
.PP
Some versions of the shell in HP-UX do not properly parse a quoted ``[''
when it is in a pattern for a \fIcase\fP statement.
The most noticeable symptom is that \fInews.daily\fP does not properly
expire articles if \fIinnwatch\fP has throttled the server.
Contact HP and get a fix for SR # 5003-009811.
.PP
On some versions of AIX on the RS/6000, using memory-mapping can eat
up all the page space or crash the machine.
This will be noticeable if you have \fIACT_STYLE\fP set to ``MMAP'' and/or
have ``-DMMAP'' in \fIDBZCFLAGS\fP.
Ask your IBM representative for the ``U413090'' PTF and prerequisites to
apply it; it is believed that this will fix it.
.bp
.SH
Appendix I:  Differences from other News software
.PP
Administrators will find that INN is fairly incompatible with B and C News.
This section tries to mention the most important places where INN differs
from the other news systems.
If you have not maintained B or C News, you should probably skip this
section.
.PP
Users will generally only notice is that INN is faster; it should be
100% compatible with the other systems at the user level.
If you had particular problems that aren't mentioned here, please let me know.
Note, however, that this is \fInot\fP a tutorial on how to set up a new
INN system, or convert older software to it; no such document exists.
.NH 0
Configuration Files
.PP
Below is a list of the data files used by B and C News, and the reference
NNTP implementation, along with a short summary of how they map into INN
configuration files.
The syntax is always different: INN files are almost always a set of
colon-separated fields where lines beginning with a poundsign are ignored.
.IP \fIexplist\fP 15
This is replaced by the similar \fIexpire.ctl\fP file.
Archiving is done by a separate program.
.IP \fImailpaths\fP 15
This is replaced by the \fImoderators\fP file.
The ``default'' entry in \fImailpaths\fP is replaced by either a
full wildcard (``*'') entry in the \fImoderators\fP file, or by a
\&``moderatormailer'' entry in the \fIinn.conf\fP file.
.IP \fInntp.access\fP 15
This is replaced by the \fIhosts.nntp\fP (for NNTP peers) and
\fInnrp.access\fP (for newsreading) files.
.IP \fInntp.sys\fP 15
This is a password file used if NNTP is compiled with the ``AUTH'' option.
It is replaced by the \fIpasswd.nntp\fP file.
Note that \fIinews\fP and \fIrnews\fP will also try to read \fIpasswd.nntp\fP.
Therefore, you will probably want to have one-line versions of it for your
on-campus clients.
.IP \fIorganization\fP 15
This is replaced by the ``organization'' entry in the \fIinn.conf\fP file.
.IP \fIrn/server\fP 15
This is replaced by the ``server'' entry in the \fIinn.conf\fP file.
.IP \fIwhoami\fP 15
This is replaced by the ``pathhost'' and ``fromhost'' entries in the
\fIinn.conf\fP file.
.NH 1
Newsgroups, Active, Sys, and Newsfeeds
.PP
The biggest difference is how the \fInewsfeeds\fP file compares with the
\fIsys\fP file.
Newsgroup patterns like ``all.all.ctl'' are completely gone.
All newsgroup patterns are shell-style wildcards, matched against the
\fIactive\fP file.
.PP
The \fIactive\fP file is taken to be the definitive list of newsgroups that you
want to receive.
With B and C news, an article must match the subscription list of the
local site as specified in the \fIsys\fP file.
If it matches, each newsgroup is then looked up in the \fIactive\fP file.
If none of the newsgroups are found, then the article is filed into the
newsgroup named ``junk''.
.PP
INN's behavior is much simpler.
If a newsgroup does not appear in the \fIactive\fP file, it is ignored.
If none of the groups are mentioned, then the article is rejected:
nothing is written to disk.
This is a deliberate design decision:  if you do not want a particular
newsgroup to take up your disk space, remove it from the \fIactive\fP file;
if your neighbors have not gotten around to updating your newsfeed, then
the only thing that will happen is that some network bandwidth will have
been wasted when they send you the article.
.PP
You can change INN's behavior so that it resembles the other systems.
To do this, compile with \fIWANT_JUNK\fP set to ``DO.''
Note that this will accept \fIeverything\fP.
Because there is no subscription list, you cannot say ``give me all of the
foo hierarchy (filed into junk), but not the alt hierarchy.''
You must list the group in the \fIactive\fP file.
.PP
INN strictly believes in distributions.
If the site named \fIME\fP has any distributions, then incoming articles
must either have no Distribution header, or the header must match the
distribution list.
If you want to blindly accept all distributions, make sure you do not
have a ``/distrib,...'' section in your \fIME\fP entry.
Distributions are fixed strings \(em there are no patterns or special
wildcards like ``all.''
.PP
For more details on these items, see \fIdoc/newsfeeds.5\fP.
.NH 1
Control Messages
.PP
Like C News, INN implements all control messages other than cancel as
shell scripts.
The number and type of parameters is different from that of C News.
All control messages consult the file \fIcontrol.ctl\fP before acting on
the message.  If the sender's address matches with the list of authorized
addresses (e.g., ``tale@uunet.uu.net'', ``*'', etc.), the control
message is either acted upon, mailed to the news administrator, or logged.
For example, messages from ``tale@uunet.uu.net'' (the current moderator
of news.announce.newgroups) are honored.
.PP
The ``control'', ``junk'', and ``to'' newsgroups can be explicitly sent
or not sent.
See \fIdoc/newsfeeds.5\fP and \fIdoc/innd.8\fP.
.PP
The \fIctlinnd\fP program is what really directs the server to create or
remove newsgroups.
This results in a semi-recursive process:  the control message arrives, and
a script is invoked to process the message.
If approved, the script invokes \fIctlinnd\fP to send a message back to the
server telling it to create or remove the group.
.NH 1
Locking
.PP
A running news system has many open files.
These files can be divided into two groups.
The first group includes the history database and \fIactive\fP file.
The second group includes the logfiles and batch files used to send articles
to your feeds.
.PP
B news uses an internal protocol for the first group.
For the second group, since \fIinews\fP does ``atomic appends,''
no locking is necessary.
C news uses the \fIlocknews\fP and \fInewslock\fP scripts for the first
group, and provides no fine-grain mechanism for the second group.
.PP
With INN, the server is running all the time and all locking is done under
the direction of \fIctlinnd\fP.
The first group is generally handled by using the ``throttle,'' ``pause,''
and ``go'' commands (sometimes ``reload'' will be necessary).
The second group is handled by the ``flushlogs'' and ``flush'' commands.
See the \fIdoc/ctlinnd.8\fP manpage; examples of their use can be found in
various scripts in the \fIsamples\fP directory.
.\"
.bp
.SH
Appendix II:  Converting from other News software
.PP
INN is a complete news transport and expiration system.
Since few people will be installing INN from scratch, this section
should help you determine what you can ``throw out'' from your earlier
news setups.
It is also compatible with much of the existing news software, so you
can create a mixed environment if you want to, and if you are careful.
.NH 0
C News Expire
.PP
The \fIexpire\fP program that is distributed with INN does not do
any archiving.
Since the history databases currently have the same format, it is possible to
use the C News \fIexpire\fP if you want to.
(The INN history database may change, however, so you should only do this
if you really have to \(em you really should use INN's \fIexpire\fP.)
There are three ways to do this.
.PP
The first way is to change your \fIdoexpire\fP script so that it calls
\fIctlinnd\fP to ``throttle'' \fIinnd\fP just before \fIexpire\fP
runs.
It should then issue a \fIctlinnd\fP ``go'' command after \fIexpire\fP
is done.
The drawback to this method is that no incoming news is accepted until
all expiration is finished.
.PP
The second way is to compile \fIlib/lock.c\fP and add it to your C News
library \fIlibcnews.a\fP, replacing the provided lock functions.
You should then remove \fIexpire\fP and relink it.
This method has not been tested very thoroughly, but it is rather simple.
.PP
The third way is to teach the C News \fIexpire\fP to talk to \fIinnd\fP
and tell it to cancel articles that it would remove.
To do this, apply the patch file \fIexpire/expire.pch\fP to your C News
\fIexpire.c\fP sources.
You will also have to add \fIlib/inndcomm.o\fP to \fIlibcnews.a\fP and
then rebuild \fIexpire\fP.
.NH 1
Standard NNTP daemon
.PP
You can use the ``standard'' \fInntpd\fP server.
You should only have to do this if you have hosts that feed you news,
and where the users on that machine also want to read news on your
machine.
.PP
Make sure that you configure \fInntpd\fP so that it is using DBZ, and have
it feed each individual article to \fIinews\fP; don't use the \&``batched
input'' option.
It should also be set up so that it acts as if it is running under
\fIinetd\fP.
You should also make sure that \fIinetd\fP does nothing with the NNTP
port, number 119.
.NH 1
NNTP-based newsreaders
.PP
If you already have your NNTP-using newsreaders installed and running,
you do not have to do anything.
This includes \fIxvnews\fP, \fIxrn\fP, \fIrrn\fP and so on.
INN implements the standard NNTP protocol, with some extensions.
INN does not provide the extensions used by \fItrn\fP, \fItin\fP or
other newsreaders.
(You can enable the \fItrn\fP ``XTHREADS'' by modifing \fInnrpd/nnrpd.h\fP;
change the ``DONT_DO_XTHREAD'' to ``DO_DO_XTHREAD'' and verify the other
macros in that section.
INN will not implement all the different indexing systems because the
right solution is to have a generic interface that all readers can use.)
.PP
For administrative convenience, however, you might wish to have all your
newsreaders use the INN library and configuration files to talk to the server.
The next section describes how to do that for \fIrn\fP.
It is provided as an example, to help you convert other programs you
might have.
INN does not provide, nor fully support, any newsreaders.
.NH 1
Remote rn
.PP
The ``remote'' version of \fIrn\fP (also called \fIrrn\fP) uses a set of
routines in the NNTP ``clientlib'' file.
INN can emulate these routines; see \fIdoc/clientlib.3\fP.
If you need to build \fIrn\fP for client machines that don't have the
entire INN distribution available, use the \fIMakeLib\fP script to
build a distribution directory of the necessary routines.
Use this script the same way you use the \fIMakeInews\fP script.
.PP
\fIRn\fP, \fIrrn\fP, and \fItrn\fP are moving targets so these instructions
may be out of date.
The maintainers have agreed to officially support INN, however, which is
a good thing.
.PP
There are two ways to build \fIrn\fP so that it uses the INN library.
If you don't have the NNTP distribution installed you will have to use
the first way.
.PP
The first way is to apply a patch to the latest \fIrn\fP \fIConfigure\fP
script and then execute it and rebuild the program.
To do this, type the following:
.DS
cd \fIrn_source\fP
patch <$inn/frontends/rn.pch
\&./Configure
make
.DE
At some point, \fIConfigure\fP will ask you if you want to use the
InterNetNews library; answer \fIyes\fP.
You can then use either the full sources, or a special library that
contains just the needed header and sources files.
Tell \fIConfigure\fP the appropriate pathnames, and then proceed
with the rest of the \fIrn\fP installation.
.PP
The second way is to edit a couple of files after you have run \fIConfigure\fP
and set it up to build the remote rn.
First, replace the \fIrn\fP file \fIserver.h\fP with the INN file
\fIinclude/myserver.h\fP.
The next step is to edit the \fIrn\fP Makefile to remove the ``clientlib''
file from the source and object file lists.
This can probably be done by commenting out the definitions of the
\fIc5\fP and \fIobj5\fP variables.
You must also edit the Makefile to add the INN library to the list of
libraries that are linked in.
This can probably be done by editing the line that defines the \fIlibs\fP
variable so that the full pathname to \fIlibinn.a\fP is the first item
after the equal sign.
.NH 1
Removing the Other Stuff
.PP
The names below assume a ``standard'' news setup; things might be different
on your machine.
Also, many programs have alternate names and links; make sure you chase down
and remove \fBall\fP of them.
.PP
You might find it easiest to rename your \fI/usr/lib/news\fP (and
\fI/usr/lib/newsbin\fP) directories to something else and start with a
clean slate, copying over the files as they are needed.
Make \fBsure\fP that your news processing is completely stopped before
you begin this process.
That includes any \fIcron\fP jobs that may be running.
.PP
The \fI/usr/lib/news\fP directory can become cluttered \(em that's why
C News split everything up into separate directories.
The following files are compatible with C News and B2.11 News, and should be
\fIkept\fP:
.DS
.ta 1.5i
active	active.times
.DE
If you are running C News keep these files, otherwise delete them and use
\fImakehistory\fP to rebuild them:
.DS
history
history.dir
history.pag
.DE
.PP
\fIRn\fP does not have to be modified so leave this directory alone (or
copy it back if you moved your original):
.DS
/usr/local/lib/rn
.DE
If you set up \fIrn\fP to use the INN library, remove this file:
.DS
/usr/local/lib/rn/server
.DE
.PP
The input system is completely replaced.
Remove the following programs and their manpages:
.DS
/bin/cunbatch
/bin/inews, /usr/lib/news/inews, etc...
/bin/rnews, /usr/bin/rnews, etc...
/usr/lib/news/rnews.stall
/etc/nntpd, /usr/etc/nntpd, etc...
.DE
Also remove the following directories and everything within them:
.DS
/usr/lib/news/bin/input
/usr/lib/news/bin/relay
/usr/lib/news/bin/ctl
/usr/lib/news/bin/inject
/usr/lib/news/nntp (mkgrdates, nntp_access, shlock, etc)
.DE
.PP
The transmission facility is completely replaced.
You may keep your current feed subsystem if you want to, but it will require
some changes to make sure that batchfiles are properly flushed; see the
\fIsend-xxx\fP scripts for examples.
Remove these files and programs:
.DS
/usr/lib/news/batchparms
/usr/man/man8/newsbatch.8
.DE
Remove the following directory and everything within it:
.DS
/usr/lib/news/bin/batch
.DE
You can continue to use \fInntplink\fP, \fInewsxd\fP, and the like, subject
to the caveat just mentioned.
.PP
Article expiration and maintenance of the history and active files is
completely replaced.
Remove this file:
.DS
/usr/lib/news/explist
.DE
Remove the following directories and everything within them:
.DS
/usr/lib/news/bin/expire
/usr/lib/news/bin/maint
.DE
If you do not remove the \fIexpire\fP directory, you will probably have
problems installing INN's \fIexpire\fP, which is a program that often
has the same name as the C News directory.
.PP
The following programs in \fI/usr/lib/newsbin\fP are not needed and can be
deleted.
Keeping them around is harmless, and if you find them useful don't delete
them:
.DS
.ta 1.5i
canonhdr	newshostname
ctime	newslock
dbz	queuelen
getabsdate	sizeof
getdate	spacefor
gngp
.DE
Note that \fIctime\fP, \fIgetabsdate\fP, and \fIgetdate\fP are replaced by
\fIconvdate\fP.
More importantly, \fInewslock\fP does not lock \fIinnd\fP; it is best
to remove it.
.PP
The following files are replaced by INN configuration files.
You should delete them, just to avoid confusion:
.DS
.ta 1.5i
mailname	sys
mailpaths	whoami
organization
.DE
If you have other software that uses them (except \fIsys\fP),
you can keep them.
The following will be rebuilt (or overwritten) by \fIinnd\fP and
\fIscanlogs\fP so you should remove them:
.DS
.ta 1.5i
errlog	log
.DE
.PP
In addition to the manpages for the programs listed above, the following
manual pages should be removed:
.DS
.ta 1.5i
active.times.5	newsmail.8
expire.8	newsmaint.8
mkgrdates.8c	nntpd.8c
news.5	nntpxmit.1
newsaux.8
.DE
.PP
Any other files and directories can probably also be discarded.
.\"
.bp
.SH
Appendix III:  Setting up different feeds
.PP
This section gives some notes and advice on how to set up different
types of outgoing news feeds.
It duplicates and expands upon the information in the manual pages.
.NH 0
Ihave/sendme feed
.PP
For a standard UUCP newsfeed, a site batches up all the articles it
receives and sends them to the downstream site, which unpacks the batch
and processes each article.  If the downstream site has multiple feeds,
however, it might want to ``filter'' the articles that get sent.  This is
done by having the feeding site send a list of Message-ID's as an
``ihave'' control message.  The receiving site examines the list to see
which articles it does not currently have, and sends it back to the
upstream site as a ``sendme'' message.  The original site receives this
message and prepares a batch in the standard way.
.PP
Note that this has nothing to do with NNTP.  It is a specialized type of
batched feed that is not used very often.
To do ihave/sendme with a site named remote, the local site must either
have a ``to.remote'' newsgroup or be compiled with MERGE_TO_GROUPS set to
\&``DO''
.PP
Accepting an ihave/sendme feed is easy.  Suppose an ``ihave'' message is
received from a site named remote.  When \fIinnd\fP processes the message
it will invoke the appropriate control script,
\fI/usr/local/news/bin/control/ihave\fP.  The script will filter the body
using \fIgrephistory\fP, creating a list of Message-ID's not found in the
\fIhistory\fP database.  It uses this output to create a ``sendme''
control article which is posted to the ``to.remote'' newsgroup using
\fIinews\fP.  This article will then be queued and sent to remote in the
normal way.  The remote site will then send the desired articles back.
.PP
Providing an ihave/sendme feed is a bit more complicated.  First, you must
create two entries in your \fInewsfeeds\fP file.  The first should be
named remote.ihave.  Make this a ``Tf,Wm'' feed that contains the remote's
subscription list.  This entry results in a a file that accumulates the
Message-ID's of all articles that remote might want.  The other entry
should be named remote.  This should be a ``Tf,Wn'' feed that only
subscribes to the ``to.remote'' newsgroup.
(Actually, if you feed some groups as a standard feed, you can put them
on the remote entry, rather then the remote.ihave entry.)
.PP
The next step is to have the ``ihave'' control messages sent out.  To do
this, review the \fIsend-ihave\fP script and make sure it is invoked as
needed (usually out of \fIcron\fP).  It splits the batchfile from the
remote.ihave \fInewsfeeds\fP entry and posts ``ihave'' control messages
into the ``to.remote'' newsgroup.  These messages will get queued for the
remote entry.
.PP
The next step is to send out any articles queued for the remote entry.
Treat this as a standard UUCP feed, invoking \fIsend-uucp\fP or
\fIsendbatch\fP as appropriate, typically a few minutes after
\fIsend-ihave\fP runs.
.PP
When the remote site receives the ``ihave'' message it will filter it and
send back a ``sendme'' message whose body is the list of desired
Message-ID's.  When \fIinnd\fP processes this message it will invoke the
appropriate control script, \fI/usr/local/news/bin/control/sendme\fP.  This
script will call \fIgrephistory\fP to turn the list into a list of files
appended to the batchfile for remote.  Examine this script (the filename
should probably match the filename in the remote \fInewsfeeds\fP entry)
and send the batch to the remote site (using \fIbatcher\fP, often called by
\fIsend-uucp\fP or \fIsendbatch\fP).
.NH 1
Feeding a large number of sites
.PP
\fIInnd\fP tries to keep as many batchfiles open for as long as possible.
It will normally open as many as it can, using all the available
descriptors minus a fixed number for internal use (log files, etc.).
You can explicitly set the number of files to open by using the ``\-o'' flag.
.PP
If you have more outgoing feeds than available descriptors, \fIinnd\fP
will recycle the files on a a ``least recently used'' basis.
If most of your feeds get most articles (or you have vastly more feeds
then available descriptors), this will lead to ``file thrashing,'' closing
and opening all the excess feeds on each article.
To reduce this, you can have \fIinnd\fP use an internal buffer for a site
by using the ``I'' parameter in the \fInewsfeeds\fP file.
If a site does not have its batchfile open, the server will not try to open
it until there is more data to be written then will fit in the buffer.
For example, suppose \fIinnd\fP was started with ``\-o10'' and there
are 12 sites, all with ``I512'' in their \fInewsfeeds\fP entry.
If each article generates 50 bytes (a pathname and a Message-ID), then
\fIinnd\fP will close and re-assign two descriptors every 10 or so articles.
.PP
A better alternative is to use funnels and an exploder.
Funnels, specified in the \fInewsfeeds\fP file, let multiple sites send
their output down a single stream.
The advantage of funnels is that this stream can be a channel; the primary
disadvantage is that the funnel specifies what data is to be written,
not the individual sites.
(Since most feeds will want either ``Wn'' or ``Wnm'' entries, this is
usually not a problem.)
.PP
In order for the funnel output to be useful, it usually must be split into
individual, per-site, files.
Programs that do this type of splitting are called ``exploders.''
INN provides two exploders, \fIfilechan\fP and \fIbuffchan\fP.
.PP
\fIFilechan\fP is the simplest, and most inefficient, exploder.
It does not keep any files open and is very system-call intensive.
It can be used to provide behavior (and performance!) that is similar to
B2.11 \fIinews\fP.
It can, however, be used as the funnel for an unlimited number of sites.
.PP
\fIBuffchan\fP keeps all its output files open all the time.
It should not be used for more sites then a single process can have open.
\fIBuffchan\fP also has flags to automatically flush its files, as well
as close and re-open them, every specified number of articles.
(The re-open capability is useful for things like \fInntplink\fP in its
\&``watch the batchfile'' mode.)
Using \fIbuffchan\fP with the ``\-l1\ \-c50'' flags will give behavior
that is similar to the C News \fIrelaynews\fP.
.PP
\fIBuffchan\fP can be run as a full exploder (``Tx'') in the
\fInewsfeeds\fP file.
This means that you can use \fIctlinnd\fP to send a command line down
\fIbuffchan\fP's input stream.
(\fIInnd\fP will also send a command whenever newsgroups are modified.)
The only useful message is ``flush'' which will close, and re-open, the
specified site files.
You should also note that the flow is one-way; full exploders cannot send
any acknowledgement back.
.NH 1
Master/slave replication
.PP
INN supports a simple model of replicated news databases:
a single master host pushes out updates to its slaves.
The master is the only host that receives articles \(em this includes
both outside newsfeeds and articles written by local users.
The slaves only receive articles from the master.
.PP
No special work is required to set up a master host.
.PP
A slave is set up by starting \fIinnd\fP with the ``\-S'' flag to specify
the name or IP address of the master host.
This should be done by modifying the ``FLAG'' variable in your
\fI_PATH_NEWSBOOT\fP script.
If \fIinnd\fP is started with the ``\-S'' flag it will pass this flag on
to \fInnrpd\fP.
This means that when anyone connects to the slave and does a ``POST'' command,
\fInnrpd\fP will connect to the master and offer the article.
.PP
Since the \fInnrpd\fP on the slave host will usually add its name to
the Path header, you should add ``Ap'' to the \fIflags\fP field of
the slave's entry on the master.
.PP
Once the slave has been set up it is necessary to have the master feed it.
This is done by using an extension to the NNTP protocol.
This extension, the ``XREPLIC'' command, is is documented in \fIinnd.8\fP.
In order to do this you will have to set up a \fInewsfeeds\fP entry for
the slave.
This should be a standard entry except that you will need to have both
the filename and the replication information written int the batchfile.
To do this, put ``WnR'' in the \fIflags\fP field of the entry.
.PP
When you want to actually send the articles to the slave site you will
have to specify the ``\-S'' flag in your \fIinnxmit\fP command.
Current versions of \fInntplink\fP use the ``\-x'' flag.
.PP
When running as a slave, \fIinnd\fP is very paranoid about staying synchronized
with its master.
Most noticeably, you should make sure that all newgroup and rmgroup control
messages are handled identically on both systems.
.\"
.bp
.SH
Appendix IV:  First-time Usenet or NNTP Installation
.PP
Since the needs and administration of systems varies so much, I can
only give some general guidelines and advice in this section.
Like 
.UX
system administration in general, it is unfortunately still true that
most of the job will be learned ``in the heat of the moment.''
Once you have INN set up, however, it should not require much attention.
For general problems, try posting to ``news.sysadmin'';
use ``news.software.nntp'' and ``news.software.b'' for installation problems.
.PP
Once all the software has been compiled and installed, you must now
get a newsfeed.
This involves having one (or more) sites pass along to you all the
articles that they have received.
Getting articles is a passive action, because it is generally more
efficient that way.
(The \fInntpget\fP program is primarily a debugging aide and utility
program.
It is not the recommended way to get a newsfeed, and most sites will
prefer you not to use it for that.)
.PP
If you already have Usenet access, you could post a note to ``news.admin''
asking for a feed.
Make sure to say that you are looking for an NNTP connection!
If you are a member of an NSFNet regional network, or subscribe to
a commercial IP network, ask your contact there at the network center.
If they do not provide feeds directly, they can probably help you find
one.
You also might try writing to the <nntp-managers@colossus.apple.com>
mailing list.
This will reach the news administrators of many NNTP sites on the Internet.
(If you want to join the list, remember to send it to
nntp-managers-request, \fBnot\fP nntp-managers!)
.PP
Once have a site willing to give you a feed, you need to get the
list of groups that they will give you.
You also need to create those groups on your machine.
The easiest way to do this is usually to ask them for a copy of
their active file, and for you to add the entries of the groups that
you're interested in.
.PP
Once the groups are set up, your newsfeed will periodically connect to
your NNTP server and offer it any new articles that have arrived since the
last connection.
\fIInnd\fP will accept the connection, receive the articles, and
queue them up for any sites that you feed.
.PP
The next step is to set it up so that your articles are sent back to
your newsfeed.
To do this, create a \fInewsfeeds\fP entry, using the same name that shows
up in the Path header that you see.
(If you use a different name, then use the ``excludes'' sub-field to avoid
offering back everything they offer you.)
This is usually done by giving them all non-local articles as a file
feed.
For example, ``Foo, Incorporated'' does not give any foo.* articles to
anyone else.
.PP
When someone at your site writes an article, \fIinnd\fP will record the
filename in the batch file for your upstream site.
Either \fIsend-nntp\fP or \fInntpsend\fP will flush and lock the batchfile,
and then call \fIinnxmit\fP to connect to the remote site and send these
queued articles out.
You should edit the script to list the sites you want, and arrange for
\fIcron\fP to run this script on a regular basis.
You can run it as often as you like, but 10 minutes is a common interval.
.PP
If you want to feed any sites via UUCP, then you will have to set
up file feed entries for them in the \fInewsfeeds\fP file, and arrange
to have \fIcron\fP run the \fIsend-uucp\fP script as desired.
(UUCP batches are typically only done every few hours.)
.PP
Once you have news flowing in and out of the system, you will have
to expire it or your disks will fill up.
The \fInews.daily\fP script should be run by \fIcron\fP in the middle
of the night.
It will summarize that day's log files, and then call \fIexpire\fP to
purge old news.
You might also want to have \fIcron\fP run \fIrnews\fP hourly to pick
up any stalled batches.
Finally, if your feeds change IP address, you might want a daily job
that does ``ctlinnd reload hosts.nntp "flush cache"''.
This is because \fIinnd\fP does not currently time-out DNS entries.
.PP
You will generally want to set up the \fIcron\fP jobs
so that they are run as the news administrator, and not as root.
A good version of \fIcron\fP that makes it easy to do this can be found
on gatekeeper.dec.com in pub/misc/vixie/cron.tar.Z.
.PP
You will also need to get one or more programs to read news.
There are several freely-available programs around.
\fIRn\fP is popular, and is probably the best place to start.
The official distribution is available for anonymous FTP at tmc.edu
in the \fIrn\fP directory.
.PP
Welcome to Usenet, and have fun!
.\"
.bp
.SH
Appendix V:  News overview database
.PP
There are now many newsreaders available that are able to do ``threading.''
This is the ability to track a discussion within a newsgroup by using
the References header (or other data), regardless of changes in headers
like the Subject line.
Examples of these readers include \fInn\fP, \fItrn\fP, and \fIgnus\fP,
and more are becoming available.
Until recently, a major problem with these readers is that they all
required a specialized external database that contained the threading
data.
.PP
In late 1992, Geoff Collyer <geoff@world.std.com> released the \fInov\fP,
or ``news overview,'' package.
This included database tools and, and client access routines,
that let the current threaded newsreaders use a common, textual,
database.
An overview database typically adds adds about 7-9% to your storage
requirements.
By default, the overview files are stored in the spool directory;
you can change this to use an alternate tree that mirrors the spool
hierarchy by changing the \fI_PATH_OVERVIEWDIR\fP parameter.
.PP
INN includes full support for creating and expiring news overview databases.
To do this, add an entry like the following to your \fInewsfeeds\fP file:
.DS
overview:*:Tc,WO:/path/to/bin/overchan
.DE
(Make sure to replace \fI/path/to/bin\fP with the value of your
\fI_PATH_NEWSBIN\fP parameter.)
Then reload the \fInewsfeeds\fP file or restart your server.
To create the initial database, run the following command after you have
started \fIoverchan\fP:
.DS
expireover -a -s
.DE
You will also need to expire the overview data.
The easiest way to do this is to add the ``expireover'' keyword to
the \fIcron\fP job that runs \fInews.daily\fP.
.PP
The \fInnrpd\fP server includes two command extensions to access the database;
they are documented in the ``protocol extensions'' part of \fIdoc/nnrpd.8\fP.
INN does not include any client code or modifications to any newsreaders
to use the overview data.
Most maintainers have agreed to support the overview database, including
the INN extensions for remote access.
You can find prototype versions of many readers (work done by Geoff) on
world.std.com in the directory src/news; look for files named
\fIreader\fP.dist.tar.Z.
.\"
.bp
.SH
Appendix VI:  Limited MIME Support
.PP
This version of INN includes limited support for MIME, the Multipurpose
Internet Mail Extensions, described in RFC 1341.
The support is the ability to do limited transport of arbitrary
MIME messages, and \fInnrpd\fP can add MIME headers to all local postings
that do not have them.
.PP
In addition, there are patches available for \fInntplink\fP that
allow it to do MIME transport.
The patches are not (yet) part of the official release; if you need them,
contact Christophe Wolfhugel <Christophe.Wolfhugel@hsc-sec.fr>; he did
most of the INN work, too.
.PP
You should be very careful if you have \fInnrpd\fP add MIME headers.
To do this, edit \fIinn.conf\fP as indicated in \fIdoc/inn.conf.5\fP.
Once this is done, \fBall\fP articles posted will get MIME headers added.
Existing MIME headers will not be modified, but missing ones will be added.
The default values to add to \fIinn.conf\fP are these:
.DS
mime-version: 1.0
mime-contenttype text/plain; charset=us-ascii
mime-encoding: 7bit
.DE
An internationalized site might want to use these values:
.DS
mime-version: 1.0
mime-contentType: text/plain; charset=iso-8859-1
mime-encoding: 8bit
.DE
It is possible to use these values because INN provides a clean eight-bit
data path.
Unless you make special arrangements with your peers, however, you
must transmit seven-bit data.
Doing this will require special transmit agents.
Note that \fInnrpd\fP is not a Mime-compatible reader.
You must have software to extract the data and present it appropriately.
.PP
If you configure your site to use seven-bit data, then you must also
make sure that none of your software creates eight-bit articles.
\fINnrpd\fP does not verify this.
If you configure your site to use eight-bit data, then ASCII works fine,
but remember that in quoted-printable long lines are cut and
that the equal sign (``='') is quoted; this is really bad for source code
postings, among others.
.PP
The character set can also cause problems.
If you use ``iso-8859-1'' you must make sure that your posting software
uses this character set (e.g., not CP-437 under MS-DOS) because \fInnrpd\fP
does not do any conversion.
.PP
In general, be very cautious.
.PP
MIME articles can only be sent using \fIinnxmit\fP; work on \fIbatcher\fP
is in progress.
Unless the ``\-M'' flag is used, no MIME conversions are done.
If the flag is used, the following happens:
Articles with a Content-Transfer-Encoding header of ``8bit'' or ``binary''
are forwaded in ``quoted-printable'' format (the ``base64'' format will
be available soon).
All other articles -- in particular, those without MIME headers, those of
type ``message'' or ``multipart,'' those with Content-Transfer-Encoding
header of ``7bit'' -- are forwarded without any change.
