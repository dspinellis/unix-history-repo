.\" @(#)$Id: mh-changes.ms,v 1.29 1994/03/23 23:46:49 jromine Exp $
.\" Standard -ms macros
.\" with the following changes
.ds lq \\*Q
.ds rq \\*U
.\" remember to update date in text below
.DA "December 1, 1993"
.if n \{\
.na
.\}
.nr PO 1i
.po 1i
.\" .EH ''Changes to MH 6.7'%'
.\" .OH ''Changes to MH 6.7'%'
.ds LH Changes to MH 6.8
.ds CH
.ds RH %
.TL
Changes to
.br
The RAND MH Message Handling System:
.br
UCI version MH 6.8
.AU
John L. Romine
.AI
Computing Support Group
Department of Information and Computer Science
University of California, Irvine
Irvine, CA  92717\-3425
Bug-MH@ICS.UCI.EDU
.AB
.PP
This document describes the changes to the
UCI version of the RAND MH system from MH 6.6
to this release of MH 6.8.
This document is meant to supplement,
not supersede,
the standard MH User's manual and MH Administrator's manual.
.PP
Comments concerning this documentation should be addressed to the
mailbox \fBBug\-MH@ICS.UCI.EDU\fP, or \fBucbvax!ucivax!bug-mh\fP.
.AE
.SH
ACKNOWLEDGEMENTS
.LP
The \fIMH\fP system described herein is
based on the original RAND \fIMH\fP system.
It has been extensively developed (perhaps too much so) by Marshall T. Rose
and John L. Romine at the University of California, Irvine.
Einar A. Stefferud, Jerry N. Sweet,
and Terry P. Domae provided numerous suggestions
to improve the UCI version of \fIMH\fP.
.PP
Of course,
a large number of people have helped \fIMH\fP along.
The list of \*(lq\fIMH\fP immortals\*(rq is too long to list here.
For this release, numerous \fIMH\-Workers\fP sent in fixes and other
changes.  A handful of courageous \fIMH\-Workers\fP volunteered
to beta-test these changes; their help is particularly appreciated.
.KS
.SH
DISCLAIMER
.LP
The Regents of the University of California wish to make it known that:
.QP
Although each program has been tested by its contributor,
no warranty, express or implied,
is made by the contributor or the University of California,
as to the accuracy and functioning of the program
and related program material,
nor shall the fact of distribution constitute any such warranty,
and no responsibility is assumed by the contributor
or the University of California in connection herewith.
.KE
.KS
.SH
CONVENTIONS
.LP
In this document,
certain formatting conventions are adhered to:
.IP
The names of
\s-2UNIX\s+2
commands, such as \fIcomp\fP
are presented in \fIitalics\fP.
.IP
Arguments to programs, such as `msgs' and `\-nobell' are
delimited by single-quotes.
.IP
Text that should be typed exactly as-is, such as 
command lines (e.g., \*(lqfolder \-pack\*(rq),
are delimited by double-quotes.
.IP
\s-2UNIX\s+2
pathnames and envariables,
such as \fB/usr/uci\fP and \fB$SIGNATURE\fP,
are presented in \fBbold font\fP.
.KE
.ds LH Changes for MH 6.8.3
.bp
.SH 
CHANGES FOR MH 6.8.3
.LP
The MH 6.8.3 maintenance release contains few user-visible
changes.  Most of the changes are internal to the
multi-media display program \fImhn\fP to support
RFC 1521 (the new MIME standard).
This is the current version of MH as of December 1, 1993.
.SH 
Runtime Tailoring
.LP
When posting mail using the SMTP,
\fIpost\fP did not normally send the \fBHELO\fP command.  
This was because \fISendMail\fP would fail
if the host name given in the \fBHELO\fP command was the local host.
Later versions
of \fISendMail\fP will now complain if you omit the \fBHELO\fP
command.
.PP
As of MH 6.8.3a,
by default
\fIpost\fP will give the SMTP \fBHELO\fP command with
the local hostname.
If you specify a hostname with the \fBclientname:\fP option
in the \fImtstailor\fP file,
\fIpost\fP will give the \fBHELO\fP command with that name instead.
If the argument to the \fBclientname:\fP option is empty,
no \fBHELO\fP command is given.
See \fImh-tailor\fP\|(5) for more details.
.SH
User Interface Programs
.IP folder \w'msgchk'u+2n
The \fIfolder\fP command now has `\-create' and `\-nocreate'
options.  See \fIfolder\fP\|(1) for details.
.IP inc
A bug where `\-host' would not override the \fBpophost\fP
as set in the \fImtstailor\fP file has been fixed.
This bug was also fixed in \fImsgchk\fP.
.IP mhn
The \fImhn\fP command has several changes:
updates for conformance with RFC 1521,
addition of two caches: public and private,
addition of two caching policies: one for reading and one for writing,
support for storing multipart entities, and
a few bug fixes.  See \fImhn\fP\|(1) for complete details.
.SH
CHANGES FOR MH 6.8.2
.LP
The MH.6.8.2 patch release contains only
internal changes to support the BSD 4.4
and 386BSD versions of 
.SM
UNIX.
.NL
This version of \fIMH\fP 
was released August 25, 1993, but was not widely distributed.
.SH
CHANGES FOR MH 6.8.1
.LP
The MH.6.8.1 patch release is a maintenance
release.
This is the current released version of \fIMH\fP
as of August 20, 1993.
.PP
This release includes a small number
of bug fixes, a few minor enhancements, some changes
for the new MIME standard, and support for ESMTP (RFC 1425).
Support for BSD 4.4 and 386BSD is planned for the next
release.
.PP
Many other fixes which have already been received
are still being merged.
If you've sent an update for MH 6.8 to \fBBug-MH@ics.uci.edu\fP
and it isn't in this release,
it'll probably appear in the next release.
.SH
Fixes and Enhancements
.LP
Many minor documentation corrections were made.
There are also a few program changes:
.IP mhn \w'post'u+2n
The `\-cache\0policy', `\-\[no\]check', and `\-\[no\]pause'
switches have been added.   Some other minor changes have
been made to comply with the new MIME standard.
See \fImhn\fP\|(1) for complete details.
.IP post 
When posting mail with SendMail, \fIpost\fP will
not use the \fBONEX\fP command when it is posting 
a message with BCCs.
.IP scan
\fIscan\fP will now work with big width values.
.SH 
Format Strings
.LP
One new function has been added:
.IP "%(profile arg)" \w'XXprofileXargX'u+2n
This function looks up a component in the 
\fB\&.mh\(ruprofile\fR or \fBcontext\fP files
and returns the value of that component.
.SH
Configuration
.LP
Two new configuration options are present:
.IP GCOS_HACK \w'NORUSERPASS'u+2n
The so-called \*(lqgcos\*(rq field of the password file is 
used as a last resort
to find the user's full name (see \fImh-profile\fP\|(5) for details).
Enable this option
if your \fIpasswd\fP\|(5) man page notes that the `&'
character in the \*(lqgcos\*(rq field stands for the login name.
.IP NORUSERPASS
Tells \fIMH\fR that your system doesn't have the
\fIruserpass\fP\|(3) routine;
\fIMH\fR will include its own copy of this
routine in its library.
.ds LH Changes for MH 6.8
.bp
.ds CF December 14, 1992
.SH
CHANGES FOR MH 6.8
.LP
This is the current released version of \fIMH\fP
as of December 14, 1992.  This release includes a number
of bug fixes and internal changes to make the code more
portable.
Two new authentication methods are provided for the POP,
and support for SVR4 shared libraries is complete.
.PP
The major user-visible change in this release is the incorporation
of support for multi-media mail as specified by the
Multi-purpose Internet Mail Extensions (\fBMIME\fP)
RFC\ 1341.
This allows you to include things like audio,
graphics, and the like, in your mail messages.
A new command, \fImhn\fP, 
has been provided to support \fBMIME\fP and
a detailed man page is provided in \fImhn\fP\|(1).
.SH
Documentation
.LP
The documentation has some general improvements, and
the \fBREAD-ME\fP document has been re-organized
to help \fIMH\fP administrators find the appropriate
configuration options for their system.
The \fBMakefile\fPs in the \fBpapers/\fP hierarchy have
been changed to invoke \fITeX\fP as
\*(lqtex\*(rq (instead of \*(lqtex82\*(rq).
.LP
The following new man pages are also available:
.IP \fImhn\fP\|(1) \w'\fIpopauth\fP\|(8)'u+2n
\fImhn\fP helps the user process multi-media mail.
.IP \fImhparam\fP\|(1)
\fImhparam\fP lets the user extract information from 
the \fIMH\fP profile.
.IP \fIpopauth\fP\|(8)
the APOP database administration program (see below).
.IP \fIpopi\fP\|(1)
the POP initiator (see below).
.IP \fIslocal\fP\|(1)
fully documents \fIslocal\fP.  The \fImhook\fP(1) man page now
documents only the \fIMH\fP receive-mail hooks.
.SH
Internal Changes
.LP
The \fIMH\fP source code is in the process of being
cleaned up to make pedantic ANSI C compilers happy.
Occurrences of \*(lqNULL\*(rq have been replaced by
\*(lq0\*(rq where appropriate.
Extra tokens after \*(lq#else\*(rq and \*(lq#endif\*(rq
have been put inside comments (this is still in progress).
The code should now compile cleanly on many more systems,
specifically, more variants of SVR4.
.PP
The version of \fBtws/dtimep.c\fP which was included in 
MH 6.7.2 was incompatible with the \fIlex\fP library
on some systems, and has been removed.
.PP
A bug in the handling of blind lists inside alias
files has been fixed.
.KS
.SH
Post Office Protocol
.LP
There were three new options added to the POP.
.IP APOP \w'APOP'u+2n
This option indicates that the POP daemon will
support the non-standard \fBAPOP\fP command which
provides a challenge-based authentication system using
the \fBMD5\fP message digest algorithm.
.IP
This option also causes the
\fIpopauth\fP program to be installed, which
allows the administrator to manipulate the \fBAPOP\fP
authorization database.
.KE
.IP KPOP
Support for KERBEROS with POP.
This code builds
\fIpopd\fP, \fIinc\fP and \fImsgchk\fP to support only the 
\*(lqkpop\*(rq protocol.
This code is still expiremental, but is available for 
those sites wishing to test it.
.IP MPOP
This option indicates that the POP daemon will
support the non-standard
\fBXTND SCAN\fP command which provides performance
enhancements when using the POP over low-speed connections.
.IP
This option also causes an interactive POP
client program, \fIpopi\fP, to be compiled and installed.
A man page for the \fIpopi\fP program is also provided.
This option requires the configuration to
have \*(lqbboards: pop\*(rq.
.LP
The APOP and MPOP non-standard POP
facilities are documented in
\fIThe Internet Message\fR (ISBN 0\-13\-092941\-7),
a book by Marshall T. Rose.
For more details, see \fBsupport/pop/pop-more.txt\fR
and the \fIAdministrator's Guide\fP.
The APOP option peacefully co-exists with the standard POP,
KPOP completely replaces the standard POP, and
MPOP requires \*(lqbboards: pop\*(rq.
.SH
File Locking
.LP
The file locking code has been cleaned up to support
three kinds of kernel-level file locking.  As appropriate
for your system, include the
LOCKF, FCNTL or FLOCK option.  For more
details, see \fImh-tailor\fP\|(5).
.SH
.KS
Configuration Directives
.LP
A number of new configuration directives have been added
or changed.  The full details are given in the \fBREAD-ME\fP.
.IP cp: \w'MAILGROUP'u+2n
The command used to install new files if not \*(lqcp\*(rq.
.IP ln:
The command used to link files together in the source tree
if not \*(lqln\*(rq.
.IP mts:
Full support for ZMAILER has been added.
.IP popdir:
The directory where \fIpopd\fP will be installed if not \fB/usr/etc\fP.
.IP regtest:
Set to \*(lqon\*(rq to prevent the hostname and compile
date from being included in \fIMH\fP binaries.
.IP sharedlib:
You may now specify \*(lqsun4\*(rq or \*(lqsys5\*(rq 
(for SVR4) shared libraries.
.IP signal:
Specifies the base type of the function returned by \fIsignal\fP\|().
This was previously defined with \*(lqoptions TYPESIG\*(rq.
.KE
.LP
Several `-D' options to \fIcc\fP have been added or changed:
.IP APOP \w'MAILGROUP'u+2n
Authenticated POP (see above).
.IP AUX 
Support for A/UX systems.
.IP DBMPWD
The DBM option has been renamed DBMPWD.
.IP HESIOD
Support for the HESIOD name server.
.IP KPOP
KERBEROS POP (see above).
.IP LOCALE
Support for local characters sets; uses the \fIsetlocal\fP\|() function.
.IP MAILGROUP
Makes \fIinc\fP set-group-id.
You may need this option if your \fB/usr/spool/mail\fP
is not world-writeable.
.IP MIME
Multi-media mail.
.IP MPOP
Mobile POP (see above).
.IP MSGID
Enables \fIslocal\fP to detect and surpress duplicate messages.
.IP OSF1
Support for DEC OSF1 systems.  May be incomplete.
.IP RENAME
Include this option if your system has a \fIrename\fP\|()
system call.
.IP SVR4
Support for System 5 Release 4 or newer systems.
.IP TYPESIG
This option has been dropped.  See `signal' above.
.IP UNISTD
Include this option if your system has the include
file \fB<unistd.h>\fP.
.IP VSPRINTF
Include this option if your system has the \fIvsprintf\fP\|()
library routine; otherwise, \fI\(rudoprnt\fP\|() will be used.
.IP YEARMOD
Forces the \fImh-format\fP `year' function to
return 2-digit values.
Use this option during a brief transition period if 
you have local \fImh-format\fP files which need to
be converted to support 4-digit years.
.SH 
FUNCTIONAL CHANGES
.LP
In addition to the configuration changes mentioned above,
a number of functional changes have been made to the system.
Many programs have new features added and a few new 
programs have are provided.  
Each command's manual page gives
complete information about the its operation.
Here is a short summary of the changes.
.SH
MH Sequences
.LP
A larger number of user-defined sequences are available.
Previously, this number had been 10.
On 32-bit systems, 26 user-defined sequences are available.
.SH
Profile Components
.LP
\fIMH\fP programs will now complain if the 
\fB\&.mh\(ruprofile\fR does not end in a newline.
Also, one enhancement and one new profile component are provided:
.IP Aliasfile: \w'AliasfileX'u+2n
Multiple filenames may now be given.
.IP Inbox:
New; the default folder (for \fIinc\fP, etc.) if not \*(lqinbox\*(rq.
.KS
.SH
Format Strings
.LP
A few minor bugs were fixed in format string handling,
and a few new features were added.  See \fImh-format\fP\|(5)
for complete details.
.IP Addresses \w'Xxyearxdatexx'u+2n
An attempt is made to decipher X\&.400 RFC\ 987-style addresses.
.IP Comments
Comments may be added to \fImh-format\fP files; a comment
begins with the 2-character sequence \*(lq%;\*(rq,
and ends with an un-escaped newline.
.IP "%(modulo n)"
The `modulo' function escape has been added.
.IP %(year{date})
The date parser has been enhanced to understand more
illegal date formats; `year' now returns a 4-digit number.
.KE
.SH
User Interface Programs
.LP
A number of \fIMH\fP commands have minor changes:
.IP ali \w'packmbox'u+2n
The output with `\-user\0\-list' was
changed to match the output with `\-nouser\0\-list'.
.IP burst
Will no longer drop the last message of a digest.
.IP inc
Accepts the `\-apop' switch for authenticated POP (see above);
will attempt to detect write
errors (e.g., no space left on device) when incorporating mail;
no longer replaces newline characters with NULLs.
.IP folder
The `\-noprint' option was broken and has been dropped.
.IP forw
Supports `\-mime' to use MIME-style multi-part messages.
.IP mhl
Will no longer put an extra space at the end of the
`%{text}' in a formatfield.
.IP mhn
New; manipulates multi-media (MIME) messages; a detailed
man page is provided.
.IP mhparam
New; reads the \fIMH\fP profile (and context) 
and writes the values of the specified components on the
standard output; useful in programmatic constructs.
.IP msgchk
Supports `\-apop' (see above).
.IP packmbox
New; packs an \fIMH\fP folder into a UUCP-style mailbox.
.IP popi
New; a client-side POP initiator; available only if you
built \fIMH\fP with the MPOP option (see above).
.IP refile
A bug where the `rmmproc' did not remove all specified
message files has been fixed.
.IP scan
The `\-file' option is fully supported and will no longer
complain about empty folders.
.IP send
Supports `\-mime' and `\-split' to split large messages
into multiple partial messages using MIME.
.SH
Support Programs
.IP fmtdump \w'packmbox'u+2n
Can now read a format file, or a format string given
on the command line.
.IP popauth
New; manages the APOP authorization database (see above).
.IP sendmail
The \fIsendmail\fP replacement will be installed
only if your `mts' setting uses the `/smtp' option.
.IP slocal
A new man page for \fIslocal\fP is available;
the new `mbox' action is available to write a file
in \fIpackf\fP format;
a bug where extra `>' characters were written to MMDF-style
maildrops has been fixed; 
if compiled with the MSGID option, can detect and suppress
reception of duplicate messages.
.IP viamail
New; bundles a directory (like \fIshar\fP\|) and
sends it through multi-media mail.

.ds LH Changes for MH 6.7.2
.bp
.ds CF Feb 1, 1992
.SH
CHANGES FOR MH 6.7.2
.LP
The MH.6.7.2 patch release is a maintenance
release.
This is the
current released version of \fIMH\fP as of February 1, 1992.
.PP
This release now supports the NCR Tower running SYS5R4.
The WP changes installed in MH.6.7.0 have been removed.
.SH 
Shared Libraries
.LP
Support for SYS 5 shared libraries is in progress.
.PP
Support for Sun OS 4.0 shared libraries had been improved.
The \fIMH\fP library has been modified to move initialized
data into a data definition file.  The shared library will
now consist of a \fBlibmh.so\fP and \fBlibmh.sa\fP file.
The shared library version number will no longer track the
\fIMH\fP patch release number, and its numbering begins with
version `1.1' with this release.
.SH
Replacement SendMail
.LP
Since many standard system programs expect to post mail by 
invoking \fB/usr/lib/sendmail\fP,
a minimal replacement \fISendMail\fP is provided in 
this release.  This replacement is meant to be installed
on (e.g., diskless) client workstations which post mail
using SMTP, and do not run a message transport system.
It will call \fIpost\fP to post mail; be sure you have
configured \fIMH\fP with the `/smtp' mts option.
This sendmail replacement is installed in your 
\fIMH\fP etc directory, and you should link 
\fB/usr/lib/sendmail\fP
to it.
.KS
.SH
Format Strings
.LP
A manual page for the \fIfmtdump\fP format string disassembler
is supplied, and some new format functions were added:
.IP folder \w'%getenv'u+2n
In \fIscan\fP, this component escape
contains the name of the current folder.
It is not defined for other \fIMH\fP commands.
.IP getenv
This function escape returns the value of an environment variable.
.KE
.PP
There will be some additional changes in these routines in the
next patch release.
.KS
.SH
Other Bug Fixes and Enhancements
.LP
In addition to some other minor enhancements,
some bugs were fixed which in general were not user\-visible:
.IP "Blind lists" \w'datexparsing'u+2n
Users may now specify RFC822 address groups in their
alias files.  These groups are implemented by \fIMH\fP 
as blind lists.
.IP "date parsing"
A number of sites have brain-damaged versions of \fBlex\fP.
\fIMH\fP will now come with the date parser already run
through lex.
.IP mark
A bug dealing with \fImark\fP and the sequence named `cur'
is fixed.  This was previously a problem for mh-e users.
.IP MH.doc
The \fIMH\fP nroff version of the manual no longer contains
teletype escape sequences.
.IP scan
Can now handle headers as long as 512 bytes.
.IP Signals
\fIMH\fP programs will no longer catch the \fBHUP\fP
and \fBTERM\fP signals while waiting for a sub-process.
This was causing hung processes when your terminal line was
was dropped unexpectedly.
.IP Signature
If your signature is not defined, \fIMH\fP will 
use the value of the gecos field of your \fB/etc/passwd\fP
entry as your signature.
.IP "version.sh"
A bug in the \fBawk\fP script in \fBconfig/version.sh\fP 
was fixed.
.KE
.ds LH Changes for MH 6.7.1a
.bp
.ds CF January 25, 1991
.SH 
CHANGES FOR MH 6.7.1a
.LP
The MH.6.7.1a patch was made available 
on January 25, 1991 for limited distribution only.
(This release had some known bugs, and so was
not widely distributed.)
This release incorporates several new features
of particular note to users of sequences and format strings,
as well as some general documentation improvements.
There are a few minor enhancements and internal bug fixes also.
Complete documentation of these changes is given in
the individual manual pages, and the \fBREAD-ME\fP file.
.SH
Message Sequences
.LP
A new manual page, \fImh\-sequence\0\fP(5), has been added.
This manual page attempts to completely document the
syntax and semantics of \fIMH\fP message sequence specifications.
.PP
A powerful new feature is the ability to specify message
ranges with user-defined sequences.  The  specification
\*(lqname:n\*(rq may be used, and it designates up to the
first `n' messages (or  last  `n' messages  for  `-n')
which  are  elements  of the user-defined sequence `name'.
.PP
The message
specifications \*(lqname:next\*(rq and \*(lqname:prev\*(rq
may also be used, and they
designate the
next or previous message (relative to the current message)
which is an element of the user-defined sequence `name'.
The specifications
\*(lqname:first\*(rq and \*(lqname:last\*(rq are equivalent
to \*(lqname:1\*(rq and \*(lqname:\-1\*(rq, respectively.
The specification \*(lqname:cur\*(rq is not allowed
(use just \*(lqcur\*(rq instead).
.PP
These specifications allow the user to step through
a sequence with a command like \*(lqshow name:next\*(rq.
.SH
Format Strings
.LP
\fIMH\fP format strings now support an if-then-elseif-else
clause (the `elseif' is new).  This will make
format strings with multi-case conditions somewhat less complex.
.PP
A new format function `addr' had been added.  This function
takes an address header name as its argument, and returns
a rendering of the address contained in that header
as \*(lquser@host\*(rq or \*(lqhost!user\*(rq.
.PP
Format widths now may be specified as a negative number.
This causes the output to be right-justified
within the format width.
.KS
.SH
Other Changes
.LP
Along with a few minor enhancements,
some bugs were fixed which in general were not user-visible:
.IP "fmtdump" \w'whatnow'u+2n
This new program 
produces an pseudo-language
representation of an \fIMH\fP format file, vaguely
reminiscent of assembly language.  While this output format
is not explicitly documented,
it can still be useful when debugging \fIMH\fP format files.
.IP "refile"
Now takes a `\-\[no\]rmmproc' switch.  This makes it
easier to avoid loops when your \*(lqrmmproc\*(rq calls \fIrefile\fP.
.IP "slocal"
A problem with the UUCP-style mailboxes,
the `RPATHS' configuration option,
and the \*(lqReturn-Path:\*(rq header was fixed.
.IP "sortm"
Will ensure that no messages are lost if it is interrupted.
.IP "whatnow"
Will now tell you where it is leaving the draft, when
interrupted in the initial edit.  Previously the draft 
was simply unlinked.
.KE
.KS
.SH
Compilation Options
.IP "LOCKF" \w'whatnow'u+2n
This option causes \fIMH\fP to use the \fBlockf()\fP
system call for locking (if available),
instead of \fBflock()\fP.
.KE
.ds LH Changes for MH 6.7.1
.bp
.ds CF December 14, 1990
.SH 
CHANGES FOR MH 6.7.1
.LP
The MH.6.7.1 patch release is a maintenance
release, and as such, provides few changes from 
the previous release.  This is the
current released version of \fIMH\fP as of December 14, 1990.
.SH
User-Visible Changes
.LP
The major change in this release is to the
POP daemon (popd).  In \fIMH\fP 6.7,
it was changed to be able to read both UUCP and
MMDF-style mailboxes.  This did not work as reported.  The
code has now been changed to parse MMDF-style mailboxes if
you are configuring MH to run with MMDF as your message
transport system.  Otherwise, UUCP-style mailboxes are
expected.
.PP
Since there are number of client programs available for
only the POP2 protocol instead of POP3, popd has been
updated to support both protocols.  This is a major
win.  If you are compiling
with POP turned on, add the `POP2' option to
your \fIMH\fP config file, and the POP daemon
will respond to POP2 or POP3 commands.  If you're using
POP, there's no reason not to include this option; it does
not affect the existing support for POP3.
.KS
.SH
Internal Changes
.LP
Some bugs were fixed which in general were not user-visible:
.IP "context" \w'replnnetcn'u+2n
Errors when writing out sequences are detected correctly.
.IP "inc"
No longer inserts extra blank lines into messages.
.IP "mh-format"
A nil pointer bug in the address parser was fixed.
.IP "repl, etc."
The malloc/free problem has been fixed.
.IP "rmf"
A spelling error in the `\-nointeractive' switch has been corrected.
.IP "rcvtty"
Will not print the message size if not available (i.e., zero).
.IP "send/post"
Illegal signatures (those containing unquoted "."s) will be quoted.
.KE
.ds LH Changes for MH 6.7.0
.bp
.ds CF April 12, 1990
.SH
GENERAL CHANGES FOR MH 6.7.0
.LP
The author is pleased to announce that there are very few
user\-visible
changes to \fIMH\fP 6.7 from the previous \fIMH\fP 6.6 distribution.
The majority of development was in the form of bug fixes and
slight enhancements.
In addition, this release is slightly faster than the
previous release.
With a few minor exceptions,
it is backward\-compatible with the previous release.
\fIMH\fP 6.7.0 is the current released version of \fIMH\fP
as of April 12, 1990.
.PP
The changes were made mainly to generalize the source code to
be compatible with a larger range of systems and compilers.
There were many small changes to add declarations for ANSI C compliance.
The System 5 support has been brought up to SYS5 R3, and there is
support for Sun OS 4.0.
.SH
User\-Visible Changes
.LP
Here a quick summary of the
changes that were made which are not backward\-compatible with the
previous release of \fIMH\fP:
.IP repl \w'sortm'u+2n
The `\-format' and `\-noformat' switches
have not been functional since \fIMH\fP 5, and have been removed.
Any users who have these switches in their \fB\&.mh\(ruprofile\fP,
will have to remove them.
.IP sortm
Previously, in most cases \fIsortm\fP would fill\-in any
gaps in the numbering of a folder, 
by renumbering the messages starting with `1'.
This will no longer occur; for this behavior,
use \*(lqfolder \-pack\*(rq.
.PP
.SH
Using Aliases
.LP
A new profile entry `Aliasfile:' has been added.  The
\fIali\fP\^, \fIsend\fP\^, and \fIwhom\fP programs will look for
this profile entry and treat it as they would an argument to
`\-alias'.
This should make it easier for novice \fIMH\fP users to begin
using aliases.
.PP
.SH
Reading Network News & BBoards
.LP
The UCI BBoards facility can read local BBoards, and if compiled
with the `bboards: pop' and `pop: on' options, can also read remote
BBoards using the Post Office Protocol (POP ver. 3).
With this release,
\fIMH\fP can instead be compiled to read the Network News
(i.e., USENET) using the Network News Transfer Protocol (NNTP).
.PP
This capability is enabled by compiling \fIMH\fP with
the `bboards: nntp' and `pop: on' options.
Unfortunately, reading remote BBoards via the POP and reading the Network
News via the NNTP are mutually exclusive options.
.PP
To support the NNTP,
a new module, \fBuip/pshsbr.c\fP, is compiled and loaded into
\fIbbc\fP and \fImsh\fP instead of \fBuip/popsbr.c\fP.
The default BBoard is changed from \*(lqsystem\*(rq to \*(lqgeneral\*(rq
for the NNTP.
.PP
When reading BBoards,
\fIbbc\fP will first look for local BBoards, and then contact the
NNTP server to read the Network News.  The location of the
NNTP server should be specified with the `nntphost:'
entry in the \fBmtstailor\fP file
(see the \fIMH\fP Administrator's Guide for details),
or may be specified on
the command line with the `\-host' switch.
.PP
.SH
Format Strings
.LP
The manual page \fImh\-format\fP\0(5) has
been rewritten to give a better explanation of how to write format strings,
and how they are interpreted by \fIMH\fP.
A line\-by\-line
description of the 
default \fIrepl\fP form file (\fBreplcomps\fP)
is now included in that manual page.
.PP
.KS
Some new format functions were added, and others were augmented:
.IP trim \w'date2local'u+2n
Strips any leading and trailing white\-space from the current string value.
.IP date2local
Will coerce the date to the local timezone.
.IP date2gmt
Will coerce the date to GMT.
.IP divide
Divides the current numeric value by its argument.
This could be useful for
building \fIscan\fP format strings which print large
message sizes in \*(lqKb\*(rq or \*(lqMb\*(rq.
.IP friendly
If the address field cannot be parsed,
this function will return the text of the address header,
instead of a null string.
.IP szone
A flag indicating whether the timezone was explicit
in the date string.
.KE
.SH
PROGRAM CHANGES
.LP
In addition to the general changes mentioned above,
many programs have specific new features added,
either by new switches or by expanded functionality.
Each command's manual page gives complete information about
its new options.  Here is a short summary.
.SH
User Interface Programs
.IP anno \w'prompter'u+2n
Accepts a `\-nodate' switch which inhibits the date annotation,
leaving only the body annotation.
.IP folder
When invoked with the `\-pack' switch
and the new `\-verbose' switch, \fIfolder\fP will
give information about the actions taken to renumber the folder.
.IP
On most systems, \fIfolder\fP can now create any non\-existing
parent folders of a new sub\-folder.
.IP forw
When making digests, \fIforw\fP\^
will put the issue and volume numbers
in addition to the digest list name, in the digest trailer.
.IP inc
Detects NFS write failures, and will
not zero your maildrop in that event.
.IP msh
Supports a variant of the new \fIsortm\fP\^.
.IP prompter
Considers a period on a line by itself to signify end\-of\-file
when the `\-doteof' switch is specified.
.IP repl
The `\-[no]format' switches
have not been used since \fIMH\fP 5 and have been deleted. \fIrepl\fP
will now find filter files in the \fIMH\fP library area.
.IP scan
With the `\-file msgbox' switch, \fIscan\fP\^
can list a \fIpackf\fP\|'d\-format file directly
(without using \fImsh\fP\^).
.IP
Lists messages in reverse order with the `\-reverse' switch.
This should be considered a bug.
.IP sortm
Now has the options:
`\-textfield field', `\-notextfield',
`\-limit days', and `\-nolimit'.
.IP
With these options, \fIsortm\fP can be instructed to sort a
folder based on the contents of an arbitrary header such
as \*(lqsubject\*(rq.
.IP
\fIsortm\fP minimizes renaming messages, and
will no longer arbitrarily pack folders; for this behavior,
use \*(lqfolder \-pack\*(rq.
.IP whatnow
Deletes the draft by renaming it with leading comma, instead
of unlinking it.
.SH
\fIMH\fP Support Programs
.LP
.KS
The following support programs also have changes or enhancements:
.IP mhl \w'rcvtty'u+2n
Will now accept a format string on any component, not just on
addresses and dates.
.KE
.IP popd
Will use \fIshadow\fP passwords if compiled with the
\fBSHADOW\fP option.  It can now also
read UUCP\-style maildrops directly.
.IP rcvtty
If given no arguments, \fIrcvtty\fP will produce a \fBscan\fP
listing as specified by a format string or file; a default
format string is used if one is not specified.
.IP
Before the listing is written to the users terminal, the
terminal's bell is rung and a newline is output.
The `\-nobell' and the `\-nonewline' options
inhibit these functions.
.IP
\fIrcvtty\fP will obey terminal write
notification set by \fImesg\fP.
With the `\-biff' switch, \fIrcvtty\fP will
also obey the mail notification status set by
\fIbiff\fP.  
.IP
On \fBBSD43\fP systems, as with \fIwrite\fP,
\fIrcvtty\fP will be installed set\-group\-id to the group \*(lqtty\*(rq.
.IP slocal
Understands UUCP\-style \*(lqFrom \*(rq lines and will write
output files using this format if appropriate.
Before invoking a delivery program, \fIslocal\fP will strip
such lines unless compiled with the \fBRPATHS\fP option,
in which case it will will convert such lines into
\*(lqReturn\-Path:\*(rq headers.
.IP
\fIslocal\fP has a new result code \*(lqN\*(rq,
for use in \fB.maildelivery\fP files.
With this result code,
\fIslocal\fP will
perform the action only if the message has not been delivered
and the previous action succeeded.  This allows for performing
an action only if multiple conditions are true.
.SH
DOCUMENTATION
.LP
Several of the older \fIMH\fP papers have been difficult to format
because they depended on an older version of
PhDTeX which was
not supplied.  These papers have been updated, and some TeX
library files are supplied in \fBpapers/doclib/\fP, so that
these papers may be generated on any system with TeX.
.PP
Many of the manual pages have been revised to include documentation
of new command options, and some have been expanded to give more detail.
All are now slightly reformatted at installation time
to make them more compatible with programs like \fImakewhatis\fP\^.
.PP
.SH
\fIMH\fP ADMINISTRATION
.LP
This section describes changes in configuring, compiling and
installing \fIMH\fP 6.7 and should not
be of interest to casual \fIMH\fP users.
The \fBREAD\-ME\fP file has been 
considerably revised and expanded to give more detail 
about the configuration and compilation options which
have been included in this release.  Some compilation options
have been removed, and many new options have been added.
.PP
All \fIMH\fP \fBMakefile\fPs have been updated to work around some
incompatibilities introduced in newer versions of \fImake\fP\^.
\fIMH\fP programs will no longer be installed with the sticky\-bit
turned on.
.PP
Reading this section not a substitute for carefully
reading the \fBREAD\-ME\fP file before attempting to compile \fIMH\fP
.PP
.KS
.SH
Bug Fixes
.LP
Some bugs were fixed which in general were not user\-visible:
.IP "address parser" \w'dynamicnmemory'u+2n
Fixed to allow use of the \*(lqAT\*(rq domain, and some
minor bugs were fixed pertaining to address groups.
.IP "date parser"
Improved to accept more forms of illegal dates.  Military
timezones were removed.
.IP "dynamic memory"
Many problems with corruption of the dynamic memory pool
have been fixed.
.IP locking
Will open files for write, if necessary to enable locking.
.IP "nil pointers"
All reported nil pointer problems have been fixed.
.IP replcomps
The \*(lqIn\-Reply\-To:\*(rq header had quotes added
around the date field to comply with RFC822.
.KE
.SH
White Pages
.LP
If \fIMH\fP is compiled with the \fBWP\fP option,
\fIsend\fP recognizes an address between \*(lq<<\*(rq and \*(lq>>\*(rq characters
such as:
.DS
To: << rose \-org psi >>
.DE
to be a name meaningful to a whitepages service.  In order to expand the
name, \fIsend\fP must be invoked interactively
(i.e., not from \fIpush\fP\^).  For each
name, \fIsend\fP will invoke a command called \fIfred\fP\^
in a special mode asking to expand the name.
.PP
To get a copy of the white pages service, contact wpp\-manager@psi.com.
.SH
Configuration Options
.LP
Some configuration options have been added or changed:
.IP cc \w'bbdelivery'u+2n
To specify an alternate C compiler.
.IP ccoptions
Defaults to `\-O'.
.IP bboards
May now be defined as \*(lqon\*(rq,
\*(lqoff\*(rq, \*(lqpop\*(rq, or \*(lqnntp\*(rq.
.IP bbdelivery
Determines whether the bboard delivery agent
and library files should be installed.
.IP lex
To specify an alternate version of \fIlex\fP\^.
.IP mailgroup
If defined, \fIinc\fP will be made set\-group\-id to this group.
.IP sharedlib
For \fBSUN40\fP systems; if \*(lqon\*(rq, makes \fBlibmh.a\fP
into a shared library.
.IP slibdir
The directory where the above shared library should be installed.
.IP sprintf
Set this to \*(lqint\*(rq if that's what your \fIsprintf\fP\0(3) library
routine returns.
.SH
Compilation Options
.LP
For different configurations,
several `\-D' options to \fIcc\fP have been added or changed:
.IP BERK \w'SENDMAILBUG'u+1n
This disables the address and date parsing routines.  If you
want to do much with \fImh\-format\fP\0(5), don't enable this.
.IP BSD43
Will make \fIrcvtty\fP set\-group\-id to the group \*(lqtty\*(rq.
.IP DBM
For sites with a dbm\-style password file (such as with Yellow
Pages), \fIMH\fP will not read the entire passwd file into a cache.
At one site that runs YP on a large passwd file, using this
showed a 6:1 performance improvement.
.IP NETWORK
This option has been deleted.  See \fBSOCKETS\fP.
.IP NOIOCTLH
Tells \fIMH\fP not to include the file \fBsys/ioctl.h\fP.  Use this
if this file is not present on your system.
.IP NTOHLSWAP
On systems with TCP/IP networking,
\fImsh\fP will try to use the \fBntohl()\fP macro from the
file \fBnetinet/in.h\fP to byte\-swap the binary map files
it writes.
.IP SENDMAILBUG
Some versions of \fIsendmail\fP return a \fB451\fP (failure) reply code
when they don't mean to indicate failure.  This option considers
that code to be equivalent to \fB250\fP (OK).
.IP SHADOW
Causes \fIpopd\fP to read the file \fB/etc/shadow\fP for
encrypted passwords instead of \fB/etc/passwd\fP.  Use this if you
have a shadow password file (such as on newer versions of SYSTEM 5).
.IP SOCKETS
Enable this if you are on a non\-BSD system with a
socket interface for TCP/IP networking compatible with 4.2BSD
.SM
UNIX.
.NL
.IP SUN40
Use on Suns running Sun OS 4.0 and later.
.IP SYS5
This option has been updated to refer to SYS5 R3 and later systems.
.IP SYS5DIR
Use this if your system uses \*(lqstruct dirent\*(rq instead of
\*(lqstruct direct\*(rq.  This should be true for systems based
on SYS5 R3 and later.
.IP TYPESIG
Defines the base type for the \fIsignal\fP system call.  This
defaults to \*(lqint\*(rq, but should be defined as \*(lqvoid\*(rq
if appropriate for your system.
.IP WP
Enables support for the White Pages service.
.SH
Installation
.LP
\fIMH\fP will now
explicitly set the protection mode on every file it installs.
.PP
Previously any existing file installed by \fIMH\fP
would be backed up into
the source tree, and then overwritten.
Now, a few system\-dependent files will not be overwritten, and your
changes will have to be merged in by hand.
See the \fBREAD\-ME\fP file for more details.
