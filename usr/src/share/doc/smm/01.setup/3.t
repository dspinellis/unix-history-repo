.\" Copyright (c) 1980, 1986, 1988, 1993
.\"	 The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)3.t	6.9 (Berkeley) %G%
.\"
.ds lq ``
.ds rq ''
.ds RH "Upgrading a \*(Ps System
.ds CF \*(Dy
.NH 1
Upgrading a \*(Ps System
.PP
This section describes the procedure for upgrading a \*(Ps
system to \*(4B.  This procedure may vary according to the version of
the system running before conversion.
If you are converting from a
System V system, some of this section will still apply (in particular,
the filesystem conversion).  However, many of the system configuration
files are different, and the executable file formats are completely
incompatible.
.NH 2
Installation overview
.PP
If you are running \*(Ps, upgrading your system
involves replacing your kernel and system utilities.
In general, there are three possible ways to install a new \*(Bs distribution:
(1) boot directly from the distribution tape, use it to load new binaries
onto empty disks, and then merge or restore any existing configuration files
and filesystems;
(2) use an existing \*(Ps or later system to extract the root and
.Pn /usr
filesystems from the distribution tape,
boot from the new system, then merge or restore existing
configuration files and filesystems; or
(3) extract the sources from the distribution tape onto an existing system,
and use that system to cross-compile and install \*(4B.
For this release, the second alternative is strongly advised if at all possible,
with the third alternative reserved as a last resort.
In general, older binaries will continue to run under \*(4B,
but there are many exceptions that are on the critical path
for getting the system running.
Ideally, the new system binaries (root and
.Pn /usr
filesystems) should be installed on spare disk partitions,
then site-specific files should be merged into them.
Once the new system is up and fully merged, the previous root and
.Pn /usr
filesystems can be reused.
Other existing filesystems can be retained and used,
except that (as usual) the new \fIfsck\fP should be run
before they are mounted.
.PP
It is \fBSTRONGLY\fP advised that you make full dumps of each filesystem
before beginning, especially any that you intend to modify in place
during the merge.
It is also desirable to run filesystem checks
of all filesystems to be converted to \*(4B before shutting down.
This is an excellent time to review your disk configuration
for possible tuning of the layout.
Most systems will need to provide a new filesystem for system use
mounted on
.Pn /var
(see below).
However, the
.Pn /tmp
filesystem can be an MFS virtual-memory-resident filesystem,
potentially freeing an existing disk partition.
(Additional swap space may be desirable as a consequence.)
See
.Xr mfs (8).
.PP
The recommended installation procedure includes the following steps.
The order of these steps will probably vary according to local needs.
.IP \(bu
Extract root and
.Pn /usr
filesystems from the distribution tapes.
.IP \(bu
Extract kernel and/or user-level sources from the distribution tape
if space permits.
This can serve as the backup documentation as needed.
.IP \(bu
Configure and boot a kernel for the local system.
This can be delayed if the generic kernel from the distribution
supports sufficient hardware to proceed.
.IP \(bu
Build a skeletal
.Pn /var
filesystem (see
.Xr mtree (8)).
.IP \(bu
Merge site-dependent configuration files from
.Pn /etc
and
.Pn /usr/lib
into the new
.Pn /etc
directory.
Note that many file formats and contents have changed; see section 3.4
of this document.
.IP \(bu
Copy or merge files from
.Pn /usr/adm ,
.Pn /usr/spool ,
.Pn /usr/preserve ,
.Pn /usr/lib ,
and other locations into
.Pn /var .
.IP \(bu
Merge local macros, dictionaries, etc. into
.Pn /usr/share .
.IP \(bu
Merge and update local software to reflect the system changes.
.IP \(bu
Take off the rest of the morning, you've earned it!
.PP
Section 3.2 lists the files to be saved as part of the conversion process.
Section 3.3 describes the bootstrap process.
Section 3.4 discusses the merger of the saved files back into the new system.
Section 3.5 provides general hints on possible problems to be
aware of when converting from \*(Ps to \*(4B.
.NH 2
Files to save
.PP
The following list enumerates the standard set of files you will want to
save and suggests directories in which site-specific files should be present.
This list will likely be augmented with non-standard files you
have added to your system.
If you do not have enough space to create parallel
filesystems, you should create a \fItar\fP image of the
following files before the new filesystems are created.
The rest of this subsection describes where theses files
have moved and how they have changed.
.TS
l c l.
/.cshrc	\(dg	root csh startup script (moves to \f(CW/root/.cshrc\fP)
/.login	\(dg	root csh login script (moves to \f(CW/root/.login\fP)
/.profile	\(dg	root sh startup script (moves to \f(CW/root/.profile\fP)
/.rhosts	\(dg	for trusted machines and users (moves to \f(CW/root/.rhosts\fP)
/etc/disktab	\(dd	in case you changed disk partition sizes
/etc/fstab	*	disk configuration data
/etc/ftpusers	\(dg	for local additions
/etc/gettytab	\(dd	getty database
/etc/group	*	group data base
/etc/hosts	\(dg	for local host information
/etc/hosts.equiv	\(dg	for local host equivalence information
/etc/inetd.conf	*	Internet services configuration data
/etc/networks	\(dg	for local network information
/etc/passwd	*	user data base
/etc/printcap	*	line printer database
/etc/protocols	\(dd	in case you added any local protocols
/etc/rc	*	for any local additions
/etc/rc.local	*	site specific system startup commands
/etc/remote	\(dg	auto-dialer configuration
/etc/services	\(dd	for local additions
/etc/syslog.conf	*	system logger configuration
/etc/securettys	*	merged into ttys
/etc/ttys	*	terminal line configuration data
/etc/ttytype	*	merged into ttys
/etc/termcap	\(dd	for any local entries that may have been added
/lib	\(dd	for any locally developed language processors
/usr/dict/*	\(dd	for local additions to words and papers
/usr/include/*	\(dd	for local additions
/usr/lib/aliases	*	mail forwarding data base (moves to \f(CW/etc/aliases\fP)
/usr/lib/crontab	*	cron daemon data base (moves to \f(CW/etc/crontab\fP)
/usr/lib/lib*.a	\(dg	for locally libraries
/usr/lib/sendmail.cf	*	sendmail configuration (moves to \f(CW/etc/sendmail.cf\fP)
/usr/lib/tmac/*	\(dd	for locally developed troff/nroff macros (moves to \f(CW/usr/share/tmac/*\fP)
/usr/lib/uucp/*	\(dg	for local uucp configuration files
/usr/man/manl	*	for manual pages for locally developed programs (moves to \f(CW/usr/local/man\fP)
/usr/spool/*	\(dg	for current mail, news, uucp files, etc. (moves to \f(CW/var/spool\fP)
/usr/src/local	\(dg	for source for locally developed programs
/sys/conf/HOST	\(dg	configuration file for your machine (moves to \f(CW/sys/<arch>/conf\fP)
/sys/conf/files.HOST	\(dg	list of special files in your kernel (moves to \f(CW/sys/<arch>/conf\fP)
/*/quotas	*	filesystem quota files (moves to \f(CW/*/quotas.user\fP)
.TE
.DS
\(dg\|Files that can be used from \*(Ps without change.
\(dd\|Files that need local modifications merged into \*(4B files.
*\|Files that require special work to merge and are discussed in section 3.3.
.DE
.NH 2
Installing \*(4B
.PP
The next step is to build a working \*(4B system.
This can be done by following the steps in section 2 of
this document for extracting the root and
.Pn /usr
filesystems from the distribution tape onto unused disk partitions.
For the SPARC, the root filesystem dump on the tape could also be
extracted directly.
For the HP300 and DecStation, the raw disk image can be copied
into an unused partition and this partition can then be dumped
to create an image that can be restored.
The exact procedure chosen will depend on the disk configuration
and the number of suitable disk partitions that may be used.
It is also desirable to run filesystem checks
of all filesystems to be converted to \*(4B before shutting down.
In any case, this is an excellent time to review your disk configuration
for possible tuning of the layout.
Section 4.2 and \fIconfig\fP(8) are required reading.
.PP
The \*(4B bootstrap routines pass the identity of the boot device
through to the kernel.
The kernel then uses that device as its root filesystem.
Thus, for example, if you boot from
.Pn /dev/\*(Dk1a ,
the kernel will use
.Pn \*(Dk1a
as its root filesystem. If
.Pn /dev/\*(Dk1b
is configured as a swap partition, 
it will be used as the initial swap area,
otherwise the normal primary swap area (\c
.Pn /dev/\*(Dk0b )
will be used.
The \*(4B bootstrap is backward compatible with \*(Ps,
so you can replace your old bootstrap if you use it
to boot your first \*(4B kernel.
.PP
Once you have extracted the \*(4B system and booted from it,
you will have to build a kernel customized for your configuration.
If you have any local device drivers,
they will have to be incorporated into the new kernel.
See section 4.1.3 and ``Building 4.3BSD UNIX Systems with Config'' (SMM:2).
.PP
If converting from \*(Ps, your old filesystems should be converted.
If you've modified the partition
sizes from the original \*(Ps ones, and are not already using the
\*(4B disk labels, you will have to modify the default disk partion
tables in the kernel.  Make the necessary table changes and boot
your custom kernel \fBBEFORE\fP trying to access any of your old
filesystems!  After doing this, if necessary, the remaining filesystems
may be converted in place by running the \*(4B version of
.IR fsck (8)
on each filesystem and allowing it to make the necessary corrections.
The new version of \fIfsck\fP is more
strict about the size of directories than the version supplied with \*(Ps.
Thus the first time that it is run on a \*(Ps filesystem,
it will produce messages of the form:
.DS
\fBDIRECTORY ...: LENGTH\fP xx \fBNOT MULTIPLE OF 512 (ADJUSTED)\fP
.DE
Length ``xx'' will be the size of the directory;
it will be expanded to the next multiple of 512 bytes.
The new \fIfsck\fP will also set default \fIinterleave\fP and
\fInpsect\fP (number of physical sectors per track) values on older
filesystems, in which these fields were unused spares; this correction
will produce messages of the form:
.DS
\fBIMPOSSIBLE INTERLEAVE=0 IN SUPERBLOCK (SET TO DEFAULT)\fP*
\fBIMPOSSIBLE NPSECT=0 IN SUPERBLOCK (SET TO DEFAULT)\fP
.DE
.FS
* The defaults are to set \fIinterleave\fP to 1 and
\fInpsect\fP to \fInsect\fP.
This is correct on most drives;
it affects only performance (and in most cases, virtually unmeasurably).
.FE
Filesystems that have had their interleave and npsect values
set will be diagnosed by the old \fIfsck\fP as having a bad superblock;
the old \fIfsck\fP will run only if given an alternate superblock
(\fIfsck \-b32\fP),
in which case it will re-zero these fields.
The \*(4B kernel will internally set these fields to their defaults
if fsck has not done so; again, the \fI\-b32\fP option may be
necessary for running the old \fIfsck\fP.
.PP
In addition, \*(4B removes several limits on filesystem sizes
that were present in \*(Ps.
The limited filesystems
continue to work in \*(4B, but should be converted
as soon as it is convenient
by running \fIfsck\fP with the \fI\-c 2\fP option.
The sequence \fIfsck \-p \-c 2\fP will update all of them,
fix the interleave and npsect fields,
fix any incorrect directory lengths,
expand maximum uid's and gid's to 32-bits,
place symbolic links less than 60 bytes into their inode,
and fill in directory type fields all at once.
The new filesystem formats are incompatible with older systems.
If you wish to continue using these filesystems with the older
systems you should make only the compatible changes using
\fIfsck \-c 1\fP.
.NH 2
Merging your files from \*(Ps into \*(4B
.PP
When your system is booting reliably and you have the \*(4B root and
.Pn /usr
filesystems fully installed you will be ready
to continue with the next step in the conversion process,
merging your old files into the new system.
.PP
If you saved the files on a \fItar\fP tape, extract them
into a scratch directory, say
.Pn /usr/convert :
.DS
\fB#\fP \fImkdir /usr/convert\fP
\fB#\fP \fIcd /usr/convert\fP
\fB#\fP \fItar xp\fP
.DE
.PP
The data files marked in the previous table with a dagger (\(dg)
may be used without change from the previous system.
Those data files marked with a double dagger (\(dd) have syntax 
changes or substantial enhancements.
You should start with the \*(4B version and carefully
integrate any local changes into the new file.
Usually these local modifications can be incorporated
without conflict into the new file;
some exceptions are noted below.
The files marked with an asterisk (*) require
particular attention and are discussed below.
.PP
The most immediately obvious change in \*(4B is the reorganization
of the system filesystems.
Users of certain recent vendor releases have seen this general organization,
although \*(4B takes the reorganization a bit further.
The directories most affected are
.Pn /etc ,
which now contains only system configuration files;
.Pn /var ,
a new filesystem containing per-system spool and log files; and
.Pn /usr/share,
which contains most of the text files shareable across architectures
such as documentation and macros.
System administration programs formerly in
.Pn /etc
are now found in
.Pn /sbin
and
.Pn /usr/sbin .
Various programs and data files formerly in
.Pn /usr/lib
are now found in
.Pn /usr/libexec
and
.Pn /usr/libdata ,
respectively.
Administrative files formerly in
.Pn /usr/adm
are in
.Pn /var/account
and, similarly, log files are now in
.Pn /var/log .
The directory
.Pn /usr/ucb
has been merged into
.Pn /usr/bin ,
and the sources for programs in
.Pn /usr/bin
are in
.Pn /usr/src/usr.bin .
Other source directories parallel the destination directories;
.Pn /usr/src/etc
has been greatly expanded, and
.Pn /usr/src/share
is new.
The source for the manual pages, in general, are with the source
code for the applications they document.
Manual pages not closely corresponding to an application program
are found in
.Pn /usr/src/share/man .
The locations of all man pages is listed in
.Pn /usr/src/share/man/man0/man[1-8] .
The manual page
.Xr hier (7)
has been updated and made more detailed;
it is included in the printed documentation.
You should review it to familiarize yourself with the new layout.
.PP
A new utility,
.Xr mtree (8),
is provided to build and check filesystem hierarchies
with the proper contents, owners and permissions.
Scripts are provided in
.Pn /etc/mtree
(and
.Pn /usr/src/etc/mtree )
for the root,
.Pn /usr
and
.Pn /var
filesystems.
Once a filesystem has been made for
.Pn /var ,
.I mtree
should be used to create a directory hierarchy there.
.NH 3
Changes in the
.Pn /etc
filesystem
.PP
The
.Pn /etc
directory now contains nearly all of the host-specific configuration
files.
Note that some file formats have changed,
and those configuration files containing pathnames are nearly all affected
by the reorganization.
See the examples provided in
.Pn /etc
(installed from
.Pn /usr/src/etc )
as a guide.
The following table lists some of the local configuration files
whose locations and/or contents have changed.
.DS I .3i
.TS
l l l
lfC lfC l.
4.3BSD and Earlier	\*(4B	Comments
_	_	_
/etc/fstab	/etc/fstab	new format; see below
/etc/inetd.conf	/etc/inetd.conf	pathnames of executables changed
/etc/printcap	/etc/printcap	pathnames changed
/etc/syslog.conf	/etc/syslog.conf	pathnames of log files changed
/etc/ttys	/etc/ttys	pathnames of executables changed
/etc/passwd	/etc/master.passwd	new format; see below
/usr/lib/sendmail.cf	/etc/sendmail.cf	changed pathnames
/usr/lib/aliases	/etc/aliases	may contain changed pathnames
/etc/*.pid	/var/run/*.pid	
	
.T&
l l l
lfC lfC l.
New in 4.3BSD-Tahoe	\*(4B	Comments
_	_	_
/usr/games/dm.config	/etc/dm.conf	configuration for games (see \fIdm\fP\|(8))
/etc/zoneinfo/localtime	/etc/localtime	timezone configuration
/etc/zoneinfo	/usr/share/zoneinfo	timezone configuration
.TE
.ne 1.5i
.TS
l l l
lfC lfC l.
	New in \*(4B	Comments
_	_	_
	/etc/man.conf	lists directories searched by \fIman\fP\|(1)
	/etc/kerberosIV	Kerberos directory; see below
.TE
.DE
.PP
System security changes require adding several new ``well-known'' groups 
to /etc/group.
The groups that are needed by the system as distributed are:
.DS
.TS
l n l.
name	number	purpose
_
wheel	0	users allowed superuser privilege
daemon	1	processes that need less than wheel privilege
kmem	2	read access to kernel memory
sys	3	access to kernel sources
tty	4	access to terminals
operator	5	read access to raw disks
bin	7	group for system binaries
news	8	group for news
wsrc	9	write access to sources
games	13	access to games
staff	20	system staff
guest	31	system guests
nobody	39	the least privileged group
utmp	45	access to utmp files
dialer	117	access to remote ports and dialers
.TE
.DE
Only users in the ``wheel'' group are permitted to \fIsu\fP to ``root''.
Most programs that manage directories in
.Pn /var/spool
now run set-group-id to ``daemon'' so that users cannot
directly access the files in the spool directories.
The special files that access kernel memory,
.Pn /dev/kmem
and
.Pn /dev/mem ,
are made readable only by group ``kmem''.
Standard system programs that require this access are
made set-group-id to that group.
The group ``sys'' is intended to control access to kernel sources,
and other sources belong to group ``wsrc.''
Rather than make user's terminals writable by all users,
they are now placed in group ``tty'' and made only group writable.
Programs that should legitimately have access to write on user's terminals
such as \fItalkd\fP and \fIwrite\fP now run set-group-id to ``tty''.
The ``operator'' group controls access to disks.
By default, disks are readable by group ``operator'',
so that programs such as \fIdump\fP can access the filesystem
information without being set-user-id to ``root''.
The
.IR shutdown (8)
program is executable only by group operator
and is setuid to root so that members of group operator may shut down
the system without root access.
.PP
The ownership and modes of some directories have changed.
The \fIat\fP programs now run set-user-id ``root'' instead of ``daemon.''
Also, the uucp directory no longer needs to be publicly writable,
as \fItip\fP reverts to privileged status to remove its lock files.
After copying your version of
.Pn /var/spool ,
you should do:
.DS
\fB#\fP \fIchown \-R root /var/spool/at\fP
\fB#\fP \fIchown \-R uucp.daemon /var/spool/uucp\fP
\fB#\fP \fIchmod \-R o\-w /var/spool/uucp\fP
.DE
.PP
The format of the cron table,
.Pn /etc/crontab ,
has been changed to specify the user-id that should be used to run a process.
The userid ``nobody'' is frequently useful for non-privileged programs.
Local modification are now put in a separate file,
.Pn /etc/crontab.local .
.PP
Some of the commands previously in
.Pn /etc/rc.local
have been moved to
.Pn /etc/rc ;
several new functions are now handled by
.Pn /etc/rc ,
.Pn /etc/netstart
and
.Pn /etc/rc.local .
You should look closely at the prototype version of these files
and read the manual pages for the commands contained in it
before trying to merge your local copy.
Note in particular that \fIifconfig\fP has had many changes,
and that host names are now fully specified as domain-style names
(e.g., vangogh.CS.Berkeley.EDU) for the benefit of the name server.
.PP
The C-library and system binaries on the distribution tape
are compiled with new versions of
\fIgethostbyname\fP and \fIgethostbyaddr\fP which use
the name server,
.IR named (8).
If you have only a small network and are not connected
to a large network, you can use the distributed library routines without
any problems; they use a linear scan of the host table
.Pn /etc/hosts
if the name server is not running.
If you are on the Internet or have a large local network,
it is recommend that you set up
and use the name server.
For instructions on how to set up the necessary configuration files,
refer to ``Name Server Operations Guide for BIND'' (SMM:10).
Several programs rely on the host name returned by \fIgethostname\fP
to determine the local domain name.
.PP
If you are using the name server, your \fIsendmail\fP configuration
file will need some updates to accommodate it.
See the ``Sendmail Installation and Operation Guide'' (SMM:8) and
the sample \fIsendmail\fP configuration files in
.Pn /usr/src/usr.sbin/sendmail/cf .
The aliases file,
.Pn /etc/aliases
has also been changed to add certain well-known addresses.
.NH 3
Shadow password files
.PP
The password file format adds change and expiration fields
and its location has changed to protect
the encrypted passwords stored there.
The actual password file is now stored in
.Pn /etc/master.passwd .
The hashed dbm password files do not contain encrypted passwords,
but contain the file offset to the entry with the password in
.Pn /etc/master.passwd
(which is readable only by root).
Thus, the
.Fn getpwnam
and
.Fn getpwuid
functions will no longer return an encrypted password string to non-root
callers.
An old-style passwd file is created in
.Pn /etc/passwd
by the
.Xr vipw (8)
and
.Xr pwd_mkdb (8)
programs.
See also
.Xr passwd (5).
.PP
Several new users have also been added to the group of ``well-known'' users 
in /etc/passwd.
The current list is:
.DS
.TS
l c.
name	number
_
root	0
daemon	1
operator	2
bin	3
games	7
uucp	66
nobody	32767
.TE
.DE
The ``daemon'' user is used for daemon processes that
do not need root privileges.
The ``operator'' user-id is used as an account for dumpers
so that they can log in without having the root password.
By placing them in the ``operator'' group, 
they can get read access to the disks.
The ``uucp'' login has existed long before \*(4B,
and is noted here just to provide a common user-id.
The password entry ``nobody'' has been added to specify
the user with least privilege.  The ``games'' user is a pseudo-user
that controls access to game programs.
.PP
After installing your updated password file, you must run
.Xr pwd_mkdb (8)
to create the password database.
Note that
.Xr pwd_mkdb (8)
is run whenever
.Xr vipw (8)
is run.
.NH 3
The
.Pn /var
filesystem
.PP
The spooling directories saved on tape may be restored in their
eventual resting places without too much concern.  Be sure to
use the `\-p' option to
.Xr tar (1)
so that files are recreated with the same file modes.
The following commands provide a guide for copying spool and log files from
an existing system into a new
.Pn /var
filesystem.
At least the following directories should already exist on
.Pn /var :
.Pn output ,
.Pn log ,
.Pn backups
and
.Pn db .
.LP
.DS
.ft CW
SRC=/oldroot/usr

cd $SRC; tar cf - msgs preserve | (cd /var && tar xpf -)
.DE
.DS
# copy $SRC/spool to /var
cd $SRC/spool
tar cf - at mail rwho | (cd /var && tar xpf -)
tar cf - ftp mqueue news secretmail uucp uucppublic | \e
	(cd /var/spool && tar xpf -)
.DE
.DS
# everything else in spool is probably a printer area
mkdir .save
mv at ftp mail mqueue rwho secretmail uucp uucppublic .save
tar cf - * | (cd /var/spool/output && tar xpf -)
mv .save/* .
rmdir .save
.DE
.DS
cd /var/spool/mqueue
mv syslog.7 /var/log/maillog.7
mv syslog.6 /var/log/maillog.6
mv syslog.5 /var/log/maillog.5
mv syslog.4 /var/log/maillog.4
mv syslog.3 /var/log/maillog.3
mv syslog.2 /var/log/maillog.2
mv syslog.1 /var/log/maillog.1
mv syslog.0 /var/log/maillog.0
mv syslog /var/log/maillog
.DE
.DS
# move $SRC/adm to /var
cd $SRC/adm
tar cf - . | (cd /var/account && tar  xpf -)
cd /var/account
rm -f msgbuf
mv messages messages.[0-9] ../log
mv wtmp wtmp.[0-9] ../log
mv lastlog ../log
.DE
.NH 3
Block devices and the root filesystem
.PP
The buffer cache in the kernel is now organized as a file block cache
rather than a device block cache.
As a consequence, cached blocks from a file
and from the corresponding block device would no longer be kept consistent.
The block device thus has little remaining value.
Three changes have been made for these reasons:
(1) block devices may not be opened while they are mounted,
and may not be mounted while open, so that the two versions of cached
file blocks cannot be created,
(2) filesystem checks of the root now use the raw device
to access the root filesystem, and
(3) the root filesystem is initially mounted read-only
so that nothing can be written back to disk during or after modification
of the raw filesystem by
.I fsck .
The root filesystem may be made writable while in single-user mode
with the command
.Li "mount -u /" .
The mount command has an option to update the flags on a mounted filesystem,
including the ability to upgrade a filesystem from read-only to read-write
or downgrade it from read-write to read-only.
.NH 3
Kerberos
.PP
The Kerberos authentication server from MIT (version 4)
is included in this release.
See \fIkerberos\fP\|(1) for a general, if MIT-specific,
introduction.
If it is configured,
.Xr login (1),
.Xr passwd (1),
.Xr rlogin (1)
and
.Xr rsh (1)
will all begin to use it automatically.
The file
.Pn /etc/kerberosIV/README
describes the configuration.
Each system needs the file
.Pn /etc/kerberosIV/krb.conf
to set its realm and local servers,
and a private key stored in
.Pn /etc/kerberosIV/srvtab
(see
.Xr ext_srvtab (8)).
The Kerberos server should be set up on a single, physically secure,
server machine.
Users and hosts may be added to the server database manually with
.Xr kdb_edit (8),
or users on authorized hosts can add themselves and a Kerberos
password upon verification of their ``local'' (passwd-file) password
using the
.Xr register (1)
program.
.PP
Note that by default the password-changing program
.Xr passwd (1)
changes the Kerberos password, which must exist.
The
.Li \-l
option to
.Xr passwd (1)
changes the ``local'' password if one exists.
.PP
Note that Version 5 of Kerberos will be released soon,
and that Version 4 should probably be replaced at that time.
.NH 3
Make and Makefiles
.PP
This release uses a completely new version of the
.I make
program derived from the
.I pmake
program developed by the Sprite project at Berkeley.
It supports existing makefiles, although certain incorrect makefiles
may fail.
The makefiles for the \*(4B sources make extensive use of the new
facilities, especially conditionals and file inclusion, and are thus
completely incompatible with older versions of
.I make
(but nearly all of the makefiles are now trivial!).
The standard include files for
.I make
are in
.Pn /usr/share/mk .
There is a bsd.README file in
.Pn /usr/src/share/mk .
.PP
Another global change supported by the new
.I make
is designed to allow multiple architectures to share a copy of the sources.
If a subdirectory named
.Pn obj
is present in the current directory,
.I make
descends into that directory and creates all object and other files there.
We use this by building a directory hierarchy in
.Pn /var/obj
that parallels
.Pn /usr/src .
We then create the
.Pn obj
subdirectories in
.Pn /usr/src
as symbolic links to the corresponding directories in
.Pn /var/obj .
(Both of these steps are automated; the makefile in the
.Pn /usr/src
directory has an example of a target (``shadow'') which builds
the object filesystem, and ``make obj'' in a directory
including the standard \*(Bs rules in its Makefile makes the
.Pn obj
links in the current directory and recursively in the normal subdirectories.)
We have one
.Pn /var/obj
hierarchy on the local system, and another on each
system that shares the source filesystem.
.NH 3
Additions and changes to filesystems
.PP
Network filesystem access is available in \*(4B.
An implementation of the Network File System (NFS) was contributed
by Rick Macklem of the University of Guelph.
Its use will be fairly familiar to users of other implementations of NFS.
See the manual pages
.Xr mount (8),
.Xr mountd (8),
.Xr fstab (5),
.Xr exports (5),
.Xr netgroup (5),
.Xr nfsd (8),
.Xr nfsiod (8),
and
.Xr nfssvc (8).
The format of
.Pn /etc/fstab
has changed from previous \*(Bs releases
to a blank-separated format to allow colons in pathnames.
.PP
An implementation of an auto-mounter daemon,
.I amd,
was contributed by Jan-Simon Pendry of the
Imperial College of Science, Technology & Medicine.
See the document ``AMD \- The 4.4BSD Automounter'' (SMM:13)
for further information.
.PP
The directory
.Pn /dev/fd
contains special files
.Pn 0
through
.Pn 63
which, when opened, duplicate the corresponding file descriptor.
The names
.Pn /dev/stdin
.Pn /dev/stdout
.Pn /dev/stderr
refer to file descriptors 0, 1 and 2.
See
.Xr fd (4)
and
.Xr mount_fdesc (8)
for more information.
.PP
The system now supports stackable filesystems;
this feature allows various filesystems to be stacked
on top of each other to provide additive sets of semantics.
For example,
the umap filesystem (see
.Xr mount_umap (8))
is used to mount a sub-tree of an existing filesystem
that uses a different set of uids and gids than the local system.
Such a filesystem could be mounted from a remote site via NFS or it
could be a filesystem on removable media brought from some foreign
location that uses a different password file.
.PP
Other new filesystems include the loopback filesystem
.Xr mount_lofs (8),
the kernel filesystem
.Xr mount_kernfs (8),
and the portal filesystem
.Xr mount_portal (8).
.NH 3
POSIX changes
.PP
The \*(4B system uses the IEEE P1003.1 (POSIX.1) terminal interface
rather than the previous \*(Bs terminal interface.
The new interface has nearly all of the functionality of the old interface,
extending the POSIX interface as necessary.
Both the old
.I ioctl
calls and old options to
.Xr stty (1)
are emulated.
This emulation is expected to be unavailable in many vendors releases,
so conversion to the new interface is encouraged.
.PP
The POSIX.1 job control interface is implemented in \*(4B.
A new system call,
.Fn setsid ,
is used to create a job-control session consisting of a single process
group with one member, the caller, which becomes a session leader.
Only a session leader may acquire a controlling terminal.
This is done explicitly via a
.Sm TIOCSCTTY
.Fn ioctl
call, not implicitly by an
.Fn open
call.
The call fails if the terminal is in use.
Programs that allocate controlling terminals (or pseudo-terminals)
require modification to work in this environment.
The versions of
.I xterm
provided in the X11R5 release includes the necessary changes.
New library routines are available for allocating and initializing
pseudo-terminals and other terminals as controlling terminal; see
.Pn /usr/src/lib/libutil/pty.c
and
.Pn /usr/src/lib/libutil/login_tty.c .
.PP
The POSIX job control model formalizes the previous conventions
used in setting up a process group.
Unfortunately, this requires that changes be made in a defined order
and with some synchronization that were not necessary in the past.
Older job control shells (csh, ksh) will generally not operate correctly
with the new system.
.PP
Most of the other kernel interfaces have been changed to correspond
with the POSIX.1 interface, although that work is not quite complete.
See the relevant manual pages, perhaps in conjunction with the IEEE POSIX
standard.
.PP
Many of the utilities have been changed to work as described in draft 14
of the POSIX.2 Shell and Utilities document.
Additional changes are likely in this area.
.NH 3
Networking additions and changes
.PP
\*(4B provides some support for the ISO OSI protocols CLNP,
TP4, and ES-IS.
User level libraries and processes
implement the application protocols such as FTAM and X.500;
these are available in ISODE,
the ISO Development Environment by Marshall Rose,
which is available via anonymous FTP
(but is not included on the distribution tape).
.PP
Kernel support for the ISO OSI protocols is enabled with the ISO option
in the kernel configuration file.
The
.Xr iso (4)
manual page describes the protocols and addressing;
see also
.Xr clnp (4),
.Xr tp (4)
and
.Xr cltp (4).
The OSI equivalent to ARP is ESIS (End System to Intermediate System Routing
Protocol); running this protocol is mandatory, however one can manually add
translations for machines that do not participate by use of the
.Xr route (8)
command.
Additional information is provided in the manual page describing
.Xr esis (4).
.PP
The command
.Xr route (8)
has a new syntax and a number of new capabilities:
it can install routes with a specified destination and mask,
and can change route characteristics such as hop count, packet size
and window size.
.PP
The format of the
.I sockaddr
structure (the structure used to describe a generic network address with an
address family and family-specific data)
has changed from previous releases,
as have the address family-specific versions of this structure.
The
.I sa_family
family field has been split into a length,
.IR sa_len ,
and a family,
.IR sa_family .
System calls that pass a
.I sockaddr
structure into the kernel (e.g.
.Fn sendto
and
.Fn connect )
have a separate parameter that specifies the 
.I sockaddr
length, and thus it is not necessary to fill in the
.I sa_len
field for those system calls.
System calls that pass a
.I sockaddr
structure back from the kernel (e.g. 
.Fn recvfrom
and
.Fn accept )
receive a completely filled-in
.I sockaddr
structure, thus the length field is valid.
Because this would not work for old binaries,
the new library uses a different system call number.
Thus, most networking programs compiled under \*(4B are incompatible
with older systems.
.PP
Although this change is mostly source and binary compatible
with old programs, there are three exceptions.
Programs with statically initialized
.I sockaddr
structures
(usually the Internet form, a
.I sockaddr_in )
are not compatible.
Generally, such programs should be changed to fill in the structure
at run time, as C allows no way to initialize a structure without
assuming the order and number of fields.
Also, programs with use structures to describe a network packet format
that contain embedded
.I sockaddr
structures also require modification; a definition of an
.I osockaddr
structure is provided for this purpose.
Finally, programs that use the
.Sm SIOCGIFCONF
ioctl to get a complete list of interface addresses
need to check the
.I sa_len
field when iterating through the array of addresses returned,
as not all of the structures returned have the same length
(in fact, this is nearly guaranteed by the presence of link-layer
address structures).
.NH 3
Additions and changes to utilities
.PP
New versions of
.Xr lex (1)
(``flex'') and 
.Xr yacc (1)
(``zoo'')
have replaced their AT&T-derived predecessors.
These should be installed early on if attempting to cross-compile \*(4B
on another system.
Note that the new
.Xr lex
program is not completely backward compatible with historic versions of
.Xr lex ,
although it is believed that all documented features are supported.
.PP
A system-call tracing facility is provided in \*(4B
that records all of the system calls made by a process or group of processes
and their outcomes.
See
.Xr ktrace (1)
and
.Xr kdump (1).
.PP
A new utility,
.Xr sysctl (8),
retrieves kernel state and allows processes with appropriate
privilege to set kernel state.
The state to be retrieved or
set is described using a ``Management Information Base'' (``MIB'') style
name, described as a dotted set of components.
.PP
The kernel runs with four different levels of security.
Any superuser process can raise the security level, but only 
.Fn init
can lower it.
Security levels are defined as follows:
.IP \-1
Permanently insecure mode \- always run system in level 0 mode.
.IP " 0"
Insecure mode \- immutable and append-only flags may be turned off.
All devices may be read or written subject to their permissions.
.IP " 1"
Secure mode \- immutable and append-only flags may not be cleared;
disks for mounted filesystems,
.Pn /dev/mem ,
and
.Pn /dev/kmem
are read-only.
.IP " 2"
Highly secure mode \- same as secure mode, plus disks are always
read-only whether mounted or not.
This level precludes tampering with filesystems by unmounting them,
but also inhibits running
.Xr newfs (8)
while the system is multi-user.
.PP
Normally, the system runs in level 0 mode while single user
and in level 1 mode while multiuser.
If the level 2 mode is desired while running multiuser,
it can be set in the startup script
.Pn /etc/rc
using
.Xr sysctl (1).
If it is desired to run the system in level 0 mode while multiuser,
the administrator must build a kernel with the variable
.Li securelevel
in the kernel source file
.Pn /sys/kern/kern_sysctl.c
initialized to \-1.
.NH 2
Hints on converting from \*(Ps to \*(4B
.PP
This section summarizes the most significant changes between
\*(Ps and \*(4B particularly those that are likely to 
cause difficulty in doing the conversion.
It does not include changes in the network;
see section 5 for information on setting up the network.
.PP
The stat st_size field is now 64-bits instead of 32.
Doing something like:
.DS
foo(st.st_size);
.DE
and then (improperly) defining foo with an "int" or "long" param:
.DS
foo(size)
	int size;
{
	...
}
.DE
will fail miserably (well, it might work on a little endian machine).
This problem showed up in emacs as well as several other programs.
A related problem is improperly casting (or failing to cast)
the second argument to lseek (or [f]truncate) ala:
.DS
lseek(fd, (long)off, 0);
.DE
or
.DS
lseek(fd, 0, 0);
.DE
The best solution is to include
.Pn <sys/types.h>
which has prototypes that catch these types of errors.
.PP
Determining the namelen param for a connect call on a unix domain socket
should use the SUN_LEN macro.
One old way that was used:
.DS
addrlen = strlen(unaddr.sun_path) + sizeof(unaddr.sun_family);
.DE
no longer works as there is an additional field "sun_len".
.PP
The timezone conversion code uses data files installed in
.Pn /usr/share/zoneinfo
to convert from "GMT" to various timezones.  The data file for the default
timezone for the system should be copied to
.Pn /etc/localtime .
Other timezones can be selected by setting the TZ environment variable.
.PP
The data files initially installed in
.Pn /usr/share/zoneinfo
include corrections for leap seconds since the beginning of 1970.
Thus, they assume that the
kernel will increment the time at a constant rate during a leap second;
that is, time just keeps on ticking.  The conversion routines will then
name a leap second 23:59:60.  For purists, this effectively means that
the kernel maintains TAI (International Atomic Time) rather than UTC
(Coordinated Universal Time, aka GMT).
.PP
For systems that run current NTP (Network Time Protocol) implementations
or that wish to conform to the letter of the POSIX.1 law, it is possible
to rebuild the timezone data files so that leap seconds are not counted.
(NTP causes the time to jump over a leap second, and POSIX effectively
requires the clock to be reset by hand when a leap second occurs.
In this mode, the kernel effectively runs UTC rather than TAI.)
.PP
The data files without leap second information
are constructed from the source directory,
.Pn /usr/src/share/zoneinfo .
Change the variable REDO in Makefile
from "right" to "posix", and then do
.DS
make obj	(if necessary)
make
make install
.DE
.PP
You will then need to copy the correct default zone file to
.Pn /etc/localtime ,
as the old one would still have used leap seconds, and because the Makefile
installs a default
.Pn /etc/localtime
each time ``make install'' is done.
.PP
It is possible to install both sets of timezone data files.  This results
in subdirectories
.Pn /usr/share/zoneinfo/right
and
.Pn /usr/share/zoneinfo/posix .
Each contain a complete set of zone files.
See
.Pn /usr/src/share/zoneinfo/Makefile
for details.
.PP
The kernel's limit on the number of open files has been
increased from 20 to 64.
It is now possible to change this limit almost arbitrarily.
The standard I/O library
autoconfigures to the kernel limit.
Note that file (``_iob'') entries may be allocated
by \fImalloc\fP from \fIfopen\fP;
this allocation has been known to cause problems with programs
that use their own memory allocators.
This does not occur until after 20 files have been opened
by the standard I/O library.
.PP
\fISelect\fP can be used with more than 32 descriptors
by using arrays of \fBint\fPs for the bit fields rather than single \fBint\fPs.
Programs that used \fIgetdtablesize\fP as their first argument to \fIselect\fP
will no longer work correctly.
Usually the program can be modified to correctly specify the number
of bits in an \fBint\fP.
Alternatively the program can be modified to use an array of \fBint\fPs.
There are a set of macros available in
.Pn <sys/types.h>
to simplify this.
See
.IR select (2).
.PP
Old core files will not be intelligible by the current debuggers
because of numerous changes to the user structure
and because the kernel stack has been enlarged.
The \fIa.out\fP header that was in the user structure is no longer present.
Locally-written debuggers that try to check the magic number
will need modification.
.PP
The system now has a database of file names,
constructed once a week from \fIcron\fP.
To find a file by name only,
the command \fIlocate name\fP will look in the database for
files that match the name.
This command is much faster than the full filesystem traversal
done by find:
.DS
find / \-name name \-print
.DE
.PP
Files may not be deleted from directories having the ``sticky'' (ISVTX) bit
set in their modes
except by the owner of the file or of the directory, or by the superuser.
This is primarily to protect users' files in publicly-writable directories
such as
.Pn /tmp
and
.Pn /var/tmp .
All publicly-writable directories should have their ``sticky'' bits set
with ``chmod +t.''
.PP
The following two sections contain additional notes concerning
changes in \*(4B that affect the installation of local files;
be sure to read them as well.
