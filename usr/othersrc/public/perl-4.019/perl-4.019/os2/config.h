/* config.h
 * This file was hand tailored for compiling under MS-DOS and MSC 5.1.
 * Diomidis Spinellis, March 1990.
 *
 * Then it got mangled again for compiling under OS/2 and MSC 6.0.
 * Raymond Chen, June 1990.
 */
#define OS2		/**/

/* OS/2 supports some additional things MS-DOS doesn't.
 */
#ifdef OS2
#define PIPE
#define GETPPID
#define HAS_GETPRIORITY
#define HAS_SETPRIORITY
#define KILL
#endif /* OS2 */

/* SUFFIX:
 *      This symbol, if defined, indicates that the function add_suffix has
 *      been supplied in a system-dependent .c file.  This function is
 *      recommended for operating systems whose filenaming conventions
 *      do not permit arbitrary strings as filenames.
 */
#define SUFFIX	/**/

/* EUNICE:
 *	This symbol, if defined, indicates that the program is being compiled
 *	under the EUNICE package under VMS.  The program will need to handle
 *	things like files that don't go away the first time you unlink them,
 *	due to version numbering.  It will also need to compensate for lack
 *	of a respectable link() command.
 */
/* VMS:
 *	This symbol, if defined, indicates that the program is running under
 *	VMS.  It is currently only set in conjunction with the EUNICE symbol.
 */
/*#undef	EUNICE		/**/
/*#undef	VMS		/**/

/* BIN:
 *	This symbol holds the name of the directory in which the user wants
 *	to put publicly executable images for the package in question.  It
 *	is most often a local directory such as /usr/local/bin.
 */
#define BIN "/usr/local/bin"             /**/

/* BYTEORDER:
 *	This symbol contains an encoding of the order of bytes in a long.
 *	Usual values (in octal) are 01234, 04321, 02143, 03412...
 */
/* CHECK */
#define BYTEORDER 0x1234		/**/

/* CPPSTDIN:
 *	This symbol contains the first part of the string which will invoke
 *	the C preprocessor on the standard input and produce to standard
 *	output.	 Typical value of "cc -{" or "/lib/cpp".
 */
/* CPPMINUS:
 *	This symbol contains the second part of the string which will invoke
 *	the C preprocessor on the standard input and produce to standard
 *	output.  This symbol will have the value "-" if CPPSTDIN needs a minus
 *	to specify standard input, otherwise the value is "".
 */
/* TODO */
#define CPPSTDIN "cc -{"
#define CPPMINUS ""

/* HAS_BCMP:
 *	This symbol, if defined, indicates that the bcmp routine is available
 *	to compare blocks of memory.  If undefined, use memcmp.  If that's
 *	not available, roll your own.
 */
/*#define	HAS_BCMP		/**/

/* HAS_BCOPY:
 *	This symbol, if defined, indicates that the bcopy routine is available
 *	to copy blocks of memory.  Otherwise you should probably use memcpy().
 */
/*#define	HAS_BCOPY		/**/

/* CHARSPRINTF:
 *	This symbol is defined if this system declares "char *sprintf()" in
 *	stdio.h.  The trend seems to be to declare it as "int sprintf()".  It
 *	is up to the package author to declare sprintf correctly based on the
 *	symbol.
 */
/*#define	CHARSPRINTF 	/**/

/* HAS_CRYPT:
 *	This symbol, if defined, indicates that the crypt routine is available
 *	to encrypt passwords and the like.
 */
/* TODO */
/*#define	HAS_CRYPT		/**/

/* DOSUID:
 *	This symbol, if defined, indicates that the C program should
 *	check the script that it is executing for setuid/setgid bits, and
 *	attempt to emulate setuid/setgid on systems that have disabled
 *	setuid #! scripts because the kernel can't do it securely.
 *	It is up to the package designer to make sure that this emulation
 *	is done securely.  Among other things, it should do an fstat on
 *	the script it just opened to make sure it really is a setuid/setgid
 *	script, it should make sure the arguments passed correspond exactly
 *	to the argument on the #! line, and it should not trust any
 *	subprocesses to which it must pass the filename rather than the
 *	file descriptor of the script to be executed.
 */
/*#define DOSUID		/**/

/* HAS_DUP2:
 *	This symbol, if defined, indicates that the dup2 routine is available
 *	to dup file descriptors.  Otherwise you should use dup().
 */
#define	HAS_DUP2		/**/

/* HAS_FCHMOD:
 *	This symbol, if defined, indicates that the fchmod routine is available
 *	to change mode of opened files.  If unavailable, use chmod().
 */
/*#define	HAS_FCHMOD		/**/

/* HAS_FCHOWN:
 *	This symbol, if defined, indicates that the fchown routine is available
 *	to change ownership of opened files.  If unavailable, use chown().
 */
/*#define	HAS_FCHOWN		/**/

/* I_FCNTL:
 *	This symbol, if defined, indicates to the C program that it should
 *	include fcntl.h.
 */
/*#define	I_FCNTL		/**/

/* HAS_FLOCK:
 *	This symbol, if defined, indicates that the flock() routine is
 *	available to do file locking.
 */
/*#define	HAS_FLOCK		/**/

/* HAS_GETGROUPS:
 *	This symbol, if defined, indicates that the getgroups() routine is
 *	available to get the list of process groups.  If unavailable, multiple
 *	groups are probably not supported.
 */
/*#define	HAS_GETGROUPS		/**/

/* HAS_GETHOSTENT:
 *	This symbol, if defined, indicates that the gethostent() routine is
 *	available to lookup host names in some data base or other.
 */
/*#define	HAS_GETHOSTENT		/**/

/* HAS_GETPGRP:
 *	This symbol, if defined, indicates that the getpgrp() routine is
 *	available to get the current process group.
 */
/*#define	HAS_GETPGRP		/**/

/* HAS_GETPRIORITY:
 *	This symbol, if defined, indicates that the getpriority() routine is
 *	available to get a process's priority.
 */
/*#define	HAS_GETPRIORITY		/**/

/* HAS_HTONS:
 *	This symbol, if defined, indicates that the htons routine (and friends)
 *	are available to do network order byte swapping.
 */
/* HAS_HTONL:
 *	This symbol, if defined, indicates that the htonl routine (and friends)
 *	are available to do network order byte swapping.
 */
/* HAS_NTOHS:
 *	This symbol, if defined, indicates that the ntohs routine (and friends)
 *	are available to do network order byte swapping.
 */
/* HAS_NTOHL:
 *	This symbol, if defined, indicates that the ntohl routine (and friends)
 *	are available to do network order byte swapping.
 */
/*#define	HAS_HTONS		/**/
/*#define	HAS_HTONL		/**/
/*#define	HAS_NTOHS		/**/
/*#define	HAS_NTOHL		/**/

/* index:
 *	This preprocessor symbol is defined, along with rindex, if the system
 *	uses the strchr and strrchr routines instead.
 */
/* rindex:
 *	This preprocessor symbol is defined, along with index, if the system
 *	uses the strchr and strrchr routines instead.
 */
#define	index strchr	/* cultural */
#define	rindex strrchr	/*  differences? */

/* I_SYSIOCTL:
 *	This symbol, if defined, indicates that sys/ioctl.h exists and should
 *	be included.
 */
/*#define	I_SYSIOCTL		/**/

/* HAS_KILLPG:
 *	This symbol, if defined, indicates that the killpg routine is available
 *	to kill process groups.  If unavailable, you probably should use kill
 *	with a negative process number.
 */
/*#define	HAS_KILLPG		/**/

/* HAS_MEMCMP:
 *	This symbol, if defined, indicates that the memcmp routine is available
 *	to compare blocks of memory.  If undefined, roll your own.
 */
#define	HAS_MEMCMP		/**/

/* HAS_MEMCPY:
 *	This symbol, if defined, indicates that the memcpy routine is available
 *	to copy blocks of memory.  Otherwise you should probably use bcopy().
 *	If neither is defined, roll your own.
 */
#define	HAS_MEMCPY		/**/

/* HAS_MKDIR:
 *	This symbol, if defined, indicates that the mkdir routine is available
 *	to create directories.  Otherwise you should fork off a new process to
 *	exec /bin/mkdir.
 */
#define	HAS_MKDIR		/**/

/* HAS_NDBM:
 *	This symbol, if defined, indicates that ndbm.h exists and should
 *	be included.
 */
#define	HAS_NDBM		/**/

/* HAS_ODBM:
 *	This symbol, if defined, indicates that dbm.h exists and should
 *	be included.
 */
/*#define	HAS_ODBM		/**/

/* HAS_READDIR:
 *	This symbol, if defined, indicates that the readdir routine is available
 *	from the C library to create directories.
 */
#define	HAS_READDIR		/**/

/* HAS_RENAME:
 *	This symbol, if defined, indicates that the rename routine is available
 *	to rename files.  Otherwise you should do the unlink(), link(), unlink()
 *	trick.
 */
#define	HAS_RENAME		/**/

/* HAS_RMDIR:
 *	This symbol, if defined, indicates that the rmdir routine is available
 *	to remove directories.  Otherwise you should fork off a new process to
 *	exec /bin/rmdir.
 */
#define	HAS_RMDIR		/**/

/* HAS_SETEGID:
 *	This symbol, if defined, indicates that the setegid routine is available
 *	to change the effective gid of the current program.
 */
/*#define	HAS_SETEGID		/**/

/* HAS_SETEUID:
 *	This symbol, if defined, indicates that the seteuid routine is available
 *	to change the effective uid of the current program.
 */
/*#define	HAS_SETEUID		/**/

/* HAS_SETPGRP:
 *	This symbol, if defined, indicates that the setpgrp() routine is
 *	available to set the current process group.
 */
/*#define	HAS_SETPGRP		/**/

/* HAS_SETPRIORITY:
 *	This symbol, if defined, indicates that the setpriority() routine is
 *	available to set a process's priority.
 */
/*#define	HAS_SETPRIORITY		/**/

/* HAS_SETREGID:
 *	This symbol, if defined, indicates that the setregid routine is available
 *	to change the real and effective gid of the current program.
 */
/*#define	HAS_SETREGID		/**/

/* HAS_SETREUID:
 *	This symbol, if defined, indicates that the setreuid routine is available
 *	to change the real and effective uid of the current program.
 */
/*#define	HAS_SETREUID		/**/

/* HAS_SETRGID:
 *	This symbol, if defined, indicates that the setrgid routine is available
 *	to change the real gid of the current program.
 */
/*#define	HAS_SETRGID		/**/

/* HAS_SETRUID:
 *	This symbol, if defined, indicates that the setruid routine is available
 *	to change the real uid of the current program.
 */
/*#define	HAS_SETRUID		/**/

/* HAS_SOCKET:
 *      This symbol, if defined, indicates that the BSD socket interface is
 *      supported.
 */
/* HAS_SOCKETPAIR:
 *      This symbol, if defined, indicates that the BSD socketpair call is
 *      supported.
 */
/* OLDSOCKET:
 *      This symbol, if defined, indicates that the 4.1c BSD socket interface
 *      is supported instead of the 4.2/4.3 BSD socket interface.
 */
/*#undef HAS_SOCKET          /**/

/*#undef HAS_SOCKETPAIR      /**/

/*#undef        OLDSOCKET       /**/

/* STATBLOCKS:
 *	This symbol is defined if this system has a stat structure declaring
 *	st_blksize and st_blocks.
 */
/*#define	STATBLOCKS 	/**/

/* STDSTDIO:
 *	This symbol is defined if this system has a FILE structure declaring
 *	_ptr and _cnt in stdio.h.
 */
#define	STDSTDIO 	/**/

/* STRUCTCOPY:
 *	This symbol, if defined, indicates that this C compiler knows how
 *	to copy structures.  If undefined, you'll need to use a block copy
 *	routine of some sort instead.
 */
#define	STRUCTCOPY	/**/

/* HAS_SYMLINK:
 *	This symbol, if defined, indicates that the symlink routine is available
 *	to create symbolic links.
 */
/*#define	HAS_SYMLINK		/**/

/* HAS_SYSCALL:
 *	This symbol, if defined, indicates that the syscall routine is available
 *	to call arbitrary system calls.  If undefined, that's tough.
 */
/*#define	HAS_SYSCALL		/**/

/* s_tm:
 *	This symbol is defined if this system declares "struct tm" in
 *	in <sys/time.h> rather than <time.h>.  We can't just say
 *	-I/usr/include/sys because some systems have both time files, and
 *	the -I trick gets the wrong one.
 */
/* I_SYS_TIME:
 *	This symbol is defined if this system has the file <sys/time.h>.
 */
/*
 * I_TIME:
 *	This symbol is defined if time this  system has the file <time.h>.
 */
/*#undef	s_tm 	/**/
/*#define	I_SYS_TIME 	/**/
#define I_TIME

/* VARARGS:
 *	This symbol, if defined, indicates to the C program that it should
 *	include varargs.h.
 */
#define	VARARGS		/**/

/* vfork:
 *	This symbol, if defined, remaps the vfork routine to fork if the
 *	vfork() routine isn't supported here.
 */
/*#undef	vfork fork	/**/

/* VOIDSIG:
 *	This symbol is defined if this system declares "void (*signal())()" in
 *	signal.h.  The old way was to declare it as "int (*signal())()".  It
 *	is up to the package author to declare things correctly based on the
 *	symbol.
 */
#define	VOIDSIG 	/**/

/* HAS_VPRINTF:
 *	This symbol, if defined, indicates that the vprintf routine is available
 *	to printf with a pointer to an argument list.  If unavailable, you
 *	may need to write your own, probably in terms of _doprnt().
 */
/* CHARVSPRINTF:
 *	This symbol is defined if this system has vsprintf() returning type
 *	(char*).  The trend seems to be to declare it as "int vsprintf()".  It
 *	is up to the package author to declare vsprintf correctly based on the
 *	symbol.
 */
#define	HAS_VPRINTF		/**/
/*#undef	CHARVSPRINTF 	/**/

/* GIDTYPE:
 *	This symbol has a value like gid_t, int, ushort, or whatever type is
 *	used to declare group ids in the kernel.
 */
/* TODO */
#define GIDTYPE int		/**/

/* I_DIRENT:
 *	This symbol, if defined, indicates to the C program that it should
 *	include dirent.h.
 */
/* DIRNAMLEN:
 *	This symbol, if defined, indicates to the C program that the length
 *	of directory entry names is provided by a d_namlen field.  Otherwise
 *	you need to do strlen() on the d_name field.
 */
/*#undef	I_DIRENT		/**/
#define	DIRNAMLEN		/**/

/* I_FCNTL:
 *	This symbol, if defined, indicates to the C program that it should
 *	include fcntl.h.
 */
/*#define	I_FCNTL		/**/

/* I_GRP:
 *	This symbol, if defined, indicates to the C program that it should
 *	include grp.h.
 */
/*#define	I_GRP		/**/

/* I_PWD:
 *	This symbol, if defined, indicates to the C program that it should
 *	include pwd.h.
 */
/* PWQUOTA:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_quota.
 */
/* PWAGE:
 *	This symbol, if defined, indicates to the C program that struct passwd
 *	contains pw_age.
 */
/*#define	I_PWD		/**/
/*#define	PWQUOTA		/**/
/*#undef	PWAGE		/**/

/* I_SYS_DIR:
 *	This symbol, if defined, indicates to the C program that it should
 *	include sys/dir.h.
 */
#define	I_SYS_DIR		/**/

/* I_SYSIOCTL:
 *	This symbol, if defined, indicates that sys/ioctl.h exists and should
 *	be included.
 */
/*#define	I_SYSIOCTL		/**/

/* I_VARARGS:
 *	This symbol, if defined, indicates to the C program that it should
 *	include varargs.h.
 */
#define	I_VARARGS		/**/

/* INTSIZE:
 *	This symbol contains the size of an int, so that the C preprocessor
 *	can make decisions based on it.
 */
#define INTSIZE 2		/**/

/* RANDBITS:
 *	This symbol contains the number of bits of random number the rand()
 *	function produces.  Usual values are 15, 16, and 31.
 */
#define RANDBITS 31		/**/

/* SIG_NAME:
 *	This symbol contains an list of signal names in order.
 */
#ifdef OS2
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","IOT","EMT","FPE",\
	  /*      0      1     2     3      4      5     6     7    8 */\
   "KILL","BUS","SEGV","SYS","PIPE","UALRM","TERM","ALRM","USR2","CLD",\
  /* 9     10     11    12    13     14     15     16     17    18 */\
   "PWR","USR3","BREAK","ABRT"
  /*19     20     21    22   */
#else
#define SIG_NAME "ZERO","HUP","INT","QUIT","ILL","TRAP","IOT","EMT","FPE","KILL","BUS","SEGV","SYS","PIPE","ALRM","TERM","URG","STOP","TSTP","CONT","CHLD","TTIN","TTOU","IO","XCPU","XFSZ","VTALRM","PROF","WINCH","USR1","USR2"		/**/
#endif /* OS2 */

/* STDCHAR:
 *	This symbol is defined to be the type of char used in stdio.h.
 *	It has the values "unsigned char" or "char".
 */
#define STDCHAR char	/**/

/* UIDTYPE:
 *	This symbol has a value like uid_t, int, ushort, or whatever type is
 *	used to declare user ids in the kernel.
 */
#define UIDTYPE int		/**/

/* VOIDFLAGS:
 *	This symbol indicates how much support of the void type is given by this
 *	compiler.  What various bits mean:
 *
 *	    1 = supports declaration of void
 *	    2 = supports arrays of pointers to functions returning void
 *	    4 = supports comparisons between pointers to void functions and
 *		    addresses of void functions
 *
 *	The package designer should define VOIDUSED to indicate the requirements
 *	of the package.  This can be done either by #defining VOIDUSED before
 *	including config.h, or by defining defvoidused in Myinit.U.  If the
 *	latter approach is taken, only those flags will be tested.  If the
 *	level of void support necessary is not present, defines void to int.
 */
#ifndef VOIDUSED
#define VOIDUSED 7
#endif
#define VOIDFLAGS 7
#if (VOIDFLAGS & VOIDUSED) != VOIDUSED
#define void int		/* is void to be avoided? */
#define M_VOID		/* Xenix strikes again */
#endif

/* PRIVLIB:
 *	This symbol contains the name of the private library for this package.
 *	The library is private in the sense that it needn't be in anyone's
 *	execution path, but it should be accessible by the world.  The program
 *	should be prepared to do ^ expansion.
 */
#define PRIVLIB "c:/bin/perl"		/**/

/*
 * BUGGY_MSC:
 *	This symbol is defined if you are the unfortunate owner of a buggy
 *	Microsoft C compiler and want to use intrinsic functions.  Versions
 *	up to 5.1 are known conform to this definition.
 */
/*#define BUGGY_MSC			/**/

/*
 * BINARY:
 *	This symbol is defined if you run under an operating system that
 *	distinguishes between binary and text files.  If so the function
 *	setmode will be used to set the file into binary mode.
 */
#define BINARY

#define S_ISUID 0
#define S_ISGID 0
#define CASTNEGFLOAT
