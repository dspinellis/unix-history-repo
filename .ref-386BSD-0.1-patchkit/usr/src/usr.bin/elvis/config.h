/*
 * vi configuration file
 */

# define BSD		1


/* All undefined symbols are defined to zero here, to allow for older    */
/* compilers which dont understand #if defined() or #if UNDEFINED_SYMBOL */

# define UNIXV	0		/* UNIX - AT&T SYSV */
# define UNIX7	0		/* UNIX - version 7 */
# define MSDOS	0		/* PC		*/
# define TOS	0		/* Atari ST	*/
# define AMIGA	0		/* Commodore Amiga */
# define OSK	0		/* OS-9 / 68k */
# define COHERENT 0		/* Coherent */
# define MINIX	0

# define ANY_UNIX 1

/******************************* Credit ************************************/

#if MSDOS
# define CREDIT "Ported to MS-DOS by Guntram Blohm & Martin Patzel"
#endif

#if TOS
# define CREDIT "Ported to Atari/TOS by Guntram Blohm & Martin Patzel"
#endif

#if OSK
# define CREDIT	"Ported to Microware OS9/68k by Peter Reinig"
#endif

#if COHERENT
# define CREDIT	"Ported to Coherent by Esa Ahola"
#endif

/*************************** functions depending on OS *********************/

extern char *strchr();

#define	tread(fd,buf,n)		read(fd,buf,(unsigned)(n))
#define twrite(fd,buf,n)	write(fd,buf,(unsigned)(n))

#define ttywrite(buf, len)	write(1, buf, (unsigned)(len))	/* raw write */

extern void *malloc();

/* Most compilers could benefit from using the "register" storage class */
#if 1
# define REG	register
#endif

typedef	unsigned char uchar;
#define	UCHAR(s)	((unsigned char) (s))

/******************* Names of files and environment vars **********************/

#define TMPDIR	"/var/tmp"
#define TMPNAME	"%s/elvis%04x%03x" /* temp file */
#define CUTNAME	"%s/elvis_%04x%03x" /* cut buffer's temp file */
# ifndef EXRC
#  define EXRC		".exrc"		/* init file in current directory */
# endif
# define SCRATCHOUT	"%s/soXXXXXX"	/* temp file used as input to filter */
# ifndef EXINIT
#  define EXINIT	"EXINIT"
# endif
# ifndef SHELL
#  define SHELL		"/bin/sh"	/* default shell */
# endif
# ifndef PRSVDIR
#  define PRSVDIR	"/var/preserve"	/* directory where preserved file live */
# endif
# ifndef PRSVINDEX
#  define PRSVINDEX	"/var/preserve/Index" /* index of files in PRSVDIR */
# endif

#ifndef	TAGS
# define TAGS		"tags"		/* tags file */
#endif

#ifndef	KEYWORDPRG
# define KEYWORDPRG	"ref"
#endif

#ifndef	SCRATCHOUT
# define SCRATCHIN	"%s/SIXXXXXX"
# define SCRATCHOUT	"%s/SOXXXXXX"
#endif

#ifndef ERRLIST
# define ERRLIST	"errlist"
#endif

#ifndef	SLASH
# define SLASH		'/'
#endif

#ifndef SHELL
# define SHELL		"shell"
#endif

#ifndef REG
# define REG
#endif

#ifndef NEEDSYNC
# define NEEDSYNC	FALSE
#endif

#ifndef FILEPERMS
# define FILEPERMS	0666
#endif

#ifndef CC_COMMAND
# define CC_COMMAND	"cc -c"
#endif

#ifndef MAKE_COMMAND
# define MAKE_COMMAND	"make"
#endif

#ifndef REDIRECT
# define REDIRECT	"2>"
#endif

#ifndef PRESERVE
# define PRESERVE	"/usr/libexec/elvispreserve"	/* name of the "preserve" program */
#endif

#if	!defined(CRUNCH) && defined(LETS_GET_SMALL)
#define CRUNCH
#endif

#if	defined(CRUNCH) && !defined(LETS_GET_SMALL)
#define LETS_GET_SMALL
#endif

#ifndef BLKSIZE
# ifdef CRUNCH
#  define BLKSIZE	1024
# else
#  define BLKSIZE	2048
# endif
#endif

#ifndef KEYBUFSIZE
# define KEYBUFSIZE	1000
#endif

#ifdef	LETS_GET_SMALL
#define NO_CHARATTR
#define NO_DIGRAPH
#define NO_SENTENCE
#define NO_EXTENSIONS
#define NO_MAGIC
#define NO_ERRLIST
#define NO_MODELINE
#define NO_SHOWMATCH
#define	NO_SHOWMODE
#define NO_OPTCOLS
#define NO_MKEXRC
#endif
