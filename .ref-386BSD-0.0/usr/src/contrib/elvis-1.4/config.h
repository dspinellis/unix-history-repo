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

/*************************** compilers **************************************/
 
#ifndef	MICROSOFT
# define MICROSOFT	0
#endif

#ifndef	TURBOC
# define TURBOC		0
#endif

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

/* Only MSDOS, TOS, and OS9 need a special function for reading from the
 * keyboard.  All others just read from file descriptor 0.
 */
#if !MSDOS && !TOS && !OSK
# define ttyread(buf, len)	read(0, buf, (unsigned)len)	/* raw read */
#endif
#if !TOS
# define ttywrite(buf, len)	write(1, buf, (unsigned)(len))	/* raw write */
#endif

/* The strchr() function is an official standard now, so everybody has it
 * except Unix version 7 (which is old) and BSD Unix (which is academic).
 * Those guys use something called index() to do the same thing.
 */
#if BSD || UNIX7 || OSK
# define strchr	index
#endif
extern char *strchr();

/* BSD uses bcopy() instead of memcpy() */
#if BSD
#define memcpy(dest, src, siz)	bcopy(src, dest, siz)
#endif

/* text versa binary mode for read/write */
#if !TOS
#define	tread(fd,buf,n)		read(fd,buf,(unsigned)(n))
#define twrite(fd,buf,n)	write(fd,buf,(unsigned)(n))
#endif

/**************************** Compiler quirks *********************************/

/* the UNIX version 7 and (some) TOS compilers, don't allow "void" */
#if UNIX7 || TOS
# define void int
#endif

/* as far as I know, all compilers except version 7 support unsigned char */
/* NEWFLASH: the Minix-ST compiler has subtle problems with unsigned char */
#if UNIX7 || MINIX
# define UCHAR(c)	((c) & 0xff)
# define uchar		char
#else
# define UCHAR(c)	((unsigned char)(c))
# define uchar		unsigned char
#endif

/* Some compilers prefer to have malloc declared as returning a (void *) */
#if BSD
extern void *malloc();
#else
extern char *malloc();
#endif

/* Most compilers could benefit from using the "register" storage class */
#if 1
# define REG	register
#endif

/******************* Names of files and environment vars **********************/

#define TMPDIR	"/var/tmp"
# define TMPNAME	"%s/elv%x%04x%03x" /* temp file */
# define CUTNAME	"%s/elv_%04x%03x" /* cut buffer's temp file */
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

#ifndef	TAGS
# define TAGS		"tags"		/* tags file */
#endif

#ifndef TMPNAME
# define TMPNAME	"%s/elv%x%04x.%03x"	/* temp file */
#endif

#ifndef CUTNAME
# define CUTNAME	"%s/elv_%04x.%03x"	/* cut buffer's temp file */
#endif

#ifndef	EXRC
# define EXRC		"elvis.rc"
#endif

#ifndef HMEXRC
# if !MSDOS && !TOS
#  define HMEXRC	EXRC
# endif
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

#ifndef MAXMAPS
# define MAXMAPS	20		/* number of :map keys */
#endif
#ifndef MAXDIGS
# define MAXDIGS	30		/* number of :digraph combos */
#endif
#ifndef MAXABBR
# define MAXABBR	20		/* number of :abbr entries */
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
