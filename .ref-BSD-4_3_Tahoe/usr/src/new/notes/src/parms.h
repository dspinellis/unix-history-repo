#if	defined(RCSIDENT) && defined(MAINLINE)
static char zzparms[] = "$Header: parms.h,v 1.7.0.8 85/09/20 12:56:25 notes Rel $";
#endif	defined(RCSIDENT) && defined(MAINLINE)

/*
 *	This file contains the constants that must be reset on each system
 *	when notefiles are first installed.  Other constants exist in
 *	structs.h, but in general should not be modified unless needed.
 */

#define		ARCHTIME 	14			/* expire threshold */
#define		WORKSETSIZE	0			/* working set size */

#define		DFLTSH "/bin/sh"			/* default shell */
#define		DFLTED "/bin/ed"			/* default editor */
#define		SEQFILE "Dflt-Seq"			/* file in MSTDIR */
#define		DFLTSEQ "general,net.general"		/* unspecified NFSEQ */

/*
 *	These define defaults for various Unix functions that can
 *	be overidden by environment variables 
 */

#define		MAILER	"/usr/ucb/mail"			/* mailer to use */
#define		SUPERMAILER				/* is a smart mailer */
#define		PAGER	"/usr/ucb/more"			/* pg/more default */
#define		WRITE	"/bin/write"			/* user-user communication */

/*
 *	Definition of the domain this binary will run in. Examples
 *	are "UUCP", "ARPA", or "Uiuc.ARPA". This string is appended
 *	to the hostname and used to build addresses. Thus with a
 *	hostname of "uiucdcsb" and a FULLDOMAIN of "Uiuc.ARPA",
 *	my articles would show as being from "essick@uiucdcsb.Uiuc.ARPA".
 *
 *	Undefining this has some effects in the notes/news gateway
 *	with stripping/adding domains. It isn't a good idea to have
 *	this undefined.
 */

#define	FULLDOMAIN	"CS.UIUC.EDU"			/* local domain */
#undef	IDDOMAIN					/* not in unique id */

/*
 *	define at most 1 of these (V7, UNIX4.0, etc.) 
 *	If none are defined, things probably won't go too well.
 */

#undef	BSD41						/* Berkeley 4.1 bsd */
#undef	BSD41A						/* Berkeley 4.1a bsd */
#define	BSD42						/* Berkeley 4.2 Bsd */
#undef	V7						/* version 7 kernel */
#undef	SYSIII						/* BTL System III kernel */
#undef	UNIX40						/* Unix 4.0 kernel */
#undef	SYSV						/* BTL System V kernel */
#undef	BSD28						/* Berkeley 2.8 */
#undef	BSD29						/* Berkeley 2.9 */

/*
 *	Some configurable options.  These depend on how you like 
 *	to flavor your stew. 
 *		Define		What it gets you
 *		--------	-------------------------------------
 *		PROMPT		if you want a command prompt.
 *		USERHOST	if you want addresses in the form 
 *				user@host instead of host!user
 *		DYNADIR		enables code to determine Mstdir
 *				(the /usr/spool/notes directory) from
 *				the /etc/passwd.  When on, Mstdir is
 *				set to the directory above notes' home.
 *		K_KEY		Defined if you want k and K to be aliases
 *				for q and Q.  It is handy and allows you
 *				to read with one hand.  You can also
 *				get very aggravated when you hit "k" instead
 *				of "j".
 *		BIGTEXT		change from a u_short to a long counter
 *				to allow longer texts...
 */

#undef	PROMPT	"? "					/* command prompt */
#undef	PROMPTMSGX	(17)				/* stren(PROMPT) */
#define	USERHOST					/* user@host */
#undef	DYNADIR						/* dynamic Mstdir */
#define	K_KEY						/* k==q, K==Q */
#define	BIGTEXT						/* 32 bit counters */


/*
 *	The remaining options are pretty much stock.  You shouldn't
 *	have to play with them unless you want to run a very non-standard
 *	shop.
 */


#define		NFMAINT	"nfmaint"			/* internal logging */
#define		AUTOCREATE				/* automatic mknf */
#define		STATS					/* keep usage stats */

#define		FASTSEQ					/* use stat to check */

#define		DUMPCORE	"coredump"		/* subdir of UTILITY */

#define		FASTFORK				/* faster forking */
/*
 *	Thanks to Malcolm Slaney of Purdue EE for both BUFIO and FASTFORK
 *	modifications. (BUFIO is now used exclusively and so isn't 
 *	surrounded by ifdefs any more.
 */

/*
 *	REMAINDER OF THIS FILE PROBABLY DOESN'T NEED TO BE CHANGED
 *
 *	Do some specific defines that go along with particluar OS's.
 *	I may have missed some stuff and if you are running hybrid
 *	versions of Unix, you might need to change things.
 */

#if	defined(BSD41)					/* UCB 4.1 BSD */
#define	VFORK
#define	BSD4x
#endif	defined(BSD41)

#if	defined(BSD41A)					/* UCB 4.1a BSD */
#define	PORTBINARY					/* portable binaries */
#define	VFORK
#define	BSD4x
#endif	defined(BSD41A)

#if	defined(BSD42)					/* UCB 4.2 BSD */
#define	PORTBINARY
#define	VFORK
#define	BSD4x
#define	FILENAMELEN	255
#endif	defined(BSD42)

#if	defined(V7)					/* Research V7 */
#define	WHOAMI
#define	WHOAMIFILE	"/usr/include/whoami.h"
/*
 *	has a line in it of the form:
 *	#define sysname "XXXX"
 *	and maybe other lines too.
 */
#endif	defined(V7)

#if	defined(SYSIII)					/* BTL System III */
#define	UNAME						/* more port-binary */
#define	USG						/* BTL Unix */
#endif	defined(SYSIII)

#if	defined(UNIX40)					/* BTL (internal) */
#define	UNAME						/* more port-binary */
#define	USG						/* BTL Unix */
#endif	defined(UNIX40)

#if	defined(SYSV)					/* BTL System V */
#define	UNAME						/* more port-binary */
#define	USG						/* BTL Unix */
#endif	defined(SYSV)

#ifdef	USG						/* a BTL Unix */
/*
 *	BTL Unix doesn't have index/rindex. Instead have strchr/strrchr.
 *	define as macros here instead of plain substitution so won't
 *	get tripped on a variable called "index" someday.
 */
#define	index(s,c)	strchr(s,c)			/* 1st char */
#define	rindex(s,c)	strrchr(s,c)			/* last char */

extern char *strchr ();					/* for lint */
extern char *strrchr ();
#else
extern char *index ();					/* for lint */
extern char *rindex ();
#endif	USG

#if	defined(BSD28)					/* UCB 2.8 BSD */
#define	BSD2x
#endif	defined(BSD28)

#if	defined(BSD29)					/* UCB 2.9 BSD */
#define	BSD2x
#endif	defined(BSD29)

/*
 *	if the kernel hasn't already selected this...
 */

#ifndef		FILENAMELEN
#define		FILENAMELEN	14
#endif
#define		ROTATE		13			/* rotation factor */
#ifndef	PROMPTMSGX
#define		PROMPTMSGX	(15)			/* make sure it's there */
#endif	PROMPTMSGX

#ifndef	VFORK
#define		vfork	fork
/*
 *	There are legit uses of both fork/vfork.
 *	This lets us use vfork where we should and it defaults 
 *	to fork on systems that don't have it.
 *
 *	This may be a red herring for this implementation, but I 
 *	like the idea.
 */
#endif	VFORK

#define		SHAREDATA				/* in note.c, resp.c */
#undef		RUNSUID					/* use set-gid */

/*
 *	Version of the database in use. You don't want to change this.
 *	This reflects the format of the notesfile database and when
 *	the database format changes, this changes. It shouldn't
 *	change unless the database format changes.
 *	Format:		yyyymmdd
 */

#define		DBVERSION	19850101		/* current format */

/*
 *	If worrying about mangled id's coming back from older
 *	notesfile sites with shorter fields. This affects code
 *	in "find.c"
 */
#define	IDCOMPAT
#define	OLDSYSSZ	10				/* old SYSSZ */
#undef	FIXTIMES					/* fix gmttimes in future */
