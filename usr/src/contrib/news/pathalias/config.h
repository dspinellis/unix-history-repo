/* pathalias -- by steve bellovin, as told to peter honeyman */

/**************************************************************************
 * +--------------------------------------------------------------------+ *
 * |                    begin configuration section                     | *
 * +--------------------------------------------------------------------+ *
 **************************************************************************/

#define STRCHR		/* have strchr -- system v and many others */

#undef UNAME		/* have uname() -- probably system v or 8th ed. */
#define MEMSET		/* have memset() -- probably system v or 8th ed. */

#define GETHOSTNAME	/* have gethostname() -- probably bsd */
#define BZERO		/* have bzero() -- probably bsd */

#define RESOLVER	/* have internet domain name resolver */

/* default place for dbm output of makedb (or use -o at run-time) */
#define	ALIASDB	"/usr/local/lib/palias"

/**************************************************************************
 * +--------------------------------------------------------------------+ *
 * |                    end of configuration section                    | *
 * +--------------------------------------------------------------------+ *
 **************************************************************************/



#ifdef MAIN
#ifndef lint
static char	*c_sccsid = "@(#)config.h	9.5 91/06/11";
#endif /*lint*/
#endif /*MAIN*/

/* the usual case: unix */
#define	NULL_DEVICE	"/dev/null"
#define	OK		0
#define	ERROR		1
#define	SEVERE_ERROR	(-1)
#define STDIO_H		<stdio.h>
#define CTYPE_H		<ctype.h>

#ifdef	VMS
#include	ssdef
#include	stsdef

#undef	NULL_DEVICE
#define	NULL_DEVICE	"NL:"

#undef	OK
#define	OK		SS$_NORMAL

#undef	ERROR
#define	ERROR		(STS$K_ERROR|STS$M_INHIB_MSG)

#undef	SEVERE_ERROR
#define	SEVERE_ERROR	(STS$K_SEVERE|STS$M_INHIB_MSG)

#undef	STDIO_H
#define	STDIO_H		stdio

#undef	CTYPE_H
#define	CTYPE_H		ctype
#endif

/*
 * malloc/free fine tuned for pathalias.
 *
 * MYMALLOC should work everwhere, so it's not a configuration
 * option (anymore).  nonetheless, if you're getting strange
 * core dumps (or panics!), comment out the following manifest,
 * and use the inferior C library malloc/free.
 */
#define MYMALLOC	/**/

#ifdef MYMALLOC
#define malloc mymalloc
#define calloc(n, s) malloc ((n)*(s))
#define free(s)
#define cfree(s)
extern char *memget();
#else /* !MYMALLOC */
extern char *calloc();
#endif /* MYMALLOC */

#ifdef STRCHR
#define index strchr
#define rindex strrchr
#else
#define strchr index
#define strrchr rindex
#endif

#ifdef BZERO
#define strclear(s, n)	((void) bzero((s), (n)))
#else /*!BZERO*/

#ifdef MEMSET
extern char	*memset();
#define strclear(s, n)	((void) memset((s), 0, (n)))
#else /*!MEMSET*/
extern void	strclear();
#endif /*MEMSET*/

#endif /*BZERO*/

extern char	*malloc();
extern char	*strcpy(), *index(), *rindex();

#ifndef STATIC

#ifdef DEBUG
#define STATIC extern
#else /*DEBUG*/
#define STATIC static
#endif /*DEBUG*/

#endif /*STATIC*/
