/* pathalias -- by steve bellovin, as told to peter honeyman */

/* #define STRCHR	/* have strchr, not index -- probably system v */
/* #define UNAME	/* have uname() -- probably system v or 8th ed. */
/* #define MEMSET	/* have memset() -- probably system v or 8th ed. */

#define GETHOSTNAME	/* have gethostname() -- probably 4.2bsd */
#define BZERO	/* have bzero() -- probably 4.2bsd */

/* default place for dbm output of makedb (or use -o file at run-time) */
#define	ALIASDB	"/usr/new/lib/palias"



/**************************************************************************
 *									  *
 * +--------------------------------------------------------------------+ *
 * |									| *
 * |			END OF CONFIGURATION SECTION			| *
 * |									| *
 * |				EDIT NO MORE				| *
 * |									| *
 * +--------------------------------------------------------------------+ *
 *									  *
 **************************************************************************/

#ifdef MAIN
#ifndef lint
static char	*c_sccsid = "@(#)config.h	8.1 (down!honey) 86/01/19";
#endif /*lint*/
#endif /*MAIN*/

/*
 * malloc/free fine tuned for pathalias.
 *
 * MYMALLOC should work everwhere, so it's not a configuration
 * option (anymore).  nonetheless, if you're getting strange
 * core dumps (or panics!), comment out the following manifest,
 * and use the inferior C library malloc/free.
 *
 * report problems to princeton!honey.
 */
#define MYMALLOC	/**/

#ifdef MYMALLOC
#define malloc mymalloc
#define calloc(n, s) malloc ((n)*(s))
#define free(s)
#define cfree(s)
extern char *memget();
#endif

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
