/* mmdfonly.h - include file for stand-alone BBoards MMDF-II channel */


#ifdef	V4_2BSD
#define	BSD42
#endif	V4_2BSD
					/* manifest constants */
#define	NOTOK	(-1)
#define	OK	0

#define	NULLCP	((char *) 0)

#define	MAXFOLDER	2000


					/* no mtstailor/.mh_profile here */
#define	mts_init(n)
#define	m_gmprot()	0644


					/* maildrop delimiters */
#define	mmdlm1	delim1
#define	mmdlm2	delim2

extern char *delim1;
extern char *delim2;


					/* locking parameters */
#define	lockstyle	LOK_UNIX
#define	lockldir	lckdfldir

extern char *lckdfldir;


					/* MH subroutines/stubs */
void	admonish ();
int	stringdex ();
char   *r1bindex ();
