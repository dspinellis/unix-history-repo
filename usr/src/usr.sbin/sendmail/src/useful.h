/*
**  USEFUL.H -- Some useful stuff.
**
**	@(#)useful.h	3.9		%G%
*/

# ifndef makedev
# include <sys/param.h>
# endif

/* support for bool type */
typedef char	bool;
# define TRUE	1
# define FALSE	0

# ifndef NULL
# define NULL	0
# endif NULL

/* bit hacking */
# define bitset(bit, word)	((word) & (bit))

/* assertions */
# ifndef NASSERT
# define ASSERT(expr, msg, parm)\
	if (!(expr))\
	{\
		fprintf(stderr, "assertion botch: %s:%d: ", __FILE__, __LINE__);\
		fprintf(stderr, msg, parm);\
	}
# else NASSERT
# define ASSERT(expr, msg, parm)
# endif NASSERT

/* sccs id's */
# ifndef lint
# define SCCSID(arg)	static char SccsId[] = "arg";
# else lint
# define SCCSID(arg)
# endif lint

/* define the types of some common functions */
extern char	*strcpy(), *strncpy();
extern char	*strcat(), *strncat();
extern char	*malloc();
extern char	*index(), *rindex();
extern int	errno;
extern char	*sprintf();
extern time_t	time();
extern char	*ctime();
# ifndef V6
extern char	*getenv();
# endif V6
# ifndef VMUNIX
typedef char	*u_short;
typedef long	u_long;
typedef char	u_char;
typedef int	void;
# endif VMUNIX
