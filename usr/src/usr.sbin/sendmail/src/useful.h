/*
**  USEFUL.H -- Some useful stuff.
**
**	@(#)useful.h	3.3	%G%
*/

# ifndef makedev
# include <sys/types.h>
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

/* define the types of some common functions */
extern char	*strcpy();
extern char	*strcat();
extern char	*malloc();
extern char	*index(), *rindex();
extern int	errno;
extern char	*sprintf();
extern long	time();
extern char	*ctime();
# ifndef V6
extern char	*getenv();
# endif
