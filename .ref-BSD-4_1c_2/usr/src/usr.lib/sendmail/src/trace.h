/*
**  Trace Package.
**
**	Version:
**		%W%	%Y%	%G%
*/

typedef u_char	*TRACEV;

extern TRACEV	tTvect;			/* current trace vector */

# ifndef tTVECT
# define tTVECT		tTvect
# endif tTVECT

# define tTf(flag, level)	(tTVECT[flag] >= level)
# define tTlevel(flag)		(tTVECT[flag])
