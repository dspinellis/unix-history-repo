/*
**  USEFUL.H -- useful stuff.
**
**	Version:
**		@(#)useful.h	7.1	2/5/81
*/

# ifndef TRUE
# define TRUE		1	/* logical one, true, yes, ok, etc.*/
# define FALSE		0	/* logical zero, false, no, nop, etc. */

typedef char	bool;		/* the boolean type */
# endif TRUE

# ifndef NULL
# define NULL		0	/* the null pointer */
# endif NULL

# ifndef bitset
# define	bitset(bit, word)	((bit) & (word))
# define	setbit(bit, word)	word |= bit
# define	clrbit(bit, word)	word &= ~bit
# endif bitset

# ifndef min
# define	min(a, b)	(((a) < (b))? (a): (b))
# define	max(a, b)	(((a) > (b))? (a): (b))
# endif min
