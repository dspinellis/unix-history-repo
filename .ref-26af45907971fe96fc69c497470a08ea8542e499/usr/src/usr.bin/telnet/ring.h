/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ring.h	5.4 (Berkeley) %G%
 */

#if defined(P)
# undef P
#endif

#if defined(__STDC__) || defined(LINT_ARGS)
# define	P(x)	x
#else
# define	P(x)	()
#endif

/*
 * This defines a structure for a ring buffer.
 *
 * The circular buffer has two parts:
 *(((
 *	full:	[consume, supply)
 *	empty:	[supply, consume)
 *]]]
 *
 */
typedef struct {
    unsigned char	*consume,	/* where data comes out of */
			*supply,	/* where data comes in to */
			*bottom,	/* lowest address in buffer */
			*top,		/* highest address+1 in buffer */
			*mark;		/* marker (user defined) */
#ifdef	ENCRYPTION
    unsigned char	*clearto;	/* Data to this point is clear text */
    unsigned char	*encryyptedto;	/* Data is encrypted to here */
#endif	/* ENCRYPTION */
    int		size;		/* size in bytes of buffer */
    u_long	consumetime,	/* help us keep straight full, empty, etc. */
		supplytime;
} Ring;

/* Here are some functions and macros to deal with the ring buffer */

/* Initialization routine */
extern int
	ring_init P((Ring *ring, unsigned char *buffer, int count));

/* Data movement routines */
extern void
	ring_supply_data P((Ring *ring, unsigned char *buffer, int count));
#ifdef notdef
extern void
	ring_consume_data P((Ring *ring, unsigned char *buffer, int count));
#endif

/* Buffer state transition routines */
extern void
	ring_supplied P((Ring *ring, int count)),
	ring_consumed P((Ring *ring, int count));

/* Buffer state query routines */
extern int
	ring_empty_count P((Ring *ring)),
	ring_empty_consecutive P((Ring *ring)),
	ring_full_count P((Ring *ring)),
	ring_full_consecutive P((Ring *ring));

#ifdef	ENCRYPTION
extern void
	ring_encrypt P((Ring *ring, void (*func)())),
	ring_clearto P((Ring *ring));
#endif	/* ENCRYPTION */

extern void
    ring_clear_mark(),
    ring_mark();
