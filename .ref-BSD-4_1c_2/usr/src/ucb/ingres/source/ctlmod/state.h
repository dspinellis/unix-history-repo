/*
**  STATE.H -- definitions for parameter vectors
**
**	Version:
**		@(#)state.h	7.1	2/5/81
*/

# ifndef CM_MAXST


# define	CM_MAXST	40	/* maximum # of states */

/* the state descriptor type */
typedef struct
{
	char	st_stat;	/* status bits, see below */
	char	st_type;	/* the type, see below */
	union
	{
		struct			/* ST_REMOT */
		{
			char	st_proc;	/* the remote process */
		} st_rem;
		struct			/* ST_LOCAL */
		{
			char	st_funcno;	/* the function number to call */
			char	st_next;	/* the next state */
		} st_loc;
	} st_v;
} state_t;

/* bits for st_stat */
# define	ST_EXTERN	0001	/* can be executed by user */

/* values for st_type */
# define	ST_UNDEF	0	/* undefined state */
# define	ST_LOCAL	1	/* state exists in this proc */
# define	ST_REMOT	2	/* state exists in another proc */


# endif CM_MAXST
