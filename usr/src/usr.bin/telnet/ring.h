/*
 * This defines a structure for a receive or send ring buffer.
 *
 * The circular buffer actually has three parts:
 *(((
 *	full, sent, not acked:	[ack, send)
 *	full, not sent:		[send, add)
 *	empty:			[add, ack)
 *]]]
 *
 * Any given byte will go through "empty" -> "send" -> "ack" -> "empty"
 * as data is moved through it.  The transition from "ack" to "empty"
 * may occur instantaneously (as in the case of sending data up to another
 * process).
 */
typedef struct {
    char	*ack,		/* where you can't add at */
    		*send,		/* where data comes out of */
    		*add,		/* where data comes in to */
		*bottom,	/* lowest address in buffer */
		*top;		/* highest address+1 in buffer */
    int		size;		/* size in bytes of buffer */
    u_long	acktime,	/* the relations between these clocks */
		sendtime,	/* help us keep straight full, empty, etc. */
		addtime;
} Ring;

/* Here are some functions and macros to deal with the ring buffer */


#if	defined(LINT_ARGS)

/* Initialization routine */
extern int
	ring_init(Ring *ring, char *buffer, int count);

/* Data movement routines */
extern void
	ring_add_data(Ring *ring, char *buffer, int count),
	ring_send_data(Ring *ring, char *buffer, int count);

/* Buffer state transition routines */
extern void
	ring_added(Ring *ring, int count),
	ring_sent(Ring *ring, int count),
	ring_acked(Ring *ring, int count);

/* Buffer state query routines */
extern int
	ring_empty_count(Ring *ring),
	ring_empty_consecutive(Ring *ring),
	ring_unsent_count(Ring *ring),
	ring_unsent_consecutive(Ring *ring),
	ring_unacked_count(Ring *ring);

#endif	/* defined(LINT_ARGS) */
