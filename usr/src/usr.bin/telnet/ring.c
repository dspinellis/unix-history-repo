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

#include	<stdio.h>
#include	<errno.h>

#ifdef	size_t
#undef	size_t
#endif

#include	<sys/types.h>
#include	<sys/ioctl.h>
#include	<sys/socket.h>

#include	"ring.h"
#include	"general.h"

/* Internal macros */

#if	!defined(MIN)
#define	MIN(a,b)	(((a)<(b))? (a):(b))
#endif	/* !defined(MIN) */

#define	ring_subtract(d,a,b)	((((int)(a))-((int)(b)) >= 0)? \
					(a)-(b): (((a)-(b))+(d)->size))

#define	ring_increment(d,a,c)	(((a)+(c) < (d)->top)? \
					(a)+(c) : (((a)+(c))-(d)->size))


/*
 * The following is a clock, used to determine full, empty, etc.
 *
 * There is some trickiness here.  Since the ring buffers are initialized
 * to ZERO on allocation, we need to make sure, when interpreting the
 * clock, that when the times are EQUAL, then the buffer is FULL, all
 * bytes have been SENT, no bytes are waiting to be ACKED, etc.
 */
static u_long ring_clock = 0;


#define	ring_add_all(d) (((d)->ack == (d)->add) && \
				((d)->acktime >= (d)->addtime))
#define	ring_send_all(d) (((d)->add == (d)->send) && \
				((d)->addtime > (d)->sendtime))
#define	ring_ack_all(d) (((d)->send == (d)->ack) && \
				((d)->sendtime > (d)->acktime))





/* Buffer state transition routines */

ring_init(ring, buffer, count)
Ring *ring;
char *buffer;
int count;
{
    memset((char *)ring, 0, sizeof *ring);

    ring->size = count;

    ring->add = ring->send = ring->ack = ring->bottom = buffer;

    ring->top = ring->bottom+ring->size;

    return 1;
}

/*
 * Add characters from current segment to ring buffer.
 */
void
ring_added(ring, count)
Ring *ring;
int count;
{
    ring->add = ring_increment(ring, ring->add, count);
    ring->addtime = ++ring_clock;
}

/*
 * We have just sent "c" bytes.
 */
void
ring_sent(ring, count)
Ring *ring;
int count;
{
    ring->send = ring_increment(ring, ring->send, count);
    ring->sendtime = ++ring_clock;
}

/*
 * We have just received an "ack" for "c" bytes.
 */
void
ring_acked(ring, count)
Ring *ring;
int count;
{
    ring->ack = ring_increment(ring, ring->ack, count);
    ring->acktime = ++ring_clock;
}

/*
 * We just sent and acked some data.
 */
void
ring_sent_acked(ring, count)
Ring *ring;
int count;
{
    ring_sent(ring, count);
    ring_acked(ring, count);
}


/* Buffer state query routines */


/* Number of bytes that may be added */
int
ring_empty_count(ring)
Ring *ring;
{
    if (ring_add_all(ring)) {	/* if empty */
	    return ring->size;
    } else {
	return ring_subtract(ring, ring->ack, ring->add);
    }
}

/* number of CONSECUTIVE bytes that may be added */
int
ring_empty_consecutive(ring)
Ring *ring;
{
    if ((ring->ack < ring->add) || ring_add_all(ring)) {
				    /*
				     * if ack is "below" add, or empty, then
				     * return distance to the top
				     */
	return ring_subtract(ring, ring->top, ring->add);
    } else {
				    /*
				     * else, return what we may.
				     */
	return ring_subtract(ring, ring->ack, ring->add);
    }
}

/* number of bytes that are available for sending */
int
ring_unsent_count(ring)
Ring *ring;
{
    if (ring_send_all(ring)) {
	return ring->size;	/* nothing sent, but full */
    } else {
	return ring_subtract(ring, ring->add, ring->send);
    }
}

/* number of CONSECUTIVE bytes available for sending */
int
ring_unsent_consecutive(ring)
Ring *ring;
{
    if ((ring->add < ring->send) || ring_send_all(ring)) {
	return ring_subtract(ring, ring->top, ring->send);
    } else {
	return ring_subtract(ring, ring->add, ring->send);
    }
}

/* number of bytes awaiting acking */
int
ring_unacked_count(ring)
Ring *ring;
{
    if (ring_ack_all(ring)) {
	    return ring->size;	/* last operation was a send - nothing done */
    } else {
	return ring_subtract(ring, ring->send, ring->ack);
    }
}


/*
 * Move data into the "add" portion of of the ring buffer.
 */
void
ring_add_data(ring, buffer, count)
Ring *ring;
char *buffer;
int count;
{
    int i;

    while (count) {
	i = MIN(count, ring_empty_consecutive(ring));
	memcpy(ring->add, buffer, i);
	ring_added(ring, i);
	count -= i;
	buffer += i;
    }
}


/*
 * Move data from the "send" portion of the ring buffer
 */
void
ring_send_data(ring, buffer, count)
Ring *ring;
char *buffer;
int count;
{
    int i;

    while (count) {
	i = MIN(count, ring_unsent_consecutive(ring));
	memcpy(buffer, ring->send, i);
	ring_sent(ring, i);
	count -= i;
	buffer += i;
    }
}

/* Mark routines */

/* XXX do something here */
void
ring_mark(ring)
Ring *ring;
{
}

int
ring_at_mark(ring)
Ring *ring;
{
    return 0;
}

void
ring_clear_mark(ring)
Ring *ring;
{
}
