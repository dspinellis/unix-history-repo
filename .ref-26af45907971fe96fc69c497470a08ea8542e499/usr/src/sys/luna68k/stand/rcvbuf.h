/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rcvbuf.h	7.1 (Berkeley) %G%
 */

/*
 * rcvbuf.h -- receive buffer control structure definition
 *	by A.Fujita, Dec-12-1992
 */

#define	RBUF_SIZE	128

struct rcvbuf {
	u_char	*rb_push;
	u_char	*rb_pop;
	u_char	 rb_buf[RBUF_SIZE+4];
};

#define RBUF_INIT(n)	rcvbuf[n].rb_pop = rcvbuf[n].rb_push = &rcvbuf[n].rb_buf[RBUF_SIZE]

#define PUSH_RBUF(n, c)	*(--rcvbuf[n].rb_push) = c ; \
			if (rcvbuf[n].rb_push == rcvbuf[n].rb_buf) \
				rcvbuf[n].rb_push = &rcvbuf[n].rb_buf[RBUF_SIZE]

#define POP_RBUF(n, c)	c= *(--rcvbuf[n].rb_pop); \
			if (rcvbuf[n].rb_pop == rcvbuf[n].rb_buf) \
				rcvbuf[n].rb_pop = &rcvbuf[n].rb_buf[RBUF_SIZE]

#define RBUF_EMPTY(n)	(rcvbuf[n].rb_push == rcvbuf[n].rb_pop ? 1: 0)
