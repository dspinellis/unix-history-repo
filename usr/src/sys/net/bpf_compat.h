/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bpf_compat.h	8.1 (Berkeley) %G%
 */

/* from: $Header: bpf_compat.h,v 1.1 92/05/22 15:33:20 mccanne Exp $ (LBL) */

/*
 * Some hacks for compatibility across SunOS and 4.4BSD.  We emulate malloc
 * and free with mbuf clusters.  We store a pointer to the mbuf in the first
 * word of the mbuf and return 8 bytes passed the start of data (for double
 * word alignment).  We cannot just use offsets because clusters are not at
 * a fixed offset from the associated mbuf.  Sorry for this kludge.
 */
#define malloc(size, type, canwait) bpf_alloc(size, canwait)
#define free(cp, type) m_free(*(struct mbuf **)(cp - 8))
#define M_WAITOK M_WAIT

/* This mapping works for our purposes. */
#define ERESTART EINTR
