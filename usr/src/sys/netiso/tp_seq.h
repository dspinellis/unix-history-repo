/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tp_seq.h	7.6 (Berkeley) %G%
 */

/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* 
 * ARGO TP
 *
 * $Header: tp_seq.h,v 5.1 88/10/12 12:20:59 root Exp $
 * $Source: /usr/argo/sys/netiso/RCS/tp_seq.h,v $
 *
 * These macros perform sequence number arithmetic modulo (2**7 or 2**31).
 * The relevant fields in the tpcb are:
 *  	tp_seqmask : the mask of bits that define the sequence space.
 *  	tp_seqbit  : 1 + tp_seqmask
 *  	tp_seqhalf : tp_seqbit / 2 or half the sequence space (rounded up)
 * Not exactly fast, but at least it's maintainable.
 */

#ifndef __TP_SEQ__
#define __TP_SEQ__

#define SEQ(tpcb,x) \
	((x) & (tpcb)->tp_seqmask)

#define SEQ_GT(tpcb, seq, operand ) \
( ((int)((seq)-(operand)) > 0)\
? ((int)((seq)-(operand)) < (int)(tpcb)->tp_seqhalf)\
: !(-((int)(seq)-(operand)) < (int)(tpcb)->tp_seqhalf))

#define SEQ_GEQ(tpcb, seq, operand ) \
( ((int)((seq)-(operand)) >= 0)\
? ((int)((seq)-(operand)) < (int)(tpcb)->tp_seqhalf)\
: !((-((int)(seq)-(operand))) < (int)(tpcb)->tp_seqhalf))

#define SEQ_LEQ(tpcb, seq, operand ) \
( ((int)((seq)-(operand)) <= 0)\
? ((-(int)((seq)-(operand))) < (int)(tpcb)->tp_seqhalf)\
: !(((int)(seq)-(operand)) < (int)(tpcb)->tp_seqhalf))

#define SEQ_LT(tpcb, seq, operand ) \
( ((int)((seq)-(operand)) < 0)\
? ((-(int)((seq)-(operand))) < (int)(tpcb)->tp_seqhalf)\
: !(((int)(seq)-(operand)) < (int)(tpcb)->tp_seqhalf))
	
#define SEQ_MIN(tpcb, a, b) ( SEQ_GT(tpcb, a, b) ? b : a)

#define SEQ_MAX(tpcb, a, b) ( SEQ_GT(tpcb, a, b) ? a : b)

#define SEQ_INC(tpcb, Seq) ((++Seq), ((Seq) &= (tpcb)->tp_seqmask))

#define SEQ_DEC(tpcb, Seq)\
	((Seq) = (((Seq)+(unsigned)((int)(tpcb)->tp_seqbit - 1))&(tpcb)->tp_seqmask))

/* (amt) had better be less than the seq bit ! */

#define SEQ_SUB(tpcb, Seq, amt)\
	(((Seq) + (unsigned)((int)(tpcb)->tp_seqbit - amt)) & (tpcb)->tp_seqmask)
#define SEQ_ADD(tpcb, Seq, amt) (((Seq) + (unsigned)amt) & (tpcb)->tp_seqmask)


#define IN_RWINDOW(tpcb, seq, lwe, uwe)\
	( SEQ_GEQ(tpcb, seq, lwe) && SEQ_LT(tpcb, seq, uwe) )

#define IN_SWINDOW(tpcb, seq, lwe, uwe)\
	( SEQ_GT(tpcb, seq, lwe) && SEQ_LEQ(tpcb, seq, uwe) )

#endif /* __TP_SEQ__ */
