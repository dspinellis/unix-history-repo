/*	nsp_seq.h	1.3	82/10/09	*/

/*
 * NSP sequence numbers are 12 bit integers operated
 * on with modular arithmetic.  These macros can be
 * used to compare and perform arithmetic on such integers.
 */
#define	MAXSEQ	(1<<12)
#define	SEQMASK	(MAXSEQ-1)

#define	SEQ_LSS(a, b)	(nsp_seqcmp(a, b) < 0)
#define	SEQ_LEQ(a, b)	(nsp_seqcmp(a, b) <= 0)
#define	SEQ_GTR(a, b)	(nsp_seqcmp(a, b) > 0)
#define	SEQ_GEQ(a, b)	(nsp_seqcmp(a, b) >= 0)

#define	SEQ_ADD(a, b)	(((a) + (b)) & SEQMASK)
#define	SEQ_SUB(a, b)	(((a) - (b)) & SEQMASK)
