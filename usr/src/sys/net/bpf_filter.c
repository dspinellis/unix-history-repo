/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that: (1) source code distributions
 * retain the above copyright notice and this paragraph in its entirety, (2)
 * distributions including binary code include the above copyright notice and
 * this paragraph in its entirety in the documentation or other materials
 * provided with the distribution, and (3) all advertising materials mentioning
 * features or use of this software display the following acknowledgement:
 * ``This product includes software developed by the University of California,
 * Lawrence Berkeley Laboratory and its contributors.'' Neither the name of
 * the University nor the names of its contributors may be used to endorse
 * or promote products derived from this software without specific prior
 * written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */
#if !(defined(lint) || defined(KERNEL))
static char rcsid[] =
    "@(#) $Header: bpf_filter.c,v 1.10 91/04/24 22:07:07 mccanne Locked $ (LBL)";
#endif

#include <sys/param.h>
#include <sys/types.h>
#include <sys/time.h>
#include <net/bpf.h>

#if defined(sparc) || defined(mips)
#define ALIGN
#endif

#ifndef ALIGN
#define EXTRACT_SHORT(p)	(ntohs(*(u_short *)p))
#define EXTRACT_LONG(p)		(ntohl(*(u_long *)p))
#else
#define EXTRACT_SHORT(p)\
	((u_short)\
		(*((u_char *)p+0)<<8|\
		 *((u_char *)p+1)<<0))
#define EXTRACT_LONG(p)\
		(*((u_char *)p+0)<<24|\
		 *((u_char *)p+1)<<16|\
		 *((u_char *)p+2)<<8|\
		 *((u_char *)p+3)<<0)
#endif

#ifdef KERNEL
#include <sys/mbuf.h>
#define MINDEX(m, k) \
{ \
	register int len = m->m_len; \
 \
	while (k >= len) { \
		k -= len; \
		m = m->m_next; \
		if (m == 0) \
			return 0; \
		len = m->m_len; \
	} \
}
#endif

/*
 * Execute the filter program starting at pc on the packet p
 * wirelen is the length of the original packet
 * buflen is the amount of data present
 */
u_int
bpf_filter(pc, p, wirelen, buflen)
	register struct bpf_insn *pc;
	register u_char *p;
	u_int wirelen;
	register u_int buflen;
{
	register long A, X;
	register int k;
	long mem[BPF_MEMWORDS];

	if (pc == 0)
		/*
		 * No filter means accept all.
		 */
		return (u_int)-1;
#ifdef lint
	A = 0;
	X = 0;
#endif
	--pc;
	while (1) {
		++pc;
		switch (pc->code) {

		default:
#ifdef KERNEL
			return 0;
#else
			abort();
#endif			
		case BPF_RET|BPF_K:
			return (u_int)pc->k;

		case BPF_RET|BPF_A:
			return (u_int)A;

		case BPF_LD|BPF_W|BPF_ABS:
			k = pc->k;
			if (k + sizeof(long) > buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				A = EXTRACT_LONG(mtod(m, char *) + k);
				break;
#else
				return 0;
#endif
			}
#ifdef ALIGN
			if (((int)(p + k) & 3) != 0)
				A = EXTRACT_LONG(&p[k]);
			else
#endif
				A = *(long *)(p + k);
			break;

		case BPF_LD|BPF_H|BPF_ABS:
			k = pc->k;
			if (k + sizeof(short) > buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				A = EXTRACT_SHORT(mtod(m, char *) + k);
				break;
#else
				return 0;
#endif
			}
			A = EXTRACT_SHORT(&p[k]);
			break;

		case BPF_LD|BPF_B|BPF_ABS:
			k = pc->k;
			if (k >= buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				A = mtod(m, char *)[k];
				break;
#else
				return 0;
#endif
			}
			A = p[k];
			break;

		case BPF_LD|BPF_W|BPF_LEN:
			A = wirelen;
			break;

		case BPF_LDX|BPF_W|BPF_LEN:
			X = wirelen;
			break;

		case BPF_LD|BPF_W|BPF_IND:
			k = X + pc->k;
			if (k + sizeof(long) > buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				A = EXTRACT_LONG(mtod(m, char *) + k);
				break;
#else
				return 0;
#endif
			}
#ifdef ALIGN
			if (((int)(p + k) & 3) != 0)
				A = EXTRACT_LONG(&p[k]);
			else
#endif
				A = *(long *)(p + k);
			break;

		case BPF_LD|BPF_H|BPF_IND:
			k = X + pc->k;
			if (k + sizeof(short) > buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				A = EXTRACT_SHORT(mtod(m, char *) + k);
				break;
#else
				return 0;
#endif
			}
			A = EXTRACT_SHORT(&p[k]);
			break;

		case BPF_LD|BPF_B|BPF_IND:
			k = X + pc->k;
			if (k >= buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				A = mtod(m, char *)[k];
				break;
#else
				return 0;
#endif
			}
			A = p[k];
			break;

		case BPF_LDX|BPF_MSH|BPF_B:
			k = pc->k;
			if (k >= buflen) {
#ifdef KERNEL
				register struct mbuf *m;

				if (buflen != 0)
					return 0;
				m = (struct mbuf *)p;
				MINDEX(m, k);
				X = (mtod(m, char *)[k] & 0xf) << 2;
				break;
#else
				return 0;
#endif
			}
			X = (p[pc->k] & 0xf) << 2;
			break;

		case BPF_LD|BPF_IMM:
			A = pc->k;
			break;

		case BPF_LDX|BPF_IMM:
			X = pc->k;
			break;

		case BPF_LD|BPF_MEM:
			A = mem[pc->k];
			break;
			
		case BPF_LDX|BPF_MEM:
			X = mem[pc->k];
			break;

		case BPF_ST:
			mem[pc->k] = A;
			break;

		case BPF_STX:
			mem[pc->k] = X;
			break;

		case BPF_JMP|BPF_JA:
			pc += pc->k;
			break;

		case BPF_JMP|BPF_JGT|BPF_K:
			pc += (A > pc->k) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JGE|BPF_K:
			pc += (A >= pc->k) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JEQ|BPF_K:
			pc += (A == pc->k) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JSET|BPF_K:
			pc += (A & pc->k) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JGT|BPF_X:
			pc += (A > X) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JGE|BPF_X:
			pc += (A >= X) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JEQ|BPF_X:
			pc += (A == X) ? pc->jt : pc->jf;
			break;

		case BPF_JMP|BPF_JSET|BPF_X:
			pc += (A & X) ? pc->jt : pc->jf;
			break;

		case BPF_ALU|BPF_ADD|BPF_X:
			A += X;
			break;
			
		case BPF_ALU|BPF_SUB|BPF_X:
			A -= X;
			break;
			
		case BPF_ALU|BPF_MUL|BPF_X:
			A *= X;
			break;
			
		case BPF_ALU|BPF_DIV|BPF_X:
			if (X == 0)
				return 0;
			A /= X;
			break;
			
		case BPF_ALU|BPF_AND|BPF_X:
			A &= X;
			break;
			
		case BPF_ALU|BPF_OR|BPF_X:
			A |= X;
			break;

		case BPF_ALU|BPF_LSH|BPF_X:
			A <<= X;
			break;

		case BPF_ALU|BPF_RSH|BPF_X:
			A >>= X;
			break;

		case BPF_ALU|BPF_ADD|BPF_K:
			A += pc->k;
			break;
			
		case BPF_ALU|BPF_SUB|BPF_K:
			A -= pc->k;
			break;
			
		case BPF_ALU|BPF_MUL|BPF_K:
			A *= pc->k;
			break;
			
		case BPF_ALU|BPF_DIV|BPF_K:
			A /= pc->k;
			break;
			
		case BPF_ALU|BPF_AND|BPF_K:
			A &= pc->k;
			break;
			
		case BPF_ALU|BPF_OR|BPF_K:
			A |= pc->k;
			break;

		case BPF_ALU|BPF_LSH|BPF_K:
			A <<= pc->k;
			break;

		case BPF_ALU|BPF_RSH|BPF_K:
			A >>= pc->k;
			break;

		case BPF_ALU|BPF_NEG:
			A = -A;
			break;

		case BPF_MISC|BPF_TAX:
			X = A;
			break;

		case BPF_MISC|BPF_TXA:
			A = X;
			break;
		}
	}
}

#ifdef KERNEL
/*
 * Return true if the 'fcode' is a valid filter program.
 * The constraints are that each jump be forward and to a valid
 * code.  The code must terminate with either an accept or reject. 
 * 'valid' is an array for use by the routine (it must be at least
 * 'len' bytes long).  
 *
 * The kernel needs to be able to verify an application's filter code.
 * Otherwise, a bogus program could easily crash the system.
 */
int
bpf_validate(f, len)
	struct bpf_insn *f;
	int len;
{
	register int i;
	register struct bpf_insn *p;

	for (i = 0; i < len; ++i) {
		/*
		 * Check that that jumps are forward, and within 
		 * the code block.
		 */
		p = &f[i];
		if (BPF_CLASS(p->code) == BPF_JMP) {
			register int from = i + 1;

			if (BPF_OP(p->code) == BPF_JA) {
				if (from + p->k >= len)
					return 0;
			}
			else if (from + p->jt >= len || from + p->jf >= len)
				return 0;
		}
		/*
		 * Check that memory operations use valid addresses.
		 */
		if ((BPF_CLASS(p->code) == BPF_ST ||
		     (BPF_CLASS(p->code) == BPF_LD && 
		      (p->code & 0xe0) == BPF_MEM)) &&
		    (p->k >= BPF_MEMWORDS || p->k < 0))
			return 0;
		/*
		 * Check for constant division by 0.
		 */
		if (p->code == BPF_ALU|BPF_DIV|BPF_K && p->k == 0)
			return;
	}
	return BPF_CLASS(f[len - 1].code) == BPF_RET;
}
#endif
