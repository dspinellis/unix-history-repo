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
    "@(#) $Header: bpf_filter.c,v 1.9 91/01/30 18:21:54 mccanne Exp $ (LBL)";
#endif

#include <sys/param.h>
#include <sys/types.h>
#include <protosw.h>
#include <netinet/in.h>
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

/*
 * Execute the filter program pointed to by 'pc' on the 
 * packet pointed to by 'p'.  'wirelen' is the length of actual
 * packet received by the interface.  'buflen' is the amount of
 * contiguous data.  The return value is the return value of the
 * filter program, or 0 on an error.
 */
u_int
bpf_filter(pc, p, wirelen, buflen)
	register struct bpf_insn *pc;
	register u_char *p;
	u_int wirelen;
	u_int buflen;
{
#define	JUMP(delta)	pc += (delta)
#define	BR(cond)	JUMP((cond) ? pc->jt : pc->jf)

	register long A, X;
	long mem[BPF_MEMWORDS];

	if (pc == 0)
		/*
		 * No filter means accept all.
		 */
		return 1;

#ifdef lint
	A = 0;
	X = 0;
#endif

	while (1) {

		switch (pc->code) {

		default:
#ifdef KERNEL
			return 0;
#else
			abort();
#endif			
		case RetOp:
			return (u_int)pc->k;

		case RetAOp:
			return (u_int)A;

		case LdOp:
			if (pc->k + sizeof(long) > buflen)
				return 0;
			A = EXTRACT_LONG(&p[pc->k]);
			break;

		case LdHOp:
			if (pc->k + sizeof(short) > buflen)
				return 0;
			A = EXTRACT_SHORT(&p[pc->k]);
			break;

		case LdBOp:
			if (pc->k >= buflen)
				return 0;
			A = p[pc->k];
			break;

		case LdLenOp:
			A = wirelen;
			break;

		case ILdOp:
			if (X + pc->k + sizeof(long) > buflen)
				return 0;
			A = EXTRACT_LONG(&p[X + pc->k]);
			break;

		case ILdHOp:
			if (X + pc->k + sizeof(short) > buflen)
				return 0;
			A = EXTRACT_SHORT(&p[X + pc->k]);
			break;

		case ILdBOp:
			if (X + pc->k >= buflen)
				return 0;
			A = p[X + pc->k];
			break;

		case LdIOp:
			A = pc->k;
			break;

		case LdXIOp:
			X = pc->k;
			break;

		case LdxmsOp:
			if (pc->k >= buflen)
				return 0;
			X = (p[pc->k] & 0xf) << 2;
			break;

		case TaxOp:
			X = A;
			break;

		case TxaOp:
			A = X;
			break;

		case StmOp:
			mem[pc->k] = A;
			break;

		case LdmOp:
			A = mem[pc->k];
			break;
			
		case StmXOp:
			mem[pc->k] = X;
			break;

		case LdmXOp:
			X = mem[pc->k];
			break;

		case NopOp:
			break;

		case GTOp:
			BR(A > pc->k);
			continue;

		case GEOp:
			BR(A >= pc->k);
			continue;

		case EQOp:
			BR(A == pc->k);
			continue;

		case AddXOp:
			A += X;
			break;
			
		case SubXOp:
			A -= X;
			break;
			
		case MulXOp:
			A *= X;
			break;
			
		case DivXOp:
			if (X == 0)
				return 0;
			A /= X;
			break;
			
		case AndXOp:
			A &= X;
			break;
			
		case OrXOp:
			A |= X;
			break;

		case LshXOp:
			A <<= X;
			break;

		case RshXOp:
			A >>= X;
			break;

		case AddIOp:
			A += pc->k;
			break;
			
		case SubIOp:
			A -= pc->k;
			break;
			
		case MulIOp:
			A *= pc->k;
			break;
			
		case DivIOp:
			if (pc->k == 0)
				return 0;
			A /= pc->k;
			break;
			
		case AndIOp:
			A &= pc->k;
			break;
			
		case OrIOp:
			A |= pc->k;
			break;

		case LshIOp:
			A <<= pc->k;
			break;

		case RshIOp:
			A >>= pc->k;
			break;

		case NegOp:
			A = -A;
			break;
		}
		++pc;
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
bpf_validate(fcode, len)
	struct bpf_insn *fcode;
	int len;
{
	struct bpf_insn *p;
	int i;

	p = fcode;
	for (i = 0; i < len; ++p, ++i)
		if (!BPF_VALIDCODE(p->code))
			return 0;
	p = fcode;
	for (i = 0; i < len; ++p, ++i) {
		/*
		 * Check that that jumps are forward, and within 
		 * the code block.
		 */
		if (BPF_ISJUMP(p->code) &&
		    (p->jt <= 0 || i + p->jt >= len ||
		     p->jf <= 0 || i + p->jf >= len))
			return 0;
		/*
		 * Check that memory operations use valid addresses.
		 */
		switch (p->code) {
		case StmOp:
		case StmXOp:
		case LdmOp:
		case LdmXOp:
			if (p->k >= BPF_MEMWORDS || p->k < 0)
				return 0;
		}
	}
	return BPF_ISLEAF(fcode[len - 1].code);
}
#endif
