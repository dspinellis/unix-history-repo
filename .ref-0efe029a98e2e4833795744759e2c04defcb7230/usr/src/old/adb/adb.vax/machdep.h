/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 *	@(#)machdep.h	5.2 (Berkeley) %G%
 */

/*
 * hword_t is a 2-byte (`halfword') type, used for (eg) w, l, x commands;
 * addr_t is address type, must be unsigned; registers pc, fp, sp
 *	(where those exist) are assumed to be of this type, and
 *	addresses in the debuggee are of this type;
 * expr_t is expression result type, size must be >= size of addr_t and
 *	reg_t; must be unsigned; it is treated as the fullword type
 *	and should therefore be 4 bytes long;
 * sexpr_t is a signed version of expr_t.
 *
 * SHOULD WORK ON ALLOWING (eg) 1 AND 2 BYTE, OR 4 AND 8 BYTE, ETC, WORDS
 */
typedef	u_int	addr_t;
typedef	u_int	expr_t;
typedef	int	sexpr_t;
typedef	u_short	hword_t;

/*
 * Since values of type addr_t, hword_t, and expr_t must be printed,
 * and the varargs mechanism assumes that the programmer has accounted
 * for any extension from `small' types (char, short) to `regular' types
 * (int), we define the following macros.  Each is supposed to produce
 * a (possibly sign-extended) expr_t value:
 *
 *	SH_ARG	a signed halfword (%d, %q formats)
 *	UH_ARG	an unsigned halfword (o, u, x)
 *	SF_ARG	a signed fullword (D, Q) 
 *	UF_ARG	an unsigned fullword (O, U, X)
 */
#define SH_ARG	(expr_t)(short)va_arg(ap, int)
#define	UH_ARG	(expr_t)(unsigned short)va_arg(ap, int)
#define	SF_ARG	(expr_t)va_arg(ap, int)
#define	UF_ARG	(expr_t)va_arg(ap, int)

/*
 * bpt_t is used to hold original instructions when their breakpoint
 * replacement(s) is/are set.
 */
typedef	char	bpt_t;

/*
 * ADDRESS_WRAP is a predicate that returns true if the two addr_t
 * arguments are in different spaces.
 */
#define	ADDRESS_WRAP(a, b) (((a) ^ (b)) >> 30)

/*
 * Struct activation is used for tracing through stack frames.
 * It must hold any information needed to locate an activation record
 * (variables and parameters) for a function, and must have two fields
 * of type addr_t called `a_pc' and `a_fp', the `program counter' and
 * the `frame pointer'.  a_pc is used by the expression evaluator to
 * find symbols; a_fp is returned as the result from an expression of
 * the form `name.' (a routine name, but no local symbol).
 * The field a_valid is cleared by a_prev() when there are no more
 * activation records on the stack.
 */
struct activation {
	int	a_valid;		/* set iff frame is valid */
	addr_t	a_ap;			/* ap */
	addr_t	a_fp;			/* fp */
	addr_t	a_pc;			/* pc */
};

/*
 * The reglist structure holds information needed to set and examine
 * registers.  It must contain an r_name field; this name must be unique
 * across the register set, cannot be a single letter or digit, and
 * cannot be a substring of any other register name.
 *
 * On the VAX, we keep an offset into the u. area, either from the
 * base of the u. area (in the pcb), or, for those registers that
 * are saved by syscalls, in the save area pointed to by u.u_ar0.
 * Offsets into the latter region are negative.
 *
 * We also keep a pointer into the current pcb for use when debugging
 * the kernel.
 */
struct reglist {
	char	*r_name;	/* name */
	int	r_offset;	/* offset into pcb, or from u.u_ar0 */
	int	*r_pcbaddr;	/* if kcore, address in current pcb */
};

/*
 * ispace_reg() is true iff register r points into I-space (usually just PC).
 */
#ifdef lint
#define	ispace_reg(r)	((r) == NULL)
#else
#define	ispace_reg(r)	0	/* ispace==dspace on VAX */
#endif

/*
 * getpc() returns as an addr_t the current PC; setpc() sets PC to its
 * addr_t argument.  entrypc() returns the addr_t value of the appropriate
 * startup PC.
 */
addr_t	getpc();
#define	entrypc()	((addr_t)2)

/*
 * INSTACK is true when its argument is a stack address.  It is
 * only used for consistency checking and may be overly permissive.
 * INKERNEL is true iff its argument is a kernel space address.
 */
#define	INSTACK(a)	(((a) & 0xc0000000) == 0x40000000) /* p1 space */
#define	INKERNEL(a)	(((a) & 0xc0000000) == 0x80000000) /* sys space */
