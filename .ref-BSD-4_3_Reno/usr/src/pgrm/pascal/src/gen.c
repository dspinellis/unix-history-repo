/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)gen.c	5.1 (Berkeley) 6/5/85";
#endif not lint

#include "whoami.h"
#ifdef OBJ
    /*
     *	and the rest of the file
     */
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "objfmt.h"

/*
 * This array tells the type
 * returned by an arithmetic
 * operation.  It is indexed
 * by the logarithm of the
 * lengths base 2.
 */
#ifndef	DEBUG
char	arret[]	= {
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	TDOUBLE,	TDOUBLE,	TDOUBLE,	TDOUBLE
};
#else
char	arret0[] = {
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	TDOUBLE,	TDOUBLE,	TDOUBLE,	TDOUBLE
};
char	arret1[] = {
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	T4INT,		T4INT,		T4INT,		TDOUBLE,
	TDOUBLE,	TDOUBLE,	TDOUBLE,	TDOUBLE
};
char	*arret = arret0;
#endif

/*
 * These array of arithmetic and set
 * operators are indexed by the
 * tree nodes and is highly dependent
 * on their order.  They thus take
 * on the flavor of magic.
 */
int	arop[] = {
	0, O_NEG2, O_MOD2, O_DIV2, O_DVD2, O_MUL2, O_ADD2, O_SUB2,
	O_REL2, O_REL2, O_REL2, O_REL2, O_REL2, O_REL2
};
int	setop[] = {
	O_MULT, O_ADDT, O_SUBT,
	O_RELT, O_RELT, O_RELT, O_RELT, O_RELT, O_RELT,
};

/*
 * The following array is
 * used when operating on
 * two reals since they are
 * shoved off in a corner in
 * the interpreter table.
 */
int	ar8op[] = {
	O_DVD8, O_MUL8, O_ADD8, O_SUB8,
	O_REL8, O_REL8, O_REL8, O_REL8, O_REL8, O_REL8,
};

/*
 * The following arrays, which are linearizations
 * of two dimensional arrays, are the offsets for
 * arithmetic, relational and assignment operations
 * indexed by the logarithms of the argument widths.
 */
#ifndef	DEBUG
char artab[] = {
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD42-O_ADD2,	O_ADD82-O_ADD2,
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD42-O_ADD2,	O_ADD82-O_ADD2,
	O_ADD24-O_ADD2,	O_ADD24-O_ADD2,	O_ADD4-O_ADD2,	O_ADD84-O_ADD2,
	O_ADD28-O_ADD2,	O_ADD28-O_ADD2,	O_ADD48-O_ADD2,	-1
};
#else
char artab0[] = {
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD42-O_ADD2,	O_ADD82-O_ADD2,
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD42-O_ADD2,	O_ADD82-O_ADD2,
	O_ADD24-O_ADD2,	O_ADD24-O_ADD2,	O_ADD4-O_ADD2,	O_ADD84-O_ADD2,
	O_ADD28-O_ADD2,	O_ADD28-O_ADD2,	O_ADD48-O_ADD2,	-1
};
char artab1[] = {
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD82-O_ADD2,
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD82-O_ADD2,
	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD2-O_ADD2,	O_ADD84-O_ADD2,
	O_ADD28-O_ADD2,	O_ADD28-O_ADD2,	O_ADD28-O_ADD2,	-1
};
char	*artab = artab0;
#endif
#ifndef DEBUG
char reltab[] = {
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL42-O_REL2,	O_REL82-O_REL2,
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL42-O_REL2,	O_REL82-O_REL2,
	O_REL24-O_REL2,	O_REL24-O_REL2,	O_REL4-O_REL2,	O_REL84-O_REL2,
	O_REL28-O_REL2,	O_REL28-O_REL2,	O_REL48-O_REL2,	O_REL8-O_REL2
};
#else
char reltab0[] = {
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL42-O_REL2,	O_REL82-O_REL2,
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL42-O_REL2,	O_REL82-O_REL2,
	O_REL24-O_REL2,	O_REL24-O_REL2,	O_REL4-O_REL2,	O_REL84-O_REL2,
	O_REL28-O_REL2,	O_REL28-O_REL2,	O_REL48-O_REL2,	O_REL8-O_REL2
};
char reltab1[] = {
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL82-O_REL2,
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL82-O_REL2,
	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL2-O_REL2,	O_REL82-O_REL2,
	O_REL28-O_REL2,	O_REL28-O_REL2,	O_REL28-O_REL2,	O_REL8-O_REL2
};
char *reltab = reltab0;
#endif

#ifndef DEBUG
char asgntab[] = {
	O_AS21-O_AS2,	O_AS21-O_AS2,	O_AS41-O_AS2,	-1,
	O_AS2-O_AS2,	O_AS2-O_AS2,	O_AS42-O_AS2,	-1,
	O_AS24-O_AS2,	O_AS24-O_AS2,	O_AS4-O_AS2,	-1,
	O_AS28-O_AS2,	O_AS28-O_AS2,	O_AS48-O_AS2,	O_AS8-O_AS2,
};
#else
char asgntb0[] = {
	O_AS21-O_AS2,	O_AS21-O_AS2,	O_AS41-O_AS2,	-1,
	O_AS2-O_AS2,	O_AS2-O_AS2,	O_AS42-O_AS2,	-1,
	O_AS24-O_AS2,	O_AS24-O_AS2,	O_AS4-O_AS2,	-1,
	O_AS28-O_AS2,	O_AS28-O_AS2,	O_AS48-O_AS2,	O_AS8-O_AS2,
};
char asgntb1[] = {
	O_AS21-O_AS2,	O_AS21-O_AS2,	O_AS21-O_AS2,	-1,
	O_AS2-O_AS2,	O_AS2-O_AS2,	O_AS2-O_AS2,	-1,
	O_AS2-O_AS2,	O_AS2-O_AS2,	O_AS2-O_AS2,	-1,
	O_AS28-O_AS2,	O_AS28-O_AS2,	O_AS28-O_AS2,	O_AS4-O_AS2,
};
char *asgntab = asgntb0;
#endif

#ifdef DEBUG
genmx()
{

	arret = arret1;
	artab = artab1;
	reltab = reltab1;
	asgntab = asgntb1;
}
#endif

/*
 * Gen generates code for assignments,
 * and arithmetic and string operations
 * and comparisons.
 */
struct nl *
gen(p, o, w1, w2)
	int p, o, w1, w2;
{
	register i, j;
	int op;

	switch (p) {
		default:
			panic("gen");
		case O_AS2:
		case NIL:
			i = j = -1;
			/*
			 * Take the log2 of the widths
			 * and linearize them for indexing.
			 * width for indexing.
			 */
#ifdef DEBUG
			if (hp21mx) {
				if (w1 == 4)
					w1 = 8;
				if (w2 == 4)
					w2 = 8;
			}
#endif
			do i++; while (w1 >>= 1);
			do j++; while (w2 >>= 1);
			i <<= 2;
			i |= j;
			if (p == O_AS2) {
				(void) put(1, O_AS2 + asgntab[i]);
				return (NIL);
			}
			op = arop[o];
			if (op == O_REL2) {
				(void) put(1, (op + reltab[i]) | (o - T_EQ) << 8+INDX);
				return (nl+TBOOL);
			}
			(void) put(1, i == 15 ? ar8op[o-T_DIVD] : op | artab[i]);
			return (op == O_DVD2 && !divchk ? nl+TDOUBLE : nl+arret[i]);
		case TREC:
		case TSTR:
			(void) put(2, O_RELG | (o - T_EQ) << 8+INDX, w1);
			return (nl+TBOOL);
		case TSET:
			op = setop[o-T_MULT];
			if (op == O_RELT)
				op |= (o - T_EQ)<<8+INDX;
			(void) put(2, op, w1);
			return (o >= T_EQ ? nl+TBOOL : nl+TSET);
	}
}
#endif OBJ
