/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Constant number storage module.
 */

#include "calc.h"

#define CONSTALLOCSIZE 400	/* number of constants to allocate */


static long constcount;		/* number of constants defined */
static long constavail;		/* number of constants available */
static NUMBER **consttable;	/* table of constants */


/*
 * Read in a constant number and add it to the table of constant numbers,
 * creating a new entry if necessary.  The incoming number is a string
 * value which must have a correct format, otherwise an undefined number
 * will result.  Returns the index of the number in the constant table.
 * Returns zero if the number could not be saved.
 */
long
addnumber(str)
	char *str;		/* string representation of number */
{
	NUMBER *q;

	q = atoq(str);
	if (q == NULL)
		return 0;
	return addqconstant(q);
}


/*
 * Add a particular number to the constant table.
 * Returns the index of the number in the constant table, or zero
 * if the number could not be saved.  The incoming number if freed
 * if it is already in the table.
 */
long
addqconstant(q)
	register NUMBER *q;	/* number to be added */
{
	register NUMBER **tp;	/* pointer to current number */
	register NUMBER *t;	/* number being tested */
	long index;		/* index into constant table */
	long numlen;		/* numerator length */
	long denlen;		/* denominator length */
	HALF numlow;		/* bottom value of numerator */
	HALF denlow;		/* bottom value of denominator */

	numlen = q->num.len;
	denlen = q->den.len;
	numlow = q->num.v[0];
	denlow = q->den.v[0];
	tp = &consttable[1];
	for (index = 1; index <= constcount; index++) {
		t = *tp++;
		if ((numlen != t->num.len) || (numlow != t->num.v[0]))
			continue;
		if ((denlen != t->den.len) || (denlow != t->den.v[0]))
			continue;
		if (q->num.sign != t->num.sign)
			continue;
		if (qcmp(q, t) == 0) {
			qfree(q);
			return index;
		}
	}
	if (constavail <= 0) {
		if (consttable == NULL) {
			tp = (NUMBER **)
				malloc(sizeof(NUMBER *) * (CONSTALLOCSIZE + 1));
			*tp = NULL;
		} else
			tp = (NUMBER **) realloc((char *) consttable,
			sizeof(NUMBER *) * (constcount+CONSTALLOCSIZE + 1));
		if (tp == NULL)
			return 0;
		consttable = tp;
		constavail = CONSTALLOCSIZE;
	}
	constavail--;
	constcount++;
	consttable[constcount] = q;
	return constcount;
}


/*
 * Return the value of a constant number given its index.
 * Returns address of the number, or NULL if the index is illegal.
 */
NUMBER *
constvalue(index)
	long index;
{
	if ((index <= 0) || (index > constcount))
		return NULL;
	return consttable[index];
}

/* END CODE */
