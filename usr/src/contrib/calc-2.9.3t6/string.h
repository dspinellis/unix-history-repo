/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */

#ifndef	CALCSTRING_H
#define	CALCSTRING_H

#include "zmath.h"


typedef struct {
	char *h_list;	/* list of strings separated by nulls */
	long h_used;	/* characters used so far */
	long h_avail;	/* characters available for use */
	long h_count;	/* number of strings */
} STRINGHEAD;


extern void initstr MATH_PROTO((STRINGHEAD *hp));
extern char *addstr MATH_PROTO((STRINGHEAD *hp, char *str));
extern char *namestr MATH_PROTO((STRINGHEAD *hp, long n));
extern long findstr MATH_PROTO((STRINGHEAD *hp, char *str));
extern char *charstr MATH_PROTO((int ch));
extern char *addliteral MATH_PROTO((char *str));
extern long stringindex MATH_PROTO((char *str1, char *str2));
extern HASH hashstr MATH_PROTO((char *cp));

#endif

/* END CODE */
