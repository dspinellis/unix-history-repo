/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */

#ifndef	LABEL_H
#define	LABEL_H


#include "zmath.h"


#define	NULL_LABEL	((LABEL *) 0)


/*
 * Label structures.
 */
typedef struct {
	long l_offset;		  /* offset into code of label */
	long l_chain;		  /* offset into code of undefined chain */
	char *l_name;		  /* name of label if any */
} LABEL;


extern void initlabels MATH_PROTO((void));
extern void definelabel MATH_PROTO((char *name));
extern void addlabel MATH_PROTO((char *name));
extern void clearlabel MATH_PROTO((LABEL *lp));
extern void setlabel MATH_PROTO((LABEL *lp));
extern void uselabel MATH_PROTO((LABEL *lp));
extern void checklabels MATH_PROTO((void));

#endif

/* END CODE */
