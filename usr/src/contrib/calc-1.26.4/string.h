/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */


typedef struct {
	char *h_list;	/* list of strings separated by nulls */
	long h_used;	/* characters used so far */
	long h_avail;	/* characters available for use */
	long h_count;	/* number of strings */
} STRINGHEAD;


extern char *addstr(), *namestr(), *charstr(), *addliteral();
extern long findstr(), stringindex();

extern void initstr();

/* END CODE */
