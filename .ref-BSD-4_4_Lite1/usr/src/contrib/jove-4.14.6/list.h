/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* generic singly linked list package */

typedef struct cons	List;

struct cons {
	UnivPtr	car;	/* pointer to element */
	List	*cdr;
};

#define list_next(lp)	((lp)->cdr)
#define list_data(lp)	((lp)->car)

extern UnivPtr
	list_push proto((List **, UnivPtr)),
	list_pop proto((List **));
