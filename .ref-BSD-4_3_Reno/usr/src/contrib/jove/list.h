/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* generic singly linked list package */

typedef char	*Element;

typedef struct cons	List;

struct cons {
	Element	*car;
	List	*cdr;
};

#define list_next(lp)	((lp)->cdr)
#define list_data(lp)	((lp)->car)

extern Element
	*list_push proto((List **, Element *)),
	*list_pop proto((List **));
