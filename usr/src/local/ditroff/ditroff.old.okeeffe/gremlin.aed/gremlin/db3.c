/* @(#)db3.c	1.2	%G%
 *
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *       This file contains additional routines to implement the database
 * manipulations necessary for the gremlin picture editor.
 */

#include "gremlin.h"
#include "grem2.h"

/* the following variable is a pointer for the current set and is
 * available to the outside world.
 */

ELT *cset;

DBAddSet(element)
ELT *element;
/*
 *      This routine adds the element to the current set database.
 */

{
	ELT *elist;

	elist = cset;
	while ( !DBNullelt(elist) )             /* makes sure element not */
	{                                       /* already in list        */
		if (elist == element) return;
		elist = DBNextofSet(elist);
	};
	element->setnext = cset;
	cset = element;
}  /* end AddSet */

DBClearSet()
/*
 *      This routine clears the current set by setting the pointer
 * to a null element.
 */

{
	while ( !DBNullelt(cset) )
		cset = DBNextofSet(cset);
}  /* end ClearSet */
