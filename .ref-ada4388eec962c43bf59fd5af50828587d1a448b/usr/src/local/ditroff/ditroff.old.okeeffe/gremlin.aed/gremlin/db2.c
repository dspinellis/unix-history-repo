/* @(#)db2.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains routines for implementing the database
 * manipulations for the gremlin picture editor.
 */

#include "gremlin.h"
#include "grem2.h"

/* imports from undodb.c  */
 
extern UNRembAdd(), UNRembMod();

/* imports from db1.c */

extern ELT *DBCreateElt();

/* imports from point.c */

extern POINT *PTMakePoint(), *PTInit();

/* imports from c */

extern char *malloc();
extern char *strcpy();

ELT *DBCopy(element,transform,db)
ELT *element, *(*db);
float transform[3][2];
/*
 *      This routine creates a copy of the the element transformed by
 * the transformation matrix and adds the new copy to the database.
 */

{
	POINT *pt, *newlist;
	char *newtext;

	newlist = PTInit();
	pt = element->ptlist;
	while ( !Nullpoint(pt) )
	{                  /* matrix multiply */
		(void) PTMakePoint((  ( (pt->x) * transform[0][0])
		                    + ( (pt->y) * transform[1][0])
		                    + transform[2][0]),
		                   (  ( (pt->x) * transform[0][1])
		                    + ( (pt->y) * transform[1][1])
		                    + transform[2][1]), &newlist);
		pt = pt->nextpt;
	}  /* end while */;
	newtext = malloc((unsigned) strlen(element->textpt) + 1);
	(void) strcpy(newtext, element->textpt);
	return( DBCreateElt(element->type, newlist, element->brushf,
	                    element->size, newtext, db) );
}  /* end copy */


DBXform(element, transform, db)
ELT *element;
float transform[3][2];
ELT *(*db);
/*
 *      This routine transforms the element by multiplying the
 * coordinates of each of the points in the element by the 
 * transformation matrix.
 */

{
	POINT *pt;
	float px, py;

	UNRembMod(element, db);
	pt = element->ptlist;
	while ( !Nullpoint(pt) )
	{
		px =  ( (pt->x) * transform[0][0] )
		    + ( (pt->y) * transform[1][0] )
		    + transform[2][0];
		py =  ( (pt->x) * transform[0][1] )
		    + ( (pt->y) * transform[1][1] )
		    + transform[2][1];
		pt->x = px;
		pt->y = py;
		pt = pt->nextpt;
	}  /* end while */;
}  /* end Xform */


DBChangeBrush(element, brush, db)
ELT *element, *(*db);
int brush;
/*
 *      This routine changes the brush attribute of the element
 */

{
	UNRembMod(element, db);
	element->brushf = brush;
}  /* end ChangeBrush */

DBChangeFont(element, font, size, db)
ELT *element, *(*db);
int font, size;
/*
 *      This routine changes the font and size attributes of the  given
 * element.
 */

{
	UNRembMod(element, db);
	element->brushf = font;
	element->size = size;
}  /* end ChangeFont */
