/*
 * @(#)undodb.c	1.1	%G%
 *
 * Routines for recording (remembering) database activity to provide
 * an undo capability for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include "gremlin.h"

/* The following are used to point to the undo database lists */

UNELT *unlist;
UNELT *unback;

/* imports from point.c */

POINT *PTMakePoint();

/* imports from C */

extern char *malloc();
extern char *strcpy();

/* imports from db.c */

extern DBClearElt();


/*
 * This routine records an addition to the database by saving
 * a pointer to the new element.
 */
UNRembAdd(element, db)
ELT *element, *(*db);
{
    register UNELT *temp;

    temp = (UNELT *) malloc(sizeof(UNELT));
    temp->action = ADD;
    temp->dbase  = db;
    temp->oldelt = NULL;
    temp->newelt = element;
    temp->nextun = unlist;
    unlist = temp;
}  /* end UNRembAdd */


/*
 * This routine records a deletion from the database by saving
 * a pointer to the deleted element.
 */
UNRembDelete(element, db)
ELT *element, *(*db);
{
    register UNELT *temp;

    temp = (UNELT *) malloc(sizeof(UNELT));
    temp->action = DELETE;
    temp->dbase  = db;
    temp->oldelt = element;
    temp->newelt = NULL;
    temp->nextun = unlist;
    unlist = temp;
}  /* end UNRembDelete */


/*
 * This routine records a modification to the database.  The
 * element passed to it is the element which will be modified and it
 * is therefore copied (copying the text and pointlist also)
 * and save as oldelt.  A pointer to the element which will be
 * modified is saved in newelt.
 */
UNRembMod(element, db)
ELT *element, *(*db);
{
    register UNELT *temp;
    register ELT *hold;
    register POINT *pt;

    temp = (UNELT *) malloc(sizeof(UNELT));
    temp->action = MOD;
    temp->dbase = db;
    temp->newelt = element;
    temp->oldelt = hold = (ELT *) malloc(sizeof(ELT));
    hold->type = element->type;
    hold->brushf = element->brushf;
    hold->size = element->size;
    hold->textpt = malloc((unsigned) strlen(element->textpt) + 1);
    (void) strcpy(hold->textpt, element->textpt);
    pt = element->ptlist;
    hold->ptlist = PTInit();

    while (!Nullpoint(pt)) {
        (void) PTMakePoint(pt->x, pt->y, &(hold->ptlist));
        pt = PTNextPoint(pt);
    }

    temp->nextun = unlist;
    unlist = temp;
}  /* end UNRembMod */


/*
 * This routine clears the undo database.  The database is copied
 * into the backup database and elements from the backup database
 * are deleted and returned to free storage.  If nothing is in the undo
 * database, the backup database is preserved.
 */
UNForget()
{
    register UNELT *temp;

    if (unlist == NULL) 
	return;

    while (unback != NULL) {
        temp = unback->nextun;
        switch (unback->action) {
	    case ADD: 
		free((char *) unback);
                break;
	    case MOD:
	    case DELETE: 
		DBClearElt(unback->oldelt);
                free((char *) unback);
                break;
        }
        unback = temp;
    }

    unback = unlist;
    unlist = NULL;
}  /* end UNForget */
