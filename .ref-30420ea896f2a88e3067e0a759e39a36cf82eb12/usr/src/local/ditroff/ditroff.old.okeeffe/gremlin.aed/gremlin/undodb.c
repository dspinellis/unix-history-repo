/* @(#)undodb.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 *      This file contains routines for recording (remembering) database
 * activity to provide an undo capability for the gremlin picture editor.
 */

#include "gremlin.h"
#include "grem2.h"

/* The following are used to point to the undo database lists */

UNELT *unlist, *unback;

/* imports from point.c */

POINT *PTInit(), *PTMakePoint();

/* imports from c */

extern char *malloc();
extern char *strcpy();

/* imports from db1.c */

extern DBClearElt();


UNRembAdd(element, db)
ELT *element, *(*db);
/*
 *      This routine records an addition to the database by saving
 * a pointer to the new element
 */

{
    UNELT *temp;

    temp = (UNELT *) malloc(sizeof(UNELT));
    temp->action = ADD;
    temp->dbase  = db;
    temp->oldelt = NULL;
    temp->newelt = element;
    temp->nextun = unlist;
    unlist = temp;
}  /* end RembAdd */

UNRembDelete(element, db)
ELT *element, *(*db);
/*
 *      This routine records a deletion from the database by saving
 * a pointer to the deleted element
 */

{
    UNELT *temp;

    temp = (UNELT *) malloc(sizeof(UNELT));
    temp->action = DELETE;
    temp->dbase  = db;
    temp->oldelt = element;
    temp->newelt = NULL;
    temp->nextun = unlist;
    unlist = temp;
}  /* end RembDelete */


UNRembMod(element, db)
ELT *element, *(*db);
/*
 *      This routine records a modification to the database.  The
 * element passed to it is the element which will be modified and it
 * is therefore copied (copying the text and pointlist also)
 * and save as oldelt.  A pointer of the element (which will be
 * modified) is saved in newelt.
 */

{
    POINT *pt;
    UNELT *temp;
    ELT *hold;

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
    while ( !Nullpoint(pt) )
    {
        (void) PTMakePoint(pt->x, pt->y, &(hold->ptlist));
        pt = PTNextPoint(pt);
    }  /* end while */
    temp->nextun = unlist;
    unlist = temp;
}  /* end RembMod */

UNForget()
/*
 *      This routine clears the undo database.  The database is copied
 * into the backup database and elements from the backup database
 * are deleted and returned to free storage.  If nothing is in the undo
 * database, the backup database is preserved.
 */

{
    UNELT *temp;

    if (unlist == nullun) return;
    while (unback != nullun)
    {
        temp = unback->nextun;
        switch (unback->action)
        {
               case ADD: free((char *) unback);
                         break;

               case MOD:
            case DELETE: DBClearElt(unback->oldelt);
                         free((char *) unback);
                         break;
        }  /* end switch */;
        unback = temp;
    }  /* end while */;
    unback = unlist;
    unlist = nullun;
}  /* end Forget */
