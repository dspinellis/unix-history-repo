/* $Header: object.c,v 7.0 86/10/08 15:12:55 lwall Exp $ */

/* $Log:	object.c,v $
 * Revision 7.0  86/10/08  15:12:55  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "INTERN.h"
#include "object.h"

void
object_init()
{
    ;
}

OBJECT *
make_object(typ, img, py, px, vy, vx, energ, mas, where)
char typ;
char img;
int px, py, vx, vy;
long energ, mas;
OBJECT *where;
{
    Reg1 OBJECT *obj;

    if (free_root.next == &free_root)
#ifndef lint
	obj = (OBJECT *) malloc(sizeof root);
#else
	obj = Null(OBJECT*);
#endif
    else {
	obj = free_root.next;
	free_root.next = obj->next;
	obj->next->prev = &free_root;
    }
    obj->type = typ;
    obj->image = img;
    obj->next = where;
    obj->prev = where->prev;
    where->prev = obj;
    obj->prev->next = obj;
    obj->velx = vx;
    obj->vely = vy;
    obj->contend = 0;
    obj->strategy = 0;
    obj->flags = 0;
    obj->posx = px;
    obj->posy = py;
    if (typ != Torp && typ != Web) {
	occupant[py][px] = obj;
    }
    obj->energy = energ;
    obj->mass = mas;
    return(obj);
}

void
unmake_object(curobj)
Reg1 OBJECT *curobj;
{
    curobj->prev->next = curobj->next;
    curobj->next->prev = curobj->prev;
    if (curobj == movers) {
	movers = curobj->next;
    }
    free_object(curobj);
}

void
free_object(curobj)
Reg1 OBJECT *curobj;
{
    curobj->next = free_root.next;
    curobj->prev = &free_root;
    free_root.next->prev = curobj;
    free_root.next = curobj;
}
