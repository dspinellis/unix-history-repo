/* @(#)display.c	1.3	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains routines to implement the higher level display
 * driver routines
 */

#include "gremlin.h"
#include "grem2.h"

/* imports from graphics1.c */

extern GRsetwmask(); 

/* imports from graphics3.c */

extern GRVector(), GRPutText(); 
extern int GRArc();
extern GRClear();


DISScreenAdd(element,layer)
ELT *element;
int layer;
/*
 *      This routine displays an arbitrary element type on the specified 
 * memory plane using the parameters stored with the element
 */

{
    POINT *p1, *p2, pos;

    if ( !DBNullelt(element) )
    {
        GRsetwmask(layer);
        if (TEXT(element->type))
        {
             p1 = element->ptlist;
             (void) GRPutText(element->type, p1, element->brushf,
                              element->size, element->textpt, &pos);
        }
        else
        {
            switch (element->type)
            {
                 case ARC:  p1 = element->ptlist;
                            p2 = PTNextPoint(p1);
		               /* angle is stored in size */
                            (void) GRArc(p1, p2, (float) element->size, 
                                         element->brushf);
                            break;

               case CURVE:  (void) GRCurve(element->ptlist, element->brushf);
                            break;

             case POLYGON:
              case VECTOR:  p1 = element->ptlist;
                            p2 = PTNextPoint(p1);
                            while ( !Nullpoint(p2) )
                            {
                                GRVector(p1, p2, element->brushf);
                                p1 = p2;
                                p2 = PTNextPoint(p2);
                            }  /* end while */;
                            break;
            }  /* end switch */;
        }  /* end else TEXT */
    }  /* end if */
}  /* end ScreenAdd */



DISScreenErase(element,layer)
ELT *element;
int layer;
/*
 *      This routine erases an arbitrary element type from the specified 
 * memory plane by redrawing the element in the background color.  It
 * uses the parameters stored with the element.
 */

{
    POINT *p1, *p2, pos;

    if ( !DBNullelt(element) )
    {
        GRsetwmask(layer);
        if (TEXT(element->type))
        {
             p1 = element->ptlist;
             (void) GRPutText(element->type, p1, eraseany,
                              element->size, element->textpt, &pos);
        }
        else
        {
            switch (element->type)
            {
                case ARC:  p1 = element->ptlist;
                           p2 = PTNextPoint(p1);
		               /* angle is stored in size */
                           (void) GRArc(p1, p2, (float) element->size, 
                                        eraseany);
                           break;
    
              case CURVE:  (void) GRCurve(element->ptlist, eraseany);
                           break;

            case POLYGON:
             case VECTOR:  p1 = element->ptlist;
                           p2 = PTNextPoint(p1);
                           while ( !Nullpoint(p2) )
                           {
                                GRVector(p1, p2, eraseany);
                                p1 = p2;
                                p2 = PTNextPoint(p2);
                            }  /* end while */;
                            break;
            }  /* end switch */;
        }  /* end else TEXT */
    }  /* end if */
}  /* end ScreenErase */


DISDisplaySet(element)
ELT *element;
/*
 *      This routine displays the specified element as the part of
 * the current set by calling screenadd with the current set layers
 * specified
 */

{
    DISScreenAdd(element, setmask);
}  /* end DisplaySet */

DISEraseSet(element)
ELT *element;
/*
 *      This routine erases the set attribute of the specifed element by
 * calling Screen Erase with the layer mask set for the current set layer(s)
 */

{
    DISScreenErase(element, setmask);
}  /* end EraseSet */

DISClearSetDisplay()
/*
 *      This routine clears the set attribute from all elements by erasing
 * the  the entire set display layer.
 */

{
    GRClear(setmask);
}
