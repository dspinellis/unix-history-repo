/* interface.c - bit manipulation utility routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/interface.c,v 7.1 91/02/22 09:29:21 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/interface.c,v 7.1 91/02/22 09:29:21 mrose Interim $
 *
 *
 * $Log:	interface.c,v $
 * Revision 7.1  91/02/22  09:29:21  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:01:44  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */



#include <stdio.h>
#include "quipu/photo.h"

/* This file contains utility routines used by both the                 */
/* encoding and decoding programs.                                      */
/* The routines are concerned with getting and setting bits of          */
/* a bit string.                                                        */

/* All these routine work in basically the same way.
/* a mask is used to get at each individual bit within
/* a byte.  Each time the next bit is required, the
/* mask is shifted right, when the mask is zero, the byte is either
/* written to the file, or the next byte read in depending upon the
/* routine.
*/

int PIC_LINESIZE,STOP,NUMLINES;

/* ROUTINE:     Get_bit                                                 */
/*                                                                      */
/* SYNOPSIS:    Gets the next bit from the input.                       */
/*              Returns 0 if it is a zero.                              */
/*              Returns 1 if it is a one                                */
char
get_bit (lineptr)

bit_string * lineptr;      /* the line to get the bit from */

{
unsigned char    result;

   /* Anding the mask and the data gives a 0 if the bit masked is 0, 1 otherwis
e */
   result = lineptr->mask & lineptr->pos;

   lineptr->mask  >>= 1;

   if (lineptr->mask == 0) {
      lineptr->pos = *lineptr->dbuf++;
      lineptr->mask = BIT_MASK;
      }

   if( result != 0 )    /* may not be 1, may be 0001000 for example */
	result = 1;

   return ( (char) result );
}



/* ROUTINE:   Set_bit                                                   */
/*                                                                      */
/* SYNOPSIS:  Sets the next bit of the bit string pointed to by         */
/*            lineptr to a one.                                         */

set_bit (lineptr)

bit_string *  lineptr;

{
   /* This sets the masked bit */
   lineptr->pos |= lineptr->mask;

   lineptr->mask  >>= 1;

   if (lineptr->mask == 0) {
      *lineptr->dbuf++ = lineptr->pos;
      lineptr->mask = BIT_MASK;
      }

}




/* ROUTINE:   Clr_bit                                                   */
/*                                                                      */
/* SYNOPSIS:  clears the next bit of the bit string pointed to by       */
/*            lineptr.  i.e set it to zero.                             */

clr_bit (lineptr)

bit_string *  lineptr;

{
    /* clear the masked bit */
   lineptr->pos &=   ~(lineptr->mask) ;

   lineptr->mask  >>= 1;         /* right shift the mask */

   if (lineptr->mask == 0) {     /* may need to move on to the next byte */
      *lineptr->dbuf++ = lineptr->pos;
      lineptr->mask = BIT_MASK;
      }

}




