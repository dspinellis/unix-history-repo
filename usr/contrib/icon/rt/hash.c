#include "../h/rt.h"

/*
 * hash - compute hash value of arbitrary object for table and set accessing.
 */

hash(d)
struct descrip *d;
   {
   register int i, j;
   register char *s;

   if (QUAL(*d)) {
      /*
       * Compute the hash value for the string by summing the value
       *  of all the characters (up to a maximum of 10) plus the length.
       */
      i = 0;
      s = STRLOC(*d);
      j = STRLEN(*d);
      for (j = (j <= 10) ? j : 10 ; j > 0; j--)
         i += *s++ & 0377;
      i += STRLEN(*d) & 0377;
      }
   else {
      switch (TYPE(*d)) {
         /*
          * The hash value for numeric types is the bitstring representation
          *  of the value.
          */
         case T_INTEGER:
            i = INTVAL(*d);
            break;

#ifdef LONGS
         case T_LONGINT:
            i = BLKLOC(*d)->longint.intval;
            break;

#endif LONGS
         case T_REAL:
            i = BLKLOC(*d)->realblk.realval;
            break;

         case T_CSET:
            /*
             * Compute the hash value for a cset by exclusive or-ing the
             *  words in the bit array.
             */
            i = 0;
            for (j = 0; j < CSETSIZE; j++)
               i ^= BLKLOC(*d)->cset.bits[j];
            break;

         default:
            /*
             * For other types, just use the type value as the hash
             *  value.  This produces loads of collisions, but that's
             *  why collision insurance is mandatory.
             */
            i = TYPE(*d);
            break;
         }
      }
   /*
    * Return the hash value as a positive number.
    */
   return ((unsigned)i);
   }
