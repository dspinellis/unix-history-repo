#include "../h/rt.h"

/*
 * map(s1,s2,s3) - map s1, using s2 and s3.
 */
Xmap(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   register int i;
   register char *s1, *s2, *s3;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING], sbuf3[MAXSTRING];
   static char maptab[MAXSTRING];
   extern char *alcstr();

   /*
    * s1 must be a string; s2 and s3 default to &ucase and &lcase,
    *  respectively.
    */
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   defany(&arg2, &ucase);
   defany(&arg3, &lcase);

   /*
    * If s2 and s3 are the same as for the last call of map,
    *  the some old values, namely maps2, maps3, and maptab
    *  can be reused.  Otherwise, the information must be
    *  recomputed.
    */
   if (maps2.type != arg2.type || maps3.type != arg3.type ||
       BLKLOC(maps2) != BLKLOC(arg2) || BLKLOC(maps3) != BLKLOC(arg3)) {
      maps2 = arg2;
      maps3 = arg3;
      /*
       * s2 and s3 must be strings and of the same length.
       */
      if (cvstr(&arg2, sbuf2) == NULL)
         runerr(103, &arg2);
      if (cvstr(&arg3, sbuf3) == NULL)
         runerr(103, &arg3);
      if (STRLEN(arg2) != STRLEN(arg3))
         runerr(208, NULL);
      /*
       * The array maptab is used to perform the mapping.  First, maptab[i]
       *  is initialized with i for i from 0 to MAXSTRING-1 (256).  Then,
       *  the for each character in s2, the position in maptab corresponding
       *  the value of the character is assigned the value of the character
       *  in s3 that is in the same ordinal position as the character from s2.
       *  For example, if s2 is "abc", and s3 is "123", the assignments are:
       *   maptab['a'] = '1'
       *   maptab['b'] = '2'
       *   maptab['c'] = '3'
       *  Note that the 0..256 (really should be 0..255!) initialization
       *   causes unmapped characters to be themselves, for example,
       *   maptab['d'] == 'd'.
       */
      s2 = STRLOC(arg2);
      s3 = STRLOC(arg3);
      for (i = MAXSTRING - 1; i >= 0; i--)
         maptab[i] = i;
      for (i = 0; i < STRLEN(arg2); i++)
         maptab[s2[i]&0377] = s3[i];
      }

   if (STRLEN(arg1) == 0) {
      arg0.type = D_NULL;
      INTVAL(arg0) = 1;
      return;
      }

   /*
    * The result is a string the size of s1, so ensure that much space.
    */
   i = STRLEN(arg1);
   sneed(i);
   s1 = STRLOC(arg1);

   /*
    * Create the result string, but specify no value for it.
    */
   STRLEN(arg0) = i;
   STRLOC(arg0) = s2 = alcstr(NULL, i);
   /*
    * Buzz through the string using values in maptab to do the mapping.
    */
   while (i-- > 0)
      *s2++ = maptab[(*s1++)&0377];
   }

Procblock(map,3)
