#include "../h/rt.h"

/*
 * center(s1,n,s2) - pad s1 on left and right with s2 to length n.
 */

Xcenter(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   register char *s, *st;
   int cnt, slen, hcnt;
   char *sbuf, *s3;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern char *alcstr();

   /*
    * s1 must be a string.  n must be a non-negative integer and defaults
    *  to 1.  s2 must be a string and defaults to a blank.
    */
   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   defshort(&arg2, 1);
   if ((cnt = INTVAL(arg2)) < 0)
      runerr(205, &arg2);
   defstr(&arg3, sbuf2, &blank);

   sneed(cnt);

   if (STRLEN(arg3) == 0) {
      /*
       * The padding string is null, make it a blank.
       */
      slen = 1;
      s3 = " ";
      }
   else {
      slen = STRLEN(arg3);
      s3 = STRLOC(arg3);
      }

   /*
    * Get n bytes of string space for the new string.  Start at the right
    *  of the new string and copy s2 into it from right to left as
    *  many times as will fit in the right half of the new string.
    */
   sbuf = alcstr(NULL, cnt);
   hcnt = cnt / 2;
   s = sbuf + cnt;
   while (s > sbuf + hcnt) {
      st = s3 + slen;
      while (st > s3 && s > sbuf + hcnt)
         *--s = *--st;
      }

   /*
    * Start at the left end of the new string and copy s1 into it from
    *  left to right as many time as will fit in the left half of the
    *  new string.
    */
   s = sbuf;
   while (s < sbuf + hcnt) {
      st = s3;
      while (st < s3 + slen && s < sbuf + hcnt)
         *s++ = *st++;
      }

   slen = STRLEN(arg1);
   if (cnt < slen) {
      /*  
       * s1 is larger than the field to center it in.  The source for the
       *  copy starts at the appropriate point in s1 and the destination
       *  starts at the left end of of the new string.
       */
      s = sbuf;
      st = STRLOC(arg1) + slen/2 - hcnt + (~cnt&slen&1);
      }
   else {
      /*
       * s1 is smaller than the field to center it in.  The source for the
       *  copy starts at the left end of s1 and the destination starts at
       *  the appropriate point in the new string.
       */
      s = sbuf + hcnt - slen/2 - (~cnt&slen&1);
      st = STRLOC(arg1);
      }
   /*
    * Perform the copy, moving min(*s1,n) bytes from st to s.
    */
   if (slen > cnt)
      slen = cnt;
   while (slen-- > 0)
      *s++ = *st++;

   /*
    * Return the new string.
    */
   STRLEN(arg0) = cnt;
   STRLOC(arg0) = sbuf;
   }

Procblock(center,3)
