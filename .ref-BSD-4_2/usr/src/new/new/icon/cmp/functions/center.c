#include "../h/rt.h"

/*
 * center(s1,n,s2) - pad s1 on left and right with s2 to length n.
 */

Xcenter(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   register char *s, *st;
   int cnt, slen;
   int tmp, hcnt;
   char *sbuf, *s3;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern char *alcstr();

   if (cvstr(&arg1, sbuf1) == NULL)
      runerr(103, &arg1);
   defshort(&arg2, 1);
   if ((cnt = arg2.value.integer) < 0)
      runerr(205, &arg2);
   defstr(&arg3, sbuf2, &blank);

   sneed(cnt);
   if (STRLEN(arg3) == 0) {
      slen = 1;
      s3 = " ";
      }
   else {
      slen = STRLEN(arg3);
      s3 = STRLOC(arg3);
      }

   sbuf = alcstr(NULL, cnt);            /* use string space as buffer */
   hcnt = cnt / 2;
   s = sbuf + cnt ;                          /* pad on right */
   while (s > sbuf + hcnt) {
      st = s3 + slen;
      while (st > s3 && s > sbuf + hcnt)
         *--s = *--st;
      }

   s = sbuf;                                 /* pad on left */
   while (s < sbuf + hcnt) {
      st = s3;
      while (st < s3 + slen && s < sbuf + hcnt)
         *s++ = *st++;
      }

   slen = STRLEN(arg1);

   if (cnt < slen) { /* s1 is larger than field to center it in */
      s = sbuf;
      st = STRLOC(arg1) + slen/2 - hcnt + (~cnt&slen&1);
      }
   else {
      s = sbuf + hcnt - slen/2 - (~cnt&slen&1);
      st = STRLOC(arg1);
      }
   if (slen > cnt)
      slen = cnt;
   while (slen-- > 0)
      *s++ = *st++;

   STRLEN(arg0) = cnt;
   STRLOC(arg0) = sbuf;
   }

struct b_iproc Bcenter = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xcenter),
   3,
   -1,
   0,
   0,
   {6, "center"}
   };
