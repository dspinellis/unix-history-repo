#include "../h/rt.h"

/*
 * right(s1,n,s2) - pad s1 on left with s2 to length n.
 */

Xright(nargs, arg3, arg2, arg1, arg0)
int nargs;
struct descrip arg3, arg2, arg1, arg0;
   {
   register char *s, *st;
   int cnt, slen, i;
   char *sbuf, *s3, sbuf1[MAXSTRING], sbuf2[MAXSTRING];
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

   s = sbuf;
   while (s < sbuf + cnt) {
      st = s3;
      while (st < s3 + slen && s < sbuf + cnt)
         *s++ = *st++;
      }

   s = sbuf + cnt;
   slen = STRLEN(arg1);
   st = STRLOC(arg1) + slen;
   if (slen > cnt)
      slen = cnt;
   while (slen-- > 0)
      *--s = *--st;

   STRLEN(arg0) = cnt;
   STRLOC(arg0) = sbuf;
   }

struct b_iproc Bright = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xright),
   3,
   -1,
   0,
   0,
   {5, "right"}
   };
