#include "../h/rt.h"

/*
 * doasgn - assign value of a2 to variable a1.
 *  Does the work for asgn, swap, rasgn, and rswap.
 */

doasgn(a1, a2)
struct descrip *a1, *a2;
   {
   register int l1, l2;
   register union block *bp;
   register struct b_table *tp;
   union block *hook;
   long l3;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING];
   extern struct descrip tended[];   /* uses tended[1] through tended[5] */
   extern struct b_lelem *alclstb();
   extern char *alcstr();

   tended[1] = *a1;
   tended[2] = *a2;

assign:
#ifdef DEBUG
   if (QUAL(tended[1]) || !VAR(tended[1]))
      syserr("doasgn: variable expected");
#endif DEBUG

   if (TVAR(tended[1])) {
      switch (TYPE(tended[1])) {
         case T_TVSUBS:
            /*
             * An assignment is being made to a substring trapped variable.
             *  Conceptually, there are three units involved: the value to
             *  be assigned to the substring, the string containing the
             *  substring and the substring itself.
             *
             * As an example, consider the action of x[2:4] := "xyz" where
             *  x == "abcd".  The string containing the substring is "abcd",
             *  the substring is "bc", and the value to be assigned is "xyz".
             *  A string is allocated for the result, and the portion of the
             *  string containing the substring up to the substring ("a" in
             *  this case) is copied into the new string.  Then, the value
             *  to be assigned, ("xyz"), is added to the new string.
             *  Finally, the portion of the substrung string to the right
             *  of the substring ("d") is copied into the new string to
             *  complete the result ("axyzd").
             *
             * The tended descriptors are used as follows:
             *   tended[1] - the substring trapped variable
             *   tended[2] - the value to assign
             *   tended[3] - the string containing the substring
             *   tended[4] - the substring
             *   tended[5] - the result string
             */
            /*
             * Be sure that the value to assign is a string.  The result
             *  is not used, so it seems like it would be much faster to
             *  see if the value is already a string and only call cvstr
             *  if necessary.
             */
            if (cvstr(&tended[2], sbuf1) == NULL)
               runerr(103, &tended[2]);
            /*
             * Be sure that the string containing the substring is a string.
             */
            tended[3] = BLKLOC(tended[1])->tvsubs.ssvar;
            if (cvstr(&tended[3], sbuf2) == NULL)
               runerr(103, &tended[3]);
            /*
             * Ensure that there is enough string space by checking for
             *  the worst case size which is the length of the substrung
             *  string plus the length of the value to be assigned.
             */
            sneed(STRLEN(tended[3]) + STRLEN(tended[2]));
            /*
             * Get a pointer to the tvsubs block and make l1 a C-style
             *  index to the character that begins the substring.
             */
            bp = BLKLOC(tended[1]);
            l1 = bp->tvsubs.sspos - 1;
            /*
             * Make tended[4] a descriptor for the substring.
             */
            STRLEN(tended[4]) = bp->tvsubs.sslen;
            STRLOC(tended[4]) = STRLOC(tended[3]) + l1;
            /*
             * Make l2 a C-style index to the character after the substring.
             *  If l2 is greater than the length of the substrung string,
             *  it's an error because the string being assigned won't fit.
             */
            l2 = l1 + STRLEN(tended[4]);
            if (l2 > STRLEN(tended[3]))
               runerr(205,NULL);
            /*
             * Form the result string.  First, copy the portion of the
             *  substring string to the left of the substring into the string
             *  space.
             */
            STRLOC(tended[5]) = alcstr(STRLOC(tended[3]), l1);
            /*
             * Copy the string to be assigned into the string space,
             *  effectively concatenating it.
             */
            alcstr(STRLOC(tended[2]), STRLEN(tended[2]));
            /*
             * Copy the portion of the substrung string to the right of
             *  the substring into the string space, completing the result.
             */
            alcstr(STRLOC(tended[3])+l2, STRLEN(tended[3])-l2);
            /*
             * Calculate the length of the new string by:
             *   length of substring string minus
             *   length of substring (it was replaced) plus
             *   length of the assigned string.
             */
            STRLEN(tended[5]) = STRLEN(tended[3]) - STRLEN(tended[4]) +
               STRLEN(tended[2]);
            /*
             * For this next portion, the parchments left by the Old Ones read
             *  "tail recursion:"
             *  "  doasgn(bp->tvsubs.ssvar,tended[5]);"
             */
            bp->tvsubs.sslen = STRLEN(tended[2]);
            tended[1] = bp->tvsubs.ssvar;
            tended[2] = tended[5];
            goto assign;

         case T_TVTBL:
            /*
             * An assignment is being made to a table element trapped
             *  variable.
             *
             * Tended descriptors:
             *  tended[1] - the table element trapped variable
             *  tended[2] - the value to be assigned
             *  tended[3] - subscripting value
             *
             * Point bp at the trapped variable block; point tended[3]
             *  at the subscripting value; point tp at the table
             *  header block.
             */
            bp = BLKLOC(tended[1]);
            if (bp->tvtbl.type == T_TELEM) {
            /*
             * It is a converted tvtbl block already in the table
             *  just assign to it and return.
             */
                bp->telem.tval = tended[2];
                clrtend();
                return;
                }
            tended[3] = bp->tvtbl.tvtref;
            tp = (struct b_table *) BLKLOC(bp->tvtbl.tvtable);
            /*
             * Get a hash value for the subscripting value and locate the
             *  element chain on which the element being assigned to will
             *  be placed.
             */
            l1 = bp->tvtbl.hashnum;
            l2 = l1 % NBUCKETS;   /* bucket number */
            bp = BLKLOC(tp->buckets[l2]);
            /*
             * Look down the bucket chain to see if the value is already
             *  in the table.  If it's there, just assign to it and return.
             */
            hook = bp;
            while (bp != NULL) {
              if ( bp->telem.hashnum > l1 ) /* past it - not there */
                   break;
              if ((bp->telem.hashnum == l1) &&
                 (equiv(&bp->telem.tref, &tended[3]))) {
                       bp->telem.tval = tended[2];
                       clrtend();
                       return;
                       }
               hook = bp; 
               bp = BLKLOC(bp->telem.blink);
               }
            /*
             * The value being assigned is new.  Increment the table size,
             *  and convert the tvtbl to a telem and link it into the chain
             *  in the table.
             */
            tp->cursize++;
            a1->type = D_VAR | D_TELEM;
            if (hook == bp) {		/* new element goes at front of chain */
               bp = BLKLOC(tended[1]);
               bp->telem.blink = tp->buckets[l2];
               BLKLOC(tp->buckets[l2]) = bp; 
               tp->buckets[l2].type = D_TELEM; 
               }
            else {			/* new element follows hook */
               bp = BLKLOC(tended[1]);
               bp->telem.blink = hook->telem.blink;
               BLKLOC(hook->telem.blink) =  bp;
               hook->telem.blink.type = D_TELEM;
               }
            bp->tvtbl.type = T_TELEM;
            bp->telem.tval = tended[2];
            clrtend();
            return;

         case T_TVPOS:
            /*
             * An assignment to &pos is being made.  Be sure that the
             *  value being assigned is a (non-long) integer.
             */
            switch (cvint(&tended[2], &l3)) {
               case T_INTEGER:  break;
#ifdef LONGS
               case T_LONGINT:  clrtend(); fail();
#endif LONGS
               default:         runerr(101, &tended[2]);
               }
            /*
             * Convert the value into a position and be sure that it's
             *  in range.  Note that cvpos fails if the position is past
             *  the end of the string.
             */
            l1 = cvpos(l3, STRLEN(k_subject));
            if (l1 <= 0) {
               clrtend();
               fail();
               }
            /*
             * If all is well, make the assignment to &pos and return.
             */
            k_pos = l1;
            clrtend();
            return;

         case T_TVRAND:
            /*
             * An assignment to &random is being made.  Be sure that the
             *  value being assigned is an integer.
             */
            switch (cvint(&tended[2], &l3)) {
               case T_INTEGER:
#ifdef LONGS
               case T_LONGINT:
#endif LONGS
                                break;
               default:         runerr(101, &tended[2]);
               }
            k_random = l3;
            clrtend();
            return;

         case T_TVTRACE:
            /*
             * An assignment to &trace is being made.  Be sure that the
             *  value being assigned is an integer.  Should it be a long
             *  integer, just set &trace to -1.
             */
            switch (cvint(&tended[2], &l3)) {
               case T_INTEGER:  k_trace = (int)l3; break;
#ifdef LONGS
               case T_LONGINT:  k_trace = -1; break;
#endif LONGS
               default:         runerr(101, &tended[2]);
               }
            clrtend();
            return;

         default:
            syserr("doasgn: illegal trapped variable");
         }
      }

   if (VARLOC(tended[1]) == &k_subject) {
      /*
       * An assignment is being made to &subject.  Be sure that the value
       *  being assigned is a string.  If the value is converted to a string,
       *  allocate it.  Note that &pos is set to 1.
       */
      switch (cvstr(&tended[2], sbuf1)) {
         case NULL:
            runerr(103, &tended[2]);
         case 1:
            sneed(STRLEN(tended[2]));
            STRLOC(tended[2]) = alcstr(STRLOC(tended[2]), STRLEN(tended[2]));
         case 2:
            k_subject = tended[2];
            k_pos = 1;
         }
      }
   else
      /*
       * The easy case, just replace the variable descriptor with the value
       *  descriptor.
       */
      *VARLOC(tended[1]) = tended[2];
   clrtend();
   return;
   }

/*
 * clrtend - clear the tended descriptors.
 */
clrtend()
   {
   register struct descrip *p;
   extern struct descrip tended[];

   for (p = &tended[1]; p <= &tended[5]; p++)
      *p = nulldesc;
   }
