#include "../h/rt.h"

/*
 * escan - restore &subject and &pos at the end of a scanning expression.
 *
 *  Arguments:
 *    arg1 - value being scanned
 *    arg2 - old value of &subject
 *    arg3 - old value of &pos
 *    arg4 - result of the scanning expression
 *
 * The result of the scanning expression is dereferenced if it refers to &subject
 *  or &pos, then copied to the first argument (the last three will
 *  be popped when escan returns).  Then the previous values of &subject
 *  and &pos are restored.
 *
 * Escan suspends once it has restored the old &subject; on failure
 *  the new &subject and &pos are "unrestored", and the failure is
 *  propagated into the using clause.
 */

escan(nargs, arg4, arg3, arg2, arg1)
int nargs;
struct descrip arg4, arg3, arg2, arg1;
   {
   DclSave
   struct descrip tmp;

   SetBound;

   /*
    * If the result of the scanning expression is &subject or &pos,
    *  it is dereferenced.
    */
   if (arg4.type == D_VAR && (int)BLKLOC(arg1) == (int)&k_subject)
      DeRef(arg4)
   if (arg4.type == D_TVPOS)
      DeRef(arg4)

   /*
    * Copy the result of the scanning expression into arg1, which will
    *  be the result of the scan.
    */
   arg1 = arg4;

   /*
    * Swap new and old values of &subject, leaving the new value in arg2.
    */
   tmp = k_subject;
   k_subject = arg2;
   arg2 = tmp;

   /*
    * Swap new and old values of &pos, leaving the new value in arg3.
    */
   tmp = arg3;
   INTVAL(arg3) = k_pos;
   k_pos = INTVAL(tmp);

   /*
    * Suspend the value of the scanning expression.
    */
   suspend();

   /*
    * Upon resumption, restore the new values for &subject and &pos
    *  and fail.
    */
   k_subject = arg2;
   k_pos = INTVAL(arg3);

   fail();
   }
