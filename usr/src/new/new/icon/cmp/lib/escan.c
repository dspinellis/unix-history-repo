#include "../h/rt.h"

/*
 * escan - restore &subject and &pos at the end of a scan expression.
 * Escan takes 4 arguments: arg1 is the variable being scanned;
 * arg2 is the old value of &subject; arg3 is the old value of
 * &pos; and arg4 is the result of the using clause.  The result
 * of the using clause is dereferenced if it refers to &subject
 * or &pos, then copied to the first argument (the last three will
 * be popped when escan returns).  Then the previous values of
 * &subject and &pos are restored.
 * Escan suspends once it has restored the old &subject; on failure
 * the new &subject and &pos are "unrestored", and the failure is
 * propagated into the using clause.
 */

escan(nargs, arg4, arg3, arg2, arg1)
int nargs;
struct descrip arg4, arg3, arg2, arg1;
   {
   DclSave
   struct descrip tmp;

   SetBound;

   if (arg4.type == D_VAR && BLKLOC(arg1) == &k_subject)
      deref(&arg4);
   if (arg4.type == D_TVPOS)
      deref(&arg4);

   arg1 = arg4;

   tmp = k_subject;
   k_subject = arg2;
   arg2 = tmp;

   tmp = arg3;
   INTVAL(arg3) = k_pos;
   k_pos = INTVAL(tmp);

   suspend();

   k_subject = arg2;
   k_pos = INTVAL(arg3);

   fail();
   }
