#include "../h/rt.h"
#include "../h/record.h"

/*
 * *x - return size of string or object x.
 */

size(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DclSave
   char sbuf[MAXSTRING];

   SetBound;
   /*
    * Make sure x isn't null.
    */
   DeRef(arg1)
   if (NULLDESC(arg1))
      runerr(112, &arg1);
   if (cvstr(&arg1, sbuf) != NULL) {
      /*
       * If x can be converted to a string, return the length of the string
       *  as the size of x.
       */
      arg0.type = D_INTEGER;
      INTVAL(arg0) = STRLEN(arg1);
      }
   else {
      /*
       * x isn't a string.  *x for lists and tables is the current size;
       *  *x for records is the number of fields, and *x for co-expressions
       *  is the number of results that have been produced.
       */
      switch (TYPE(arg1)) {
         case T_LIST:
            arg0.type = D_INTEGER;
            INTVAL(arg0) = BLKLOC(arg1)->list.cursize;
            break;

         case T_TABLE:
            arg0.type = D_INTEGER;
            INTVAL(arg0) = BLKLOC(arg1)->table.cursize;
            break;
#ifdef SETS
         case T_SET:
            arg0.type = D_INTEGER;
            INTVAL(arg0) = BLKLOC(arg1)->set.setsize;
            break;
#endif SETS
         case T_RECORD:
            arg0.type = D_INTEGER;
            INTVAL(arg0) = BLKLOC(arg1)->record.recptr->nfields;
            break;

         case T_ESTACK:
            arg0.type = D_INTEGER;
            INTVAL(arg0) = BLKLOC(arg1)->estack.nresults;
            break;

         default:
            /*
             * There is no notion of size for this datatype.
             */
            runerr(112, &arg1);
         }
      }
   ClearBound;
   }

Opblock(size,1,"*")
