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
   deref(&arg1);
   if (NULLDESC(arg1))
      runerr(112, &arg1);
   if (cvstr(&arg1, sbuf) != NULL) {
      arg0.type = D_INTEGER;
      INTVAL(arg0) = STRLEN(arg1);
      }
   else {
      switch (TYPE(arg1)) {
         case T_LIST:
            arg0.type = D_INTEGER;
	    BLKLOC(arg0) = BLKLOC(arg1)->list.cursize;
	    break;

         case T_TABLE:
            arg0.type = D_INTEGER;
            BLKLOC(arg0) = BLKLOC(arg1)->table.cursize;
            break;

         case T_RECORD:
            arg0.type = D_INTEGER;
	    BLKLOC(arg0) = BLKLOC(arg1)->record.recptr->proc.nfields;
	    break;

         case T_ESTACK:
            arg0.type = D_INTEGER;
	    BLKLOC(arg0) = BLKLOC(arg1)->estack.nresults;
	    break;

         default:
            runerr(112, &arg1);
         }
      }
   ClearBound;
   }
struct b_iproc Bsize = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(size),
   1,
   -1,
   0,
   0,
   {1, "*"}
   };
