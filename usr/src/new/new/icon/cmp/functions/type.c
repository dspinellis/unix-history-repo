#include "../h/rt.h"
#include "../h/record.h"

/*
 * type(x) - return type of x as a string.
 */

Xtype(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   deref(&arg1);

   if (NULLDESC(arg1)) {
      STRLEN(arg0) = 4;
      STRLOC(arg0) = "null";
      }
   else if (QUAL(arg1)) {
      STRLEN(arg0) = 6;
      STRLOC(arg0) = "string";
      }
   else {
      switch (TYPE(arg1)) {
         case T_INTEGER:
#ifndef BIT32
         case T_LONGINT:
#endif
            STRLEN(arg0) = 7;
            STRLOC(arg0) = "integer";
            break;
         case T_REAL:
            STRLEN(arg0) = 4;
            STRLOC(arg0) = "real";
            break;
         case T_CSET:
            STRLEN(arg0) = 4;
            STRLOC(arg0) = "cset";
            break;
         case T_FILE:
            STRLEN(arg0) = 4;
            STRLOC(arg0) = "file";
            break;
         case T_PROC:
            STRLEN(arg0) = 9;
            STRLOC(arg0) = "procedure";
            break;
         case T_LIST:
            STRLEN(arg0) = 4;
            STRLOC(arg0) = "list";
            break;
         case T_TABLE:
            STRLEN(arg0) = 5;
            STRLOC(arg0) = "table";
            break;
         case T_RECORD:
            arg0 = BLKLOC(arg1)->record.recptr->proc.recname;
            break;
         case T_ESTACK:
            STRLEN(arg0) = 13;
            STRLOC(arg0) = "co-expression";
            break;
         default:
            syserr("type: unknown type.");
         }
      }
   }

struct b_iproc Btype = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xtype),
   1,
   -1,
   0,
   0,
   {4, "type"}
   };
