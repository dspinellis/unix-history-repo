#include "../h/rt.h"
#include "../h/record.h"

/*
 * type(x) - return type of x as a string.
 */

Xtype(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   DeRef(arg1)

   /*
    * This is pretty simple. Just make a string naming the type of
    *  x and return it.  For records, the name of the record is returned.
    */
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
#ifdef LONGS
         case T_LONGINT:
#endif LONGS
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
         case T_LELEM:
            STRLEN(arg0) = 18;
            STRLOC(arg0) = "list element block";
            break;
         case T_TABLE:
            STRLEN(arg0) = 5;
            STRLOC(arg0) = "table";
            break;
         case T_TELEM:
            STRLEN(arg0) = 19;
            STRLOC(arg0) = "table element block";
            break;
#ifdef SETS
         case T_SET:
            STRLEN(arg0) = 3;
            STRLOC(arg0) = "set";
            break;
         case T_SELEM:
            STRLEN(arg0) = 17;
            STRLOC(arg0) = "set element block";
            break;
#endif SETS

         case T_RECORD:
            arg0 = BLKLOC(arg1)->record.recptr->recname;
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

Procblock(type,1)
