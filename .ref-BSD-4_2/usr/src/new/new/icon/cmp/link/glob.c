#include "ulink.h"
#include "opcode.h"

int trace = 0;			/* initial setting of &trace */
int nrecords = 0;		/* number of records in program */

/*
 * globals - read individual global table from infile and
 * merge it with main global table.
 * Record table is also processed.
 */

globals(i)
int i;
   {
   register char *id;
   register int n, op;
   int k;
   int implicit;
   char *name;
   struct gentry *gp, *glocate();
   extern char *getid();
   extern struct gentry *putglob();

   while ((op = getop(&name)) != EOF) {
      switch (op) {
         case OP_RECORD:
            id = getid();
            n = getdec();
            newline();
	    gp = glocate(id);
	    if (gp == NULL || (gp->g_flag & ~F_GLOBAL) == 0) {
	       putglob(id, F_RECORD, n, ++nrecords);
               while (n--) {
                  k = getdec();
                  putfield(getid(), nrecords, k);
                  newline();
                  }
               }
	    else {
	       err(id, "inconsistent redeclaration", 0);
               while (n--)
                  newline();
               }
            break;

         case OP_IMPL:
	    if (getop(&name) == OP_LOCAL)
	       implicit = 0;
	    else
	       implicit = F_IMPERROR;
	    break;

	 case OP_TRACE:
	    trace = -1;
	    break;

	 case OP_GLOBAL:
            n = getdec();
            newline();
            while (n--) {
               getdec();
               k = getoct();
	       if (k & (F_PROC & ~F_GLOBAL))
		  k |= implicit;
               id = getid();
	       gp = glocate(id);
	       if (gp != NULL &&
		   (k & (F_PROC & ~F_GLOBAL)) && gp->g_flag != F_GLOBAL)
   	          err(id, "inconsistent redeclaration", 0);
	       else if (gp == NULL || (k & (F_PROC & ~F_GLOBAL)))
                  putglob(id, k, getdec(), 0);
               newline();
               }
            break;

	 default:
            fprintf(stderr, "%s: ill-formed global file %s\n", pname, inname);
            exit(1);
         }
      }
   }
