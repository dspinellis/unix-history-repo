/*
 * Routines for processing .u2 files.
 */

#include "ilink.h"
#include "opcode.h"

int trace = 0;			/* initial setting of &trace */
int nrecords = 0;		/* number of records in program */

/*
 * globals reads the global information from infile (.u2) and merges it with
 *  the global table and record table.
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
   extern char *getid(), *getstr();
   extern struct gentry *putglob();

   while ((op = getop(&name)) != EOF) {
      switch (op) {
         case OP_RECORD:	/* a record declaration */
            id = getid();	/* record name */
            n = getdec();	/* number of fields */
            newline();
            gp = glocate(id);
            /*
             * It's ok if the name isn't already in use or if the
             *  name is just used in a "global" declaration.  Otherwise,
             *  it is an inconsistent redeclaration.
             */
            if (gp == NULL || (gp->g_flag & ~F_GLOBAL) == 0) {
               putglob(id, F_RECORD, n, ++nrecords);
               while (n--) {	/* loop reading field numbers and names */
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

         case OP_IMPL:		/* undeclared identifiers should be noted */
            if (getop(&name) == OP_LOCAL)
               implicit = 0;
            else
               implicit = F_IMPERROR;
            break;

         case OP_TRACE:		/* turn on tracing */
            trace = -1;
            break;

         case OP_GLOBAL:	/* global variable declarations */
            n = getdec();	/* number of global declarations */
            newline();
            while (n--) {	/* process each declaration */
               getdec();	/* throw away sequence number */
               k = getoct();	/* get flags */
               if (k & (F_PROC & ~F_GLOBAL))
                  k |= implicit;
               id = getid();	/* get variable name */
               gp = glocate(id);
               /*
                * Check for conflicting declarations and install the
                *  variable.
                */
               if (gp != NULL &&
                   (k & (F_PROC & ~F_GLOBAL)) && gp->g_flag != F_GLOBAL)
                  err(id, "inconsistent redeclaration", 0);
               else if (gp == NULL || (k & (F_PROC & ~F_GLOBAL)))
                  putglob(id, k, getdec(), 0);
               newline();
               }
            break;

         case OP_LINK:		/* link the named file */
            name = getstr();	/* get the name and */
            addlfile(name);	/*  put it on the list of files to link */
            newline();
            break;

         default:
            fprintf(stderr, "%s: ill-formed global file %s\n", pname, inname);
            exit(1);
         }
      }
   }
