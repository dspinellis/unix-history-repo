
/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)debug.c	1.3\t%G%";

/*
 *  Debug routines
 */

#include "defs.h"
#include "tree.h"
#include "operators.h"
#include "eval.h"
#include "events.h"
#include "symbols.h"
#include "scanner.h"
#include "source.h"
#include "object.h"
#include "mappings.h"
#include "process.h"
#include "machine.h"
#include <signal.h>


public int debug_flag[20];

public debug(p)
Node p;
{
   int code;
   code = p->value.lcon;

   if ( (code >= 0) and (code < 10) ) {
   	switch(code)  {
	case 2:   if(debug_flag[2])  debug_flag[2]=0;
		  else debug_flag[2] =1;
                  printf(" flag 2 is %d \n",debug_flag[2]);
	          break;

	case 3:   if(debug_flag[3])  debug_flag[3]=0;
		  else debug_flag[3] =1;
                  printf(" flag 3 is %d \n",debug_flag[3]);
	          break;

	case 4:   if(debug_flag[4])  debug_flag[4]=0;
		  else debug_flag[4] =1;
                  printf(" flag 4 is %d \n",debug_flag[4]);
	          break;

	case 5:   if(debug_flag[5])  debug_flag[5]=0;
		  else debug_flag[5] =1;
                  printf(" flag 5 is %d \n",debug_flag[5]);
	          break;

	case 6:   dumpfunctab();
	          break;

	default:  printf(" unknown debug code %ld \n",p->value.lcon);
                  break;
        }
   }
   else if (debug_flag[3]) symbol_dump(code);
   else if (debug_flag[4]) psym(code);
}

public char *showoperator(op)
Operator op;
{
static char *operator_str[] = {
"O_NOP", "O_NAME", "O_SYM", "O_LCON", "O_FCON", "O_SCON", "O_RVAL", "O_INDEX",
"O_INDIR", "O_DOT", "O_COMMA", "O_ITOF", "O_ADD", "O_ADDF", "O_SUB", "O_SUBF",
"O_NEG", "O_NEGF", "O_MUL", "O_MULF", "O_DIVF", "O_DIV", "O_MOD", "O_AND",
"O_OR", "O_LT", "O_LTF", "O_LE", "O_LEF", "O_GT", "O_GTF", "O_GE", "O_GEF",
"O_EQ", "O_EQF", "O_NE", "O_NEF", "O_ALIAS", "O_ASSIGN", "O_CALL", "O_CATCH",
"O_CHFILE", "O_CONT", "O_DEBUG", "O_DELETE", "O_DUMP", "O_EDIT", "O_FUNC",
"O_GRIPE", "O_HELP", "O_IGNORE", "O_LIST", "O_PRINT", "O_PSYM", "O_RUN",
"O_SKIP", "O_SOURCE", "O_STATUS", "O_STEP", "O_STOP", "O_STOPI", "O_TRACE",
"O_TRACEI", "O_WHATIS", "O_WHERE", "O_WHEREIS", "O_WHICH", "O_EXAMINE",
"O_ADDEVENT", "O_ENDX", "O_IF", "O_ONCE", "O_PRINTCALL", "O_PRINTIFCHANGED",
"O_PRINTRTN", "O_PRINTSRCPOS", "O_PROCRTN", "O_QLINE", "O_STOPIFCHANGED",
"O_STOPX", "O_TRACEON", "O_TRACEOFF", "O_TYPERENAME", "O_LASTOP" };
return( operator_str[ord(op)] );
}

/*
 * Dump a tree recursively
 */

public dumptree(f, p)
File f;
register Node p;
{
    register Node q;
    Operator op;
    static recurse  =0;
    ++recurse;

    if (p != nil) {
	op = p->op;
	if (ord(op) > ord(O_LASTOP)) {
	    panic("bad op %d in dumptree", p->op);
	}
        { int n_args;
	  fprintf(f, "\n level %d op %s node %ld ",recurse,showoperator(op), p);
          for(n_args=0;n_args < nargs(op); n_args++) 
            fprintf(f," arg%d %ld ",n_args,p->value.arg[n_args]);
          fprintf(f,"\n");
        }
        if(p->nodetype) {fprintf(f,"nodetype: "); psym(p->nodetype);}
	switch (op) {
	    case O_NAME:
		fprintf(f, "%s", ident(p->value.name));
		break;

	    case O_SYM:
		printname(f, p->value.sym);
		break;

	    case O_QLINE:
		if (nlhdr.nfiles > 1) {
		    dumptree(f, p->value.arg[0]);
		    fprintf(f, ":");
		}
		dumptree(f, p->value.arg[1]);
		break;

	    case O_LCON:
		if (compatible(p->nodetype, t_char)) {
		    fprintf(f, "'%c'", p->value.lcon);
		} else {
		    fprintf(f, "%d", p->value.lcon);
		}
		break;

	    case O_FCON:
		fprintf(f, "%g", p->value.fcon);
		break;

	    case O_SCON:
		fprintf(f, "\"%s\"", p->value.scon);
		break;

	    case O_INDEX:
		dumptree(f, p->value.arg[0]);
		fprintf(f, "[");
		dumptree(f, p->value.arg[1]);
		fprintf(f, "]");
		break;

	    case O_COMMA:
		dumptree(f, p->value.arg[0]);
		if (p->value.arg[1] != nil) {
		    fprintf(f, ", ");
		    dumptree(f, p->value.arg[1]);
		}
		break;

	    case O_RVAL:
		if (p->value.arg[0]->op == O_SYM) {
		    printname(f, p->value.arg[0]->value.sym);
		} else {
		    dumptree(f, p->value.arg[0]);
		}
		break;

	    case O_ITOF:
		dumptree(f, p->value.arg[0]);
		break;

	    case O_CALL:
		dumptree(f, p->value.arg[0]);
		if (p->value.arg[1]!= nil) {
		    fprintf(f, "(");
		    dumptree(f, p->value.arg[1]);
		    fprintf(f, ")");
		}
		break;

	    case O_INDIR:
		q = p->value.arg[0];
		if (isvarparam(q->nodetype)) {
		    dumptree(f, q);
		} else {
		    if (q->op == O_SYM or q->op == O_LCON or q->op == O_DOT) {
			dumptree(f, q);
			fprintf(f, "^");
		    } else {
			fprintf(f, "*(");
			dumptree(f, q);
			fprintf(f, ")");
		    }
		}
		break;

	    case O_DOT:
		q = p->value.arg[0];
		if (q->op == O_INDIR) {
		    dumptree(f, q->value.arg[0]);
		} else {
		    dumptree(f, q);
		}
		fprintf(f, ".%s", symname(p->value.arg[1]->value.sym));
		break;

	    default:
		switch (degree(op)) {
		    case BINARY:
			dumptree(f, p->value.arg[0]);
			fprintf(f, "%s", opinfo[ord(op)].opstring);
			dumptree(f, p->value.arg[1]);
			break;

		    case UNARY:
			fprintf(f, "%s", opinfo[ord(op)].opstring);
			dumptree(f, p->value.arg[0]);
			break;

		    default:
                        if(degree(op) < ord(O_LASTOP) )
                        {      int i;
                               if( nargs(op)  != 0) 
                                 for(i=0;i<nargs(op);i++) 
                                  dumptree(f, p->value.arg[i]);
			} 
			else
                          error("internal error: bad op %d in dumptree", op);
		}
		break;
	}
    }
   recurse--;
   fflush(f);
}
