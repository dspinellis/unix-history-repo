#include "ulink.h"
#include "code.h"
#include "opcode.h"
#include "datatype.h"
#ifdef VAX
int nproc = 0;			/* number of procedures encountered */

/*
 * gencode - read .ucode file, resolve variable references,
 * and generate assembler code.
 */

gencode(n)
int n;
   {
   register int op, k, lab;
   int j, nargs, flags, implicit;
   char *id, *name, *procname;
   struct centry *cp;
   struct gentry *gp;
   struct fentry *fp;

   extern long getint();
   extern double getreal();
   extern char *getid(), *getstrlit();
   extern struct gentry *glocate();

   while ((op = getop(&name)) != EOF) {
      switch (op) {
         case OP_TOBY:		/* ternary operators */
            newline();
	    emit(C_CALL, 3, name, 0);
            break;
	 case OP_SECT:
            newline();
	    emit(C_PUSHN,
		 C_CALL, 4, name, 0);
            break;

         case OP_ASGN:          /* binary operators */
         case OP_CAT:
         case OP_DIFF:
         case OP_DIV:
         case OP_EQV:
         case OP_INTER:
	 case OP_LCONCAT:
         case OP_LEXEQ:
         case OP_LEXGE:
         case OP_LEXGT:
         case OP_LEXLE:
         case OP_LEXLT:
         case OP_LEXNE:
         case OP_MINUS:
	 case OP_MOD:
         case OP_MULT:
         case OP_NEQV:
         case OP_NUMEQ:
         case OP_NUMGE:
         case OP_NUMGT:
         case OP_NUMLE:
         case OP_NUMLT:
         case OP_NUMNE:
         case OP_PLUS:
         case OP_POWER:
         case OP_RASGN:
         case OP_UNIONCS:
	    newline();
	    emit(C_CALL, 2, name, 0);
	    break;
         case OP_RSWAP:
	 case OP_SUBSC:
         case OP_SWAP:
	    newline();
	    emit(C_PUSHN,
		 C_CALL, 3, name, 0);
	    break;

         case OP_COMPL:		/* unary operators */
         case OP_NEG:
         case OP_NONNULL:
         case OP_NULL:
         case OP_NUMBER:
         case OP_REFRESH:
	 case OP_SIZE:
	 case OP_VALUE:
	    newline();
	    emit(C_CALL, 1, name, 0);
	    break;
         case OP_BANG:		
	 case OP_RANDOM:
         case OP_TABMAT:
	    newline();
	    emit(C_PUSHN,
		 C_CALL, 2, name, 0);
	    break;

         case OP_BSCAN:
	    newline();
	    emit(C_MOVQ, "_k_subject", "-(sp)",
		 C_PUSH, "_k_pos", C_PUSHI, D_INTEGER,
		 C_CALL, 0, name, 0);
	    break;

         case OP_CCASE:
	    newline();
            emit(C_PUSHN,
		 C_MOVQ, "4(r11)", "-(sp)", 0);
	    break;

         case OP_CHFAIL:
	    lab = getlab();
            newline();
	    emit(C_MOVL, lab, n, "-8(r11)", 0);
	    break;

         case OP_COACT:
	    newline();
	    emit(C_CALL, 1, "coact", 0);
	    break;

	 case OP_COFAIL:
	    newline();
	    emit(C_CALL, 0, "cofail", 0);
	    break;

         case OP_CORET:
	    newline();
	    emit(C_CALL, 0, "coret", 0);
	    break;

         case OP_CREATE:
            lab = getlab();
            newline();
            emit(C_PUSHL, lab, n, C_PUSHI, D_INTEGER,
                 C_CALL, 0, "create", 0);
            break;

         case OP_CSET:
	    k = getdec();
	    newline();
	    cp = &ctable[k];
	    emit(C_PUSHC, k, nproc,
		 C_PUSHI, D_CSET, 0);
	    break;

	 case OP_DUP:
	    newline();
      	    emit(C_PUSHN,
                 C_MOVQ, "8(sp)", "-(sp)", 0);
	    break;

	 case OP_EFAIL:
	    newline();
	    emit(C_CALLJ, 0, "efail", 0);
	    break;

	 case OP_ERET:
	    newline();
	    emit(C_MOVQ, "(sp)+","r0",
		 C_MOV, "-4(r11)", "r10",
        	 C_MOV, "r11", "sp",
		 C_POP, "r11",
		 C_MOVQ, "r0", "-(sp)", 0);
	    break;

         case OP_ESCAN:
	    newline();
    	    emit(C_CALL, 3, "escan", 0);
	    break;

         case OP_ESUSP:
	    newline();
    	    emit(C_CALL, 1, "esusp", 0);
	    break;

         case OP_FIELD:
	    id = getid();
	    newline();
	    fp = flocate(id);
	    if (fp == NULL) {
	       err(id, "invalid field name", 0);
	       break;
	       }
	    emit(C_PUSHI, fp->f_fid, C_PUSHI, D_INTEGER,
		 C_CALL, 2, "field", 0);
	    break;

	 case OP_FILE:
	    file = getid();
	    newline();
	    emit(C_MOVA, "ident", file - strings, "_file", 0);
	    break;

         case OP_GOTO:
	    lab = getlab();
	    newline();
	    emit(C_JUMP, "br", lab, n, 0);
	    break;

         case OP_INCRES:
	    newline();
	    emit(C_MOV, "_current+4", "r0",
                 C_MISC, "incl", "28(r0)",
                 0);
	    break;

         case OP_INIT:
	    lab = getlab();
	    newline();
	    emit(C_INIT, nproc, lab, n, 0);
	    break;

         case OP_INT:
	    k = getdec();
	    newline();
	    cp = &ctable[k];
#ifndef VAX
	    if (cp->c_flag & F_LONGLIT)
	       emit(C_PUSHC, k, nproc,
		    C_PUSHI, D_LONGINT, 0);
	    else
#endif
   	       emit(C_PUSHI, (int) cp->c_val.ival,
   		    C_PUSHI, D_INTEGER, 0);
	    break;

         case OP_INVOKE:
	    k = getdec();
	    newline();
	    emit(C_CALL, k, "invoke", 0);
	    break;

         case OP_KEYWD:
            k = getdec();
	    newline();
            emit(C_PUSHI, k, C_PUSHI, D_INTEGER,
		 C_CALL, 0, "keywd", 0);
	    break;

         case OP_LAB:
	    lab = getlab();
	    newline();
	    emit(C_LABEL, lab, n, 0);
	    break;

	 case OP_LIMIT:
	    newline();
	    emit(C_CALL, 0, "limit", 0);
	    break;

	 case OP_LINE:
	    line = getdec();
	    newline();
	    emit(C_MOVI, line, "_line", 0);
	    break;

         case OP_LLIST:
	    k = getdec();
	    newline();
	    emit(C_CALL, k, "llist", 0);
	    break;

	 case OP_LSUSP:
	    newline();
	    emit(C_CALL, 0, "lsusp", 0);
	    break;

         case OP_MARK:
            lab = getlab();
	    newline();
	    emit(C_PUSH, "r11",
		 C_MOV, "sp", "r11",
		 C_PUSH, "r10",
		 C_CLR, "r10", 0);
	    if (lab != 0)
	       emit(C_PUSHL, lab, n, 0);
	    else
	       emit(C_PUSHZ, 0);
	    break;

	 case OP_PFAIL:
	    newline();
	    emit(C_CALLJ, 0, "pfail", 0);
	    break;

         case OP_PNULL:
	    newline();
	    emit(C_PUSHN, 0);
	    break;

         case OP_POP:
	    newline();
	    emit(C_MISC, "addl2", "$8,sp", 0);
	    break;

         case OP_PRET:
	    newline();
	    emit(C_CALL, 1, "pret", 0);
	    break;

         case OP_PSUSP:
	    newline();
	    emit(C_CALL, 0, "psusp", 0);
	    break;

         case OP_PUSH1:
	    newline();
	    emit(C_PUSHI, 1, C_PUSHI, D_INTEGER, 0);
	    break;

         case OP_PUSHN1:
	    newline();
	    emit(C_PUSHI, -1, C_PUSHI, D_INTEGER, 0);
	    break;

         case OP_REAL:
	    k = getdec();
	    newline();
	    emit(C_PUSHC, k, nproc,
		 C_PUSHI, D_REAL, 0);
	    break;

	 case OP_SDUP:
	    newline();
	    emit(C_MOVQ, "(sp)", "-(sp)", 0);
	    break;

         case OP_STR:
	    k = getdec();
	    newline();
	    id = ctable[k].c_val.sval;
	    emit(C_PUSHA, "ident", id - strings,
		 C_PUSHI, ctable[k].c_length, 0);
	    break;

	 case OP_UNMARK:
	    k = getdec();
	    newline();
	    while (k-- > 1)
	       emit(C_MOV, "(r11)", "r11", 0);
	    emit(C_MOV, "-4(r11)", "r10",
		 C_MOV, "r11", "sp",
		 C_POP, "r11", 0);
	    break;

         case OP_VAR:
	    k = getdec();
	    newline();
	    if ((flags = ltable[k].l_flag) & F_GLOBAL)
	       emit(C_PUSHA, "globals", 2*BYTES*(ltable[k].l_val.global-gtable),
		    C_PUSHI, D_VAR, 0);
	    else if (flags & F_STATIC)
	       emit(C_PUSHA, "statics", 2*BYTES*(ltable[k].l_val.staticid-1),
		    C_PUSHI, D_VAR, 0);
	    else if (flags & F_ARGUMENT)
	       emit(C_PUSHIN, 2*BYTES + 2*BYTES*(nargs-ltable[k].l_val.offset),
		    "ap",
		    C_PUSHI, D_VAR, 0);
	    else
	       emit(C_PUSHIN, -2*BYTES - 2*BYTES*ltable[k].l_val.offset, "fp",
		    C_PUSHI, D_VAR, 0);
	    break;

         case OP_PROC:
	    locinit();
	    line = 0;
	    procname = getid();
	    newline();
	    gp = glocate(procname);
	    implicit = gp->g_flag & F_IMPERROR;
	    nargs = gp->g_nargs;
	    gp->g_procid = ++nproc;
	    break;

         case OP_LOCAL:
	    k = getdec();
	    flags = getoct();
	    id = getid();
	    putloc(k, id, flags, implicit, procname);
	    break;

         case OP_CON:
	    k = getdec();
	    flags = getoct();
	    if (flags & F_INTLIT)
	       putconst(k, flags, 0, getint());
	    else if (flags & F_REALLIT)
	       putconst(k, flags, 0, getreal());
	    else if (flags & F_STRLIT) {
	       j = getdec();
	       putconst(k, flags, j, getstrlit(j));
               }
	    else if (flags & F_CSETLIT) {
	       j = getdec();
	       putconst(k, flags, j, getstrlit(j));
	       }
	    else
	       fprintf(stderr, "gencode: illegal constant\n");
	    newline();
	    break;

	 case OP_DECLEND:
	    newline();
	    emitproc(nproc, procname, nargs, dynoff,
		     statics-static1, static1);
	    break;

         case OP_END:
	    newline();
	    procend();
	    break;

	 default:
	    fprintf(stderr, "gencode: illegal opcode(%d): %s\n", op, name);
	    newline();
	 }
      }
   }

/*
 * emit - emit assembler code to outfile.  This routine
 * takes a variable number of arguments.  Each group of
 * arguments begins with a template number, which selects
 * one of the fprintf statements below.  Following the
 * template number is zero or more parameters to that
 * template.
 */

emit(a)
int a;
   {
   register union {
      char *str;
      int   integ;
      } *ap;
   int sp_incr;
   int r0save = 0;

   ap = &a;
   while ((*ap).integ != 0) {
      switch ((*ap++).integ) {
	 case C_PUSH:		/* push something on the stack */
	    fprintf(outfile, "\tpushl\t%s\n", ap[0].str);
	    ap++;
	    break;

	 case C_PUSHI:   	/* push an immediate on the stack */
	    fprintf(outfile, "\tpushl\t$0x%x\n", ap[0].integ);
	    ap++;
	    break;

	 case C_PUSHIN:		/* push an indexed value onto the stack */
	    fprintf(outfile, "\tpushab\t%d(%s)\n", ap[0].integ, ap[1].str);
	    ap += 2;
	    break;

	case C_PUSHN:		/* push a null descriptor on the stack */
	    fprintf(outfile, "\tclrq\t-(sp)\n");
	    break;

	 case C_PUSHZ:    	/* push a zero on the stack */
	    fprintf(outfile, "\tpushl\t$0\n");
	    break;

	 case C_PUSHR:		/* push registers */
	    fprintf(outfile, "\tpushr\t$0x%x\n", ap[0].integ);
	    ap++;
	    break;

	 case C_PUSHA:    	/* push an address on the stack */
	    fprintf(outfile, "\tpushal\t_%s+%d\n", ap[0].str, ap[1].integ);
	    ap += 2;
	    break;

	 case C_PUSHC:   	/* push the address of a constant */
	    fprintf(outfile, "\tpushl\t$C%dP%d\n",
			     ap[0].integ, ap[1].integ);
	    ap += 2;
	    break;

	 case C_PUSHL:    	/* push an label on the stack */
	    fprintf(outfile, "\tpushl\t$L%dF%d\n",
                             ap[0].integ, ap[1].integ);
	    ap += 2;
	    break;

	 case C_POP:		/* pop the stack into some location */
	    fprintf(outfile, "\tmovl\t(sp)+,%s\n", ap[0].str);
	    ap++;
	    break;

	 case C_MOV:		/* move one location to another */
	    fprintf(outfile, "\tmovl\t%s,%s\n", ap[0].str, ap[1].str);
	    ap += 2;
	    break;

	 case C_MOVQ:		/* move a quadword */
	    fprintf(outfile, "\tmovq\t%s,%s\n", ap[0].str, ap[1].str);
	    ap += 2;
	    break;

	 case C_MOVI:		/* move an immediate to some location */
	    fprintf(outfile, "\tmovl\t$%d,%s\n", ap[0].integ, ap[1].str);
	    ap += 2;
	    break;

	 case C_MOVL:		/* move a label to some location */
	    fprintf(outfile, "\tmovl\t$L%dF%d,%s\n",
  		    ap[0].integ, ap[1].integ, ap[2].str);
	    ap += 3;
	    break;

	 case C_MOVA:     	/* move an address to some location */
	    fprintf(outfile, "\tmoval\t_%s+%d,%s\n",
		    ap[0].str, ap[1].integ, ap[2].str);
	    ap += 3;
	    break;

	 case C_CLR:		/* clr a word */
	    fprintf(outfile, "\tclrl\t%s\n", ap[0].str);
	    ap++;
	    break;

	 case C_ADDTOP:		/* add an immediate to the top of stack */
	    fprintf(outfile, "\taddl\t$%d,(sp)\n", ap[0].integ);
	    ap++;
	    break;

	 case C_INIT:		/* test initial flag of procedure */
	    fprintf(outfile, "\tjbss\t$0,P%dI,L%dF%d\n",
		    ap[0].integ, ap[1].integ, ap[2].integ);
	    ap += 3;
	    break;

	 case C_CALLJ:		 /* jump to a subroutine */		
	  fprintf(outfile, "\tpushl\t$%d\n",ap[0].integ);
	  fprintf(outfile, "\tjmp\t_%s\n",ap[1].str);
	    ap += 2;
	    break;

	 case C_CALL:		/* call a subroutine */			
	  fprintf(outfile, "\tpushl\t$%d\n",ap[0].integ);
	  fprintf(outfile, "\tcalls\t$%d,_%s\n",1+2*ap[0].integ,ap[1].str);
	    ap += 2;
	    break;

	 case C_JUMP:		/* conditional jump to a local label */
	    fprintf(outfile, "\tj%s\tL%dF%d\n", ap[0].str,
		    ap[1].integ, ap[2].integ);
	    ap += 3;
	    break;

	 case C_LABEL:		/* place a local label */
	    fprintf(outfile, "L%dF%d:\n", ap[0].integ, ap[1].integ);
	    ap += 2;
	    break;

    	 case C_MISC:		/* miscellaneous */
	    fprintf(outfile, "\t%s\t%s\n", ap[0].str, ap[1].str);
	    ap += 2;
	    break;

	 default:
	    syserr("emit: illegal template number");
	 }
      }
   }

/*
 * emitproc - emit code for procedure block #n.
 */

emitproc(n, name, nargs, ndyn, nstat, fstat)
int n;
char *name;
int nargs, ndyn, nstat, fstat;
   {
   register int i;
   register char *p;

   fprintf(outfile, ".data\n");
   fprintf(outfile, "P%d:\n", n);
   fprintf(outfile, iformat, T_PROC); 	        /* type code */
   fprintf(outfile, iformat,                    /* size of block */
	   9*BYTES + 2*BYTES * (nargs+ndyn+nstat));
   fprintf(outfile, "\t.long\tI%d\n", n);	/* entry point */
   fprintf(outfile, iformat, nargs);      	/* # arguments */
   fprintf(outfile, iformat, ndyn);       	/* # dynamic locals */
   fprintf(outfile, iformat, nstat);           	/* # static locals */
   fprintf(outfile, iformat, fstat);     	/* first static */
   fprintf(outfile, nameformat,              /* qualifiers */
	   strlen(name), name - strings, name);
   for (i = 0; i <= nlocal; i++) {           /* names of arguments */
      if (ltable[i].l_flag & F_ARGUMENT) {
         p = ltable[i].l_name;
         fprintf(outfile, nameformat,
                 strlen(p), p - strings, p);
         }
      }
   for (i = 0; i <= nlocal; i++) {           /* names of dynamic locals */
      if (ltable[i].l_flag & F_DYNAMIC) {
         p = ltable[i].l_name;
         fprintf(outfile, nameformat,
                 strlen(p), p - strings, p);
         }
      }
   for (i = 0; i <= nlocal; i++) {           /* names of static locals */
      if (ltable[i].l_flag & F_STATIC) {
         p = ltable[i].l_name;
         fprintf(outfile, nameformat,
                 strlen(p), p - strings, p);
         }
      }
   fprintf(outfile, "P%dI:\t.long 0\n", n);  /* initial flag */
   fprintf(outfile, ".text\n");
   fprintf(outfile, "I%d:\n", n);
   }

/*
 * procend - emit constant table at end of procedure.
 */

procend()
   {
   register int i, k;
   register char *s;
   int csbuf[CSETSIZE];
   union {
      unsigned int i[4];
      long int l;
      double f;
      } x;

   fprintf(outfile, ".data\n");
   for (k = 0; k <= nconst; k++) {
      if (ctable[k].c_flag & F_REALLIT) {
         x.f = ctable[k].c_val.rval;
         fprintf(outfile, "C%dP%d:", k, nproc);
	 fprintf(outfile, iformat, T_REAL);
	 fprintf(outfile, iformat, x.i[0]);
	 fprintf(outfile, iformat, x.i[1]);
	 fprintf(outfile, iformat, x.i[2]);
	 fprintf(outfile, iformat, x.i[3]);
	 fprintf(outfile, "\t# %g\n", x.f);
         }
      else if (ctable[k].c_flag & F_LONGLIT) {
         x.l = ctable[k].c_val.ival;
         fprintf(outfile, "C%dP%d:", k, nproc);
	 fprintf(outfile, iformat, T_LONGINT);
	 fprintf(outfile, iformat, x.i[0]);
	 fprintf(outfile, iformat, x.i[1]);
	 fprintf(outfile, "\t# %ld\n", x.l);
         }
      else if (ctable[k].c_flag & F_CSETLIT) {
         for (i = 0; i < CSETSIZE; i++)
            csbuf[i] = 0;
         s = ctable[k].c_val.sval;
         i = ctable[k].c_length;
         while (i--) {
            setb(*s, csbuf);
            s++;
            }
         fprintf(outfile, "C%dP%d:", k, nproc);
	 fprintf(outfile, iformat, T_CSET);
         for (i = 0; i < CSETSIZE; i++)
            fprintf(outfile, iformat, csbuf[i]);
         fprintf(outfile, "\n");
         }
      }
   fprintf(outfile, ".text\n");
   }

/*
 * gentables - generate assembler code for global, static,
 * identifier, and record tables, and built-in procedure blocks.
 */

gentables()
   {
   register int i;
   register char *s;
   register struct gentry *gp;
   int j;
   struct fentry *fp;
   struct rentry *rp;

   /* output record constructor procedure blocks */
   for (gp = gtable; gp < gfree; gp++) {
      if (gp->g_flag & (F_RECORD & ~F_GLOBAL)) {
	 s = gp->g_name;
	 fprintf(outfile, ".data\n");
         fprintf(outfile, "R%d:", gp->g_procid);
         fprintf(outfile, iformat, T_PROC);
         fprintf(outfile, iformat, 9*BYTES);
         fprintf(outfile, "\t.long\t_mkrec+2\n");
         fprintf(outfile, iformat, gp->g_nargs);
         fprintf(outfile, iformat, -2);
         fprintf(outfile, iformat, gp->g_procid);
         fprintf(outfile, iformat, 0);
         fprintf(outfile, nameformat,
		 strlen(s), s - strings, s);
	 fprintf(outfile, ".text\n");
         }
      }

   /* output record/field tables */
   fprintf(outfile, ".data\n");
   j = ffree - ftable;
   for (fp = ftable; fp < ffree; fp++) {
      fprintf(outfile, "F%d:", fp->f_fid);
      rp = fp->f_rlist;
      for (i = 1; i <= nrecords; i++) {
         if (rp != NULL && rp->r_recid == i) {
            fprintf(outfile, iformat, rp->r_fnum);
            rp = rp->r_link;
            }
         else
            fprintf(outfile, iformat, -1);
         if (i == nrecords || (i & 03) == 0)
            fprintf(outfile, "\n");
         else
            fprintf(outfile, ";");
         }
      }
   fprintf(outfile, ".globl\t_ftab\n_ftab:\t.long %d\n", j);
   for (i = 1; i <= j; i++) {
      fprintf(outfile, "\t.long F%d", i);
      if (i == j || (i & 03) == 0)
         fprintf(outfile, "\n");
      else
         fprintf(outfile, ";");
      }

   /* output global declarations */
   fprintf(outfile, ".globl\t_globals\n");
   fprintf(outfile, ".globl\t_eglobals\n");
   fprintf(outfile, ".globl\t_gnames\n");
   fprintf(outfile, ".globl\t_egnames\n");
   fprintf(outfile, ".globl\t_statics\n");
   fprintf(outfile, ".globl\t_estatics\n");
   fprintf(outfile, ".globl\t_ident\n");
   fprintf(outfile, ".globl\t_eident\n");
   fprintf(outfile, ".globl\t_k_trace\n_k_trace:");
   fprintf(outfile, iformat, trace);

   /* output global variables */
   fprintf(outfile, "\n_globals:\n");
   for (gp = gtable; gp < gfree; gp++) {
      if (gp->g_flag & (F_BUILTIN & ~F_GLOBAL))
	 fprintf(outfile, "\t.long\t%d,_B%s\t# %s\n",
		 D_PROC, gp->g_name, gp->g_name);
      else if (gp->g_flag & (F_PROC & ~F_GLOBAL))
	 fprintf(outfile, "\t.long \t%d,P%d\t# %s\n",
		 D_PROC, gp->g_procid, gp->g_name);
      else if (gp->g_flag & (F_RECORD & ~F_GLOBAL))
	 fprintf(outfile, "\t.long\t%d,R%d\t# %s\n",
		 D_PROC, gp->g_procid, gp->g_name);
      else
	 fprintf(outfile, "\t.long\t0,0\t# %s\n", gp->g_name);
      }
   fprintf(outfile, "_eglobals:\n");

   /* output names of global variables */
   fprintf(outfile, "_gnames:\n");
   for (gp = gtable; gp < gfree; gp++)
      fprintf(outfile, nameformat,
	      strlen(gp->g_name), gp->g_name - strings, gp->g_name);
   fprintf(outfile, "_egnames:\n");

   /* output static variables */
   fprintf(outfile, "_statics:\n");
   for (i = statics; i > 0; i--)
      fprintf(outfile, "\t.long\t0,0\n");
   fprintf(outfile, "_estatics:\n");

   /* output identifier table */
   fprintf(outfile, "_ident:\n");
   for (s = strings; s < sfree;) {
      fprintf(outfile, "\t.byte\t0%03o", *s++);
      for (i = 7; i > 0; i--) {
	 if (s >= sfree)
	    break;
	 fprintf(outfile, ",0%03o", *s++);
	 }
      fprintf(outfile, "\n");
      }
   fprintf(outfile, ".align\t2\n");
   fprintf(outfile, "_eident:\n");
   }
#endif VAX
#ifdef PDP11
int nproc = 0;			/* number of procedures encountered */

/*
 * gencode - read .ucode file, resolve variable references,
 * and generate assembler code.
 */

gencode(n)
int n;
   {
   register int op, k, lab;
   int j, nargs, flags, implicit;
   char *id, *name, *procname;
   struct centry *cp;
   struct gentry *gp;
   struct fentry *fp;

   extern long getint();
   extern double getreal();
   extern char *getid(), *getstrlit();
   extern struct gentry *glocate();

   while ((op = getop(&name)) != EOF) {
      switch (op) {
         case OP_TOBY:		/* ternary operators */
            newline();
	    emit(C_CALL, 3, name, 0);
            break;
	 case OP_SECT:
            newline();
	    emit(C_PUSHZ, C_PUSHZ,
		 C_CALL, 4, name, 0);
            break;

         case OP_ASGN:          /* binary operators */
         case OP_CAT:
         case OP_DIFF:
         case OP_DIV:
         case OP_EQV:
         case OP_INTER:
	 case OP_LCONCAT:
         case OP_LEXEQ:
         case OP_LEXGE:
         case OP_LEXGT:
         case OP_LEXLE:
         case OP_LEXLT:
         case OP_LEXNE:
         case OP_MINUS:
	 case OP_MOD:
         case OP_MULT:
         case OP_NEQV:
         case OP_NUMEQ:
         case OP_NUMGE:
         case OP_NUMGT:
         case OP_NUMLE:
         case OP_NUMLT:
         case OP_NUMNE:
         case OP_PLUS:
         case OP_POWER:
         case OP_RASGN:
         case OP_UNIONCS:
	    newline();
	    emit(C_CALL, 2, name, 0);
	    break;
         case OP_RSWAP:
	 case OP_SUBSC:
         case OP_SWAP:
	    newline();
	    emit(C_PUSHZ, C_PUSHZ,
		 C_CALL, 3, name, 0);
	    break;

         case OP_COMPL:		/* unary operators */
         case OP_NEG:
         case OP_NONNULL:
         case OP_NULL:
         case OP_NUMBER:
         case OP_REFRESH:
	 case OP_SIZE:
	 case OP_VALUE:
	    newline();
	    emit(C_CALL, 1, name, 0);
	    break;
         case OP_BANG:		
	 case OP_RANDOM:
         case OP_TABMAT:
	    newline();
	    emit(C_PUSHZ, C_PUSHZ,
		 C_CALL, 2, name, 0);
	    break;

         case OP_BSCAN:
	    newline();
	    emit(C_PUSH, "_k_subject+2", C_PUSH, "_k_subject",
		 C_PUSH, "_k_pos", C_PUSHI, D_INTEGER,
		 C_CALL, 0, name, 0);
	    break;

         case OP_CCASE:
	    newline();
            emit(C_PUSHZ, C_PUSHZ,
	         C_PUSH, "4(r4)", C_PUSH, "2(r4)", 0);
	    break;

         case OP_CHFAIL:
	    lab = getlab();
            newline();
	    emit(C_MOVL, lab, n, "-4(r4)", 0);
	    break;

         case OP_COACT:
	    newline();
	    emit(C_CALL, 1, "coact", 0);
	    break;

	 case OP_COFAIL:
	    newline();
	    emit(C_CALL, 0, "cofail", 0);
	    break;

         case OP_CORET:
	    newline();
	    emit(C_CALL, 0, "coret", 0);
	    break;

         case OP_CREATE:
            lab = getlab();
            newline();
            emit(C_PUSHL, lab, n, C_PUSHI, D_INTEGER,
                 C_CALL, 0, "create", 0);
            break;

         case OP_CSET:
	    k = getdec();
	    newline();
	    cp = &ctable[k];
	    emit(C_PUSHC, k, nproc,
		 C_PUSHI, D_CSET, 0);
	    break;

	 case OP_DUP:
	    newline();
	    emit(C_PUSHZ, C_PUSHZ,
                 C_PUSH, "6(sp)", C_PUSH, "6(sp)", 0);
	    break;

	 case OP_EFAIL:
	    newline();
	    emit(C_CALL, 0, "efail", 0);
	    break;

	 case OP_ERET:
	    newline();
	    emit(C_POP, "r0", C_POP, "r1",
		 C_MOV, "-2(r4)", "r3",
		 C_MOV, "r4", "sp",
		 C_POP, "r4",
		 C_PUSH, "r1", C_PUSH, "r0", 0);
	    break;

         case OP_ESCAN:
	    newline();
    	    emit(C_CALL, 3, "escan", 0);
	    break;

         case OP_ESUSP:
	    newline();
    	    emit(C_CALL, 1, "esusp", 0);
	    break;

         case OP_FIELD:
	    id = getid();
	    newline();
	    fp = flocate(id);
	    if (fp == NULL) {
	       err(id, "invalid field name", 0);
	       break;
	       }
	    emit(C_PUSHI, fp->f_fid, C_PUSHI, D_INTEGER,
		 C_CALL, 2, "field", 0);
	    break;

	 case OP_FILE:
	    file = getid();
	    newline();
	    emit(C_MOVA, "ident", file - strings, "_file", 0);
	    break;

         case OP_GOTO:
	    lab = getlab();
	    newline();
	    emit(C_JUMP, "br", lab, n, 0);
	    break;

         case OP_INCRES:
	    newline();
	    emit(C_MOV, "_current+2", "r0",
                 C_MISC, "inc", "14.(r0)",
                 0);
	    break;

         case OP_INIT:
	    lab = getlab();
	    newline();
	    emit(C_INIT, nproc, lab, n, 0);
	    break;

         case OP_INT:
	    k = getdec();
	    newline();
	    cp = &ctable[k];
	    if (cp->c_flag & F_LONGLIT)
	       emit(C_PUSHC, k, nproc,
		    C_PUSHI, D_LONGINT, 0);
	    else
   	       emit(C_PUSHI, (int) cp->c_val.ival,
   		    C_PUSHI, D_INTEGER, 0);
	    break;

         case OP_INVOKE:
	    k = getdec();
	    newline();
	    emit(C_CALL, k, "invoke", 0);
	    break;

         case OP_KEYWD:
            k = getdec();
	    newline();
            emit(C_PUSHI, k, C_PUSHI, D_INTEGER,
		 C_CALL, 0, "keywd", 0);
	    break;

         case OP_LAB:
	    lab = getlab();
	    newline();
	    emit(C_LABEL, lab, n, 0);
	    break;

	 case OP_LIMIT:
	    newline();
	    emit(C_CALL, 0, "limit", 0);
	    break;

	 case OP_LINE:
	    line = getdec();
	    newline();
	    emit(C_MOVI, line, "_line", 0);
	    break;

         case OP_LLIST:
	    k = getdec();
	    newline();
	    emit(C_CALL, k, "llist", 0);
	    break;

	 case OP_LSUSP:
	    newline();
	    emit(C_CALL, 0, "lsusp", 0);
	    break;

         case OP_MARK:
            lab = getlab();
	    newline();
	    emit(C_PUSH, "r4",
		 C_MOV, "sp", "r4",
		 C_PUSH, "r3",
		 C_CLR, "r3", 0);
	    if (lab != 0)
	       emit(C_PUSHL, lab, n, 0);
	    else
	       emit(C_PUSHZ, 0);
	    break;

	 case OP_PFAIL:
	    newline();
	    emit(C_CALL, 0, "pfail", 0);
	    break;

         case OP_PNULL:
	    newline();
	    emit(C_PUSHZ, C_PUSHZ, 0);
	    break;

         case OP_POP:
	    newline();
	    emit(C_MISC, "cmp", "(sp)+,(sp)+", 0);
	    break;

         case OP_PRET:
	    newline();
	    emit(C_CALL, 1, "pret", 0);
	    break;

         case OP_PSUSP:
	    newline();
	    emit(C_CALL, 0, "psusp", 0);
	    break;

         case OP_PUSH1:
	    newline();
	    emit(C_PUSHI, 1, C_PUSHI, D_INTEGER, 0);
	    break;

         case OP_PUSHN1:
	    newline();
	    emit(C_PUSHI, -1, C_PUSHI, D_INTEGER, 0);
	    break;

         case OP_REAL:
	    k = getdec();
	    newline();
	    emit(C_PUSHC, k, nproc,
		 C_PUSHI, D_REAL, 0);
	    break;

	 case OP_SDUP:
	    newline();
	    emit(C_PUSH, "2(sp)", C_PUSH, "2(sp)", 0);
	    break;

         case OP_STR:
	    k = getdec();
	    newline();
	    id = ctable[k].c_val.sval;
	    emit(C_PUSHA, "ident", id - strings,
		 C_PUSHI, ctable[k].c_length, 0);
	    break;

	 case OP_UNMARK:
	    k = getdec();
	    newline();
	    while (k-- > 1)
	       emit(C_MOV, "(r4)", "r4", 0);
	    emit(C_MOV, "-2(r4)", "r3",
		 C_MOV, "r4", "sp",
		 C_POP, "r4", 0);
	    break;

         case OP_VAR:
	    k = getdec();
	    newline();
	    if ((flags = ltable[k].l_flag) & F_GLOBAL)
	       emit(C_PUSHA, "globals", 4*(ltable[k].l_val.global-gtable),
		    C_PUSHI, D_VAR, 0);
	    else if (flags & F_STATIC)
	       emit(C_PUSHA, "statics", 4*(ltable[k].l_val.staticid-1),
		    C_PUSHI, D_VAR, 0);
	    else if (flags & F_ARGUMENT)
	       emit(C_PUSH, "r5",
		    C_ADDTOP, 6 + 4*(nargs-ltable[k].l_val.offset),
		    C_PUSHI, D_VAR, 0);
	    else
	       emit(C_PUSH, "r5",
		    C_ADDTOP, -10 - 4*ltable[k].l_val.offset,
		    C_PUSHI, D_VAR, 0);
	    break;

         case OP_PROC:
	    locinit();
	    line = 0;
	    procname = getid();
	    newline();
	    gp = glocate(procname);
	    implicit = gp->g_flag & F_IMPERROR;
	    nargs = gp->g_nargs;
	    gp->g_procid = ++nproc;
	    break;

         case OP_LOCAL:
	    k = getdec();
	    flags = getoct();
	    id = getid();
	    putloc(k, id, flags, implicit, procname);
	    break;

         case OP_CON:
	    k = getdec();
	    flags = getoct();
	    if (flags & F_INTLIT)
	       putconst(k, flags, 0, getint());
	    else if (flags & F_REALLIT)
	       putconst(k, flags, 0, getreal());
	    else if (flags & F_STRLIT) {
	       j = getdec();
	       putconst(k, flags, j, getstrlit(j));
               }
	    else if (flags & F_CSETLIT) {
	       j = getdec();
	       putconst(k, flags, j, getstrlit(j));
	       }
	    else
	       fprintf(stderr, "gencode: illegal constant\n");
	    newline();
	    break;

	 case OP_DECLEND:
	    newline();
	    emitproc(nproc, procname, nargs, dynoff,
		     statics-static1, static1);
	    break;

         case OP_END:
	    newline();
	    procend();
	    break;

	 default:
	    fprintf(stderr, "gencode: illegal opcode(%d): %s\n", op, name);
	    newline();
	 }
      }
   }

/*
 * emit - emit assembler code to outfile.  This routine
 * takes a variable number of arguments.  Each group of
 * arguments begins with a template number, which selects
 * one of the fprintf statements below.  Following the
 * template number is zero or more parameters to that
 * template.
 */

emit(a)
int a;
   {
   register union {
      char *str;
      int   integ;
      } *ap;

   ap = &a;
   while ((*ap).integ != 0) {
      switch ((*ap++).integ) {
	 case C_PUSH:		/* push something on the stack */
	    fprintf(outfile, "\tmov\t%s,-(sp)\n", ap[0].str);
	    ap++;
	    break;

	 case C_PUSHI:   	/* push an immediate on the stack */
	    fprintf(outfile, "\tmov\t$%o,-(sp)\n", ap[0].integ);
	    ap++;
	    break;

	 case C_PUSHZ:    	/* push a zero on the stack */
	    fprintf(outfile, "\tclr\t-(sp)\n");
	    break;

	 case C_PUSHA:    	/* push an address on the stack */
	    fprintf(outfile, "\tmov\t$_%s+%o,-(sp)\n", ap[0].str, ap[1].integ);
	    ap += 2;
	    break;

	 case C_PUSHC:   	/* push the address of a constant */
	    fprintf(outfile, "\tmov\t$C%dP%d,-(sp)\n",
			     ap[0].integ, ap[1].integ);
	    ap += 2;
	    break;

	 case C_PUSHL:    	/* push an label on the stack */
	    fprintf(outfile, "\tmov\t$L%dF%d,-(sp)\n",
                             ap[0].integ, ap[1].integ);
	    ap += 2;
	    break;

	 case C_POP:		/* pop the stack into some location */
	    fprintf(outfile, "\tmov\t(sp)+,%s\n", ap[0].str);
	    ap++;
	    break;

	 case C_MOV:		/* move one location to another */
	    fprintf(outfile, "\tmov\t%s,%s\n", ap[0].str, ap[1].str);
	    ap += 2;
	    break;

	 case C_MOVI:		/* move an immediate to some location */
	    fprintf(outfile, "\tmov\t$%o,%s\n", ap[0].integ, ap[1].str);
	    ap += 2;
	    break;

	 case C_MOVL:		/* move a label to some location */
	    fprintf(outfile, "\tmov\t$L%dF%d,%s\n",
		    ap[0].integ, ap[1].integ, ap[2].str);
	    ap += 3;
	    break;

	 case C_MOVA:     	/* move an address to some location */
	    fprintf(outfile, "\tmov\t$_%s+%o,%s\n",
		    ap[0].str, ap[1].integ, ap[2].str);
	    ap += 3;
	    break;

	 case C_CLR:		/* clr a word */
	    fprintf(outfile, "\tclr\t%s\n", ap[0].str);
	    ap++;
	    break;

	 case C_ADDTOP:		/* add an immediate to the top of stack */
	    fprintf(outfile, "\tadd\t$%o,(sp)\n", ap[0].integ);
	    ap++;
	    break;

	 case C_INIT:		/* test initial flag of procedure */
	    fprintf(outfile, "\ttst\tP%dI\n", ap[0].integ);
	    fprintf(outfile, "\tjne\tL%dF%d\n", ap[1].integ, ap[2].integ);
	    fprintf(outfile, "\tinc\tP%dI\n", ap[0].integ);
	    ap += 3;
	    break;

	 case C_CALL:		/* call a subroutine */
	    if (ap[0].integ == 0)
	       fprintf(outfile, "\tclr\t-(sp)\n");
	    else
	       fprintf(outfile, "\tmov\t$%o,-(sp)\n", ap[0].integ);
	    fprintf(outfile, "\tjsr\tpc,_%s\n", ap[1].str);
	    ap += 2;
	    break;

	 case C_JUMP:		/* conditional jump to a local label */
	    fprintf(outfile, "\tj%s\tL%dF%d\n", ap[0].str,
		    ap[1].integ, ap[2].integ);
	    ap += 3;
	    break;

	 case C_LABEL:		/* place a local label */
	    fprintf(outfile, "L%dF%d:\n", ap[0].integ, ap[1].integ);
	    ap += 2;
	    break;

    	 case C_MISC:		/* miscellaneous */
	    fprintf(outfile, "\t%s\t%s\n", ap[0].str, ap[1].str);
	    ap += 2;
	    break;

	 default:
	    syserr("emit: illegal template number");
	 }
      }
   }

/*
 * emitproc - emit code for procedure block #n.
 */

emitproc(n, name, nargs, ndyn, nstat, fstat)
int n;
char *name;
int nargs, ndyn, nstat, fstat;
   {
   register int i;
   register char *p;

   fprintf(outfile, ".data\n");
   fprintf(outfile, "P%d:\n", n);
   fprintf(outfile, "\t%06o;", T_PROC); 	/* type code */
   fprintf(outfile, "\t%06o;",                  /* size of block */
	   18 + 4 * (nargs+ndyn+nstat));
   fprintf(outfile, "\tI%d\n", n);		/* entry point */
   fprintf(outfile, "\t%06o;", nargs);    	/* # arguments */
   fprintf(outfile, "\t%06o;", ndyn);     	/* # dynamic locals */
   fprintf(outfile, "\t%06o;", nstat);         	/* # static locals */
   fprintf(outfile, "\t%06o\n", fstat);  	/* first static */
   fprintf(outfile, "\t%06o;\t_ident+%o\t/ %s\n", /* qualifiers */
	   strlen(name), name - strings, name);
   for (i = 0; i <= nlocal; i++) {           /* names of arguments */
      if (ltable[i].l_flag & F_ARGUMENT) {
         p = ltable[i].l_name;
         fprintf(outfile, "\t%06o;\t_ident+%o\t/ %s\n",
                 strlen(p), p - strings, p);
         }
      }
   for (i = 0; i <= nlocal; i++) {           /* names of dynamic locals */
      if (ltable[i].l_flag & F_DYNAMIC) {
         p = ltable[i].l_name;
         fprintf(outfile, "\t%06o;\t_ident+%o\t/ %s\n",
                 strlen(p), p - strings, p);
         }
      }
   for (i = 0; i <= nlocal; i++) {           /* names of static locals */
      if (ltable[i].l_flag & F_STATIC) {
         p = ltable[i].l_name;
         fprintf(outfile, "\t%06o;\t_ident+%o\t/ %s\n",
                 strlen(p), p - strings, p);
         }
      }
   fprintf(outfile, "P%dI:\t0\n", n);	     /* initial flag */
   fprintf(outfile, ".text\n");
   fprintf(outfile, "I%d:\n", n);
   }

/*
 * procend - emit constant table at end of procedure.
 */

procend()
   {
   register int i, k;
   register char *s;
   int csbuf[CSETSIZE];
   union {
      unsigned int i[4];
      long int l;
      double f;
      } x;

   fprintf(outfile, ".data\n");
   for (k = 0; k <= nconst; k++) {
      if (ctable[k].c_flag & F_REALLIT) {
         x.f = ctable[k].c_val.rval;
         fprintf(outfile,
	         "C%dP%d:\t%06o;\t%06o;\t%06o;\t%06o;\t%06o\t/ %g\n",
   	         k, nproc, T_REAL, x.i[0], x.i[1], x.i[2], x.i[3], x.f);
         }
      else if (ctable[k].c_flag & F_LONGLIT) {
         x.l = ctable[k].c_val.ival;
         fprintf(outfile, "C%dP%d:\t%06o;\t%06o;\t%06o\t/ %ld\n",
   	         k, nproc, T_LONGINT, x.i[0], x.i[1], x.l);
         }
      else if (ctable[k].c_flag & F_CSETLIT) {
         for (i = 0; i < CSETSIZE; i++)
            csbuf[i] = 0;
         s = ctable[k].c_val.sval;
         i = ctable[k].c_length;
         while (i--) {
            setb(*s, csbuf);
            s++;
            }
         fprintf(outfile, "C%dP%d:\t%06o;", k, nproc, T_CSET);
         for (i = 0; i < CSETSIZE; i++)
            fprintf(outfile, "\t%06o;", csbuf[i]);
         fprintf(outfile, "\n");
         }
      }
   fprintf(outfile, ".text\n");
   }

/*
 * gentables - generate assembler code for global, static,
 * identifier, and record tables, and built-in procedure blocks.
 */

gentables()
   {
   register int i;
   register char *s;
   register struct gentry *gp;
   int j;
   struct fentry *fp;
   struct rentry *rp;

   /* output record constructor procedure blocks */
   for (gp = gtable; gp < gfree; gp++) {
      if (gp->g_flag & (F_RECORD & ~F_GLOBAL)) {
	 s = gp->g_name;
	 fprintf(outfile, ".data\n");
         fprintf(outfile, "R%d:", gp->g_procid);
         fprintf(outfile, "\t%06o;", T_PROC);
         fprintf(outfile, "\t%06o;", 18);
         fprintf(outfile, "\t_mkrec+4\n");
         fprintf(outfile, "\t%06o;", gp->g_nargs);
         fprintf(outfile, "\t%06o;", -2);
         fprintf(outfile, "\t%06o;", gp->g_procid);
         fprintf(outfile, "\t%06o\n", 0);
         fprintf(outfile, "\t%06o;\t_ident+%o\t/ %s\n",
		 strlen(s), s - strings, s);
	 fprintf(outfile, ".text\n");
         }
      }

   /* output record/field tables */
   fprintf(outfile, ".data\n");
   j = ffree - ftable;
   for (fp = ftable; fp < ffree; fp++) {
      fprintf(outfile, "F%d:", fp->f_fid);
      rp = fp->f_rlist;
      for (i = 1; i <= nrecords; i++) {
         if (rp != NULL && rp->r_recid == i) {
            fprintf(outfile, "\t%06o", rp->r_fnum);
            rp = rp->r_link;
            }
         else
            fprintf(outfile, "\t-1");
         if (i == nrecords || (i & 03) == 0)
            fprintf(outfile, "\n");
         else
            fprintf(outfile, ";");
         }
      }
   fprintf(outfile, ".globl\t_ftab\n_ftab:\t%06o\n", j);
   for (i = 1; i <= j; i++) {
      fprintf(outfile, "\tF%d", i);
      if (i == j || (i & 03) == 0)
         fprintf(outfile, "\n");
      else
         fprintf(outfile, ";");
      }

   /* output global declarations */
   fprintf(outfile, ".globl\t_globals,_eglobals\n");
   fprintf(outfile, ".globl\t_gnames,_egnames\n");
   fprintf(outfile, ".globl\t_statics,_estatics\n");
   fprintf(outfile, ".globl\t_ident,_eident\n");
   fprintf(outfile, ".globl\t_k_trace\n_k_trace: %o\n", trace);

   /* output global variables */
   fprintf(outfile, "_globals:\n");
   for (gp = gtable; gp < gfree; gp++) {
      if (gp->g_flag & (F_BUILTIN & ~F_GLOBAL))
	 fprintf(outfile, "\t%06o;\t_B%s\t/ %s\n",
		 D_PROC, gp->g_name, gp->g_name);
      else if (gp->g_flag & (F_PROC & ~F_GLOBAL))
	 fprintf(outfile, "\t%06o;\tP%d\t/ %s\n",
		 D_PROC, gp->g_procid, gp->g_name);
      else if (gp->g_flag & (F_RECORD & ~F_GLOBAL))
	 fprintf(outfile, "\t%06o;\tR%d\t/ %s\n",
		 D_PROC, gp->g_procid, gp->g_name);
      else
	 fprintf(outfile, "\t0;\t0\t/ %s\n", gp->g_name);
      }
   fprintf(outfile, "_eglobals:\n");

   /* output names of global variables */
   fprintf(outfile, "_gnames:\n");
   for (gp = gtable; gp < gfree; gp++)
      fprintf(outfile, "\t%06o;\t_ident+%o\t/ %s\n",
	      strlen(gp->g_name), gp->g_name - strings, gp->g_name);
   fprintf(outfile, "_egnames:\n");

   /* output static variables */
   fprintf(outfile, "_statics:\n");
   for (i = statics; i > 0; i--)
      fprintf(outfile, "\t0;\t0\n");
   fprintf(outfile, "_estatics:\n");

   /* output identifier table */
   fprintf(outfile, "_ident:\n");
   for (s = strings; s < sfree;) {
      fprintf(outfile, "\t.byte\t%03o", *s++);
      for (i = 7; i > 0; i--) {
	 if (s >= sfree)
	    break;
	 fprintf(outfile, ",%03o", *s++);
	 }
      fprintf(outfile, "\n");
      }
   fprintf(outfile, ".even\n");
   fprintf(outfile, "_eident:\n");
   }
#endif PDP11
