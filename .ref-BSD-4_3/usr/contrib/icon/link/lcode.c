/*
 * Routines to parse .u1 files and produce icode.
 */

#include "ilink.h"
#include "opcode.h"
#include "datatype.h"

static int pc = 0;		/* simulated program counter */

/*
 * gencode - read .u1 file, resolve variable references, and generate icode.
 *  Basic process is to read each line in the file and take some action
 *  as dictated by the opcode.  This action sometimes involves parsing
 *  of operands and usually culminates in the call of the appropriate
 *  emit* routine.
 *
 * Appendix C of the "tour" has a complete description of the intermediate
 *  language that gencode parses.
 */
gencode()
   {
   register int op, k, lab;
   int j, nargs, flags, implicit;
   char *id, *name, *procname;
   struct centry *cp;
   struct gentry *gp;
   struct fentry *fp, *flocate();

   extern long getint();
   extern double getreal();
   extern char *getid(), *getstrlit();
   extern struct gentry *glocate();

   while ((op = getop(&name)) != EOF) {
      switch (op) {

         /* Ternary operators. */

         case OP_TOBY:
         case OP_SECT:

         /* Binary operators. */

         case OP_ASGN:
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
         case OP_RSWAP:
         case OP_SUBSC:
         case OP_SWAP:
         case OP_UNIONCS:

         /* Unary operators. */

         case OP_BANG:
         case OP_COMPL:
         case OP_NEG:
         case OP_NONNULL:
         case OP_NULL:
         case OP_NUMBER:
         case OP_RANDOM:
         case OP_REFRESH:
         case OP_SIZE:
         case OP_TABMAT:
         case OP_VALUE:

         /* Instructions. */

         case OP_BSCAN:
         case OP_CCASE:
         case OP_COACT:
         case OP_COFAIL:
         case OP_CORET:
         case OP_DUP:
         case OP_EFAIL:
         case OP_ERET:
         case OP_ESCAN:
         case OP_ESUSP:
         case OP_INCRES:
         case OP_LIMIT:
         case OP_LSUSP:
         case OP_PFAIL:
         case OP_PNULL:
         case OP_POP:
         case OP_PRET:
         case OP_PSUSP:
         case OP_PUSH1:
         case OP_PUSHN1:
         case OP_SDUP:
            newline();
            emit(op, name);
            break;

         case OP_CHFAIL:
         case OP_CREATE:
         case OP_GOTO:
         case OP_INIT:
            lab = getlab();
            newline();
            emitl(op, lab, name);
            break;

         case OP_CSET:
         case OP_REAL:
            k = getdec();
            newline();
            emitr(op, ctable[k].c_pc, name);
            break;

         case OP_FIELD:
            id = getid();
            newline();
            fp = flocate(id);
            if (fp == NULL) {
               err(id, "invalid field name", 0);
               break;
               }
            emitn(op, fp->f_fid-1, name);
            break;

         case OP_FILE:
            file = getid();
            newline();
            emiti(op, file - strings, name);
            break;

         case OP_INT:
            k = getdec();
            newline();
            cp = &ctable[k];
            if (cp->c_flag & F_LONGLIT)
               emitr(OP_CON, cp->c_pc, name);
            else {
               int i;
               i = (int)cp->c_val.ival;
               if (i >= 0 && i < 16)
                  emit(OP_INTX+i, name);
               else
                  emitint(op, i, name);
                  }
            break;

         case OP_INVOKE:
            k = getdec();
            newline();
            abbrev(op, k, name, OP_INVKX, 8);
            break;

         case OP_KEYWD:
         case OP_LLIST:
            k = getdec();
            newline();
            emitn(op, k, name);
            break;

         case OP_LAB:
            lab = getlab();
            newline();
            if (Dflag)
               fprintf(dbgfile, "L%d:\n", lab);
            backpatch(lab);
            break;

         case OP_LINE:
            line = getdec();
            newline();
            abbrev(op, line, name, OP_LINEX, 64);
            break;

         case OP_MARK:
            lab = getlab();
            newline();
            if (lab != 0)
               emitl(op, lab, name);
            else
               emit(OP_MARK0, "mark0");
            break;

         case OP_STR:
            k = getdec();
            newline();
            cp = &ctable[k];
            id = cp->c_val.sval;
            emitin(op, id-strings, cp->c_length, name);
            break;

         case OP_UNMARK:
            k = getdec();
            newline();
            abbrev(op, k, name, OP_UNMKX, 8);
            break;

         case OP_VAR:
            k = getdec();
            newline();
            flags = ltable[k].l_flag;
            if (flags & F_GLOBAL)
               abbrev(OP_GLOBAL, ltable[k].l_val.global-gtable, "global",
                      OP_GLOBX, 16);
            else if (flags & F_STATIC)
               abbrev(OP_STATIC, ltable[k].l_val.staticid-1, "static",
                      OP_STATX, 8);
            else if (flags & F_ARGUMENT)
               abbrev(OP_ARG, nargs-ltable[k].l_val.offset, "arg",
                      OP_ARGX,  8);
            else
               abbrev(OP_LOCAL, ltable[k].l_val.offset-1, "local",
                      OP_LOCX, 16);
            break;

         /* Declarations. */

         case OP_PROC:
            procname = getid();
            newline();
            locinit();
            clearlab();
            line = 0;
            gp = glocate(procname);
            implicit = gp->g_flag & F_IMPERROR;
            nargs = gp->g_nargs;
            emiteven();
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
               putconst(k, flags, 0, pc, getint());
            else if (flags & F_REALLIT)
               putconst(k, flags, 0, pc, getreal());
            else if (flags & F_STRLIT) {
               j = getdec();
               putconst(k, flags, j, pc, getstrlit(j));
               }
            else if (flags & F_CSETLIT) {
               j = getdec();
               putconst(k, flags, j, pc, getstrlit(j));
               }
            else
               fprintf(stderr, "gencode: illegal constant\n");
            newline();
            emitcon(k);
            break;

         case OP_DECLEND:
            newline();
            gp->g_pc = pc;
            emitproc(procname, nargs, dynoff, statics-static1, static1);
            break;

         case OP_END:
            newline();
            flushcode();
            break;

         default:
            fprintf(stderr, "gencode: illegal opcode(%d): %s\n", op, name);
            newline();
         }
      }
   }

/*
 * abbrev - for certain opcodes with integer arguments that are small enough,
 * use an abbreviated opcode that includes the integer argument in it.
 */
abbrev(op, n, name, altop, limit)
int op, n;
char *name;
int altop, limit;
   {
   if (n >= 0 && n < limit)
      emit(altop+n, name);
   else
      emitn(op, n, name);
   }

/*
 *  emit - emit opcode.
 *  emitl - emit opcode with reference to program label, consult the "tour"
 *	for a description of the chaining and backpatching for labels.
 *  emitn - emit opcode with integer argument.
 *  emitr - emit opcode with pc-relative reference.
 *  emiti - emit opcode with reference to identifier table.
 *  emitin - emit opcode with reference to identifier table & integer argument.
 *  emitint - emit INT opcode with integer argument.
 *  emiteven - emit null bytes to bring pc to word boundary.
 *  emitcon - emit constant table entry.
 *  emitproc - emit procedure block.
 *
 * The emit* routines call out* routines to effect the "outputting" of icode.
 *  Note that the majority of the code for the emit* routines is for debugging
 *  purposes.
 */
emit(op, name)
int op;
char *name;
   {
   if (Dflag)
      fprintf(dbgfile, "%d:\t%d\t\t\t\t# %s\n", pc, op, name);
   outop(op);
   }

emitl(op, lab, name)
int op, lab;
char *name;
   {
   if (Dflag)
      fprintf(dbgfile, "%d:\t%d\tL%d\t\t\t# %s\n", pc, op, lab, name);
   if (lab >= maxlabels)
      syserr("too many labels in ucode");
   outop(op);
   if (labels[lab] <= 0) {		/* forward reference */
      outopnd(labels[lab]);
      labels[lab] = OPNDSIZE - pc;	/* add to front of reference chain */
      }
   else					/* output relative offset */
      outopnd(labels[lab] - (pc + OPNDSIZE));
   }

emitn(op, n, name)
int op, n;
char *name;
   {
   if (Dflag)
      fprintf(dbgfile, "%d:\t%d\t%d\t\t\t# %s\n", pc, op, n, name);
   outop(op);
   outopnd(n);
   }

emitr(op, loc, name)
int op, loc;
char *name;
   {
   loc -= pc + (OPSIZE + OPNDSIZE);
   if (Dflag) {
      if (loc >= 0)
         fprintf(dbgfile, "%d:\t%d\t*+%d\t\t\t# %s\n", pc, op, loc, name);
      else
         fprintf(dbgfile, "%d:\t%d\t*-%d\t\t\t# %s\n", pc, op, -loc, name);
      }
   outop(op);
   outopnd(loc);
   }

emiti(op, offset, name)
int op, offset;
char *name;
   {
   if (Dflag)
      fprintf(dbgfile, "%d:\t%d\tI+%d\t\t\t# %s\n", pc, op, offset, name);
   outop(op);
   outopnd(offset);
   }

emitin(op, offset, n, name)
int op, offset, n;
char *name;
   {
   if (Dflag)
      fprintf(dbgfile, "%d:\t%d\tI+%d,%d\t\t\t# %s\n", pc, op, offset, n, name);
   outop(op);
   outopnd(offset);
   outopnd(n);
   }
/*
 * emitint can have some pitfalls.  outword is used to output the
 *  integer and this is picked up in the interpreter as the second
 *  word of a short integer.  The integer value output must be
 *  the same size as what the interpreter expects.  See op_int and op_intx
 *  in interp.s
 */
emitint(op, i, name)
int op, i;
char *name;
   {
   if (Dflag)
        fprintf(dbgfile, "%d:\t%d\t%d\t\t\t# %s\n", pc, op, i, name);
   outop(op);
   outword(i); 
   }

emiteven()
   {
   while ((pc % WORDSIZE) != 0) {
      if (Dflag)
         fprintf(dbgfile, "%d:\t0\n", pc);
      outop(0);
      }
   }

emitcon(k)
register int k;
   {
   register int i;
   register char *s;
   int csbuf[CSETSIZE];
   union {
      char ovly[1];  /* Array used to overlay l and f on a bytewise basis. */
      long int l;
      double f;
      } x;

   if (ctable[k].c_flag & F_REALLIT) {
      x.f = ctable[k].c_val.rval;
      if (Dflag) {
         fprintf(dbgfile, "%d:\t%d", pc, T_REAL);
         dumpblock(x.ovly,sizeof(double));
         fprintf(dbgfile, "\t\t\t( %g )\n",x.f);
         }
      outword(T_REAL);
      outblock(x.ovly,sizeof(double));
      }
   else if (ctable[k].c_flag & F_LONGLIT) {
      x.l = ctable[k].c_val.ival;
      if (Dflag) {
         fprintf(dbgfile, "%d:\t%d", pc, T_LONGINT);
         dumpblock(x.ovly,sizeof(long));
         fprintf(dbgfile,"\t\t\t( %ld)\n",x.l);
         }
      outword(T_LONGINT);
      outblock(x.ovly,sizeof(long));
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
      if (Dflag)
         fprintf(dbgfile, "%d:\t%d", pc, T_CSET);
      outword(T_CSET);
      outblock(csbuf,sizeof(csbuf));
      if (Dflag)
         dumpblock(csbuf,CSETSIZE);
      }
   }

emitproc(name, nargs, ndyn, nstat, fstat)
char *name;
int nargs, ndyn, nstat, fstat;
   {
   register int i;
   register char *p;
   int size;
   /*
    * ProcBlockSize = sizeof(BasicProcBlock) + 
    *  sizeof(descrip)*(# of args + # of dynamics + # of statics).
    */
   size = (9*WORDSIZE) + (2*WORDSIZE) * (nargs+ndyn+nstat);
   
   if (Dflag) {
      fprintf(dbgfile, "%d:\t%d", pc, T_PROC);		/* type code */
      fprintf(dbgfile, "\t%d", size);			/* size of block */
      fprintf(dbgfile, "\tZ+%d\n", pc+size);		/* entry point */
      fprintf(dbgfile, "\t%d", nargs);			/* # of arguments */
      fprintf(dbgfile, "\t%d", ndyn);			/* # of dynamic locals */
      fprintf(dbgfile, "\t%d", nstat);			/* # of static locals */
      fprintf(dbgfile, "\t%d\n", fstat);		/* first static */
      fprintf(dbgfile, "\t%d\tI+%d\t\t\t# %s\n",	/* name of procedure */
         strlen(name), name-strings, name);
      }
   outword(T_PROC);
   outword(size);
   outword(pc + size - 2*WORDSIZE); /* Have to allow for the two words
                                     that we've already output. */
   outword(nargs);
   outword(ndyn);
   outword(nstat);
   outword(fstat);
   outword(strlen(name));
   outword(name - strings);

   /*
    * Output string descriptors for argument names by looping through
    *  all locals, and picking out those with F_ARGUMENT set.
    */
   for (i = 0; i <= nlocal; i++) {
      if (ltable[i].l_flag & F_ARGUMENT) {
         p = ltable[i].l_name;
         if (Dflag)
            fprintf(dbgfile, "\t%d\tI+%d\t\t\t# %s\n", strlen(p), p-strings, p);
         outword(strlen(p));
         outword(p - strings);
         }
      }

   /*
    * Output string descriptors for local variable names.
    */
   for (i = 0; i <= nlocal; i++) {
      if (ltable[i].l_flag & F_DYNAMIC) {
         p = ltable[i].l_name;
         if (Dflag)
            fprintf(dbgfile, "\t%d\tI+%d\t\t\t# %s\n", strlen(p), p-strings, p);
         outword(strlen(p));
         outword(p - strings);
         }
      }

   /*
    * Output string descriptors for local variable names.
    */
   for (i = 0; i <= nlocal; i++) {
      if (ltable[i].l_flag & F_STATIC) {
         p = ltable[i].l_name;
         if (Dflag)
            fprintf(dbgfile, "\t%d\tI+%d\t\t\t# %s\n", strlen(p), p-strings, p);
         outword(strlen(p));
         outword(p - strings);
         }
      }
   }

/*
 * gentables - generate interpreter code for global, static,
 *  identifier, and record tables, and built-in procedure blocks.
 */

gentables()
   {
   register int i;
   register char *s;
   register struct gentry *gp;
   struct fentry *fp;
   struct rentry *rp;
   struct header hdr;

   emiteven();

   /*
    * Output record constructor procedure blocks.
    */
   hdr.records = pc;
   if (Dflag)
      fprintf(dbgfile, "%d:\t%d\t\t\t\t# record blocks\n", pc, nrecords);
   outword(nrecords);
   for (gp = gtable; gp < gfree; gp++) {
      if (gp->g_flag & (F_RECORD & ~F_GLOBAL)) {
         s = gp->g_name;
         gp->g_pc = pc;
         if (Dflag) {
            fprintf(dbgfile, "%d:", pc);
            fprintf(dbgfile, "\t%d", T_PROC);
            fprintf(dbgfile, "\t%d", RKBLKSIZE);
            fprintf(dbgfile, "\t_mkrec+4\n");
            fprintf(dbgfile, "\t%d", gp->g_nargs);
            fprintf(dbgfile, "\t-2");
            fprintf(dbgfile, "\t%d", gp->g_procid);
            fprintf(dbgfile, "\t0\n");
            fprintf(dbgfile, "\t%d\tI+%d\t\t\t# %s\n", strlen(s), s-strings, s);
            }
         outword(T_PROC);		/* type code */
         outword(RKBLKSIZE);		/* size of block */
         outword(0);			/* entry point (filled in by interp)*/
         outword(gp->g_nargs);		/* number of fields */
         outword(-2);			/* record constructor indicator */
         outword(gp->g_procid);		/* record id */
         outword(0);			/* not used */
         outword(strlen(s));		/* name of record */
         outword(s - strings);
         }
      }

   /*
    * Output record/field table.
    */
   hdr.ftab = pc;
   if (Dflag)
      fprintf(dbgfile, "%d:\t\t\t\t\t# record/field table\n", pc);
   for (fp = ftable; fp < ffree; fp++) {
      if (Dflag)
         fprintf(dbgfile, "%d:", pc);
      rp = fp->f_rlist;
      for (i = 1; i <= nrecords; i++) {
         if (rp != NULL && rp->r_recid == i) {
            if (Dflag)
               fprintf(dbgfile, "\t%d", rp->r_fnum);
            outword(rp->r_fnum);
            rp = rp->r_link;
            }
         else {
            if (Dflag)
               fprintf(dbgfile, "\t-1");
            outword(-1);
            }
         if (Dflag && (i == nrecords || (i & 03) == 0))
            putc('\n', dbgfile);
         }
      }

   /*
    * Output global variable descriptors.
    */
   hdr.globals = pc;
   for (gp = gtable; gp < gfree; gp++) {
      if (gp->g_flag & (F_BUILTIN & ~F_GLOBAL)) {	/* built-in procedure */
         if (Dflag)
            fprintf(dbgfile, "%d:\t%06o\t%d\t\t\t# %s\n",
               pc, D_PROC, -gp->g_procid, gp->g_name);
         outword(D_PROC);
         outword(-gp->g_procid);
         }
      else if (gp->g_flag & (F_PROC & ~F_GLOBAL)) {	/* Icon procedure */
         if (Dflag)
            fprintf(dbgfile, "%d:\t%06o\tZ+%d\t\t\t# %s\n",
               pc, D_PROC, gp->g_pc, gp->g_name);
         outword(D_PROC);
         outword(gp->g_pc);
         }
      else if (gp->g_flag & (F_RECORD & ~F_GLOBAL)) {	/* record constructor */
         if (Dflag)
            fprintf(dbgfile, "%d:\t%06o\tZ+%d\t\t\t# %s\n",
               pc, D_PROC, gp->g_pc, gp->g_name);
         outword(D_PROC);
         outword(gp->g_pc);
         }
      else {	/* global variable */
         if (Dflag)
            fprintf(dbgfile, "%d:\t0\t0\t\t\t# %s\n", pc, gp->g_name);
         outword(0);
         outword(0);
         }
      }

   /*
    * Output descriptors for global variable names.
    */
   hdr.gnames = pc;
   for (gp = gtable; gp < gfree; gp++) {
      if (Dflag)
         fprintf(dbgfile, "%d:\t%d\tI+%d\t\t\t# %s\n",
                 pc, strlen(gp->g_name), gp->g_name-strings, gp->g_name);
      outword(strlen(gp->g_name));
      outword(gp->g_name - strings);
      }

   /*
    * Output a null descriptor for each static variable.
    */
   hdr.statics = pc;
   for (i = statics; i > 0; i--) {
      if (Dflag)
         fprintf(dbgfile, "%d:\t0\t0\n", pc);
      outword(0);
      outword(0);
      }
   flushcode();

   /*
    * Output the identifier table.  Note that the call to write
    *  really does all the work.
    */
   hdr.ident = pc;
   if (Dflag) {
      for (s = strings; s < sfree; ) {
         fprintf(dbgfile, "%d:\t%03o", pc, *s++);
         for (i = 7; i > 0; i--) {
            if (s >= sfree)
               break;
            fprintf(dbgfile, " %03o", *s++);
            }
         putc('\n', dbgfile);
         }
      }
   write(fileno(outfile), strings, sfree - strings);
   pc += sfree - strings;

   /*
    * Output icode file header.
    */
   hdr.size = pc;
   hdr.trace = trace;
   if (Dflag) {
      fprintf(dbgfile, "size:    %d\n", hdr.size);
      fprintf(dbgfile, "trace:   %d\n", hdr.trace);
      fprintf(dbgfile, "records: %d\n", hdr.records);
      fprintf(dbgfile, "ftab:    %d\n", hdr.ftab);
      fprintf(dbgfile, "globals: %d\n", hdr.globals);
      fprintf(dbgfile, "gnames:  %d\n", hdr.gnames);
      fprintf(dbgfile, "statics: %d\n", hdr.statics);
      fprintf(dbgfile, "ident:   %d\n", hdr.ident);
      }
   fseek(outfile, (long)hdrloc, 0);
   write(fileno(outfile), &hdr, sizeof hdr);
   }

#define CodeCheck if (codep >= code + maxcode)\
                     syserr("out of code buffer space")
/*
 * outop(i) outputs the integer i as an interpreter opcode.  This
 *  assumes opcodes fit into a char.  If they don't, outop will
 *  need to look like outword and outopnd.
 */
outop(op)
int op;
   {
   CodeCheck;
   *codep++ = op;
   pc++;
   }
/*
 * outopnd(i) outputs i as an operand for an interpreter operation.
 *  OPNDSIZE bytes must be moved from &opnd[0] to &codep[0].
 */
outopnd(opnd)
int opnd;
   {
   int i;
   union {
        char *i;
        char c[OPNDSIZE];
        } u;

   CodeCheck;
   u.i = (char *) opnd;
   
   for (i = 0; i < OPNDSIZE; i++)
      codep[i] = u.c[i];

   codep += OPNDSIZE;
   pc += OPNDSIZE;
   }
/*
 * outword(i) outputs i as a word that is used by the runtime system
 *  WORDSIZE bytes must be moved from &word[0] to &codep[0].
 */
outword(word)
int word;
   {
   int i;
   union {
        char *i;
        char c[WORDSIZE];
        } u;

   CodeCheck;
   u.i = (char *) word;
   
   for (i = 0; i < WORDSIZE; i++)
      codep[i] = u.c[i];

   codep += WORDSIZE;
   pc += WORDSIZE;
   }
/*
 * outblock(a,i) output i bytes starting at address a.
 */
outblock(addr,count)
char *addr;
int count;
   {
   if (codep + count > code + maxcode)
      syserr("out of code buffer space");
   pc += count;
   while (count--)
      *codep++ = *addr++;
   }
/*
 * dumpblock(a,i) dump contents of i bytes at address a, used only
 *  in conjunction with -D.
 */
dumpblock(addr, count)
char *addr;
int count;
   {
   int i;
   for (i = 0; i < count; i++) {
      if ((i & 7) == 0)
         fprintf(dbgfile,"\n\t");
      fprintf(dbgfile," %03o",(unsigned)addr[i]);
      }
   putc('\n',dbgfile);
   }

/*
 * flushcode - write buffered code to the output file.
 */
flushcode()
   {
   if (codep > code)
      /*fwrite(code, 1, codep - code, outfile);*/
      write(fileno(outfile), code, codep - code);
   codep = code;
   }

/*
 * clearlab - clear label table to all zeroes.
 */
clearlab()
   {
   register int i;

   for (i = 0; i < maxlabels; i++)
      labels[i] = 0;
   }

/*
 * backpatch - fill in all forward references to lab.
 */
backpatch(lab)
int lab;
   {
   register int p, r;
#ifdef VAX
   register int *q;
#endif VAX
#ifdef PORT
   int *q;	/* BE SURE to properly declare q - this won't always work. */
   return;
#endif PORT
#ifdef PDP11
   register char *q;
#endif PDP11

   if (lab >= maxlabels)
      syserr("too many labels in ucode");
   p = labels[lab];
   if (p > 0)
      syserr("multiply defined label in ucode");
   while (p < 0) {		/* follow reference chain */
      r = pc - (OPNDSIZE - p);	/* compute relative offset */
#ifdef VAX
      q = (int *) (codep - (pc + p));	/* point to word with address */
      p = *q;			/* get next address on chain */
      *q = r;			/* fill in correct offset */
#endif VAX

#ifdef PORT
#endif PORT

#ifdef PDP11
      q = codep - (pc + p);	/* point to word with address */
      p = *q++ & 0377;		/* get next address on chain */
      p |= *q << 8;
      *q = r >> 8;		/* fill in correct offset */
      *--q = r;
#endif PDP11
      }
   labels[lab] = pc;
   }

/*
 * genheader - output the header line to the .u1 file.
 */
genheader()
   {
   fprintf(outfile,"%s",ixhdr);
   }
