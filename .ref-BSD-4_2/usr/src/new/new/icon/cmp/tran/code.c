#include "../h/config.h"
#include "utran.h"
#include "token.h"
#include "tree.h"
#include "code.h"
#include "sym.h"

#ifdef INT
static int nextlab;		/* next label allocated by alclab() */
#endif INT

/*
 * node construction routines
 */

nodeptr tree1(type)
int type;
   {
   register nodeptr t;

   t = tfree;
   tfree = (int *)tfree + 1;
   if (tfree > tend)
      syserr("out of tree space");
   t->n_type = type;
   return (t);
   }

nodeptr tree3(type, a, b)
int type, a, b;
   {
   register nodeptr t;

   t = tfree;
   tfree = (int *)tfree + 3;
   if (tfree > tend)
      syserr("out of tree space");
   t->n_type = type;
   t->n_line = a;
   t->n_col = b;
   return (t);
   }

nodeptr tree4(type, a, b, c)
int type, a, b, c;
   {
   register nodeptr t;

   t = tfree;
   tfree = (int *)tfree + 4;
   if (tfree > tend)
      syserr("out of tree space");
   t->n_type = type;
   t->n_line = a;
   t->n_col = b;
   t->n_field[0].n_val = c;
   return (t);
   }

nodeptr tree5(type, a, b, c, d)
int type, a, b, c, d;
   {
   register nodeptr t;

   t = tfree;
   tfree = (int *)tfree + 5;
   if (tfree > tend)
      syserr("out of tree space");
   t->n_type = type;
   t->n_line = a;
   t->n_col = b;
   t->n_field[0].n_val = c;
   t->n_field[1].n_val = d;
   return (t);
   }

nodeptr tree6(type, a, b, c, d, e)
int type, a, b, c, d, e;
   {
   register nodeptr t;

   t = tfree;
   tfree = (int *)tfree + 6;
   if (tfree > tend)
      syserr("out of tree space");
   t->n_type = type;
   t->n_line = a;
   t->n_col = b;
   t->n_field[0].n_val = c;
   t->n_field[1].n_val = d;
   t->n_field[2].n_val = e;
   return (t);
   }

nodeptr tree7(type, a, b, c, d, e, f)
int type, a, b, c, d, e, f;
   {
   register nodeptr t;

   t = tfree;
   tfree = (int *)tfree + 7;
   if (tfree > tend)
      syserr("out of tree space");
   t->n_type = type;
   t->n_line = a;
   t->n_col = b;
   t->n_field[0].n_val = c;
   t->n_field[1].n_val = d;
   t->n_field[2].n_val = e;
   t->n_field[3].n_val = f;
   return (t);
   }

/*
 * clear tree space
 */

treeinit()
   {
   tfree = tree;
   }

/*
 * code generation
 */

codegen(t)
nodeptr t;
   {
#ifdef INT
   nextlab = 1;
#endif INT
   traverse(t);
   }

/*
 * tree traversal routine
 */

traverse(t)
register nodeptr t;
   {
   register int lab, n;
   struct loopstk loopsave;
   static char *name;
   static struct loopstk loopstk[LOOPDEPTH];	/* loop stack */
   static struct loopstk *loopsp;
   static struct casestk casestk[CASEDEPTH];	/* case stack */
   static struct casestk *casesp;
   static struct creatstk creatstk[CREATDEPTH]; /* create stack */
   static struct creatstk *creatsp;

   n = 1;
   switch (TYPE(t)) {

      case N_ACTIVAT:
         if (VAL0(TREE0(t)) == AUGACT)
            emit("pnull");
         traverse(TREE2(t));            /* evaluate result expression */
         if (VAL0(TREE0(t)) == AUGACT)
            emit("sdup");
         traverse(TREE1(t));            /* evaluate activate expression */
	 setline(LINE(t));
         emit("coact");
         if (VAL0(TREE0(t)) == AUGACT)
            emit("asgn");
         break;

      case N_ALT:
	 lab = alclab(2);
         emitl("mark", lab);
         loopsp->markcount++;
	 traverse(TREE0(t));      	/* evaluate first alternative */
         loopsp->markcount--;
         emit("esusp");			/*  and suspend with its result */
	 emitl("goto", lab+1);
	 emitlab(lab);
	 traverse(TREE1(t));	        /* evaluate second alternative */
	 emitlab(lab+1);
	 break;

      case N_AUGOP:
      case N_BINOP:
	 emit("pnull");
	 traverse(TREE1(t));
	 if (TYPE(t) == N_AUGOP)
   	    emit("dup");
	 traverse(TREE2(t));
	 setline(LINE(t));
	 binop(VAL0(TREE0(t)));
	 break;

      case N_BAR:
	 lab = alclab(1);
         emitlab(lab);
         emitl("mark", 0);		/* fail if expr fails first time */
         loopsp->markcount++;
	 traverse(TREE0(t));        	/* evaluate first alternative */
         loopsp->markcount--;
         emitl("chfail", lab);          /* change to loop on failure */
         emit("esusp");			/* suspend result */
	 break;

      case N_BREAK:
	 if (loopsp->breaklab <= 0)
	    lerr(LINE(t), "invalid context for break");
	 else {
	    emitn("unmark", loopsp->markcount);
            loopsave = *loopsp--;
            traverse(TREE0(t));
	    *++loopsp = loopsave;
	    emitl("goto", loopsp->breaklab);
            }
	 break;

      case N_CASE:
	 lab = alclab(1);
	 casesp++;
	 casesp->endlab = lab;
	 casesp->deftree = NULL;
	 emitl("mark", 0);
         loopsp->markcount++;
	 traverse(TREE0(t));	        /* evaluate control expression */
         loopsp->markcount--;
	 emit("eret");
	 traverse(TREE1(t));	        /* do rest of case (CLIST) */
	 if (casesp->deftree != NULL) { /* evaluate default clause */
	    emit("pop");
	    traverse(casesp->deftree);
	    }
	 else
	    emit("efail");
	 emitlab(lab);			/* end label */
	 casesp--;
	 break;

      case N_CCLS:
	 if (TYPE(TREE0(t)) == N_RES &&		/* default clause */
	     VAL0(TREE0(t)) == DEFAULT) {
	    if (casesp->deftree != NULL)
	       lerr(LINE(t), "more than one default clause");
	    else
	       casesp->deftree = TREE1(t);
	    }
	 else { 				/* case clause */
	    lab = alclab(1);
            emitl("mark", lab);
            loopsp->markcount++;
            emit("ccase");
            traverse(TREE0(t));             	/* evaluate selector */
   	    setline(LINE(t));
	    emit("eqv");
            loopsp->markcount--;
            emitn("unmark", 1);
	    emit("pop");
	    traverse(TREE1(t));      		/* evaluate expression */
	    emitl("goto", casesp->endlab);	/* goto end label */
	    emitlab(lab);   			/* label for next clause */
	    }
	 break;

      case N_CLIST:
	 traverse(TREE0(t));
	 traverse(TREE1(t));
	 break;

      case N_CONJ:
         if (VAL0(TREE0(t)) == AUGAND)
            emit("pnull");
	 traverse(TREE1(t));
         if (VAL0(TREE0(t)) != AUGAND)
            emit("pop");
	 traverse(TREE2(t));
         if (VAL0(TREE0(t)) == AUGAND)
            emit("asgn");
	 break;

      case N_CREATE:
         creatsp++;
         creatsp->nextlab = loopsp->nextlab;
         creatsp->breaklab = loopsp->breaklab;
         loopsp->nextlab = 0;		/* make break and next illegal */
         loopsp->breaklab = 0;
         lab = alclab(3);
         emitl("goto", lab+2);          /* skip over code for coexpression */
         emitlab(lab);                  /* entry point */
         emit("pop");                   /* pop the result from activation */
         emitl("mark", lab+1);
         loopsp->markcount++;
         traverse(TREE0(t));            /* traverse code for coexpression */
         loopsp->markcount--;
         emit("incres");                /* increment number of results */
	 setline(LINE(t));
         emit("coret");                 /* return to activator */
         emit("efail");                 /* drive coexpression */
         emitlab(lab+1);                /* loop on exhaustion */
	 setline(0);
	 setline(LINE(t));
         emit("cofail");                /* and fail each time */
         emitl("goto", lab+1);
         emitlab(lab+2);
	 setline(0);
	 setline(LINE(t));
         emitl("create", lab);          /* create entry block */
         loopsp->nextlab = creatsp->nextlab; /* legalize break and next */
         loopsp->breaklab = creatsp->breaklab;
         creatsp--;
         break;

      case N_CSET:
	 emitn("cset", VAL0(t));
	 break;

      case N_ELIST:
	 n = traverse(TREE0(t));
	 n += traverse(TREE1(t));
	 break;

      case N_EMPTY:
	 emit("pnull");
	 break;

      case N_FIELD:
	 emit("pnull");
	 traverse(TREE0(t));
	 setline(LINE(t));
	 emits("field", STR0(TREE1(t)));
	 break;

      case N_ID:
	 emitn("var", VAL0(t));
	 break;

      case N_IF:
	 if (TYPE(TREE2(t)) == N_EMPTY)
	    lab = 0;
	 else
	    lab = alclab(2);
         emitl("mark", lab);
         loopsp->markcount++;
	 traverse(TREE0(t));
         loopsp->markcount--;
         emitn("unmark", 1);
	 traverse(TREE1(t));
	 if (lab > 0) {
	    emitl("goto", lab+1);
	    emitlab(lab);
	    traverse(TREE2(t));
	    emitlab(lab+1);
	    }
	 break;

      case N_INT:
	 emitn("int", VAL0(t));
	 break;

      case N_INVOK:
         if (TYPE(TREE0(t)) != N_EMPTY)
   	    traverse(TREE0(t));
         else
            emit("pushn1");             /* assume -1(e1,...,en) */
	 n = traverse(TREE1(t));
	 setline(LINE(t));
	 emitn("invoke", n);
	 n = 1;
	 break;

      case N_KEY:
	 setline(LINE(t));
	 emitn("keywd", VAL0(t));
	 break;

      case N_LIMIT:
         traverse(TREE1(t));
	 setline(LINE(t));
         emit("limit");
         emitl("mark", 0);
         loopsp->markcount++;
	 traverse(TREE0(t));
         loopsp->markcount--;
         emit("lsusp");
         break;

      case N_LIST:
	 emit("pnull");
	 if (TYPE(TREE0(t)) == N_EMPTY)
	    n = 0;
	 else
   	    n = traverse(TREE0(t));
	 setline(LINE(t));
	 emitn("llist", n);
	 n = 1;
	 break;

      case N_LOOP:
	 switch (VAL0(TREE0(t))) {
	    case EVERY:
	       lab = alclab(2);
	       loopsp++;
               loopsp->ltype = EVERY;
	       loopsp->nextlab = lab;
               loopsp->breaklab = lab + 1;
	       loopsp->markcount = 1;
	       emitl("mark", 0);
	       traverse(TREE1(t));
	       emit("pop");
	       if (TYPE(TREE2(t)) != N_EMPTY) { /* every e1 do e2 */
   	       	  emitl("mark", 0);
                  loopsp->ltype = N_LOOP;
		  loopsp->markcount++;
	   	  traverse(TREE2(t));
		  loopsp->markcount--;
                  emitn("unmark", 1);
                  }
	       emitlab(loopsp->nextlab);
	       emit("efail");
	       emitlab(loopsp->breaklab);
	       loopsp--;
	       break;

	    case REPEAT:
	       lab = alclab(3);
	       loopsp++;
               loopsp->ltype = N_LOOP;
	       loopsp->nextlab = lab + 1;
	       loopsp->breaklab = lab + 2;
	       loopsp->markcount = 1;
	       emitlab(lab);
      	       setline(0);
      	       setline(LINE(t));
               emitl("mark", lab);
	       traverse(TREE1(t));
               emitlab(loopsp->nextlab);
	       emitn("unmark", 1);
	       emitl("goto", lab);
	       emitlab(loopsp->breaklab);
	       loopsp--;
	       break;

	    case WHILE:
	       lab = alclab(3);
	       loopsp++;
               loopsp->ltype = N_LOOP;
	       loopsp->nextlab = lab + 1;
               loopsp->breaklab = lab + 2;
	       loopsp->markcount = 1;
	       emitlab(lab);
      	       setline(0);
      	       setline(LINE(t));
               emitl("mark", 0);
	       traverse(TREE1(t));
               if (TYPE(TREE2(t)) != N_EMPTY) {
                  emitn("unmark", 1);
	          emitl("mark", lab);
	          traverse(TREE2(t));
		  }
	       emitlab(loopsp->nextlab);
               emitn("unmark", 1);
	       emitl("goto", lab);
	       emitlab(loopsp->breaklab);
	       loopsp--;
	       break;

	    case UNTIL:
	       lab = alclab(4);
	       loopsp++;
               loopsp->ltype = N_LOOP;
	       loopsp->nextlab = lab + 2;
               loopsp->breaklab = lab + 3;
	       loopsp->markcount = 1;
	       emitlab(lab);
      	       setline(0);
      	       setline(LINE(t));
               emitl("mark", lab+1);
	       traverse(TREE1(t));
               emitn("unmark", 1);
               emit("efail");
	       emitlab(lab+1);
	       emitl("mark", lab);
	       traverse(TREE2(t));
               emitlab(loopsp->nextlab);
	       emitn("unmark", 1);
	       emitl("goto", lab);
	       emitlab(loopsp->breaklab);
	       loopsp--;
	       break;
	    }
	 break;

      case N_NEXT:
	 if (loopsp < loopstk || loopsp->nextlab <= 0)
	    lerr(LINE(t), "invalid context for next");
	 else {
            if (loopsp->ltype != EVERY && loopsp->markcount > 1)
	       emitn("unmark", loopsp->markcount - 1);
	    emitl("goto", loopsp->nextlab);
	    }
	 break;

      case N_NOT:
         lab = alclab(1);
         emitl("mark", lab);
         loopsp->markcount++;
         traverse(TREE0(t));
         loopsp->markcount--;
         emitn("unmark", 1);
         emit("efail");
         emitlab(lab);
         emit("pnull");
	 break;

      case N_PROC:
	 loopsp = loopstk;
	 loopsp->nextlab = 0;
	 loopsp->breaklab = 0;
	 loopsp->markcount = 0;
	 casesp = casestk;
         creatsp = creatstk;
	 fprintf(codefile, "proc %s\n", STR0(TREE0(t)));
	 lout(codefile);
	 cout(codefile);
	 emit("declend");
	 emits("file", *filep);
	 setline(0);
         setline(LINE(t));
	 if (TYPE(TREE1(t)) != N_EMPTY) {
	    lab = alclab(1);
	    emitl("init?", lab);
            emitl("mark", lab);
	    traverse(TREE1(t));
            emitn("unmark", 1);
	    emitlab(lab);
	    }
	 if (TYPE(TREE2(t)) != N_EMPTY)
	    traverse(TREE2(t));
         setline(LINE(TREE3(t)));
         emit("pfail");
	 emit("end");
	 if (!silence)
            fprintf(stderr, "  %s (%d/%d)\n", STR0(TREE0(t)),
		(int *)tfree - (int *)tree, tsize);
	 break;

      case N_REAL:
	 emitn("real", VAL0(t));
	 break;

      case N_RET:
	 if (creatsp > creatstk)
	    lerr(LINE(t), "invalid context for return or fail");
         if (VAL0(TREE0(t)) != FAIL) {
	    lab = alclab(1);
	    emitl("mark", lab);
            loopsp->markcount++;
	    traverse(TREE1(t));
            loopsp->markcount--;
   	    setline(LINE(t));
	    emit("pret");
	    emitlab(lab);
	    }
	 setline(0);
	 setline(LINE(t));
	 emit("pfail");
	 break;

      case N_SCAN:
         if (VAL0(TREE0(t)) == SCANASGN)
   	    emit("pnull");
	 traverse(TREE1(t));
         if (VAL0(TREE0(t)) == SCANASGN)
   	    emit("sdup");
	 setline(LINE(t));
         emit("bscan");
	 traverse(TREE2(t));
	 setline(LINE(t));
         emit("escan");
         if (VAL0(TREE0(t)) == SCANASGN)
            emit("asgn");
	 break;

      case N_SECT:
	 emit("pnull");
	 traverse(TREE1(t));
	 traverse(TREE2(t));
	 if (VAL0(TREE0(t)) == PCOLON || VAL0(TREE0(t)) == MCOLON)
            emit("dup");
	 traverse(TREE3(t));
	 setline(LINE(TREE0(t)));
	 if (VAL0(TREE0(t)) == PCOLON)
	    emit("plus");
	 else if (VAL0(TREE0(t)) == MCOLON)
	    emit("minus");
	 setline(LINE(t));
	 emit("sect");
	 break;

      case N_SLIST:
	 lab = alclab(1);
         emitl("mark", lab);
         loopsp->markcount++;
	 traverse(TREE0(t));
         loopsp->markcount--;
         emitn("unmark", 1);
         emitlab(lab);
	 traverse(TREE1(t));
	 break;

      case N_STR:
	 emitn("str", VAL0(t));
	 break;

      case N_SUSP:
	 if (creatsp > creatstk)
	    lerr(LINE(t), "invalid context for suspend");
	 emitl("mark", 0);
         loopsp->markcount++;
	 traverse(TREE0(t));
         loopsp->markcount--;
	 setline(LINE(t));
	 emit("psusp");
	 emit("efail");
	 break;

      case N_TO:
	 emit("pnull");
	 traverse(TREE0(t));
	 traverse(TREE1(t));
	 emit("push1");
	 setline(LINE(t));
	 emit("toby");
	 break;

      case N_TOBY:
	 emit("pnull");
	 traverse(TREE0(t));
	 traverse(TREE1(t));
	 traverse(TREE2(t));
	 setline(LINE(t));
	 emit("toby");
	 break;

      case N_UNOP:
	 unopa(VAL0(TREE0(t)));
         traverse(TREE1(t));
	 setline(LINE(t));
	 unopb(VAL0(TREE0(t)));
	 break;

      default:
	 emitn("?????", TYPE(t));
	 syserr("traverse: undefined node type");
      }
   return (n);
   }

binop(op)
int op;
   {
   register int asgn;
   register char *name;

   asgn = 0;
   switch (op) {

      case ASSIGN:
	 name = "asgn";
	 break;

      case CARETASGN:
	 asgn++;
      case CARET:
	 name = "power";
	 break;

      case CONCATASGN:
	 asgn++;
      case CONCAT:
	 name = "cat";
	 break;

      case DIFFASGN:
	 asgn++;
      case DIFF:
	 name = "diff";
	 break;

      case AUGEQV:
	 asgn++;
      case EQUIV:
	 name = "eqv";
	 break;

      case INTERASGN:
	 asgn++;
      case INTER:
	 name = "inter";
	 break;

      case LBRACK:
	 name = "subsc";
	 break;

      case LCONCATASGN:
	 asgn++;
      case LCONCAT:
	 name = "lconcat";
	 break;

      case AUGSEQ:
	 asgn++;
      case LEXEQ:
	 name = "lexeq";
	 break;

      case AUGSGE:
	 asgn++;
      case LEXGE:
	 name = "lexge";
	 break;

      case AUGSGT:
	 asgn++;
      case LEXGT:
	 name = "lexgt";
	 break;

      case AUGSLE:
	 asgn++;
      case LEXLE:
	 name = "lexle";
	 break;

      case AUGSLT:
	 asgn++;
      case LEXLT:
	 name = "lexlt";
	 break;

      case AUGSNE:
	 asgn++;
      case LEXNE:
	 name = "lexne";
	 break;

      case MINUSASGN:
	 asgn++;
      case MINUS:
	 name = "minus";
	 break;

      case MODASGN:
	 asgn++;
      case MOD:
	 name = "mod";
	 break;

      case AUGNEQV:
	 asgn++;
      case NOTEQUIV:
	 name = "neqv";
	 break;

      case AUGEQ:
	 asgn++;
      case NUMEQ:
	 name = "numeq";
	 break;

      case AUGGE:
	 asgn++;
      case NUMGE:
	 name = "numge";
	 break;

      case AUGGT:
	 asgn++;
      case NUMGT:
	 name = "numgt";
	 break;

      case AUGLE:
	 asgn++;
      case NUMLE:
	 name = "numle";
	 break;

      case AUGLT:
	 asgn++;
      case NUMLT:
	 name = "numlt";
	 break;

      case AUGNE:
	 asgn++;
      case NUMNE:
	 name = "numne";
	 break;

      case PLUSASGN:
	 asgn++;
      case PLUS:
	 name = "plus";
	 break;

      case REVASSIGN:
	 name = "rasgn";
	 break;

      case REVSWAP:
	 name = "rswap";
	 break;

      case SLASHASGN:
	 asgn++;
      case SLASH:
	 name = "div";
	 break;

      case STARASGN:
	 asgn++;
      case STAR:
	 name = "mult";
	 break;

      case SWAP:
	 name = "swap";
	 break;

      case UNIONASGN:
	 asgn++;
      case UNION:
	 name = "unioncs";
	 break;

      default:
	 emitn("?binop", op);
	 syserr("binop: undefined binary operator");
      }
   emit(name);
   if (asgn)
      emit("asgn");
   return;
   }

unopa(op)
int op;
   {
   switch (op) {
      case NOTEQUIV:		/* unary ~ and three = operators */
	 emit("pnull");
      case LEXNE:		/* unary ~ and two = operators */
      case EQUIV:		/* three unary = operators */
	 emit("pnull");
      case NUMNE:		/* unary ~ and = operators */
      case UNION:               /* two unary + operators */
      case DIFF:                /* two unary - operators */
      case LEXEQ:		/* two unary = operators */
      case INTER:               /* two unary * operators */
	 emit("pnull");
      case DOT:			/* unary . operator */
      case BACKSLASH:		/* unary \ operator */
      case BANG:		/* unary ! operator */
      case CARET:               /* unary ^ operator */
      case PLUS:		/* unary + operator */
      case TILDE:		/* unary ~ operator */
      case MINUS:		/* unary - operator */
      case NUMEQ:		/* unary = operator */
      case STAR:		/* unary * operator */
      case QMARK: 		/* unary ? operator */
      case SLASH:	        /* unary / operator */
	 emit("pnull");
	 break;
      default:
	 syserr("unopa: undefined unary operator");
      }
   return;
   }

unopb(op)
int op;
   {
   register char *name;

   switch (op) {

      case DOT:			/* unary . operator */
	 name = "value";
	 break;

      case BACKSLASH:		/* unary \ operator */
	 name = "nonnull";
	 break;

      case BANG:		/* unary ! operator */
	 name = "bang";
	 break;

      case CARET:               /* unary ^ operator */
         name = "refresh";
         break;

      case UNION:               /* two unary + operators */
         unopb(PLUS);
      case PLUS:		/* unary + operator */
	 name = "number";
	 break;

      case NOTEQUIV:		/* unary ~ and three = operators */
	 unopb(NUMEQ);
      case LEXNE:		/* unary ~ and two = operators */
	 unopb(NUMEQ);
      case NUMNE:		/* unary ~ and = operators */
	 unopb(NUMEQ);
      case TILDE:		/* unary ~ operator (cset compl) */
	 name = "compl";
	 break;

      case DIFF:                /* two unary - operators */
         unopb(MINUS);
      case MINUS:		/* unary - operator */
	 name = "neg";
	 break;

      case EQUIV:		/* three unary = operators */
	 unopb(NUMEQ);
      case LEXEQ:		/* two unary = operators */
	 unopb(NUMEQ);
      case NUMEQ:		/* unary = operator */
	 name = "tabmat";
	 break;

      case INTER:               /* two unary * operators */
         unopb(STAR);
      case STAR:		/* unary * operator */
         name = "size";
         break;

      case QMARK: 		/* unary ? operator */
         name = "random";
         break;

      case SLASH:	        /* unary / operator */
         name = "null";
         break;

      default:
	 emitn("?unop", op);
	 syserr("unopb: undefined unary operator");
      }
   emit(name);
   return;
   }

setline(n)
int n;
   {
   static lastline = 0;

   if (n != lastline) {
      lastline = n;
      if (n > 0)
         emitn("line", n);
      }
   }

emitlab(l)
int l;
   {
   fprintf(codefile, "lab L%d\n", l);
   }

emit(s)
char *s;
   {
   fprintf(codefile, "\t%s\n", s);
   }

emitl(s, a)
char *s;
int a;
   {
   fprintf(codefile, "\t%s\tL%d\n", s, a);
   }

emitn(s, a)
char *s;
int a;
   {
   fprintf(codefile, "\t%s\t%d\n", s, a);
   }

emitnl(s, a, b)
char *s;
int a, b;
   {
   fprintf(codefile, "\t%s\t%d,L%d\n", s, a, b);
   }

emits(s, a)
char *s, *a;
   {
   fprintf(codefile, "\t%s\t%s\n", s, a);
   }

alclab(n)
int n;
   {
   register int lab;
#ifdef CMP
   static next = 1;
#endif CMP

#ifdef INT
   lab = nextlab;
   nextlab += n;
#endif INT
#ifdef CMP
   lab = next;
   next += n;
#endif CMP
   return (lab);
   }
