/*
 * Routines for producing error messages.
 */

#include "itran.h"
#include "token.h"
#include "tree.h"
#include "lex.h"

struct errmsg {
   int	e_state;		/* parser state number */
   char *e_mesg;		/* message text */
   } errtab[] = {
#include "synerr.h"
    -1,  "syntax error"
   };

/*
 * yyerror produces syntax error messages.  tok is the offending token
 *  (yychar), lval is yylval, and state is the parser's state.
 *
 * errtab is searched for the state, if it is found, the associated
 *  message is produced; if the state isn't found, "syntax error"
 *  is produced.
 */
yyerror(tok, lval, state)
int tok, state;
nodeptr lval;
   {
   register struct errmsg *p;
   char *mapterm();

   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   if (tok == EOFX)   /* special case end of file */
      fprintf(stderr, "unexpected end of file\n");
   else {
      fprintf(stderr, "line %d: ", LINE(lval));
      if (COL(lval))
         fprintf(stderr, "\"%s\": ", mapterm(tok,lval));
      for (p = errtab; p->e_state != state && p->e_state >= 0; p++) ;
      fprintf(stderr, "%s\n", p->e_mesg);
      }
   fatalerrs++;
   nocode++;
   }
/*
 * err produces the error messages s1 and s2 (if non-null).  The
 *  current line number is found in tline.
 */
err(s1, s2)
char *s1, *s2;
   {
   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   fprintf(stderr, "line %d: ", tline);
   if (s2)
      fprintf(stderr, "\"%s\": ", s2);
   fprintf(stderr, "%s\n", s1);
   fatalerrs++;
   nocode++;
   }

/*
 * lerr produces the error message s and associates it with line l.
 */
lerr(l, s)
int l;
char *s;
   {
   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   fprintf(stderr, "line %d: ", l);
   fprintf(stderr, "%s\n", s);
   fatalerrs++;
   nocode++;
   }

/*
 * warn produces s1 and s2 (if non-null) as warning messages.  The current
 *  line is in tline.
 */
warn(s1, s2)
char *s1, *s2;
   {
   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   fprintf(stderr, "line %d: ", tline);
   if (s2)
      fprintf(stderr, "\"%s\": ", s2);
   fprintf(stderr, "%s\n", s1);
   warnings++;
   }

/*
 * syserr is called for fatal errors.  The message s is produced and the
 *  translator exits.
 */
syserr(s)
char *s;
   {
   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   fprintf(stderr, "line %d: %s\n", inline, s);
   exit(1);
   }

/*
 * mapterm finds a printable string for the given token type
 *  and value.
 */
char *mapterm(typ,val)
int typ;
nodeptr val;
   {
   register struct toktab *t;
   register i;

   i = typ;
   if (i == IDENT || i == INTLIT || i == REALLIT || i == STRINGLIT || i == CSETLIT)
      return (STR0(val));
   for (t = toktab; t->t_type != i; t++)
      if (t->t_type == 0)
         return ("???");
   return (t->t_word);
   }
