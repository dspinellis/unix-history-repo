#include "utran.h"
#include "token.h"
#include "tree.h"
#include "lex.h"

struct errmsg {
   int  e_state;
   char *e_mesg;
   } errtab[] = {
#include "err.h"
    -1,  "syntax error"
   };

yyerror(tok, lval, state)
int tok, state;
nodeptr lval;
   {
   register struct errmsg *p;

   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   if (tok == EOFX)
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

syserr(s)
char *s;
   {
   if (*filep)
      fprintf(stderr, "%s, ", *filep);
   fprintf(stderr, "line %d: %s\n", inline, s);
   exit(1);
   }

/*
 * mapterm - finds a printable string for the given token type
 * and value.
 */

mapterm(typ,val)
int typ;
nodeptr val;
   {
   register struct toktab *t;
   register i;

   i = typ;
   if (i == IDENT || i == INTLIT || i == REALLIT || i == STRINGLIT)
      return (STR0(val));
   for (t = toktab; t->t_type != i; t++)
      if (t->t_type == 0)
         return ("???");
   return (t->t_word);
   }
