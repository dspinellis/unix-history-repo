#include "utran.h"
#include "token.h"
#include "lex.h"
#include "char.h"
#include "tree.h"

int tline;
int tcol;

/*
 * yylex - finds the next token from the input stream,
 * and returns its token type and value to the parser.
 */

yylex() {
   register struct toktab *t;
   register int c;
   int nlflag;
   int comflag;
   static struct toktab *lasttok = NULL;
   static nodeptr lastval;
   static int lastend = 0;
   static int eofflag = 0;
   static int lastline = 0;
   static int cc = '\n';
   extern struct toktab *getident(), *getnum(), *getstring(), *getop();

   if (lasttok != NULL) {
      yylval = lastval;
      tline = LINE(lastval);
      tcol = COL(lastval);
      t = lasttok;
      goto ret;
      }
   nlflag = 0;
   comflag = 0;
loop:
   c = cc;                      /* skip spaces and comments */
   while (c != EOF && (comflag || c == COMMENT || isspace(c))) {
      if (c == '\n') {
         nlflag++;
         comflag = 0;
         }
      else if (c == COMMENT)
         comflag++;
      c = NEXTCHAR;
      }
   lastline = tline;
   tline = inline;
   tcol = incol;
   if (c == EOF) {                      /* end of file */
      if (eofflag++) {
         eofflag = 0;
         cc = '\n';
         return (yylval = 0);
         }
      cc = EOF;
      t = T_EOF;
      yylval = 0;
      goto ret;
      }
   c = ctran[c];
   if (isalpha(c)) {                    /* gather ident or reserved word */
      if ((t = getident(c, &cc)) == NULL)
         goto loop;
      }
   else if (isdigit(c)) {               /* gather numeric literal */
      if ((t = getnum(c, &cc)) == NULL)
         goto loop;
      }
   else if (c == '"' || c == '\'') {    /* gather string or cset literal */
      if ((t = getstring(c, &cc)) == NULL)
         goto loop;
      }
   else {                               /* gather longest legal operator */
      if ((t = getop(c, &cc)) == NULL)
         goto loop;
      yylval = OPNODE(t->t_type);
      }
   if (nlflag && lastend && (t->t_flags & BEGINNER)) { /* insert a semicolon */
      lastval = yylval;
      lasttok = t;
      tline = lastline;
      tcol = 0;
      yylval = OPNODE(SEMICOL);
      return (SEMICOL);
      }
ret:
   lasttok = 0;
   lastend = t->t_flags & ENDER;
   return (t->t_type);
   }

/*
 * getident - gather an identifier beginning with ac.
 * Character following identifier goes in cc.
 */

struct toktab *getident(ac, cc)
char ac;
int *cc;
   {
   register c;
   register char *p;
   register struct toktab *t;
   extern char *putident();
   extern struct toktab *findres();

   c = ac;
   p = sfree;
   do {
      if (p >= send)
	 syserr("out of string space");
      *p++ = c;
      c = ctran[NEXTCHAR];
      } while (isalnum(c));
   if (p >= send)
      syserr("out of string space");
   *p++ = 0;
   *cc = c;
   if ((t = findres()) != NULL) {
      yylval = RESNODE(t->t_type);
      return (t);
      }
   else {
      yylval = IDNODE(putident(p-sfree));
      return (T_IDENT);
      }
   }

/*
 * findres - find a reserved word and return a pointer to
 * its entry in the token table, or NULL if not reserved
 */

struct toktab *findres()
   {
   register struct toktab *t;
   register char c, *p;

   p = sfree;
   c = *p;
   if (!islower(c))
      return (NULL);
   if ((t = restab[c - '_']) == NULL)
      return (NULL);
   while (t->t_word[0] == c) {
      if (strcmp(t->t_word, p) == 0)
         return (t);
      t++;
      }
   return (NULL);
   }

/*
 * getnum - gather a numeric literal
 */

struct toktab *getnum(ac, cc)
char ac;
int *cc;
   {
   register c;
   register r;
   register state;
   char *p;
   int realflag;
   extern char *putident();

   c = ac;
   r = tonum(c);
   p = sfree;
   state = 0;
   realflag = 0;
   for (;;) {
      if (p >= send)
	 syserr("out of string space");
      *p++ = c;
      c = ctran[NEXTCHAR];
      switch (state) {
         case 0:        /* integer part */
            if (isdigit(c))         { r = r * 10 + tonum(c); continue; }
            if (c == '.')           { state = 1; realflag++; continue; }
            if (tolower(c) == 'e')  { state = 2; realflag++; continue; }
            if (tolower(c) == 'r')  {
               state = 5;
               if (r < 2 || r > 36)
                  err("invalid radix for integer literal", 0);
               continue;
               }
            break;
         case 1:        /* fractional part */
            if (isdigit(c))   continue;
            if (tolower(c) == 'e')   { state = 2; continue; }
            break;
         case 2:        /* optional exponent sign */
            if (c == '+' || c == '-') { state = 3; continue; }
         case 3:        /* first digit after e, e+, or e- */
            if (isdigit(c)) { state = 4; continue; }
            err("invalid real literal", 0);
            break;
         case 4:        /* remaining digits after e */
            if (isdigit(c))   continue;
            break;
         case 5:        /* first digit after r */
            if ((isdigit(c) || isletter(c)) && tonum(c) < r)
               { state = 6; continue; }
            err("invalid integer literal", 0);
            break;
         case 6:        /* remaining digits after r */
            if (isdigit(c) || isletter(c)) {
               if (tonum(c) >= r) {     /* illegal digit for radix r */
                  err("invalid digit in integer literal", 0);
                  r = tonum('z');       /* prevent more messages */
                  }
               continue;
               }
            break;
         }
      break;
      }
   if (p >= send)
      syserr("out of string space");
   *p++ = 0;
   *cc = c;
   if (realflag) {
      yylval = REALNODE(putident(p-sfree));
      return (T_REAL);
      }
   yylval = INTNODE(putident(p-sfree));
   return (T_INT);
   }

/*
 * getstring - gather a string literal, follow continuation
 * and escape conventions
 */

struct toktab *getstring(ac, cc)
char ac;
int *cc;
   {
   register c, sc;
   register char *p;
   char *lc;
   extern char *putident();

   sc = c = ac;
   p = sfree;
   lc = 0;
   while ((c = NEXTCHAR) != sc && c != '\n' && c != EOF) {
   contin:
      if (c == '_')
         lc = p;
      else if (!isspace(c))
         lc = 0;
      if (ctran[c] == ESCAPE) {
         c = NEXTCHAR;
         if (isoctal(c))
            c = octesc(c);
         else if (ctran[c] == 'x')
            c = hexesc();
         else if (ctran[c] == '^')
            c = ctlesc();
         else
            c = esctab[c];
         if (c == EOF)
            goto noquote;
         }
      if (p >= send)
         syserr("out of string space");
      *p++ = c;
      }
   if (p >= send)
      syserr("out of string space");
   *p++ = 0;
   if (c == sc)
      *cc = ' ';
   else {
      if (c == '\n' && lc) {
         p = lc;
         while ((c = NEXTCHAR) != EOF && isspace(c)) ;
         if (c != EOF)
            goto contin;
         }
noquote:
      err("unclosed quote", 0);
      *cc = c;
      }
   if (ac == '"') {                             /* have string */
      yylval = STRNODE(putident(p-sfree), p-sfree);
      return (T_STRING);
      }
   else {                                       /* have cset */
      yylval = CSETNODE(putident(p-sfree), p-sfree);
      return (T_CSET);
      }
   }

/*
 * ctlesc - translate a control escape -- backslash followed by
 * caret and one character.
 */

ctlesc()
   {
   register c;

   c = NEXTCHAR;
   if (c == EOF)
      return (EOF);
   return (c & 037);
   }

/*
 * octesc - translate an octal escape -- backslash followed by
 * one, two, or three octal digits
 */

octesc(ac)
char ac;
   {
   register c, nc, i;

   c = 0;
   nc = ac;
   i = 1;
   do {
      c = (c << 3) | (nc - '0');
      nc = NEXTCHAR;
      if (nc == EOF)
         return (EOF);
      } while (isoctal(nc) && i++ < 3);
   PUSHCHAR(nc);
   return (c & 0377);
   }

/*
 * hexesc - translate a hexadecimal escape -- backslash-x
 * followed by one or two hexadecimal digits
 */

hexesc()
   {
   register c, nc, i;

   c = 0;
   i = 0;
   while (i++ < 2) {
      nc = NEXTCHAR;
      if (nc == EOF)
         return (EOF);
      if (nc >= 'a' && nc <= 'f')
         nc -= 'a' - 10;
      else if (nc >= 'A' && nc <= 'F')
         nc -= 'A' - 10;
      else if (isdigit(nc))
         nc -= '0';
      else {
         PUSHCHAR(nc);
         break;
         }
      c = (c << 4) | nc;
      }
   return (c);
   }

/*
 * getop - find the longest legal operator and return a pointer
 * to its entry in the token table
 */

struct toktab *getop(ac, cc)
char ac;
int *cc;
   {
   register struct optab *state;
   register char c, i;

   state = state0;
   c = ac;
   for (;;) {
      while ((i = state->o_input) && c != i)
         state++;
      switch (state->o_action) {
         case A_GOTO:
            state = state->o_val;
            c = ctran[NEXTCHAR];
            continue;
         case A_ERROR:
            err("invalid character", 0);
            *cc = ' ';
            return (NULL);
         case A_RETURN:
            *cc = c;
            return (state->o_val);
         case A_IMMRET:
            *cc = ' ';
            return (state->o_val);
         }
      }
   }

nextchar()
   {
   register char c;

   if (c = peekc) {
      peekc = 0;
      return (c);
      }
   c = getc(infile);
   switch (c) {
      case EOF:
         inline = 0;
         incol = 0;
         break;
      case '\n':
         inline++;
         incol = 0;
         break;
      case '\t':
         incol = (incol | 7) + 1;
         break;
      case '\b':
         if (incol)
            incol--;
         break;
      default:
         incol++;
      }
   return (c);
   }
