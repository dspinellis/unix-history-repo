/*
 * The lexical analyzer.
 */

#include "itran.h"
#include "token.h"
#include "lex.h"
#include "char.h"
#include "tree.h"

int tline;
int tcol;

/*
 * yylex - find the next token in the input stream, and return its token
 *  type and value to the parser.
 *
 * Variables of interest:
 *
 *  cc - character following last token.
 *  comflag - set if in a comment.
 *  nlflag - set if a newline was between the last token and the current token
 *  lastend - set if the last token was an ENDER.
 *  lastval - when a semicolon is inserted and returned, lastval gets the
 *   token value that would have been returned if the semicolon hadn't
 *   been inserted.
 */

yylex()
   {
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
      /*
       * A semicolon was inserted and returned on the last call to yylex,
       *  instead of going to the input, return lasttok and set the
       *  appropriate variables.
       */
      yylval = lastval;
      tline = LINE(lastval);
      tcol = COL(lastval);
      t = lasttok;
      goto ret;
      }
   nlflag = 0;
   comflag = 0;
loop:
   c = cc;
   /*
    * Skip whitespace and comments.
    */
   while (c != EOF && (comflag || c == COMMENT || isspace(c))) {
      if (c == '\n') {
         nlflag++;
         comflag = 0;
         }
      else if (c == COMMENT)
         comflag++;
      c = NEXTCHAR;
      }
   /*
    * A token is the next thing in the input.  Record the last line number
    *  and set tline and tcol to the current line and column.
    */
   lastline = tline;
   tline = inline;
   tcol = incol;

   if (c == EOF) {
      /*
       * End of file has been reached.  Set eofflag, return T_EOF, and
       *  set cc to EOF so that any subsequent scans also return T_EOF.
       */
      if (eofflag++) {
         eofflag = 0;
         cc = '\n';
         return (int) (yylval = 0);
         }
      cc = EOF;
      t = T_EOF;
      yylval = 0;
      goto ret;
      }

   /*
    * Look at current input character to determine what class of token
    *  is next and take the appropriate action.  Note that the various
    *  token gathering routines write a value into cc.
    */
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
   else {			/* gather longest legal operator */
      if ((t = getop(c, &cc)) == NULL)
         goto loop;
      yylval = OPNODE(t->t_type);
      }
   if (nlflag && lastend && (t->t_flags & BEGINNER)) {
      /*
       * A newline was encountered between the current token and the last,
       *  the last token was an ENDER, and the current token is a BEGINNER.
       *  Return a semicolon and save the current token in lastval.
       */
      lastval = yylval;
      lasttok = t;
      tline = lastline;
      tcol = 0;
      yylval = OPNODE(SEMICOL);
      return (SEMICOL);
      }
ret:
   /*
    * Clear lasttok, set lastend if the token being returned is an
    *  ENDER, and return the token.
    */
   lasttok = 0;
   lastend = t->t_flags & ENDER;
   return (t->t_type);
   }

/*
 * getident - gather an identifier beginning with ac.  The character
 *  following identifier goes in cc.
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
   /*
    * Copy characters into string space until a non-alphanumeric character
    *  is found.
    */
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
   /*
    * If the identifier is a reserved word, make a RESNODE for it and return
    *  the token value.  Otherwise, install it with putident, make an
    *  IDNODE for it, and return.
    */
   if ((t = findres()) != NULL) {
      yylval = RESNODE(t->t_type);
      return (t);
      }
   else {
      yylval = IDNODE((int)putident(p-sfree));
      return (T_IDENT);
      }
   }

/*
 * findres - if the string just copied into the string space by getident
 *  is a reserved word, return a pointer to its entry in the token table.
 *  Return NULL if the string isn't a reserved word.
 */

struct toktab *findres()
   {
   register struct toktab *t;
   register char c, *p;

   p = sfree;
   c = *p;
   if (!islower(c))
      return (NULL);
   /*
    * Point t at first reserved word that starts with c (if any).
    */
   if ((t = restab[c - '_']) == NULL)
      return (NULL);
   /*
    * Search through reserved words, stopping when a match is found
    *  or when the current reserved word doesn't start with c.
    */
   while (t->t_word[0] == c) {
      if (strcmp(t->t_word, p) == 0)
         return (t);
      t++;
      }
   return (NULL);
   }

/*
 * getnum - gather a numeric literal starting with ac and put the
 *  character following the literal into *cc.
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
         case 0:		/* integer part */
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
         case 1:		/* fractional part */
            if (isdigit(c))   continue;
            if (tolower(c) == 'e')   { state = 2; continue; }
            break;
         case 2:		/* optional exponent sign */
            if (c == '+' || c == '-') { state = 3; continue; }
         case 3:		/* first digit after e, e+, or e- */
            if (isdigit(c)) { state = 4; continue; }
            err("invalid real literal", 0);
            break;
         case 4:		/* remaining digits after e */
            if (isdigit(c))   continue;
            break;
         case 5:		/* first digit after r */
            if ((isdigit(c) || isletter(c)) && tonum(c) < r)
               { state = 6; continue; }
            err("invalid integer literal", 0);
            break;
         case 6:		/* remaining digits after r */
            if (isdigit(c) || isletter(c)) {
               if (tonum(c) >= r) {	/* illegal digit for radix r */
                  err("invalid digit in integer literal", 0);
                  r = tonum('z');	/* prevent more messages */
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
      yylval = REALNODE((int)putident(p-sfree));
      return (T_REAL);
      }
   yylval = INTNODE((int)putident(p-sfree));
   return (T_INT);
   }

/*
 * getstring - gather a string literal starting with ac and place the
 *  character following the literal in *cc.
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
   if (ac == '"') {	/* a string literal */
      yylval = STRNODE((int)putident(p-sfree), p-sfree);
      return (T_STRING);
      }
   else {		/* a cset literal */
      yylval = CSETNODE((int)putident(p-sfree), p-sfree);
      return (T_CSET);
      }
   }

/*
 * ctlesc - translate a control escape -- backslash followed by
 *  caret and one character.
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
 *  one, two, or three octal digits.
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
 *  followed by one or two hexadecimal digits.
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
 *  to its entry in the token table.  The tour describes the
 *  operator recognition process in detail.
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
            state = (struct optab *) state->o_val;
            c = ctran[NEXTCHAR];
            continue;
         case A_ERROR:
            err("invalid character", 0);
            *cc = ' ';
            return (NULL);
         case A_RETURN:
            *cc = c;
            return (struct toktab *) (state->o_val);
         case A_IMMRET:
            *cc = ' ';
            return (struct toktab *) (state->o_val);
         }
      }
   }

/*
 * nextchar - return the next character in the input.
 */

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
