#include "ulink.h"
#include "opcode.h"

static int nlflag = 0;		/* newline last seen */

/*
 * getop - get an opcode from infile, and return the opcode number.
 */

getop(id)
char **id;
   {
   register char *s;
   register struct opentry *p;
   register int test;
   int low, high, cmp;
   extern char *getstr();

   s = getstr();
   if (s == NULL)
      return (EOF);
   low = 0;
   high = NOPCODES;
   do {
      test = (low + high) / 2;
      p = &optable[test];
      if ((cmp = strcmp(p->op_name, s)) < 0)
	 low = test + 1;
      else if (cmp > 0)
	 high = test;
      else {
	 *id = p->op_name;
	 return (p->op_code);
	 }
      } while (low < high);
   *id = s;
   return (NULL);
   }

/*
 * getid - get an identifier from infile, put it in the identifier
 * table, and return a pointer to it.
 */

char *getid()
   {
   register char *s;
   extern char *getstr();
   extern char *putident();

   s = getstr();
   if (s == NULL)
      return (NULL);
   return (putident(strlen(s)+1));
   }

/*
 * getstr - get an identifier from infile and return a pointer to it.
 */

char *getstr()
   {
   register int c;
   register char *p;

   p = sfree;
   while ((c = getc(infile)) == ' ' || c == '\t') ;
   if (c == EOF)
      return (NULL);
   while (c != ' ' && c != '\t' && c != '\n' && c != ',' && c != EOF) {
      *p++ = c;
      c = getc(infile);
      }
   *p = 0;
   nlflag = (c == '\n');
   return (sfree);
   }

/*
 * getdec - get a decimal integer from infile, and return it.
 */

getdec()
   {
   register int c, n;

   n = 0;
   while ((c = getc(infile)) == ' ' || c == '\t') ;
   if (c == EOF)
      return (0);
   while (c >= '0' && c <= '9') {
      n = n * 10 + (c - '0');
      c = getc(infile);
      }
   nlflag = (c == '\n');
   return (n);
   }

/*
 * getoct - get an octal number from infile, and return it.
 */

getoct()
   {
   register int c, n;

   n = 0;
   while ((c = getc(infile)) == ' ' || c == '\t') ;
   if (c == EOF)
      return (0);
   while (c >= '0' && c <= '7') {
      n = (n << 3) | (c - '0');
      c = getc(infile);
      }
   nlflag = (c == '\n');
   return (n);
   }

/*
 * getint - get an icon integer from infile, and return it.
 */

long getint()
   {
   register c;
   register int r;
   long int n;

   n = 0L;
   while ((c = getc(infile)) >= '0' && c <= '9')
      n = n * 10 + (c - '0');
   if (c == 'r' || c == 'R') {
      r = n;
      n = 0L;
      while (c = getc(infile)) {
	 if (c >= '0' && c <= '9')
	    c -= '0';
	 else if (c >= 'a' && c <= 'z')
	    c -= 'a' - 10;
	 else if (c >= 'A' && c <= 'Z')
	    c -= 'A' - 10;
	 else
	    break;
         n = n * r + c;
         }
      }
   nlflag = (c == '\n');
   return (n);
   }

/*
 * getreal - get an icon real number from infile, and return it.
 */

double getreal()
   {
   double n;
   register int c, d, e;
   int esign;
   static double tens[] = {
      1.0e0,  1.0e1,  1.0e2,  1.0e3,  1.0e4,  1.0e5,
      1.0e6,  1.0e7,  1.0e8,  1.0e9,  1.0e10, 1.0e11,
      1.0e12, 1.0e13, 1.0e14, 1.0e15, 1.0e16, 1.0e17,
      1.0e18, 1.0e19, 1.0e20, 1.0e21, 1.0e22, 1.0e23,
      1.0e24, 1.0e25, 1.0e26, 1.0e27, 1.0e28, 1.0e29,
      1.0e30, 1.0e31, 1.0e32, 1.0e33, 1.0e34, 1.0e35,
      1.0e36, 1.0e37, 1.0e38
      };
   static double ntens[] = {
      1.0e0,   1.0e-1,  1.0e-2,  1.0e-3,  1.0e-4,  1.0e-5,
      1.0e-6,  1.0e-7,  1.0e-8,  1.0e-9,  1.0e-10, 1.0e-11,
      1.0e-12, 1.0e-13, 1.0e-14, 1.0e-15, 1.0e-16, 1.0e-17,
      1.0e-18, 1.0e-19, 1.0e-20, 1.0e-21, 1.0e-22, 1.0e-23,
      1.0e-24, 1.0e-25, 1.0e-26, 1.0e-27, 1.0e-28, 1.0e-29,
      1.0e-30, 1.0e-31, 1.0e-32, 1.0e-33, 1.0e-34, 1.0e-35,
      1.0e-36, 1.0e-37, 1.0e-38
      };

   n = 0.0;
   d = e = 0;
   esign = 1;
   while ((c = getc(infile)) >= '0' && c <= '9')
      n = n * 10 + (c - '0');
   if (c == '.') {
      while ((c = getc(infile)) >= '0' && c <= '9') {
	 n = n * 10 + (c - '0');
	 d++;
	 }
      }
   if (c == 'e' || c == 'E') {
      if ((c = getc(infile)) == '+' || c == '-') {
  	 if (c == '-')
	    esign = -1;
	 c = getc(infile);
	 }
      while (c >= '0' && c <= '9') {
	 e = e * 10 + (c - '0');
	 c = getc(infile);
   	 }
      }
   if (esign < 0)
      e = -e;
   e = e - d;
   if (e >= -38 && e < 0)
      n = n * ntens[-e];
   else if (e > 0 && e <= 38)
      n = n * tens[e];
   nlflag = (c == '\n');
   return (n);
   }

/*
 * getlab - get a label ("L" followed by a number) from infile,
 * and return the number.
 */

getlab()
   {
   register int c;

   while ((c = getc(infile)) != 'L' && c != EOF && c != '\n') ;
   if (c == 'L')
      return (getdec());
   nlflag = (c == '\n');
   return (0);
   }

/*
 * getstrlit - get a string literal from infile, as a string
 * of octal bytes, and return it.
 */

char *getstrlit(l)
register int l;
   {
   register char *p;
   extern char *putident();

   p = sfree;
   while (!nlflag && l--)
      *p++ = getoct();
   *p++ = 0;
   return (putident(p-sfree));
   }

/*
 * newline - skip to next line.
 */

newline()
   {
   register int c;

   if (!nlflag) {
      while ((c = getc(infile)) != '\n' && c != EOF) ;
      }
   nlflag = 0;
   }
