#include "ulink.h"

#define MAXNAME  40		/* maximum length of file name */

FILE *infile;			/* input file (.u1 or .u2) */
FILE *outfile;			/* assembler code output file */
char inbuf[BUFSIZ];		/* buffer for input file */
char outbuf[BUFSIZ];		/* buffer for output file */
char inname[MAXNAME];		/* input file name */
char outname[MAXNAME];		/* output file name */
char icnname[MAXNAME];		/* icon source file name */

int line = 0;			/* current source program line number */
char *file = NULL;              /* current source program file */
int fatalerrs = 0;		/* number of errors encountered */

char *pname;

extern char *filelist[];         /* list of input file names */
char **filep;

main(argc, argv)
int argc;
char **argv;
   {
   register int i;
   extern char *maknam();

   pname = argv[0];
   meminit(argc,argv);

   /* Phase I:	merge individual global symbol tables */

   filep = filelist;
   for (i = 1; *filep; i++, filep++) {
      maknam(inname, *filep, ".u2");
      maknam(icnname, *filep, ".icn");
      infile = fopen(inname, "r");
      if (infile == NULL) {
	 fprintf(stderr, "%s: cannot open %s\n", pname, inname);
	 exit(1);
	 }
      setbuf(infile, inbuf);
      globals(i);
      fclose(infile);
      }


   /* Phase II: resolve undeclared variables and generate code */

   maknam(outname, filelist[0], ".s");
   outfile = fopen(outname, "w");
   if (outfile == NULL) {
      fprintf(stderr, "%s: cannot create %s\n", pname, outname);
      exit(1);
      }
   setbuf(outfile, outbuf);

   filep = filelist;
   for (i = 1; *filep; i++, filep++) {
      maknam(inname, *filep, ".u1");
      maknam(icnname, *filep, ".icn");
      infile = fopen(inname, "r");
      if (infile == NULL) {
	 fprintf(stderr, "%s: cannot open %s\n", pname, inname);
	 exit(1);
	 }
      setbuf(infile, inbuf);
      gencode(i);
      fclose(infile);
      }
   gentables();

   if (fatalerrs > 0)
      exit(1);
   exit(0);
   }

/*
 * maknam - makes a file name from prefix and suffix.
 *
 * Uses only the last file specification if name is a path,
 * replaces suffix of name with suffix argument.
 */

char *maknam(dest, name, suffix)
char *dest, *name, *suffix;
   {
   register char *d, *pre, *suf;
   char *mark;

   d = dest;
   pre = name;
   suf = suffix;
   mark = pre;
   while (*pre) 		/* find last slash */
      if (*pre++ == '/')
	 mark = pre;
   pre = mark;
   mark = 0;
   while (*d = *pre++)		/* copy from last slash into dest */
      if (*d++ == '.')		/*   look for last dot, too */
	 mark = d - 1;
   if (mark)			/* if no dot, just append suffix */
      d = mark;
   while (*d++ = *suf++) ;	/* copy suffix into dest */
   return (dest);
   }

/*
 * syserr - issue error message and die.
 */

syserr(s)
char *s;
   {
   fprintf(stderr, "%s\n", s);
   exit(1);
   }

/*
 * warn - issue a warning message.
 */

warn(s1, s2, s3)
char *s1, *s2, *s3;
   {
   fprintf(stderr, "%s: ", icnname);
   if (line)
      fprintf(stderr, "%d: ", line);
   if (s1)
      fprintf(stderr, "\"%s\": ", s1);
   if (s2)
      fprintf(stderr, "%s", s2);
   if (s3)
      fprintf(stderr, "%s", s3);
   fprintf(stderr, "\n");
   }

/*
 * err - issue an error message.
 */

err(s1, s2, s3)
char *s1, *s2, *s3;
   {
   fprintf(stderr, "%s: ", icnname);
   if (line)
      fprintf(stderr, "%d: ", line);
   if (s1)
      fprintf(stderr, "\"%s\": ", s1);
   if (s2)
      fprintf(stderr, "%s", s2);
   if (s3)
      fprintf(stderr, "%s", s3);
   fprintf(stderr, "\n");
   fatalerrs++;
   }
