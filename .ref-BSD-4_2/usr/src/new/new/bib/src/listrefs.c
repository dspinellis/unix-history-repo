#ifndef lint
static char sccsid[] = "@(#)listrefs.c	2.2	9/23/83";
#endif not lint
/*
        Listrefs - list references for bib system

        Authored by: Tim Budd, University of Arizona, 1983.
                lookup routines written by gary levin 2/82

                version 7/4/83

        Various modifications suggested by:
                David Cherveny - Duke University Medical Center
                Phil Garrison - UC Berkeley
                M. J. Hawley - Yale University




                                                        */
# include <stdio.h>
# include <ctype.h>
# include "bib.h"
# include "streams.h"
# define MAXLIST 2000  /* maximum number of references that can be listed */
# define getch(c,fd) (c = getc(fd))

FILE *tfd;

FILE *rfd;                      /* reference file position */
char reffile[] = TMPREFFILE;    /* temporary file (see bib.h) */
long int refspos[MAXLIST];      /* references temporary file, seek positions */
long int rend = 1;              /* last used position in reference file */
int numrefs = -1;               /* number of references */
char *citestr[MAXLIST];         /* citation strings */
extern int sort;                /* see if things are to be sorted */
extern char bibfname[];
extern int biblineno;

main(argc, argv)
   int argc;
   char **argv;
{  char defult[120];
   int  i, rcomp();

   tfd = stdout;
   strcpy(defult, BMACLIB);
   strcat(defult,"/bib.list");
   mktemp(reffile);
   rfd = fopen(reffile,"w+");
   if (rfd == NULL)
      error("can't open temporary reference file");
   putc('x', rfd);      /* put garbage in first position */

   doargs(argc, argv, defult);

   if (sort)
      qsort(refspos, numrefs+1, sizeof(long), rcomp);
   makecites(citestr);
   disambiguate();

   for (i = 0; i <= numrefs; i++)
      dumpref(i, stdout);

   exit(0);
}

/* rdtext - process a file */
   rdtext(ifile)
   FILE *ifile;
{  char c, *p, rec[REFSIZE];
   int i;

   biblineno = 1;
   for (;;) {
      while (getch(c, ifile) == '\n')
         biblineno++;   /* skip leading newlines */
      if (c == EOF)
         return;

      p = rec;          /* read a reference */
      for (;;) {
         for (*p++ = c; getch(c, ifile) != '\n'; )
            if (c == EOF)
               error("ill formed reference file");
            else
               *p++ = c;
         if (getch(c, ifile) == '\n' || c == EOF) {
            biblineno++;
            *p++ = '\n';
            break;
            }
         if (c == '.' || c == '%')
            *p++ = '\n';
         else
            *p++ = ' ';
         }

      *p = 0;
      expand(rec);

      if (numrefs++ > MAXLIST)
         error("too many references");
      refspos[numrefs] = rend;
#ifdef READWRITE
      fixrfd( WRITE );          /* fix access mode of rfd, if nec. */
#else
      fseek(rfd, rend, 0);
#endif
      i = strlen(rec) + 1;
      fwrite(rec, 1, i, rfd);
      rend = rend + i;
      }
}

