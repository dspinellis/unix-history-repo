#ifndef lint
static char sccsid[] = "@(#)bib.c	2.2	9/23/83";
#endif not lint
/*
        Bib - bibliographic formatter

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

# define HUNTSIZE 512                /* maximum size of hunt string         */
# define MAXREFS  300                /* maximum number of references        */
# define MAXATONCE 35                /* maximum references at one location  */

# define getch(c,fd) (c = getc(fd))
# define echoc(c,ifd,ofd) (getch(c,ifd) == EOF ? c : putc(c,ofd))
# define testc(c,d,ifd,ofd) (getch(c, ifd) == d ? putc(c, ofd) : 0)

/* global variables */
   FILE *rfd;                   /* reference temporary file              */
   char reffile[] = TMPREFFILE ;/* temporary file (see bib.h)            */
   long int refspos[MAXREFS];   /* reference seek positions              */
   long int rend = 1;           /* last position in rfd (first char unused)*/
   int numrefs = -1;            /* number of references generated so far */
   FILE *tfd;                   /* output of pass 1 of file(s)           */
   char tmpfile[] = TMPTEXTFILE ; /* output of pass 1                    */
   char common[] = COMFILE ;    /* common word file                      */
   char *citestr[MAXREFS];      /* citation strings                      */
   int  findex = false;         /* can we read the file INDEX ?          */

/* global variables in bibargs */
   extern int foot, sort, personal;
   extern int hyphen, ordcite, biblineno;
   extern char sortstr[], pfile[], citetemplate[], bibfname[];


main(argc, argv)
   int argc;
   char **argv;
{  int rcomp();

   /* the file INDEX in the current directory is the default index,
      if it is present */

   rfd = fopen( INDXFILE , "r");
   if (rfd != NULL) {
      findex = true;
      fclose(rfd);
      }

   /* open temporaries, reffile will contain references collected in
      pass 1, and tmpfile will contain text.
   */
   mktemp(reffile);
   rfd = fopen(reffile,"w+");
   if (rfd == NULL)
      error("can't open temporary reference file");
   putc('x', rfd);      /* put garbage in first position (not used) */
   mktemp(tmpfile);
   tfd = fopen(tmpfile,"w");
   if (tfd == NULL)
      error("can't open temporary output file");

    /*
       pass1 - read files, looking for citations
               arguments are read by doargs (bibargs.c)
    */

   if (doargs(argc, argv, DEFSTYLE ) == 0) {
      strcpy(bibfname, "<stdin>");
      rdtext(stdin);
      }

   /*
    sort references, make citations, add disambiguating characters
   */

   if (sort)
      qsort(refspos, numrefs+1, sizeof(long), rcomp);
   makecites(citestr);
   disambiguate();

   /*
   reopen temporaries
   */

   fclose(tfd);
   tfd = fopen(tmpfile,"r");
   if (tfd == NULL)
      error("can't open temporary output file for reading");

   /*
   pass 2 - reread files, replacing references
   */

   pass2(tfd, stdout);

   /*
   clean up
   */

   fclose(tfd);
   fclose(rfd);
   unlink(tmpfile);
   unlink(reffile);
   exit(0);
}

/* rdtext - read and process a text file, looking for [. commands */
   rdtext(fd)
   FILE *fd;
{  char lastc, c, d;

   lastc = '\0';
   biblineno = 1;
   while (getch(c, fd) != EOF)
      if (c == '[' || c == '{')
         if (getch(d, fd) == '.') { /* found a reference */
            if (c == '{') { if (lastc) putc(lastc, tfd);}
            else
               switch (lastc) {
                  case '\0': break;
                  case ' ': fputs("\\*([<", tfd); break;
                  case '.': case ',': case '?': case ':':
                  case ';': case '!': case '"': case '\'':
                            fputs("\\*([", tfd);  /* fall through */
                  default:  putc(lastc, tfd); break;
                  }
            rdcite(fd, c);
            if (c == '[')
               switch (lastc) {
                  case '\0': break;
                  case ' ': fputs("\\*(>]", tfd); break;
                  case '.': case ',': case '?': case ':':
                  case ';': case '!': case '"': case '\'':
                            fprintf(tfd,"\\*(%c]", lastc); break;
                  }
            lastc = '\0';
            }
         else {
            if (lastc != '\0') putc(lastc, tfd);
            ungetc(d, fd);
            lastc = c;
            }
      else {
         if (lastc != '\0') putc(lastc, tfd);
         lastc = c;
         if (c == '\n') biblineno++;
         }
   if (lastc != '\0') putc(lastc, tfd);
}

/* rdcite - read citation information inside a [. command */
   rdcite(fd, ch)
   FILE *fd;
   char ch;
{  long int n, getref();
   char huntstr[HUNTSIZE], c, info[HUNTSIZE];

   if (ch == '[')
      fputs("\\*([[", tfd);
   else
      fputs("\\*([{", tfd);
   huntstr[0] = info[0] = 0;
   while (getch(c, fd) != EOF)
      switch (c) {
         case ',':
            n = getref(huntstr);
            if (n > 0)
               fprintf(tfd, "%c%ld%c%s%c", CITEMARK, n, CITEMARK, info, CITEEND);
            else
               fprintf(tfd, "%c0%c%s%s%c", CITEMARK, CITEMARK,
                                           huntstr, info, CITEEND);
            huntstr[0] = info[0] = 0;
            break;

         case '.':
            while (getch(c, fd) == '.') ;
            if (c == ']') {
               n = getref(huntstr);
               if (n > 0)
                  fprintf(tfd, "%c%ld%c%s%c\\*(]]", CITEMARK, n,
                                                  CITEMARK, info, CITEEND);
               else
                  fprintf(tfd, "%c0%c%s%s%c\\*(]]", CITEMARK, CITEMARK,
                                              huntstr, info, CITEEND);
               return;
               }
            else if (c == '}') {
               n = getref(huntstr);
               if (n > 0)
                  fprintf(tfd, "%c%ld%c%s%c\\*(}]", CITEMARK, n,
                                                    CITEMARK, info, CITEEND);
               else
                  fprintf(tfd, "%c0%c%s%s%c\\*(}]", CITEMARK, CITEMARK,
                                              huntstr, info, CITEEND);
               return;
               }
            else
               addc(huntstr, c);
            break;

         case '{':
            while (getch(c, fd) != '}')
               if (c == EOF) {
                  fprintf(stderr, "Error: ill formed reference\n");
                  exit(1);
                  }
                else
                  addc(info, c);
            break;

         case '\n':
            biblineno++;
         case '\t':
            c = ' ';   /* fall through */

         default:
            addc(huntstr,c);
         }
   error("end of file reading citation");
}

/* addc - add a character to hunt string */
   addc(huntstr, c)
   char huntstr[HUNTSIZE], c;
{  int  i;

   i = strlen(huntstr);
   if (i > HUNTSIZE)
      error("citation too long");
   huntstr[i] = c;
   huntstr[i+1] = 0;
}

/* getref - if an item was already referenced, return its pointer in
                the reference file, otherwise create a new entry */
   long int getref(huntstr)
   char huntstr[HUNTSIZE];
{  char rf[REFSIZE], ref[REFSIZE], *r, *hunt();
   int  i, match(), getwrd();

   r = hunt(huntstr);
   if (r != NULL) {
      /* exapand defined string */
      strcpy(rf, r);
      free(r);
      expand(rf);

      /* see if reference has already been cited */

      if (foot == false)
         for (i = 0; i <= numrefs; i++) {
             rdref(refspos[i], ref);
             if (strcmp(ref, rf) == 0)
                return(refspos[i]);
          }

      /* didn't match any existing reference, create new one */

      numrefs++;
      refspos[numrefs] = rend;
#ifdef READWRITE
      fixrfd( WRITE );                 /* fix access mode of rfd, if nec. */
#else
      fseek(rfd, rend, 0);             /* go to end of rfd */
#endif
      i = strlen(rf) + 1;
      fwrite(rf, 1, i, rfd);
      rend = rend + i;
      return(refspos[numrefs]);
      }
   else {
      bibwarning("no reference matching %s\n", huntstr);
      return( (long) -1 );
      }
}

/* hunt - hunt for reference from either personal or system index */
   char *hunt(huntstr)
   char huntstr[];
{  char *fhunt(), *r, *p, *q, fname[120];

   if (personal) {
      for (p = fname, q = pfile; ; q++)
         if (*q == ',' || *q == 0) {
            *p = 0;
            if ((r = fhunt(fname, huntstr)) != NULL)
               return(r);
            else if (*q == 0)
               break;
            p = fname;
            }
         else *p++ = *q;
      }
   else if (findex) {
      if ((r = fhunt( INDXFILE , huntstr)) != NULL)
         return(r);
      }
   if ((r = fhunt(SYSINDEX , huntstr)) != NULL)
      return(r);
   return(NULL);
}

/* fhunt - hunt from a specific file */
   char *fhunt(file, huntstr)
   char file[], huntstr[];
{  char *p, *r, *locate();

   r = locate(huntstr, file, 6, common);

   if (r == NULL)
      return(NULL);  /* error */
   if (*r == 0)
      return(NULL);  /* no match */

   for (p = r; *p; p++)
      if (*p == '\n')
         if (*(p+1) == '\n') { /* end */
            if (*(p+2) != 0)
               bibwarning("multiple references match %s\n",huntstr);
            *(p+1) = 0;
            break;
            }
         else if (*(p+1) != '%' && *(p+1) != '.') /* unnecessary newline */
            *p = ' ';
   return(r);
}

/* putrefs - gather contiguous references together, sort them if called
   for, hyphenate if necessary, and dump them out */
int putrefs(ifd, ofd, footrefs, fn)
FILE *ifd, *ofd;
int  fn, footrefs[];
{  int  citenums[MAXATONCE];   /* reference numbers */
   char *citeinfo[MAXATONCE];  /* reference information */
   char infoword[HUNTSIZE];    /* information line */
   int  rtop, n, i, j;         /* number of citations being dumped */
   char c, *p, *walloc();

/* first gather contiguous references together, and order them if
   required      */

   rtop = -1;
   do {
      n = 0;
      while (isdigit(getch(c, ifd)))
         n = 10 * n + (c - '0');
      if (c ^= CITEMARK)
         error("inconsistant citation found in pass two");
      if (n == 0) {     /* reference not found */
         rtop++;
         j = rtop;
         citenums[j] = -1;
         citeinfo[j] = 0;
         }
      else {
         for (i = 0; i <= numrefs; i++)
            if (refspos[i] == n) { /* its the ith item in reference list */
               rtop++;
               j = rtop;
               if (ordcite)
                  for ( ; j > 0 && citenums[j-1] > i; j--) {
                     citenums[j] = citenums[j-1];
                     citeinfo[j] = citeinfo[j-1];
                     }
               citenums[j] = i;
               citeinfo[j] = 0;
               break;
               }
         if (i > numrefs)
            error("citation not found in pass two");
         }
      if (getch(c, ifd) != CITEEND) {
         for (p = infoword; c != CITEEND ; ) {
            *p++ = c;
            getch(c, ifd);
            }
         *p = 0;
         citeinfo[j] = walloc(infoword);
         }
      getch(c, ifd);
      }  while (c == CITEMARK);
   ungetc(c, ifd);

   /* now dump out values */
   for (i = 0; i <= rtop; i++) {
      if (citenums[i] >= 0)
         fputs(citestr[citenums[i]], ofd);
      if (citeinfo[i]) {
         fputs(citeinfo[i], ofd);
         free(citeinfo[i]);
         }
      if (hyphen) {
         for (j = 1; j + i <= rtop && citenums[i+j] == citenums[i] + j; j++);
         if (j + i > rtop) j = rtop;
         else j = j + i - 1;
         }
      else
         j = i;
      if (j > i + 1) {
         fputs("\\*(]-", ofd);
         i = j - 1;
         }
      else if (i != rtop)
         fputs("\\*(],", ofd);
      if (foot) {
         fn++;
         footrefs[fn] = citenums[i];
         }
      }
   return(fn);
}

/* pass2 - read pass 1 files entering citation */
   pass2(ifd, ofd)
   FILE *ifd, *ofd;
{
   char c;
   int  i, fn, footrefs[25], dumped;

   fn = -1;
   dumped = foot;
   while (getch(c, ifd) != EOF) {
      while (c == '\n') {
         putc(c, ofd);
         if (foot && fn >= 0) {
            for (i = 0; i <= fn; i++)
                dumpref(footrefs[i], ofd);
            fn = -1;
            }
         if (testc(c, '.', ifd, ofd))
            if (testc(c, '[', ifd, ofd))
               if (testc(c, ']', ifd, ofd)) {
                  while (echoc(c, ifd, ofd) != '\n')
                     ;
                  dumped = true;
                  for (i = 0; i <= numrefs; i++)
                     dumpref(i, ofd);
                  getch(c, ifd);
                  }
         }
      if (c == CITEMARK)
         fn = putrefs(ifd, ofd, footrefs, fn);
      else if (c != EOF)
         putc(c, ofd);
      }
   if (dumped == false)
      bibwarning("Warning: references never dumped\n","");
}
