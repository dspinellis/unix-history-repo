/*
   bib - bibliographic formatter
         timothy a. budd, 1/82
         lookup routines supplied by gary levin 2/82
         reworked several new features added, 11/82.
*/
# include <stdio.h>
# include <ctype.h>
# include "bib.h"

# define HUNTSIZE 512                /* maximum size of hunt string         */
# define MAXFIELD 250                /* maximum field length                */
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
   extern int hyphen, ordcite;
   extern char sortstr[], pfile[], citetemplate[];


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
   mktemp(tmpfile);
   tfd = fopen(tmpfile,"w");
   if (tfd == NULL)
      error("can't open temporary output file");

    /*
       pass1 - read files, looking for citations
               arguments are read by doargs (bibargs.c)
    */

   if (doargs(argc, argv, DEFSTYLE ) == 0)
      rdtext(stdin);

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

   lastc = 0;
   while (getch(c, fd) != EOF)
      if (c == '[' || c == '{')
         if (getch(d, fd) == '.') { /* found a reference */
            if (c == '{') { if (lastc) putc(lastc, tfd);}
            else
               if (lastc == ' ')       fputs("\\*([<", tfd);
               else if (lastc == '.')  fputs("\\*([.", tfd);
               else if (lastc == ',')  fputs("\\*([,", tfd);
               else if (lastc)         putc(lastc, tfd);
            rdcite(fd, c);
            if (c == '[')
               if (lastc == ' ')       fputs("\\*(>]", tfd);
               else if (lastc == '.')  fputs("\\*(.]", tfd);
               else if (lastc == ',')  fputs("\\*(,]", tfd);
            lastc = 0;
            }
         else {
            if (lastc) putc(lastc, tfd);
            ungetc(d, fd);
            lastc = c;
            }
      else {
         if (lastc) putc(lastc, tfd);
         lastc = c;
         }
   if (lastc) putc(lastc, tfd);
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
      fseek(rfd, rend, 0);
      i = strlen(rf) + 1;
      fwrite(rf, 1, i, rfd);
      rend = rend + i;
      return(refspos[numrefs]);
      }
   else {
      fprintf(stderr,"no reference matching %s\n", huntstr);
      return( (long) -1 );
      }
}

/* rdref - read text for an already cited reference */
   rdref(i, ref)
   long int  i;
   char ref[REFSIZE];
{
   ref[0] = 0;
   fseek(rfd, i, 0);
   fread(ref, 1, REFSIZE, rfd);
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
               fprintf(stderr,"multiple references match %s\n",huntstr);
            *(p+1) = 0;
            break;
            }
         else if (*(p+1) != '%' && *(p+1) != '.') /* unnecessary newline */
            *p = ' ';
   return(r);
}

/* rcomp - reference comparison routine for qsort utility */
   int rcomp(ap, bp)
   long int *ap, *bp;
{  char ref1[REFSIZE], ref2[REFSIZE], field1[MAXFIELD], field2[MAXFIELD];
   char *p, *q, *getfield();
   int  neg, res;

   rdref(*ap, ref1);
   rdref(*bp, ref2);
   for (p = sortstr; *p; p = q) {
      if (*p == '-') {
         p++;
         neg = true;
         }
      else
         neg = false;
      q = getfield(p, field1, ref1);
      if (q == 0)
         res = 1;
      else if (getfield(p, field2, ref2) == 0)
         res = -1;
      else {
         if (*p == 'A') {
            if (isupper(field1[0]))
               field1[0] -= 'A' - 'a';
            if (isupper(field2[0]))
               field2[0] -= 'A' - 'a';
            }
         res = strcmp(field1, field2);
         }
      if (neg)
         res = - res;
      if (res != 0)
         break;
      }
   if (res == 0)
      if (ap < bp)
         res = -1;
      else
         res = 1;
   return(res);
}

/* makecites - make citation strings */
   makecites(citestr)
   char *citestr[];
{  char ref[REFSIZE], tempcite[100], *malloc();
   int  i;

   for (i = 0; i <= numrefs; i++) {
      rdref(refspos[i], ref);
      bldcite(tempcite, i, ref);
      citestr[i] = malloc(2 + strlen(tempcite)); /* leave room for disambig */
      if (citestr[i] == NULL)
         error("out of storage");
      strcpy(citestr[i], tempcite);
      }
}

/* bldcite - build a single citation string */
   bldcite(cp, i, ref)
   char *cp, ref[];
   int  i;
{  char *p, *q, c, *fp, *np, field[REFSIZE], temp[100], *getfield();
   int  j;

   getfield("F", field, ref);
   if (field[0] != 0)
      for (p = field; *p; p++)
         *cp++ = *p;
   else {
      p = citetemplate;
      field[0] = 0;
      while (c = *p++)
         if (isalpha(c)) {
            q = getfield(p-1, field, ref);
            if (q != 0) {
               p = q;
               for (fp = field; *fp; )
                  *cp++ = *fp++;
               }
            }
         else if (c == '1') {
            sprintf(field,"%d",1 + i);
            for (fp = field; *fp; )
               *cp++ = *fp++;
            }
         else if (c == '2') {
            if (getname(1, field, temp, ref)) {
               np = cp;
               fp = field;
               for (j = 1; j <= 3; j++)
                  if (*fp != 0)
                     *cp++ = *fp++;
               if (getname(2, field, temp, ref))
                  np[2] = field[0];
               if (getname(3, field, temp, ref)) {
                  np[1] = np[2];
                  np[2] = field[0];
                  }
               }
            }
         else if (c == '{') {
            while (*p ^= '}')
               if (*p == 0)
                  error("unexpected end of citation template");
               else
                  *cp++ = *p++;
            p++;
            }
         else if (c == '<') {
            while (*p ^= '>')
               if (*p == 0)
                  error("unexpected end of citation template");
               else
                  *cp++ = *p++;
            p++;
            }
         else if (c != '@')
            *cp++ = c;
      }
   *cp++ = 0;
}

/* getfield - get a single field from reference */
   char *getfield(ptr, field, ref)
   char *ptr, field[], ref[];
{  char *p, *q, temp[100];
   int  n, len, i, getname();

   field[0] = 0;
   if (*ptr == 'A')
      getname(1, field, temp, ref);
   else
      for (p = ref; *p; p++)
         if (*p == '%' && *(p+1) == *ptr) {
            for (p = p + 2; *p == ' '; p++)
               ;
            for (q = field; *p != '\n'; )
               *q++ = *p++;
            *q = 0;
            break;
            }
   n = 0;
   len = strlen(field);
   if (*++ptr == '-') {
      for (ptr++; isdigit(*ptr); ptr++)
         n = 10 * n + (*ptr - '0');
      if (n > len)
         n = 0;
      else
         n = len - n;
      for (i = 0; field[i] = field[i+n]; i++)
         ;
      }
   else if (isdigit(*ptr)) {
      for (; isdigit(*ptr); ptr++)
         n = 10 * n + (*ptr - '0');
      if (n > len)
         n = len;
      field[n] = 0;
      }

   if (*ptr == 'u') {
      ptr++;
      for (p = field; *p; p++)
         if (islower(*p))
            *p = (*p - 'a') + 'A';
      }
   else if (*ptr == 'l') {
      ptr++;
      for (p = field; *p; p++)
         if (isupper(*p))
            *p = (*p - 'A') + 'a';
      }
   return(ptr);
}

/* getname - get the nth name field from reference, breaking into
             first and last names */
   int getname(n, last, first, ref)
   int  n;
   char last[], first[], ref[];
{  char *p;

   for (p = ref; *p; p++)
      if (*p == '%' & *(p+1) == 'A') {
         n--;
         if (n == 0) {
            for (p = p + 2; *p == ' '; p++) ;
            breakname(p, first, last) ;
            return(true);
            }
         }
   return(false);
}

/* disambiguate - compare adjacent citation strings, and if equal, add
                  single character disambiguators */
   disambiguate()
{  int i, j;
   char adstr[2];

   for (i = 0; i < numrefs; i = j) {
      j = i + 1;
      if (strcmp(citestr[i], citestr[j])==0) {
         adstr[0] = 'a'; adstr[1] = 0;
         for (j = i+1; strcmp(citestr[i],citestr[j]) == 0; j++) {
            adstr[0] = 'a' + (j-i);
            strcat(citestr[j], adstr);
            if (j == numrefs)
               break;
            }
         adstr[0] = 'a';
         strcat(citestr[i], adstr);
         }
     }
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
      fprintf(stderr,"Warning: references never dumped\n");
}


/* dumpref - dump reference number i */
   dumpref(i, ofd)
   int i;
   FILE *ofd;
{  char ref[REFSIZE], *p, line[REFSIZE];
   int numauths, maxauths, numeds, maxeds;

   rdref(refspos[i], ref);
   maxauths = maxeds = 0;
   numauths = numeds = 0;
   for (p = ref; *p; p++)
      if (*p == '%')
         if (*(p+1) == 'A') maxauths++;
         else if (*(p+1) == 'E') maxeds++;
   fprintf(ofd, ".[-\n");
   fprintf(ofd, ".ds [F %s\n",citestr[i]);
   fseek(rfd, (long) refspos[i], 0);
   while (fgets(line, REFSIZE, rfd) != NULL) {
      if (line[0] == 0)        break;
      else if (line[0] == '.') fprintf(ofd,"%s",line);
      else {
         if (line[0] == '%') {
            for (p = &line[2]; *p == ' '; p++);
            if (line[1] == 'A')       numauths++;
            else if (line[1] == 'E')  numeds++;

            doline(line[1], p, numauths, maxauths, numeds, maxeds, ofd);
            }
         }
      }
   fprintf(ofd,".][\n");
}
