#ifndef lint
static char sccsid[] = "@(#)bibargs.c	2.1	%G%";
#endif not lint

/*
        read argument strings for bib and listrefs
        do name formatting, printing lines, other actions common to both
                                                        */
# include <stdio.h>
# include <ctype.h>
# include "bib.h"
# define LINELENGTH 1024
# define MAXDEFS     500             /* maximum number of defined words */

/* global variables */
   int  abbrev       = false;   /* automatically abbreviate names            */
   int  capsmcap     = false;   /* print names in caps small caps (CACM form)*/
   int  numrev       = 0;       /* number of authors names to reverse        */
   int  edabbrev     = false;   /* abbreviate editors names ?                */
   int  edcapsmcap   = false;   /* print editors in cap small caps           */
   int  ednumrev     = 0;       /* number of editors to reverse              */
   int  sort         = false;   /* sort references ? (default no)            */
   int  foot         = false;   /* footnoted references ? (default endnotes) */
   int  hyphen       = false;   /* hypenate contiguous references            */
   int  ordcite      = true;    /* order multiple citations                  */
   char sortstr[80]  = "1";     /* sorting template                          */
   char trailstr[80] = "";      /* trailing characters to output             */
   char pfile[120];             /* private file name                         */
   int  personal = false;       /* personal file given ? (default no)        */
   char citetemplate[80] = "1"; /* citation template                         */
   char *words[MAXDEFS];        /* defined words                             */
   char *defs[MAXDEFS];         /* defined word definitions                  */
   int  wordtop = -1;           /* top of defined words array                */
   char *bibfname;
   char *biblineno;

/* where output goes */
   extern FILE *tfd;

/* doargs - read command argument line for both bib and listrefs
            set switch values
            call rdtext on file arguments, after dumping
            default style file if no alternative style is given
*/
   int doargs(argc, argv, defstyle)
   int argc;
   char **argv, defstyle[];
{  int numfiles, i, style;
   char *p, *q, *walloc();
   FILE *fd;

   numfiles = 0;
   style = true;
   words[0] = walloc("BMACLIB");
   defs[0]  = walloc(BMACLIB);
   wordtop++;
   fputs(".ds l] ",tfd);
   fputs(BMACLIB, tfd);
   fputs("\n", tfd);

   for (i = 1; i < argc; i++)
      if (argv[i][0] == '-')
         switch(argv[i][1]) {

            case 'a':  for (p = &argv[i][2]; *p; p++) /* author */
                          if (*p == 'a')
                             abbrev = true;
                           else if (*p == 'x')
                             capsmcap = true;
                           else if (*p == 'r') {
                             if (*(p+1))
                                numrev = atoi(p+1);
                              else
                                numrev = 1000;
                              break;
                              }
                       break;

            case 'c':  if (argv[i][2] == 0)
                          error("citation string expected");
                       else
                          for (p = citetemplate,q = &argv[i][2]; *p++ = *q++; );
                       break;

            case 'e':  for (p = &argv[i][2]; *p; p++)
                          if (*p == 'a')
                             edabbrev = true;
                           else if (*p == 'x')
                             edcapsmcap = true;
                           else if (*p == 'r') {
                             if (*(p+1))
                                ednumrev = atoi(p+1);
                              else
                                ednumrev = 1000;
                              break;
                              }
                       break;

            case 'f':  foot = true;
                       hyphen = false;
                       break;

            case 'h':  hyphen = ordcite = true;
                       break;

            case 'n':  for (p = &argv[i][2]; *p; p++)
                          if (*p == 'a')
                             abbrev = false;
                          else if (*p == 'f')
                             foot = false;
                          else if (*p == 'h')
                             hyphen = false;
                          else if (*p == 'o')
                             ordcite = false;
                          else if (*p == 'r')
                             numrev = 0;
                          else if (*p == 's')
                             sort = false;
                          else if (*p == 'x')
                             capsmcap = false;
                       break;

            case 'o':  ordcite = true;
                       break;

            case 'p':  if (argv[i][2])
                          p = &argv[i][2];
                       else {  /* take next arg */
                          i++;
                          p = argv[i];
                          }
                       strcpy(pfile, p);
                       personal = true;
                       break;

            case 'r':  if (argv[i][2] == 0)
                          numrev = 1000;
                       else
                          numrev = atoi(&argv[i][2]);
                       break;

            case 's':  sort = true;
                       if (argv[i][2])
                          for (p = sortstr,q = &argv[i][2]; *p++ = *q++; );
                       break;

            case 'i':
            case 't':  if (argv[i][1] == 't')
                          style = false;
                       if (argv[i][2])
                          p = &argv[i][2];
                       else { /* take next arg */
                          i++;
                          p = argv[i];
                          }
                       incfile(p);
                       break;

            case 'x':  capsmcap = true;
                       break;

            case 0:    if (style) {  /* no style command given, take default */
                          style = false;
                          incfile( defstyle );
                          }
		       bibfname = "<stdin>";
                       rdtext(stdin);
                       numfiles++;
                       break;

            default:   fputs(argv[i], stderr);
                       error(": invalid switch");
            }
      else { /* file name */
         numfiles++;
         if (style) {
            style = false;
            incfile( defstyle );
            }
         fd = fopen(argv[i], "r");
         if (fd == NULL) {
            fputs(argv[i], stderr);
            error(": can't open");
            }
         else {
            bibfname = argv[i];
            rdtext(fd);
            fclose(fd);
            }
         }

   if (style) incfile( defstyle );
   return(numfiles);

}

/* incfile - read in an included file  */
incfile(np)
   char *np;
{  char name[120];
   FILE *fd;

   fd = fopen(np, "r");
   if (fd == NULL && *np != '/') {
      strcpy(name, "bib.");
      strcat(name, np);
      fd = fopen(name, "r");
      }
   if (fd == NULL && *np != '/') {
      strcpy(name,BMACLIB);
      strcat(name, "/bib.");
      strcat(name, np);
      fd = fopen(name, "r");
      }
   if (fd == NULL) {
      fprintf(stderr,"%s", np);
      error(": can't open");
      }
   setswitch(fd);
   fclose(fd);
}

/* error - report unrecoverable error message */
  error(str)
  char str[];
{
  fputs(str, stderr);
  putc('\n', stderr);
  exit(1);
}

/* tfgets - fgets which trims off newline */
   char *tfgets(line, n, ptr)
   char line[];
   int  n;
   FILE *ptr;
{  char *p;

   p = fgets(line, n, ptr);
   if (p == NULL)
      return(NULL);
   else
      for (p = line; *p; p++)
         if (*p == '\n')
            *p = 0;
   return(line);
}

/* getwrd - place next word from in[i] into out */
int getwrd(in, i, out)
   char in[], out[];
   int i;
{  int j;

   j = 0;
   while (in[i] == ' ' || in[i] == '\n' || in[i] == '\t')
      i++;
   if (in[i])
      while (in[i] && in[i] != ' ' && in[i] != '\t' && in[i] != '\n')
         out[j++] = in[i++];
   else
      i = 0;    /* signals end of in[i..]   */
   out[j] = 0;
   return (i);
}

/* walloc - allocate enough space for a word */
char *walloc(word)
   char *word;
{  char *i, *malloc();
   i = malloc(1 + strlen(word));
   if (i == NULL)
      error("out of storage");
   strcpy(i, word);
   return(i);
}

/* setswitch - set document switch settings from format file */
   setswitch(fp)
   FILE *fp;
{  char *p, line[LINELENGTH], dline[LINELENGTH], word[80];
   int  i, j, getwrd();

   while (tfgets(line, LINELENGTH, fp) != NULL)
      switch(line[0]) {

         case '#': break;

         case 'A': for (p = &line[1]; *p; p++)
                      if (*p == 'A')
                         abbrev = true;
                      else if (*p == 'X')
                         capsmcap = true;
                      else if (*p == 'R') {
                         if (*(p+1))
                            numrev = atoi(p+1);
                         else
                            numrev = 1000;
                         break;
                         }
                   break;

         case 'C': for (p = &line[1]; *p == ' '; p++) ;
                   strcpy(citetemplate, p);
                   break;

         case 'D': if ((i = getwrd(line, 1, word)) == 0)
                      error("word expected in definition");
                   for (j = 0; j <= wordtop; j++)
                      if (strcmp(word, words[j]) == 0)
                         break;
                   if (j > wordtop) {
                      if ((j = ++wordtop) > MAXDEFS)
                         error("too many defintions");
                      words[wordtop] = walloc(word);
                      }
                   for (p = &line[i]; *p == ' '; p++) ;
                   for (strcpy(dline, p); dline[strlen(dline)-1] == '\\'; ){
                       dline[strlen(dline)-1] = '\n';
                       if (tfgets(line, LINELENGTH, fp) == NULL) break;
                       strcat(dline, line);
                       }
                   defs[j] = walloc(dline);
                   break;

         case 'E': for (p = &line[1]; *p; p++)
                      if (*p == 'A')
                         edabbrev = true;
                      else if (*p == 'X')
                         edcapsmcap = true;
                      else if (*p == 'R') {
                         if (*(p+1))
                            ednumrev = atoi(p+1);
                         else
                            ednumrev = 1000;
                         break;
                         }
                   break;

         case 'F': foot = true;
                   hyphen = false;
                   break;

         case 'I': for (p = &line[1]; *p == ' '; p++);
                   expand(p);
                   incfile(p);
                   break;

         case 'H': hyphen = ordcite = true;
                   break;

         case 'O': ordcite = true;
                   break;

         case 'R': if (line[1] == 0)
                      numrev = 1000;
                   else
                      numrev = atoi(&line[1]);
                   break;

         case 'S': sort = true;
                   for (p = &line[1]; *p == ' '; p++) ;
                   strcpy(sortstr, p);
                   break;

         case 'T': for (p = &line[1]; *p == ' '; p++) ;
                   strcpy(trailstr, p);
                   break;

         case 'X': capsmcap = true;
                   break;

         default:  fprintf(tfd,"%s\n",line);
                   while (fgets(line, LINELENGTH, fp) != NULL)
                      fputs(line, tfd);
                   return;
         }
   return;
}

/* isword - see if character is legit word char */
int iswordc(c)
char c;
{
   if (isalnum(c) || c == '&' || c == '_')
      return(true);
   return(false);
}

/* expand - expand reference, replacing defined words */
   expand(line)
   char *line;
{  char line2[REFSIZE], word[LINELENGTH], *p, *q, *w;
   int  replaced, i;

   replaced  = true;
   while (replaced) {
      replaced = false;
      p = line;
      q = line2;
      while (*p) {
         if (isalnum(*p)) {
            for (w = word; *p && iswordc(*p); )
               *w++ = *p++;
            *w = 0;
            for (i = 0; i <= wordtop; i++)
               if (strcmp(word, words[i]) == 0) {
                  strcpy(word, defs[i]);
                  replaced = true;
                  break;
                  }
            for (w = word; *w; )
               *q++ = *w++;
            }
         else
            *q++ = *p++;
         }
      *q = 0;
      p = line;
      q = line2;
      while (*p++ = *q++);
      }
}

/* breakname - break a name into first and last name */
   breakname(line, first, last)
   char line[], first[], last[];
{  char *p, *q, *r, *t, *f;

   for (t = line; *t != '\n'; t++);
   for (t--; isspace(*t); t--);

   /* now strip off last name */
   for (q = t; isspace(*q) == 0 || ((*q == ' ') & (*(q-1) == '\\')); q--)
      if (q == line)
         break;
   f = q;
   if (q != line)
      q++;

   for (; isspace(*f); f--);

   /* first name is start to f, last name is q to t */

   for (r = first, p = line, f++; p != f; )
      *r++ = *p++;
   *r = 0;
   for (r = last, p = q, t++; q != t; )
      *r++ = *q++;
   *r = 0;
}

/* match - see if string1 is a substring of string2 (case independent)*/
   int match(str1, str2)
   char str1[], str2[];
{  int  i, j;
   char a, b;

   for (i = 0; str2[i]; i++) {
      for (j = 0; str1[j]; j++) {
         if (isupper(a = str2[i+j]))
            a = (a - 'A') + 'a';
         if (isupper(b = str1[j]))
            b = (b - 'A') + 'a';
         if (a != b)
            break;
         }
      if (str1[j] == 0)
         return(true);
      }
   return(false);
}

/* scopy - append a copy of one string to another */
   char *scopy(p, q)
   char *p, *q;
{
   while (*p++ = *q++)
      ;
   return(--p);
}

/* bldname - build a name field
             doing abbreviations, reversals, and caps/small caps
*/
   bldname(first, last, name, reverse)
   char *first, *last, name[];
   int reverse;
{
   char newfirst[120], newlast[120], *p, *q, *f, *l, *scopy();
   int  flag;

   if (abbrev) {
      p = first;
      q = newfirst;
      flag = false;
      while (*p) {
         while (*p == ' ')
            p++;
         if (*p == 0)
            break;
         if (isupper(*p)) {
            if (flag)
               q = scopy(q, "\\*(a]");
            flag = true;
            *q++ = *p;
            *q++ = '.';
            }
         if (*++p == '.')
            p++;
         else while (*p != 0 && ! isspace(*p))
            p++;
         }
      *q = 0;
      f = newfirst;
      }
   else
      f = first;

   if (capsmcap) {
      p = last;
      q = newlast;
      flag = 0;  /* 1 - printing cap, 2 - printing small */
      while (*p)
         if (islower(*p)) {
            if (flag != 2)
               q = scopy(q, "\\s-2");
            flag = 2;
            *q++ = (*p++ - 'a') + 'A';
            }
         else {
            if (flag == 2)
               q = scopy(q,"\\s+2");
            flag = 1;
            *q++ = *p++;
            }
      if (flag == 2)
         q = scopy(q, "\\s+2");
      *q = 0;
      l = newlast;
      }
   else
      l = last;

   if (reverse)
      sprintf(name, "%s, %s\n", l, f);
   else
      sprintf(name, "%s %s\n", f, l);
}

/* prtauth - print author or editor field */
   prtauth(c, line, num, max, ofd, abbrev, capsmcap, numrev)
   char c, *line;
   int  num, max, abbrev, capsmcap, numrev;
   FILE *ofd;
{  char first[LINELENGTH], last[LINELENGTH];

   if (num <= numrev || abbrev || capsmcap) {
      breakname(line, first, last);
      bldname(first, last, line, num <= numrev);
      }
   if (num == 1)
      fprintf(ofd,".ds [%c %s", c, line);
   else if (num < max)
      fprintf(ofd,".as [%c \\*(c]%s", c, line);
   else if (max == 2)
      fprintf(ofd,".as [%c \\*(n]%s", c, line);
   else
      fprintf(ofd,".as [%c \\*(m]%s", c, line);
   if (num == max && index(trailstr, c))
      fprintf(ofd,".ds ]%c %c\n", c, line[strlen(line)-2]);
}

/* doline - actually print out a line of reference information */
   doline(c, line, numauths, maxauths, numeds, maxeds, ofd)
   char c, *line;
   int numauths, maxauths, numeds, maxeds;
   FILE *ofd;
{

   switch(c) {
      case 'A':
          prtauth(c, line, numauths, maxauths, ofd, abbrev, capsmcap, numrev);
          break;

       case 'E':
          prtauth(c, line, numeds, maxeds, ofd, edabbrev, edcapsmcap, ednumrev);
          if (numeds == maxeds)
             fprintf(ofd,".nr [E %d\n", maxeds);
          break;

       case 'P':
          if (index(line, '-'))
             fprintf(ofd,".nr [P 1\n");
          else
             fprintf(ofd,".nr [P 0\n");
          fprintf(ofd,".ds [P %s",line);
          if (index(trailstr, 'P'))
             fprintf(ofd,".ds ]P %c\n",line[strlen(line)-2]);
          break;

       case 'F':
       case 'K': break;

       default:
          fprintf(ofd,".ds [%c %s", c, line);
          if (index(trailstr, c))
             fprintf(ofd,".ds ]%c %c\n", c, line[strlen(line)-2]);
          }
}

