/*
        list all documents in ref index file
                                                        */
# include <stdio.h>
# include <ctype.h>
# include "bib.h"
# include "streams.h"
# define MAXLINE 250

FILE *tfd;
int  count = 1;
char refs[REFSIZE], *rp;

main(argc, argv)
   int argc;
   char **argv;
{
   tfd = stdout;
   doargs(argc, argv, "/usr/lib/bmac/bib.list");
   exit(0);
}

/* rdtext - process a file */
   rdtext(ifile)
   FILE *ifile;
{
   long int start, length;
   int  i, numauths, numeds;
   char *p, c;

   start = length = 0L;

   for (;;) {
      start = nextrecord(ifile, start + length);
      if (start == EOF) break;
      length = recsize(ifile, start);

      /* count number of authors */
      numauths = numeds = 0;
      p = refs;
      for (i = length; i > 0; i--)
         if ((*p++ = getc(ifile)) == '%') {
            i--;
            c = *p++ = getc(ifile);
            if (c == 'A')
               numauths++;
            else if (c == 'E')
               numeds++;
            }

      *p = 0;
      expand(refs);
      rp = refs;
      dumpref(stdout, numauths, numeds);

     }
}

/* get a line from reference file */
   char refgets(line)
   char line[];
{
   char c, *p;

   if (*rp == 0)
      return(0);
   for (p = line;;) {
      while ((c = *rp++) != '\n')
         if (c == 0)
            return(0);
         else
            *p++ = c;
      c = *rp;
      if (c == 0)
         break;
      if (c == '.' || c == '%' || c == '\n')
         break;
      *p++ = ' ';
      }
   *p++ = '\n';
   *p = 0;
   return(' ');
}

/* dump reference */
   dumpref(ofile, maxauths, maxeds)
   FILE *ofile;
   int maxauths, maxeds;
{
   char line[250], *p;
   int  numauths, numeds;

   fprintf(tfd, ".[-\n");
   fprintf(tfd, ".ds [F %d\n", count++);
   numauths = numeds = 0;
   while (refgets(line) != 0) {
      if (line[0] == '\n')
         break;
      else if (line[0] == '.')
         fprintf(ofile, "%s\n", line);
      else {
         if (line[0] == '%') {
            for (p = &line[2]; isspace(*p); p++);
            if (line[1] == 'A')
               numauths++;
            else if (line[1] == 'E')
               numeds++;
            doline(line[1], p, numauths, maxauths, numeds, maxeds, ofile);
            }
         }
      }
   fprintf(tfd, ".][\n");
}
