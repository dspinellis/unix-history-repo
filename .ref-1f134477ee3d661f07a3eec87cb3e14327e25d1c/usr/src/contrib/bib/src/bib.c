#ifndef lint
static char sccsid[] = "@(#)bib.c	2.12	%G%";
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

	       version 8/23/1988
	 
	 Adapted to use TiB style macro calls (i.e. |macro|)
	       A. Dain Samples

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
#ifndef INCORE
   char reffile[] = TMPREFFILE ;/* temporary file (see bib.h)            */
#endif not INCORE
   struct refinfo refinfo[MAXREFS];	/* reference information */
   struct refinfo *refssearch();
   struct refinfo *refshash[HASHSIZE];
   long int rend = 1;           /* last position in rfd (first char unused)*/
   int numrefs = 0;            /* number of references generated so far */
   FILE *tfd;                   /* output of pass 1 of file(s)           */
   char bibtmpfile[] = TMPTEXTFILE ; /* output of pass 1                    */
   char *common = COMFILE;       /* common word file                      */
   int  findex = false;         /* can we read the file INDEX ?          */

char *programName;

/* global variables in bibargs */
   extern int foot, doacite, sort, max_klen, personal;
   extern int hyphen, ordcite, biblineno;
   extern char sortstr[], pfile[], citetemplate[], bibfname[];
   extern int TibOption;

#include <signal.h>

main(argc, argv)
   int argc;
   char **argv;
{  int rcomp();
   void intr();

   /* the file INDEX in the current directory is the default index,
      if it is present */

   InitDirectory(BMACLIB,N_BMACLIB);
   InitDirectory(COMFILE,N_COMFILE);
   InitDirectory(DEFSTYLE,N_DEFSTYLE);

   signal(SIGINT, intr);
   rfd = fopen( INDXFILE , "r");
   if (rfd != NULL) {
      findex = true;
      fclose(rfd);
      }

#ifndef INCORE
   /* open temporaries, reffile will contain references collected in
      pass 1, and bibtmpfile will contain text.
   */
   mktemp(reffile);
   rfd = fopen(reffile,"w+");
   if (rfd == NULL)
      error("can't open temporary reference file, %s", reffile);
   putc('x', rfd);      /* put garbage in first position (not used) */
#endif not INCORE
   mktemp(bibtmpfile);
   tfd = fopen(bibtmpfile,"w");
   if (tfd == NULL)
      error("can't open temporary output file, %s", bibtmpfile);

    /*
       pass1 - read files, looking for citations
               arguments are read by doargs (bibargs.c)
    */

   if (doargs(argc, argv, DEFSTYLE ) == 0) { /* may not return */
      strcpy(bibfname, "<stdin>");
      rdtext(stdin);
      }

   /*
    sort references, make citations, add disambiguating characters
   */

   if (sort)
      qsort(refinfo, numrefs, sizeof(struct refinfo), rcomp);
   makecites();
   disambiguate();

   /*
   reopen temporaries
   */

   fclose(tfd);
   tfd = fopen(bibtmpfile,"r");
   if (tfd == NULL)
      error("can't open temporary output file %s for reading", bibtmpfile);
   /*
   pass 2 - reread files, replacing references
   */
   pass2(tfd, stdout);
   cleanup(0);
}
/* interrupt processing */
void
intr()
{
   cleanup(1);
}
/* clean up and exit */
cleanup(val)
{
   fclose(tfd);
#ifndef INCORE
   fclose(rfd);
   unlink(reffile);
#endif INCORE
#ifndef DEBUG
   unlink(bibtmpfile);
#endif DEBUG
   exit(val);
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
{  int getref();
   char huntstr[HUNTSIZE], c, info[HUNTSIZE];

   if (ch == '[')
      if (doacite) fputs("\\*([[", tfd);
   else
      if (doacite) fputs("\\*([{", tfd);
   huntstr[0] = info[0] = 0;
   while (getch(c, fd) != EOF)
      switch (c) {
         case ',':
	    citemark(info, huntstr, "");
            huntstr[0] = info[0] = 0;
            break;
         case '.':
            while (getch(c, fd) == '.') ;
            if (c == ']') {
	       citemark(info, huntstr, "\\*(]]");
               return;
               }
            else if (c == '}') {
	       citemark(info, huntstr, "\\*(}]");
               return;
               }
            else
               addc(huntstr, c);
            break;

         case '{':
            while (getch(c, fd) != '}')
               if (c == EOF) {
                  error("ill formed reference");
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
char	ncitetemplate[64];
int	changecite;
citemark(info, huntstr, tail)
	char *info, *huntstr, *tail;
{
	char c = CITEMARK;
        long int  n;
	/*
	 *	getref sets ncitetemplate as a side effect
	 */
	n = getref(huntstr);
	if (ncitetemplate[0]){
		fprintf(tfd, "%c%s%c", FMTSTART, ncitetemplate, FMTEND);
		ncitetemplate[0] = 0;
	}
	fprintf(tfd, "%c%d%c%s%c%s", c ,n, c, info, CITEEND, doacite?tail:"");

}

/* addc - add a character to hunt string */
addc(huntstr, c)
   char huntstr[HUNTSIZE], c;
{  int  i;

   i = strlen(huntstr);
   if (i > HUNTSIZE)
      error("citation too long, max of %d", HUNTSIZE);
   huntstr[i] = c;
   huntstr[i+1] = 0;
}

/* getref - if an item was already referenced, return its reference index
                otherwise create a new entry */
int getref(huntstr)
   char huntstr[HUNTSIZE];
{  char rf[REFSIZE], *r, *hunt();
   int	match(), getwrd();
   char	*realhstr;
   int hash;
   struct refinfo *rp;
   int	lg;

   realhstr = huntstr;
   if (strncmp(huntstr, "$C$", 3) == 0){
	char *from, *to;
	changecite++;
	for(from = huntstr + 3, to = ncitetemplate; *from; from++, to++){
		switch(*from){
		case '\0':
		case ' ':
		case '\n':
		case '\t':	goto outcopy;
		default:	*to = *from;
		}
	}
   outcopy: ;
	*to = 0;
	*from = 0;
	realhstr = from + 1;
   }
   r = hunt(realhstr);
   if (r != NULL) {
      /* expand defined string */
      strcpy(rf, r);
      free(r);
      expand(rf);
      /* see if reference has already been cited */
      if (foot == false && (rp = refssearch(rf))){
		return(rp - refinfo);
      }
      /* didn't match any existing reference, create new one */
      if (numrefs >= MAXREFS)
	error("too many references, max of %d", MAXREFS);
      hash = strhash(rf);
      lg = strlen(rf) + 1;
      refinfo[numrefs].ri_pos = rend;
      refinfo[numrefs].ri_length = lg;
      refinfo[numrefs].ri_hp = refshash[hash];
      refinfo[numrefs].ri_n = numrefs;
      refshash[hash] = &refinfo[numrefs];
      wrref(&refinfo[numrefs], rf);
      return(numrefs++);
      }
   else {
      bibwarning("no reference matching %s\n", realhstr);
      return(-1);
      }
}

struct refinfo *refssearch(rf)
   char *rf;
{
   char ref[REFSIZE];
   reg	int i;
   int	lg;
   reg	struct refinfo *rp;
   lg = strlen(rf) + 1;
   for (rp = refshash[strhash(rf)]; rp; rp = rp->ri_hp){
	     if (rp->ri_length == lg){
		     rdref(rp, ref);
		     if (strcmp(ref, rf) == 0)
			return(rp);
	     }
   }
   return(0);
}
/* hunt - hunt for reference from either personal or system index */
/* the old versions would stop at the first index file where a citation
 * matched.  This is NOT what is desired.  I have changed it so that it still
 * returns the first citation found, but also reports the existence of
 * duplicate entries in an INDEX file as well as across INDEX files.
 * Also, we do NOT assume that the SYSINDEX has been Tib'd.  Therefore,
 * if tib style expansion is in effect, the SYSINDEX is not searched.
 * (Besides which, on Sun systems at least, the SYSINDEX files are
 * created by refer, not bib, so we can't use them very effectively
 * anyway.  Besides which again, everything in SYSINDEX is in our
 * local files anyway.)
 *                   - ads 8/88
 */
char *hunt(huntstr)
   char huntstr[];
{  char *found, *fhunt(), *r, *tp, *sp, fname[120];

   found = NULL;
   if (personal) {
      for (tp = fname, sp = pfile; ; sp++)
         if (*sp == ',' || *sp == '\0') {
            *tp = '\0';
            if ((r = fhunt(fname, huntstr)) != NULL) {
		if (found != NULL) {
		    /* we need an option to suppress this message -ads 5/89 */
		    bibwarning("multiple INDEX files match citation %s\n",
							huntstr);
		    return (found);
		    }
		found = r;
		}
            if (*sp == '\0')
               break;
            tp = fname;
            }
         else *tp++ = *sp;
      if (found != NULL) return (found);
      }
   else if (findex) {
      if ((r = fhunt(INDXFILE , huntstr)) != NULL)
         return(r);
      }
   if (!TibOption) {
      if ((r = fhunt(SYSINDEX , huntstr)) != NULL)
	 return(r);
      }
   return(NULL);
}

/* fhunt - hunt from a specific file */
   char *fhunt(file, huntstr)
   char file[], huntstr[];
{  char *p, *r, *locate();

   r = locate(huntstr, file, max_klen, common);

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
struct cite{
	int	num;
	char	*info;
};
citesort(p1, p2)
	struct cite *p1, *p2;
{
	return(p1->num - p2->num);
}

/* putrefs - gather contiguous references together, sort them if called
   for, hyphenate if necessary, and dump them out */
int putrefs(ifd, ofd, footrefs, fn)
FILE *ifd, *ofd;
int  fn, footrefs[];
{
	struct cite cites[MAXATONCE];
	char	infoword[HUNTSIZE];    /* information line */
	reg	int i;
	reg	char *p;
	reg	int  ncites, n, j;         /* number of citations being dumped */
	char	c, *walloc();
	int neg;
	/*
	 * first gather contiguous references together,
	 * and order them if required     
	 */

	ncites = 0;
	do {
		neg = 1;
		n = 0;
		do{
			getch(c, ifd);
			if (isdigit(c))
				n = 10 * n + (c - '0');
			else if (c == '-')
				neg *= -1;
			else if (c == CITEMARK)
				break;
			else
				error("bad cite char 0%03o in pass two",c);
		} while(1);
		if (neg < 0) {     /* reference not found */
			cites[ncites].num = -1;
			cites[ncites].info = 0;
			ncites++;
		} else {
			/*
			 * Find reference n in the references
			 */
			int i;
			for (i = 0; i < numrefs; i++){
				if (refinfo[i].ri_n == n){
					cites[ncites].num = i;
					cites[ncites].info = 0;
					ncites++;
					break;
				}
			}
			if (i == numrefs)
				error("citation	%d not found in pass 2", n);
		}
		if (getch(c, ifd) != CITEEND) {
			for (p = infoword; c != CITEEND ; ) {
				*p++ = c;
				getch(c, ifd);
			}
			*p = 0;
			cites[ncites-1].info = walloc(infoword);
		}
		getch(c, ifd);
	} while (c == CITEMARK);
	ungetc(c, ifd);
	if (ordcite)
		qsort(cites, ncites, sizeof(struct cite), citesort);

	/* now dump out values */
	for (i = 0; i < ncites; i++) {
		if (cites[i].num >= 0) {
			if (changecite){
				char tempcite[128];
				char ref[REFSIZE];
				struct refinfo *p;
				/*
				 * rebuild the citation string,
				 * using the current template in effect
				 */
				p = &refinfo[cites[i].num];
				rdref(p, ref);
				bldcite(tempcite, cites[i].num, ref);
				strcat(tempcite, p->ri_disambig);
				if (doacite) fputs(tempcite, ofd);
			} else {
				if (doacite) fputs(refinfo[cites[i].num].ri_cite, ofd);
			}
			if (!doacite) fputs("\\&", ofd);
		}
		if (cites[i].info) {
			if (doacite) fputs(cites[i].info, ofd);
			if (!doacite) fputs("\\&", ofd);
			free(cites[i].info);
		}
		if (hyphen) {
			for (j = 1;
			     j + i <= ncites && cites[i+j].num == cites[i].num + j;
			     j++)/*VOID*/;
			if (j + i > ncites)
				j = ncites;
			else
				j = j + i - 1;
		} else {
			j = i;
		}
		if (j > i + 1) {
			fputs("\\*(]-", ofd);
			i = j - 1;
		} else if (i != ncites - 1) {
			fputs("\\*(],", ofd);
		}
		if (foot) {
			fn++;
			footrefs[fn] = cites[i].num;
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
                  for (i = 0; i < numrefs; i++){
                     dumpref(i, ofd);
		  }
                  getch(c, ifd);
                  }
         }
      if (c == FMTSTART)
	 changefmt(ifd);
      else if (c == CITEMARK)
         fn = putrefs(ifd, ofd, footrefs, fn);
      else if (c != EOF)
         putc(c, ofd);
      }
   if (dumped == false)
      bibwarning("Warning: references never dumped\n","");
}
/*
 *	change citation format
 */
changefmt(ifd)
	FILE	*ifd;
{
	char	c;
	char	*to;
	to = ncitetemplate;
	while (getch(c, ifd) != FMTEND)
		*to++ = c;
	*to = 0;
	strcpy(citetemplate, ncitetemplate);
}
