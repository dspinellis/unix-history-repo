#ifndef lint
static char sccsid[] = "@(#)bibargs.c	2.11	%G%";
#endif not lint
/*
        Authored by: Tim Budd, University of Arizona, 1983.
                version 7/4/83

        Various modifications suggested by:
                David Cherveny - Duke University Medical Center
                Phil Garrison - UC Berkeley
                M. J. Hawley - Yale University




        read argument strings for bib and listrefs
        do name formatting, printing lines, other actions common to both
                                                        */
# include <stdio.h>
# include <ctype.h>
# include "bib.h"
# define LINELENGTH 1024
# define MAXDEFS     500             /* maximum number of defined words */

/* global variables */
   char bibfname[120];          /* file name currently being read            */
   int  biblineno;              /* line number currently being referenced    */
   int  abbrev       = false;   /* automatically abbreviate names            */
   int  capsmcap     = false;   /* print names in caps small caps (CACM form)*/
   int  numrev       = 0;       /* number of authors names to reverse        */
   int  edabbrev     = false;   /* abbreviate editors names ?                */
   int  edcapsmcap   = false;   /* print editors in cap small caps           */
   int  ednumrev     = 0;       /* number of editors to reverse              */
   int	max_klen     = 6;	/* max size of key			     */
   int  sort         = false;   /* sort references ? (default no)            */
   int  foot         = false;   /* footnoted references ? (default endnotes) */
   int  doacite      = true;    /* place citations ? */
   int  hyphen       = false;   /* hypenate contiguous references            */
   int  ordcite      = true;    /* order multiple citations                  */
   char sortstr[80]  = "1";     /* sorting template                          */
   char trailstr[80] = "";      /* trailing characters to output             */
   char pfile[400];             /* private file name                         */
   int  personal = false;       /* personal file given ? (default no)        */
   char citetemplate[80] = "1"; /* citation template                         */
   struct wordinfo words[MAXDEFS];     /* defined words */
   struct wordinfo *wordhash[HASHSIZE];
   struct wordinfo *wordsearch();
   int  wordtop = 0;           /* number of defined words         */

/* where output goes */
   extern FILE *tfd;
/* reference file information */
   extern struct refinfo refinfo[];
   extern char reffile[];
#ifndef INCORE
   extern FILE *rfd;
#endif not INCORE
   extern int numrefs;

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
   newbibdir(BMACLIB);

   for (i = 1; i < argc; i++)
      if (argv[i][0] == '-')
         switch(argv[i][1]) {
			case 'd':
				if (argv[i][2])
					p = &argv[i][2];
				else {  /* take next arg */
					i++;
					p = argv[i];
			}
			newbibdir(p);
            case 'a':  for (p = &argv[i][2]; *p; p++)
                          if (*p == 'a' || *p == 0)
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
                          error("citation string expected for 'c'");
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

	    case 'l':  if (argv[i][2]){
                          max_klen  = atoi(&argv[i][2]);
			  if (max_klen > REFSIZE)
			      error("too long key size");
		       } else {
			  error("-l needs a numeric value");
		       }
		       break;

            case 'v':  doacite = false;
			/*FALLTHROUGH*/
            case 'f':  foot = true;
                       hyphen = false;
                       break;

            case 'h':  hyphen = ordcite = true;
                       break;

            case 'n':  for (p = &argv[i][2]; *p; p++)
                          if (*p == 'a')
                             abbrev = false;
                          else if (*p == 'v')
                             doacite = true;
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

            case 'r':  if (argv[i][2] == 0)  /* this is now replaced by -ar */
                          numrev = 1000;
                       else
                          numrev = atoi(&argv[i][2]);
                       break;

            case 's':  sort = true;
                       if (argv[i][2])
                          for (p = sortstr,q = &argv[i][2]; *p++ = *q++; );
                       break;

            case 't':  style = false;           /* fall through */
            case 'i':  if (argv[i][2])
                          p = &argv[i][2];
                       else { /* take next arg */
                          i++;
                          p = argv[i];
                          }
                       incfile(p);
                       break;

            case 'x':  capsmcap = true; /* this is now replaced by -ax */
                       break;

            case 0:    if (style) {  /* no style command given, take default */
                          style = false;
                          incfile( defstyle );
                          }
                       strcpy(bibfname,"<stdin>");
                       rdtext(stdin);
                       numfiles++;
                       break;

            default:   fputs(argv[i], stderr);
                       error("'%c' invalid switch", argv[i][1]);
            }
      else { /* file name */
         numfiles++;
         if (style) {
            style = false;
            incfile( defstyle );
            }
         fd = fopen(argv[i], "r");
         if (fd == NULL) {
            error("can't open file %s", argv[i]);
            }
         else {
            strcpy(bibfname, argv[i]);
            rdtext(fd);
            fclose(fd);
            }
         }

   if (style) incfile( defstyle );
   return(numfiles);

}

newbibdir(name)
	char *name;
{
	strreplace(COMFILE, BMACLIB, name);
	strreplace(DEFSTYLE, BMACLIB, name);
	strcpy(BMACLIB, name);
	wordstuff("BMACLIB", BMACLIB);
	fprintf(tfd, ".ds l] %s\n", BMACLIB);
}

/* incfile - read in an included file  */
incfile(np)
   char *np;
{  char name[120];
   FILE *fd;
   char *p, line[LINELENGTH], dline[LINELENGTH], word[80], *tfgets();
   int  i, getwrd();

   strcpy(bibfname, np);
   fd = fopen(np, "r");
   if (fd == NULL && *np != '/') {
      strcpy(name, "bib.");
      strcat(name, np);
      strcpy(bibfname, name);
      fd = fopen(name, "r");
      }
   if (fd == NULL && *np != '/') {
      strcpy(name,BMACLIB);
      strcat(name, "/bib.");
      strcat(name, np);
      strcpy(bibfname, name);
      fd = fopen(name, "r");
      }
   if (fd == NULL) {
      bibwarning("%s: can't open", np);
      exit(1);
      }

   /* now go off and process file */
   biblineno = 1;
   while (tfgets(line, LINELENGTH, fd) != NULL) {
      biblineno++;
      switch(line[0]) {

         case '#': break;

         case 'A': for (p = &line[1]; *p; p++)
                      if (*p == 'A' || *p == '\0')
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
		   if (wordsearch(word)) { /* already there-toss rest of def.*/
			while(line[strlen(line)-1] == '\\' ) {
                            if (tfgets(line, LINELENGTH, fd) == NULL) break;
			}
			break;
		   }
                   for (p = &line[i]; *p == ' '; p++) ;
                   for (strcpy(dline, p); dline[strlen(dline)-1] == '\\'; ){
                       dline[strlen(dline)-1] = '\n';
                       if (tfgets(line, LINELENGTH, fd) == NULL) break;
                       strcat(dline, line);
                       }
		   wordstuff(word, dline);
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

         case 'R': if (line[1] == 0)  /* this is now replaced by AR */
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

         case 'X': capsmcap = true;     /* this is now replace by AX */
                   break;

         default:  fprintf(tfd,"%s\n",line);
                   while (fgets(line, LINELENGTH, fd) != NULL)
                      fputs(line, tfd);
                   return;
         }

   }
   /* close up */
   fclose(fd);
}

/* bibwarning - print out a warning message */
  /*VARARGS1*/
  bibwarning(msg, a1, a2)
  char *msg;
{
  fprintf(stderr,"`%s', line %d: ", bibfname, biblineno);
  fprintf(stderr, msg, a1, a2);
  fprintf(stderr, "\n");
}

/* error - report unrecoverable error message */
  /*VARARGS1*/
  error(str, a1, a2)
  char *str;
{
  bibwarning(str, a1, a2);
  /*
   *	clean up temp files and exit
   */
  cleanup(1);
}

#ifndef INCORE
#ifdef READWRITE
/*
** fixrfd( mode ) -- re-opens the rfd file to be read or write,
**      depending on the mode.  Uses a static int to save the current mode
**      and avoid unnecessary re-openings.
*/
fixrfd( mode )
register int mode;
{
	static int cur_mode = WRITE;    /* rfd open for writing initially */

	if (mode != cur_mode)
	{
		rfd = freopen(reffile, ((mode == READ)? "r" : "a"), rfd);
		cur_mode = mode;
		if (rfd == NULL)
		      error("Hell!  Couldn't re-open reference file %s",
			reffile);
	}
}
#endif
#endif not INCORE


/* tfgets - fgets which trims off newline */
   char *tfgets(line, n, ptr)
   char line[];
   int  n;
   FILE *ptr;
{  reg char *p;

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
   reg char in[], out[];
   reg int i;
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

/* isword - see if character is legit word char */
int iswordc(c)
char c;
{
   if (isalnum(c) || c == '&' || c == '_')
      return(true);
   return(false);
}
   expand(line)
   char *line;
{  char line2[REFSIZE], word[LINELENGTH];
   reg	struct wordinfo *wp;
   reg	char *p, *q, *w;

	q = line2;
	for (p = line; *p; /*VOID*/){
		if (isalnum(*p)) {
			for (w = word; *p && iswordc(*p); ) *w++ = *p++;
			*w = 0;
			if (wp = wordsearch(word)){
				strcpy(word, wp->wi_def);
				expand(word);
			}
			strcpy(q, word);
			q += strlen(q);
		} else {
			*q++ = *p++;
		}
	}
	*q = 0;
	strcpy(line, line2);
}

/* wordstuff- save a word and its definition, building a hash table */
   wordstuff(word, def)
   char *word, *def;
{
   int i;
   if (wordtop >= MAXDEFS)
	error("too many definitions, max of %d", MAXDEFS);
   words[wordtop].wi_length = strlen(word);
   words[wordtop].wi_word = word ? walloc(word) : 0;
   words[wordtop].wi_def = def ? walloc(def) : 0;
   i = strhash(word);
   words[wordtop].wi_hp = wordhash[i];
   wordhash[i] = &words[wordtop];
   wordtop++;
}
   struct wordinfo *wordsearch(word)
   char *word;
{
   reg int lg;
   reg struct wordinfo *wp;
   lg = strlen(word);
   for (wp = wordhash[strhash(word)]; wp; wp = wp->wi_hp){
	if (wp->wi_length == lg && (strcmp(wp->wi_word, word) == 0)){
		return(wp);
	}
   }
   return(0);
}

   int strhash(str)
   reg char *str;
{
   reg int value = 0;
   for (value = 0; *str; value <<= 2, value += *str++)/*VOID*/;
   value %= HASHSIZE;
   if (value < 0)
	value += HASHSIZE;
   return(value);
}

/* rdref - read text for an already cited reference */
   rdref(p, ref)
   struct refinfo *p;
   char ref[REFSIZE];
{
   ref[0] = 0;
#ifndef INCORE
#ifdef READWRITE
   fixrfd( READ );                      /* fix access mode of rfd, if nec. */
#endif
   fseek(rfd, p->ri_pos, 0);
   fread(ref, p->ri_length, 1, rfd);
#else INCORE
   strcpy(ref, p->ri_ref);
#endif INCORE
}

/* wrref - write text for a new reference */
   wrref(p, ref)
   struct refinfo *p;
   char ref[REFSIZE];
{
#ifndef INCORE
#ifdef READWRITE
    fixrfd( WRITE );                 /* fix access mode of rfd, if nec. */
#else
    fseek(rfd, p->ri_pos, 0);        /* go to end of rfd */
#endif
    fwrite(ref, p->ri_length, 1, rfd);
#else INCORE
   p->ri_ref = walloc(ref);
#endif INCORE
}

/* breakname - break a name into first and last name */
   breakname(line, first, last)
   char line[], first[], last[];
{  reg char *t, *f, *q, *r, *p;

   for (t = line; *t != '\n'; t++);
   for (t--; isspace(*t); t--);

   /* now strip off last name */
   for (q = t; isspace(*q) == 0 || ((*q == ' ') & (*(q-1) == '\\')); q--)
      if (q == line)
         break;
   f = q;
   if (q != line) {
      q++;
      for (; isspace(*f); f--);
      f++;
      }

   /* first name is start to f, last name is q to t */

   for (r = first, p = line; p != f; )
      *r++ = *p++;
   *r = 0;
   for (r = last, p = q, t++; q != t; )
      *r++ = *q++;
   *r = 0;

}

/* match - see if string1 is a substring of string2 (case independent)*/
   int match(str1, str2)
   reg char str1[], str2[];
{  reg int  j, i;
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
   reg char *p, *q;
{
   while (*p++ = *q++)
      ;
   return(--p);
}

/* rcomp - reference comparison routine for qsort utility */
   int rcomp(ap, bp)
   struct refinfo *ap, *bp;
{  char ref1[REFSIZE], ref2[REFSIZE], field1[MAXFIELD], field2[MAXFIELD];
   reg	char *p, *q;
   char *getfield();
   int  neg, res;
   int  fields_found;

   rdref(ap, ref1);
   rdref(bp, ref2);
   for (p = sortstr; *p; p = q) {
      if (*p == '-') {
         p++;
         neg = true;
         }
      else
         neg = false;
      q = getfield(p, field1, ref1);
      fields_found = true;
      if (q == 0) {
	 res = 1;
	 fields_found = false;
      } else if (strcmp (field1, "") == 0) {	/* field not found */
         if (*p == 'A') {
            getfield("F", field1, ref1);
	    if (strcmp (field1, "") == 0) {
               getfield("I", field1, ref1);
	       if (strcmp (field1, "") == 0) {
	          res = 1;
		  fields_found = false;
	       }
	    }
	 } else {
	    res = 1;
	    fields_found = false;
	 }
      }

      if (getfield(p, field2, ref2) == 0) {
	 res = -1;
	 fields_found = false;
      } else if (strcmp (field2, "") == 0) {	/* field not found */
         if (*p == 'A') {
            getfield("F", field2, ref2);
	    if (strcmp (field2, "") == 0) {
               getfield("I", field2, ref2);
	       if (strcmp (field2, "") == 0) {
	          res = -1;
		  fields_found = false;
	       }
	    }
	 } else {
	    res = -1;
	    fields_found = false;
	 }
      }
      if (fields_found) {
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

/* makecites - make standard citation strings, using citetemplate currently in effect */
   makecites()
{  char ref[REFSIZE], tempcite[100], *malloc();
   reg int  i;

   for (i = 0; i < numrefs; i++) {
      rdref(&refinfo[i], ref);
      bldcite(tempcite, i, ref);
      refinfo[i].ri_cite = malloc(2 + strlen(tempcite));
      if (refinfo[i].ri_cite == NULL)
         error("out of storage");
      strcpy(refinfo[i].ri_cite, tempcite);
      }
}

/* bldcite - build a single citation string */
   bldcite(cp, i, ref)
   char *cp, ref[];
   int  i;
{  reg char *p, *q, *fp;
   char c;
   char field[REFSIZE];
   char *getfield(), *aabet(), *aabetlast(),
        *fullaabet(), *multfull();

   getfield("F", field, ref);
   if (field[0] != 0)
      for (p = field; *p; p++)
         *cp++ = *p;
   else {
      p = citetemplate;
      field[0] = 0;
      while (c = *p++) {
         if (isalpha(c)) {                      /* field name   */
            q = getfield(p-1, field, ref);
            if (q != 0) {
               p = q;
               for (fp = field; *fp; )
                  *cp++ = *fp++;
               }
            }
         else if (c == '1') {                   /* numeric  order */
            sprintf(field,"%d",1 + i);
            for (fp = field; *fp; )
               *cp++ = *fp++;
            }
         else if (c == '2')                     /* alternate alphabetic */
            cp = aabet(cp, ref);
         else if (c == '3')                     /* Astrophysical Journal style*/
            cp = multfull(cp, ref, 3);
         else if (c == '4')                     /* Computing Surveys style*/
            cp = multfull(cp, ref, 2);
	 else if (c == '8')			/* Full alphabetic */
	    cp = fullaabet(cp, ref);
         else if (c == '9')                     /* Last name of Senior Author*/
            cp = aabetlast(cp, ref);
	 else if (c == '0') {			/* print nothing */
            for (fp = field; *fp; )
               *cp++ = *fp++;
            }
/*       else if (c == '4')          here is how to add new styles */
         else if (c == '{') {                   /* other information   */
            while (*p != '}')
               if (*p == 0)
                  error("unexpected end of citation template");
               else
                  *cp++ = *p++;
            p++;
            }
         else if (c == '<') {
            while (*p != '>') {
               if (*p == 0)
                  error("unexpected end of citation template");
               else
                  *cp++ = *p++;
               }
            p++;
            }
         else if (c != '@')
            *cp++ = c;
         }
      }
   *cp++ = 0;
}

/* alternate alphabetic citation style -
        if 1 author - first three letters of last name
        if 2 authors - first two letters of first, followed by first letter of
                                seond
        if 3 or more authors - first letter of first three authors */
   char *aabet(cp, ref)
   char *cp, ref[];
{  char field[REFSIZE], temp[100];
   reg char *np, *fp;
   int j, getname();

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
return(cp);
}

/* alternate alphabetic citation style -
	first two characters of last names of all authors
	up to max_klen characters.
*/
   char *fullaabet(cp, ref)
   char *cp, ref[];
{  char field[REFSIZE], temp[100];
   reg char	*fp;
   char	*lastcp;
   int getname();
   int i;

   lastcp = cp + max_klen;
   for (i= 1; getname(i, field, temp, ref); i++) {
      for (fp = field; *fp && (fp < &(field[3])); )
	 if (cp > lastcp)
	     break;
         else if (isalpha(*fp))
	     *cp++ = *fp++;
	 else
	     fp++;
   }
   return(cp);
}


/* alternate alphabetic citation style -
	entire last name of senior author
*/
   char *aabetlast(cp, ref)
   char *cp, ref[];
{  char field[REFSIZE], temp[100];
   reg char	*fp;
   int getname();

   if (getname(1, field, temp, ref)) {
      for (fp = field; *fp; )
         *cp++ = *fp++;
   }
   return(cp);
}

/* 
  Multiple full authors last names (1, 2 or 3 full names).

  If maxauthors<3
        if 1 author - last name date
        if 2 authors - last name and last name date
        if 3 or more authors - last name et al. date
  If maxauthors>=3
        if 1 author - last name date
        if 2 authors - last name and last name date
        if 3 authors - last name, last name and last name date
        if 4 or more authors - last name et al. date */
   char *multfull(cp, ref, maxauthors)
   char *cp, ref[];
   int maxauthors;
{  char name1[100], name2[100], name3[100], temp[100];
   reg char *fp;
   int getname();

   if (getname(1, name1, temp, ref)) {
      for (fp = name1; *fp; )
         *cp++ = *fp++;
      if ((maxauthors >= 3) && (getname(4, name3, temp, ref))) {
         for (fp = " \\*(e]"; *fp; )
            *cp++ = *fp++;
         }
      else if (getname(2, name2, temp, ref)) {
         if (getname(3, name3, temp, ref)) {
            for (fp = "\\*(c]"; *fp; )
               *cp++ = *fp++;
            for (fp = name2; *fp; )
               *cp++ = *fp++;
            for (fp = "\\*(m]"; *fp; )
               *cp++ = *fp++;
            for (fp = name3; *fp; )
               *cp++ = *fp++;
            }
         else {
            for (fp = "\\*(n]"; *fp; )
               *cp++ = *fp++;
            for (fp = name2; *fp; )
               *cp++ = *fp++;
            }
         }
    }
return(cp);
}

/* getfield - get a single field from reference */
   char *getfield(ptr, field, ref)
   char *ptr, field[], ref[];
{  reg	char *p, *q;
   char	temp[100];
   int  n, len, i, getname();

   field[0] = 0;
   if (*ptr == 'A')
      getname(1, field, temp, ref);
   else
      for (p = ref; *p; p++)
         if (*p == '%' && *(p+1) == *ptr) {
            for (p = p + 2; *p == ' '; p++)
               ;
            for (q = field; (*p != '\n') && (*p != '\0'); )
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
{  reg char *p;
   int  m;

   m = n;
   for (p = ref; *p; p++)
      if (*p == '%' & *(p+1) == 'A') {
         n--;
         if (n == 0) {
            for (p = p + 2; *p == ' '; p++) ;
            breakname(p, first, last) ;
            return(true);
            }
         }

   if (n == m)          /* no authors, try editors */
      for (p = ref; *p; p++)
         if (*p == '%' & *(p+1) == 'E') {
            n--;
            if (n == 0) {
               for (p = p + 2; *p == ' '; p++) ;
               breakname(p, first, last) ;
               return(true);
               }
            }

   if (n == m) {        /* no editors, either, try institution */
      first[0] = last[0] = '\0';
      getfield("I", last, ref);
      if (last[0] != '\0')
         return(true);
      }

   return(false);
}

/* disambiguate - compare adjacent citation strings, and if equal, add
                  single character disambiguators */
   disambiguate()
{  reg int i, j;
	char adstr;

   for (i = 0; i < numrefs-1; i = j) {
      j = i + 1;
      if (strcmp(refinfo[i].ri_cite, refinfo[j].ri_cite)==0) {
         adstr = 'a';
         for(j = i+1;
	     j<numrefs && strcmp(refinfo[i].ri_cite,refinfo[j].ri_cite) == 0;
	     j++) {
            adstr = 'a' + (j-i);
	    refinfo[j].ri_disambig[0] = adstr;
            }
	 refinfo[i].ri_disambig[0] = 'a';
         }
     }
  for (i = 0; i < numrefs; i++){
	strcat(refinfo[i].ri_cite, refinfo[i].ri_disambig);
  }
}


/* bldname - build a name field
             doing abbreviations, reversals, and caps/small caps
*/
   bldname(first, last, name, reverse)
   char *first, *last, name[];
   int reverse;
{
   char newfirst[120], newlast[120];
   reg char *p, *q, *f, *l;
   char *scopy();
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
            if (flag)           /* between initial gap */
               q = scopy(q, "\\*(a]");
            flag = true;
            *q++ = *p;
            q = scopy(q, "\\*(p]");
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

   if (f[0] == 0)
      sprintf(name, "%s\n", l);
   else if (reverse)
      sprintf(name, "%s\\*(b]%s\n", l, f);
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

/* dumpref - dump reference number i */
   dumpref(i, ofd)
   int i;
   FILE *ofd;
{  char ref[REFSIZE], line[REFSIZE];
   reg char *p, *q;
   char *from;
   int numauths, maxauths, numeds, maxeds;

   if ( i < 0 ) ref[0] = 0; /* ref not found */
   else {
	   rdref(&refinfo[i], ref);
	   maxauths = maxeds = 0;
	   numauths = numeds = 0;
	   for (p = ref; *p; p++)
	      if (*p == '%')
	         if (*(p+1) == 'A') maxauths++;
	         else if (*(p+1) == 'E') maxeds++;
	   fprintf(ofd, ".[-\n");
	   fprintf(ofd, ".ds [F %s\n", refinfo[i].ri_cite);
#ifndef INCORE
	   fseek(rfd, (long)refinfo[i].ri_pos, 0);
	   while (fgets(line, REFSIZE, rfd) != NULL) {
#else INCORE
	   for (q = line, from = refinfo[i].ri_ref; *from; /*VOID*/) { /*} */
		if (*from == '\n'){
			*q++ = '\n';
			*q = 0;
			q = line;
			from++;
		} else {
			*q++ = *from++;
			continue;
		}
#endif INCORE
		switch(line[0]){
		case 0:
			goto doneref;
		case '.':
			fprintf(ofd, "%s", line);
			break;
		case '%':
			switch(line[1]){
			case 'A':	numauths++;	break;
			case 'E':	numeds++;	break;
			}
			for (p = &line[2]; *p == ' '; p++) /*VOID*/;
			doline(line[1], p, numauths, maxauths, numeds, maxeds, ofd);
		}
	   }
	   doneref:;
	   fprintf(ofd,".][\n");
   }
}
