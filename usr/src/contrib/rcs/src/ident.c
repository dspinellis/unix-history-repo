/* Copyright (C) 1982, 1988, 1989 Walter Tichy
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Walter Tichy.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Report all problems and direct all questions to:
 *   rcs-bugs@cs.purdue.edu
 * 







*/

/*
 *                     RCS identification operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /usr/src/local/bin/rcs/src/RCS/ident.c,v 4.5 89/05/01 15:11:54 narten Exp $Purdue CS";
#endif

/* $Log:	ident.c,v $
 * Revision 4.5  89/05/01  15:11:54  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.4  87/10/23  17:09:57  narten
 * added exit(0) so exit return code would be non random
 * 
 * Revision 4.3  87/10/18  10:23:55  narten
 * Updating version numbers. Changes relative to 1.1 are actually relative
 * to 4.1
 * 
 * Revision 1.3  87/07/09  09:20:52  trinkle
 * Added check to make sure there is at least one arg before comparing argv[1]
 * with "-q".  This necessary on machines that don't allow dereferncing null
 * pointers (i.e. Suns).
 * 
 * Revision 1.2  87/03/27  14:21:47  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:03  kcs
 * Initial revision
 * 
 * Revision 4.1  83/05/10  16:31:02  wft
 * Added option -q and input from reading stdin.
 * Marker matching is now done with trymatch() (independent of keywords).
 * 
 * Revision 3.4  83/02/18  17:37:49  wft
 * removed printing of new line after last file.
 *
 * Revision 3.3  82/12/04  12:48:55  wft
 * Added LOCKER.
 *
 * Revision 3.2  82/11/28  18:24:17  wft
 * removed Suffix; added ungetc to avoid skipping over trailing KDELIM.
 *
 * Revision 3.1  82/10/13  15:58:51  wft
 * fixed type of variables receiving from getc() (char-->int).
*/

#include  "rcsbase.h"
#define fflsbuf _flsbuf
/* redefinition of _flsbuf in putc not needed */
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif

extern enum markers trymatch();

int quietflag;

main(argc, argv)
int  argc; char  *argv[];
/*  Ident searches the named files for all occurrences
 *  of the pattern $keyword:...$, where the keywords are
 *  Author, Date, Header, Id, Log, RCSfile, Revision, Source, and State.
 */

{
   FILE *fp, *fopen();

   quietflag = false;
   if (argc > 1 && strcmp("-q",argv[1])==0) {
        quietflag = true;
        argc--; argv++;
   }

   if (argc<2) {
	 if ((scanfile(stdin) == 0) && !quietflag)
	    VOID fprintf(stderr, "ident warning: no id keywords in input\n");
	exit(0);
   }

   while ( --argc > 0 ) {
      if ( (fp = fopen(*++argv, "r") ) == NULL ) {
         VOID fprintf(stderr,  "ident error: can't open %s\n", *argv);
         continue;
      } else {
         VOID printf( "%s:\n", *argv);   /*  print file name  */
	 if ((scanfile(fp) == 0) && !quietflag)
	    VOID fprintf(stderr, "ident warning: no id keywords in %s\n", *argv);
	 if (argc>1) putchar('\n');
	 VOID fclose(fp);
      }
   }
   exit(0);
}


int scanfile(file)
FILE * file;
/* Function: scan an open file with descriptor file for keywords.
 * Returns the number of matches.
 */
{
   register int matchcount;
   register int c;


   matchcount = 0;
   while( (c=getc(file)) != EOF) {
      if ( (char)c==KDELIM)
	 matchcount += match(file);
   }
   return matchcount;
}



match(fp)   /* group substring between two KDELIM's; then do pattern match */
FILE *fp;
{
   char line[keyvallength];
   register int c;
   register char * tp;

   tp = line;
   while( (c = getc(fp)) != KDELIM ) {
      *tp++ = c;
      if ( c==EOF || c=='\n' || tp>= line+keyvallength-2)
         return(0);
   }
   *tp++ = c;     /*append trailing KDELIM*/
   *tp   = '\0';
   if (trymatch(line,true)!=Nomatch) {
        VOID fprintf(stdout,"     $%s\n",line);
        return(1);
   } else {
      /* no match; put trailing KDELIM back into input */
      VOID ungetc(c,fp );
      return(0);
   }
}
