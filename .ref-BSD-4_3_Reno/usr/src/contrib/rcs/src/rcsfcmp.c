/*
 *                     RCS file comparison
 */
#ifndef lint
static char rcsid[]= "$Id: rcsfcmp.c,v 4.5 89/05/01 15:12:42 narten Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       rcsfcmp()
 *                       Testprogram: define FCMPTEST
 *****************************************************************************
 */

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





/* $Log:	rcsfcmp.c,v $
 * Revision 4.5  89/05/01  15:12:42  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.4  88/11/08  12:01:33  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.4  88/08/09  19:12:50  eggert
 * Shrink stdio code size.
 * 
 * Revision 4.3  87/12/18  11:40:02  narten
 * lint cleanups (Guy Harris)
 * 
 * Revision 4.2  87/10/18  10:33:06  narten
 * updting version number. Changes relative to 1.1 actually relative to 
 * 4.1
 * 
 * Revision 1.2  87/03/27  14:22:19  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:23  kcs
 * Initial revision
 * 
 * Revision 4.1  83/05/10  16:24:04  wft
 * Marker matching now uses trymatch(). Marker pattern is now
 * checked precisely.
 * 
 * Revision 3.1  82/12/04  13:21:40  wft
 * Initial revision.
 *
 */

/*
#define FCMPTEST
/* Testprogram; prints out whether two files are identical,
 * except for keywords
 */

#include  "rcsbase.h"
extern FILE * fopen();
extern enum markers trymatch(); /* check for keywords */


rcsfcmp(xfname,uxfname,delta)
char * xfname, *uxfname; struct hshentry *delta;
/* Function: compares the files xfname and uxfname. Returns true
 * if xfname has the same contents as uxfname, while disregarding
 * keyword values. For the LOG-keyword, rcsfcmp skips the log message
 * given by the parameter delta in xfname. Thus, rcsfcmp returns true
 * if xfname contains the same as uxfname, with the keywords expanded.
 * Implementation: character-by-character comparison until $ is found.
 * If a $ is found, read in the marker keywords; if they are real keywords
 * and identical, read in keyword value. If value is terminated properly,
 * disregard it and optionally skip log message; otherwise, compare value.
 */
{
    register int xc,uxc;
    char xkeyword[keylength+2],   uxkeyword[keylength+2];
    char xkeyval[keyvallength+1], uxkeyval[keyvallength+1];
    register FILE * xfp, * uxfp;
    register char * tp;
    int result;
    enum markers match1,match2;

    if ((xfp=fopen(tp=xfname,"r"))==NULL || (uxfp=fopen(tp=uxfname,"r"))==NULL) {
       faterror("Can't open %s\n", tp);
       return false;
    }
    result=false;
    xc=getc(xfp); uxc=getc(uxfp);
    while( xc == uxc) { /* comparison loop */
        if (xc==EOF) { /* finished; everything is the same*/
            result=true;
            break;
        }
        if ( xc!=KDELIM) {
            /* get the next characters */
            xc=getc(xfp); uxc=getc(uxfp);
        } else {
            /* try to get both keywords */
            tp = xkeyword;
            while( (xc=getc(xfp))!=EOF && (tp< xkeyword+keylength) && (xc!='\n')
                   && (xc!=KDELIM) && (xc!=VDELIM))
                *tp++ = xc;
	    *tp++ = xc;  /* add closing K/VDELIM */
            *tp='\0';
            tp = uxkeyword;
            while( (uxc=getc(uxfp))!=EOF && (tp< uxkeyword+keylength) && (uxc!='\n')
                   && (uxc!=KDELIM) && (uxc!=VDELIM))
                *tp++ = uxc;
	    *tp++ = xc;  /* add closing K/VDELIM */
            *tp='\0';
            /* now we have 2 keywords, or something thal looks like it.*/
	    match1=trymatch(xkeyword,false);
	    match2=trymatch(uxkeyword,false);
	    if (match1 != match2) break; /* not identical */
#ifdef FCMPTEST
	    VOID printf("found potential keywords %s and %s\n",xkeyword,uxkeyword);
#endif

	    if (match1 == Nomatch) {
		/* not a keyword pattern, but could still be identical */
		if (strcmp(xkeyword,uxkeyword)==0)
		     continue;
		else break;
	    }
#ifdef FCMPTEST
	    VOID printf("found common keyword %s\n",xkeyword);
#endif
	    tp=xkeyval;
	    if (xc==VDELIM) {/* get value */
		while (((xc=getc(xfp))!=KDELIM) && (xc!='\n') && (xc!=EOF) &&
			(tp<xkeyval+keyvallength))
		    *tp++ = xc;
	    }
	    *tp = '\0';   /*xkeyval now filled with value; possibly empty*/
	    tp=uxkeyval;
	    if (uxc==VDELIM) {/* get value */
		while (((uxc=getc(uxfp))!=KDELIM) && (uxc!='\n') && (uxc!=EOF) &&
			(tp<uxkeyval+keyvallength))
		    *tp++ = uxc;
	    }
	    *tp = '\0';   /*uxkeyval now filled with value; possibly empty*/
	    if (xc!=uxc) break; /* not the same */
	    if (xc==KDELIM) {
		xc=getc(xfp); uxc=getc(uxfp); /* skip closing KDELIM */
		/* if the keyword is LOG, also skip the log message in xfp*/
		if (match1==Log) {
		    /* first, compute the number of line feeds in log msg */
		    int lncnt, ccnt;
		    lncnt=2; tp=delta->log;
		    while(*tp) if(*tp++=='\n') lncnt++;
		    while(xc!=EOF) {
			if (xc=='\n')
			    if(--lncnt==0) break;
			xc=getc(xfp);
		    }
		    /* skip last comment leader */
		    /* Can't just skip another line here, because there may be */
		    /* additional characters on the line (after the Log....$)  */
		    for (ccnt=strlen(Comment); ccnt>=0; lncnt--) {
			xc=getc(xfp);
			if(xc=='\n') break;
			/* reads to the end of the comment leader or '\n',     */
			/* whatever comes first. This is because some editors  */
			/* strip off trailing blanks from a leader like " * ". */
		    }
		}
	    } else {
		/* both end in the same character, but not a KDELIM */
		/* must compare string values.*/
#ifdef FCMPTEST
		VOID printf("non-terminated keywords %s, potentially different values\n",xkeyword);
#endif
		if (strcmp(xkeyval,uxkeyval)!=0) break; /*different */
		xc=getc(xfp); uxc=getc(uxfp); /* skip closing char  */
            }
        }
    }
    VOID fclose(xfp); VOID fclose(uxfp);
    return result;
}



#ifdef FCMPTEST
char * RCSfilename, * workfilename;

char * Comment;

main(argc, argv)
int  argc; char  *argv[];
/* first argument: comment leader; 2nd: log message, 3rd: expanded file,
 * 4th: unexpanded file
 */
{       struct hshentry delta;

        cmdid="rcsfcmp";
        Comment=argv[1];
        delta.log=argv[2];
        if (rcsfcmp(argv[3],argv[4],&delta))
                VOID printf("files are the same\n");
        else    VOID printf("files are different\n");
}
#endif
