/*
 *                     RCS keyword extraction
 */
#ifndef lint
static char rcsid[]= "$Id: rcskeep.c,v 4.6 89/05/01 15:12:56 narten Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       main routine: getoldkeys()
 *                       Testprogram: define KEEPTEST
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



/* $Log:	rcskeep.c,v $
 * Revision 4.6  89/05/01  15:12:56  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.5  88/11/08  12:01:05  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.5  88/08/09  19:13:03  eggert
 * Remove lint and speed up by making FILE *fp local, not global.
 * 
 * Revision 4.4  87/12/18  11:44:21  narten
 * more lint cleanups (Guy Harris)
 * 
 * Revision 4.3  87/10/18  10:35:50  narten
 * Updating version numbers. Changes relative to 1.1 actually relative
 * to 4.1
 * 
 * Revision 1.3  87/09/24  14:00:00  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:29  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:30  kcs
 * Initial revision
 * 
 * Revision 4.1  83/05/10  16:26:44  wft
 * Added new markers Id and RCSfile; extraction added.
 * Marker matching with trymatch().
 * 
 * Revision 3.2  82/12/24  12:08:26  wft
 * added missing #endif.
 *
 * Revision 3.1  82/12/04  13:22:41  wft
 * Initial revision.
 *
 */

/*
#define KEEPTEST
/* Testprogram; prints out the keyword values found. */

#include  "rcsbase.h"
extern char * checkid();
extern FILE * fopen();
static int getval();
extern enum markers trymatch();

#define IDLENGTH 30
char prevauthor[IDLENGTH];
char prevdate[datelength];
char prevRCS[NCPFN];
char prevrev[revlength];
char prevsource[NCPPN];
char prevstate [IDLENGTH];
char prevlocker[IDLENGTH];
char dummy[IDLENGTH];

getoldkeys(fname)
char * fname;
/* Function: Tries to read keyword values for author, date,
 * revision number, RCS file, (both with and without path),
 * state, and workfilename out of the file fname.
 * The results are placed into
 * prevauthor, prevdate, prevRCS, prevrev, prevsource, prevstate.
 * Aborts immediately if it finds an error and returns false.
 * If it returns true, it doesn't mean that any of the
 * values were found; instead, check to see whether the corresponding arrays
 * contain the empty string.
 */
{
    register FILE *fp;
    register int c;
    char keyword[keylength+2];
    register char * tp;
    enum markers mresult;

    /* initialize to empty */
    prevauthor[0]=prevsource[0]=prevstate[0]=prevdate[0]=prevrev[0]= '\0';

    if ( (fp = fopen(fname, "r") ) == NULL ) {
       error("Can't open %s\n", fname);
       return false;
    }
    while( (c=getc(fp)) != EOF) {
        if ( c==KDELIM) {
            /* try to get keyword */
            tp = keyword;
	    while( (c=getc(fp))!=EOF && (tp< keyword+keylength) && (c!='\n')
		   && (c!=KDELIM) && (c!=VDELIM))
		  *tp++ = c;

            if (c==KDELIM) {VOID ungetc(c,fp);continue;}
            if (c!=VDELIM) continue;
	    *tp++ = c;
            *tp='\0';
            while ((c=getc(fp))==' '||c=='\t'); /* skip blanks */
            VOID ungetc(c,fp); /* needed for getval */

	    switch (mresult=trymatch(keyword,true)) {
            case Author:
		if (getval(fp,prevauthor,IDLENGTH,true))
                    if (!checkid(prevauthor, '\0')) goto errexit;
                break;
            case Date:
		if (!getprevdate(fp,true)) goto errexit;
                break;
            case Header:
            case Id:
		if (mresult==Header) {
		    if (!getval(fp,prevsource,NCPPN,true)) break; /*unexpanded*/
		} else {
		    if (!getval(fp,prevRCS,NCPFN,true))    break; /*unexpanded*/
		}
		if (!getval(fp,prevrev,revlength,false)) goto errexit;
		if (!checknum(prevrev,-1)) {
		    error("Bad revision number");
		    goto errexit;
		}
		if (!getprevdate(fp,false)) goto errexit;
		if (!getval(fp,prevauthor,IDLENGTH,false)) goto errexit;
		if (!checkid(prevauthor, '\0')) goto errexit;
		if (!getval(fp,prevstate,IDLENGTH,false)) goto errexit;
		if (!checkid(prevstate, '\0')) goto errexit;
		VOID getval(fp, dummy, IDLENGTH, true);    /* optional locker*/
		VOID getval(fp, prevlocker,IDLENGTH,true); /* optional locker*/
                break;
            case Locker:
                VOID getval(fp,prevlocker,IDLENGTH,true);
		if (!checkid(prevlocker, '\0')) goto errexit;
                break;
            case Log:
		VOID getval(fp,prevRCS,NCPPN,true);
                break;
            case RCSfile:
                VOID getval(fp,prevRCS,NCPFN,true);
                break;
            case Revision:
                if (getval(fp,prevrev,revlength,true))
                    if (!checknum(prevrev,-1)) {
                        error("Bad revision number");
                        goto errexit;
                    }
                break;
            case Source:
                VOID getval(fp,prevsource,NCPPN,true);
                break;
            case State:
                if (getval(fp,prevstate,IDLENGTH,true))
                    if (!checkid(prevstate, '\0')) goto errexit;
                break;
            default:
               continue;
            }
            if (getc(fp)!=KDELIM)
                warn("Closing %c missing on keyword",KDELIM);
            if (prevauthor[0]!='\0'&&prevrev[0]!='\0'&&prevstate[0]!='\0'&&
                prevdate[0]!='\0' &&
		 ((prevsource[0]!='\0')||(prevRCS[0]!='\0'))){
                /* done; prevlocker is irrelevant */
                break;
           }
        }
    }
    VOID fclose(fp);
    return true;

errexit:
    prevauthor[0]=prevsource[0]=prevstate[0]=prevdate[0]=prevrev[0]= '\0';
    VOID fclose(fp); return false;
}


static int getval(fp,target,maxchars,optional)
register FILE *fp;
char * target; int maxchars, optional;
/* Function: Places a keyword value into target, but not more
 * than maxchars characters. Prints an error if optional==false
 * and there is no keyword. Returns true if one is found, false otherwise.
 */
{   register char * tp;
    register int c;

    tp=target;
    c=getc(fp);
    if (c==KDELIM) {
        if (!optional)
            error("Missing keyword value");
        VOID ungetc(c,fp);
        return false;
    } else {
        while (!(c==' '||c=='\n'||c=='\t'||c==KDELIM||c==EOF)) {
            if (tp-target>=maxchars-1) {
                error("keyword value too long");
                return false;
            } else {
                *tp++ =c;
                c=getc(fp);
            }
        }
        *tp= '\0';
#       ifdef KEEPTEST
        VOID printf("getval: %s\n",target);
#       endif
        while(c==' '||c=='\t') c=getc(fp); /* skip trailing blanks */
    }
    VOID ungetc(c,fp);
    return true;
}


int getprevdate(fp,optional)
FILE *fp;
int optional;
/* Function: reads a date prevdate; checks format
 * If there is not date and optional==false, an error is printed.
 * Returns false on error, true otherwise.
 */
{   char prevday[10];
    char prevtime[10];

    prevday[0]=prevtime[0]='\0';
    if (!getval(fp,prevday,9,optional)) return optional;
    if (!getval(fp,prevtime,9,false)) return false;
    /*process date */
    prevday[2]=prevday[5]=prevday[8]=prevtime[2]=prevtime[5]='.';
    prevday[9]='\0';
    VOID strcpy(prevdate,prevday);
    VOID strcat(prevdate,prevtime);
    if (!checknum(prevdate,5)) {
            error("Bad date: %s",prevdate);
            prevdate[0]='\0';
            return false;
    }
    return true;
}

int checknum(sp,fields)
register char * sp; int fields;
{    register int dotcount;
     if (sp==nil||*sp=='\0') return true;
     dotcount=0;
     while(*sp) {
        if (*sp=='.') dotcount++;
        elsif (ctab[*sp]!=DIGIT) return false;
        sp++;
     }
     if (fields >= 0 && dotcount!=fields) return false;
     return true;
}



#ifdef KEEPTEST
char * RCSfilename, * workfilename;

main(argc, argv)
int  argc; char  *argv[];
{
	cmdid="keeptest";
        while (*(++argv)) {
                if (getoldkeys(*argv))
                VOID printf("%s:  revision: %s, date: %s, author: %s, state: %s\n",
                        *argv, prevrev, prevdate, prevauthor,prevstate);
		VOID printf("Source: %s, RCSfile: %s\n",prevsource,prevRCS);
	}
	exit(0);
}
#endif
