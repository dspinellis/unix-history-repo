/*
 *                     RCS keyword extraction
 */
 static char rcsid[]=
 "$Header: /usr/wft/RCS/SRC/RCS/rcskeep.c,v 3.2 82/12/24 12:08:26 wft Exp $ Purdue CS";
/*****************************************************************************
 *                       main routine: getoldkeys()
 *                       Testprogram: define GETOLDTEST
 *****************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 * Report problems and direct all inquiries to Tichy@purdue (ARPA net).
 */



/* $Log:	rcskeep.c,v $
 * Revision 3.2  82/12/24  12:08:26  wft
 * added missing #endif.
 * 
 * Revision 3.1  82/12/04  13:22:41  wft
 * Initial revision.
 *
 */

/*
#define GETOLDTEST
/* Testprogram; prints out the keyword values found. */

#include  "rcsbase.h"
extern char * checkid();
extern FILE * fopen();

FILE * fp;
#define IDLENGTH 30
char prevauthor[IDLENGTH];
char prevdate[datelength];
char prevrev[revlength];
char prevsource[NCPPN];
char prevstate [IDLENGTH];
char prevlocker[IDLENGTH];

getoldkeys(fname)
/* Function: Tries to read keyword values for author, date,
 * revision number, RCS file, and state out of the file fname.
 * The results are placed into
 * prevauthor, prevdate, prevrev, prevsource, and prevstate.
 * Aborts immediately if it finds an error and returns false.
 * If it returns true, it doesn't mean that any of the
 * values were found; instead, check to see whether the corresponding arrays
 * contain the empty string.
 */
{
    register int c;
    char keyword[keylength+2];
    register char * tp;

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
            while( ctab[(c=getc(fp))]==LETTER && tp< keyword+keylength)
                *tp++ = c;
            if (c==KDELIM) {ungetc(c,fp);continue;}
            if (c!=VDELIM) continue;
            *tp='\0';
            while ((c=getc(fp))==' '||c=='\t'); /* skip blanks */
            ungetc(c,fp); /* needed for getval */
            if (strcmp(keyword, AUTHOR)==0 ) {
                if (getval(prevauthor,IDLENGTH,true))
                    if (!checkid(prevauthor)) goto errexit;
            } elsif ( strcmp(keyword,DATE)==0 ) {
                if (!getprevdate(true)) goto errexit;
            } elsif ( strcmp(keyword, HEADER)==0 ) {
                if (getval(prevsource,NCPPN,true)) {
                    if (!getval(prevrev,revlength,false)) goto errexit;
                    if (!checknum(prevrev,-1)) {
                        error("Bad revision number");
                        goto errexit;
                    }
                    if (!getprevdate(false)) goto errexit;
                    if (!getval(prevauthor,IDLENGTH,false)) goto errexit;
                    if (!checkid(prevauthor)) goto errexit;
                    if (!getval(prevstate,IDLENGTH,false)) goto errexit;
                    if (!checkid(prevstate)) goto errexit;
                }
            } elsif ( strcmp(keyword, LOCKER)==0 ) {
                getval(prevlocker,IDLENGTH,true);
            } elsif ( strcmp(keyword, LOG)==0 ) {
                getval(prevsource,NCPPN,true);
            } elsif (strcmp(keyword, REVISION)==0 ) {
                if (getval(prevrev,revlength,true))
                    if (!checknum(prevrev,-1)) {
                        error("Bad revision number");
                        goto errexit;
                    }
            } elsif (strcmp(keyword, SOURCE)==0 ) {
                getval(prevsource,NCPPN,true);
            } elsif ( strcmp(keyword, STATE)==0 ) {
                if (getval(prevstate,IDLENGTH,true))
                    if (!checkid(prevstate)) goto errexit;
            } else {
               continue;
            }
            if (getc(fp)!=KDELIM)
                warn("Closing %c missing on keyword",KDELIM);
            if (prevauthor[0]!='\0'&&prevrev[0]!='\0'&&prevstate[0]!='\0'&&
                prevdate[0]!='\0' && prevsource[0]!='\0') {
                /* done; prevlocker is irrelevant */
                break;
           }
        }
    }
    fclose(fp);
    return true;

errexit:
    prevauthor[0]=prevsource[0]=prevstate[0]=prevdate[0]=prevrev[0]= '\0';
    fclose(fp); return false;
}


getval(target,maxchars,optional)
char * target; int maxchars, optional;
/* Function: Places a keyword value into target, but not more
 * than maxchars characters. Prints an error if optiona==false
 * and there is no keyword. Returns true if one is found, false otherwise.
 */
{   register char * tp;
    register int c;

    tp=target;
    c=getc(fp);
    if (c==KDELIM) {
        if (!optional)
            error("Missing keyword value");
        ungetc(c,fp);
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
#       ifdef GETOLDTEST
        printf("getval: %s\n",target);
#       endif
        while(c==' '||c=='\t') c=getc(fp); /* skip trailing blanks */
    }
    ungetc(c,fp);
    return true;
}


int getprevdate(optional)
int optional;
/* Function: reads a date prevdate; checks format 
 * If there is not date and optional==false, an error is printed.
 * Returns false on error, true otherwise.
 */
{   char prevday[10];
    char prevtime[10];

    prevday[0]=prevtime[0]='\0';
    if (!getval(prevday,9,optional)) return optional;
    if (!getval(prevtime,9,false)) return false;
    /*process date */
    prevday[2]=prevday[5]=prevday[8]=prevtime[2]=prevtime[5]='.';
    prevday[9]='\0';
    strcpy(prevdate,prevday);
    strcat(prevdate,prevtime);
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



#ifdef GETOLDTEST
cleanup(){} /* dummy */

main(argc, argv)
int  argc; char  *argv[];
{
        cmdid="getoldkeys";
        while (*(++argv)) {
                if (getoldkeys(*argv))
                printf("%s:  revision: %s, date: %s, author: %s, state: %s\n",
                        *argv, prevrev, prevdate, prevauthor,prevstate);
        }
}
#endif
