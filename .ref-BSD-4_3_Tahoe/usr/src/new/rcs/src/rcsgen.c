/*
 *                     RCS revision generation
 */
#ifndef lint
static char rcsid[]= "$Id: rcsgen.c,v 3.6 88/04/24 17:32:22 bostic Exp $ Purdue CS";
#endif
/*********************************************************************************
 *********************************************************************************
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


/* $Log:	rcsgen.c,v $
 * Revision 3.6  88/04/24  17:32:22  bostic
 * fix for ANSI C
 * 
 * Revision 3.5  88/02/18  11:58:44  bostic
 * replaced with version 4
 * 
 * Revision 4.5  87/12/18  11:43:25  narten
 * additional lint cleanups, and a bug fix from the 4.3BSD version that
 * keeps "ci" from sticking a '\377' into the description if you run it
 * with a zero-length file as the description. (Guy Harris)
 * 
 * Revision 4.4  87/10/18  10:35:10  narten
 * Updating version numbers. Changes relative to 1.1 actually relative to
 * 4.2
 * 
 * Revision 1.3  87/09/24  13:59:51  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:27  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:28  kcs
 * Initial revision
 * 
 * Revision 4.2  83/12/02  23:01:39  wft
 * merged 4.1 and 3.3.1.1 (clearerr(stdin)).
 * 
 * Revision 4.1  83/05/10  16:03:33  wft
 * Changed putamin() to abort if trying to reread redirected stdin.
 * Fixed getdesc() to output a prompt on initial newline.
 * 
 * Revision 3.3.1.1  83/10/19  04:21:51  lepreau
 * Added clearerr(stdin) for re-reading description from stdin.
 * 
 * Revision 3.3  82/11/28  21:36:49  wft
 * 4.2 prerelease
 * 
 * Revision 3.3  82/11/28  21:36:49  wft
 * Replaced ferror() followed by fclose() with ffclose().
 * Putdesc() now suppresses the prompts if stdin
 * is not a terminal. A pointer to the current log message is now
 * inserted into the corresponding delta, rather than leaving it in a
 * global variable.
 *
 * Revision 3.2  82/10/18  21:11:26  wft
 * I added checks for write errors during editing, and improved
 * the prompt on putdesc().
 *
 * Revision 3.1  82/10/13  15:55:09  wft
 * corrected type of variables assigned to by getc (char --> int)
 */




#include "rcsbase.h"

extern struct hshentry * getnum();
extern char * mktemp();
extern FILE * fopen();
extern savestring();
extern struct hshentry * genrevs();
extern editstring();

extern int nextc;          /* next character from lexical analyzer          */
extern char * RCSfilename, * workfilename;
extern struct hshentry * targetdelta; /* delta to be generated              */
extern char * Ktext;       /* keywords from syntax analyzer                 */
extern char * Klog;        /* Keyword "log"                                 */
extern char * Kdesc;       /* Keyword for description                       */
extern FILE * finptr;      /* RCS input file                                */
extern FILE * frewrite;    /* new RCS file                                  */
extern FILE * fcopy;       /* result file during editing                    */
extern FILE * fedit;       /* edit file                                     */
extern char * resultfile, *editfile;/* file names for fcopy and fedit       */
extern int    rewriteflag; /* indicates whether to rewrite the input file   */


char    curlogmsg[logsize];/* buffer for current log message                */

enum stringwork {copy, edit, expand, edit_expand };
/* parameter to scandeltatext() */




char * buildrevision(deltas, target, dir, expandflag)
struct hshentry ** deltas, * target;
char * dir; int expandflag;
/* Function: Generates the revision given by target
 * by retrieving all deltas given by parameter deltas and combining them.
 * If dir==nil, the revision is printed on the standard output,
 * otherwise written into a temporary file in directory dir.
 * if expandflag==true, keyword expansion is performed.
 * returns false on errors, the name of the file with the revision otherwise.
 *
 * Algorithm: Copy inital revision unchanged. Then edit all revisions but
 * the last one into it, alternating input and output files (resultfile and
 * editfile). The last revision is then edited in, performing simultaneous
 * keyword substitution (this saves one extra pass).
 * All this simplifies if only one revision needs to be generated,
 * or no keyword expansion is necessary, or if output goes to stdout.
 */
{
        int i;

        if (deltas[0]==target) {
                /* only latest revision to generate */
                if (dir==nil) {/* print directly to stdout */
                        fcopy=stdout;
                        scandeltatext(target,expand);
                        return(char *) true;
                } else {
                        initeditfiles(dir);
                        scandeltatext(target,expandflag?expand:copy);
                        ffclose(fcopy);
                        return(resultfile);
                }
        } else {
                /* several revisions to generate */
                initeditfiles(dir?dir:"/tmp/");
                /* write initial revision into fcopy, no keyword expansion */
                scandeltatext(deltas[0],copy);
                i = 1;
                while (deltas[i+1] != nil) {
                        /* do all deltas except last one */
                        scandeltatext(deltas[i++],edit);
                }
                if (!expandflag) {
                        /* no keyword expansion; only invoked from ci */
                        scandeltatext(deltas[i],edit);
                        finishedit((struct hshentry *)nil);
                        ffclose(fcopy);
                } else {
                        /* perform keyword expansion*/
                        /* first, get to beginning of file*/
                        finishedit((struct hshentry *)nil); swapeditfiles(dir==nil);
                        scandeltatext(deltas[i],edit_expand);
                        finishedit(deltas[i]);
                        if (dir!=nil) ffclose(fcopy);
                }
                return(resultfile); /*doesn't matter for dir==nil*/
        }
}



scandeltatext(delta,func)
struct hshentry * delta; enum stringwork func;
/* Function: Scans delta text nodes up to and including the one given
 * by delta. For the one given by delta, the log message is saved into
 * curlogmsg and the text is processed according to parameter func.
 * Assumes the initial lexeme must be read in first.
 * Does not advance nexttok after it is finished.
 */
{       struct hshentry * nextdelta;

        do {
                nextlex();
                if (!(nextdelta=getnum())) {
                        fatserror("Can't find delta for revision %s", delta->num);
                }
                if (!getkey(Klog) || nexttok!=STRING)
                        serror("Missing log entry");
                elsif (delta==nextdelta) {
                        VOID savestring(curlogmsg,logsize);
                        delta->log=curlogmsg;
                } else {readstring();
                        delta->log= "";
                }
                nextlex();
                if (!getkey(Ktext) || nexttok!=STRING)
                        fatserror("Missing delta text");

                if(delta==nextdelta)
                        /* got the one we're looking for */
                        switch (func) {
                        case copy:      copystring();
                                        break;
                        case expand:    xpandstring(delta);
                                        break;
                        case edit:      editstring((struct hshentry *)nil);
                                        break;
                        case edit_expand: editstring(delta);
                                        break;
                        }
                else    readstring(); /* skip over it */

        } while (delta!=nextdelta);
}


int stdinread = 0; /* stdinread>0 if redirected stdin has been read once */

int putdesc(initflag,textflag,textfile,quietflag)
int initflag,textflag; char * textfile; int quietflag;
/* Function: puts the descriptive text into file frewrite.
 * if !initflag && !textflag, the text is simply copied from finptr.
 * Otherwise, if the textfile!=nil, the text is read from that
 * file, or from stdin, if textfile==nil.
 * Increments stdinread if text is read from redirected stdin.
 * if initflag&&quietflag&&!textflag, an empty text is inserted.
 * if !initflag, the old descriptive text is discarded.
 * Returns true is successful, false otherwise.
 */
{       FILE * txt; register int c, old1, old2;
#ifdef lint
	if (quietflag ==  0) initflag = quietflag; /* silencelint */
#endif	
        if (!initflag && !textflag) {
                /* copy old description */
                VOID fprintf(frewrite,"\n\n%s%c",Kdesc,nextc);
                rewriteflag=true; getdesc(false);
                return true;
        } else {
                /* get new description */
               if (!initflag) {
                        /*skip old description*/
                        rewriteflag=false; getdesc(false);
                }
                VOID fprintf(frewrite,"\n\n%s\n%c",Kdesc,SDELIM);
                if (textfile) {
                        old1='\n';
                        /* copy textfile */
                        if ((txt=fopen(textfile,"r"))!=NULL) {
                                while ((c=getc(txt))!=EOF) {
                                        if (c==SDELIM) VOID putc(c,frewrite); /*double up*/
                                        VOID putc(c,frewrite);
                                        old1=c;
                                }
                                if (old1!='\n') VOID putc('\n',frewrite);
                                VOID fclose(txt);
                                VOID putc(SDELIM,frewrite);
				VOID fputs("\n\n", frewrite);
                                return true;
                        } else {
                                error("Can't open file %s with description",textfile);
                                if (!isatty(fileno(stdin))) return false;
                                /* otherwise, get description from terminal */
                        }
                }
                /* read text from stdin */
                if (isatty(fileno(stdin))) {
                    VOID fputs("enter description, terminated with ^D or '.':\n",stderr);
                    VOID fputs("NOTE: This is NOT the log message!\n>> ",stderr);
		    if (feof(stdin))
		            clearerr(stdin);
                } else {  /* redirected stdin */
                    if (stdinread>0)
                        faterror("Can't reread redirected stdin for description; use -t<file>");
                    stdinread++;
                }
                c = '\0'; old2= '\n';
                if ((old1=getchar())==EOF) {
                        if (isatty(fileno(stdin))) {
                             VOID putc('\n',stderr);
                             clearerr(stdin);
			}
		} else {
		     if (old1=='\n' && isatty(fileno(stdin)))
			 VOID fputs(">> ",stderr);
		     for (;;) {
                            c=getchar();
                            if (c==EOF) {
                                    if (isatty(fileno(stdin))) {
                                            VOID putc('\n',stderr);
                                            clearerr(stdin);
				    }
                                    VOID putc(old1,frewrite);
                                    if (old1!='\n') VOID putc('\n',frewrite);
                                    break;
                            }
                            if (c=='\n' && old1=='.' && old2=='\n') {
                                    break;
                            }
                            if (c=='\n' && isatty(fileno(stdin))) VOID fputs(">> ",stderr);
                            if(old1==SDELIM) VOID putc(old1,frewrite); /* double up*/
                            VOID putc(old1,frewrite);
                            old2=old1;
                            old1=c;
                    } /* end for */
		}
                VOID putc(SDELIM,frewrite);VOID fputs("\n\n",frewrite);
                return true;
        }
}
