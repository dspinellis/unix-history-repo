/*
 *                     RCS revision generation
 */
 static char rcsid[]=
 "$Header: rcsgen.c,v 3.4 86/05/15 02:18:42 lepreau Exp $ Purdue CS";
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
 * Revision 3.4  86/05/15  02:18:42  lepreau
 * Fix immediate EOF from non-tty files: avoid 0377's in description.
 * 
 * Revision 3.3  82/11/28  21:36:49  wft
 * *** empty log message ***
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


char    curlogmsg[logsize] /* buffer for current log message                */
        ='\0';

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
                        finishedit(nil);
                        ffclose(fcopy);
                } else {
                        /* perform keyword expansion*/
                        /* first, get to beginning of file*/
                        finishedit(nil); swapeditfiles(dir==nil);
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
                        savestring(curlogmsg,logsize);
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
                        case edit:      editstring(nil);
                                        break;
                        case edit_expand: editstring(delta);
                                        break;
                        }
                else    readstring(); /* skip over it */

        } while (delta!=nextdelta);
}


int putdesc(initflag,textflag,textfile,quietflag)
int initflag,textflag; char * textfile; int quietflag;
/* Function: puts the descriptive text into file frewrite.
 * if !initflag && !textflag, the text is simply copied from finptr.
 * Otherwise, if the textfile!=nil, the text is read from that
 * file, or from stdin, if textfile==nil.
 * if initflag&&quietflag&&!textflag, an empty text is inserted.
 * if !initflag, the old descriptive text is discarded.
 * Returns true is successful, false otherwise.
 */
{       FILE * txt; register int c, old1, old2;

        if (!initflag && !textflag) {
                /* copy old description */
                fprintf(frewrite,"\n\n%s%c",Kdesc,nextc);
                rewriteflag=true; getdesc(false);
                return true;
        } else {
                /* get new description */
               if (!initflag) {
                        /*skip old description*/
                        rewriteflag=false; getdesc(false);
                }
                fprintf(frewrite,"\n\n%s\n%c",Kdesc,SDELIM);
                if (textfile) {
                        old1='\n';
                        /* copy textfile */
                        if ((txt=fopen(textfile,"r"))!=NULL) {
                                while ((c=getc(txt))!=EOF) {
                                        if (c==SDELIM) putc(c,frewrite); /*double up*/
                                        putc(c,frewrite);
                                        old1=c;
                                }
                                if (old1!='\n') putc('\n',frewrite);
                                fclose(txt);
                                putc(SDELIM,frewrite);fputs("\n\n", frewrite);
                                return true;
                        } else {
                                error("Can't open file with description%s",textfile);
                        }
                }
                if (initflag&&quietflag) {
                        warn("empty descriptive text");
                        putc(SDELIM,frewrite);fputs("\n\n", frewrite);
                        return true;
                }
                /* read text from stdin */
                if (isatty(fileno(stdin))) {
                    fputs("enter description, terminated with ^D or '.':\n",stdout);
                    fputs("NOTE: This is NOT the log message!\n>> ",stdout);
                }
                c = '\0'; old2= '\n';
                if ((old1=getchar())==EOF) {
		    if (isatty(fileno(stdin))) {
			putc('\n',stdout);
			clearerr(stdin);
		    }
		}
                else for (;;) {
                            c=getchar();
                            if (c==EOF) {
                                    if (isatty(fileno(stdin))) {
					putc('\n',stdout);
					clearerr(stdin);
				    }
                                    putc(old1,frewrite);
                                    if (old1!='\n') putc('\n',frewrite);
                                    break;
                            }
                            if (c=='\n' && old1=='.' && old2=='\n') {
                                    break;
                            }
                            if (c=='\n' && isatty(fileno(stdin))) fputs(">> ",stdout);
                            if(old1==SDELIM) putc(old1,frewrite); /* double up*/
                            putc(old1,frewrite);
                            old2=old1;
                            old1=c;
                    } /* end for */
                putc(SDELIM,frewrite);fputs("\n\n",frewrite);
                return true;
        }
}
