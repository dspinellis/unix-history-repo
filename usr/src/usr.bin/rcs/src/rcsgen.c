/*
 *                     RCS revision generation
 */
#ifndef lint
static char rcsid[]= "$Id: rcsgen.c,v 3.8 89/08/15 21:38:51 bostic Exp $ Purdue CS";
#endif

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



/* $Log:	rcsgen.c,v $
 * Revision 3.8  89/08/15  21:38:51  bostic
 * Version 4 from Tom Narten at Purdue
 * 
 * Revision 4.7  89/05/01  15:12:49  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.6  88/11/08  12:01:13  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.6  88/08/28  14:59:10  eggert
 * Shrink stdio code size; allow cc -R; remove lint; isatty() -> ttystdin()
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
extern FILE * fopen();
extern savestring();
extern editstring();

extern int nextc;          /* next character from lexical analyzer          */
extern char Ktext[];       /* keywords from syntax analyzer                 */
extern char Klog[];        /* Keyword "log"                                 */
extern char Kdesc[];       /* Keyword for description                       */
extern FILE * frewrite;    /* new RCS file                                  */
extern FILE * fcopy;       /* result file during editing                    */
extern char * resultfile;  /* file name for fcopy                           */
extern int    rewriteflag; /* indicates whether to rewrite the input file   */


char    curlogmsg[logsize]; /* buffer for current log message                */

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


int stdinread; /* stdinread>0 if redirected stdin has been read once */

int ttystdin()
{
	static int initialized, istty;
	if (!initialized) {
		istty = isatty(fileno(stdin));
		initialized = 1;
	}
	return istty;
}

putdesc(initflag,textflag,textfile,quietflag)
int initflag,textflag; char * textfile; int quietflag;
/* Function: puts the descriptive text into file frewrite.
 * if !initflag && !textflag, the text is copied from the old description.
 * Otherwise, if the textfile!=nil, the text is read from that
 * file, or from stdin, if textfile==nil.
 * Increments stdinread if text is read from redirected stdin.
 * if initflag&&quietflag&&!textflag, an empty text is inserted.
 * if !initflag, the old descriptive text is discarded.
 */
{       register FILE * txt; register int c, old1, old2;
	register FILE * frew;
#ifdef lint
	if (quietflag ==  0) initflag = quietflag; /* silencelint */
#endif

	frew = frewrite;
        if (!initflag && !textflag) {
                /* copy old description */
                VOID fprintf(frew,"\n\n%s%c",Kdesc,nextc);
                rewriteflag=true; getdesc(false);
        } else {
                /* get new description */
               if (!initflag) {
                        /*skip old description*/
                        rewriteflag=false; getdesc(false);
                }
                VOID fprintf(frew,"\n\n%s\n%c",Kdesc,SDELIM);
                if (textfile) {
                        old1='\n';
                        /* copy textfile */
                        if ((txt=fopen(textfile,"r"))!=NULL) {
                                while ((c=getc(txt))!=EOF) {
                                        if (c==SDELIM) VOID putc(c,frew); /*double up*/
                                        VOID putc(c,frew);
                                        old1=c;
                                }
                                if (old1!='\n') VOID putc('\n',frew);
                                VOID fclose(txt);
				VOID putc(SDELIM,frew);
				VOID fputs("\n\n", frew);
				return;
                        } else {
                                error("Can't open file %s with description",textfile);
                                if (!ttystdin()) return;
                                /* otherwise, get description from terminal */
                        }
                }
                /* read text from stdin */
                if (ttystdin()) {
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
                        if (ttystdin()) {
                             VOID putc('\n',stderr);
                             clearerr(stdin);
			}
		} else {
		     if (old1=='\n' && ttystdin())
			 VOID fputs(">> ",stderr);
		     for (;;) {
                            c=getchar();
                            if (c==EOF) {
                                    if (ttystdin()) {
                                            VOID putc('\n',stderr);
                                            clearerr(stdin);
				    }
                                    VOID putc(old1,frew);
                                    if (old1!='\n') VOID putc('\n',frew);
                                    break;
                            }
                            if (c=='\n' && old1=='.' && old2=='\n') {
                                    break;
                            }
                            if (c=='\n' && ttystdin()) VOID fputs(">> ",stderr);
			    if(old1==SDELIM) VOID putc(old1,frew); /* double up*/
			    VOID putc(old1,frew);
                            old2=old1;
                            old1=c;
                    } /* end for */
		}
		VOID putc(SDELIM,frew); VOID fputs("\n\n",frew);
        }
}
