/*
 *                     RCS rcsdiff operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /usr/src/new/rcs/src/RCS/rcsdiff.c,v 3.9 88/02/18 11:55:57 bostic Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       generate difference between RCS revisions
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


/* $Log:	rcsdiff.c,v $
 * Revision 3.9  88/02/18  11:55:57  bostic
 * replaced with version 4
 * 
 * Revision 4.4  87/12/18  11:37:46  narten
 * changes Jay Lepreau made in the 4.3 BSD version, to add support for
 * "-i", "-w", and "-t" flags and to permit flags to be bundled together, 
 * merged in.
 * 
 * Revision 4.3  87/10/18  10:31:42  narten
 * Updating version numbers. Changes relative to 1.1 actually
 * relative to 4.1
 * 
 * Revision 1.3  87/09/24  13:59:21  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:15  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:18  kcs
 * Initial revision
 * 
 * Revision 4.1  83/05/03  22:13:19  wft
 * Added default branch, option -q, exit status like diff.
 * Added fterror() to replace faterror().
 * 
 * Revision 3.6  83/01/15  17:52:40  wft
 * Expanded mainprogram to handle multiple RCS files.
 *
 * Revision 3.5  83/01/06  09:33:45  wft
 * Fixed passing of -c (context) option to diff.
 *
 * Revision 3.4  82/12/24  15:28:38  wft
 * Added call to catchsig().
 *
 * Revision 3.3  82/12/10  16:08:17  wft
 * Corrected checking of return code from diff; improved error msgs.
 *
 * Revision 3.2  82/12/04  13:20:09  wft
 * replaced getdelta() with gettree(). Changed diagnostics.
 *
 * Revision 3.1  82/11/28  19:25:04  wft
 * Initial revision.
 *
 */
#include <ctype.h>
#include "rcsbase.h"
#define ERRCODE 2                   /*error code for exit status            */
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif

extern int    cleanup();            /* cleanup after signals                */
extern char * mktempfile();         /*temporary file name generator         */
extern int    fterror();            /*forward for special fatal error func. */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int    nerror;               /*counter for errors                    */
extern int    quietflag;            /*suppresses diagnostics                */
extern FILE * finptr;               /* RCS input file                       */

char *RCSfilename;
char *workfilename;
char * temp1file, * temp2file;

char bops[10] = "-";
char otherops[10] = "-";

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
        char command[NCPPN+revlength+40];
        int  revnums;                 /* counter for revision numbers given */
        char * rev1, * rev2;          /* revision numbers from command line */
        char numericrev[revlength];   /* holds expanded revision number     */
        char * xrev1, * xrev2;        /* expanded revision numbers          */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;
	char * boption, * otheroption;
        int  exit_stats;
        int  filecounter;
	char *argp;
	register c;

        catchints();
        otheroption = otherops + 1;
	boption = bops + 1;
        cmdid = "rcsdiff";
	cmdusage = "command format:\n    rcsdiff [-biwt] [-q] [-cefhn] [-rrev1] [-rrev2] file";
        filecounter=revnums=0;
        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
	    argp = &((*argv)[1]);
	    while (c = *argp++) switch (c) {
                case 'r':
		        if (*argp!='\0') {
                            if (revnums==0) {
                                    rev1= argp; revnums=1;
                            } elif (revnums==1) {
                                    rev2= argp; revnums=2;
                            } else {
				    fterror("too many revision numbers");
                            }
                        } /* do nothing for empty -r */
			argp += strlen(argp);
                        break;
                case 'b':
                case 'i':
                case 'w':
                case 't':
			*boption++ = c;
			break;
		case 'q':
			quietflag=true;
			break;
                case 'c':
                case 'e':
                case 'f':
                case 'h':
                case 'n':
                        if (otheroption == otherops + 1) {
				*otheroption++ = c;
				if (c == 'c' && isdigit(*argp)) {
					while (isdigit(*argp))
						*otheroption++ = *argp++;
					if (*argp)
						faterror("-c: bad count");
					argp = "";
				}
                        } else {
				fterror("Options c,e,f,h,n are mutually exclusive");
                        }
			break;
                default:
			fterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        } /* end of option processing */

	if (boption != bops + 1) {
 	    *boption = ' ';
	    boption = bops;
	}
	if (otheroption != otherops + 1) {
 	    *otheroption = ' ';
	    otheroption = otherops;
	}
	if (argc<1) fterror("No input file\n%s",cmdusage);

        /* now handle all filenames */
        do {
                finptr=NULL;

                if (pairfilenames(argc,argv,true,false)!=1) continue;
                if (++filecounter>1)
                        diagnose("===================================================================");
                diagnose("RCS file: %s",RCSfilename);
                if (revnums<2 && !(access(workfilename,4)==0)) {
                        error("Can't open %s",workfilename);
                        continue;
                }
                if (!trysema(RCSfilename,false)) continue; /* give up */


                gettree(); /* reads in the delta tree */

                if (Head==nil) {
                        error("no revisions present");
                        continue;
                }
                if (revnums==0)
                        rev1=Dbranch!=nil?Dbranch->num:Head->num; /* default rev1 */

                if (!expandsym(rev1,numericrev)) continue;
                if (!(target=genrevs(numericrev,(char *)nil,(char *)nil,(char *)nil,gendeltas))) continue;
                xrev1=target->num;

                if (revnums==2) {
                        if (!expandsym(rev2,numericrev)) continue;
                        if (!(target=genrevs(numericrev,(char *)nil,(char *)nil,(char *)nil,gendeltas))) continue;
                        xrev2=target->num;
                }


                temp1file=mktempfile("/tmp/",TMPFILE1);
                diagnose("retrieving revision %s",xrev1);
                VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                        TARGETDIR,xrev1,RCSfilename,temp1file);
                if (system(command)){
                        error("co failed");
                        continue;
                }
                if (revnums<=1) {
                        temp2file=workfilename;
			diagnose("diff %s%s-r%s %s",boption,otheroption,xrev1,workfilename);
                } else {
                        temp2file=mktempfile("/tmp/",TMPFILE2);
                        diagnose("retrieving revision %s",xrev2);
                        VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                                TARGETDIR,xrev2,RCSfilename,temp2file);
                        if (system(command)){
                                error("co failed");
                                continue;
                        }
                        diagnose("diff %s%s-r%s -r%s",boption,otheroption,xrev1,xrev2);
                }
                VOID sprintf(command,"%s %s%s%s %s\n",DIFF,boption,
                        otheroption, temp1file, temp2file);
                exit_stats = system (command);
                if (exit_stats != 0 && exit_stats != (1 << BYTESIZ)) {
                        error ("diff failed");
                        continue;
                }
        } while (cleanup(),
                 ++argv, --argc >=1);


	if (nerror>0) {
		exit(ERRCODE);
	} else {
		exit(exit_stats>>BYTESIZ);
		/* return exit status from diff */
	}

}


/*VARARGS3*/
fterror(e, e1, e2)
char * e, * e1, * e2;
/* prints error message and terminates program with ERRCODE */
{       nerror++;
        VOID fprintf(stderr,"%s error: ",cmdid);
	VOID fprintf(stderr,e, e1, e2);
        VOID fprintf(stderr,"\n%s aborted\n",cmdid);
        VOID cleanup();
	exit(ERRCODE);
}

