/*
 *                     RCS rcsdiff operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /usr/src/contrib/rcs/src/RCS/rcsdiff.c,v 3.11 89/08/15 21:37:01 bostic Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       generate difference between RCS revisions
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




/* $Log:	rcsdiff.c,v $
 * Revision 3.11  89/08/15  21:37:01  bostic
 * Version 4 from Tom Narten at Purdue
 * 
 * Revision 4.6  89/05/01  15:12:27  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.5  88/11/08  12:01:51  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.5  88/08/09  19:12:41  eggert
 * Use execv(), not system(); yield exit status like diff(1)s; allow cc -R.
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
extern char *rindex();
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif
static char co[] = CO;

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

char bops[10];
char otherops[10];

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
	char commarg[revlength+3];
        int  revnums;                 /* counter for revision numbers given */
        char * rev1, * rev2;          /* revision numbers from command line */
        char numericrev[revlength];   /* holds expanded revision number     */
        char * xrev1, * xrev2;        /* expanded revision numbers          */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;
	char * boption, * otheroption;
        int  exit_stats;
	int  diffs_found;
	char *argp;
	register c;

        catchints();
        otheroption = otherops + 2;
	boption = bops + 2;
        cmdid = "rcsdiff";
	cmdusage = "command format:\n    rcsdiff [-biwt] [-q] [-cefhn] [-rrev1] [-rrev2] file";
	diffs_found=revnums=0;
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
			if (!rindex(bops + 2, c))
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
                        if (otheroption == otherops + 2) {
				*otheroption++ = c;
				if (c == 'c' && isdigit(*argp)) {
					while (isdigit(*argp) && otheroption < otherops+sizeof(otherops)-1)
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

	if (boption != bops + 2) {
	    bops[0] = ' ';
	    bops[1] = '-';
	    boption = bops;
	}
	if (otheroption != otherops + 2) {
 	    otherops[0] = ' ';
	    otherops[1] = '-';
	    otheroption = otherops;
	}
	if (argc<1) fterror("No input file\n%s",cmdusage);

        /* now handle all filenames */
        do {
                finptr=NULL;

                if (pairfilenames(argc,argv,true,false)!=1) continue;
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
                VOID sprintf(commarg,"-p%s",xrev1);
                if (run((char*)nil,temp1file, co,"-q",commarg,RCSfilename,(char*)nil)){
                        error("co failed");
                        continue;
                }
                if (revnums<=1) {
                        temp2file=workfilename;
                        diagnose("diff%s%s -r%s %s",boption,otheroption,xrev1,workfilename);
                } else {
                        temp2file=mktempfile("/tmp/",TMPFILE2);
                        diagnose("retrieving revision %s",xrev2);
			VOID sprintf(commarg,"-p%s",xrev2);
                        if (run((char*)nil,temp2file, co,"-q",commarg,RCSfilename,(char *)nil)){
                                error("co failed");
                                continue;
                        }
                        diagnose("diff%s%s -r%s -r%s",boption,otheroption,xrev1,xrev2);
                }

                exit_stats =
			*boption
			? *otheroption
			  ? run((char*)nil,(char*)nil, DIFF, boption+1,	otheroption+1,	temp1file,temp2file,(char*)nil)
			  : run((char*)nil,(char*)nil, DIFF, boption+1,			temp1file,temp2file,(char*)nil)
			: *otheroption
			  ? run((char*)nil,(char*)nil, DIFF, 		otheroption+1,	temp1file,temp2file,(char*)nil)
			  : run((char*)nil,(char*)nil, DIFF, 				temp1file,temp2file,(char*)nil);

                if (exit_stats == (1 << BYTESIZ))
			diffs_found = 1;
                else if (exit_stats != 0) {
                        error ("diff failed");
                        continue;
                }
        } while (cleanup(),
                 ++argv, --argc >=1);


        exit(nerror ? ERRCODE : diffs_found);
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

