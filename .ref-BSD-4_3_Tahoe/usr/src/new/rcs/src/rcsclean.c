/*
 *                     RCS rcsclean operation
 */
static char rcsid[]=
"$Header $ Purdue CS";
/*****************************************************************************
 *                       remove unneded working files
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


/* $Log:	rcsclean.c,v $
 * Revision 4.2  87/10/18  10:30:43  narten
 * Updating version numbers. Changes relative to 1.1 are actually
 * relative to 4.1
 * 
 * Revision 1.2  87/09/24  13:59:13  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.1  84/01/23  14:50:16  kcs
 * Initial revision
 * 
 * Revision 4.1  83/12/15  12:26:18  wft
 * Initial revision.
 * 
 */
#include "rcsbase.h"
#define ERRCODE 2                   /*error code for exit status            */
static char rcsbaseid[] = RCSBASE;

extern int    cleanup();            /* cleanup after signals                */
extern char * mktempfile();         /*temporary file name generator         */
extern int    fterror();            /*forward for special fatal error func. */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int    nerror;               /*counter for errors                    */
extern int    quietflag;            /*suppresses diagnostics                */
extern FILE * finptr;               /* RCS input file                       */
extern FILE * fopen();

char *RCSfilename;
char *workfilename;
char * tempfile;
FILE * file1, * file2;              /*file descriptors for comparison       */

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
        char command[NCPPN+revlength+40];
	char * rev;                   /* revision number from command line  */
        char numericrev[revlength];   /* holds expanded revision number     */
	int  revnums;                 /* number of -r options               */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;
        int  filecounter;
	register int c1;              /* reading input                      */
	int  result;                  /* result of comparison               */
	int pairresult;               /* reulst of pairfilenames            */

        catchints();
	cmdid = "rcsclean";
	cmdusage = "command format:\n    rcsclean [-rrev] file";
        filecounter=revnums=0;
	quietflag=true; /* default no diagnostics */
        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {
                case 'r':
		revno:  if ((*argv)[2]!='\0') {
                            if (revnums==0) {
				    rev= *argv+2; revnums=1;
                            } else {
				    fterror("too many revision numbers");
                            }
                        } /* do nothing for empty -r */
                        break;
		case 'D': /* debug option */
			quietflag = false;
			break;

                default:
			fterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        } /* end of option processing */

	if (argc<1) fterror("No input file\n%s",cmdusage);

        /* now handle all filenames */
        do {
                finptr=NULL;
		pairresult=pairfilenames(argc,argv,false,false);

		if (pairresult==0) continue; /* error */
		if (!(access(workfilename,4)==0)) {
			diagnose("Can't open %s",workfilename);
                        continue;
		} elsif (pairresult == -1) {
			warn("Can't find RCS file for %s",workfilename);
			continue;
		}
                diagnose("RCS file: %s",RCSfilename);
                if (!trysema(RCSfilename,false)) continue; /* give up */


                gettree(); /* reads in the delta tree */

                if (Head==nil) {
                        error("no revisions present");
                        continue;
                }
                if (revnums==0)
			rev=(Dbranch!=nil?Dbranch->num:Head->num); /* default rev1 */

		if (!expandsym(rev,numericrev)) continue;
                if (!(target=genrevs(numericrev,nil,nil,nil,gendeltas))) continue;

		tempfile=mktempfile("/tmp/",TMPFILE1);
		diagnose("retrieving revision %s",target->num);
                sprintf(command,"%s/co -q -p%s %s > %s\n",
			TARGETDIR,target->num,RCSfilename,tempfile);
                if (system(command)){
                        error("co failed");
                        continue;
                }
		/* now do comparison */
		if ((file1=fopen(tempfile,"r"))==NULL) {
			error("Can't open checked out file %s",tempfile);
			continue;
		}
		if ((file2=fopen(workfilename,"r"))==NULL) {
			error("Can't open %s",workfilename);
			continue;
		}
		result=1;
		while ((c1=getc(file1))==getc(file2)) {
			if (c1==EOF) {
				/* identical files; can remove working file */
				result=0;
				diagnose("files identical; %s removed",workfilename);
				if (unlink(workfilename)!=0) {
					error("Can't unlink %s",workfilename);
				}
				break;
			}
		}
		fclose(file1); fclose(file2);

		if (result==1) diagnose ("files different");


        } while (cleanup(),
                 ++argv, --argc >=1);


	if (nerror>0) {
		exit(ERRCODE);
	} else {
		exit(result);
	}

}


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

