/*
 *                       rcsmerge operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /arthur/src/local/bin/rcs/src/RCS/rcsmerge.c,v 4.3 87/10/18 10:38:02 narten Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       join 2 revisions with respect to a third
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


/* $Log:	rcsmerge.c,v $
 * Revision 4.3  87/10/18  10:38:02  narten
 * Updating version numbers. Changes relative to version 1.1 
 * actually relative to 4.1
 * 
 * Revision 1.3  87/09/24  14:00:31  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:36  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:36  kcs
 * Initial revision
 * 
 * Revision 4.1  83/03/28  11:14:57  wft
 * Added handling of default branch.
 * 
 * Revision 3.3  82/12/24  15:29:00  wft
 * Added call to catchsig().
 *
 * Revision 3.2  82/12/10  21:32:02  wft
 * Replaced getdelta() with gettree(); improved error messages.
 *
 * Revision 3.1  82/11/28  19:27:44  wft
 * Initial revision.
 *
 */
#include "rcsbase.h"
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif

extern int  cleanup();              /* cleanup after signals                */
extern char * mktempfile();         /*temporary file name generator         */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern int  nerror;                 /*counter for errors                    */

char *RCSfilename;
char *workfilename;
char * temp1file, * temp2file;

main (argc, argv)
int argc; char **argv;
{
        char * cmdusage;
        char command[NCPPN+revlength+40];
        int  revnums; /* counter for revision numbers given */
        int tostdout;
        char * rev1, * rev2; /*revision numbers*/
        char numericrev[revlength];   /* holds expanded revision number     */
        struct hshentry * gendeltas[hshsize];/*stores deltas to be generated*/
        struct hshentry * target;

        catchints();
        cmdid = "rcsmerge";
        cmdusage = "command format:\n    rcsmerge -p -rrev1 -rrev2 file\n    rcsmerge -p -rrev1 file";
        revnums=0;tostdout=false;

        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {
                case 'p':
                        tostdout=true;
                        /* falls into -r */
                case 'r':
                        if ((*argv)[2]!='\0') {
                            if (revnums==0) {
                                    rev1= *argv+2; revnums=1;
                            } elif (revnums==1) {
                                    rev2= *argv+2; revnums=2;
                            } else {
                                    faterror("too many revision numbers");
                            }
                        } /* do nothing for empty -r or -p */
                        break;

                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        } /* end of option processing */

        if (argc<1) faterror("No input file\n%s",cmdusage);
        if (revnums<1) faterror("no base revision number given");

        /* now handle all filenames */

        if (pairfilenames(argc,argv,true,false)==1) {

                if (argc>2 || (argc==2&&argv[1]!=nil))
                        warn("too many arguments");
                diagnose("RCS file: %s",RCSfilename);
                if (!(access(workfilename,4)==0))
                        faterror("Can't open %s",workfilename);

                if (!trysema(RCSfilename,false)) goto end; /* give up */

                gettree();  /* reads in the delta tree */

                if (Head==nil) faterror("no revisions present");


                if (!expandsym(rev1,numericrev)) goto end;
                if (!(target=genrevs(numericrev, (char *)nil, (char *)nil, (char *)nil,gendeltas))) goto end;
                rev1=target->num;
                if (revnums==1)  /*get default for rev2 */
                        rev2=Dbranch!=nil?Dbranch->num:Head->num;
                if (!expandsym(rev2,numericrev)) goto end;
                if (!(target=genrevs(numericrev, (char *)nil, (char *)nil, (char *)nil,gendeltas))) goto end;
                rev2=target->num;

                temp1file=mktempfile("/tmp/",TMPFILE1);
                temp2file=mktempfile("/tmp/",TMPFILE2);

                diagnose("retrieving revision %s",rev1);
                VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                        TARGETDIR,rev1,RCSfilename,temp1file);
                if (system(command)){
                        faterror("co failed");
                }
                diagnose("retrieving revision %s",rev2);
                VOID sprintf(command,"%s/co -q -p%s %s > %s\n",
                         TARGETDIR,rev2,RCSfilename,temp2file);
                if (system(command)){
                        faterror("co failed");
                }
                diagnose("Merging differences between %s and %s into %s%s",
                         rev1, rev2, workfilename,
                         tostdout?"; result to stdout":"");

                VOID sprintf(command,"%s %s%s %s %s %s %s\n",MERGE,tostdout?"-p ":"",
                        workfilename,temp1file,temp2file,workfilename,rev2);
                if (system(command)) {
                        faterror("merge failed");
                }
        }

end:
        VOID cleanup();
        exit(nerror!=0);

}

