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

/*
 *                     RCS checkin operation
 */
#ifndef lint
 static char rcsid[]=
 "$Header: /usr/src/local/bin/rcs/src/RCS/ci.c,v 4.9 89/05/01 15:10:54 narten Exp $ Purdue CS";
#endif
/*******************************************************************
 *                       check revisions into RCS files
 *******************************************************************
 */



/* $Log:	ci.c,v $
 * Revision 4.9  89/05/01  15:10:54  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.8  88/11/08  13:38:23  narten
 * changes from root@seismo.CSS.GOV (Super User)
 * -d with no arguments uses the mod time of the file it is checking in
 * 
 * Revision 4.7  88/11/08  10:59:04  narten
 * changes from eggert
 * 
 * Revision 4.7  88/08/09  19:12:07  eggert
 * Make sure workfile is a regular file; use its mode if RCSfile doesn't have one.
 * Use execv(), not system(); allow cc -R; remove lint.
 * isatty(fileno(stdin)) -> ttystdin()
 * 
 * Revision 4.6  87/12/18  11:34:41  narten
 * lint cleanups (from Guy Harris)
 * 
 * Revision 4.5  87/10/18  10:18:48  narten
 * Updating version numbers. Changes relative to revision 1.1 are actually
 * relative to 4.3
 * 
 * Revision 1.3  87/09/24  13:57:19  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:21:33  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:49:54  kcs
 * Initial revision
 * 
 * Revision 4.3  83/12/15  12:28:54  wft
 * ci -u and ci -l now set mode of working file properly.
 * 
 * Revision 4.2  83/12/05  13:40:54  wft
 * Merged with 3.9.1.1: added calls to clearerr(stdin).
 * made rewriteflag external.
 * 
 * Revision 4.1  83/05/10  17:03:06  wft
 * Added option -d and -w, and updated assingment of date, etc. to new delta.
 * Added handling of default branches.
 * Option -k generates std. log message; fixed undef. pointer in reading of log.
 * Replaced getlock() with findlock(), link--unlink with rename(),
 * getpwuid() with getcaller().
 * Moved all revision number generation to new routine addelta().
 * Removed calls to stat(); now done by pairfilenames().
 * Changed most calls to catchints() with restoreints().
 * Directed all interactive messages to stderr.
 * 
 * Revision 3.9.1.1  83/10/19  04:21:03  lepreau
 * Added clearerr(stdin) to getlogmsg() for re-reading stdin.
 * 
 * Revision 3.9  83/02/15  15:25:44  wft
 * 4.2 prerelease
 * 
 * Revision 3.9  83/02/15  15:25:44  wft
 * Added call to fastcopy() to copy remainder of RCS file.
 *
 * Revision 3.8  83/01/14  15:34:05  wft
 * Added ignoring of interrupts while new RCS file is renamed;
 * Avoids deletion of RCS files by interrupts.
 *
 * Revision 3.7  82/12/10  16:09:20  wft
 * Corrected checking of return code from diff.
 *
 * Revision 3.6  82/12/08  21:34:49  wft
 * Using DATEFORM to prepare date of checked-in revision;
 * Fixed return from addbranch().
 *
 * Revision 3.5  82/12/04  18:32:42  wft
 * Replaced getdelta() with gettree(), SNOOPDIR with SNOOPFILE. Updated
 * field lockedby in removelock(), moved getlogmsg() before calling diff.
 *
 * Revision 3.4  82/12/02  13:27:13  wft
 * added option -k.
 *
 * Revision 3.3  82/11/28  20:53:31  wft
 * Added mustcheckin() to check for redundant checkins.
 * Added xpandfile() to do keyword expansion for -u and -l;
 * -m appends linefeed to log message if necessary.
 * getlogmsg() suppresses prompt if stdin is not a terminal.
 * Replaced keeplock with lockflag, fclose() with ffclose(),
 * %02d with %.2d, getlogin() with getpwuid().
 *
 * Revision 3.2  82/10/18  20:57:23  wft
 * An RCS file inherits its mode during the first ci from the working file,
 * otherwise it stays the same, except that write permission is removed.
 * Fixed ci -l, added ci -u (both do an implicit co after the ci).
 * Fixed call to getlogin(), added call to getfullRCSname(), added check
 * for write error.
 * Changed conflicting identifiers.
 *
 * Revision 3.1  82/10/13  16:04:59  wft
 * fixed type of variables receiving from getc() (char -> int).
 * added include file dbm.h for getting BYTESIZ. This is used
 * to check the return code from diff portably.
 */

#include "rcsbase.h"
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include "time.h"

extern int    rename();                /*rename files                       */
extern char * getcaller();             /*login of caller                    */
extern struct hshentry * genrevs();    /*generate delta numbers             */
extern quietflag;                      /*suppresses diagnostics if true     */
extern int  nerror;                    /*counter for errors                 */
extern char * buildrevision();         /*constructs desired revision        */
extern char * checkid();               /*check identifiers                  */
extern int    partime();               /*parse free-format date/time        */
extern long   maketime();              /*convert parsed time to unix time.  */
extern long   time();                  /*get date and time                  */
extern struct tm * localtime();        /*convert unixtime into tm-structure */
extern char * getdate();               /*formates current date  (forward)   */
extern char * mktempfile();            /*temporary file name generator      */
extern struct lock * addlock();        /*adds a new lock                    */
extern char * getlogmsg();             /*obtains log message; forward       */
extern struct hshentry * removelock(); /*finds a caller's lock  (forward)   */
extern struct hshentry * findlock();   /*finds a lock                       */
extern char * xpandfile();             /*perform keyword expansion; forward */

extern char prevauthor[];
extern char prevdate[];
extern char prevrev[];
extern char prevstate [];
extern FILE * finptr;                  /* RCS input file                    */
extern FILE * frewrite;                /* new RCS file                      */
extern int    rewriteflag;             /* indicates whether input should be */
				       /* echoed to frewrite                */

char * newRCSfilename, * diffilename;
char * RCSfilename,*workfilename,*expfilename,*newworkfilename;
extern struct stat RCSstat, workstat; /* file status of RCS and work file   */
extern int  haveRCSstat, haveworkstat;/* status indicators                  */


int    copyflag;    /* indicates whether a string should be copied into memory*/

char * rev, * state, *msg;

int initflag, rcsinitflag;
int lockflag, keepworkingfile,keepflag;
int forceciflag;                      /* forces check in                    */
int symrebindflag; char * symbol;
int textflag; char * textfile;
char * caller;                        /* caller's login;                    */
char * author;                        /* alternate author for -w option     */
char altdate[datelength];             /* alternate date for -d              */
int usestatdate;		      /* use mod time of file for -d 	    */
struct hshentry * targetdelta;        /* old delta to be generated          */
char   * olddeltanum;                 /* number of old delta                */
struct hshentry * gendeltas[hshsize]; /* stores deltas to be generated      */
char   newdelnum[revlength];          /* holds new revision number          */
int    newdnumlength;                 /* actual length of new rev. num.     */
char   branchpointnum[revlength];     /* number of branchpoint              */
struct hshentry newdelta;             /* new delta to be inserted           */
struct branchhead newbranch;          /* new branch to be inserted          */
char   logmsg[logsize];               /* buffer for log message             */

main (argc, argv)
int argc;
char * argv[];
{
	char * nametest;
        char * cmdusage;         /* holds command format                    */
        struct stat filestatus;  /* used for getting the mode               */
        int  msglen;             /* length of message given by -m           */
	int exit_stats;		 /* return code for command invocations     */
	int newRCSmode;          /* mode for RCS file                       */
	long unixtime;
	struct tm  parseddate, *ftm;

	catchints();
        cmdid = "ci";
        cmdusage = "command format:\nci -r[rev] -l[rev] -u[rev] -f[rev] -k[rev] -q[rev] -mmsg -nname -Nname -sstate -t[txtfile] file ...";
	rev = state = msg = symbol = textfile = nil;
        initflag= rcsinitflag= symrebindflag= textflag= quietflag= false;
        forceciflag= lockflag= keepworkingfile= keepflag= false;
	caller = getcaller(); author = nil; /* author may be reset by -w */
	altdate[0]= '\0'; /* empty alternate date for -d */
	usestatdate=false;

        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {

                case 'r':
                        lockflag=false;
                revno:  if ((*argv)[2]!='\0') {
                                if (rev!=nil) warn("Redefinition of revision number");
                                rev = (*argv)+2;
                        }
                        break;

                case 'l':
                        keepworkingfile=lockflag=true;
                        goto revno;

                case 'u':
                        keepworkingfile=true; lockflag=false;
                        goto revno;

                case 'q':
                        quietflag=true;
                        goto revno;

                case 'f':
                        forceciflag=true;
                        goto revno;

                case 'k':
                        keepflag=true;
                        goto revno;

                case 'm':
                        if ((*argv)[2]!='\0'){
                                if (msg!=nil)warn("Redefinition of -m option");
                                msg = (*argv)+2;
                                msglen=strlen(msg);
                                if (msglen >= logsize) {
                                   warn("log message truncated to %d characters",
                                        logsize);
                                   msg[logsize-2]='\n';
                                   msg[logsize-1]='\0';
                                }
                                if (msg[msglen-1]!='\n') {
                                   /*append linefeed*/
                                   VOID strcpy(logmsg,msg);msg=logmsg;
                                   msg[msglen]  = '\n';
                                   msg[++msglen]= '\0';
                                }
                        } else warn("Missing message for -m option");
                        break;

                case 'n':
                        symrebindflag=false;
                        if ((*argv)[2]!='\0'){
                                if (symbol!=nil)warn("Redefinition of symbolic name");
                                symbol = (*argv)+2;
				if (!(nametest=checkid(symbol,' '))||*nametest)
					faterror("Name %s must be one word",symbol);
                        } else warn("Missing name for -n option");
                        break;

                case 'N':
                        symrebindflag=true;
                        if ((*argv)[2]!='\0'){
                                if (symbol!=nil)warn("Redefinition of symbolic name");
                                symbol = (*argv)+2;
				if (!(nametest=checkid(symbol,' '))||*nametest)
					faterror("Name %s must be one word",symbol);
                        } else warn("Missing name for -N option");
                        break;

                case 's':
                        if ((*argv)[2]!='\0'){
                                if (state!=nil)warn("Redefinition of -s option");
                                state = (*argv)+2;
                                VOID checkid(state,' ');
                        } else warn("Missing state for -s option");
                        break;

                case 't':
                        textflag=true;
                        if ((*argv)[2]!='\0'){
                                if (textfile!=nil)warn("Redefinition of -t option");
                                textfile = (*argv)+2;
                        }
                        break;

		case 'd':
                        if ((*argv)[2]!='\0'){
				if (altdate[0]!='\0' || usestatdate==true)
				    warn("Redefinition of -d option");
				/* process the date */
				if ( partime((*argv)+2, &parseddate) == 0) {
				    faterror("Can't parse date/time: %s", (*argv)+2);
				    break;
				}
				if ( (unixtime = maketime(&parseddate)) == 0L) {
				    faterror("Inconsistent date/time: %s",(*argv)+2);
				    break;
				}
				ftm = localtime(&unixtime);
				VOID sprintf(altdate,DATEFORM,
				ftm->tm_year,ftm->tm_mon+1,ftm->tm_mday,ftm->tm_hour,ftm->tm_min,ftm->tm_sec);
			} else 
				usestatdate++;
                        break;

		case 'w':
                        if ((*argv)[2]!='\0'){
				if (author!=nil)warn("Redefinition of -w option");
				author = (*argv)+2;
				VOID checkid(author,' ');
			} else warn("Missing author for -w option");
                        break;




                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);
                };
        }  /* end processing of options */

        if (argc<1) faterror("No input file\n%s",cmdusage);

        if (!ttystdin() && msg==nil && textflag && textfile==nil) {
                /* would need both log message and descriptive text from a file */
                faterror("Can't take both log and description from redirected stdin; use -ttextfile");
        }
        /* now handle all filenames */
        do {
        gendeltas[0] = nil;
        copyflag=rewriteflag=false;
        finptr=frewrite=NULL;
        targetdelta=nil;
        olddeltanum=nil;

        switch (pairfilenames(argc,argv,false,false)) {

        case -1:                /* New RCS file */
                initflag=true; rcsinitflag=false;
                break;

        case 0:                 /* Error */
                continue;

        case 1:                 /* Normal checkin with prev . RCS file */
                initflag=false; rcsinitflag=(Head==nil);
        }

        /* now RCSfilename contains the name of the RCS file, and
         * workfilename contains the name of the working file.
         * if !initflag, finptr contains the file descriptor for the
         * RCS file. The admin node is initialized.
         * workstat and RCSstat are set.
         */

        diagnose("%s  <--  %s", RCSfilename,workfilename);

        if (access(workfilename,4)!=0) {
                error("working file %s not readable or nonexistent",
                       workfilename);
                continue;
        }

        /*
         * make sure workfile is a regular file.
         */
        VOID stat(workfilename, &filestatus);
        if ((filestatus.st_mode & S_IFMT) != S_IFREG) {
                error("working file %s isn't a regular file", workfilename);
                continue;
        }

        /*
         * if RCSfile doesn't exist, use mode from workfile, otherwise
         * keep the one from the RCSfile.
         */
        if (! (initflag || rcsinitflag))
            VOID fstat(fileno(finptr), &filestatus);

        if (!trydiraccess(RCSfilename)) continue; /* give up */
        if (!initflag && !checkaccesslist(caller))   continue; /* give up */
        if (!trysema(RCSfilename,true)) continue; /* give up */

        if (keepflag) {
                /* get keyword values from working file */
                if (!getoldkeys(workfilename)) continue;
                if (rev==nil && *(rev=prevrev)=='\0') {
                        error("Can't find a revision number in %s",workfilename);
                        continue;
                }
		if (*prevdate=='\0' && *altdate=='\0' && usestatdate==false)
			warn("Can't find a date in %s",workfilename);
		if (*prevauthor=='\0' && author==nil)
                        warn("Can't find an author in %s", workfilename);
		if (*prevstate=='\0' && state==nil)
                        warn("Can't find a state in %s", workfilename);
        } /* end processing keepflag */

        gettree(); /* reads in the delta tree.*/

        /* expand symbolic revision number */
        if (!expandsym(rev,newdelnum)) continue;

        /* splice new delta into tree */
        if (!addelta()) continue;

        if (initflag||rcsinitflag) {
                diagnose("initial revision: %s",newdelnum);
        } else  diagnose("new revision: %s; previous revision: %s",
                newdelnum,olddeltanum);

        newdelta.num=newdelnum;
        newdelta.branches=nil;
        newdelta.log=nil;
        newdelta.lockedby=nil; /*might be changed by addlock() */
	/* set author */
	if (author!=nil)
		newdelta.author=author;     /* set author given by -w         */
	elsif (keepflag && *prevauthor!='\0')
		newdelta.author=prevauthor; /* preserve old author of possible*/
	else    newdelta.author=caller;     /* otherwise use caller's id      */
	if (state!=nil)
		newdelta.state=state;       /* set state given by -s          */
	elsif (keepflag && *prevstate!='\0')
		newdelta.state=prevstate;   /* preserve old state if possilbe */
	else    newdelta.state=DEFAULTSTATE;/* otherwise use default state    */
	if (usestatdate==true) {
	    if(haveworkstat<0) {
		error("can't stat %s",workfilename);
		continue;
	    } 
	    ftm = localtime(&workstat.st_mtime);
	    VOID sprintf(altdate,DATEFORM,ftm->tm_year,ftm->tm_mon+1,
	    ftm->tm_mday,ftm->tm_hour,ftm->tm_min,ftm->tm_sec);
	}
	if (*altdate!='\0')
		newdelta.date=altdate;      /* set date given by -d           */
	elsif (keepflag && *prevdate!='\0') /* preserve old date if possible  */
                newdelta.date  =prevdate;
	else
		newdelta.date = getdate();  /* use current date               */
	/* now check validity of date -- needed because of -d and -k          */
	if (targetdelta!=nil &&
	    cmpnum(newdelta.date,targetdelta->date)<=0) {
		error("Date %s is not later than %s in existing revision %s",
		       newdelta.date,targetdelta->date, targetdelta->num);
		continue;
	}


        if (lockflag && !addlock(&newdelta,caller)) continue;
        if (symbol && !addsymbol(&newdelta,symbol,symrebindflag)) continue;

        /* prepare for rewriting the RCS file */
        newRCSfilename=mktempfile(RCSfilename,NEWRCSFILE);
        if ((frewrite=fopen(newRCSfilename, "w"))==NULL) {
                error("Can't open file %s",newRCSfilename);
                continue;
        }
        putadmin(frewrite);
        puttree(Head,frewrite);
        putdesc(initflag,textflag,textfile,quietflag);


        /* build rest of file */
        if (initflag||rcsinitflag) {
                /* get logmessage */
                newdelta.log=getlogmsg();
                if(!putdtext(newdelnum,newdelta.log,workfilename,frewrite)) continue;
                ffclose(frewrite); frewrite=NULL;
        } else {
                diffilename=mktempfile("/tmp/",DIFFILE);
                if (&newdelta==Head) {
                        /* prepend new one */
                        rewriteflag=false;
                        if (!(expfilename=
                              buildrevision(gendeltas,targetdelta,"/tmp/",false))) continue;
                        if (!mustcheckin(expfilename,targetdelta)) continue;
                                /* don't check in files that aren't different, unless forced*/
                        newdelta.log=getlogmsg();
                        exit_stats = run((char*)nil, diffilename,
				DIFF,"-n",workfilename,expfilename, (char*)nil);
                        if (exit_stats != 0 && exit_stats != (1 << BYTESIZ))
                            faterror ("diff failed");
                        /* diff returns 2 in the upper byte on failure */
                        if(!putdtext(newdelnum,newdelta.log,workfilename,frewrite)) continue;
                        if(!putdtext(olddeltanum,targetdelta->log,diffilename,frewrite)) continue;
                } else {
                        /* insert new delta text */
                        rewriteflag=true;
                        if (!(expfilename=
                              buildrevision(gendeltas,targetdelta,"/tmp/",false))) continue;
                        if (!mustcheckin(expfilename,targetdelta)) continue;
                                /* don't check in files that aren't different, unless forced*/
                        newdelta.log=getlogmsg();
                        exit_stats = run((char*)nil, diffilename,
				DIFF,"-n",expfilename,workfilename, (char*)nil);
                        if (exit_stats != 0 && exit_stats != (1 << BYTESIZ))
                            faterror ("diff failed");
                        if(!putdtext(newdelnum,newdelta.log,diffilename,frewrite)) continue;
                }

                /* rewrite rest of RCS file */
                fastcopy(finptr,frewrite);
                ffclose(frewrite); frewrite=NULL;
        }
	ignoreints();
        if (rename(newRCSfilename,RCSfilename)<0) {
                error("Can't write new RCS file %s; saved in %s",
                      RCSfilename,newRCSfilename);
                newRCSfilename[0]='\0'; /* avoid deletion by cleanup*/
                restoreints();
                VOID cleanup();
                break;
        }
        newRCSfilename[0]='\0'; /* avoid re-unlinking by cleanup()*/

	newRCSmode= (initflag|rcsinitflag?workstat.st_mode:RCSstat.st_mode)& ~0222;
	/* newRCSmode is also used to adjust mode of working file for -u and -l */
	if (chmod(RCSfilename,newRCSmode)<0)
                warn("Can't set mode of %s",RCSfilename);

        restoreints();
#       ifdef SNOOPFILE
        logcommand("ci",&newdelta,gendeltas,caller);
#       endif

        if (!keepworkingfile) {
                VOID unlink(workfilename); /* get rid of old file */
        } else {
                /* expand keywords in file */
                newworkfilename=
                xpandfile(workfilename,workfilename /*for directory*/,&newdelta);
                if (!newworkfilename) continue; /* expand failed */
		ignoreints();
		if (rename(newworkfilename,workfilename) <0) {
                    error("Can't expand keywords in %s",workfilename);
                    restoreints();
                    continue;
                }
		newworkfilename[0]='\0'; /* avoid re-unlink by cleanup */
		if (chmod(workfilename, WORKMODE(newRCSmode))<0)
                    warn("Can't adjust mode of %s",workfilename);
                restoreints();
        }
        diagnose("done");

        } while (cleanup(),
                 ++argv, --argc >=1);

        exit(nerror!=0);
	/*NOTREACHED*/
}       /* end of main (ci) */
/*****************************************************************/
/* the rest are auxiliary routines                               */


int addelta()
/* Function: Appends a delta to the delta tree, whose number is
 * given by newdelnum[]. Updates Head, newdelnum, newdenumlength,
 * olddeltanum and the links in newdelta.
 * Retruns false on error, true on success.
 */
{
        register char * sp, * tp;
        register int i;

        newdnumlength=countnumflds(newdelnum);

        if (initflag || rcsinitflag ) {
                /* this covers non-existing RCS file and a file initialized with rcs -i */
		if ((newdnumlength==0)&&(Dbranch!=nil)) {
			VOID strcpy(newdelnum,Dbranch->num);
			newdnumlength=countnumflds(newdelnum);
		}
                if (newdnumlength==0) VOID strcpy(newdelnum,"1.1");
                elsif (newdnumlength==1) VOID strcat(newdelnum,".1");
                elsif (newdnumlength>2) {
                    error("Branch point does not exist for %s",newdelnum);
                    return false;
                } /* newdnumlength == 2 is OK;  */
                olddeltanum=nil;
                Head = &newdelta;
                newdelta.next=nil;
                return true;
        }
        if (newdnumlength==0) {
                /* derive new revision number from locks */
		targetdelta=findlock(caller,true); /*find and delete it*/
                if (targetdelta) {
                    /* found an old lock */
                    olddeltanum=targetdelta->num;
                    /* check whether locked revision exists */
                    if (!genrevs(olddeltanum,(char *)nil,(char *)nil,(char *)nil,gendeltas)) return false;
                    if (targetdelta==Head) {
                        /* make new head */
                        newdelta.next=Head;
                        Head= &newdelta;
                        incnum(olddeltanum, newdelnum);
                    } elsif ((targetdelta->next==nil)&&(countnumflds(olddeltanum)>2)) {
                        /* new tip revision on side branch */
                        targetdelta->next= &newdelta;
                        newdelta.next = nil;
                        incnum(olddeltanum, newdelnum);
                    } else {
                        /* middle revision; start a new branch */
                        newdelnum[0]='\0';
                        if (!addbranch(targetdelta,newdelnum)) return false;
                    }
                    return true; /* successfull use of existing lock */
                } else {
                    /* no existing lock; try Dbranch */
                    /* update newdelnum */
                    if (!((StrictLocks==false) && (getuid() == RCSstat.st_uid))) {
                        error("no lock set by %s",caller);
                        return false;
                    }
                    if (Dbranch) {
                        VOID strcpy(newdelnum,Dbranch->num);
                    } else {
                        incnum(Head->num,newdelnum);
                    }
                    newdnumlength=countnumflds(newdelnum);
                    /* now fall into next statement */
                }
        }
        if (newdnumlength<=2) {
                /* add new head per given number */
                olddeltanum=Head->num;
                if(newdnumlength==1) {
                    /* make a two-field number out of it*/
                    if (cmpnumfld(newdelnum,olddeltanum,1)==0)
                          incnum(olddeltanum,newdelnum);
                    else  VOID strcat(newdelnum, ".1");
                }
                if (cmpnum(newdelnum,olddeltanum) <= 0) {
                    error("deltanumber %s too low; must be higher than %s",
                          newdelnum,Head->num);
                    return false;
                }
                if (!(targetdelta=removelock(caller,Head))) return false;
                if (!(genrevs(olddeltanum,(char *)nil,(char *)nil,(char *)nil,gendeltas))) return false;
                newdelta.next=Head;
                Head= &newdelta;
        } else {
                /* put new revision on side branch */
                /*first, get branch point */
                tp=branchpointnum; sp=newdelnum;
                for(i=newdnumlength-(newdnumlength%2==1?1:2);i>0;i--) {
                    while (*sp != '.') *tp++ = *sp++; /*copy field*/
                    *tp++ = *sp++;                    /*copy dot  */
                }
                *(tp-1) = '\0'; /* kill final dot */
                olddeltanum=branchpointnum; /*temporary old delta*/
                if (!(targetdelta=genrevs(branchpointnum,(char *)nil,(char *)nil,(char *)nil,gendeltas)))
                     return false;
                if (cmpnum(targetdelta->num,branchpointnum)!=0) {
                    error("Cannot find branchpoint %s",branchpointnum);
                    return false;
                }
                if (!addbranch(targetdelta,newdelnum)) return false;
        }
        return true;
}



int addbranch(branchpoint,num)
struct hshentry * branchpoint;
char * num;
/* adds a new branch and branch delta at branchpoint.
 * If num is the null string, appends the new branch, incrementing
 * the highest branch number (initially 1), and setting the level number to 1.
 * the new delta and branchhead are in globals newdelta and newbranch, resp.
 * the new number is placed into num.
 * returns false on error.
 */
{
        struct branchhead * bhead, * btrail;
        char branchnum[revlength];
        int numlength, result, field;

        numlength = countnumflds(num);

        if (branchpoint->branches==nil) {
                /* start first branch */
                branchpoint->branches = &newbranch;
                if (numlength==0) {
                        VOID strcpy(num, branchpoint->num);
                        VOID strcat(num,".1.1");
                } elsif(countnumflds(num)%2 == 1)
                        VOID strcat(num, ".1");
                newbranch.nextbranch=nil;

        } elsif (numlength==0) {
                /* append new branch to the end */
                bhead=branchpoint->branches;
                while (bhead->nextbranch) bhead=bhead->nextbranch;
                bhead->nextbranch = &newbranch;
                getbranchno(bhead->hsh->num,branchnum);
                incnum(branchnum,num);
                VOID strcat(num,".1");
                newbranch.nextbranch=nil;
        } else {
                /* place the branch properly */
                field = numlength - (numlength%2 ==1?0:1);
                /* field of branch number */
                bhead=branchpoint->branches;
                while ((bhead!=nil) &&
                       ((result=cmpnumfld(num,bhead->hsh->num,field))>0)) {
                        btrail=bhead;
                        bhead=bhead->nextbranch;
                }
                if (bhead==nil || result<0) {
                        /* insert/append new branchhead */
                        if (bhead==branchpoint->branches)
                                branchpoint->branches= &newbranch;
                        else    btrail->nextbranch= &newbranch;
                        newbranch.nextbranch=bhead;
                        if (numlength%2 ==1) VOID strcat(num,".1");
                } else {
                        /* branch exists; append to end */
                        getbranchno(num,branchnum);
                        if (!(targetdelta=genrevs(branchnum,(char *)nil,(char *)nil,(char *)nil,
                                gendeltas))) return false;
                        olddeltanum=targetdelta->num;
                        if (cmpnum(num,olddeltanum) <= 0) {
                                error("deltanumber %s too low; must be higher than %s",
                                      num,olddeltanum);
                                return false;
                        }
                        if (!removelock(caller,targetdelta)) return false;
                        if (numlength%2==1) incnum(olddeltanum,num);
                        targetdelta->next= &newdelta;
                        newdelta.next=nil;
                        return true; /* Don't do anything to newbranch */
                }
        }
        newbranch.hsh = &newdelta;
        newdelta.next=nil;
        return true;
}



struct hshentry * removelock(who,delta)
char * who; struct hshentry * delta;
/* function: Finds the lock held by who on delta,
 * removes it, and returns a pointer to the delta.
 * Prints an error message and returns nil if there is no such lock.
 * An exception is if StrictLocks==false, and who is the owner of
 * the RCS file. If who does not have a lock in this case,
 * delta is returned.
 */
{
        register struct lock * next, * trail;
        char * num;
        struct lock dummy;
        int whomatch, nummatch;

        num=delta->num;
        dummy.nextlock=next=Locks;
        trail = &dummy;
        while (next!=nil) {
                whomatch=strcmp(who,next->login);
                nummatch=strcmp(num,next->delta->num);
                if ((whomatch==0) && (nummatch==0)) break;
                     /*found a lock on delta by who*/
                if ((whomatch!=0)&&(nummatch==0)) {
                    error("revision %s locked by %s",num,next->login);
                    return nil;
                }
                trail=next;
                next=next->nextlock;
        }
        if (next!=nil) {
                /*found one; delete it */
                trail->nextlock=next->nextlock;
                Locks=dummy.nextlock;
                next->delta->lockedby=nil; /* reset locked-by */
                return next->delta;
        } else {
                if (!((StrictLocks==false) && (getuid() == RCSstat.st_uid))) {
                    error("no lock set by %s for revision %s",who,num);
                    return nil;
                } else {
                        return delta;
                }
        }
}



char * getdate()
/* Function: returns a pointer to the current date in the form
 * YY.MM.DD.hh.mm.ss\0
 */
{
        long clock;
        struct tm * tm;
	static char buffer[datelength]; /* date buffer */
        clock=time((long *)0);
        tm=localtime(&clock);
        VOID sprintf(buffer, DATEFORM,
                tm->tm_year, tm->tm_mon+1, tm->tm_mday,
                tm->tm_hour, tm->tm_min, tm->tm_sec);
        return buffer;
}


char * xpandfile (unexfname,dir,delta)
char * unexfname, * dir;
struct hshentry * delta;
/* Function: Reads file unexpfname and copies it to a
 * file in dir, performing keyword substitution with data from delta.
 * returns the name of the expanded file if successful, nil otherwise.
 */
{       char * targetfname;
        FILE * unexfile, *exfile;

        targetfname=mktempfile(dir,TMPFILE3);
        if ((unexfile=fopen(unexfname,  "r" ))==NULL ||
            (exfile  =fopen(targetfname,"w"))==NULL) {
                error("Can't expand file %s",unexfname);
                return nil;
        }
        while (expandline(unexfile,exfile,delta,false,false)); /*expand*/
        ffclose(unexfile);ffclose(exfile);
        return targetfname;
}


mustcheckin (unexfname,delta)
char * unexfname; struct hshentry * delta;
/* Function: determines whether checkin should proceed.
 * Compares the wrkfilename with unexfname, disregarding keywords.
 * If the 2 files differ, returns true. If they do not differ, asks the user
 * whether to return true or false (i.e., whether to checkin the file anyway.
 * If the files do not differ, and quietflag==true, returns false.
 * Shortcut: If forceciflag==true, mustcheckin() always returns true.
 */
{       register int c;
        int response, result;

        if (forceciflag) return true;

        if (!rcsfcmp(workfilename,unexfname,delta)) return true;
        /* If files are different, must check them in. */

        /* files are the same */
        diagnose("File %s is unchanged with respect to revision %s",
                workfilename,delta->num);
        if (quietflag || !ttystdin()) {
                /* Files are the same, but can't ask, so don't checkin*/
                result=false;
        } else {
                /* ask user whether to check in */
                VOID fputs("checkin anyway? [ny](n): ",stderr);
                response=c=getchar();
                while (!(c==EOF || c=='\n')) c=getchar();/*skip to end of line*/
                result=(response=='y'||response=='Y');
        }
        if (result==false) {
                if (quietflag) {
                    warn("checkin aborted since %s was not changed; %s %sdeleted.",
                             workfilename,workfilename,keepworkingfile?"not ":"");
                } else {
                    diagnose("checkin aborted; %s %sdeleted.",
                             workfilename,keepworkingfile?"not ":"");
                }
                if (!keepworkingfile) VOID unlink(workfilename);
        }
        return result;
}




/* --------------------- G E T L O G M S G --------------------------------*/
extern int stdinread; /* is >0 if redirected stdin has been read once.     */


char * getlogmsg()
/* Function: obtains a log message and returns a pointer to it.
 * If a log message is given via the -m option, a pointer to that
 * string is returned.
 * If this is the initial revision, a standard log message is returned.
 * Otherwise, reads a character string from the terminal.
 * The string must be terminated with a control-d or a single '.' on a
 * line. getlogmsg prompts the first time it is called for the
 * log message; during all later calls it asks whether the previous
 * log message can be reused.
 * returns a pointer to the character string; the pointer is always non-nil.
 */
{
        static logyet;      /*indicates whether previous log present*/
        static char emptylog[]  = "*** empty log message ***\n";
        static char initiallog[]= "Initial revision\n";
        char response;
	int cin;
        register char c, old1, old2, * tp;

        if (msg) return msg;

        if ((olddeltanum==nil)&&
	    ((cmpnum(newdelnum,"1.1")==0)||(cmpnum(newdelnum,"1.0")==0))) {
                return initiallog;
	}
	if (keepflag) {
		/* generate std. log message */
		VOID sprintf(logmsg, "checked in with -k by %s at %s.\n",caller,getdate());
		return(logmsg);
	}
        if (logyet) {
                /*previous log available*/
                if (!ttystdin()) return logmsg; /* reuse if stdin is not a terminal*/
                /* otherwise ask */
		clearerr(stdin);		/* reset EOF ptr */
		VOID fputs("reuse log message of previous file? [yn](y): ",stderr);
                cin=getchar();
		response=cin;
                while (!(cin==EOF || cin=='\n')) cin=getchar();/*skip to end of line*/
                if (response=='\n'||response=='y'||response=='Y')
                        return logmsg;
                else
                        logmsg[0]='\0'; /*kill existing log message */
        }

        /* now read string from stdin */
        if (ttystdin()) {
                VOID fputs("enter log message:\n(terminate with ^D or single '.')\n>> ",stderr);
        } else {  /* redirected stdin */
                if (stdinread>0)
                    faterror("Can't reread redirected stdin for log message; use -m");
                stdinread++;
        }

	tp=logmsg; old1='\n'; old2=' ';
	if (feof(stdin))
		clearerr(stdin);
        for (;;) {
                cin=getchar();
                if (cin==EOF) {
			if(ttystdin()) {
				VOID printf("\n");
				clearerr(stdin);
			}
			if ((tp==logmsg)||(*(tp-1)!='\n')) *tp++ = '\n'; /* append newline */
                        *tp = '\0'; /*terminate*/
                        break;
                }
                if (cin=='\n' && old1=='.' && old2=='\n') {
                        *(tp-1) = '\0'; /*kill last period */
                        break;
                }
                if (tp>=logmsg+logsize-2) { /* overflow */
                        if (!ttystdin()) {
                                warn("log message truncated to %d characters",logsize);
                                logmsg[logsize-2]='\n';logmsg[logsize-1]='\0';
                                return logmsg;
                        }
                        VOID fprintf(stderr,"log message too long. Maximum: %d\n",logsize);
                        VOID fputs("reenter log message:\n>> ",stderr);
                        tp=logmsg; old1='\n'; old2=' ';
                        while (cin!='\n') cin=getchar(); /*skip line */
                        continue;
                }
                if (cin=='\n' && ttystdin()) VOID fputs(">> ",stderr);
                *tp++ = cin; old2=old1; old1=cin; /* this is the actual work!*/
                /*SDELIM will be changed to double SDELIM by putdtext*/
        } /* end for */

        /* now check whether the log message is not empty */
        tp=logmsg;
        while ((c= *tp++)==' '||c=='\t'||c=='\n'||c=='\f');
        if (*tp=='\0') {
                logyet=false;
                return emptylog;
        } else {
                logyet=true;
                return logmsg;
        }
}
