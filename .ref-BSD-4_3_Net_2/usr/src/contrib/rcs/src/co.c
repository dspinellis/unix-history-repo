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
 *                     RCS checkout operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /usr/src/local/bin/rcs/src/RCS/co.c,v 4.7 89/05/01 15:11:41 narten Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       check out revisions from RCS files
 *****************************************************************************
 */


/* $Log:	co.c,v $
 * Revision 4.7  89/05/01  15:11:41  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.6  88/11/08  12:02:31  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.6  88/08/09  19:12:15  eggert
 * Fix "co -d" core dump; rawdate wasn't always initialized.
 * Use execv(), not system(); fix putchar('\0') and diagnose() botches; remove lint
 * 
 * Revision 4.5  87/12/18  11:35:40  narten
 * lint cleanups (from Guy Harris)
 * 
 * Revision 4.4  87/10/18  10:20:53  narten
 * Updating version numbers changes relative to 1.1, are actually
 * relative to 4.2
 * 
 * Revision 1.3  87/09/24  13:58:30  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:21:38  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:49:58  kcs
 * Initial revision
 * 
 * Revision 4.2  83/12/05  13:39:48  wft
 * made rewriteflag external.
 * 
 * Revision 4.1  83/05/10  16:52:55  wft
 * Added option -u and -f.
 * Added handling of default branch.
 * Replaced getpwuid() with getcaller().
 * Removed calls to stat(); now done by pairfilenames().
 * Changed and renamed rmoldfile() to rmworkfile().
 * Replaced catchints() calls with restoreints(), unlink()--link() with rename();
 * 
 * Revision 3.7  83/02/15  15:27:07  wft
 * Added call to fastcopy() to copy remainder of RCS file.
 *
 * Revision 3.6  83/01/15  14:37:50  wft
 * Added ignoring of interrupts while RCS file is renamed; this avoids
 * deletion of RCS files during the unlink/link window.
 *
 * Revision 3.5  82/12/08  21:40:11  wft
 * changed processing of -d to use DATEFORM; removed actual from
 * call to preparejoin; re-fixed printing of done at the end.
 *
 * Revision 3.4  82/12/04  18:40:00  wft
 * Replaced getdelta() with gettree(), SNOOPDIR with SNOOPFILE.
 * Fixed printing of "done".
 *
 * Revision 3.3  82/11/28  22:23:11  wft
 * Replaced getlogin() with getpwuid(), flcose() with ffclose(),
 * %02d with %.2d, mode generation for working file with WORKMODE.
 * Fixed nil printing. Fixed -j combined with -l and -p, and exit
 * for non-existing revisions in preparejoin().
 *
 * Revision 3.2  82/10/18  20:47:21  wft
 * Mode of working file is now maintained even for co -l, but write permission
 * is removed.
 * The working file inherits its mode from the RCS file, plus write permission
 * for the owner. The write permission is not given if locking is strict and
 * co does not lock.
 * An existing working file without write permission is deleted automatically.
 * Otherwise, co asks (empty answer: abort co).
 * Call to getfullRCSname() added, check for write error added, call
 * for getlogin() fixed.
 *
 * Revision 3.1  82/10/13  16:01:30  wft
 * fixed type of variables receiving from getc() (char -> int).
 * removed unused variables.
 */




#include "rcsbase.h"
#include "time.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif
static char co[] = CO;
static char merge[] = MERGE;

extern FILE * fopen();
extern int    rename();
extern char * getcaller();          /*get login of caller                   */
extern struct hshentry * genrevs(); /*generate delta numbers                */
extern char * getancestor();
extern int  nextc;                  /*next input character                  */
extern int  nerror;                 /*counter for errors                    */
extern char Kdesc[];		    /*keyword for description		    */
extern char * buildrevision();      /*constructs desired revision           */
extern int    buildjoin();          /*join several revisions                */
extern char * mktempfile();         /*temporary file name generator         */
extern struct hshentry * findlock();/*find (and delete) a lock              */
extern struct lock * addlock();     /*add a new lock                        */
extern long   maketime();           /*convert parsed time to unix time.     */
extern struct tm * localtime();     /*convert unixtime into a tm-structure  */
extern FILE * finptr;               /* RCS input file                       */
extern FILE * frewrite;             /* new RCS file                         */
extern int    rewriteflag;          /* indicates whether input should be    */
				    /* echoed to frewrite                   */

char * newRCSfilename, * neworkfilename;
char * RCSfilename, * workfilename;
extern struct stat RCSstat, workstat; /* file status of RCS and work file   */
extern int  haveRCSstat, haveworkstat;/* status indicators                  */

char * date, * rev, * state, * author, * join;
char finaldate[datelength];

int forceflag, lockflag, unlockflag, tostdout;
char * caller;                        /* caller's login;                    */
extern quietflag;

char numericrev[revlength];           /* holds expanded revision number     */
struct hshentry * gendeltas[hshsize]; /* stores deltas to be generated      */
struct hshentry * targetdelta;        /* final delta to be generated        */

char * joinlist[joinlength];          /* pointers to revisions to be joined */
int lastjoin;                         /* index of last element in joinlist  */

main (argc, argv)
int argc;
char * argv[];
{
        int killock;                  /* indicates whether a lock is removed*/
        char * cmdusage;
        struct tm parseddate, *ftm;
        char * rawdate;
        long unixtime;

	catchints();
        cmdid = "co";
	cmdusage = "command format:\nco -f[rev] -l[rev] -p[rev] -q[rev] -r[rev] -ddate -sstate -w[login] -jjoinlist file ...";
        date = rev = state = author = join = nil;
	forceflag = lockflag = unlockflag = tostdout = quietflag = false;
	caller=getcaller();
	rawdate = "";

        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {

                case 'r':
                revno:  if ((*argv)[2]!='\0') {
                                if (rev!=nil) warn("Redefinition of revision number");
                                rev = (*argv)+2;
                        }
                        break;

		case 'f':
			forceflag=true;
			goto revno;

                case 'l':
                        lockflag=true;
                        if (unlockflag) {
                                warn("-l has precedence over -u");
                                unlockflag=false;
                        }
                        goto revno;

                case 'u':
                        unlockflag=true;
                        if (lockflag) {
                                warn("-l has precedence over -u");
                                unlockflag=false;
                        }
                        goto revno;

                case 'p':
                        tostdout=true;
                        goto revno;

                case 'q':
                        quietflag=true;
                        goto revno;

                case 'd':
                        if ((*argv)[2]!='\0') {
                                if (date!=nil) warn("Redefinition of -d option");
                                rawdate=(*argv)+2;
                        }
                        /* process date/time */
                        if (partime(rawdate,&parseddate)==0)
                                faterror("Can't parse date/time: %s",rawdate);
                        if ((unixtime=maketime(&parseddate))== 0L)
                                faterror("Inconsistent date/time: %s",rawdate);
                        ftm=localtime(&unixtime);
                        VOID sprintf(finaldate,DATEFORM,
                        ftm->tm_year,ftm->tm_mon+1,ftm->tm_mday,ftm->tm_hour,ftm->tm_min,ftm->tm_sec);
                        date=finaldate;
                        break;

                case 'j':
                        if ((*argv)[2]!='\0'){
                                if (join!=nil)warn("Redefinition of -j option");
                                join = (*argv)+2;
                        }
                        break;

                case 's':
                        if ((*argv)[2]!='\0'){
                                if (state!=nil)warn("Redefinition of -s option");
                                state = (*argv)+2;
                        }
                        break;

                case 'w':
                        if (author!=nil)warn("Redefinition of -w option");
                        if ((*argv)[2]!='\0')
                                author = (*argv)+2;
                        else    author = caller;
                        break;

                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);

                };
        } /* end of option processing */

        if (argc<1) faterror("No input file\n%s",cmdusage);

        /* now handle all filenames */
        do {
        rewriteflag=false;
        finptr=frewrite=NULL;
        neworkfilename=nil;

        if (!pairfilenames(argc,argv,true,tostdout)) continue;

        /* now RCSfilename contains the name of the RCS file, and finptr
         * the file descriptor. If tostdout is false, workfilename contains
         * the name of the working file, otherwise undefined (not nil!).
         * Also, RCSstat, workstat, and haveworkstat have been set.
         */
        diagnose("%s  -->  %s", RCSfilename,tostdout?"stdout":workfilename);


        if (!tostdout && !trydiraccess(workfilename)) continue; /* give up */
        if ((lockflag||unlockflag) && !checkaccesslist(caller)) continue;     /* give up */
        if (!trysema(RCSfilename,lockflag||unlockflag)) continue;           /* give up */


        gettree();  /* reads in the delta tree */

        if (Head==nil) {
                /* no revisions; create empty file */
                diagnose("no revisions present; generating empty revision 0.0");
                if (!tostdout)
                        if (!creatempty()) continue;
                /* Can't reserve a delta, so don't call addlock */
        } else {
                if (rev!=nil) {
                        /* expand symbolic revision number */
                        if (!expandsym(rev,numericrev))
                                continue;
		} elsif (unlockflag && (targetdelta=findlock(caller,false))!=nil) {
			VOID strcpy(numericrev,targetdelta->num);
                } elsif (Dbranch!=nil) {
                        VOID strcpy(numericrev,Dbranch->num);
		} else  numericrev[0]='\0'; /* empty */
                /* get numbers of deltas to be generated */
                if (!(targetdelta=genrevs(numericrev,date,author,state,gendeltas)))
                        continue;
                /* check reservations */
                if (lockflag && !addlock(targetdelta,caller))
                        continue;

                if (unlockflag) {
                        if((killock=rmlock(caller,targetdelta))== -1)
                                continue;
                } else {
                        killock=0;
                }

                if (join && !preparejoin()) continue;

		diagnose("revision %s%s",targetdelta->num,
			 lockflag?" (locked)":
			 unlockflag?" (unlocked)":"");

                /* remove old working file if necessary */
                if (!tostdout)
                        if (!rmworkfile()) continue;

                /* prepare for rewriting the RCS file */
                if (lockflag||(killock==1)) {
                        newRCSfilename=mktempfile(RCSfilename,NEWRCSFILE);
                        if ((frewrite=fopen(newRCSfilename, "w"))==NULL) {
                                error("Can't open file %s",newRCSfilename);
                                continue;
                        }
                        putadmin(frewrite);
                        puttree(Head,frewrite);
                        VOID fprintf(frewrite, "\n\n%s%c",Kdesc,nextc);
                        rewriteflag=true;
                }

                /* skip description */
                getdesc(false); /* don't echo*/

                if (!(neworkfilename=buildrevision(gendeltas,targetdelta,
                      tostdout?(join!=nil?"/tmp/":(char *)nil):workfilename,true)))
                                continue;

                if ((lockflag||killock==1)&&nerror==0) {
                        /* rewrite the rest of the RCSfile */
                        fastcopy(finptr,frewrite);
                        ffclose(frewrite); frewrite=NULL;
			ignoreints();
                        if (rename(newRCSfilename,RCSfilename)<0) {
                                error("Can't rewrite %s; saved in: %s",
                                RCSfilename, newRCSfilename);
                                newRCSfilename[0]='\0'; /* avoid deletion*/
                                restoreints();
                                break;
                        }
                        newRCSfilename[0]='\0'; /* avoid re-deletion by cleanup()*/
                        if (chmod(RCSfilename,RCSstat.st_mode & ~0222)<0)
                            warn("Can't preserve mode of %s",RCSfilename);
                        restoreints();
                }

#               ifdef SNOOPFILE
                logcommand("co",targetdelta,gendeltas,caller);
#               endif

                if (join) {
                        rmsema(); /* kill semaphore file so other co's can proceed */
			if (!buildjoin(neworkfilename)) continue;
                }
                if (!tostdout) {
			if (rename(neworkfilename,workfilename) <0) {
                                error("Can't create %s; see %s",workfilename,neworkfilename);
                                neworkfilename[0]= '\0'; /*avoid deletion*/
                                continue;
                        }
			neworkfilename[0]= '\0'; /*avoid re-deletion by cleanup()*/
		}
        }
	if (!tostdout)
            if (chmod(workfilename, WORKMODE(RCSstat.st_mode))<0)
                warn("Can't adjust mode of %s",workfilename);


        if (!tostdout) diagnose("done");
        } while (cleanup(),
                 ++argv, --argc >=1);

        exit(nerror!=0);

}       /* end of main (co) */


/*****************************************************************
 * The following routines are auxiliary routines
 *****************************************************************/

int rmworkfile()
/* Function: unlinks workfilename, if it exists, under the following conditions:
 * If it is read-only, workfilename is unlinked.
 * Otherwise (file writable):
 *   if !quietmode asks the user whether to really delete it (default: fail);
 *   otherwise failure.
 * Returns false on failure to unlink, true otherwise.
 */
{
        int response, c;    /* holds user response to queries */

        if (haveworkstat< 0)      /* File doesn't exist; set by pairfilenames*/
            return (true);        /* No problem */

	if ((workstat.st_mode & 0222)&&!forceflag) {    /* File is writable */
            if (!quietflag) {
                VOID fprintf(stderr,"writable %s exists; overwrite? [ny](n): ",workfilename);
                /* must be stderr in case of IO redirect */
                c=response=getchar();
                while (!(c==EOF || c=='\n')) c=getchar(); /*skip rest*/
                if (!(response=='y'||response=='Y')) {
                        warn("checkout aborted.");
                        return false;
                }
            } else {
                error("writable %s exists; checkout aborted.",workfilename);
                return false;
            }
        }
	/* now unlink: either not writable, forceflag, or permission given */
        if (unlink(workfilename) != 0) {            /* Remove failed   */
            error("Can't unlink %s",workfilename);
            return false;
        }
        return true;
}


creatempty()
/* Function: creates an empty working file.
 * First, removes an existing working file with rmworkfile().
 */
{
        int  fdesc;              /* file descriptor */

        if (!rmworkfile()) return false;
        fdesc=creat(workfilename,0);
        if (fdesc < 0) {
                faterror("Cannot create %s",workfilename);
                return false;
        } else {
                VOID close(fdesc); /* empty file */
                return true;
        }
}


int rmlock(who,delta)
char * who; struct hshentry * delta;
/* Function: removes the lock held by who on delta.
 * Returns -1 if someone else holds the lock,
 * 0 if there is no lock on delta,
 * and 1 if a lock was found and removed.
 */
{       register struct lock * next, * trail;
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
                    error("revision %s locked by %s; use co -r or rcs -u",num,next->login);
                    return -1;
                }
                trail=next;
                next=next->nextlock;
        }
        if (next!=nil) {
                /*found one; delete it */
                trail->nextlock=next->nextlock;
                Locks=dummy.nextlock;
                next->delta->lockedby=nil; /* reset locked-by */
                return 1; /*success*/
        } else  return 0; /*no lock on delta*/
}




/*****************************************************************
 * The rest of the routines are for handling joins
 *****************************************************************/

char * getrev(sp, tp, buffsize)
register char * sp, *tp; int buffsize;
/* Function: copies a symbolic revision number from sp to tp,
 * appends a '\0', and returns a pointer to the character following
 * the revision number; returns nil if the revision number is more than
 * buffsize characters long.
 * The revision number is terminated by space, tab, comma, colon,
 * semicolon, newline, or '\0'.
 * used for parsing the -j option.
 */
{
        register char c;
        register int length;

        length = 0;
        while (((c= *sp)!=' ')&&(c!='\t')&&(c!='\n')&&(c!=':')&&(c!=',')
                &&(c!=';')&&(c!='\0')) {
                if (length>=buffsize) return false;
                *tp++= *sp++;
                length++;
        }
        *tp= '\0';
        return sp;
}



int preparejoin()
/* Function: Parses a join list pointed to by join and places pointers to the
 * revision numbers into joinlist.
 */
{
        struct hshentry * * joindeltas;
        struct hshentry * tmpdelta;
        register char * j;
        char symbolrev[revlength],numrev[revlength];

        joindeltas = (struct hshentry * *)talloc(hshsize*sizeof(struct hshentry *));
        j=join;
        lastjoin= -1;
        for (;;) {
                while ((*j==' ')||(*j=='\t')||(*j==',')) j++;
                if (*j=='\0') break;
                if (lastjoin>=joinlength-2) {
                        error("too many joins");
                        return(false);
                }
                if(!(j=getrev(j,symbolrev,revlength))) return false;
                if (!expandsym(symbolrev,numrev)) return false;
                tmpdelta=genrevs(numrev,(char *)nil,(char *)nil,(char *)nil,(struct hshentry * *)joindeltas);
                if (tmpdelta==nil)
                        return false;
                else    joinlist[++lastjoin]=tmpdelta->num;
                while ((*j==' ') || (*j=='\t')) j++;
                if (*j == ':') {
                        j++;
                        while((*j==' ') || (*j=='\t')) j++;
                        if (*j!='\0') {
                                if(!(j=getrev(j,symbolrev,revlength))) return false;
                                if (!expandsym(symbolrev,numrev)) return false;
                                tmpdelta=genrevs(numrev,(char *)nil,(char *)nil,(char *)nil, (struct hshentry * *) joindeltas);
                                if (tmpdelta==nil)
                                        return false;
                                else    joinlist[++lastjoin]=tmpdelta->num;
                        } else {
                                error("join pair incomplete");
                                return false;
                        }
                } else {
                        if (lastjoin==0) { /* first pair */
                                /* common ancestor missing */
                                joinlist[1]=joinlist[0];
                                lastjoin=1;
                                /*derive common ancestor*/
                                joinlist[0]=talloc(revlength);
                                if (!getancestor(targetdelta->num,joinlist[1],joinlist[0]))
                                       return false;
                        } else {
                                error("join pair incomplete");
                                return false;
                        }
                }
        }
        if (lastjoin<1) {
                error("empty join");
                return false;
        } else  return true;
}



buildjoin(initialfile)
char * initialfile;
/* Function: merge pairs of elements in joinlist into initialfile
 * If tostdout==true, copy result to stdout.
 * All unlinking of initialfile, rev2, and rev3 should be done by cleanup().
 */
{
	char commarg[revlength+3];
        char subs[revlength];
        char * rev2, * rev3;
        int i;

        rev2=mktempfile("/tmp/",JOINFIL2);
        rev3=mktempfile("/tmp/",JOINFIL3);

        i=0;
        while (i<lastjoin) {
                /*prepare marker for merge*/
                if (i==0)
                        VOID strcpy(subs,targetdelta->num);
                else    VOID sprintf(subs, "merge%d",i/2);
                diagnose("revision %s",joinlist[i]);
                VOID sprintf(commarg,"-p%s",joinlist[i]);
                if (run((char*)nil,rev2, co,commarg,"-q",RCSfilename,(char*)nil)) {
                        nerror++;return false;
                }
                diagnose("revision %s",joinlist[i+1]);
                VOID sprintf(commarg,"-p%s",joinlist[i+1]);
                if (run((char *)nil,rev3, co,commarg,"-q",RCSfilename,(char*)nil)) {
                        nerror++; return false;
                }
                diagnose("merging...");
		if (
                        (i+2)>=lastjoin && tostdout
		    ?	run((char*)nil,(char*)nil, merge,"-p",initialfile,rev2,rev3,subs,joinlist[i+1],(char*)nil)
		    :	run((char*)nil,(char*)nil, merge,     initialfile,rev2,rev3,subs,joinlist[i+1],(char*)nil)) {
                        nerror++; return false;
                }
                i=i+2;
        }
        return true;
}
