/*
 *                      RCS create/change operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /usr/src/local/bin/rcs/src/RCS/rcs.c,v 4.7 87/12/18 11:37:17 narten Exp $ Purdue CS";
#endif
/***************************************************************************
 *                       create RCS files or change RCS file attributes
 *                       Compatibility with release 2: define COMPAT2
 ***************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 */



/* $Log:	rcs.c,v $
 * Revision 4.7  87/12/18  11:37:17  narten
 * lint cleanups (Guy Harris)
 * 
 * Revision 4.6  87/10/18  10:28:48  narten
 * Updating verison numbers. Changes relative to 1.1 are actually 
 * relative to 4.3
 * 
 * Revision 1.4  87/09/24  13:58:52  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.3  87/03/27  14:21:55  jenkins
 * Port to suns
 * 
 * Revision 1.2  85/12/17  13:59:09  albitz
 * Changed setstate to rcs_setstate because of conflict with random.o.
 * 
 * Revision 1.1  84/01/23  14:50:09  kcs
 * Initial revision
 * 
 * Revision 4.3  83/12/15  12:27:33  wft
 * rcs -u now breaks most recent lock if it can't find a lock by the caller.
 * 
 * Revision 4.2  83/12/05  10:18:20  wft
 * Added conditional compilation for sending mail.
 * Alternatives: V4_2BSD, V6, USG, and other.
 * 
 * Revision 4.1  83/05/10  16:43:02  wft
 * Simplified breaklock(); added calls to findlock() and getcaller().
 * Added option -b (default branch). Updated -s and -w for -b.
 * Removed calls to stat(); now done by pairfilenames().
 * Replaced most catchints() calls with restoreints().
 * Removed check for exit status of delivermail().
 * Directed all interactive output to stderr.
 * 
 * Revision 3.9.1.1  83/12/02  22:08:51  wft
 * Added conditional compilation for 4.2 sendmail and 4.1 delivermail.
 * 
 * Revision 3.9  83/02/15  15:38:39  wft
 * Added call to fastcopy() to copy remainder of RCS file.
 *
 * Revision 3.8  83/01/18  17:37:51  wft
 * Changed sendmail(): now uses delivermail, and asks whether to break the lock.
 *
 * Revision 3.7  83/01/15  18:04:25  wft
 * Removed putree(); replaced with puttree() in rcssyn.c.
 * Combined putdellog() and scanlogtext(); deleted putdellog().
 * Cleaned up diagnostics and error messages. Fixed problem with
 * mutilated files in case of deletions in 2 files in a single command.
 * Changed marking of selector from 'D' to DELETE.
 *
 * Revision 3.6  83/01/14  15:37:31  wft
 * Added ignoring of interrupts while new RCS file is renamed;
 * Avoids deletion of RCS files by interrupts.
 *
 * Revision 3.5  82/12/10  21:11:39  wft
 * Removed unused variables, fixed checking of return code from diff,
 * introduced variant COMPAT2 for skipping Suffix on -A files.
 *
 * Revision 3.4  82/12/04  13:18:20  wft
 * Replaced getdelta() with gettree(), changed breaklock to update
 * field lockedby, added some diagnostics.
 *
 * Revision 3.3  82/12/03  17:08:04  wft
 * Replaced getlogin() with getpwuid(), flcose() with ffclose(),
 * /usr/ucb/Mail with macro MAIL. Removed handling of Suffix (-x).
 * fixed -u for missing revno. Disambiguated structure members.
 *
 * Revision 3.2  82/10/18  21:05:07  wft
 * rcs -i now generates a file mode given by the umask minus write permission;
 * otherwise, rcs keeps the mode, but removes write permission.
 * I added a check for write error, fixed call to getlogin(), replaced
 * curdir() with getfullRCSname(), cleaned up handling -U/L, and changed
 * conflicting, long identifiers.
 *
 * Revision 3.1  82/10/13  16:11:07  wft
 * fixed type of variables receiving from getc() (char -> int).
 */


#include <sys/types.h>
#include <sys/stat.h>
#include "rcsbase.h"
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif

extern FILE * fopen();
extern char * bindex();
extern int  expandsym();                /* get numeric revision name        */
extern struct  hshentry  * getnum();
extern struct  lock      * addlock();   /* add a lock                       */
extern char              * getid();
extern char              * getkeyval();
extern char              * Klog, *Khead, *Kaccess, *Ktext;
#ifdef COMPAT2
extern char * Ksuffix;
#endif
extern char * getcaller();              /* get login of caller              */
extern char * malloc();
extern struct hshentry   * genrevs();
extern struct hshentry   * breaklock(); /* remove locks (forward)           */
extern struct hshentry   * findlock();  /* find and remove lock             */
extern char * checkid();                /* check an identifier              */
extern char * getfullRCSname();         /* get full path name of RCS file   */
extern char * mktempfile();             /* temporary file name generator    */
extern free();
extern void catchints();
extern void ignoreints();
extern int nextc;                       /* next input character             */
extern int  nerror;                     /* counter for errors               */
extern int  quietflag;                  /* diagnoses suppressed if true     */
extern char curlogmsg[];                /* current log message              */
extern char * resultfile, *editfile;    /* filename for fcopy and fedit     */
extern FILE *fcopy;                     /* result file during editing       */
extern FILE *fedit;                     /* edit file                        */
extern FILE * finptr;                   /* RCS input file                   */
extern FILE * frewrite;                 /* new RCS file                     */
extern int    rewriteflag;              /* indicates whether input should be*/
					/* echoed to frewrite               */

char * newRCSfilename, * diffilename, * cutfilename;
char * RCSfilename, * workfilename;
extern struct stat RCSstat, workstat; /* file status of RCS and work file   */
extern int  haveRCSstat, haveworkstat;/* status indicators                  */

char accessorlst[strtsize];
FILE * fcut;        /* temporary file to rebuild delta tree                 */
int    oldumask;    /* save umask                                           */

int initflag, strictlock, strict_selected, textflag;
char * textfile, * accessfile;
char * caller, numrev[30];            /* caller's login;               */
struct  access  * newaccessor,  * rmvaccessor,  * rplaccessor;
struct  access  *curaccess,  *rmaccess;
struct  hshentry        * gendeltas[hshsize];

struct  Lockrev {
        char    * revno;
        struct  Lockrev   * nextrev;
};

struct  Symrev {
        char    * revno;
        char    * ssymbol;
        int     override;
        struct  Symrev  * nextsym;
};

struct  Status {
        char    * revno;
        char    * status;
        struct  Status  * nextstatus;
};

struct delrevpair {
        char    * strt;
        char    * end;
        int     code;
};

struct  Lockrev * newlocklst,   * rmvlocklst;
struct  Symrev  * assoclst,  * lastassoc;
struct  Status  * statelst,  * laststate;
struct  delrevpair      * delrev;
struct  hshentry        * cuthead,  *cuttail,  * delstrt;
char    branchnum[revlength], * branchsym;
struct  hshentry branchdummy;
char    command[80], * commsyml;
char    * headstate;
int     lockhead,unlockcaller,chgheadstate,branchflag,commentflag;
int     delaccessflag;
enum    stringwork {copy, edit, empty}; /* expand and edit_expand not needed */


main (argc, argv)
int argc;
char * argv[];
{
        char    *comdusge;
        int     result;
	struct	access	*removeaccess(),  * getaccessor();
        struct  Lockrev *rmnewlocklst();
        struct  Lockrev *curlock,  * rmvlock, *lockpt;
        struct  Status  * curstate;
        struct  access    *temp, *temptr;

        nerror = 0;
	catchints();
        cmdid = "rcs";
        quietflag = false;
        comdusge ="command format:\nrcs -i -alogins -Alogins -e[logins] -b[rev] -c[commentleader] -l[rev] -u[rev] -L -U -nname[:rev] -Nname[:rev] -orange -sstate[:rev] -t[textfile] file....";
        rplaccessor = nil;     delstrt = nil;
        accessfile = textfile = caller = nil;
        branchflag = commentflag = chgheadstate = false;
        lockhead = false; unlockcaller=false;
        initflag= textflag = false;
        strict_selected = 0;

	caller=getcaller();
        laststate = statelst = nil;
        lastassoc = assoclst = nil;
        curlock = rmvlock = newlocklst = rmvlocklst = nil;
        curaccess = rmaccess = rmvaccessor = newaccessor = nil;
        delaccessflag = false;

        /*  preprocessing command options    */
        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {

                case 'i':   /*  initail version  */
                        initflag = true;
                        break;

                case 'b':  /* change default branch */
                        if (branchflag)warn("Redfinition of option -b");
                        branchflag= true;
                        branchsym = (*argv)+2;
                        break;

                case 'c':   /*  change comment symbol   */
                        if (commentflag)warn("Redefinition of option -c");
                        commentflag = true;
                        commsyml = (*argv)+2;
                        break;

                case 'a':  /*  add new accessor   */
                        if ( (*argv)[2] == '\0') {
                            error("Login name missing after -a");
                        }
                        if ( (temp = getaccessor((*argv)+1)) ) {
                            if ( newaccessor )
                                curaccess->nextaccess = temp->nextaccess;
                            else
                                newaccessor = temp->nextaccess;
                            temp->nextaccess = nil;
                            curaccess = temp;
                        }
                        break;

                case 'A':  /*  append access list according to accessfile  */
                        if ( (*argv)[2] == '\0') {
                            error("Missing file name after -A");
                            break;
                        }
                        if ( accessfile) warn("Redefinition of option -A");
                        *argv = *argv+2;
                        if( pairfilenames(1, argv, true, false) > 0) {
                            releaselst(newaccessor);
                            newaccessor = curaccess = nil;
                            releaselst(rmvaccessor);
                            rmvaccessor = rmaccess = nil;
                            accessfile = RCSfilename;
                        }
                        else
                            accessfile = nil;
                        break;

                case 'e':    /*  remove accessors   */
                        if ( (*argv)[2] == '\0' ) {
                            delaccessflag = true;
                            break;
                        }
                        if ( (temp = getaccessor((*argv)+1))  ) {
                            if ( rmvaccessor )
                                rmaccess->nextaccess = temp->nextaccess;
                            else
                                rmvaccessor = temp->nextaccess;
                            temptr = temp->nextaccess;
                            temp->nextaccess = nil;
                            rmaccess = temp;
                            while( temptr ) {
                                newaccessor = removeaccess(temptr,newaccessor,false);
                                temptr = temptr->nextaccess;
                            }
                            curaccess = temp = newaccessor;
                            while( temp){
                                curaccess = temp;
                                temp = temp->nextaccess;
                            }
                        }
                        break;

                case 'l':    /*   lock a revision if it is unlocked   */
                        if ( (*argv)[2] == '\0'){ /* lock head or def. branch */
                            lockhead = true;
                            break;
                        }
                        lockpt = (struct Lockrev *)malloc(sizeof(struct Lockrev));
                        lockpt->revno = (*argv)+2;
                        lockpt->nextrev = nil;
                        if ( curlock )
                            curlock->nextrev = lockpt;
                        else
                            newlocklst = lockpt;
                        curlock = lockpt;
                        break;

                case 'u':   /*  release lock of a locked revision   */
                        if ( (*argv)[2] == '\0'){ /*  unlock head  */
                            unlockcaller=true;
                            break;
                        }
                        lockpt = (struct Lockrev *)malloc(sizeof(struct Lockrev));
                        lockpt->revno = (*argv)+2;
                        lockpt->nextrev = nil;
                        if (rmvlock)
                            rmvlock->nextrev = lockpt;
                        else
                            rmvlocklst = lockpt;
                        rmvlock = lockpt;

                        curlock = rmnewlocklst(lockpt);
                        break;

                case 'L':   /*  set strict locking */
                        if (strict_selected++) {  /* Already selected L or U? */
			   if (!strictlock)	  /* Already selected -U? */
			       warn("Option -L overrides -U");
                        }
                        strictlock = true;
                        break;

                case 'U':   /*  release strict locking */
                        if (strict_selected++) {  /* Already selected L or U? */
			   if (strictlock)	  /* Already selected -L? */
			       warn("Option -L overrides -U");
                        }
			else
			    strictlock = false;
                        break;

                case 'n':    /*  add new association: error, if name exists */
                        if ( (*argv)[2] == '\0') {
                            error("Missing symbolic name after -n");
                            break;
                        }
                        getassoclst(false, (*argv)+1);
                        break;

                case 'N':   /*  add or change association   */
                        if ( (*argv)[2] == '\0') {
                            error("Missing symbolic name after -N");
                            break;
                        }
                        getassoclst(true, (*argv)+1);
                        break;

                case 'o':   /*  delete revisins  */
                        if (delrev) warn("Redefinition of option -o");
                        if ( (*argv)[2] == '\0' ) {
                            error("Missing revision range after -o");
                            break;
                        }
                        getdelrev( (*argv)+1 );
                        break;

                case 's':   /*  change state attribute of a revision  */
                        if ( (*argv)[2] == '\0') {
                            error("State missing after -s");
                            break;
                        }
                        getstates( (*argv)+1);
                        break;

                case 't':   /*  change descriptive text   */
                        textflag=true;
                        if ((*argv)[2]!='\0'){
                                if (textfile!=nil)warn("Redefinition of -t option");
                                textfile = (*argv)+2;
                        }
                        break;

                case 'q':
                        quietflag = true;
                        break;
                default:
                        faterror("Unknown option: %s\n%s", *argv, comdusge);
                };
        }  /* end processing of options */

        if (argc<1) faterror("No input file\n%s", comdusge);
        if (nerror) {   /*  exit, if any error in command options  */
            diagnose("%s aborted",cmdid);
            exit(1);
        }
        if (accessfile) /*  get replacement for access list   */
            getrplaccess();
        if (nerror) {
            diagnose("%s aborted",cmdid);
            exit(1);
        }

        /* now handle all filenames */
        do {
        rewriteflag = false;
        finptr=frewrite=NULL;
        nerror=0;

        if ( initflag ) {
            switch( pairfilenames(argc, argv, false, false) ) {
                case -1: break;        /*  not exist; ok */
                case  0: continue;     /*  error         */
                case  1: error("file %s exists already", RCSfilename);
                         VOID fclose(finptr);
                         continue;
            }
	}
        else  {
            switch( pairfilenames(argc, argv, true, false) ) {
                case -1: continue;    /*  not exist      */
                case  0: continue;    /*  errors         */
                case  1: break;       /*  file exists; ok*/
            }
	}


        /* now RCSfilename contains the name of the RCS file, and
         * workfilename contains the name of the working file.
         * if !initflag, finptr contains the file descriptor for the
         * RCS file. The admin node is initialized.
         */

        diagnose("RCS file: %s", RCSfilename);

        if (!trydiraccess(RCSfilename))            continue; /* give up */
        if (!initflag && !checkaccesslist(caller)) continue; /* give up */
        if (!trysema(RCSfilename,true))            continue; /* give up */

        gettree(); /* read in delta tree */

        /*  update admin. node    */
        if (strict_selected) StrictLocks = strictlock;
        if (commentflag) Comment = commsyml;

        /* update default branch */
        if (branchflag && expandsym(branchsym, branchnum)) {
            if (countnumflds(branchnum)>0) {
                branchdummy.num=branchnum;
                Dbranch = &branchdummy;
            } else
                Dbranch = nil;
        }

        /*  update access list   */
        if ( delaccessflag ) AccessList = nil;
        if ( accessfile ) {
            temp = rplaccessor;
            while( temp ) {
                temptr = temp->nextaccess;
                if ( addnewaccess(temp) )
                    temp->nextaccess = nil;
                temp = temptr;
            }
        }
        temp = rmvaccessor;
        while(temp)   {         /*  remove accessors from accesslist   */
            AccessList = removeaccess(temp, AccessList,true);
            temp = temp->nextaccess;
        }
        temp = newaccessor;
        while( temp)  {         /*  add new accessors   */
            temptr = temp->nextaccess;
            if ( addnewaccess( temp ) )
                temp->nextaccess = nil;
            temp = temptr;
        }

        updateassoc();          /*  update association list   */

        updatelocks();          /*  update locks              */

        /*  update state attribution  */
        if (chgheadstate) {
            /* change state of default branch or head */
            if (Dbranch==nil) {
                if (Head==nil)
                     warn("Can't change states in an empty tree");
                else Head->state = headstate;
            } else {
                rcs_setstate(Dbranch->num,headstate); /* Can't set directly */
            }
        }
        curstate = statelst;
        while( curstate ) {
            rcs_setstate(curstate->revno,curstate->status);
            curstate = curstate->nextstatus;
        }

        cuthead = cuttail = nil;
        if ( delrev && removerevs()) {
            /*  rebuild delta tree if some deltas are deleted   */
            if ( cuttail )
		VOID genrevs(cuttail->num, (char *)nil,(char *)nil,
			     (char *)nil, gendeltas);
            buildtree();
        }


        /* prepare for rewriting the RCS file */
        newRCSfilename=mktempfile(RCSfilename,NEWRCSFILE);
        oldumask = umask(0222); /* turn off write bits */
        if ((frewrite=fopen(newRCSfilename, "w"))==NULL) {
                VOID fclose(finptr);
                error("Can't open file %s",newRCSfilename);
                continue;
        }
        VOID umask(oldumask);
        putadmin(frewrite);
        if ( Head )
           puttree(Head, frewrite);
	VOID putdesc(initflag,textflag,textfile,quietflag);
        rewriteflag = false;

        if ( Head) {
            if (!delrev) {
                /* no revision deleted */
                fastcopy(finptr,frewrite);
            } else {
                if ( cuttail )
                    buildeltatext(gendeltas);
                else
                    scanlogtext((struct hshentry *)nil,empty);
                    /* copy rest of delta text nodes that are not deleted      */
            }
        }
        ffclose(frewrite);   frewrite = NULL;
        if ( ! nerror ) {  /*  move temporary file to RCS file if no error */
	    ignoreints();		/* ignore interrupts */
            if(rename(newRCSfilename,RCSfilename)<0) {
                error("Can't create RCS file %s; saved in %s",
                   RCSfilename, newRCSfilename);
                newRCSfilename[0] = '\0';  /*  avoid deletion by cleanup  */
                restoreints();
                VOID cleanup();
                break;
            }
            newRCSfilename[0]='\0'; /* avoid re-unlinking by cleanup()*/
            /* update mode */
            result=0;
            if (!initflag) /* preserve mode bits */
                result=chmod(RCSfilename,RCSstat.st_mode & ~0222);
            elsif (haveworkstat==0)  /* initialization, and work file exists */
                result=chmod(RCSfilename,workstat.st_mode & ~0222);
            if (result<0) warn("Can't set mode of %s",RCSfilename);

            restoreints();                /* catch them all again */
            diagnose("done");
        } else {
	    diagnose("%s aborted; %s unchanged.",cmdid,RCSfilename);
        }
        } while (cleanup(),
                 ++argv, --argc >=1);

        exit(nerror!=0);
}       /* end of main (rcs) */



getassoclst(flag, sp)
int     flag;
char    * sp;
/*  Function:   associate a symbolic name to a revision or branch,      */
/*              and store in assoclst                                   */

{
        struct   Symrev  * pt;
        char             * temp, *temp2;
        int                c;

        while( (c=(*++sp)) == ' ' || c == '\t' || c =='\n')  ;
        temp = sp;
        temp2=checkid(sp, ':');  /*  check for invalid symbolic name  */
        sp = temp2; c = *sp;   *sp = '\0';
        while( c == ' ' || c == '\t' || c == '\n')  c = *++sp;

        if ( c != ':' && c != '\0') {
	    error("Invalid string %s after option -n or -N",sp);
            return;
        }

        pt = (struct Symrev *)malloc(sizeof(struct Symrev));
        pt->ssymbol = temp;
        pt->override = flag;
	if (c == '\0')  /*  delete symbol  */
            pt->revno = nil;
        else {
            while( (c = *++sp) == ' ' || c == '\n' || c == '\t')  ;
	    if ( c == '\0' )
                pt->revno = nil;
	    else
                pt->revno = sp;
        }
        pt->nextsym = nil;
        if (lastassoc)
            lastassoc->nextsym = pt;
        else
            assoclst = pt;
        lastassoc = pt;
        return;
}



struct access * getaccessor( sp)
char            *sp;
/*   Function:  get the accessor list of options -e and -a,     */
/*              and store in curpt                              */


{
        struct  access  * curpt, * pt,  *pre;
        char    *temp;
        register c;

        while( ( c = *++sp) == ' ' || c == '\n' || c == '\t' || c == ',') ;
        if ( c == '\0') {
            error("Missing login name after option -a or -e");
            return nil;
        }

        curpt = pt = nil;
        while( c != '\0') {
                temp=checkid(sp,',');
                pt = (struct access *)malloc(sizeof(struct access));
                pt->login = sp;
                if ( curpt )
                    pre->nextaccess = pt;
                else
                    curpt = pt;
                pt->nextaccess = curpt;
                pre = pt;
                sp = temp;    c = *sp;   *sp = '\0';
                while( c == ' ' || c == '\n' || c == '\t'|| c == ',')c =(*++sp);
        }
        return pt;
}



getstates(sp)
char    *sp;
/*   Function:  get one state attribute and the corresponding   */
/*              revision and store in statelst                  */

{
        char    *temp, *temp2;
        struct  Status  *pt;
        register        c;

        while( (c=(*++sp)) ==' ' || c == '\t' || c == '\n')  ;
        temp = sp;
        temp2=checkid(sp,':');  /* check for invalid state attribute */
        sp = temp2;   c = *sp;   *sp = '\0';
        while( c == ' ' || c == '\t' || c == '\n' )  c = *++sp;

        if ( c == '\0' ) {  /*  change state of def. branch or Head  */
            chgheadstate = true;
            headstate  = temp;
            return;
        }
        else if ( c != ':' ) {
            error("Missing ':' after state in option -s");
            return;
        }

        while( (c = *++sp) == ' ' || c == '\t' || c == '\n')  ;
        pt = (struct Status *)malloc(sizeof(struct Status));
        pt->status     = temp;
        pt->revno      = sp;
        pt->nextstatus = nil;
        if (laststate)
            laststate->nextstatus = pt;
        else
            statelst = pt;
        laststate = pt;
}



getrplaccess()
/*   Function : get the accesslist of the 'accessfile'  */
/*              and place in rplaccessor                */
{
        register        char    *id, *nextp;
        struct          access  *newaccess, *curaccess;

        if ( (finptr=fopen(accessfile, "r")) == NULL) {
            faterror("Can't open file %s", accessfile);
        }
        Lexinit();
        nextp = &accessorlst[0];

        if ( ! getkey(Khead)) faterror("Missing head in %s", accessfile);
        VOID getnum();
        if ( ! getlex(SEMI) ) serror("Missing ';' after head in %s",accessfile);

#ifdef COMPAT2
        /* read suffix. Only in release 2 format */
        if (getkey(Ksuffix)) {
            if (nexttok==STRING) {
                readstring(); nextlex(); /*through away the suffix*/
            } elsif(nexttok==ID) {
                nextlex();
            }
            if ( ! getlex(SEMI) ) serror("Missing ';' after suffix in %s",accessfile);
        }
#endif

        if (! getkey(Kaccess))fatserror("Missing access list in %s",accessfile);
        curaccess = nil;
        while( id =getid() ) {
            newaccess = (struct access *)malloc(sizeof(struct access));
            newaccess->login = nextp;
            newaccess->nextaccess = nil;
            while( ( *nextp++ = *id++) != '\0')  ;
            if ( curaccess )
                curaccess->nextaccess = newaccess;
            else
                rplaccessor = newaccess;
            curaccess = newaccess;
        }
        if ( ! getlex(SEMI))serror("Missing ';' after access list in %s",accessfile);
        return;
}



getdelrev(sp)
char    *sp;
/*   Function:  get revision range or branch to be deleted,     */
/*              and place in delrev                             */
{
        int    c;
        struct  delrevpair      *pt;

        if (delrev) free((char *)delrev);

        pt = (struct delrevpair *)malloc(sizeof(struct delrevpair));
        while((c = (*++sp)) == ' ' || c == '\n' || c == '\t') ;

        if ( c == '<' || c == '-' ) {  /*  -o  -rev  or <rev  */
            while( (c = (*++sp)) == ' ' || c == '\n' || c == '\t')  ;
            pt->strt = sp;    pt->code = 1;
            while( c != ' ' && c != '\n' && c != '\t' && c != '\0') c =(*++sp);
            *sp = '\0';
            pt->end = nil;  delrev = pt;
            return;
        }
        else {
            pt->strt = sp;
            while( c != ' ' && c != '\n' && c != '\t' && c != '\0'
                   && c != '-' && c != '<' )  c = *++sp;
            *sp = '\0';
            while( c == ' ' || c == '\n' || c == '\t' )  c = *++sp;
            if ( c == '\0' )  {  /*   -o rev or branch   */
                pt->end = nil;   pt->code = 0;
                delrev = pt;
                return;
            }
            if ( c != '-' && c != '<') {
                faterror("Invalid range %s %s after -o", pt->strt, sp);
                free((char *)pt);
                return;
            }
            while( (c = *++sp) == ' ' || c == '\n' || c == '\t')  ;
            if ( c == '\0') {  /*  -o   rev-   or   rev<   */
                pt->end = nil;   pt->code = 2;
                delrev = pt;
                return;
            }
        }
        /*   -o   rev1-rev2    or   rev1<rev2   */
        pt->end = sp;  pt->code = 3;   delrev = pt;
        while( c!= ' ' && c != '\n' && c != '\t' && c != '\0') c = *++sp;
        *sp = '\0';
}




scanlogtext(delta,func)
struct hshentry * delta; enum stringwork func;
/* Function: Scans delta text nodes up to and including the one given
 * by delta, or up to last one present, if delta==nil.
 * For the one given by delta (if delta!=nil), the log message is saved into
 * curlogmsg and the text is processed according to parameter func.
 * Assumes the initial lexeme must be read in first.
 * Does not advance nexttok after it is finished, except if delta==nil.
 */
{       struct hshentry * nextdelta;

        do {
                rewriteflag = false;
                nextlex();
                if (!(nextdelta=getnum())) {
                    if(delta)
                        faterror("Can't find delta for revision %s", delta->num);
                    else return; /* no more delta text nodes */
                }
                if ( nextdelta->selector != DELETE) {
                        rewriteflag = true;
                        VOID fprintf(frewrite,DELNUMFORM,nextdelta->num,Klog);
                }
                if (!getkey(Klog) || nexttok!=STRING)
                        serror("Missing log entry");
                elsif (delta==nextdelta) {
                        VOID savestring(curlogmsg,logsize);
                        delta->log=curlogmsg;
                } else {readstring();
                        if (delta!=nil) delta->log="";
                }
                nextlex();
                if (!getkey(Ktext) || nexttok!=STRING)
                        fatserror("Missing delta text");

                if(delta==nextdelta)
                        /* got the one we're looking for */
                        switch (func) {
                        case copy:      copystring();
                                        break;
                        case edit:      editstring((struct hshentry *)nil);
                                        break;
                        default:        faterror("Wrong scanlogtext");
                        }
                else    readstring(); /* skip over it */

        } while (delta!=nextdelta);
}



releaselst(sourcelst)
struct  access  * sourcelst;
/*   Function:  release the storages whose address are in sourcelst   */

{
        struct  access  * pt;

        pt = sourcelst;
        while(pt) {
            free((char *)pt);
            pt = pt->nextaccess;
        }
}



struct  Lockrev  * rmnewlocklst(which)
struct  Lockrev  * which;
/*   Function:  remove lock to revision which->revno form newlocklst   */

{
        struct  Lockrev   * pt, *pre;

        while( newlocklst && (! strcmp(newlocklst->revno, which->revno))){
            free((char *)newlocklst);
            newlocklst = newlocklst->nextrev;
        }

        pt = pre = newlocklst;
        while( pt ) {
            if ( ! strcmp(pt->revno, which->revno) ) {
                free((char *)pt);
                pt = pt->nextrev;
                pre->nextrev = pt;
            }
            else {
                pre = pt;
                pt = pt->nextrev;
            }
        }
        return pre;
}



struct  access  * removeaccess( who, sourcelst,flag)
struct  access  * who, * sourcelst;
int     flag;
/*   Function:  remove the accessor-- who from sourcelst     */

{
        struct  access  *pt, *pre;

        pt = sourcelst;
        while( pt && (! strcmp(who->login, pt->login) )) {
            free((char *)pt);
            flag = false;
            pt = pt->nextaccess;
	}
        pre = sourcelst = pt;
        while( pt ) {
            if ( ! strcmp(who->login, pt->login) ) {
		free((char *)pt);
                flag = false;
                pt = pt->nextaccess;
                pre->nextaccess = pt;
            }
            else {
                pre = pt;
                pt = pt->nextaccess;
            }
        }
        if ( flag ) warn("Can't remove a nonexisting accessor %s",who->login);
        return sourcelst;
}



int addnewaccess( who )
struct  access  * who;
/*   Function:  add new accessor-- who into AccessList    */

{
        struct  access  *pt,  *pre;

        pre = pt = AccessList;

        while( pt ) {
            if ( strcmp( who->login, pt->login) ) {
                pre = pt;
                pt = pt->nextaccess;
            }
            else
                return 0;
        }
        if ( pre == pt )
            AccessList = who;
        else
            pre->nextaccess = who;
        return 1;
}


sendmail(Delta, who)
char    * Delta,  *who;
/*   Function:  mail to who, informing him that his lock on delta was
 *   broken by caller. Ask first whether to go ahead. Return false on
 *   error or if user decides not to break the lock.
 */
{
        char    * messagefile;
        int   old1, old2, c, response;
        FILE    * mailmess;


	VOID fprintf(stderr, "Revision %s is already locked by %s.\n", Delta, who);
        VOID fprintf(stderr, "Do you want to break the lock? [ny](n): ");
        response=c=getchar();
        while (!(c==EOF || c=='\n')) c=getchar();/*skip to end of line*/
        if (response=='\n'||response=='n'||response=='N') return false;

        /* go ahead with breaking  */
        messagefile=mktempfile("/tmp/", "RCSmailXXXXXX");
        if ( (mailmess = fopen(messagefile, "w")) == NULL) {
            faterror("Can't open file %s", messagefile);
        }

	VOID fprintf(mailmess, "Subject: Broken lock on %s\n\n",bindex(RCSfilename,'/'));
        VOID fprintf(mailmess, "Your lock on revision %s of file %s\n",Delta, getfullRCSname());
        VOID fprintf(mailmess,"has been broken by %s for the following reason:\n",caller);
        VOID fputs("State the reason for breaking the lock:\n", stderr);
        VOID fputs("(terminate with ^D or single '.')\n>> ", stderr);

        old1 = '\n';    old2 = ' ';
        for (; ;) {
            c = getchar();
            if ( c == EOF ) {
                VOID putc('\n',stderr);
                VOID fprintf(mailmess, "%c\n", old1);
                break;
            }
            else if ( c == '\n' && old1 == '.' && old2 == '\n')
                break;
            else {
                VOID fputc( old1, mailmess);
                old2 = old1;   old1 = c;
                if (c== '\n') VOID fputs(">> ", stderr);
            }
        }
        ffclose(mailmess);

#ifdef SENDMAIL
     VOID sprintf(command, "/usr/lib/sendmail %s < %s",who,messagefile);
#else
#    ifdef DELIVERMAIL
        VOID sprintf(command, "/etc/delivermail -w %s < %s",who,messagefile);
#    else
	VOID sprintf(command, "/bin/mail %s < %s",who,messagefile);
#    endif DELIVERMAIL
#endif SENDMAIL

        VOID system(command);
	    /* ignore the exit status, even if delivermail unsuccessful */
        VOID unlink(messagefile);
	return(true);
}



struct hshentry * breaklock(who,delta)
char * who; struct hshentry * delta;
/* function: Finds the lock held by who on delta,
 * removes it, and returns a pointer to the delta.
 * Sends mail if a lock different from the caller's is broken.
 * Prints an error message and returns nil if there is no such lock or error.
 */
{
        register struct lock * next, * trail;
        char * num;
        struct lock dummy;
        int whor, numr;

	num=delta->num;
        dummy.nextlock=next=Locks;
        trail = &dummy;
        while (next!=nil) {
		if (num != nil)
			numr = strcmp(num, next->delta->num);

		whor=strcmp(who,next->login);
		if (whor==0 && numr==0) break; /* exact match */
		if (numr==0 && whor !=0) {
                        if (!sendmail( num, next->login)){
                            diagnose("%s still locked by %s",num,next->login);
                            return nil;
                        } else break; /* continue after loop */
                }
                trail=next;
                next=next->nextlock;
        }
        if (next!=nil) {
                /*found one */
                diagnose("%s unlocked",next->delta->num);
                trail->nextlock=next->nextlock;
                next->delta->lockedby=nil;
                Locks=dummy.nextlock;
                return next->delta;
        } else  {
		error("no lock set on revision %s", num);
                return nil;
        }
}



struct hshentry *searchcutpt(object, length, store)
char    * object;
int     length;
struct  hshentry   * * store;
/*   Function:  Search store and return entry with number being object. */
/*              cuttail = nil, if the entry is Head; otherwise, cuttail */
/*              is the entry point to the one with number being object  */

{
        while( compartial( (*store++)->num, object, length)  )  ;
        store--;

        if ( *store == Head)
            cuthead = nil;
        else
            cuthead = *(store -1);
        return *store;
}



int  branchpoint(strt, tail)
struct  hshentry        *strt,  *tail;
/*   Function: check whether the deltas between strt and tail	*/
/*		are locked or branch point, return 1 if any is  */
/*		locked or branch point; otherwise, return 0 and */
/*              mark DELETE on selector                         */

{
        struct  hshentry    *pt;
	struct lock  *lockpt;
        int     flag;


        pt = strt;
        flag = false;
        while( pt != tail) {
            if ( pt->branches ){ /*  a branch point  */
                flag = true;
                error("Can't remove branch point %s", pt->num);
            }
	    lockpt = Locks;
	    while(lockpt && lockpt->delta != pt)
		lockpt = lockpt->nextlock;
	    if ( lockpt ) {
		flag = true;
		error("Can't remove locked revision %s",pt->num);
	    }
            pt = pt->next;
        }

        if ( ! flag ) {
            pt = strt;
            while( pt != tail ) {
                pt->selector = DELETE;
                diagnose("deleting revision %s ",pt->num);
                pt = pt->next;
            }
        }
        return flag;
}



removerevs()
/*   Function:  get the revision range to be removed, and place the     */
/*              first revision removed in delstrt, the revision before  */
/*              delstrt in cuthead( nil, if delstrt is head), and the   */
/*              revision after the last removed revision in cuttail(nil */
/*              if the last is a leaf                                   */

{
        struct  hshentry    *target, *target2, * temp, *searchcutpt();
        int     length, flag;

        flag = false;
        if ( ! expandsym(delrev->strt, &numrev[0]) ) return 0;
        target = genrevs(&numrev[0], (char *)nil, (char *)nil, (char *)nil, gendeltas);
        if ( ! target ) return 0;
        if ( cmpnum(target->num, &numrev[0]) ) flag = true;
        length = countnumflds( &numrev[0] );

        if ( delrev->code == 0 ) {  /*  -o  rev    or    -o  branch   */
	    if ( length % 2)
		temp=searchcutpt(target->num,length+1,gendeltas);
	    else if (flag) {
                error("Revision %s does not exist", &numrev[0]);
		return 0;
	    }
	    else
		temp = searchcutpt(&numrev[0],length,gendeltas);
	    cuttail = target->next;
            if ( branchpoint(temp, cuttail) ) {
                cuttail = nil;
                return 0;
            }
            delstrt = temp;     /* first revision to be removed   */
            return 1;
        }

        if ( length % 2 ) {   /*  invalid branch after -o   */
            error("Invalid branch range %s after -o", &numrev[0]);
            return 0;
        }

        if ( delrev->code == 1 )  {  /*  -o  -rev   */
            if ( length > 2 ) {
                temp = searchcutpt( target->num, length-1, gendeltas);
                cuttail = target->next;
            }
            else {
                temp = searchcutpt(target->num, length, gendeltas);
                cuttail = target;
                while( cuttail && ! cmpnumfld(target->num,cuttail->num,1) )
                    cuttail = cuttail->next;
            }
            if ( branchpoint(temp, cuttail) ){
                cuttail = nil;
                return 0;
            }
            delstrt = temp;
            return 1;
        }

        if ( delrev->code == 2 )  {   /*  -o  rev-   */
            if ( length == 2 ) {
                temp = searchcutpt(target->num, 1,gendeltas);
                if ( flag)
                    cuttail = target;
                else
                    cuttail = target->next;
            }
            else  {
                if ( flag){
                    cuthead = target;
                    if ( !(temp = target->next) ) return 0;
                }
                else
                    temp = searchcutpt(target->num, length, gendeltas);
                getbranchno(temp->num, &numrev[0]);  /*  get branch number  */
                target = genrevs(&numrev[0], (char *)nil, (char *)nil, (char *)nil, gendeltas);
            }
            if ( branchpoint( temp, cuttail ) ) {
                cuttail = nil;
                return 0;
            }
            delstrt = temp;
            return 1;
        }

        /*   -o   rev1-rev2   */
        if ( ! expandsym(delrev->end, &numrev[0])  )  return 0;
        if ( length != countnumflds( &numrev[0] ) ) {
            error("Invalid revision range %s-%s", target->num, &numrev[0]);
            return 0;
        }
        if ( length > 2 && compartial( &numrev[0], target->num, length-1) ) {
            error("Invalid revision range %s-%s", target->num, &numrev[0]);
            return 0;
        }

        target2 = genrevs( &numrev[0], (char *)nil, (char *)nil, (char *)nil,gendeltas);
        if ( ! target2 ) return 0;

        if ( length > 2) {  /* delete revisions on branches  */
            if ( cmpnum(target->num, target2->num) > 0) {
                if ( cmpnum(target2->num, &numrev[0]) )
                    flag = true;
                else
                    flag = false;
                temp = target;
                target = target2;
                target2 = temp;
            }
            if ( flag ) {
                if ( ! cmpnum(target->num, target2->num) ) {
                    error("Revisions %s-%s don't exist", delrev->strt,delrev->end);
                    return 0;
                }
                cuthead = target;
                temp = target->next;
            }
            else
                temp = searchcutpt(target->num, length, gendeltas);
            cuttail = target2->next;
        }
        else { /*  delete revisions on trunk  */
            if ( cmpnum( target->num, target2->num) < 0 ) {
                temp = target;
                target = target2;
                target2 = temp;
            }
            else
                if ( cmpnum(target2->num, &numrev[0]) )
                    flag = true;
                else
                    flag = false;
            if ( flag ) {
                if ( ! cmpnum(target->num, target2->num) ) {
                    error("Revisions %s-%s don't exist", delrev->strt, delrev->end);
                    return 0;
                }
                cuttail = target2;
            }
            else
                cuttail = target2->next;
            temp = searchcutpt(target->num, length, gendeltas);
        }
        if ( branchpoint(temp, cuttail) )  {
            cuttail = nil;
            return 0;
        }
        delstrt = temp;
        return 1;
}



updateassoc()
/*   Function: add or delete(if revno is nil) association	*/
/*		which is stored in assoclst			*/

{
        struct  Symrev  * curassoc;
	struct  assoc   * pre,  * pt;
        struct  hshentry    * target;

        /*  add new associations   */
        curassoc = assoclst;
        while( curassoc ) {
            if ( curassoc->revno == nil ) {  /* delete symbol  */
		pre = pt = Symbols;
                while( pt && strcmp(pt->symbol,curassoc->ssymbol) ) {
		    pre = pt;
		    pt = pt->nextassoc;
		}
		if ( pt )
		    if ( pre == pt )
			Symbols = pt->nextassoc;
		    else
			pre->nextassoc = pt->nextassoc;
		else
                    warn("Can't delete nonexisting symbol %s",curassoc->ssymbol);
	    }
            else if ( expandsym( curassoc->revno, &numrev[0] ) ) {
	    /*   add symbol  */
               target = (struct hshentry *) malloc(sizeof(struct hshentry));
               target->num = &numrev[0];
               VOID addsymbol(target, curassoc->ssymbol, curassoc->override);
            }
            curassoc = curassoc->nextsym;
        }

}



updatelocks()
/* Function: remove lock for caller or first lock if unlockcaller==true;
 *           remove locks which are stored in rmvlocklst,
 *           add new locks which are stored in newlocklst,
 *           add lock for Dbranch or Head if lockhead==true.
 */
{
        struct  hshentry        *target;
        struct  Lockrev         *lockpt;

        if(unlockcaller == true) { /*  find lock for caller  */
            if ( Head ) {
		if (Locks) {
		    target=findlock(caller,true);
		    if (target==nil) {
			breaklock(caller, Locks->delta); /* remove most recent lock */
		    } else {
			diagnose("%s unlocked",target->num);
		    }
		} else {
		    warn("There are no locks set.");
		}
            } else {
                warn("Can't unlock an empty tree");
            }
        }

        /*  remove locks which are stored in rmvlocklst   */
        lockpt = rmvlocklst;
        while( lockpt ) {
	    if (expandsym(lockpt->revno, numrev)) {
		target = genrevs(numrev, (char *)nil, (char *)nil, (char *)nil, gendeltas);
                if ( target )
		   if ( !(countnumflds(numrev)%2) && cmpnum(target->num,numrev))
			error("Can't unlock nonexisting revision %s",lockpt->revno);
                   else
                        breaklock(caller, target);
                        /* breaklock does its own diagnose */
            }
            lockpt = lockpt->nextrev;
        }

        /*  add new locks which stored in newlocklst  */
        lockpt = newlocklst;
        while( lockpt ) {
            setlock(lockpt->revno,caller);
            lockpt = lockpt->nextrev;
        }

        if ( lockhead == true) {  /*  lock default branch or head  */
            if (Dbranch) {
                setlock(Dbranch->num,caller);
            } elsif ( Head) {
                if (addlock(Head, caller))
                    diagnose("%s locked",Head->num);
            } else {
                warn("Can't lock an empty tree");
            }
        }

}



setlock(rev,who)
char * rev, * who;
/* Function: Given a revision or branch number, finds the correponding
 * delta and locks it for who.
 */
{
        struct  lock     *lpt;
        struct  hshentry *target;

        if (expandsym(rev, &numrev[0]) ){
            target = genrevs(&numrev[0],(char *) nil,(char *) nil,
			     (char *)nil, gendeltas);
            if ( target )
               if ( !(countnumflds(&numrev[0])%2) && cmpnum(target->num,&numrev[0]))
                    error("Can't lock nonexisting revision %s",numrev);
               else
                    if(lpt=addlock(target, who))
                        diagnose("%s locked",lpt->delta->num);
        }
}



rcs_setstate(rev,status)
char * rev, * status;
/* Function: Given a revision or branch number, finds the corresponding delta
 * and sets its state to status.
 */
{
        struct  hshentry *target;

        if ( expandsym(rev, &numrev[0]) ) {
            target = genrevs(&numrev[0],(char *) nil, (char *)nil,
			     (char *) nil, gendeltas);
            if ( target )
               if ( !(countnumflds(&numrev[0])%2) && cmpnum(target->num, &numrev[0]) )
                    error("Can't set state of nonexisting revision %s to %s",
                           numrev,status);
               else
                    target->state = status;
        }
}





buildeltatext(deltas)
struct  hshentry        ** deltas;
/*   Function:  put the delta text on frewrite and make necessary   */
/*              change to delta text                                */
{
        int  i, c, exit_stats;

        cuttail->selector = DELETE;
        initeditfiles("/tmp/");
        scanlogtext(deltas[0], copy);
        i = 1;
        if ( cuthead )  {
            cutfilename=mktempfile("/tmp/", "RCScutXXXXXX");
            if ( (fcut = fopen(cutfilename, "w")) == NULL) {
                faterror("Can't open temporary file %s", cutfilename);
            }

            while( deltas[i-1] != cuthead )  {
                scanlogtext(deltas[i++], edit);
            }

            finishedit((struct hshentry *)nil);    rewind(fcopy);
            while( (c = getc(fcopy)) != EOF) VOID putc(c, fcut);
            swapeditfiles(false);
            ffclose(fcut);
        }

        while( deltas[i-1] != cuttail)
            scanlogtext(deltas[i++], edit);
        finishedit((struct hshentry *)nil);    ffclose(fcopy);

        if ( cuthead ) {
            diffilename=mktempfile("/tmp/", "RCSdifXXXXXX");
            VOID sprintf(command, "%s -n %s %s > %s", DIFF,cutfilename, resultfile, diffilename);
            exit_stats = system (command);
            if (exit_stats != 0 && exit_stats != (1 << BYTESIZ))
                faterror ("diff failed");
            if(!putdtext(cuttail->num,curlogmsg,diffilename,frewrite)) return;
        }
        else
            if (!putdtext(cuttail->num,curlogmsg,resultfile,frewrite)) return;

        scanlogtext((struct hshentry *)nil,empty); /* read the rest of the deltas */
}



buildtree()
/*   Function:  actually removes revisions whose selector field  */
/*              is DELETE, and rebuilds  the linkage of deltas.  */
/*              asks for reconfirmation if deleting last revision*/
{
	int c,  response;

	struct	hshentry   * Delta;
        struct  branchhead      *pt, *pre;

        if ( cuthead )
           if ( cuthead->next == delstrt )
                cuthead->next = cuttail;
           else {
                pre = pt = cuthead->branches;
                while( pt && pt->hsh != delstrt )  {
                    pre = pt;
                    pt = pt->nextbranch;
                }
                if ( cuttail )
                    pt->hsh = cuttail;
                else if ( pt == pre )
                    cuthead->branches = pt->nextbranch;
                else
                    pre->nextbranch = pt->nextbranch;
            }
	else {
            if ( cuttail == nil && !quietflag) {
                VOID fprintf(stderr,"Do you really want to delete all revisions ?[ny](n): ");
		c = response = getchar();
		while( c != EOF && c != '\n') c = getchar();
                if ( response != 'y' && response != 'Y') {
                    diagnose("No revision deleted");
		    Delta = delstrt;
		    while( Delta) {
			Delta->selector = 'S';
			Delta = Delta->next;
		    }
		    return;
		}
	    }
            Head = cuttail;
	}
        return;
}

