/*
 *                       RLOG    operation
 */
#ifndef lint
static char rcsid[]=
"$Header: /usr/src/local/bin/rcs/src/RCS/rlog.c,v 4.7 89/05/01 15:13:48 narten Exp $ Purdue CS";
#endif
/*****************************************************************************
 *                       print contents of RCS files
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




/* $Log:	rlog.c,v $
 * Revision 4.7  89/05/01  15:13:48  narten
 * changed copyright header to reflect current distribution rules
 * 
 * Revision 4.6  88/11/08  11:59:40  narten
 * changes from  eggert@sm.unisys.com (Paul Eggert)
 * 
 * Revision 4.6  88/08/09  19:13:28  eggert
 * Check for memory exhaustion; don't access freed storage.
 * Shrink stdio code size; remove lint.
 * 
 * Revision 4.5  87/12/18  11:46:38  narten
 * more lint cleanups (Guy Harris)
 * 
 * Revision 4.4  87/10/18  10:41:12  narten
 * Updating version numbers
 * Changes relative to 1.1 actually relative to 4.2
 * 
 * Revision 1.3  87/09/24  14:01:10  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:45  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:45  kcs
 * Initial revision
 * 
 * Revision 4.2  83/12/05  09:18:09  wft
 * changed rewriteflag to external.
 * 
 * Revision 4.1  83/05/11  16:16:55  wft
 * Added -b, updated getnumericrev() accordingly.
 * Replaced getpwuid() with getcaller().
 * 
 * Revision 3.7  83/05/11  14:24:13  wft
 * Added options -L and -R;
 * Fixed selection bug with -l on multiple files.
 * Fixed error on dates of the form -d'>date' (rewrote getdatepair()).
 * 
 * Revision 3.6  82/12/24  15:57:53  wft
 * shortened output format.
 *
 * Revision 3.5  82/12/08  21:45:26  wft
 * removed call to checkaccesslist(); used DATEFORM to format all dates;
 * removed unused variables.
 *
 * Revision 3.4  82/12/04  13:26:25  wft
 * Replaced getdelta() with gettree(); removed updating of field lockedby.
 *
 * Revision 3.3  82/12/03  14:08:20  wft
 * Replaced getlogin with getpwuid(), %02d with %.2d, fancydate with PRINTDATE.
 * Fixed printing of nil, removed printing of Suffix,
 * added shortcut if no revisions are printed, disambiguated struct members.
 *
 * Revision 3.2  82/10/18  21:09:06  wft
 * call to curdir replaced with getfullRCSname(),
 * fixed call to getlogin(), cosmetic changes on output,
 * changed conflicting long identifiers.
 *
 * Revision 3.1  82/10/13  16:07:56  wft
 * fixed type of variables receiving from getc() (char -> int).
 */



#include "time.h"
#include "rcsbase.h"
#ifndef lint
static char rcsbaseid[] = RCSBASE;
#endif

extern char * partialno();
extern char * getcaller();          /*get login of caller                   */
extern        free();
extern int    countnumflds();
extern int    compartial();
extern int    expandsym();          /*get numeric name of a revision        */
extern int nextc;                   /*next input character                  */
extern char Klog[];
extern char Ktext[];
extern int  partime();
extern long maketime();             /*convert parsed time to unix time.     */
extern struct tm * localtime();     /*convert unixtime into a tm-structure  */
extern int  pairfilenames();
extern struct hshentry  * getnum();
extern FILE * finptr;               /* RCS input file                       */
extern FILE * frewrite;             /* new RCS file                         */
extern int    rewriteflag;          /* indicates whether input should be    */
				    /* echoed to frewrite */
extern int    nerror;               /* error counter                        */

char * RCSfilename, * workfilename;

char * caller;                        /* caller's login;                    */
int  descflag, selectflag, selectop;  /* option to print access list, symbolic  */
                                      /* names, descriptive text, locks and */
                                      /* Head                               */
int  onlylockflag;		      /* option to print only files         */
				      /* with locks			    */
int  onlyRCSflag;                     /* option to print only RCS file name */
int  lockflag;                        /* whether locker option is set       */
int  revno;                           /* number of revision chosen          */

struct  lockers {                     /* lockers in locker option; stored   */
     char               * login;      /* lockerlist                         */
     struct     lockers * lockerlink;
     }  ;

struct  stateattri {                  /* states in state option; stored in  */
     char               * status;     /* statelist                          */
     struct  stateattri * nextstate;
     }  ;

struct  authors {                     /* login names in author option;      */
     char               * login;      /* stored in authorlist               */
     struct     authors * nextauthor;
     }  ;

struct Revpairs{                      /* revision or branch range in -r     */
     int                  numfld;     /* option; stored in revlist          */
     char               * strtrev;
     char               * endrev;
     struct  Revpairs   * rnext;
     } ;

struct Datepairs{                     /* date range in -d option; stored in */
     char               strtdate[datelength];   /* duelst and datelist      */
     char               enddate[datelength];
     struct  Datepairs  * dnext;
     };

char   Dotstring[200];                /* string of numeric revision name    */
char   * Nextdotstring;               /* next available place of Dotstring  */
struct  Datepairs       * datelist,  * duelst;
struct  Revpairs        * revlist, * Revlst;
int                     branchflag; /* set on -b */
struct  lockers         * lockerlist;
struct  stateattri      * statelist;
struct  authors         * authorlist;



main (argc, argv)
int argc;
char * argv[];
{
        struct  Datepairs       * currdate;
        struct  assoc         * curassoc;
        struct  access        * curaccess;
        struct  lock          * currlock;
        char * cmdusage;

	cmdusage = "command format:\nrlog -L -R -h -t -b -ddates -l[lockers] -rrevisions -sstates -w[logins] file ...";
        cmdid = "rlog";
        descflag = selectflag = true;
        lockflag = onlylockflag = selectop = false;
	onlyRCSflag = false;
        lockerlist = nil;
        authorlist = nil;
        statelist = nil;
        Revlst = revlist = nil;
        branchflag= false;
        duelst = datelist = nil;
	caller=getcaller();

        while (--argc,++argv, argc>=1 && ((*argv)[0] == '-')) {
                switch ((*argv)[1]) {

		case 'L':
			onlylockflag = true;
			break;

		case 'R':
			onlyRCSflag =true;
			break;

                case 'l':
                        selectop = true;
                        lockflag = true;
                        getlocker( (*argv)+2 );
                        break;

                case 'b':
                        selectop = true;
                        branchflag = true;
                        break;

                case 'r':
                        selectop = true;
                        getrevpairs( (*argv)+2 );
                        break;

                case 'd':
                        selectop = true;
                        getdatepair( (*argv)+2 );
                        break;

                case 's':
                        selectop = true;
                        getstate( (*argv)+2);
                        break;

                case 'w':
                        selectop = true;
                        getauthor( (*argv)+2);
                        break;

                case 'h':
                        if ( ! selectflag ) warn("option -t overrides -h");
                        else    descflag = false;
                        break;

                case 't':
                        selectflag = false;
                        if ( ! descflag ) warn("option -t overrides -h");
                        descflag = true;
                        break;

                default:
                        faterror("unknown option: %s\n%s", *argv,cmdusage);

                };
        } /* end of option processing */

        if (argc<1) faterror("No input file\n%s",cmdusage);


        /* now handle all filenames */
        do {
            rewriteflag=false;
            finptr=frewrite=nil;


            if (!pairfilenames(argc, argv, true,false)) continue;

            /* now RCSfilename contains the name of the RCS file, and finptr
             * the file descriptor. Workfilename contains the name of the
             * working file.
             */

            if ( !trysema(RCSfilename, false)) goto loopend; /*  give up */

            /* do nothing if -L is given and there are no locks*/
	    if ( onlylockflag && Locks == nil ) goto loopend;

	    if ( onlyRCSflag ) {
		VOID fprintf(stdout, "%s\n", RCSfilename);
		goto loopend;
	    }
            /*   print RCS filename , working filename and optional
                 administrative information                         */
            VOID fprintf(stdout, "\nRCS file:        %s;   ",RCSfilename);
            /* could use getfullRCSname() here, but that is very slow */
            VOID fprintf(stdout, "Working file:    %s\n", workfilename);
            VOID fprintf(stdout, "head:            %s\n", Head==nil?"":Head->num);
            VOID fprintf(stdout, "branch:          %s\n", Dbranch==nil?"":Dbranch->num);

            VOID fputs("locks:         ", stdout);  /*  print locker list   */
            currlock = Locks;
            while( currlock ) {
                VOID fprintf(stdout,"  %s: %s;", currlock->login,
                                currlock->delta->num);
                currlock = currlock->nextlock;
            }
            if ( StrictLocks )
                VOID fputs(Locks==nil?"  ;  strict":"  strict",stdout);

            VOID fputs("\naccess list:   ", stdout);      /*  print access list  */
            curaccess = AccessList;
            while(curaccess) {
                VOID fputs("  ",stdout);
                VOID fputs(curaccess->login, stdout);
                curaccess = curaccess->nextaccess;
            }

            VOID fputs("\nsymbolic names:", stdout);   /*  print symbolic names   */
            curassoc = Symbols;
            while( curassoc ) {
                VOID fprintf(stdout, "  %s: %s;",curassoc->symbol,
                           curassoc->delta->num);
                curassoc = curassoc->nextassoc;
            }

            VOID fprintf(stdout,"\ncomment leader:  \"%s\"\n",Comment);

            gettree();
            VOID fprintf(stdout, "total revisions: %d;    ", TotalDeltas);
            if ( Head == nil || !selectflag || !descflag) {
                VOID putc('\n',stdout);
                if (descflag) VOID fputs("description:\n", stdout);
                getdesc(descflag);
                VOID fputs("=============================================================================\n",stdout);
                goto loopend;
            }


            /*  keep only those locks given by -l */
            if (lockflag)
                trunclocks();
            getnumericrev();    /* get numeric revision or branch names */
            revno = 0;

            exttree(Head);

            /*  get most recently date of the dates pointed by duelst  */
            currdate = duelst;
            while( currdate) {
                recentdate(Head, currdate);
                currdate = currdate->dnext;
	    }

            extdate(Head);

            /*  reinitialize the date specification list   */
            currdate = duelst;
            while(currdate) {
                VOID sprintf(currdate->strtdate,DATEFORM,0,0,0,0,0,0);
                currdate = currdate->dnext;
            }

            if ( selectop || ( selectflag && descflag) )
                VOID fprintf(stdout, "selected revisions: %d", revno);
            VOID putc('\n', stdout);
            if (descflag) VOID fputs("description:\n", stdout);
            getdesc(descflag);
            while( (nexttok != EOFILE) && readdeltalog());
            if (selectflag && descflag && revno) {
                putrunk();
                putree(Head);
                if (nextlex(), nexttok != EOFILE)
                    fatserror("syntax error; expecting EOF");
            }
            VOID fputs("=============================================================================\n",stdout);
        loopend:
            VOID fclose(finptr);
        } while( ++argv, --argc >= 1);
        exit(nerror!=0);
}



putrunk()
/*  function:  print revisions chosen, which are in trunk      */

{
        struct  hshentry        * ptr, * pre;

        if (Head == nil) return;   /*  empty tree  */

        pre = Head;
        ptr = Head->next;
        while( ptr ) {
            putadelta(pre,ptr,true);
            pre = ptr;
            ptr = ptr->next;
        }
        putadelta(pre,ptr,true);
}



putree(root)
struct  hshentry  *root;
/*   function: print delta tree( not include trunck) in reversed calender
               order on each branch                                        */

{
        if ( root == nil ) return;

        putree(root->next);

        putforest(root->branches);
}




putforest(branchroot)
struct   branchhead     * branchroot;
/*   function:  print branches that has the same direct ancestor    */
{

        if ( branchroot == nil ) return;

        putforest(branchroot->nextbranch);

        putabranch(branchroot->hsh);
        putree(branchroot->hsh);
}




putabranch(root)
struct      hshentry   *root;
/*   function  :  print one branch     */

{

        if ( root == nil) return;

        putabranch(root->next);

        putadelta(root, root, false);
}





putadelta(node,editscript,trunk)
register  struct   hshentry    * node;
register  struct   hshentry    * editscript;
int                              trunk;
/*  function: print delta node if node->selector is 's'.        */
/*      editscript indicates where the editscript is stored     */
/*      trunk indicated whether this node is in trunk           */
{
        struct  branchhead      * newbranch;
        char                    * branchnum,  branch[40];

        if ( ( node == nil) || ( node->selector == 'u'))
            return;

        VOID fprintf(stdout,"----------------------------\n");
        VOID fprintf(stdout, "revision %s        ",node->num);
        if ( node->lockedby )
           VOID fprintf(stdout, "locked by: %s;       ", node->lockedby);
        VOID putc('\n', stdout);

        VOID fputs("date: ",stdout);
        VOID PRINTDATE(stdout,node->date); VOID putc(' ',stdout);
        VOID PRINTTIME(stdout,node->date);
        VOID fprintf(stdout, ";  author: %s;  ", node->author);
        VOID fprintf(stdout, "state: %s;  ", node->state);

        if ( editscript )
           if(trunk)
              VOID fprintf(stdout,"lines added/del: %d/%d",
                             editscript->deletelns, editscript->insertlns);
           else
              VOID fprintf(stdout,"lines added/del: %d/%d",
                             editscript->insertlns, editscript->deletelns);

        VOID putc('\n', stdout);

        branchnum = & (branch[0]);
        newbranch = node->branches;
        if ( newbranch ) {
           VOID fputs("branches:  ", stdout);
           while( newbranch ) {
                getbranchno(newbranch->hsh->num, branchnum);
                VOID fprintf(stdout, "%s;  ", branchnum);
                newbranch = newbranch->nextbranch;
           }
           VOID putc('\n', stdout);
        }

        VOID fputs(node->log,stdout);
}





readdeltalog()
/*  Function : get the log message and skip the text of a deltatext node.
 *             Return false if current block does not start with a number.
 *             Assumes the current lexeme is not yet in nexttok; does not
 *             advance nexttok.
 */
{
        register struct  hshentry  * Delta;

        nextlex();
        if ( !(Delta = getnum() )) return(false);
        if ( ! getkey(Klog) || ( nexttok != STRING ) )
                fatserror("Missing log entry");
        Delta->log = talloc(logsize);
        VOID savestring(Delta->log, logsize);
        nextlex();
        if ( ! getkey(Ktext) || (nexttok != STRING) )
                fatserror("Missing delta text");
        Delta->insertlns = Delta->deletelns = 0;
        if ( Delta != Head)
                getscript(Delta);
        else
                readstring();
        return true;
}



getscript(Delta)
struct    hshentry   * Delta;
/*   function:  read edit script of Delta and count how many lines added  */
/*              and deleted in the script                                 */

{
        int ed;   /*  editor command  */
	register FILE * fin;
        register  int   c;
        register  int   i;
        int             length;

	fin = finptr;
	while( (ed = getc(fin)) != EOF) {
           /*  assume first none white character is command name  */
            while( ed == '\n' || ed == ' ' || ed == '\t')
		ed = getc(fin);
            if (ed == SDELIM) break;  /*  script text is ended   */
	    while( ( c = getc(fin)) == ' ' );  /*  skip blank  */
            if ( ! ('0' <= c && c <= '9')) {
                faterror("Missing line number in edit script");
                break;
            }
	    while( '0' <= (c = getc(fin)) && c <= '9' ) ;

	    while( c == ' ')c = getc(fin);  /*  skip blanks  */
            if ( !('0' <= c && c <= '9' ) ) {
                faterror("Incorrect range in edit script");
                break;
            }
            length = c - '0';
	    while( '0' <= (c = getc(fin)) && c <= '9' )
                length = length * 10 + c - '0';
	    while( c != '\n' && c != EOF) c = getc(fin);
            switch (ed) {
            case 'd' :
                 Delta->deletelns += length;
                 break;

            case 'a' :
                 /*  skip scripted lines  */
                 for ( i=length; i > 0 && c != EOF; i--){
		     while( (c=getc(fin)) != '\n' && c != EOF);
                     Delta->insertlns++;
                 }
                 break;

            default:
                 faterror("Unknown command in edit script: %c", ed);
                 break;
            }
        }
	nextc = getc(fin);
}







exttree(root)
struct hshentry  *root;
/*  function: select revisions , starting with root             */

{
        struct branchhead       * newbranch;

        if (root == nil) return;

        extractdelta(root);
        exttree(root->next);

        newbranch = root->branches;
        while( newbranch ) {
            exttree(newbranch->hsh);
            newbranch = newbranch->nextbranch;
        }
}




getlocker(argv)
char    * argv;
/*   function : get the login names of lockers from command line   */
/*              and store in lockerlist.                           */

{
        register char c;
        struct   lockers   * newlocker;
        argv--;
        while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                 c == '\n' || c == ';')  ;
        if (  c == '\0') {
            lockerlist=nil;
            return;
        }

        while( c != '\0' ) {
            newlocker = ( struct lockers *)talloc( sizeof(struct lockers) );
            newlocker->lockerlink = lockerlist;
            newlocker->login = argv;
            lockerlist = newlocker;
            while ( ( c = (*++argv)) != ',' && c != '\0' && c != ' '
                       && c != '\t' && c != '\n' && c != ';') ;
            *argv = '\0';
            if ( c == '\0' ) return;
            while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                     c == '\n' || c == ';')  ;
        }
}



getauthor(argv)
char   *argv;
/*   function:  get the author's name form command line   */
/*              and store in aauthorlist                  */

{
        register    c;
        struct     authors  * newauthor;

        argv--;
        while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                 c == '\n' || c == ';')  ;
        if ( c == '\0' ) {
            authorlist = (struct authors *)talloc(sizeof(struct authors));
            authorlist->login = caller;
            authorlist->nextauthor  = nil;
            return;
        }

        while( c != '\0' ) {
            newauthor = (struct authors *)talloc(sizeof(struct authors));
            newauthor->nextauthor = authorlist;
            newauthor->login = argv;
            authorlist = newauthor;
            while( ( c = *++argv) != ',' && c != '\0' && c != ' '
                     && c != '\t' && c != '\n' && c != ';') ;
            * argv = '\0';
            if ( c == '\0') return;
            while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                     c == '\n' || c == ';')  ;
        }
}




getstate(argv)
char   * argv;
/*   function :  get the states of revisions from command line  */
/*               and store in statelist                         */

{
        register  char  c;
        struct    stateattri    *newstate;

        argv--;
        while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                 c == '\n' || c == ';')  ;
        if ( c == '\0'){
            warn(" Missing state attributes after -s options");
            return;
        }

        while( c != '\0' ) {
            newstate = (struct stateattri *)talloc(sizeof(struct stateattri));
            newstate->nextstate = statelist;
            newstate->status = argv;
            statelist = newstate;
            while( (c = (*++argv)) != ',' && c != '\0' && c != ' '
                    && c != '\t' && c != '\n' && c != ';')  ;
            *argv = '\0';
            if ( c == '\0' ) return;
            while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                     c == '\n' || c == ';')  ;
        }
}



trunclocks()
/*  Function:  Truncate the list of locks to those that are held by the  */
/*             id's on lockerlist. Do not truncate if lockerlist empty.  */

{
        struct lockers  * plocker;
        struct lock     * plocked,  * nextlocked;

        if ( (lockerlist == nil) || (Locks == nil)) return;

        /* shorten Locks to those contained in lockerlist */
        plocked = Locks;
        Locks = nil;
        while( plocked != nil) {
            plocker = lockerlist;
            while((plocker != nil) && ( strcmp(plocker->login, plocked->login)!=0))
                plocker = plocker->lockerlink;
            nextlocked = plocked->nextlock;
            if ( plocker != nil) {
                plocked->nextlock = Locks;
                Locks = plocked;
            }
            plocked = nextlocked;
        }
}



recentdate(root, pd)
struct 	hshentry	* root;
struct	Datepairs	* pd;
/*  function:  Finds the delta that is closest to the cutoff date given by   */
/*             pd among the revisions selected by exttree.                   */
/*             Successively narrows down the interfal given by pd,           */
/*             and sets the strtdate of pd to the date of the selected delta */
{
        struct  branchhead      * newbranch;

	if ( root == nil) return;
        if ( root->selector == 's') {
             if ( cmpnum(root->date, pd->strtdate) >= 0 &&
                  cmpnum(root->date, pd->enddate) <= 0)
		VOID strcpy(pd->strtdate, root->date);
        }

        recentdate(root->next, pd);
        newbranch = root->branches;
        while( newbranch) {
           recentdate(newbranch->hsh, pd);
           newbranch = newbranch->nextbranch;
	}
}






extdate(root)
struct  hshentry        * root;
/*  function:  select revisions which are in the date range specified     */
/*             in duelst  and datelist, start at root                     */

{
        struct  branchhead      * newbranch;
        struct  Datepairs       * pdate;

        if ( root == nil) return;

        if ( datelist || duelst) {
            pdate = datelist;
            while( pdate ) {
                if ( (pdate->strtdate)[0] == '\0' || cmpnum(root->date,pdate->strtdate) >= 0){
                   if ((pdate->enddate)[0] == '\0' || cmpnum(pdate->enddate,root->date) >= 0)
                        break;
                }
                pdate = pdate->dnext;
            }
            if ( pdate == nil) {
                pdate = duelst;
                while(pdate) {
                   if ( cmpnum(root->date, pdate->strtdate) == 0)
                      break;
                   pdate = pdate->dnext;
                }
            }
            if ( pdate == nil)
                root->selector = 'u';
        }
        if (root->selector == 's') revno++;

        extdate(root->next);

        newbranch = root->branches;
        while( newbranch ) {
           extdate(newbranch->hsh);
           newbranch = newbranch->nextbranch;
        }
}



extractdelta(pdelta)
struct  hshentry        * pdelta;
/*  function:  compare information of pdelta to the authorlst, lockerlist, */
/*             statelist, revlist and mark 's' on selector if pdelta is    */
/*             selected; otherwise, mark 'u'                               */

{
        struct  lock            * plock;
        struct  stateattri      * pstate;
        struct  authors         * pauthor;
        struct  Revpairs        * prevision;
        int                       length;

        pdelta->selector = 's';
        if ( authorlist ) {  /*  certain author's revisions wanted only  */
            pauthor = authorlist;
            while((pauthor != nil) && ( strcmp(pauthor->login, pdelta->author)!=0))
                pauthor = pauthor->nextauthor;
            if ( pauthor == nil ) {
                pdelta->selector = 'u';
                return;
            }
        }
        if ( statelist ) {   /* revisions with certain state wanted  */
            pstate = statelist;
            while((pstate != nil) && (strcmp(pstate->status, pdelta->state)!=0))
                pstate = pstate->nextstate;
            if ( pstate == nil ) {
                pdelta->selector = 'u';
                return;
            }
        }
        if ( lockflag ) {    /*  locked revisions   */
            plock = Locks;
            while( plock && (plock->delta != pdelta))
                plock = plock->nextlock;
            if (plock == nil ) {
                pdelta->selector = 'u';
                return;
            }
        }
        if ( Revlst ) {   /*  revisions or branches selected  */

            prevision = Revlst;
            while( prevision != nil ) {
                length = prevision->numfld;
                if ( length % 2 == 1) { /*  a branch number  */
                     if ( countnumflds(pdelta->num) ==(length+1))
                        if ( (compartial(pdelta->num, prevision->strtrev,length) >= 0)&&
                             (compartial(prevision->endrev, pdelta->num, length) >= 0) )
                             break;
                }
                else if ( countnumflds(pdelta->num ) == length)  /*  a revision */
                    if ( (compartial(pdelta->num, prevision->strtrev, length) >= 0) &&
                         (compartial(prevision->endrev, pdelta->num, length) >= 0) )
                        break;
                prevision = prevision->rnext;
            }
            if (prevision == nil)  {
                pdelta->selector = 'u';
                return;
            }
        }
}



char * procdate(target, source)
char * target, * source;
/* Function: Parses a free-format date in target, converts it
 * into RCS internal format, and stores the result into source.
 * Returns target on success, nil otherwise.
 */
{
	long            unixtime;
	struct     tm   parseddate,  *ftm;

	if ( partime(source, &parseddate) == 0) {
	    error("Can't parse date/time: %s", source);
	    *target= '\0';
	    return nil;
	}
	if ( (unixtime = maketime(&parseddate)) == 0L) {
	    error("Inconsistent date/time: %s", source);
	    *target='\0';
	    return nil;
	}
	ftm = localtime(&unixtime);
	VOID sprintf(target,DATEFORM,
	ftm->tm_year,ftm->tm_mon+1,ftm->tm_mday,ftm->tm_hour,ftm->tm_min,ftm->tm_sec);
	return target;
}



getdatepair(argv)
   char   * argv;
/*  function:  get time range from command line and store in datelist if    */
/*             a time range specified or in duelst if a time spot specified */

{
        register   char         c;
        struct     Datepairs    * nextdate;
        char                    * rawdate;
	int                     switchflag;

        argv--;
        while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                 c == '\n' || c == ';')  ;
        if ( c == '\0' ) {
            warn("Missing date/time after -d");
            return;
        }

        while( c != '\0' )  {
	    switchflag = false;
	    nextdate = (struct Datepairs *) talloc(sizeof(struct Datepairs));
            if ( c == '<' ) {   /*   case: -d <date   */
                c = *++argv;
                (nextdate->strtdate)[0] = '\0';
	    } elsif (c == '>') {        /*  case:  -d >date     */
		c = *++argv;
		(nextdate->enddate)[0] = '\0';
		switchflag = true;
	    } else {
                rawdate = argv;
		while( c != '<' && c != '>' && c != ';' && c != '\0')
		     c = *++argv;
                *argv = '\0';
		if ( c == '>' ) switchflag=true;
		if (procdate(switchflag?nextdate->enddate:nextdate->strtdate,
			     rawdate)==nil) continue;
		if ( c == ';' || c == '\0') {  /*  case: -d date  */
		    VOID strcpy(nextdate->enddate,nextdate->strtdate);
		    VOID sprintf(nextdate->strtdate,DATEFORM,0,0,0,0,0,0);
                    nextdate->dnext = duelst;
                    duelst = nextdate;
		    goto end;
		} else {
		    /*   case:   -d date<  or -d  date>; see switchflag */
		    while ( (c= *++argv) == ' ' || c=='\t' || c=='\n');
		    if ( c == ';' || c == '\0') {
			/* second date missing */
			if (switchflag)
			    *nextdate->strtdate= '\0';
			else
			    *nextdate->enddate= '\0';
			nextdate->dnext = datelist;
			datelist = nextdate;
			goto end;
		    }
                }
            }
            rawdate = argv;
	    while( c != '>' && c != '<' && c != ';' && c != '\0')
 		c = *++argv;
            *argv = '\0';
	    if (procdate(switchflag?nextdate->strtdate:nextdate->enddate,
			 rawdate)==nil) continue;
            nextdate->dnext = datelist;
	    datelist = nextdate;
     end:
/*
	    VOID printf("startdate: %s; enddate: %s;\n", nextdate->strtdate,nextdate->enddate);
*/
	    if ( c == '\0')  return;
            while( (c = *++argv) == ';' || c == ' ' || c == '\t' || c =='\n');
        }
}





getnumericrev()
/*  function:  get the numeric name of revisions which stored in revlist  */
/*             and then stored the numeric names in Revlst                */
/*             if branchflag, also add default branch                     */

{
        struct  Revpairs        * ptr, *pt;
        int     flag;
        char    *temprev;

        /*  free the previous numeric revision list  */
        pt = Revlst;
        while( pt) {
	   ptr = pt->rnext;
           free((char *)pt);
           pt = ptr;
        }
        Nextdotstring = &Dotstring[0]; /* reset buffer */


        Revlst = nil;
        ptr = revlist;
        while( ptr ) {
            pt = (struct Revpairs *) talloc(sizeof(struct Revpairs));
            if ( ptr->numfld == 1 ){ /*  case:  -r rev   */
                if ( (flag = expandsym(ptr->strtrev, Nextdotstring)) == true ) {
                    pt->numfld = countnumflds(Nextdotstring);
                    pt->strtrev = pt->endrev = Nextdotstring;
                    while( *Nextdotstring++ != '\0' )  ;
                }
            }
            else if( ptr->numfld == 2){ /*  case: -r rev-   */
                if ( (flag = expandsym(ptr->strtrev, Nextdotstring)) == true) {
                    pt->numfld = countnumflds(Nextdotstring);
                    pt->strtrev = Nextdotstring;
                    while( *Nextdotstring++ != '\0' ) ;
                    pt->endrev = Nextdotstring;
                    if ( pt->numfld > 2) choptail(pt->strtrev);
                    * Nextdotstring++ = '\0';
                }
             }
             else if(ptr->numfld == 3)  { /*  case: -r -rev   */
                if ( (flag = expandsym(ptr->endrev, Nextdotstring)) == true) {
                    pt->endrev = Nextdotstring;
                    while( *Nextdotstring++ != '\0' )  ;
                    pt->numfld = countnumflds(pt->endrev);
                    pt->strtrev = Nextdotstring;
                    if ( pt->numfld == 2)
                        *Nextdotstring++ = '1';
                    else
                        choptail(pt->endrev);
                    *Nextdotstring++ = '.';
                    *Nextdotstring++ = '1';
                    *Nextdotstring++ = '\0';
                }
             }
             else  {     /*   case:  -r rev1-rev2   */
                if ( (flag = expandsym(ptr->strtrev, Nextdotstring)) == true ) {
                    pt->strtrev = Nextdotstring;
                    while( *Nextdotstring++ != '\0' )  ;
                    if ( ( flag = expandsym(ptr->endrev, Nextdotstring)) == true)  {
                        pt->numfld = countnumflds(pt->strtrev);
                        pt->endrev = Nextdotstring;
                        while( *Nextdotstring++ != '\0' ) ;
                        if((flag = checkrevpair(pt->strtrev, pt->endrev)) == true)
                           /*  switch pt->strtrev with pt->endrev, if pt->strtrev > pt->endre  */
                            if (compartial(pt->strtrev, pt->endrev, pt->numfld) > 0 ) {
                                temprev = pt->strtrev;
                                pt->strtrev = pt->endrev;
                                pt->endrev = temprev;
                            }
                     }
                }
             }

             if ( flag ){
                pt->rnext = Revlst;
                Revlst = pt;
             }
             else
                free((char *)pt);
             ptr = ptr->rnext;
        }
        /* Now take care of branchflag */
        if (branchflag) {
            flag =true;
            pt = (struct Revpairs *) talloc(sizeof(struct Revpairs));
            if (Dbranch) {
                pt->strtrev = pt->endrev = Dbranch->num;
            } elsif (Head!=nil) {
                pt->strtrev = pt->endrev = /* branch number of head */
                    partialno(Nextdotstring,Head->num,1);
                while( *Nextdotstring++ != '\0' ) ;
            } else flag = false;
            if (flag) { /* prepend new node */
                pt->rnext=Revlst; Revlst=pt;
                pt->numfld = countnumflds(pt->strtrev);
            }
        }

}



checkrevpair(num1,num2)
char    *num1,  *num2;
/*  function:  check whether num1, num2 are legal pair,i.e.
    only the last field are different and have same number of
    feilds( if length <= 2, may be different if first field)   */

{
        int    length;

        if ( (length = countnumflds(num1)) != countnumflds(num2) ) {
            error(" Invalid branch or revision pair %s : %s", num1, num2);
            return false;
        }
        if ( length > 2 )
            if (compartial(num1, num2, length-1) != 0) {
                error("Invalid branch or revision pair %s : %s", num1, num2);
                return false;
            }

        return true;
}



getrevpairs(argv)
register     char    * argv;
/*  function:  get revision or branch range from command line, and   */
/*             store in revlist                                      */

{
        register    char    c;
        struct      Revpairs  * nextrevpair;
        int         flag;

        argv--;
        while( ( c = (*++argv)) == ',' || c == ' ' || c == '\t' ||
                 c == '\n' || c == ';')  ;
        if ( c == '\0' ) {
            warn(" Missing revision or branch number after -r");
            return;
        }

        while( c != '\0') {
            while(  c  == ',' || c == ' ' || c == '\t' ||
                     c == '\n' || c == ';') c = *++argv;
            if (c == '\0')  return;
            nextrevpair = (struct Revpairs *) talloc(sizeof(struct Revpairs));
            nextrevpair->rnext = revlist;
            revlist = nextrevpair;
            nextrevpair->numfld  = nil;
            nextrevpair->strtrev = nil;
            nextrevpair->endrev  = nil;
            flag = false;
            if (  c == '<' || c == '-' ) {  /*  case: -r -rev  or -r <rev  */
                flag = true;
                while( (c =(*++argv)) == ' ' || c == '\t' || c =='\n') ;
            }
            else {
                nextrevpair->strtrev = argv;
                /*   get a revision or branch name  */
                while( c != ',' && c != ';' && c != ' ' && c != '\0' && c != '-'
                        && c != '\t' && c != '\n' && c != '<') c = *++argv;

                *argv = '\0';

                if ( c != '<' && c != '-') {    /*  case: rev  */
                    nextrevpair->numfld = 1;
                    continue;
                }

                if ( (c =(*++argv)) == ',' || c == '\0' || c == ' '
                      || c == '\t' || c == '\n' || c == ';') {/*  case: rev_  */
                    nextrevpair->numfld = 2;
                    continue;
                }
            }
            nextrevpair->endrev = argv;
            while( c != ',' && c != ' ' && c != '\0' && c != '\t' && c != '<'
                   && c != '\n' && c != '-' && c != ';')  c = *++argv;

            * argv = '\0';
            if ( c == '<'){
                error("separator expected near %s", nextrevpair->endrev);
                while( (c = *++argv) != ',' && c != ' ' && c != '\0' &&
                        c != '\t' && c != '\n' && c != ';' ) ;
                revlist = nextrevpair->rnext;
                continue;
            }
            else  {
                if (flag)   /*  case:  -rev   */
                    nextrevpair->numfld  = 3;

                else     /*   rev1-rev2  appears  */
                    nextrevpair->numfld = 4;
            }
        }
}



choptail(strhead)
char     * strhead;
/*   function : chop off the last field of a branch or a revision number  */

{
        char    *pt, *sp;

        for(pt = Nextdotstring-1; pt != strhead && *pt != '.'; pt--) ;
        for(sp = strhead; sp < pt; sp++) *Nextdotstring++ = *sp;
}

