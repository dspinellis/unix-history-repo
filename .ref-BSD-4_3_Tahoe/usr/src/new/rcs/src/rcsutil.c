/*
 *                     RCS utilities
 */
#ifndef lint
static char rcsid[]= "$Id: rcsutil.c,v 4.3 87/10/18 10:40:22 narten Exp $ Purdue CS";
#endif
/*****************************************************************************
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



/* $Log:	rcsutil.c,v $
 * Revision 4.3  87/10/18  10:40:22  narten
 * Updating version numbers. Changes relative to 1.1 actually
 * relative to 4.1
 * 
 * Revision 1.3  87/09/24  14:01:01  narten
 * Sources now pass through lint (if you ignore printf/sprintf/fprintf 
 * warnings)
 * 
 * Revision 1.2  87/03/27  14:22:43  jenkins
 * Port to suns
 * 
 * Revision 1.1  84/01/23  14:50:43  kcs
 * Initial revision
 * 
 * Revision 4.1  83/05/10  15:53:13  wft
 * Added getcaller() and findlock().
 * Changed catchints() to check SIGINT for SIG_IGN before setting up the signal
 * (needed for background jobs in older shells). Added restoreints().
 * Removed printing of full RCS path from logcommand().
 * 
 * Revision 3.8  83/02/15  15:41:49  wft
 * Added routine fastcopy() to copy remainder of a file in blocks.
 *
 * Revision 3.7  82/12/24  15:25:19  wft
 * added catchints(), ignoreints() for catching and ingnoring interrupts;
 * fixed catchsig().
 *
 * Revision 3.6  82/12/08  21:52:05  wft
 * Using DATEFORM to format dates.
 *
 * Revision 3.5  82/12/04  18:20:49  wft
 * Replaced SNOOPDIR with SNOOPFILE; changed addlock() to update
 * lockedby-field.
 *
 * Revision 3.4  82/12/03  17:17:43  wft
 * Added check to addlock() ensuring only one lock per person.
 * Addlock also returns a pointer to the lock created. Deleted fancydate().
 *
 * Revision 3.3  82/11/27  12:24:37  wft
 * moved rmsema(), trysema(), trydiraccess(), getfullRCSname() to rcsfnms.c.
 * Introduced macro SNOOP so that snoop can be placed in directory other than
 * TARGETDIR. Changed %02d to %.2d for compatibility reasons.
 *
 * Revision 3.2  82/10/18  21:15:11  wft
 * added function getfullRCSname().
 *
 * Revision 3.1  82/10/13  16:17:37  wft
 * Cleanup message is now suppressed in quiet mode.
 */




#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include "rcsbase.h"
#include <pwd.h>

extern char * malloc();
extern char * bindex();
extern FILE * finptr;
extern char * RCSfilename;
extern char * getlogin();
extern struct passwd *getpwuid();

int    (*oldSIGINT)();         /* saves the original value for SIGINT */



char * getcaller()
/* Function: gets the callers login from his uid.
 * If the uid is root, tries to get the true login with getlogin().
 */
{       char * name;
	int uid;
	uid=getuid();
	if (uid==0) {
		/* super user; try getlogin() to distinguish */
		name = getlogin();
		if (name!=nil && *name!='\0')
			return name;
	}
	return(getpwuid(uid)->pw_name);
}



struct hshentry * findlock(who,delete)
char * who; int delete;
/* Finds the first lock held by who and returns a pointer
 * to the locked delta; also removes the lock if delete==true.
 * Returns nil if there is no lock held by who.
 */
{
        register struct lock * next, * trail;
        struct lock dummy;

        dummy.nextlock=next=Locks;
        trail = &dummy;
        while (next!=nil) {
                if(strcmp(who,next->login)==0) break; /*found a lock*/
                trail=next;
                next=next->nextlock;
        }
        if (next!=nil) {
		/* found one */
		if (delete) {
		    /* delete it */
		    trail->nextlock=next->nextlock;
		    Locks=dummy.nextlock;
		    next->delta->lockedby=nil; /* reset locked-by */
		}
                return next->delta;
        } else  return nil;
}







struct lock * addlock(delta,who)
struct hshentry * delta; char * who;
/* Given a delta, addlock checks whether
 * the delta is locked by somebody other than who.
 * If so, an error message is printed, and false returned.
 * If the delta is not reserved at all, a lock for it is added,
 * and a pointer for the lock returned.
 */
{
        struct lock * next;

        next=Locks;
        while (next!=nil) {
                if (cmpnum(delta->num,next->delta->num)==0) {
                        if (strcmp(who,next->login)==0)
                                return next;
                                /* lock exists already */
                        else {
                                error("revision %s already locked by %s",
                                      delta->num, next->login);
                                return false;
                        }
                } else {
                        if (strcmp(who,next->login)==0) {
                                error("you already locked %s; only one lock allowed per person.",
                                       next->delta->num);
                                return false;
                        } else {
                                next=next->nextlock;
                        }
                }
        }
        /* not found; set up new lockblock */
        next= (struct lock *) malloc(sizeof (struct lock));
        delta->lockedby=next->login=who;
        next->delta= delta;
        next->nextlock=Locks;
        Locks=next;
        return next;
}



int addsymbol(delta,name,rebind)
struct hshentry * delta; char * name; int rebind;
/* Function: adds a new symbolic name and associates it with node delta.
 * If name already exists and rebind is true, the name is associated
 * with the new delta; otherwise, an error message is printed and
 * false returned. Returns true it successful.
 */
{       register struct assoc * next;
        next=Symbols;
        while (next!=nil) {
                if (strcmp(name,next->symbol)==0) {
                        if (rebind) {
                                next->delta=delta;
                                return true;
                        } else {
                                error("symbolic name %s already bound to %s",
                                        name,next->delta->num);
                                return false;
                        }
                } else  next = next->nextassoc;
        }
        /* not found; insert new pair. */
        next = (struct assoc *) malloc(sizeof(struct assoc));
        next->symbol=name;
        next->delta=delta;
        next->nextassoc=Symbols;
        Symbols = next;
        return true;
}




int checkaccesslist(who)
char * who;
/* function: Returns true if who is the superuser, the owner of the
 * file, the access list is empty, or who is on the access list.
 * Prints an error message and returns false otherwise.
 */
{
        register struct access * next;
        struct stat statbuf;

        if ((AccessList==nil) || (strcmp(who,"root")==0))
                return true;

        next=AccessList;
        do {
                if (strcmp(who,next->login)==0)
                        return true;
                next=next->nextaccess;
        } while (next!=nil);

        VOID fstat(fileno(finptr),&statbuf);  /* get owner of file */
        if (getuid() == statbuf.st_uid) return true;

        error("User %s not on the access list",who);
        return false;
}

catchsig(sig)
{
	VOID signal(sig, SIG_IGN);
        diagnose("\nRCS: cleaning up\n");
        VOID cleanup();
        exit(1);
}

  
  void catchints()
  {
        cksignal(SIGINT); cksignal(SIGHUP);
        cksignal(SIGQUIT); cksignal(SIGPIPE);
 	cksignal(SIGTERM);
  }
  
 
cksignal(sig)
int	sig;
{
	if (signal(sig,SIG_IGN) != SIG_IGN)
		VOID signal(sig,catchsig);
}

  void ignoreints()
  {
        VOID signal(SIGINT,SIG_IGN); VOID signal(SIGHUP,SIG_IGN);
        VOID signal(SIGQUIT,SIG_IGN); VOID signal(SIGPIPE,SIG_IGN);
	VOID signal(SIGTERM,SIG_IGN);
  }
  
  
void restoreints()
{
        if (oldSIGINT!=SIG_IGN)
                VOID signal(SIGINT,catchsig);
        VOID signal(SIGHUP,catchsig); VOID signal(SIGQUIT,catchsig);
        VOID signal(SIGPIPE,catchsig); VOID signal(SIGTERM,catchsig);
}


fastcopy(inf,outf)
FILE * inf, * outf;
/* Function: copies the remainder of file inf to outf. First copies the
 * rest that is in the IO-buffer of inf character by character, and then
 * copies the remainder in blocks.
 */
{       char buf[BUFSIZ];
        register int rcount, wcount;

        /* write the rest of the buffer to outf */
        while ((--inf->_cnt)>=0) {
                VOID putc(*inf->_ptr++&0377,outf);
        }
        if (fflush(outf) == EOF) {
		faterror("write error");
	}

        /*now read the rest of the file in blocks*/
        while ((rcount=read(fileno(inf),buf,BUFSIZ))>0) {
                wcount=write(fileno(outf),buf,rcount);
                if (wcount!=rcount) {
                    faterror("write error");
                }
        }
}






#ifdef SNOOPFILE

#include "time.h"
extern struct tm* localtime();
extern long time();

logcommand(commandname,delta, sequence,login)
char* commandname; struct hshentry * delta, * sequence[];char * login;
/* Function: start a process to write the file that
 * logs the RCS command.
 * Each line in the log file contains the following information:
 * operation, revision(r), backward deltas applied(b), forward deltas applied(f),
 * total deltas present(t), creation date of delta(d), date of operation(o),
 * login of caller, RCS file name.
 */
{
        char command[200];
        char curdate[datelength];
        register int i, backward, forward;
        long clock;
        struct tm * tm;

        clock=time((long *)0);
        tm=localtime(&clock);

        VOID sprintf(curdate,DATEFORM,
                tm->tm_year, tm->tm_mon+1, tm->tm_mday,
                tm->tm_hour, tm->tm_min, tm->tm_sec);

        i= backward=forward=0;
        while(sequence[i]!=nil) {  /* count deltas to be applied*/
        if (countnumflds(sequence[i]->num) == 2)
                backward++;  /* reverse delta */
        else    forward++;   /* branch delta  */
        i++;
        }
        VOID sprintf(command,"%s \"%s %10sr %3db %3df %3dt %sc %so %s %s\" &\n",
                SNOOP, commandname,delta->num,backward,forward,TotalDeltas,delta->date,
                curdate,login,bindex(RCSfilename,'/'));
        VOID system(command);
}
#endif









