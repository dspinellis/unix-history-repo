/*
 *      TREE.C
 *      UTREE tree menu routines.
 *      3.01-um klin, Tue Jun  4 14:19:16 1991
 *              klin, Tue Oct 15 14:02:37 1991, Handling of symlinks changed
 *              klin, Sat Oct 26 15:07:06 1991, Tagging files changed
 *                                              Sorting and zooming changed
 *                                              Select directories added
 *                                              Print tree list added
 *                                              More local functions
 *      3.02-um klin, Fri Nov  1 10:46:14 1991, Screen layout changed
 *                                              Goto parent added
 *              klin, Sun Nov 24 19:30:43 1991, Cd to current directory before
 *                                              executing some commands
 *                                              Video attributes changed
 *      3.03-um klin, Tue Feb 11 22:58:03 1992, Screen layout changed
 *                                              Shell escape, variables and
 *                                              filetype commands changed
 *              klin, Sat Feb 15 14:44:52 1992, Video handling and partinioning of
 *                                              directory and file windows changed
 *              klin, Sat Feb 22 10:34:03 1992, Many commands changed to work
 *                                              on current directory or subtree
 *                                              or tagged files
 *              klin, Sun Feb 23 17:32:57 1992, Key handling and key bindings
 *                                              changed
 *                                              No removing of subtrees
 *            a klin, Sun Mar 15 19:08:25 1992, Bug fix in gotree()
 *            b klin, Sun Mar 22 10:56:31 1992, Minor changes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03b-um (klin) Mar 22 1992 tree.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

LOCAL dlist *tdlist = DNULL;    /* Top dlist on tree screen             */
LOCAL dlist *mdlist = DNULL;    /* Marked dlist entry                   */
LOCAL dlist *tdlast = DNULL;    /* Last tdlist                          */
LOCAL dlist *cdlast = DNULL;    /* Last current dlist entry             */
LOCAL char  *mustup = "Tree must be updated. Continue ?";
LOCAL char  *cancel = "(Hit BREAK to abort)";

/* Tree menu commands in help line                                      */
LOCAL char *tmline =
" Help Backup Chdir Find Grep Info List Mkdir Out Rmdir Stat Tag Untag Quit";
LOCAL char *menuline = NULL;

#define BCOL    0               /* Column for tree position bar         */
#define SCOL    1               /* Column for tag or mark sign          */
#define TCOL    2               /* Startcolumn for directory tree       */
#define UCOL    -1              /* Column for number of files unknown   */
#define FCOL    -5              /* Column for number of files           */
#define SLIN    (lastdline+1)   /* Line for separator line              */
#define NFFMT   "%5d"           /* Format for number of files           */

#define ONTR(c) ((c) < 'a')     /* Command works on subtree             */
#define ONTG(c) ((c) < 'a')     /* Command works on tagged files        */

/* ---- External variables and functions ------------------------------ */

EXTRN FILE *popen();
EXTRN char *writedlist();
EXTRN char *selectdir();
LOCAL char *dirselect();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      TREE DISPLAY ROUTINES
 */

/* Display files of current directory in file window starting at line l */
LOCAL VOID showflist(l)
  register int l;
{
  register int f, i;

  if((f = (l - firstfline) * fperline) < CNFIL) {
    while(f < CNFIL && l++ <= lastfline)
      for(i = 0; f < CNFIL && i < fperline; f++, i++) {
	putfile(cdlist, f, 0);
	clearline();
      }
    if(l <= lastfline)
      clearwindow(l, lastfline);
  }
  else if(CNFIL == 0 && l == firstfline)
    clearwindow(l, lastfline);

} /* showflist() */

/* Display directory list entry dp */
LOCAL VOID showdlist(dp, f)
  register dlist *dp;
  register int f;
{
  /* Is directory on screen? */
  if(dp && DTROW(dp) >= firstdline && DTROW(dp) <= lastdline) {
    /* Display directory tag marker */
    if(DNTAG(dp)) {
      (void) setgraphic(GC_ON);
      (void) putcxy(SCOL, DTROW(dp), GC_TG);
      (void) setgraphic(GC_OFF);
    }
    else
      (void) putcxy(SCOL, DTROW(dp), ' ');
    /* Display directory filename */
    if(f && dp == cdlist) {     /* Highlight current directory */
      if(CCANC) {
	setvideo(DA_BOLDREV);
	(void) putfxy(TCOL+DTCOL(dp)-1, DTROW(dp), 0, ">%s ", CFNAM);
      }
      else  {
	setvideo(DA_HALFREV);
	(void) putfxy(TCOL+DTCOL(dp)-1, DTROW(dp), 0, " %s ", CFNAM);
      }
    }
    else {                      /* Other directory */
      setvideo(DCANC(dp) ? DA_BOLD : DA_HALF);
      (void) putfxy(TCOL+DTCOL(dp)-1, DTROW(dp), 0, " %s ", DFNAM(dp));
    }
    /* Display number of files if known */
     setvideo(DA_NORMAL);
    if(DCANC(dp)) {
      if(DFLAG(dp) != FL_FIL)
	putcxy(UCOL, DTROW(dp), '?');
      else
	(void) putfxy(FCOL, DTROW(dp), 0, NFFMT, DNFIL(dp));
    }
    else
      putcxy(UCOL, DTROW(dp), '-');
  }

} /* showdlist() */

/* Display whole directory line for directory dp */
LOCAL VOID showdline(dp)
  register dlist *dp;
{
  register int i, j;

  /* Is directory on screen? */
  if(dp && DTROW(dp) >= firstdline && DTROW(dp) <= lastdline) {
    (void) cursorxy(TCOL, DTROW(dp));
    clearline();
    (void) setgraphic(GC_ON);
    if(dp == droot)             /* Root directory */
      (void) putchar(DNEXT(dp) ? GC_UL : GC_HB);
    else {                      /* Other directory */
      (void) putchar(DNEXT(dp) ? GC_VB : GC_LL);
      for(i = 1, j = DLEVL(dp) - 1; i < j; i++)
	if(DINFO(dp) & (1 << (i-1)))
	  putcxy(TCOL+i*indent, DTROW(dp), GC_VB);
      if(DINFO(dp) & (1 << (i-1)))
	putcxy(TCOL+i*indent, DTROW(dp), GC_LT);
      else
	putcxy(TCOL+i*indent, DTROW(dp), GC_LL);
    }
    for(i = 2; i < indent; i++)
      (void) putchar(GC_HB);
    (void) setgraphic(GC_OFF);
    showdlist(dp, 0);           /* Display name */
  }

} /* showdline() */

/* Display the directory list from line f to line t */
LOCAL VOID showdtree(f, t, c)
  register int f, t, c;
{
  register dlist *dp;

  /* Search for first directory to print ... */
  for(dp = tdlist; dp && DTROW(dp) < f; dp = (dlist *) DNEXT(dp))
    ;
  /* ... and print out from f to t */
  for( ; dp && DTROW(dp) <= t; dp = (dlist *) DNEXT(dp))
    showdline(dp);
  /* Clear to end of tree window */
  if(c && dp && DTROW(dp) < lastdline)
    clearwindow(DTROW(dp), lastdline);

} /* showdtree() */

/* Display tree position bar */
LOCAL VOID showtbar()
{
  static int bar = 0;
  register dlist *dp;
  register int f, l, i;

  if(dircount > (ndlines + 1)) {      /* More dirs than lines */
    dp = tdlist;
    while(DNEXT(dp) && DTROW(dp) < lastdline)
      dp = (dlist *) DNEXT(dp);
    f = ((DDNUM(tdlist) + 1) * ndlines) / dircount + firstdline;
    l = ((DDNUM(dp)     + 1) * ndlines) / dircount + firstdline;
    if(f <= firstdline)
      f = DPREV(tdlist) ? firstdline + 1 : firstdline;
    if(l >= lastdline)
      l = DNEXT(dp)     ? lastdline  - 1 : lastdline;
    i = firstdline;
    while(i < f)
      putcxy(BCOL, i++, ' ');
    if(videomode && (videocap & VA_REVERSE)) {
      setvideo(DA_HALFREV);
      putcxy(BCOL, i++, ' ');
      while(i <= l)
	putcxy(BCOL, i++, ' ');
      setvideo(DA_NORMAL);
    }
    else {
      (void) setgraphic(GC_ON);
      putcxy(BCOL, i++, GC_TT);
      while(i < l)
	putcxy(BCOL, i++, GC_VB);
      putcxy(BCOL, i++, GC_BT);
      (void) setgraphic(GC_OFF);
    }
    while(i <= lastdline)
      putcxy(BCOL, i++, ' ');
    bar = 1;
  }
  else if(bar) {
    for(i = firstdline; i <= lastdline; i++)
      putcxy(BCOL, i, ' ');
    bar = 0;
  }

} /* showtbar() */

/* Display separator line between tree and file window */
LOCAL VOID showsline()
{
  register char *cp, *cz;
  register int i;

  cp = CPNAM + strlen(rootdir);
  cz = CZOOM ? CZOOM : "*";
  setvideo(DA_REVERSE);
  if(*cp)
    i = putfxy(0, SLIN, 0, ".%s/%s: %d file(s) %d dir(s)", cp, cz, CNFIL, CNDIR);
  else
    i = putfxy(0, SLIN, 0, "./%s: %d file(s) %d dir(s)", cz, CNFIL, CNDIR);
  while(i++ < columns)
    (void) putchar(' ');
  setvideo(DA_NORMAL);

} /* showsline() */

/*
 *      TREE SCREEN UPDATE AND REFRESH
 */

/* Update tree screen */
LOCAL int updatetree(f)
  register int f;
{
  register int n, rv;

  if(treeflag & SF_LIST) {                      /* Check current directory */
    if((CFLAG != FL_FIL || changedlist(cdlist)) && newflist(cdlist) != RV_OK)
      return(RV_ERR);
  }
  rv = RV_OK;
  if(keypressed())                              /* There are chars in input buffer */
    return(rv);
  if(treeflag == SF_FULL) {                     /* Full screen update */
    clearscreen();
    cdlast = tdlast = DNULL;
  }
  if(treeflag & SF_TREE) {                      /* Tree screen */
    n = tdlast ? DDNUM(tdlast) - DDNUM(tdlist) : 0;
    if(CANSCROLL && n < 0 && n > -ndlines) {
      (void) windowup(firstdline, lastdline, -n);
      showdtree(lastdline + n + 1, lastdline, 0);
      showdlist(cdlast, 0);
    }
    else if(CANSCROLL && n > 0 && n < ndlines) {
      (void) windowdown(firstdline, lastdline, n);
      showdtree(firstdline, firstdline + n - 1, 0);
      showdlist(cdlast, 0);
    }
    else
      showdtree(firstdline, lastdline, treeflag != SF_FULL);
    treeflag |= SF_PBAR;
  }
  else if(treeflag & SF_LAST)                   /* Last directory */
    showdlist(cdlast, 0);
  if(treeflag & SF_PBAR)                        /* Tree position bar */
    showtbar();
  if(treeflag & SF_LIST) {                      /* Current directory */
    showdlist(cdlist, 1);
    showsline();
  }
  else if(treeflag & SF_SEPL)
    showsline();
  if(treeflag & SF_FILE)                        /* File list */
    showflist(firstfline);
  if(treeflag & SF_HELP && !f)                  /* Help line */
    putmenu("TREE:", menuline);
  if(treeflag & SF_ECHO && !f)                  /* Echo line */
    (void) putecho("%s: %d dir(s) %d file(s)", rootdir, dircount, filecount);
  /* Position to current directory, set variables and return */
  (void) cursorxy(TCOL+DTCOL(cdlist)-1, DTROW(cdlist));
  cdlast = cdlist;
  tdlast = tdlist;
  treeflag = 0;
  return(rv);

} /* updatetree() */

/*
 *      SCROLL UP OR DOWN DIRECTORY TREE
 */

/* Scroll directory tree */
LOCAL int scrolltree(dir)
  register int dir;
{
  register dlist *dp;
  register int i;

  /* Is scrolling possible? */
  if((dir < 0 && DPREV(tdlist) == GNULL) || (dir > 0 && CNEXT == GNULL))
    return(0);
  if(dir < 0) {                 /* Scroll down */
    tdlist = (dlist *) DPREV(tdlist);
    if(CANSCROLL) {
      (void) windowdown(firstdline, lastdline, 1);
      showdline(tdlist);
      treeflag |= SF_MOVE|SF_PBAR;
    }
    else
      treeflag |= SF_TREE|SF_LIST|SF_PBAR;
  }
  else {                        /* Scroll up */
    for(dp = tdlist, i = ndlines; i >= 0 ; i--)
      if(((dp = (dlist *) DNEXT(dp))) == DNULL)
	return(0);
    tdlist =  (dlist *) DNEXT(tdlist);
    if(CANSCROLL) {
      (void) windowup(firstdline, lastdline, 1);
      for(dp = tdlist; DTROW(dp) < lastdline; dp = (dlist *) DNEXT(dp))
	;
      showdline(dp);
      treeflag |= SF_MOVE|SF_PBAR;
    }
    else
      treeflag |= SF_TREE|SF_LIST|SF_PBAR;
  }
  if(DTROW(cdlist) < firstdline)        /* Change current directory */
    (void) gotree(1);                   /* if out of screen         */
  else if(DTROW(cdlist) > lastdline)
    (void) gotree(-1);
  return(1);

} /* scrolltree() */

/*
 *      CHECK TREE
 */

/* Check if all directories in tree are unchanged or read in */
LOCAL int checktree(msg)
  register char *msg;
{
  register dlist *dp;
  register int c;

  if(VARSET(V_ST) && scandlist(cdlist) != RV_OK)
    return(RV_NUL);
  for(dp = (dlist *) CNEXT; dp && DLEVL(dp) > CLEVL; dp = (dlist *) DNEXT(dp)) {
    if(DFLAG(dp) != FL_FIL) {
      ++buildflag;
      bell(VARSET(V_BL));
      puthelp("%s (Y:continue  ELSE:quit)", who);
      c = hitakey(msg, echoline, DA_NONE);
      return(c == 'y' ? RV_OK : c);
    }
  }
  return(RV_OK);

} /* checktree() */

/*
 *      TAG/UNTAG FILES IN TREE
 */

/* Tag files in directory tree */
LOCAL int tagtree(t)
  register int t;
{
  char input[PATLEN];
  register dlist *dp;
  register int f, c, ff, nt;

  who = t ? "TAG TREE" : "TAG DIRECTORY";
  if(t && (c = checktree(mustup)) != RV_OK)
    return(c);

  puthelp("%s: Give file pattern (CR:%s)", who, tpattern[0] ? tpattern : "quit");
  if((c = getpattern(input, "Tag which files:")) == RV_OK)
    (void) strcpy(tpattern, input);
  else if(c < RV_NUL || (c == RV_NUL && tpattern[0] == '\0'))
    return(c);

  /* Walk thru subtree */
  puthelp("%s %s", who, cancel);
  dp = cdlist;
  ff = 0;
  do {
    nt = DNTAG(dp);
    if( !DCANC(dp))
      continue;
    else if(DFLAG(dp) != FL_FIL && (f = newflist(dp)) != RV_OK) /* Update! */
      return(f);
    else if(keypressed() && hitakey(NULL) < RV_NUL)
      break;
    /* Walk thru file list */
    for(f = 0; f < DNFIL(dp); f++)
      if(umatch(dp, f, tpattern) > 0) {
	FITAG(dp, f) = FF_TAG;
	++DNTAG(dp);
	++ff;
      }
    if(nt != DNTAG(dp))
      showdlist(dp, 0);
  } while(t && (dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > CLEVL);

  if(ff > 0)
    treeflag |= SF_FILE|SF_LIST;
  puthelp("%s: %s %s", who, tpattern, hitkey);
  (void) putecho("Tagged %d file(s) matching %s", ff, tpattern);
  return(hitakey(NULL));

} /* tagtree() */

/* Untag files in directory tree */
LOCAL int untagtree(t)
  register int t;
{
  char pat[PATLEN];
  register dlist *dp;
  register int f, c, ff, nt;

  who = t ? "UNTAG TREE" : "UNTAG DIRECTORY";
  if(t && (c = checktree(mustup)) != RV_OK)
    return(c);

  puthelp("%s: Give file pattern (CR:all files)", who);
  if((c = getpattern(pat, "Untag which files:")) < RV_NUL)
    return(c);
  else if(c == RV_NUL)
    (void) strcpy(pat, "*");

  /* Walk thru subtree */
  puthelp("%s %s", who, cancel);
  dp = cdlist;
  ff = 0;
  do {
    nt = DNTAG(dp);
    if( !DCANC(dp))
      continue;
    else if(DFLAG(dp) != FL_FIL && (f = newflist(dp)) != RV_OK)
      return(f);
    else if(keypressed() && hitakey(NULL) < RV_NUL)
      break;
    /* Walk thru file list */
    for(f = 0; f < DNFIL(dp); f++)
      if(ISTAG(dp, f) && umatch(dp, f, pat) > 0) {
	FITAG(dp, f) = FF_NONE;
	if(DNTAG(dp) > 0)
	  --DNTAG(dp);
	++ff;
      }
    if(nt != DNTAG(dp))
      showdlist(dp, 0);
  } while(t && (dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > CLEVL);

  if(ff > 0)
    treeflag |= SF_FILE|SF_LIST;
  puthelp("%s: %s %s", who, pat, hitkey);
  (void) putecho("Untagged %d file(s) matching %s", ff, pat);
  return(hitakey(NULL));

} /* untagtree() */

/* Check if there are tagged files */
LOCAL int checktagged()
{
  register dlist *dp;
  register int f, n;

  /* Walk thru subtree */
  dp = cdlist;
  n = 0;
  do {
    if( !DCANC(dp))
      continue;
    /* Walk thru file list */
    for(f = 0; f < DNFIL(dp); f++)
      if(ISTAG(dp, f))
	++n;
  } while((dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > CLEVL);
  return(n);

} /* checktagged() */

/*
 *      COMMANDS WORKING ON TAGGED FILES
 */

/* Remove tagged files in tree */
GLOBL int removetagged()
{
  register dlist *dp;
  register int c, f, n, rflag;

  who = "REMOVE TAGGED FILES";
  if(checktagged() == 0) {      /* No tagged files */
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "No tagged files found"));
  }

  puthelp("%s (Y:request  N:don't request  ELSE:quit)", who);
  c = hitakey("Request before removing tagged files ?", echoline, DA_NONE);
  if( !(c == 'y' || c == 'n'))
    return(c);
  rflag = c == 'y';

  if(rflag)
    puthelp("REMOVE FILE (Y:remove  Q:quit  ELSE:don't remove)");
  else
    puthelp("%s (In progress ...)", who);

  /* Walk thru subtree */
  dp = cdlist;
  n = 0;
  do {
    if(CNTAG > 0) {             /* Contains tagged files */
      treeflag &= ~(SF_ECHO|SF_HELP);
      (void) updatetree(0);
      /* Walk thru file list */
      for(f = CNFIL - 1; f >= 0; f--) {
	if(ISTAG(cdlist, f)) {
	  c = removefile(cdlist, f, rflag);
	  if(c == 'q' || c < RV_NUL)
	    goto ENDLOOP;
	  else if(c == RV_OK) {
	    ++n;
	    FITAG(cdlist, f) = FF_NONE;
	    if(CNTAG > 0)
	      --CNTAG;
	    if(rflag)
	      showflist(firstfline);
	  }
	}
      }
      if(CFLAG == FL_CHG) {
	if((c = newflist(cdlist)) != RV_OK)
	  goto ENDLOOP;
	else
	  showdlist(cdlist, 0);
      }
    }
  } while(gotree(1) && CLEVL > DLEVL(dp));

ENDLOOP:
  if(c == RV_END)
    return(c);
  while(cdlist != dp)           /* Position to starting directory */
    (void) gotree(-1);
  puthelp("%s %s", who, hitkey);
  if(n > 0) {
    treeflag = SF_FULL;
    (void) putecho("Removed %d tagged file(s)",  n);
  }
  else
    (void) putecho("No files removed");
  return(hitakey(NULL));

} /* removetagged() */

LOCAL char *dirselect(what)
  register char *what;
{
  register char *dn;

  dn = selectdir(what);
  treeflag = SF_FULL;
  treeflag &= ~(SF_HELP|SF_ECHO);
  (void) updatetree(0);
  return(dn);

} /* dirselect() */

/* Move tagged files in tree */
GLOBL int movetagged()
{
  char name[NAMELEN];
  struct stat st;
  register dlist *dp;
  register char *to;
  register int c, f, n, rflag;

  who = "MOVE TAGGED FILES";
  if(checktagged() == 0) {      /* No tagged files */
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "No tagged files found"));
  }

  puthelp("%s: Give destination directory (CR:select one)", who);
  c = putecho("Move tagged files to:");
  if((c = getline(name, sizeof(name), c, 0, NULL, CLIST, 0)) < RV_NUL)
    return(c);
  if(c == RV_OK) {
    to = strcpy(name, pathname(name, CPNAM));
    if((*statfun)(to, &st) < 0) {
      puthelp("%s %s", who, hitkey);
      return(errequest(name, "Cannot stat"));
    }
    else if(STFMT(&st) != S_IFDIR) {
      puthelp("%s %s", who, hitkey);
      return(errequest(name, "Is not a directory"));
    }
  }
  else if((to = dirselect("moving files")) == NULL) {
    fileflag |= SF_ECHO|SF_HELP;
    return(RV_NUL);
  }

  puthelp("%s (Y:request  N:don't request  ELSE:quit)", who);
  c = hitakey("Request before moving tagged files ?", echoline, DA_NONE);
  if( !(c == 'y' || c == 'n'))
    return(c);
  rflag = c == 'y';

  if(rflag)
    puthelp("MOVE FILE (Y:copy  Q:quit  ELSE:don't copy)");
  else
    puthelp("%s (In progress ...)", who);

  /* Walk thru subtree */
  dp = cdlist;
  n = 0;
  do {
    if(CNTAG > 0) {             /* Contains tagged files */
      treeflag &= ~(SF_ECHO|SF_HELP);
      (void) updatetree(0);
      /* Walk thru file list */
      for(f = CNFIL - 1; f >= 0; f--) {
	if(ISTAG(cdlist, f)) {
	  c = movefile(cdlist, f, to, rflag);
	  if(c == 'q' || c < RV_NUL)
	    goto ENDLOOP;
	  else if(c == RV_OK) {
	    ++n;
	    FITAG(cdlist, f) = FF_NONE;
	    if(CNTAG > 0)
	      --CNTAG;
	  }
	}
      }
      if(CFLAG == FL_CHG) {
	if((c = newflist(cdlist)) != RV_OK)
	  goto ENDLOOP;
	else
	  showdlist(cdlist, 0);
      }
    }
  } while(gotree(1) && CLEVL > DLEVL(dp));

ENDLOOP:
  if(c == RV_END)
    return(c);
  checkdlist(to);
  while(cdlist != dp)           /* Position to starting directory */
    (void) gotree(-1);
  puthelp("%s %s", who, hitkey);
  if(n > 0) {
    treeflag = SF_FULL;
    (void) putecho("Moved %d file(s) to %s",  n, to);
  }
  else
    (void) putecho("No files moved");
  return(hitakey(NULL));

} /* movetagged() */

/* Copy tagged files in tree */
GLOBL int copytagged()
{
  char name[NAMELEN];
  struct stat st;
  register dlist *dp;
  register char *to;
  register int c, f, n, rflag;

  who = "COPY TAGGED FILES";
  if(checktagged() == 0) {      /* No tagged files */
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "No tagged files found"));
  }

  puthelp("%s: Give destination directory (CR:select one)", who);
  c = putecho("Copy tagged files to:");
  if((c = getline(name, sizeof(name), c, 0, NULL, CLIST, 0)) < RV_NUL)
    return(c);
  if(c == RV_OK) {
    to = strcpy(name, pathname(name, CPNAM));
    if((*statfun)(to, &st) < 0) {
      puthelp("%s %s", who, hitkey);
      return(errequest(name, "Cannot stat"));
    }
    else if(STFMT(&st) != S_IFDIR) {
      puthelp("%s %s", who, hitkey);
      return(errequest(name, "Is not a directory"));
    }
  }
  else if((to = dirselect("copying files")) == NULL) {
    fileflag |= SF_ECHO|SF_HELP;
    return(RV_NUL);
  }

  puthelp("%s (Y:request  N:don't request  ELSE:quit)", who);
  c = hitakey("Request before copying tagged files ?", echoline, DA_NONE);
  if( !(c == 'y' || c == 'n'))
    return(c);
  rflag = c == 'y';

  if(rflag)
    puthelp("COPY FILE (Y:copy  Q:quit  ELSE:don't copy)");
  else
    puthelp("%s (In progress ...)", who);

  /* Walk thru subtree */
  dp = cdlist;
  n = 0;
  do {
    if(CNTAG > 0) {             /* Contains tagged files */
      treeflag &= ~(SF_ECHO|SF_HELP);
      (void) updatetree(0);
      /* Walk thru file list */
      for(f = CNFIL - 1; f >= 0; f--) {
	if(ISTAG(cdlist, f)) {
	  c = copyfile(cdlist, f, to, rflag);
	  if(c == 'q' || c < RV_NUL)
	    goto ENDLOOP;
	  else if(c == RV_OK) {
	    ++n;
	    FITAG(cdlist, f) = FF_NONE;
	    if(CNTAG > 0)
	      --CNTAG;
	  }
	}
      }
    }
  } while(gotree(1) && CLEVL > DLEVL(dp));

ENDLOOP:
  if(c == RV_END)
    return(c);
  checkdlist(to);
  while(cdlist != dp)           /* Position to starting directory */
    (void) gotree(-1);
  puthelp("%s %s", who, hitkey);
  if(n > 0) {
    treeflag = SF_FULL;
    (void) putecho("Copied %d file(s) to %s",  n, to);
  }
  else
    (void) putecho("No files copied");
  return(hitakey(NULL));

} /* copytagged() */

/*
 *      RESIZE DIRECTORY TREE WINDOW
 */

/* Recalculate and update dlists on directory window */
GLOBL VOID calculatetree(n)
  register int n;
{
  register dlist *dp;

  if(tdlist && cdlist) {        /* Tree window needs update */
    if(n < 0 && DTROW(cdlist) > lastdline) {
      while(DTROW(cdlist) > lastdline)
	tdlist = (dlist *) DNEXT(tdlist);
    }
    else if(n > 0) {
      for( ; n > 0; n--) {
	for(dp = cdlist; dp && DTROW(dp) < lastdline; dp = (dlist *) DNEXT(dp))
	  ;
	if(DPREV(tdlist) && dp == DNULL)
	  tdlist = (dlist *) DPREV(tdlist);
	else
	  break;
      }
    }
  }

} /* calculatetree() */

/* Enlarge or shrink the tree window  */
LOCAL int resizetree(dir)
  register int dir;
{
  register dlist *dp;

  /* Enlarge tree window if possible */
  if(dir > 0 && nflines > MINFIL) {
    ++lastdline;
    for(dp = cdlist; dp && DTROW(dp) < lastdline; dp = (dlist *) DNEXT(dp))
      ;
    if(DPREV(tdlist) && dp == DNULL) {
      tdlist = (dlist *) DPREV(tdlist);
      if(CANSCROLL) {
	(void) windowdown(firstdline, lastfline, 1);
	showdline(tdlist);
	treeflag |= SF_MOVE;
      }
      else
	treeflag |= SF_TREE|SF_LIST;
    }
    else if(CANSCROLL) {
      (void) windowdown(lastdline, lastfline, 1);
      showdline(dp);
      treeflag |= SF_MOVE;
    }
    else
      treeflag |= SF_TREE|SF_LIST|SF_FILE;
    ++ndlines;
    --nflines;
    ++firstfline;
    checklines(0);
  }
  /* Shrink tree window if possible */
  else if(dir < 0 && nflines < calculatelines()) {
    --ndlines;
    --firstfline;
    if(DTROW(cdlist) == lastdline) {
      tdlist = (dlist *) DNEXT(tdlist);
      if(CANSCROLL) {
	(void) windowup(firstdline, lastfline, 1);
	showflist(lastfline);
	treeflag |= SF_MOVE;
      }
      else
	treeflag |= SF_TREE|SF_LIST|SF_FILE;
    }
    else if(CANSCROLL) {
      (void) windowup(lastdline, lastfline, 1);
      showflist(lastfline);
      treeflag |= SF_MOVE;
    }
    else
      treeflag |= SF_TREE|SF_LIST|SF_FILE;
    --lastdline;
    ++nflines;
    checklines(0);
  }
  else
    return(0);

  treeflag |= SF_SEPL|SF_PBAR;
  return(1);

} /* resizetree() */

/*
 *      INFORMATION AND STATUS OF DIRECTORY
 */

/* Show some directory information */
LOCAL int infodir()
{
  char buf[EXECLEN];
  struct stat st;
  register FILE *pp;
  register int i;

  who = "INFO";
  puthelp("%s: %s %s", who, CPNAM, hitkey);
  if((*statfun)(CPNAM, &st))
    return(errequest(CFNAM, "Cannot stat"));

  (void) putecho("Scanning disk for disk usage, wait a moment ... ");
  flushout();
  (void) sprintf(buf, "%s %s", DUDIR, CPNAM);
  if(pp = popen(buf, "r")) {    /* Let du summarize used blocks */
    (void) fgets(buf, sizeof(buf), pp);
    (void) pclose(pp);
    i = 0;
    while(buf[i] >= '0' && buf[i] <= '9')
      ++i;
    buf[i] = '\0';
  }
  else                          /* Error in calling du */
    (void) strcpy(buf, "?");
  (void) putecho("Access:%s Blocks:%s Files:%d Dirs:%d Date:%s",
		  fileaccess(&st), buf, CNFIL, CNDIR, ctime(&st.st_mtime));
  return(hitakey(NULL));

} /* infodir() */

/* Show and change directory status */
LOCAL int statusdir()
{
  register int c;

  c = statusfile(CPNAM, 0);
  if(buildflag) {               /* Rebuilding needed */
    c = newflist(cdlist);
    buildflag = 0;
  }
  return(c);

} /* statusdir() */

/*
 *      BACKUP DIRECTORY OR TREE
 */

/* Create filelist for backup */
LOCAL int backuplist(name, t)
  register char *name;
  register int t;
{
  char fname[NAMELEN];
  register FILE *file;
  register dlist *dp;
  register char *dname;
  register int dlen, i;

  (void) strcpy(fname, pathname(name, CPNAM));
  if(file = fopen(fname, "w")) {
    /* Write out tree or subtree list */
    dlen = strlen(CPNAM);
    for(i = 0; i < CNFIL ; i++)
      if( !t || ISTAG(cdlist, i)) {
	(void) fprintf(file, "%s\n", FFNAM(cdlist, i));
	FITAG(cdlist, i) = FF_NONE;
	if(CNTAG > 0)
	  --CNTAG;
      }
    for(dp = (dlist *) CNEXT; dp && DLEVL(dp) > CLEVL; dp = (dlist *) DNEXT(dp)) {
      if( !DCANC(dp))
	continue;
      dname = &(DPNAM(dp)[dlen+1]);
      for(i = 0; i < DNFIL(dp); i++)
	if( !t || ISTAG(dp, i)) {
	  (void) fprintf(file, "%s\n", pathname(FFNAM(dp, i), dname));
	  FITAG(dp, i) = FF_NONE;
	  if(DNTAG(dp) > 0)
	    --DNTAG(dp);
	}
    }
    (void) fclose(file);
  }
  else {
    puthelp("%s %s", who, hitkey);
    return(errequest(prgname, "Cannot create backup list file"));
  }

  checkdlist(fname);            /* Update needed? */
  return(buildflag ? updatedlist() : RV_OK);

} /* backuplist() */

/* Backup directory/subtree or tagged files in subtree */
LOCAL int backupdir(t)
  register int t;
{
  char name[INPLEN], list[NAMELEN], buf[EXECLEN];
  register dlist *dp;
  register int c;

  who = t ? "BACKUP TAGGED FILES" : "BACKUP TREE";
  if( !VARSET(V_BK)) {                  /* No backup program */
    puthelp("%s %s", who, hitkey);
    return(errequest(prgname, "No backup program defined"));
  }
  else if(t && checktagged() == 0) {    /* No tagged files */
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "No tagged files found"));
  }
  else if((c = changelist(cdlist, who)) != RV_OK)
    return(c);

  /* Update subtree if needed */
  if( !t) {
    if((c = checktree(mustup)) != RV_OK)
      return(c);
    for(dp = (dlist *) CNEXT; dp && DLEVL(dp) > CLEVL; dp = (dlist *) DNEXT(dp))
      if(DFLAG(dp) != FL_FIL) {
	if((c = newflist(dp)) != RV_OK)
	  return(c);
	else
	  treeflag |= SF_TREE|SF_LIST;
      }
    if(treeflag)
      (void) updatetree(0);
  }

  puthelp("%s: Give filename for backup list (CR:$HOME/%s)", who, UTBACK);
  c = putecho("List file name:");
  if((c = getline(name, sizeof(name), c, 0, NULL, CLIST, 1)) == RV_OK)
    (void) strcpy(list, name);
  else if(c == RV_NUL)
    (void) strcpy(list, pathname(UTBACK, home));
  else
    return(c);
  if((c = backuplist(list, t)) != RV_OK)
    return(c);

  /* Build command line and call backup program */
  treeflag = SF_FULL;
  if( !VARSET(V_BKO))
    (void) sprintf(buf, "%s %s", VARVAL(V_BK), list);
  else
    (void) sprintf(buf, "%s %s %s", VARVAL(V_BK), VARVAL(V_BKO), list);
  puthelp("%s %s", who, cancel);
  (void) putecho("Executing backup program %s ...", VARVAL(V_BK));
  c = callsystem(buf, 1, 0);

  puthelp("%s %s", who, hitkey);
  if(c != RV_OK)
    return(errequest(VARVAL(V_BK), "Error in backup"));
  bell(VARSET(V_BL));
  return(hitakey("Backup done", echoline, DA_NONE));

} /* backupdir() */

/*
 *      CHANGE TO DIRECTORY
 */

/* Goto a directory */
LOCAL int changedir()
{
  static char pattern[PATLEN] = { '\0' };
  char input[PATLEN];
  dlist *dp;
  int c, path, found;

  who = "CHANGE DIRECTORY";
  /* Get directory name to change to */
  puthelp("%s: Give directory name (CR:next %s)", who, pattern[0] ? pattern : "quit");
  c = putecho("Change to:");
  if((c = getline(input, sizeof(input), c, 0, NULL, CLIST, 0)) < RV_NUL)
    return(c);
  else if(c == RV_NUL) {
    if(pattern[0] == '\0')
      return(c);
  }
  else
    (void) strcpy(pattern, input);

  /* Search for directory in tree */
  found = -1;
  path  = strchr(pattern, '/') != NULL;
  for(dp = (dlist *) CNEXT; dp; dp = (dlist *) DNEXT(dp))
    if(match(path ? DPNAM(dp) : DFNAM(dp), pattern) > 0) {
      found = DDNUM(dp);
      break;
    }
  if(found < 0)
    for(dp = droot; dp && DDNUM(dp) <= CDNUM; dp = (dlist *) DNEXT(dp))
      if(match(path ? DPNAM(dp) : DFNAM(dp), pattern) > 0) {
	found = DDNUM(dp);
	break;
      }

  if(found < 0) {               /* Not found */
    puthelp("%s %s", hitkey, who);
    return(errequest(pattern, "Not found"));
  }

  if(CDNUM == found)            /* Found */
    return(RV_OK);
  else if(CDNUM > found)        /* Position to directory in tree */
    while(CDNUM > found)
      (void) gotree(-1);
  else
    while(CDNUM < found)
      (void) gotree(1);

  return(RV_OK);

} /* changedir() */

/*
 *      LIST FILES IN TREE
 */

/* List matching/tagged files in tree */
LOCAL int listtree(t)
  register int t;
{
  char pat[PATLEN];
  register dlist *dp;
  register int c, f, ff, l;

  who = t ? "LIST TAGGED FILES" : "LIST FILES";
  if(t && checktagged() == 0) {
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "No tagged files found"));
  }
  else if((c = checktree(mustup)) != RV_OK)
    return(c);

  if( !t) {
    puthelp("%s: Give file pattern (CR:all files)", who);
    if((c = getpattern(pat, "List which files:")) < RV_NUL)
      return(c);
    else if(c == RV_NUL)
      (void) strcpy(pat, "*");
  }

  /* Show all matching files */
  dp = cdlist;
  l = firstline;
  ff = 0;
  c = RV_OK;
  do {
    if( !DCANC(dp))
      continue;
    else if(DFLAG(dp) != FL_FIL && (c = newflist(dp)) != RV_OK)
      return(c);
    for(f = 0; f < DNFIL(dp); f++)
      if((t && ISTAG(dp, f)) || umatch(dp, f, pat) > 0) {
	if(l == firstline) {
	  if(ff > 0) {
	    if(t)
	      puthelp("%s (CR:continue  ELSE:quit)", who);
	    else
	      puthelp("%s: %s (CR:continue  ELSE:quit)", who, pat);
	    c = hitakey("Continue listing ?", echoline, DA_NONE);
	    if( !(c == '\n' || c == ' '))
	      break;
	    else
	      c = RV_OK;
	  }
	  treeflag = SF_FULL;
	  clearwindow(firstline, lastline);
	}
	++ff;
	(void) putfxy(0, l, 0, "%s", pathname(FFNAM(dp, f), DPNAM(dp)));
	if(++l > lastline)
	  l = firstline;
      }
  } while(c == RV_OK && (dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > CLEVL);

  if(c >= RV_NUL) {
    puthelp("%s %s", who, hitkey);
    if(t)
      (void) putecho("Listed %d tagged file(s)", ff);
    else
      (void) putecho("Listed %d file(s) matching %s", ff, pat);
    c = hitakey(NULL);
  }
  return(c);

} /* listtree() */

/*
 *      CREATE A DIRECTORY
 */

/* Create a directory */
LOCAL int makedir()
{
  char newname[NAMELEN], buf[EXECLEN];
  register dlist *dp;
  register int c, i;

  who = "MAKE DIRECTORY";
  if( !CCANC) {                 /* Cannot change to directory */
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "Cannot make directory"));
  }

  puthelp("%s: Give directory name (CR:quit)", who);
  c = putecho("Make directory:");
  if((c = getline(newname, sizeof(newname), c, 0, NULL, GNULL, 0)) != RV_OK)
    return(c);

  puthelp("%s %s", who, hitkey);
  if(strchr(newname, '/') || EQU(newname, ".") || EQU(newname, ".."))
    return(errequest(newname, "Cannot make directory"));
  for(i = 0; i < CNFIL; i++)
    if(EQU(FFNAM(cdlist, i), newname))
      return(errequest(newname, "Already exists"));

  /* Build command line and call mkdir program */
  (void) sprintf(buf, "%s %s", MKDIR, pathname(newname, CPNAM));
  if(callsystem(buf, 0, 0) != RV_OK)
    return(errequest(newname, "Error in creating"));

  /* Insert new directory into tree and file lists */
  dp = newdlist(newname, FL_FIL);
  c  = newflist(cdlist);

  /* Update flag and return */
  if(dp) {
    treeflag = SF_FULL;
    return(c);
  }
  return(RV_NUL);

} /* makedir() */

/*
 *      REMOVE A DIRECTORY OR TREE
 */

/* Remove a directory */
LOCAL int removedir()
{
  char buf[EXECLEN];
  register dlist *dp;
  register int c, i, rflag;

  who = "REMOVE DIRECTORY";
  /* Check if removing is permitted */
  if( !CCANC || cdlist == droot || CNDIR > 0) {
    puthelp("%s %s", who, hitkey);
    if( !CCANC)                 /* Cannot change to directory */
      return(errequest(CFNAM, "Cannot remove"));
    else if(cdlist == droot)    /* Root cannot be removed */
      return(errequest(CFNAM, "Cannot remove root directory"));
    else if(CNDIR > 0)          /* Contains subdirectories */
      return(errequest(CFNAM, "Contains subdirectories. Cannot remove subtrees"));
  }

  puthelp("%s (Y:remove  ELSE:quit)", who);
  rflag = 0;
  (void) putecho("Remove directory %s ?", CFNAM);
  if((c = hitakey(NULL)) != 'y')
    return(c);
  if(CNFIL > 0) {
    puthelp("%s (Y:request  N:don't request  ELSE:quit)", who);
    c = hitakey("Directory is not empty, request before removing files ?", echoline, DA_NONE);
    if( !(c == 'y' || c == 'n'))
      return(c);
    rflag = c == 'y';
  }

  /* First remove files from directory */
  if(rflag)
    puthelp("REMOVE FILE (Y:remove  Q:quit  ELSE:don't remove)");
  else
    puthelp("%s (In progress ...)", who);
  for(i = CNFIL - 1; i >= 0; i--) {
    c = removefile(cdlist, i, rflag);
    if(c == 'q' || c < RV_NUL)
      return(c);
    else if(c == RV_OK && rflag) {
      showflist(firstfline);
      (void) putfxy(FCOL, DTROW(cdlist), 0, NFFMT, CNFIL);
    }
  }

  /* There are files: cannot remove directory */
  if(CNFIL > 0) {
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "Is not empty"));
  }

  if(rflag) {                   /* Request before removing */
    puthelp("%s (Y:remove  ELSE:quit)", who);
    (void) putecho("Remove directory %s ?", CFNAM);
    if((c = hitakey(NULL)) != 'y')
      return(c);
  }
  (void) changelist(droot, NULL);
  (void) sprintf(buf, "%s %s", RMDIR, CPNAM);
  if(callsystem(buf, 0, 0) != RV_OK) {
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "Error in removing"));
  }

  /* Get new top directory on screen and current directory */
  for(dp = cdlist; dp && DTROW(dp) <= lastdline; dp = (dlist *) DNEXT(dp))
    ;
  if(cdlist == tdlist || (tdlist != droot && dp == DNULL)) {
    tdlist = (dlist *) DPREV(tdlist);
    c = 1;
  }
  else {
    tdlast = DNULL;
    c = 0;
  }
  cdlist = (dlist *) CPREV;

  /* Delete directory list entry */
  deletedlist(CNEXT);

  /* Update flags */
  treeflag = SF_FULL;
  writeflag = 1;
  return(RV_OK);

} /* removedir() */

/*
 *      BUILD SUBDIRECTORY TREE
 */

/* Scan current directory for subdirs and build up and insert subtree */
LOCAL int buildtree()
{
  char inp[5], name[NAMELEN];
  register dlist *np, *dp, *p;
  register int lev, f, n;

  who = "BUILD TREE";
  /* Check if current directory already contains subdirectories */
  if((p = (dlist *) CNEXT) && DLEVL(p) > CLEVL) {
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "Contains subdirectories"));
  }
  /* Check if there is any directory to build */
  for(f = 0; f < CNFIL; f++)
    if(FMODE(cdlist, f) == FF_DIR)
      break;
  if(f >= CNFIL) {
    puthelp("%s %s", who, hitkey);
    return(errequest(CFNAM, "No subdirectories found"));
  }

  /* Get max level to build up the subtree */
  puthelp("%s: Give max tree level (CR:quit)", who, CFNAM);
  n = putecho("Give level:");
  n = getline(inp, sizeof(inp), n, 0, NULL, GNULL, 0);
  if(n != RV_OK || (lev = atoi(inp)) <= 0)
    return(n);

  /* Preserve next dlist and open dlist chain for buildread() */
  np    = (dlist *) CNEXT;
  CNEXT = GNULL;
  n = dircount;

  /* Build up the tree for all files which are directories */
  for(f = 0; f < CNFIL; f++)
    if(FMODE(cdlist, f) == FF_DIR) {
      (void) strcpy(name, pathname(FFNAM(cdlist, f), CPNAM));
      puthelp("%s: Building %s %s", FFNAM(cdlist, f), who, cancel);
      if(buildread(name, CLEVL+1, CLEVL+lev, 1) == RV_INT)
	break;
    }
  n = dircount - n;

  if(n > 0) {
    /* Search for last new dlist, update dlist numbers and close dlist chain */
    dp = cdlist;
    f = CDNUM;
    do
     DDNUM(dp) = f++;
    while(DNEXT(dp) && (dp = (dlist *) DNEXT(dp)));
    DNEXT(dp) = (glist *) np;   /* Close the chain */
    if(np)
      DPREV(np) = (glist *) dp;
    for(dp = np; dp; dp = (dlist *) DNEXT(dp), f++)
      DDNUM(dp) = f;
    infodlist();                /* Rebuild treeinfo */
    checkindent();              /* Check indention */
    treeflag = SF_FULL;
    bell(VARSET(V_BL));
    puthelp("%s %s", who, hitkey);
    (void) putecho("%d new directories built and inserted", n);
  }
  else {
    /* No new directories found and inserted, close dlist chain */
    CNEXT = (glist *) np;
    bell(VARSET(V_BL));
    puthelp("%s %s", who, hitkey);
    (void) putecho("No new directories built or inserted");
  }
  return(hitakey(NULL));

} /* buildtree() */

/*
 *      UPDATE ALL FILE LISTS
 */

/* Scan directory tree and update all file lists not yet read in or changed */
LOCAL int scantree(f)
  register int f;
{
  register dlist *dp;
  register int c;

  who = "UPDATE TREE";
  if(f && (c = checktree("Update tree ?")) != RV_OK)
    return(c);

  /* Walk thru directory list and update file lists if needed */
  if(buildflag) {
    puthelp("%s %s", who, cancel);
    dp = cdlist;
    do {
      if(keypressed() && hitakey(NULL) < RV_NUL)
	break;
      if( !DCANC(dp)) {                 /* Cannot cd: skip */
	DFLAG(dp) = FL_FIL;
	continue;
      }
      else if(DFLAG(dp) != FL_FIL) {    /* Rebuild file list */
	if((c = newflist(dp)) != RV_OK)
	  return(c);
	showdline(dp);
	flushout();
	treeflag |= SF_MOVE|SF_ECHO;
      }
    } while((dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > CLEVL);
  }

  return(RV_OK);

} /* scantree() */

/*
 *      SORT FILELISTS IN TREE
 */

/* Sort filelists in current directory or subtree */
LOCAL int sorttree(t)
  register int t;
{
  register dlist *dp;
  register int s;

  who = t ? "SORT TREE" : "SORT DIRECTORY";
  s = CSORT ? 0 : 1;            /* Toggle sort flag */

  /* Walk thru subtree */
  puthelp("%s %s", who, cancel);
  if(t) {
    for(dp = (dlist *) CNEXT; dp && DLEVL(dp) > CLEVL; dp = (dlist *) DNEXT(dp)) {
      if(keypressed() && hitakey(NULL) < RV_NUL)
	return(RV_NUL);
      (void) sortlist(dp, s);
    }
  }
  if((s = sortlist(cdlist, s)) == RV_OK)
    treeflag |= SF_FILE;

  return(s);

} /* sorttree() */

/*
 *      ZOOM FILELISTS IN TREE
 */

/* Get zoom pattern and rebuild filelists in directory or subtree */
LOCAL int zoomtree(t)
  register int t;
{
  char pat[PATLEN];
  register dlist *dp;
  register int c;

  who = t ? "ZOOM TREE" : "ZOOM DIRECTORY";
  puthelp("%s: Give file pattern (CR:all files)", who);
  if((c = getpattern(pat, "Zoom which files:")) < RV_NUL)
    return(c);

  /* Walk thru subtree */
  puthelp("%s %s", who, cancel);
  dp = cdlist;
  do {
    if(keypressed() && hitakey(NULL) < RV_NUL)
      break;
    if(zoomlist(dp, pat)) {
      showdline(dp);
      flushout();
    }
  } while(t && (dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > CLEVL);

  treeflag |= SF_FILE|SF_LIST;
  return(RV_OK);

} /* zoomtree() */

/*
 *      MARKED AND TAGGED DIRECTORIES
 */

/* Set/unset mark on current directory */
LOCAL VOID markdir(f)
  register int f;
{
  if(mdlist == cdlist && !f)    /* Reset mark */
    mdlist = DNULL;
  else                          /* Set mark */
    mdlist = cdlist;

} /* markdir() */

/* Go to marked directory */
LOCAL int gomarkdir()
{
  register dlist *mp, *dp;

  if(mdlist) {
    mp = mdlist;
    mdlist = cdlist;
    for(dp = cdlist; dp; dp = (dlist *) DNEXT(dp))      /* Search forward */
      if(dp == mp) {
	while(cdlist != mp && gotree(1))
	  ;
	return(1);
      }
    for(dp = droot; dp; dp = (dlist *) DNEXT(dp))       /* Search backward */
      if(dp == mp) {
	while(cdlist != mp && gotree(-1))
	  ;
	return(1);
      }
  }

  return(0);                                    /* No mark set */

} /* gomarkdir() */

/* Goto directory containing tagged files */
LOCAL int gotagged()
{
  register dlist *dp;

  for(dp = (dlist *) DNEXT(cdlist); dp; dp = (dlist *) DNEXT(dp))
    if(DNTAG(dp)) {
      while(gotree(1) && dp != cdlist)
	;
      return(1);
    }
  for(dp = droot; dp; dp = (dlist *) DNEXT(dp)) /* Search backward */
    if(DNTAG(dp)) {
      if(dp != cdlist)
	while(gotree(-1) && dp != cdlist)
	  ;
      return(1);
    }

  return(0);                                    /* No match */

} /* gotagged() */

/* Goto parent directory */
LOCAL int goparent()
{
  register int lev;

  if(cdlist != droot) {
    lev = CLEVL - 1;
    while(gotree(-1) && CLEVL != lev)
      ;
    return(1);
  }
  return(0);

} /* goparent() */

/*
 *      SEARCH FOR PATTERN IN TREE
 */

/* Search for pattern in tree */
LOCAL int greptree(t)
  register int t;
{
  char input[PATLEN];
  register dlist *dp;
  register int f, c, ff, nt;

  who = t ? "GREP TREE" : "GREP DIRECTORY";
  if(t && (c = checktree(mustup)) != RV_OK)
    return(c);

  puthelp("%s: Give search pattern (CR:%s)", who, gpattern[0] ? gpattern : "quit");
  c = putecho("Search for pattern:");
  if((c = getline(input, sizeof(input), c, 0, NULL, GNULL, 0)) == RV_OK)
    (void) strcpy(gpattern, input);
  else if(c < RV_NUL || (c == RV_NUL && gpattern[0] == '\0'))
    return(c);
  puthelp("%s: Give file pattern (CR:all files)", who);
  if((c = getpattern(input, "Search in which files:")) == RV_NUL)
    (void) strcpy(input, "*");
  else if(c != RV_OK)
    return(c);

  /* Walk thru subtree */
  dp = cdlist;
  do {
    nt = DNTAG(dp);
    ff = -1;
    if(CFLAG != FL_FIL && (c = newflist(cdlist)) != RV_OK)      /* Update! */
      return(c);
    /* Walk thru file list */
    for(c = RV_NUL, f = 0; f < CNFIL; f++) {
      ff = -1;
      /* Search in all matching files */
      if((c = umatch(cdlist, f, input)) > 0) {
	(void) putecho("Search for \'%s\' in %s", gpattern, pathname(FFNAM(cdlist, f), CPNAM));
	flushout();
	/* Search pattern found: what to do? */
	if((c = grepfile(cdlist, f)) == RV_OK) {
	  ff = f;
	  treeflag &= ~(SF_ECHO|SF_HELP);
	  (void) updatetree(0);
	  puthelp("%s (CR:next  SP:change dir  M:mark dir  T:tag file  ELSE:quit)", who);
	  (void) putecho("Found \'%s\': %s -> %s", gpattern, FFNAM(cdlist, f), CFNAM);
	  if((c = hitakey(NULL)) == 't') {
	    FITAG(cdlist, f) = FF_TAG;
	    ++CNTAG;
	    c = '\n';
	  }
	  else if(c == 'm') {
	    markdir(1);
	    c = '\n';
	  }
	  else if(c == ' ') {
	    if((c = filemenu(ff, RV_NUL)) == RV_END)
	      return(c);
	    (void) updatetree(0);
	    puthelp("%s (CR:continue  SP:select  ELSE:quit)", who);
	    (void) putecho("Continue searching for \'%s\':", gpattern);
	    if((c = hitakey(NULL)) == ' ')
	      return(RV_OK);
	  }
	  if(c != '\n')
	    break;
	}
	else if(c == RV_INT)
	  break;
      }
    }
    if(nt != DNTAG(dp))
      showdlist(dp, 0);
  } while(t && (c == RV_NUL || c == '\n') && gotree(1) && CLEVL > DLEVL(dp));

  if(c == RV_END)
    return(c);
  while(cdlist != dp)           /* Position to starting directory */
    (void) gotree(-1);

  if(ff < 0) {
     puthelp("%s %s", who, hitkey);
     return(errequest(gpattern, "Not found"));
  }
  else
    return(RV_OK);

} /* greptree() */

/*
 *      FIND A FILE IN TREE
 */

/* Find a file in file tree */
LOCAL int findtree(t)
  register int t;
{
  char input[PATLEN];
  register dlist *dp;
  register int f, c, ff, nt;

  who = t ? "FIND TREE" : "FIND DIRECTORY";
  if(t && (c = checktree(mustup)) != RV_OK)
    return(c);

  puthelp("%s: Give file pattern (CR:%s)", who, fpattern[0] ? fpattern : "quit");
  if((c = getpattern(input, "Search for which file:")) == RV_OK)
    (void) strcpy(fpattern, input);
  else if(c < RV_NUL || (c == RV_NUL && fpattern[0] == '\0'))
    return(c);

  /* Walk thru subtree */
  dp = cdlist;
  do {
    nt = DNTAG(dp);
    ff = -1;
    if(CFLAG != FL_FIL && (c = newflist(cdlist)) != RV_OK)      /* Update! */
      return(c);
    (void) putecho("Find \'%s\' in %s", fpattern, CPNAM);
    flushout();
    /* Walk thru file list */
    for(c = RV_NUL, f = 0; f < CNFIL; f++) {
      ff = -1;
      /* File found: what to do now? */
      if((c = findfile(cdlist, f)) == RV_OK) {
	ff = f;
	treeflag &= ~(SF_ECHO|SF_HELP);
	(void) updatetree(0);
	puthelp("%s (CR:next  SP:change dir  M:mark dir  T:tag file  ELSE:quit)", who);
	(void) putecho("Found: %s -> %s", FFNAM(cdlist, f), CFNAM);
	if((c = hitakey(NULL)) == 't') {
	  FITAG(cdlist, f) = FF_TAG;
	  ++CNTAG;
	  c = '\n';
	}
	else if(c == 'm') {
	  markdir(1);
	  c = '\n';
	}
	else if(c == ' ') {
	  if((c = filemenu(ff, RV_NUL)) == RV_END)
	    return(c);
	  (void) updatetree(0);
	  puthelp("%s (CR:continue  SP:select  ELSE:quit)", who);
	  (void) putecho("Continue searching file \'%s\':", fpattern);
	  if((c = hitakey(NULL)) == ' ')
	    return(RV_OK);
	}
	if(c != '\n')
	  break;
      }
    }
    if(nt != DNTAG(dp))
      showdlist(dp, 0);
  } while(t && (c == RV_NUL || c == '\n') && gotree(1) && CLEVL > DLEVL(dp));

  if(c == RV_END)
    return(c);
  while(cdlist != dp)           /* Position to starting directory */
    (void) gotree(-1);

  if(ff < 0) {
    puthelp("%s %s", who, hitkey);
    return(errequest(fpattern, "Not found"));
  }
  else
    return(RV_OK);

} /* findtree() */

/*
 *      WRITE TREE LIST
 */

/* Write tree list */
LOCAL int writetreelist()
{
  char list[INPLEN], name[NAMELEN], pat[PATLEN];
  register char *fn, *w;
  register int c, wc;

  who = "WRITE TREE";
  if((c = changelist(cdlist, who)) < RV_NUL)
    return(c);
  puthelp("%s: Give list filename (CR:quit)", who);
  c = putecho("Write list to:");
  if((c = getline(list, sizeof(list), c, 0, NULL, CLIST, 1)) != RV_OK)
    return(c);
  puthelp("%s: Give choice (D:dirs  F:files  L:list  M:matches  T:tags  ELSE:quit)", who);
  (void) putecho("Write out what:");
  switch(c = hitakey(NULL)) {
    default:  return(c);
    case 'D':
    case 'd':
      wc = 'd';
      w = "Directory";
      break;
    case 'F':
    case 'f':
      wc = 'f';
      w = "File";
      break;
    case 'L':
    case 'l':
      wc = 'l';
      w = "Tree";
      break;
    case 'T':
    case 't':
      wc = 't';
      w = "Tagged file";
      break;
    case 'M':
    case 'm':
      puthelp("WRITE MATCHING FILES: Give file pattern (CR:quit)");
      if((c = getpattern(pat, "Write which files:")) < RV_NUL)
	return(c);
      wc = 'm';
      w = pat;
      break;
  }

  /* Write out list file */
  puthelp("%s %s", who, hitkey);
  if(fn = writedlist(list, cdlist, w, wc)) {
    (void) strcpy(name, fn);
    checkdlist(name);
    if(buildflag) {
      if((c = updatedlist()) != RV_OK)
	return(c);
      treeflag |= SF_FILE;
    }
    if(wc == 'm')
      (void) putecho("Files matching \'%s\' written to \'%s\'", w, name);
    else
      (void) putecho("%s list written to \'%s\'", w, name);
    return(hitakey(NULL));
  }
  /* Error in writing */
  return(errequest(list, "Cannot write"));

} /* writetreelist() */

/*
 *      MOVE UP OR DOWN IN DIRECTORY TREE
 */

/* Go up or down in directory list */
GLOBL int gotree(dir)
  register int dir;
{
  register int i;

  /* At beginning or end of directory tree */
  if((dir < 0 && CPREV == GNULL) || (dir > 0 && CNEXT == GNULL))
    return(0);
  if(dir < 0) {                 /* Previous directory in tree */
    cdlist = (dlist *) CPREV;
    treeflag |= SF_LIST;
    /* Out of screen boundaries */
    if(DTROW(cdlist) <= firstdline && cdlist != droot) {
      tdlist = cdlist;
      for(i = ndlines / 2; i > 0 && DPREV(tdlist); i--)
	tdlist = (dlist *) DPREV(tdlist);
      treeflag |= SF_TREE;
    }
    else
      treeflag |= SF_LAST;
  }
  else {                        /* Next directory in tree */
    cdlist = (dlist *) CNEXT;
    treeflag |= SF_LIST;
    /* Out of screen boundaries */
    if(DTROW(cdlist) > lastdline || (DTROW(cdlist) == lastdline && CNEXT)) {
      tdlist = cdlist;
      for(i = ndlines / 2; i > 0 &&  DNEXT(tdlist); i--)
	tdlist = (dlist *) DNEXT(tdlist);
      for(i = ndlines; DPREV(tdlist) && i > 0; i--)
	tdlist = (dlist *) DPREV(tdlist);
      treeflag |= SF_TREE;
    }
    else
      treeflag |= SF_LAST;
  }

  treeflag |= SF_FILE;
  return(1);

} /* gotree() */

/* Go up or down on same level */
LOCAL int golevel(dir)
  register int dir;
{
  register dlist *dp;
  register int l;

  l = CLEVL;
  if(dir < 0 && cdlist != droot) {      /* Up */
    for(dp = (dlist *) CPREV; dp; dp = (dlist *) DPREV(dp))
      if(DLEVL(dp) <= l)
	break;
      do
	(void) gotree(dir);
      while(dp && cdlist != dp);
      if(dp && DLEVL(dp) == l)
	return(1);
  }
  else if(dir > 0 && CNEXT) {           /* Down */
    for(dp = (dlist *) CNEXT; dp; dp = (dlist *) DNEXT(dp))
      if(DLEVL(dp) <= l)
	break;
      do
	(void) gotree(dir);
      while(dp && cdlist != dp);
      if(dp && DLEVL(dp) == l)
	return(1);
  }

  return(0);                            /* Not possible */

} /* golevel() */

/* Go page up or down */
LOCAL int gopage(dir)
  register int dir;
{
  register int l;

  if(dir < 0 && CPREV) {                /* Page up */
    for(l = ndlines; l > 0 && gotree(dir); l--)
      ;
    return(1);
  }
  else if(dir > 0 && CNEXT) {           /* Page down */
    for(l = ndlines; l > 0 && gotree(1) ; l--)
      ;
    return(1);
  }

  return(0);                            /* Not possible */

} /* gopage() */

/* Go to beginning or end of tree */
LOCAL int gobegend(dir)
  register int dir;
{
  if(dir < 0 && CPREV) {                /* Beginning */
    mdlist = cdlist;
    while(gotree(dir))
      ;
    return(1);
  }
  else if(dir > 0 && CNEXT) {           /* End */
    mdlist = cdlist;
    while(gotree(dir))
      ;
    return(1);
  }

  return(0);                            /* Not possible */

} /* gobegend() */

/*
 *      REFRESH ON SCREEN RESIZING
 */

#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
/* Refresh tree screen after screen size changes */
GLOBL int refreshtree(f)
  register int f;
{
  register dlist *dp;

  if(f)
    (void) refreshfile(0);
  checkindent();
  ndlines = lastdline - firstdline;
  dp = cdlist;
  cdlist = tdlist = droot;
  while(cdlist != dp && gotree(1))
    ;
  treeflag = SF_FULL;
  return(RV_OK);

} /* refreshtree() */
#endif  /* SIGWINCH && TIOCGWINSZ */

/*
 *      SELECT DIRECTORY
 */

/* Walk thru tree and select a directory */
GLOBL char *selectdir(what)
  register char *what;
{
  register dlist *cd, *td;
  register char *dn;
  register int c, f;

  /* Save current and top dirs */
  cd = cdlist;
  td = tdlist;
  dn = what;
  treeflag = SF_FULL;

  /* Select tree loop */
  do {
    /* Special update for tree screen if needed */
    if(treeflag) {
      f = 0;
      if(treeflag & SF_HELP)
	f |= SF_HELP;
      if(treeflag & SF_ECHO)
	f |= SF_ECHO;
      if((c = updatetree(1)) != RV_OK) {
	dn = NULL;
	break;
      }
      if(f & SF_HELP)
	puthelp("SELECT DIRECTORY (CR:select  Q:quit)");
      if(f & SF_ECHO)
	(void) putecho("Move to and select directory for %s", what);
      if(f)
	(void) cursorxy(TCOL+DTCOL(cdlist)-1, DTROW(cdlist));
      f = 0;
    }
    switch(c = getkey()) {
      default:                  /* Ignore */
	bell(VARSET(V_BL));
	break;
      case '>':                 /* Select current directory */
      case ' ':
      case K_SEL:
      case K_INS:
	dn = CPNAM;
	break;
      case '<':                 /* Parent */
      case K_DEL:
	if( !goparent())
	  bell(VARSET(V_BL));
	break;
      case 'q':                 /* Return */
      case 'Q':
      case K_BRK:
      case K_EOF:               /* EOF */
	dn = NULL;
	break;
      case K_PREV:              /* Previous */
      case 'k':                 /* For vi fans */
	if( !gotree(-1))
	  bell(VARSET(V_BL));
	break;
      case K_NEXT:              /* Next */
      case 'j':                 /* For vi fans */
	if( !gotree(1))
	  bell(VARSET(V_BL));
	break;
      case K_BACK:              /* Up on same level */
	if( !golevel(-1))
	  bell(VARSET(V_BL));
	break;
      case K_FORW:              /* Down on same level */
	if( !golevel(1))
	  bell(VARSET(V_BL));
	break;
      case K_PPAG:              /* Page up */
	if( !gopage(-1))
	  bell(VARSET(V_BL));
	break;
      case K_NPAG:              /* Page down */
	if( !gopage(1))
	  bell(VARSET(V_BL));
	break;
      case K_HOME:              /* Beginning */
	if( !gobegend(-1))
	  bell(VARSET(V_BL));
	break;
      case K_END:               /* End */
	if( !gobegend(1))
	  bell(VARSET(V_BL));
	break;
      case 'c':                 /* Change to directory */
	c = changedir();
	break;
      case '@':                 /* Mark current directory */
      case K_MARK:
	markdir(0);
	break;
      case '#':                 /* Goto previously marked directory */
      case K_GOTO:
	if( !gomarkdir())
	  bell(VARSET(V_BL));
	break;
      case K_SIZE:              /* Screen size changed */
	c = RV_SIZ;
	/*FALL THROUGH*/
      case K_REFR:              /* Refresh */
	treeflag = SF_FULL;
	break;
    }
#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
    /* Refresh screen after screen resize */
    if(c == RV_SIZ)
      (void) refreshtree(1);
#endif  /* SIGWINCH && TIOCGWINSZ */
  } while(dn == what);

  /* Restore current and top dirs */
  cdlist = cd;
  tdlist = td;

  return(dn);

} /* selectdir() */

/*
 *      DIRECTORY TREE MENU LOOP
 */

/* Tree menu */
GLOBL int treemenu(update)
  register int update;
{
  register int c;

  /* Init tree variables */
  checkindent();
  cdlist   = tdlist = droot;
  menuline = tmline;
  treeflag = SF_FULL;

  /* Scan and update file lists in tree if reading from a list file */
  if(update && (c = scantree(0)) != RV_OK)
    return(c);

  /* Tree menu loop */
  do {
    /* Update tree screen if needed and clock */
    buildflag = 0;
    if(treeflag && (c = updatetree(0)) != RV_OK)
      return(c);
#ifdef  UTCLOCK
    if(VARSET(V_CL))
      clockon();
#endif  /* UTCLOCK */
    c = getkey();
#ifdef  UTCLOCK
    if(VARSET(V_CL))
      clockoff();
#endif  /* UTCLOCK */
    switch(c) {
      case K_BRK:               /* Ignore interrupt */
      default:                  /* Unknown: ring the bell */
	bell(VARSET(V_BL));
	break;
      case K_PREV:              /* Previous */
      case 'k':                 /* For vi fans */
	if( !gotree(-1))
	  bell(VARSET(V_BL));
	break;
      case K_NEXT:              /* Next */
      case 'j':                 /* For vi fans */
	if( !gotree(1))
	  bell(VARSET(V_BL));
	break;
      case K_BACK:              /* Up on same level */
	if( !golevel(-1))
	  bell(VARSET(V_BL));
	break;
      case K_FORW:              /* Down on same level */
	if( !golevel(1))
	  bell(VARSET(V_BL));
	break;
      case K_TAG:               /* Next directory containing tagged files */
	if( !gotagged())
	  bell(VARSET(V_BL));
	break;
      case K_UP:                /* Scroll up */
	if( !scrolltree(1))
	  bell(VARSET(V_BL));
	break;
      case K_DOWN:              /* Scroll down */
	if( !scrolltree(-1))
	  bell(VARSET(V_BL));
	break;
      case K_PPAG:              /* Page up */
	if( !gopage(-1))
	  bell(VARSET(V_BL));
	break;
      case K_NPAG:              /* Page down */
	if( !gopage(1))
	  bell(VARSET(V_BL));
	break;
      case K_HOME:              /* Begin */
	if( !gobegend(-1))
	  bell(VARSET(V_BL));
	break;
      case K_END:               /* End */
	if( !gobegend(1))
	  bell(VARSET(V_BL));
	break;
      case '@':                 /* Mark current directory */
      case K_MARK:
	markdir(0);
	break;
      case '#':                 /* Goto previously marked directory */
      case K_GOTO:
	if( !gomarkdir())
	  bell(VARSET(V_BL));
	break;
      case K_SIZE:              /* Screen size changed */
	c = RV_SIZ;
	/*FALL THROUGH*/
      case K_REFR:              /* Refresh */
	treeflag = SF_FULL;
	break;
      case '?':                 /* Help */
      case 'h':
      case 'H':
      case K_HELP:
	c = showhelp('t');
	break;
      case '<':                 /* Change to parent */
      case K_DEL:
	if( !goparent()) {
	  bell(VARSET(V_BL));
	  break;
	}
	/*FALL THROUGH*/
      case ' ':                 /* Change to directory */
      case '>':
      case K_SEL:
      case K_INS:
	do
	  c = filemenu(-1, c);
	while(c == RV_DIR);
	break;
      case 'b':                 /* Backup tree */
	c = backupdir(ONTG(c));
	break;
      case 'l':                 /* List file in tree */
	c = listtree(ONTG(c));
	break;
      case 'm':                 /* Create a directory */
	c = makedir();
	break;
      case 'r':                 /* Remove a directory */
	c = removedir();
	break;
      case 'c':                 /* Change to directory */
	c = changedir();
	break;
      case 'L':                 /* List tagged files */
	c = listtree(ONTG(c));
	break;
      case 'B':                 /* Backup tagged files */
	c = backupdir(ONTG(c));
	break;
      case 'C':                 /* Copy tagged files */
	c = copytagged();
	break;
      case 'R':                 /* Remove tagged files */
	c = removetagged();
	break;
      case 'M':                 /* Move tagged files */
	c = movetagged();
	break;
      case 's':                 /* Display directory status info */
      case 'S':
	c = statusdir();
	break;
      case 'i':                 /* Display directory information */
      case 'I':
	c = infodir();
	break;
      case 'g':                 /* Search for string */
      case 'G':
	c = greptree(ONTR(c));
	break;
      case 'f':                 /* Find a file */
      case 'F':
	c = findtree(ONTR(c));
	break;
      case 't':                 /* Tag files */
      case 'T':
	c = tagtree(ONTR(c));
	break;
      case 'u':                 /* Untag files */
      case 'U':
	c = untagtree(ONTR(c));
	break;
      case 'n':                 /* New sort file list */
      case 'N':
	c = sorttree(ONTR(c));
	break;
      case 'z':                 /* Zoom file list */
      case 'Z':
	c = zoomtree(ONTR(c));
	break;
      case '0':                 /* Switch menu line */
	menuline = menuline == utreemenu ? tmline : utreemenu;
	treeflag |= SF_HELP;
	break;
      case '1':                 /* User defined tree command 1..9 */
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
	if(changelist(cdlist, "USER COMMAND") < RV_NUL)
	  break;
	c = usercommand(c - '0' + V_TC0);
	break;
      case '!':                 /* Escape to shell */
      case '$':
	if(changelist(cdlist, "SHELL ESCAPE") < RV_NUL)
	  break;
	c = history(c, V_TC1);
	if(VARSET(V_ST))
	  (void) scandlist(droot);
	if(buildflag)
	  c = updatedlist();
	break;
      case '=':                 /* Show/set variables */
	c = variables();
	break;
      case ':':                 /* Show/set file type commands */
	c = commands();
	break;
      case '|':                 /* Show key bindings */
	c = bindings();
	break;
      case '+':                 /* Enlarge tree window */
	if( !resizetree(1))
	  bell(VARSET(V_BL));
	break;
      case '-':                 /* Shrink tree window */
	if( !resizetree(-1))
	  bell(VARSET(V_BL));
	break;
      case 'o':                 /* Write out tree list */
      case 'O':
	c = writetreelist();
	break;
      case '/':                 /* Scan directory tree and update file lists */
	c = scantree(1);
	break;
      case '\\':                /* Build subdirectory tree */
	c = buildtree();
	break;
      case 'a':                 /* Display version string */
      case 'A':
	c = putversion(echoline, "ABOUT: Utree version");
	break;
      case 'd':                 /* Date */
      case 'D':
	c = printdate();
	break;
      case 'w':                 /* Print current working directory */
      case 'W':
	c = printcwd();
	break;
      case 'q':                 /* Exit utree */
      case 'Q':
      case K_EOF:
	c = RV_END;
	break;
    }
#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
    /* Refresh screen after screen resize */
    if(c == RV_SIZ)
      c = refreshtree(1);
#endif  /* SIGWINCH && TIOCGWINSZ */
  } while( !(c == RV_END || c == RV_ERR));

  return(c);

} /* treemenu() */
