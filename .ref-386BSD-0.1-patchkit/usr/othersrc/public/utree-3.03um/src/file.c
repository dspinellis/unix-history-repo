/*
 *      FILE.C
 *      UTREE file menu routines.
 *      3.01-um klin, Tue Jun  4 14:20:31 1991
 *              klin, Tue Oct 15 14:02:37 1991, Handling of symlinks changed
 *              klin, Sat Oct 26 15:07:06 1991, Copying and moving changed
 *                                              Sorting and zooming changed
 *                                              Select directories added
 *      3.02-um klin, Fri Nov  1 10:46:14 1991, Screen layout changed
 *                                              Marking files changed
 *                                              Bug in edit() deleted
 *              klin, Sun Nov 24 19:30:43 1991, Cd to current directory before
 *                                              executing some commands
 *                                              Video attributes changed
 *      3.03-um klin, Tue Feb 11 19:39:09 1992, Screen layout changed,
 *                                              Variables and filetype commands
 *                                              changed
 *              klin, Sun Feb 23 17:32:31 1992, Key handling and key bindings
 *                                              changed
 *              klin, Fri Mar  6 08:18:38 1992, Minor changes in execute()
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03-um (klin) Mar  6 1992 file.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

LOCAL int flast;                /* Last current file                    */
LOCAL int fmark;                /* Marked file                          */
LOCAL int nscroll;              /* Lines to scroll                      */

/* Default file menu commands in help line                              */
LOCAL char *fmline =
" Help Copy Edit Find Grep Move List Print Remove Stat Tag Untag View Quit";
LOCAL char *menuline = NULL;

#define CUR(c)  ((c) >= 'a')    /* Command works on current file only   */

/* ---- External variables and functions ------------------------------ */

EXTRN char *selectdir();

/* ---- Local/global functions and procedures ------------------------- */

/*
 *      FILE SCREEN UPDATE AND REFRESH
 */

/* Show file list from file f to file t */
LOCAL VOID showfiles(f, t, c)
  register int f, t, c;
{
  if(c) {                       /* Clear file window */
    (void) cursorxy(0, firstline);
    clearwindow(firstline, lastline);
  }
  while(f < CNFIL && f < t && FFROW(cdlist, f) <= lastline)
    putfile(cdlist, f++, FF_TAG);

} /* showfiles() */

/* Update file screen */
LOCAL VOID updatefiles()
{
  if(fileflag == SF_FULL)                       /* Full update */
    clearscreen();
  if(fileflag & SF_ECHO)                        /* Echo line */
    if(CZOOM)
      (void) putecho("%s/%s: %d file(s)", CPNAM, CZOOM, CNFIL);
    else
      (void) putecho("%s/*: %d file(s) %d dir(s)", CPNAM, CNFIL, CNDIR);
  if(fileflag & SF_HELP)                        /* Help line */
    putmenu("FILE:", menuline);
  if(fileflag & SF_TREE) {                      /* File window */
    if(CANSCROLL && nscroll < 0 && nscroll > (firstline-lastline)) {
      (void) windowup(firstline, lastline, -nscroll);
      showfiles(CFTOP+fperpage-fperline+(nscroll*fperline), CFTOP+fperpage, 0);
      if(flast >= 0)
	putfile(cdlist, flast, FF_TAG);
    }
    else if(CANSCROLL && nscroll > 0 && nscroll < (lastline-firstline)) {
      (void) windowdown(firstline, lastline, nscroll);
      showfiles(CFTOP, CFTOP + (nscroll * fperline) + fperline, 0);
      if(flast >= 0)
	putfile(cdlist, flast, FF_TAG);
    }
    else
      showfiles(CFTOP, CNFIL, fileflag != SF_FULL);
    nscroll = 0;
    fileflag |= SF_LIST;
  }
  else if(fileflag & SF_LAST && flast >= 0)     /* Last file */
    putfile(cdlist, flast, FF_TAG);
  if((fileflag & SF_LIST) && CNFIL > 0) {       /* Current file */
    putfile(cdlist, CFCUR, FF_TAG);
    flast = CFCUR;
  }
  if(CNFIL > 0)                                 /* Position to current file */
    (void) cursorxy(FFCOL(cdlist, CFCUR), FFROW(cdlist, CFCUR));
  else
    (void) cursorxy(0, firstline);
  fileflag = 0;                                 /* Reset fileflag */

} /* updatefiles() */

/*
 *      MARK/TAG FILE
 */

/* Set mark w on file f */
LOCAL VOID setmark(f, w)
  register int f, w;
{
  if(f >= CFTOP && f < (CFTOP + fperpage)) {
    putfile(cdlist, f, w);
    (void) cursorxy(FFCOL(cdlist, f), FFROW(cdlist, f));
    flushout();
  }

} /* setmark() */

/* Reset mark on file f */
LOCAL VOID unsetmark(f)
  register int f;
{
  if(f >= CFTOP && f < (CFTOP + fperpage)) {
    putfile(cdlist, f, FF_TAG);
    (void) cursorxy(FFCOL(cdlist, f), FFROW(cdlist, f));
    flushout();
  }

} /* unsetmark() */

/* Tag single file f */
LOCAL VOID tagfile(f)
  register int f;
{
  FITAG(cdlist, f) = FF_TAG;
  ++CNTAG;
  setmark(f, FF_TAG);

} /* tagfile() */

/* Untag single file f */
LOCAL VOID untagfile(f)
  register int f;
{
  FITAG(cdlist, f) = FF_NONE;
  if(CNTAG > 0)
    --CNTAG;

} /* untagfile() */

/*
 *      MOVE IN FILELIST
 */

/* Go forward or backward in file list */
GLOBL int gofile(dp, dir)
  register dlist *dp;
  register int dir;
{
  register int l;

  if(dir > 0 && (DFCUR(dp) + 1) < DNFIL(dp)) {  /* Next file */
    ++DFCUR(dp);
    fileflag |= SF_LIST;
    /* Out of bounds: Search for new top file */
    if(DFCUR(dp) >= (DFTOP(dp)+fperpage) && (DFTOP(dp)+fperpage) < DNFIL(dp)) {
      l = (lastline - firstline) / 2;
      while(l-- > 0 && (DFTOP(dp) + fperpage) < DNFIL(dp)) {
	DFTOP(dp) += fperline;
	--nscroll;
      }
      fileflag |= SF_TREE;
    }
    else
      fileflag |= SF_LAST;
  }
  else if(dir < 0 && DFCUR(dp) > 0) {           /* Previous file */
    --DFCUR(dp);
    fileflag |= SF_LIST;
    /* Out of bounds: Search for new top file */
    if(DFCUR(dp) <  DFTOP(dp) && DFTOP(dp) > 0) {
      l = (lastline - firstline) / 2;
      while(l-- > 0 && DFTOP(dp) > 0) {
	DFTOP(dp) -= fperline;
	++nscroll;
      }
      fileflag |= SF_TREE;
    }
    else
      fileflag |= SF_LAST;
  }
  else
    return(0);
  return(1);

} /* gofile() */

/*
 *      SCROLL FILE LIST
 */

/* Scroll up or down file list */
LOCAL int scrollfile(dir)
  register int dir;
{
  if(dir < 0 && (CFTOP + fperpage) < CNFIL) {   /* Scroll up */
    CFTOP += fperline;
    if(CANSCROLL) {
      (void) windowup(firstline, lastline, 1);
      showfiles(CFTOP + fperpage - fperline, CFTOP + fperpage, 0);
      fileflag |= SF_MOVE;
    }
    else
      fileflag |= SF_TREE;
    if(CFCUR < CFTOP) {
      CFCUR += fperline;
      fileflag |= SF_LIST;
    }
  }
  else if(dir > 0 && CFTOP > 0) {               /* Scroll down */
    CFTOP -= fperline;
    if(CANSCROLL) {
      (void) windowdown(firstline, lastline, 1);
      showfiles(CFTOP, CFTOP + fperline, 0);
      fileflag |= SF_MOVE;
    }
    else
      fileflag |= SF_TREE;
    if(CFCUR >= (CFTOP + fperpage)) {
      CFCUR -= fperline;
      fileflag |= SF_LIST;
    }
  }
  else
    return(0);
  return(1);

} /* scrollfile() */

/*
 *      SELECT DIRECTORY
 */
LOCAL char *doselect(what)
  register char *what;
{
  register char *dn;

  dn = selectdir(what);
  fileflag = SF_FULL;
  fileflag &= ~(SF_HELP|SF_ECHO);
  updatefiles();
  return(dn);

} /* doselect() */

/*
 *      LIST FILE(S)
 */

/* List file(s) in current file list */
LOCAL int list(t)
  register int t;
{
  char pat[PATLEN];
  struct stat st;
  register char *ct;
  register int c, f, ff, l;

  if(CNFIL == 0)                /* Nothing to list */
    return(RV_OK);

  who = "LIST FILE";
  if(t || CNTAG <= 0) {
    puthelp("%s: Give file pattern (CR:all files)", who);
    if((c = getpattern(pat, "List which files:")) < RV_NUL)
      return(c);
    else if(c == RV_NUL)
      (void) strcpy(pat, "*");
    t = 1;
  }

  /* Show all matching files */
  l = firstline;
  ff = 0;
  c = RV_OK;
  for(f = 0; f < CNFIL; f++) {
    if(( !t && ISTAG(cdlist, f)) || umatch(cdlist, f, pat) > 0) {
      if((*statfun)(FFNAM(cdlist, f), &st) != 0)
	continue;
      if(l == firstline) {
	if(ff > 0) {
	  puthelp("%s %s (CR:continue  ELSE:quit)", who, pat);
	  c = hitakey("Continue listing ?", echoline, DA_NONE);
	  if( !(c == '\n' || c == ' '))
	    break;
	}
	fileflag = SF_FULL;
	clearwindow(firstline, lastline);
      }
      ++ff;
      ct = ctime(&st.st_mtime);
      ct[strlen(ct) - 1] = '\0';
      (void) putfxy(0, l, 0, "%s  %8ld  %s  %s", fileaccess(&st), st.st_size, ct,
		    FFNAM(cdlist, f));
      if(++l > lastline)
	l = firstline;
    }
  }

  if(c >= RV_NUL) {
    puthelp("%s %s", who, hitkey);
    if(t)
      (void) putecho("Listed %d file(s) matching %s", ff, pat);
    else
      (void) putecho("Listed %d tagged file(s)", ff);
    c = hitakey(NULL);
  }
  return(c);

} /* list() */

/*
 *      SEARCH IN FILE(S)
 */

/* Search for pattern in file(s) */
LOCAL int grep(one)
  register int one;
{
  char input[PATLEN];
  register int f, c;

  if(CNFIL == 0)                /* Nothing to search */
    return(RV_OK);

  who = "GREP FILE";
  puthelp("%s: Give pattern (CR:%s)", who, gpattern[0] ? gpattern : "quit");
  c = putecho("Search for pattern in file:");
  if((c = getline(input, sizeof(input), c, 0, NULL, GNULL, 0)) == RV_OK)
    (void) strcpy(gpattern, input);
  else if(c < RV_NUL || (c == RV_NUL && gpattern[0] == '\0'))
    return(c);

  if( !one && CNTAG) {          /* Tagged files */
    c = RV_NUL; f = -1;
    do {
      ++f;
      if(ISTAG(cdlist, f)) {    /* File tagged */
	untagfile(f);
	setmark(f, FF_MARK);
	(void) putecho("Searching in %s", FFNAM(cdlist, f));
	flushout();
	if((c = grepfile(cdlist, f)) == RV_OK) {
	  puthelp("%s (CR:next  SP:select  M:mark  T:tag  ELSE:quit)", who);
	  (void) putecho("Found \'%s\' -> %s:", gpattern, FFNAM(cdlist, f));
	  c = hitakey(NULL);
	  unsetmark(f);
	  if(c == 't') {
	    tagfile(f);
	    c = '\n';
	  }
	  else if(c == 'm') {
	    fmark = f;
	    c = '\n';
	  }
	  else if(c == ' ')
	    while(f != CFCUR)
	      (void) gofile(cdlist, f > CFCUR ? 1 : -1);
	}
	else
	  unsetmark(f);
      }
    } while(f < CNFIL && (c == RV_NUL || c == '\n'));
  }
  else if((c = grepfile(cdlist, CFCUR)) == RV_OK) {     /* Current file */
    puthelp("%s %s", who, hitkey);
    (void) putecho("Found \'%s\'", gpattern);
    c = hitakey(NULL);
  }

  if(c)
    return(c);
  puthelp("%s: %s %s", who, gpattern, hitkey);
  return(errequest(gpattern, "Not found"));

} /* grep() */

/*
 *      FIND FILE(S)
 */

/* Find file(s) in current file list */
LOCAL int find(one)
  register int one;
{
  char input[PATLEN];
  register int f, c;

  if(CNFIL == 0)                /* Nothing to find */
    return(RV_OK);

  who = "FIND FILE";
  puthelp("%s: Give file pattern (CR:%s)", who, fpattern[0] ? fpattern : "quit");
  if((c = getpattern(input, "Find which file:")) == RV_OK)
    (void) strcpy(fpattern, input);
  else if(c < RV_NUL || (c == RV_NUL && fpattern[0] == '\0'))
    return(c);

  f = one ? CFCUR : -1;
  do {
    ++f;
    if((c = findfile(cdlist, f)) == RV_OK) {
      setmark(f, FF_MARK);
      puthelp("%s (CR:next  SP:select  M:mark  T:tag  ELSE:quit)", who);
      (void) putecho("Found %s:", FFNAM(cdlist, f));
      c = hitakey(NULL);
      unsetmark(f);
      if(c == 't') {
	tagfile(f);
	c = '\n';
      }
      else if(c == 'm') {
	fmark = f;
	c = '\n';
      }
      else if(c == ' ')
	while(f != CFCUR)
	  (void) gofile(cdlist, f > CFCUR ? 1 : -1);
    }
  } while(f < CNFIL && (c == RV_NUL || c == '\n'));

  if(c)
    return(c);

  puthelp("%s: %s %s", who, fpattern, hitkey);
  return(errequest(fpattern, "Not found"));

} /* find() */

/*
 *      FILE STATUS AND CHANGES
 */

/* Status information(s)/change(s) for current or tagged file(s) */
LOCAL int status(one, s)
  register int one, s;
{
  register int f, c;

  if(CNFIL == 0)                /* No need for status of nothing */
    return(RV_OK);

  if( !one && CNTAG) {          /* Tagged files */
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	if(s)
	  c = statusfile(FFNAM(cdlist, f), 1);
	else {
	  setmark(f, FF_MARK);
	  c = infofile(f, CNTAG > 0);
	  unsetmark(f);
	}
	if( !(c == RV_NUL || c == '\n' || c == ' '))
	  break;
      }
  }
  else {
    if(s)
      c = statusfile(FFNAM(cdlist, CFCUR), 1);    /* Current files */
    else
      c = infofile(CFCUR, 0);
  }

  if(buildflag)                 /* Rebuilding needed */
    CFLAG = FL_CHG;
  return(c);

} /* status() */

/* Display size and modification time of file f */
LOCAL int infofile(f, h)
  register int f, h;
{
  struct stat st;

  who = "INFO FILE";
  if((*statfun)(FFNAM(cdlist, f), &st)) {
    puthelp("%s %s", who, hitkey);
    return(errequest(FFNAM(cdlist, f), "Cannot stat"));
  }
  if(h)
    puthelp("%s %s (CR:continue  ELSE:quit)", who, FFNAM(cdlist, f));
  else
    puthelp("%s %s %s", who, FFNAM(cdlist, f), hitkey);
  (void) putecho("Access:%s Size:%ld Date:%s",
		  fileaccess(&st), st.st_size, ctime(&st.st_mtime));
  return(hitakey(NULL));

} /* infofile() */

/*
 *      MOVE OR RENAME FILE(S)
 */

/* Move current or tagged file(s) */
LOCAL int move(one)
  register int one;
{
  struct stat st;
  char name[NAMELEN];
  register char *to;
  register int f, c, req;

  if(CNFIL == 0)                /* Nothing to move */
    return(RV_OK);

  who = "MOVE FILE";
  if( !one && CNTAG) {          /* Tagged files */
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
    else if((to = doselect("moving files")) == NULL) {
      fileflag |= SF_ECHO|SF_HELP;
      return(RV_NUL);
    }
    puthelp("%s (N:don't request  Q:quit  ELSE:request)", who);
    (void) putecho("Request before moving to %s ?", to);
    c = hitakey(NULL);
    if(c == 'q' || c < RV_NUL)
      return(c);
    if(req = (c != 'n'))
      puthelp("%s (Y:move  Q:quit  ELSE:don't move)", who);
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	setmark(f, FF_MARK);
	c = movefile(cdlist, f, to, req);
	unsetmark(f);
	if(c == 'q' || c < RV_NUL)
	  break;
      }
  }
  else {                        /* Current file */
    puthelp("%s: Give destination (CR:select directory)", who);
    c = putecho("Move file to:");
    if((c = getline(name, sizeof(name), c, 0, NULL, CLIST, 0)) < RV_NUL)
      return(c);
    else if(c == RV_OK)
      to = strcpy(name, pathname(name, CPNAM));
    else if((to = doselect("moving file")) == NULL) {
      fileflag |= SF_ECHO|SF_HELP;
      return(RV_NUL);
    }
    c = movefile(cdlist, CFCUR, to, 0);
  }

  checkdlist(to);               /* Directory needs checking */
  ++buildflag;                  /* Set rebuild flags */
  CFLAG = FL_CHG;
  fileflag |= SF_ECHO|SF_HELP;
  return(c);

} /* move() */

/*
 *      VIEW FILE(S)
 */

/* View current or tagged file(s) */
LOCAL int view(one)
  register int one;
{
  register int f, c;

  if(CNFIL == 0)                /* Nothing to view */
    return(RV_OK);

  if( !one && CNTAG) {          /* Tagged files */
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	if((c = viewfile(f, 0)) == 'n' || c < RV_NUL)
	  break;
      }
  }
  else                          /* Current file */
    c = viewfile(CFCUR, 1);

  return(c);

} /* view() */

/* View single file f */
LOCAL int viewfile(f, one)
  register int f, one;
{
  char name[NAMELEN], buf[EXECLEN];
  register FILE *file;
  register int c, n;

  /* Check if viewing is allowed */
  if( !isallowed(FFNAM(cdlist, f), (int) FMODE(cdlist, f)))
    return(hitakey(NULL));

  who = "VIEW FILE";
  /* Simple check for text or binary file. May fail! */
  (void) strcpy(name, pathname(FFNAM(cdlist, f), CPNAM));
  if(file = fopen(name, "r")) {
    n = fread(buf, sizeof(char), 4, file);
    (void) fclose(file);
  }
  else
    return(errequest(FFNAM(cdlist, f), "Cannot open"));

  if(n <= 0)                    /* File is empty */
    return(errequest(FFNAM(cdlist, f), "Is empty"));
  else if(istextfile(buf, n)) { /* Text file */
    (void) sprintf(buf, "%s %s %s", VARVAL(V_PG), VARVAL(V_PGO), name);
    (void) callsystem(buf, 1, 1);
  }
  else {                        /* Binary file */
    puthelp("%s (Y:hex dump  ELSE:don't view)", who);
    (void) putecho("File %s is not a text file, hex dump ?", FFNAM(cdlist, f));
    if((c = hitakey(NULL)) != 'y')
      return(c);
    (void) sprintf(buf, "%s %s %s|%s %s", VARVAL(V_XD), VARVAL(V_XDO), name,
					  VARVAL(V_PG), VARVAL(V_PGO));
    (void) callsystem(buf, 1, 1);
  }

  if(one || CNTAG < 1)
    return(hitakey("Viewing done (Hit a key)", lines-1, DA_REVERSE));
  else
    return(hitakey("Continue (N:no  ELSE:yes) ?", lines-1, DA_REVERSE));

} /* viewfile() */

/* Simple check if a file is a text file */
LOCAL int istextfile(s, n)
  register char *s;
  register int n;
{
  while(--n > 0) {                      /* Simple check if all n chars */
    if( !(isprint(*s) || isspace(*s)))  /* are printable or whitespace */
      return(0);
    ++s;
  }
  return(1);

} /* istextfile() */

/*
 *      PRINT FILE(S)
 */

/* Print current or tagged file(s) */
LOCAL int print(one)
  register int one;
{
  register int f, c;

  if(CNFIL == 0)                /* Nothing to print */
    return(RV_OK);

  who = "PRINT FILE";
  if( !one && CNTAG) {          /* Tagged files */
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	setmark(f, FF_MARK);
	puthelp("%s (Y:print  Q:quit  ELSE:don't print)", who);
	c = printfile(f, 1);
	unsetmark(f);
	if(c == 'q' || c < RV_NUL)
	  break;
      }
  }
  else                          /* Current file */
    c = printfile(CFCUR, 0);

  if(c < RV_NUL || one || !CNTAG)
    return(c);
  puthelp("%s %s", who, hitkey);
  return(hitakey("Printing done", echoline, DA_NONE));

} /* print() */

/* Print single file f with request if req is set */
LOCAL int printfile(f, req)
  register int f, req;
{
  char name[NAMELEN], buf[EXECLEN];
  register int c;

  (void) strcpy(name, pathname(FFNAM(cdlist, f), CPNAM));
  if(req) {
    (void) putecho("Print %s ?", name);
    if((c = hitakey(NULL)) != 'y')
      return(c);
  }
  (void) putecho("Printing %s", name);
  (void) sprintf(buf, "%s %s %s", VARVAL(V_LP), VARVAL(V_LPO), name);
  if((c = callsystem(buf, 0, 0)) != RV_OK)
    return(errequest(FFNAM(cdlist, f), "Error in printing"));
  return(RV_OK);

} /* printfile() */

/*
 *      REMOVE FILE(S)
 */

/* Remove current or tagged file(s) */
LOCAL int Remove(one)
  register int one;
{
  register int f, c, req;

  if(CNFIL == 0)                /* Nothing to remove */
    return(RV_OK);

  who = "REMOVE FILE";
  if( !one && CNTAG) {          /* Tagged files */
    if(CNTAG > 1) {
      puthelp("%s (N:don't request  Q:quit  ELSE:request)", who);
      c = hitakey("Request before removing ?", echoline, DA_NONE);
      if(c == 'q' || c < RV_NUL)
	return(c);
      else if(req = (c != 'n'))
	puthelp("%s (Y:remove  Q:quit  ELSE:don't remove)", who);
    }
    else
      req = 1;
    for(f = CNFIL - 1; f >= 0; f--) {
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	setmark(f, FF_MARK);
	if((c = removefile(cdlist, f, req)) != RV_OK)
	  unsetmark(f);
	if(c == 'q' || c < RV_NUL)
	  break;
      }
    }
  }
  else {                        /* Current file */
    puthelp("%s (Y:remove  ELSE:don't remove)", who);
    c = removefile(cdlist, CFCUR, 1);
  }

  return(c);

} /* remove() */

/*
 *      EDIT FILE(S)
 */

/* Edit current or tagged file(s) */
LOCAL int edit(one)
  register int one;
{
  char name[FILELEN];
  register char *fname;
  register int f, c, mode, req;

  who = "EDIT FILE";
  if( !one && CNTAG) {          /* Tagged files */
    if(CNTAG > 1) {
      puthelp("%s (N:don't request  Q:quit  ELSE:request)", who);
      c = hitakey("Request before edit ?", echoline, DA_NONE);
      if(c == 'q' || c < RV_NUL)
	return(c);
      else if(req = (c != 'n'))
	puthelp("%s (Y:edit  Q:quit  ELSE:don't edit)", who);
    }
    else
      req = 1;
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	c = editfile(FFNAM(cdlist, f), (int) FMODE(cdlist, f), req);
	if(c == 'q' || c < RV_NUL)
	  break;
      }
  }
  else {                        /* Current or no files */
    if(CNFIL > 0) {
      setmark(CFCUR, FF_MARK);
      puthelp("%s: Give file name (CR:%s)", who, FFNAM(cdlist, CFCUR));
      c = putecho("Edit file:");
      c = getline(name, sizeof(name), c, 0, NULL, GNULL, 0);
      unsetmark(CFCUR);
      if(c < RV_NUL)
	return(c);
    }
    else {                      /* Directory is empty */
      puthelp("%s: Give file name (CR:quit)", who);
      c = putecho("Edit which file:");
      if((c = getline(name, sizeof(name), c, 0, NULL, GNULL, 0)) != RV_OK)
	return(c);
    }
    fname = c == RV_NUL ? FFNAM(cdlist, CFCUR) : name;
    mode  = c == RV_NUL ? FMODE(cdlist, CFCUR) : FF_NONE;
    c = editfile(fname, mode, 0);
  }

  return(c);

} /* edit() */

/* Edit single file name (mode mode) with request if req is set */
LOCAL int editfile(name, mode, req)
  register char *name;
  register int mode, req;
{
  char pname[NAMELEN], buf[EXECLEN];
  register int c;

  /* Check if editing is allowed */
  if( !isallowed(name, mode))
    return(hitakey(NULL));

  (void) strcpy(pname, pathname(name, CPNAM));
  if(req) {
    (void) putecho("Edit %s ?", name);
    c = hitakey(NULL);
    if(c != 'y')
      return(c);
  }
  (void) sprintf(buf, "%s %s %s", VARVAL(V_ED), VARVAL(V_EDO), pname);
  (void) callsystem(buf, 1, 1);

  checkdlist(CPNAM);            /* Directory needs checking */
  return(RV_OK);

} /* editfile() */

/*
 *      COPY FILE(S)
 */

/* Copy current or tagged file(s) */
LOCAL int copy(one)
  register int one;
{
  struct stat st;
  char name[NAMELEN];
  register char *to;
  register int f, c, req;

  if(CNFIL == 0)                /* Nothing to copy */
    return(RV_OK);

  who = "COPY FILE";
  if( !one && CNTAG) {          /* Tagged files */
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
    else if((to = doselect("copying files")) == NULL) {
      fileflag |= SF_ECHO|SF_HELP;
      return(RV_NUL);
    }
    puthelp("%s (N:don't request  Q:quit  ELSE:request)", who);
    (void) putecho("Request before copying to %s ?", to);
    c = hitakey(NULL);
    if(c == 'q' || c < RV_NUL)
      return(c);
    if(req = (c != 'n'))
      puthelp("%s (Y:copy  Q:quit  ELSE:don't copy)", who);
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	setmark(f, FF_MARK);
	c = copyfile(cdlist, f, to, req);
	unsetmark(f);
	if(c == 'q' || c < RV_NUL)
	  break;
      }
  }
  else {                        /* Current file */
    puthelp("%s: Give destination (CR:select directory)", who);
    c = putecho("Copy file to:");
    if((c = getline(name, sizeof(name), c, 0, NULL, CLIST, 0)) < RV_NUL)
      return(c);
    else if(c == RV_OK)
      to = strcpy(name, pathname(name, CPNAM));
    else if((to = doselect("copying file")) == NULL) {
      fileflag |= SF_ECHO|SF_HELP;
      return(RV_NUL);
    }
    c = copyfile(cdlist, CFCUR, to, 0);
  }

  checkdlist(to);               /* Directory needs checking */
  fileflag |= SF_ECHO|SF_HELP;
  return(c);

} /* copy() */

/*
 *      ZOOM FILES
 */

/* Get zoom pattern and rebuild filelist */
LOCAL int zoomfile()
{
  char pat[PATLEN];
  register int c;

  who = "ZOOM FILE";
  puthelp("%s: Give file pattern (CR:all files)", who);
  if((c = getpattern(pat, "Zoom which files:")) < RV_NUL)
    return(c);
  if(zoomlist(cdlist, pat))
    fileflag |= SF_TREE;
  return(RV_OK);

} /* zoomfile() */

/*
 *      GOTO DIRECTORY / PARENT DIRECTORY
 */

/* Goto directory */
LOCAL int gotodirectory()
{
#ifdef  S_IFLNK
  struct stat st;
#endif  /* S_IFLNK */
  register dlist *dp;
  register int lev, c;

  if(CNFIL == 0)                /* Nothing to change to */
    return(RV_OK);

  who = "GOTO DIRECTORY";
  puthelp("%s: %s", who, FFNAM(cdlist, CFCUR));
  if(FMODE(cdlist, CFCUR) != FF_DIR) {
#ifdef  S_IFLNK
    if( !(FMODE(cdlist, CFCUR) == FF_SLNK && ISDIR(FFNAM(cdlist, CFCUR), st)))
#endif  /* S_IFLNK */
    return(errequest(FFNAM(cdlist, CFCUR), "Is not a directory"));
  }

  /* Search directory in directory tree */
  for(dp = (dlist *) CNEXT, lev = CLEVL+1; dp && DLEVL(dp) > CLEVL; dp = (dlist *) DNEXT(dp))
    if(DLEVL(dp) == lev && EQU(FFNAM(cdlist, CFCUR), DFNAM(dp)))
      break;
  /* Directory is not yet in directory tree */
  if(dp == DNULL || DLEVL(dp) != lev)
    dp = newdlist(FFNAM(cdlist, CFCUR), FL_NUL);

  /* Directory file list must be read in */
  if((DFLAG(dp) != FL_FIL || changedlist(dp)) && (c = newflist(dp)) != RV_OK)
    return(c);

  if( !DCANC(dp))
    return(errequest(DFNAM(dp), "Cannot change"));
  else if(DNFIL(dp) == 0) {
    puthelp("%s: %s (y:change  ELSE:don't change)", who, DFNAM(dp));
    c = errequest(DFNAM(dp), "Is empty, change anyway ?");
    if(c != 'y')
      return(c);
  }

  /* Position to directory in directory tree and return */
  while(cdlist != dp)
    (void) gotree(1);

  return(RV_DIR);

} /* gotodirectory() */

/* Goto parent directory */
LOCAL int gotoparent()
{
  register char *name;
  register int lev, c;

  if(cdlist == droot)           /* Root has no parent */
    return(RV_RET);

  /* Position to parent directory in directory tree */
  name = CFNAM;
  lev  = CLEVL - 1;
  while(CLEVL != lev)
    (void) gotree(-1);
  /* Parent directory needs rebuilding */
  if((CFLAG != FL_FIL || changedlist(cdlist))
     && (c = newflist(cdlist)) != RV_OK)
    return(c);
  /* Position to where we came from in parent directory file list */
  CFCUR = CFTOP = 0;
  while(CMP(name, FFNAM(cdlist, CFCUR)) && CFCUR < CNFIL)
    (void) gofile(cdlist, 1);

  return(RV_DIR);

} /* gotoparent() */

/*
 *      TAG / UNTAG FILE(S)
 */

/* Tag current or selected file(s) */
LOCAL int tag(one)
  register int one;
{
  char input[PATLEN];
  register int f, c;

  if(CNFIL == 0)                /* Nothing to tag */
    return(RV_OK);

  who = "TAG FILE";
  if( !one) {                   /* Multiple files */
    puthelp("%s: Give file pattern (CR:%s)", who, tpattern[0] ? tpattern : "quit");
    if((c = getpattern(input, "Tag which file:")) == RV_OK)
      (void) strcpy(tpattern, input);
    else if(c < RV_NUL || (c == RV_NUL && tpattern[0] == '\0'))
      return(c);
  }
  if(one)                       /* Currrent file */
    tagfile(CFCUR);
  else                          /* Matching files */
    for(f = 0; f < CNFIL; f++)
      if(umatch(cdlist, f, tpattern) > 0)
	tagfile(f);

  return(RV_OK);

} /* tag() */

/* Untag current or selected file(s) */
LOCAL int untag(one)
  register int one;
{
  char pattern[PATLEN];
  register int f, c;

  if(CNFIL == 0)                /* Nothing to untag */
    return(RV_OK);

  who = "UNTAG FILE";
  if( !one) {                   /* Multiple files */
    setmark(CFCUR, FF_MARK);
    puthelp("%s: Give file pattern (CR:all files)", who);
    c = getpattern(pattern, "Untag which file:");
    unsetmark(CFCUR);
    if(c < RV_NUL)
      return(c);
    else if(c == RV_NUL)
      (void) strcpy(pattern, "*");
  }
  if(one && ISTAG(cdlist, CFCUR)) {     /* Current file */
    untagfile(CFCUR);
    unsetmark(CFCUR);
  }
  else                                  /* Matching files */
    for(f = 0; f < CNFIL; f++)
      if(umatch(cdlist, f, pattern) > 0 && ISTAG(cdlist, f)) {
	untagfile(f);
	unsetmark(f);
      }

  return(RV_OK);

} /* untag() */

/*
 *      EXECUTE CURRENT FILE
 */

/* Check if current file is given in command */
LOCAL int filemissing(s)
  register char *s;
{
  while(*s) {
    if(*s == '%') {             /* Leadin found */
      ++s;
      if(*s == 'f' || *s == 'F' || *s == 'p' || *s == 'P')
	return(0);              /* Found */
    }
    else
      ++s;
  }
  return(1);

} /* filemissing() */

/* Execute or execute on file f */
LOCAL int execfile(f, m)
  register int f, m;
{
  char cmd[EXECLEN], buf[2*INPLEN], par[INPLEN];
  register xlist *xp;
  register int c;

  if(m)
    setmark(f, FF_MARK);
  /* Check if a command is defined for filetype */
  for(xp = xroot; xp; xp = (xlist *) XNEXT(xp))
    if(umatch(cdlist, f, XTYPE(xp)))
      break;

  /* Execute filetype dependent command if defined */
  if(xp && XCOMD(xp))  {
    if(XCOMM(xp))
      puthelp("%s: %s #%s", who, FFNAM(cdlist, f), XCOMM(xp));
    else
      puthelp("%s: %s", who, FFNAM(cdlist, f));
    c = putecho("Execute:");
    c = getline(buf, sizeof(buf), c, 'l', XCOMD(xp), GNULL, 0);
    if(m)
      unsetmark(f);
    if(c != RV_OK)
      return(c);
    if(filemissing(buf))
      (void) sprintf(buf, "%s %%F", buf);
  }
  /* File is executable */
  else if(FMODE(cdlist, f) == FF_EXEC) {
    puthelp("%s: Give parameter(s) or a command", who);
    c = putecho("Execute %s:", FFNAM(cdlist, f));
    c = getline(par, sizeof(par), c, 'l', NULL, GNULL, 0);
    if(m)
      unsetmark(f);
    if(c < RV_NUL)
      return(c);
    else if(c == RV_NUL)
      (void) strcpy(buf, "./%F");
    else if(filemissing(par))
      (void) sprintf(buf, "%%F %s", par);
    else
      (void) strcpy(buf, par);
  }
  /* Execute command on current file */
  else {
    puthelp("%s: Give a command and parameter(s)", who);
    c = putecho("Execute on %s:", FFNAM(cdlist, f));
    c = getline(par, sizeof(par), c, 'l', NULL, GNULL, 0);
    if(m)
      unsetmark(f);
    if(c != RV_OK)
      return(c);
    if(filemissing(par))
      (void) sprintf(buf, "%s %%F", par);
    else
      (void) strcpy(buf, par);
  }

  /* Format command line and execute */
  c = userformat(cmd, buf, V_FC1, "EXECUTE");
  if(c == RV_NUL) {
    puthelp("%s %s", who, hitkey);
    return(errequest(FFNAM(cdlist, f), "Bad format"));
  }
  else if(c < RV_NUL)
    return(c);

  puthelp("%s: %s", who, cmd);
  c = callsystem(cmd, 1, 0);

  if(c != RV_OK) {
    puthelp("%s: %s %s", who, cmd, hitkey);
    return(errequest(FFNAM(cdlist, f), "Error in executing"));
  }
  return(hitakey("Return from execute (Hit a key)", lines-1, DA_REVERSE));

} /* execfile() */

/* Execute or execute on files */
LOCAL int execute(one)
  register int one;
{
  register int c, f, m;

  if(CNFIL == 0)                /* Nothing to execute */
    return(RV_OK);

  who = "EXECUTE FILE";
  if( !one && CNTAG) {          /* Tagged files */
    m = 1;
    for(f = 0; f < CNFIL; f++)
      if(ISTAG(cdlist, f)) {
	untagfile(f);
	c = execfile(f, m);
	m = 0;
	if(c == 'q' || c < RV_NUL)
	  break;
      }
  }
  else                          /* Current file */
    c = execfile(CFCUR, 1);

  checkdlist(CPNAM);            /* Directory needs checking */
  return(c);

} /* execute() */

/*
 *      FILE MENU LOOP
 */

#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
/* Refresh file screen after screen size changes */
GLOBL int refreshfile(f)
  register int f;
{
  if(f)
    (void) refreshtree(0);
  f = CFCUR;
  CFTOP = CFCUR = 0;
  while(CFCUR != f && gofile(cdlist, 1))
    ;
  flast = -1;
  fileflag = SF_FULL;
  return(RV_OK);

} /* refreshfile() */
#endif  /* SIGWINCH && TIOCGWINSZ */

/* File menu */
GLOBL int filemenu(f, r)
  register int f, r;
{
  register int c, i, j;

  who = "FILE MENU";
  /* Change to directory and check if it is empty */
  if((c = changelist(cdlist, who)) != RV_OK)
    return(c);
  else if(r != RV_DIR && CNFIL == 0) {
    puthelp("%s: %s (Y:change  ELSE:don't change)", who, CFNAM);
    if((c = errequest(CFNAM, "Is empty, change anyway ?")) != 'y')
      return(c);
  }

  /* Position to current file on screen */
  if(f >= 0 && f < CNFIL)
    while(CFCUR != f)
      (void) gofile(cdlist, CFCUR < f ? 1 : -1);

  /* Init file variables */
  if(menuline == NULL)
    menuline = fmline;
  buildflag = 0;
  nscroll   = 0;
  flast     = -1;
  fmark     = -1;
  fileflag  = SF_FULL;

  /* File menu loop */
  do {
    /* Update file screen if needed and clock */
    if(fileflag && !keypressed())
      updatefiles();
#ifdef  UTCLOCK
    if(VARSET(V_CL))
      clockon();
#endif  /* UTCLOCK */
    flushout();
    c = getkey();
#ifdef  UTCLOCK
    if(VARSET(V_CL))
      clockoff();
#endif  /* UTCLOCK */
    switch(c) {
      default:                  /* Unknown: ring the bell */
	bell(VARSET(V_BL));
	break;
      case K_BACK:              /* Previous file */
      case 'k':                 /* For vi fans */
	if( !gofile(cdlist, -1))
	  bell(VARSET(V_BL));
	break;
      case K_FORW:              /* Next file */
      case 'j':                 /* For vi fans */
	if( !gofile(cdlist, 1))
	  bell(VARSET(V_BL));
	break;
      case K_PREV:              /* Up file */
	if(gofile(cdlist, -1))
	  for(i = 1; i < fperline; i++)
	    (void) gofile(cdlist, -1);
	else
	  bell(VARSET(V_BL));
	break;
      case K_NEXT:              /* Down file */
	if(gofile(cdlist, 1))
	  for(i = 1; i < fperline; i++)
	    (void) gofile(cdlist, 1);
	else
	  bell(VARSET(V_BL));
	break;
      case K_PPAG:              /* Previous page */
	if(CFTOP > 0 && gofile(cdlist, -1))
	  for(i = 1; i < fperpage && gofile(cdlist, -1); i++)
	    ;
	else
	  bell(VARSET(V_BL));
	break;
      case K_NPAG:              /* Next page */
	if((CFTOP + fperpage) < CNFIL && gofile(cdlist, 1))
	  for(i = 1; i < fperpage && gofile(cdlist, 1); i++)
	    ;
	else
	  bell(VARSET(V_BL));
	break;
      case K_HOME:              /* First file */
	i = CFCUR;
	if(gofile(cdlist, -1)) {
	  fmark = i;
	  while(gofile(cdlist, -1))
	    ;
	}
	else
	  bell(VARSET(V_BL));
	break;
      case K_END:               /* Last file */
	i = CFCUR;
	if(gofile(cdlist, 1)) {
	  fmark = i;
	  while(gofile(cdlist, 1))
	    ;
	}
	else
	  bell(VARSET(V_BL));
	break;
      case '@':                 /* Mark current file */
      case K_MARK:
	fmark = CFCUR;
	break;
      case '#':                 /* Goto previously marked file */
      case K_GOTO:
	j = fmark;
	for(i = CFCUR; i < CNFIL; i++)
	  if(fmark == i) {
	    while(gofile(cdlist, 1) && CFCUR != fmark)
	      ;
	    fmark = j;
	    goto MDONE;
	  }
	for(i = 0; i < CFCUR; i++)
	  if(fmark == i) {
	    while(gofile(cdlist, -1) && CFCUR != fmark)
	      ;
	    fmark = j;
	    goto MDONE;
	  }
	bell(VARSET(V_BL));
MDONE:  break;
      case K_TAG:               /* Next tagged file */
	for(i = CFCUR + 1; i < CNFIL; i++)
	  if(ISTAG(cdlist, i)) {
	    while(gofile(cdlist, 1) && CFCUR != i)
	      ;
	    goto TDONE;
	  }
	for(i = 0; i < CFCUR; i++)
	  if(ISTAG(cdlist, i)) {
	    while(gofile(cdlist, -1) && CFCUR != i)
	      ;
	    goto TDONE;
	  }
	bell(VARSET(V_BL));
TDONE:  break;
      case K_UP:                /* Scroll up */
	if( !scrollfile(-1))
	  bell(VARSET(V_BL));
	break;
      case K_DOWN:              /* Scroll down */
	if( !scrollfile(1))
	  bell(VARSET(V_BL));
	break;
      case '>':                 /* Change to directory */
      case K_INS:
	c = gotodirectory();
	break;
      case '<':                 /* Back to parent directory */
      case K_DEL:
	c = gotoparent();
	break;
      case K_SIZE:              /* Screen size changed */
#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
	c = RV_SIZ;
#else   /* !SIGWINCH || !TIOCGWINSZ */
	c = RV_OK;
#endif  /* SIGWINCH && TIOCGWINSZ */
	/*Fall thru*/
      case K_REFR:              /* Refresh */
	fileflag = SF_FULL;
	break;
      case 'n':                 /* New sort file list */
      case 'N':
	fmark = -1;
	if((c = sortlist(cdlist, CSORT ? 0 : 1)) == RV_OK)
	  fileflag |= SF_TREE;
	break;
      case 'd':                 /* Date */
      case 'D':
	c = printdate();
	break;
      case 'w':                 /* Current directory */
      case 'W':
	c = printcwd();
	break;
      case '?':                 /* Help */
      case 'H':
      case 'h':
      case K_HELP:
	c = showhelp('f');
	break;
      case 't':                 /* Tag current or tagged file(s) */
      case 'T':
	c = tag(CUR(c));
	break;
      case 'u':                 /* Untag current or tagged file(s) */
      case 'U':
	c = untag(CUR(c));
	break;
      case 'g':                 /* Search string in file */
      case 'G':
	c = grep(CUR(c));
	break;
      case 'f':                 /* Find file */
      case 'F':
	c = find(CUR(c));
	break;
      case 'c':                 /* Copy current or tagged file(s) */
      case 'C':
	c = copy(CUR(c));
	break;
      case 'e':                 /* Edit current or tagged file(s) */
      case 'E':
	c = edit(CUR(c));
	break;
      case 's':                 /* Status of current or tagged file(s) */
      case 'S':
	c = status(CUR(c), 1);
	break;
      case 'i':                 /* Short info of current or tagged file(s) */
      case 'I':
	c = status(CUR(c), 0);
	break;
      case 'p':                 /* Print current or tagged file(s) */
      case 'P':
	c = print(CUR(c));
	break;
      case 'v':                 /* View current or tagged file(s) */
      case 'V':
	c = view(CUR(c));
	break;
      case 'm':                 /* Move current or tagged file(s) */
      case 'M':
	c = move(CUR(c));
	break;
      case 'r':                 /* Remove current or tagged file(s) */
      case 'R':
	c = Remove(CUR(c));
	break;
      case 'l':                 /* List files */
      case 'L':
	c = list(CUR(c));
	break;
      case 'x':                 /* Execute current file */
      case 'X':
	c = execute(CUR(c));
	break;
      case '0':                 /* Switch menu line */
	menuline = menuline == ufilemenu ? fmline : ufilemenu;
	fileflag |= SF_HELP;
	break;
      case '1':                 /* User defined file commands 1..9 */
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
	c = usercommand(c - '0' + V_FC0);
	break;
      case '!':                 /* Shell escape */
      case '$':
	c = history(c, V_FC1);
	if(VARSET(V_ST))
	  (void) scandlist(droot);
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
      case '/':                 /* Rebuild file list */
	fmark = -1;
	CFLAG = FL_CHG;
	++buildflag;
	c = RV_OK;
	break;
      case 'z':                 /* Zoom file list */
      case 'Z':
	c = zoomfile();
	break;
      case 'a':                 /* Display version string */
      case 'A':
	c = putversion(echoline, "ABOUT: Utree version");
	break;
      case 'q':                 /* Return to tree screen */
      case 'Q':
      case ' ':
      case K_SEL:
      case K_BRK:
	c = RV_RET;
	break;
      case K_EOF:               /* Exit */
	c = RV_END;
	break;
    }
#if     defined(SIGWINCH) && defined(TIOCGWINSZ)
    /* Refresh screen after screen resize */
    if(c == RV_SIZ)
      c = refreshfile(1);
#endif  /* SIGWINCH && TIOCGWINSZ */
    /* Rebuilding needed */
    if(buildflag && updatedlist() != RV_OK)
      c = RV_ERR;
  } while(c >= RV_INT);

  /* Set treeflag and return */
  treeflag = SF_FULL;
  return(c);

} /* filemenu() */
