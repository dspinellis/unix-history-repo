/*
 *      COMM.C
 *      UTREE common tree and file commands.
 *      3.01-um klin, Tue Jun  4 14:16:30 1991
 *              klin, Mon Oct  7 15:16:22 1991, File size patterns added
 *              klin, Sat Oct 26 15:07:06 1991, Sorting and zooming filelists changed
 *      3.02-um klin, Fri Nov  1 13:42:28 1991, Minor changes
 *              klin, Sun Nov 24 19:30:43 1991, Cd to current directory before
 *                                              executing some commands
 *      3.03-um klin, Tue Feb 11 19:40:06 1992, Status screen into stat.c
 *                                              getline() changed
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03-um (klin) Feb 11 1992 comm.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

#define CCOLUMN -8              /* Column for clock                     */

/* Tokens for file pattern and time pattern matching                    */
#define TK_ERR  'F'             /* Error                                */
#define TK_END  'E'             /* End of pattern                       */
#define TK_WORD 'W'             /* Word or file pattern matching        */
#define TK_OR   '|'             /* ORing patterns                       */
#define TK_AND  '&'             /* ANDing pattern                       */
#define TK_SEQ  '='             /* Filesize equate                      */
#define TK_SNE  '!'             /* Filesize unequate                    */
#define TK_SGT  '>'             /* Filesize bigger                      */
#define TK_SLT  '<'             /* Filesize smaller                     */
#define TK_MNE  ')'             /* File     modified within (newer)     */
#define TK_MOL  '('             /* File not modified within (older)     */

/* States from token()                                                  */
#define ST_NORM 0               /* Normal state                         */
#define ST_WORD 1               /* In word                              */
#define ST_QUOT 2               /* In quotes                            */
#define ST_BRCK 3               /* In brackets                          */

LOCAL int patpos;               /* Current position in pattern          */
LOCAL char patbuf[PATLEN];      /* Pattern buffer for internal use      */

/* ---- External variables and functions ------------------------------ */

EXTRN struct tm *localtime();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      COMMON COMMANDS
 */

/* Display current date and time in echo line */
GLOBL int printdate()
{
  time_t tloc;

  tloc = time((time_t *) 0);
  puthelp("DATE: Current date and time %s", hitkey);
  (void) putecho("%s", ctime(&tloc));
  return(hitakey(NULL));

} /* printdate() */

/* Display current directory in echo line */
GLOBL int printcwd()
{
  puthelp("WHERE: Pathname of current directory %s", hitkey);
  (void) putecho("%s", CPNAM);
  return(hitakey(NULL));

} /* printcwd() */

/*
 *      CLOCK ROUTINES
 */

#ifdef  UTCLOCK
/* Turn on clock in echo line, update every second */
GLOBL int clockon()
{
  time_t tloc;
  register struct tm *tm;

  /* Get and display current time in echo line */
  tloc = time((time_t *) 0);
  tm   = localtime(&tloc);
  if(cursorcap & CF_SAVE);
    cursorset(CF_SAVE);
  (void) putfxy(CCOLUMN, echoline, 0, "%02d:%02d:%02d",
		tm->tm_hour, tm->tm_min, tm->tm_sec);
  if(cursorcap & CF_RESTORE);
    cursorset(CF_RESTORE);
  flushout();
  /* Set alarm for the next second and catch signal SIGALRM */
  (void) alarm(1);
  (void) signal(SIGALRM, (SIGNL(*)()) clockon);

} /* clockon() */

/* Turn off clock in echo line */
GLOBL VOID clockoff()
{
  (void) alarm(0);              /* Suppress alarm */

} /* clockoff() */
#endif  /* UTCLOCK */

/*
 *      FILE AND TIME STAMP PATTERN MATCHING ROUTINES
 */

/* Get and return next token. If a word is found copy it to wp */
LOCAL int token(wp)
  register char *wp;
{
  register int s;
  register char c, *w;

  s = ST_NORM;
  w = wp;
  /* Get token loop */
  while(1) {
    c = patbuf[patpos++];
    switch(s) {
      case ST_NORM:             /* Normal state */
	switch(c) {
	  case '\n':
	  case '\r':
	  case '\0':
	    c = TK_END;
	    /*Fall thru*/
	  case TK_END:
	  case TK_AND:
	  case TK_OR:
	  case TK_SEQ:
	  case TK_SNE:
	  case TK_SGT:
	  case TK_SLT:
	  case TK_MNE:
	  case TK_MOL:
	    return(c);
	  case ' ':
	  case '\t':
	    continue;
	  case '"':
	  case '\'':
	    s = ST_QUOT;
	    continue;
	  case '[':
	    s = ST_BRCK;
	    *w++ = c;
	    continue;
	  case '\\':
	    if((c = patbuf[patpos++]) == '\0')
	      return(TK_ERR);
	    /*Fall thru*/
	  default:
	    s = ST_WORD;
	    *w++ = c;
	    continue;
	}
      case ST_BRCK:             /* In brackets */
	switch(c) {
	  case '\n':
	  case '\r':
	  case '\0':
	    return(TK_ERR);
	  case ']':
	    s = ST_WORD;
	    /*Fall thru*/
	  default:
	    *w++ = c;
	    continue;
	}
      case ST_QUOT:             /* In quotes */
	switch(c) {
	  case '"':
	  case '\'':
	    *w = '\0';
	    return(TK_WORD);
	  case '\\':
	    if((c = patbuf[patpos++]) == '\0')
	      return(TK_ERR);
	    /*Fall thru*/
	  default:
	    *w++ = c;
	    continue;
	}
      case ST_WORD:             /* In word */
	switch(c) {
	  case TK_AND:
	  case TK_OR:
	  case TK_SEQ:
	  case TK_SNE:
	  case TK_SGT:
	  case TK_SLT:
	  case TK_MNE:
	  case TK_MOL:
	  case '\n':
	  case ' ':
	  case '\t':
	  case '\0':
	    --patpos;
	    *w = '\0';
	    return(TK_WORD);
	  case '[':
	    s = ST_BRCK;
	    *w++ = c;
	    continue;
	  case '\\':
	    if((c = patbuf[patpos++]) == '\0')
	      return(TK_ERR);
	    /*Fall thru*/
	  default:
	    *w++ = c;
	    continue;
	}
    }
  }
  /*NOTREACHED*/

} /* token() */

/* Token parsing and file/size/time pattern matching */
LOCAL int imatch(dp, f)
  register dlist *dp;
  register int f;
{
  static int lt;
  char w[PATLEN];
  register time_t ct;
  register off_t fs;
  register int t, i;

  /* Inner matching loop */
  while(1) {
    switch(t = token(w)) {      /* Get next token */
      default:                  /* Unknown */
	t = TK_ERR;
	/*Fall thru*/
      case TK_ERR:              /* Return as it is */
	return(t);
      case TK_END:              /* End of pattern */
      case TK_OR:               /* ORing patterns */
      case TK_AND:              /* ANDing patterns */
	if(lt != TK_WORD)
	  return(TK_ERR);
	return(lt = t);
      case TK_WORD:             /* File pattern matching */
	lt = t;
	if(dp)
	  return(match(FFNAM(dp, f), w));
	return(1);
      case TK_SEQ:              /* Filesize equate   */
      case TK_SNE:              /* Filesize unequate */
      case TK_SGT:              /* Filesize bigger   */
      case TK_SLT:              /* Filesize smaller  */
	if((lt = token(w)) != TK_WORD || !isdigit(w[0]))
	  return(TK_ERR);
	fs = (off_t) 0;
	for(i = 0; w[i] && isdigit(w[i]); i++)
	  fs = fs * (off_t) 10 + (off_t) (w[i] - '0');
	switch(w[i]) {
	  default:              /* Error */
	    return(TK_ERR);
	  case 'm':             /* Megabytes */
	    fs *= (off_t) 1024;
	    /*Fall thru*/
	  case 'k':             /* Kilobytes */
	    fs *= (off_t) 1024;
	    /*Fall thru*/
	  case 'b':             /* Bytes (default) */
	  case '\0':
	    break;
	}
	if(dp) {
	  switch(t) {
	    case TK_SEQ:
	      return(FSIZE(dp,f ) == fs);
	    case TK_SNE:
	      return(FSIZE(dp, f) != fs);
	    case TK_SGT:
	      return(FSIZE(dp, f)  > fs);
	    case TK_SLT:
	      return(FSIZE(dp,f )  < fs);
	  }
	}
	return(1);
      case TK_MNE:              /* Modified within     days/hours/minutes */
      case TK_MOL:              /* Not modified within days/hours/minutes */
	if((lt = token(w)) != TK_WORD || !isdigit(w[0]))
	  return(TK_ERR);
	ct = (time_t) 0;
	for(i = 0; w[i] && isdigit(w[i]); i++)
	  ct = ct * (time_t) 10 + (time_t) (w[i] - '0');
	switch(w[i]) {
	  default:              /* Error */
	    return(TK_ERR);
	  case 'w':             /* Weeks */
	    ct *= (time_t) 7;
	    /*Fall thru*/
	  case 'd':             /* Days */
	    ct *= (time_t) 24;
	    /*Fall thru*/
	  case 'h':             /* Hours (default) */
	  case '\0':
	    ct *= (time_t) 60;
	    /*Fall thru*/
	  case 'm':             /* Minutes */
	    ct *= (time_t) 60;
	    break;
	}
	if(ct == (time_t) 0)
	  return(TK_ERR);
	else if(dp) {
	  ct = time((time_t *) 0) - ct;
	  return(t == TK_MNE ? FMTIM(dp, f) > ct : FMTIM(dp, f) <= ct);
	}
	return(1);
    }
  }
  /*NOT REACHED*/

} /* imatch() */

/* Do/combine file/size/time pattern matching */
GLOBL int umatch(dp, f, p)
  register dlist *dp;
  register int f;
  register char *p;
{
  register int rv;

  (void) strcpy(patbuf, p);
  patpos = 0;
  rv = -1;
  /* Outer matching loop */
  while(1) {
    switch(imatch(dp, f)) {
      case TK_END:              /* End */
	return(rv <= 0 ? 0 : 1);
      case 1:                   /* Match */
	rv = 1;
	break;
      case 0:                   /* No match */
	rv = 0;
	break;
      case TK_OR:               /* ORing */
	if(rv < 0)
	  return(rv);
	else
	  rv = (rv || imatch(dp, f) == 1) ? 1 : 0;
	if(dp && rv)
	  return(rv);
	break;
      case TK_AND:              /* ANDing */
	if(rv < 0)
	  return(rv);
	else
	  rv = (rv && imatch(dp, f) == 1) ? 1 : 0;
	if(dp && rv == 0)
	  return(rv);
	break;
      default:
      case TK_ERR:
	return(-1);
    }
  }
  /*NOT REACHED*/

} /* umatch() */

/* Get file/size/time pattern and test it */
GLOBL int getpattern(p, m)
  register char *p, *m;
{
  register int c;

  /* Get/test pattern loop */
  while(1) {
    c = putecho(m);
    c = getline(p, PATLEN, c, 'p', NULL, GNULL, 0);
    if(c == RV_OK && umatch(DNULL, 0, p) < 0)
      if((c = errequest(p, "Bad file pattern")) >= RV_NUL)
	continue;
    break;
  }
  return(c);

} /* getpattern() */

/*
 *      COMMON DIRECTORY AND FILE ROUTINES
 */

/* Change current working directory to directory list entry dp */
GLOBL int changelist(dp, who)
  register dlist *dp;
{
  if(cwlist == dp)                  /* Is already cwd */
    return(RV_OK);
  else if(DCANC(dp) && chdir(DPNAM(dp)) == 0) {
    cwlist = dp;
    return(RV_OK);
  }
  else if(who) {
    puthelp("%s: %s %s", who, CFNAM, hitkey);
    return(errequest(CFNAM, "Cannot change"));
  }
  return(RV_NUL);

} /* changelist() */

/* Check if operation for file fn with mode m is allowed */
GLOBL int isallowed(fn, m)
  register char *fn;
  register int m;
{
#ifdef  S_IFLNK
  struct stat st;
#endif  /* S_IFLNK */
  register char *err;

  switch(m) {
    default:                    /* Others */
      return(1);
#ifdef  S_IFLNK
    case FF_SLNK:               /* Check symbolic link */
      if( !ISDIR(fn, st))
	return(1);
      /* Fall thru */
#endif  /* S_IFLNK */
    case FF_DIR:                /* Is directory */
      err = "Is a directory";
      break;
    case FF_ERR:                /* Error in stat */
      err = "Cannot stat";
      break;
  }
  setvideo(DA_ERROR);
  (void) putecho("!! %s: %s", fn, err);
  setvideo(DA_NORMAL);
  return(0);

} /* isallowed() */

/* Set sort flag to f and sort/resort filelist */
GLOBL int sortlist(dp, f)
  register dlist *dp;
  register int f;
{
  register char *n;

  if(DSORT(dp) != f) {
    DSORT(dp) = f;
    if(DNFIL(dp) > 0) {
      n = FFNAM(dp, DFCUR(dp));
      sortflist(dp);
      DFTOP(dp) = DFCUR(dp) = 0;
      while(*n != *FFNAM(dp, DFCUR(dp)) || CMP(n, FFNAM(dp, DFCUR(dp))))
	(void) gofile(dp, 1);
    }
  }
  return(DNFIL(dp) ? RV_OK : RV_NUL);

} /* sortlist() */

/* Rebuild file list with new zoom pattern pat */
GLOBL int zoomlist(dp, pat)
  register dlist *dp;
  register char *pat;
{
  register int n;

  if(DZOOM(dp)) {               /* Free old pattern */
    ufree(DZOOM(dp));
    DZOOM(dp) = NULL;
  }
  if(pat && pat[0])             /* Save new pattern */
    DZOOM(dp) = strsav(pat);

  n = DNFIL(dp);
  if(DFLAG(dp) != FL_NUL) {     /* Rebuild filelist */
    (void) newflist(dp);
    if(n != DNFIL(dp));
      return(1);
  }

  return(0);

} /* zoomlist() */

/* Search for pattern in file f from directory dp */
GLOBL int grepfile(dp, f)
  register dlist *dp;
  register int f;
{
  char buf[EXECLEN];
  register int rv;

  (void) sprintf(buf, "%s %s \"%s\" %s",
		 GRFILE, GROPTS, gpattern, pathname(FFNAM(dp, f), DPNAM(dp)));
  if((rv = callsystem(buf, 0, 0)) == RV_INT)
    return(errequest("Grep", "Interrupted"));
  return(rv);

} /* grepfile() */

/* Find file f in directory dp */
GLOBL int findfile(dp, f)
  dlist *dp;
  register int f;
{

  if(f < DNFIL(dp) && umatch(dp, f, fpattern))
    return(RV_OK);
  return(RV_NUL);

} /* findfile() */

/* Remove file f from directory dp */
GLOBL int removefile(dp, f, req)
  register dlist *dp;
  register int f, req;
{
  char name[NAMELEN], buf[EXECLEN];
  register int c;

  /* Check if file can be removed */
  if( !isallowed(FFNAM(dp, f), (int) FMODE(dp, f)))
    return(hitakey(NULL));

  (void) strcpy(name, pathname(FFNAM(dp, f), DPNAM(dp)));
  if(req) {
    (void) putecho("Remove %s ?", FFNAM(dp, f));
    if((c = hitakey(NULL)) != 'y')
      return(c);
  }
  else {
    (void) putecho("Removing %s", FFNAM(dp, f));
    flushout();
  }
  (void) sprintf(buf, "%s %s", RMFILE, name);
  if(callsystem(buf, 0, 0) != RV_OK)
    return(errequest(FFNAM(dp, f), "Cannot remove"));

  /* Preserve current file, delete flist and set flags */
  if(f < DFCUR(dp))
    (void) gofile(dp, -1);
  deleteflist(dp, f);
  ++buildflag;
  DFLAG(dp) = FL_CHG;

  return(RV_OK);

} /* removefile() */

/* Copy file f from directory dp to directory/file to */
GLOBL int copyfile(dp, f, to, req)
  register dlist *dp;
  register int f, req;
  register char *to;
{
  char from[NAMELEN], buf[EXECLEN];
  register int c;

  /* Check if copying is allowed */
  if( !isallowed(FFNAM(dp, f), (int) FMODE(dp, f)))
    return(hitakey(NULL));

  (void) strcpy(from, pathname(FFNAM(dp, f), CPNAM));
  if(req) {
    (void) putecho("Copy %s ?", FFNAM(dp, f));
    if((c = hitakey(NULL)) != 'y')
      return(c);
  }

  (void) putecho("Copying %s to %s", FFNAM(dp, f), to);
  (void) sprintf(buf, "%s %s %s", CPFILE, from, to);
  if(callsystem(buf, 0, 0) != RV_OK)
    return(errequest(FFNAM(dp, f), "Error in copying"));

  return(RV_OK);

} /* copyfile() */

/* Move file f from directory dp to directory/file to */
GLOBL int movefile(dp, f, to, req)
  register dlist *dp;
  register int f, req;
  register char *to;
{
  char from[NAMELEN], buf[EXECLEN];
  register int c;

  /* Check if moving is allowed */
  if( !isallowed(FFNAM(dp, f), (int) FMODE(dp, f)))
    return(hitakey(NULL));

  (void) strcpy(from, pathname(FFNAM(dp, f), CPNAM));
  if(req) {
    (void) putecho("Move %s to %s ?", FFNAM(dp, f), to);
    if((c = hitakey(NULL)) != 'y')
      return(c);
  }

  (void) putecho("Moving %s to %s", FFNAM(dp, f), to);
  (void) sprintf(buf, "%s %s %s", MVFILE, from, to);
  if(callsystem(buf, 0, 0) != RV_OK)
    return(errequest(FFNAM(dp, f), "Error in moving"));

  return(RV_OK);

} /* movefile() */
