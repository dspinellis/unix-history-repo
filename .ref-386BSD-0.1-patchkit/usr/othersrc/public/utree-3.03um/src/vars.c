/*
 *      VARS.C
 *      UTREE variable handling routines.
 *      3.01-um klin, Sun May  5 11:05:05 1991
 *      3.02-um klin, Fri Nov  1 10:46:14 1991, Option -u changed to -n
 *              klin, Sun Nov 24 15:26:19 1991, Video attributes changed
 *      3.03-um klin, Tue Feb 11 14:18:50 1992, Generic lists for variables
 *                                              and file type commands
 *              klin, Sat Feb 15 14:44:52 1992, Video handling and partinioning of
 *                                              directory and file windows changed
 *              klin, Sun Feb 23 18:45:19 1992, Keybindings and variable
 *                                              AUTOSAVE added
 *              klin, Fri Mar  6 09:34:43 1992, Release undefined commands
 *            a klin, Sun Mar 15 19:08:25 1992, Accept command without comment
 *            b klin, Sun Mar 22 10:41:36 1992, Bug fix and minor changes
 *            e klin, Sat Apr 11 11:05:54 1992, Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 vars.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

/* Format for user command menulines on tree and file screen            */
#define UMENUFORM \
" 1:      2:      3:      4:      5:      6:      7:      8:      9:     "
#define FIRST   3
#define OFFSET  8
#define TITLE   5

#define XUNDEF  ((xlist *) -1)  /* Undefined or released xlist          */

LOCAL int vcchg = 0;            /* Variables/commands changed           */
LOCAL char *novar = "";         /* Empty string variable                */

GLOBL VOID checkindent();
GLOBL VOID checklines();
LOCAL VOID setsortflag();
#ifdef  USEANSICOLORS
LOCAL VOID setcolorflag();
#endif  /* USEANSICOLORS */

/* ---- External variables and functions ------------------------------ */

EXTRN char *getversion();
EXTRN char *strclean();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Show all variables */
LOCAL int showallvars()
{
  register vlist *vp;
  register int l, c;

  l = firstline;
  c = RV_OK;
  clearwindow(firstline, lastline);
  for(vp = VARTAB(0), l = firstline; vp; vp = (vlist *) VNEXT(vp)) {
    (void) putfxy(0, l, 0, "%s %12s=", VSCUT(vp), VNAME(vp));
    if(VTYPE(vp) == VT_B)
      c = putfxy(16, l, 0, "%s", VVALE(vp) == VB_ON ? "ON" : "OFF");
    else
      c = putfxy(16, l, 0, "%s", VVALE(vp) ? VVALE(vp) : "");
    if(VCOMM(vp))
      (void) putfxy(c > columns/2 ? c : columns/2, l, 0, "#%s", VCOMM(vp));
    if(++l > lastline && VNEXT(vp)) {
      puthelp("VARIABLES (CR:continue  Q:quit  ELSE:set/unset)");
      c = hitakey("More variables ?", echoline, DA_NONE);
      if( !(c == ' ' || c == '\n'))
	break;
      clearwindow(firstline, lastline);
      l = firstline;
    }
  }
  treeflag = fileflag = SF_FULL;
  return(c);

} /* showallvars() */

/* Display file type dependent commands */
LOCAL int showallcmds()
{
  register xlist *xp;
  register int l, c;

  l = firstline;
  c = RV_OK;
  clearwindow(firstline, lastline);
  for(xp = xroot, l = firstline; xp; xp = (xlist *) XNEXT(xp)) {
    c = putfxy(0, l, 0, "%s:%s", XTYPE(xp), XCOMD(xp));
    if(XCOMM(xp))
      (void) putfxy(c > (columns/2) ? c+1 : columns/2, l, 0, "#%s", XCOMM(xp));
    if(++l > lastline && XNEXT(xp)) {
      puthelp("FILETYPE COMMANDS (CR:continue  Q:quit  ELSE:set/unset)");
      c = hitakey("More filetype commands ?", echoline, DA_NONE);
      if( !(c == ' ' || c == '\n'))
	break;
      clearwindow(firstline, lastline);
      l = firstline;
    }
  }
  treeflag = fileflag = SF_FULL;
  return(c);

} /* showallcmds() */

/* Set sort criteria flag for file lists */
LOCAL VOID setsortflag(f)
  register int f;
{
  register dlist *dp;

  sortbytime = f ? 0 : 1;
  for(dp = droot; dp; dp = (dlist *) DNEXT(dp))
    (void) sortlist(dp, sortbytime);

} /* setsortflag() */

#ifdef  USEANSICOLORS
/* Set color using flag */
LOCAL VOID setcolorflag(f)
  register int f;
{
  usecolors = f ? 1 : 0;
  if(colorcap)
    colorset(usecolors ? CS_INIT : CS_RESET);

} /* setcolorflag() */
#endif  /* USEANSICOLORS */

/* Set tree indention variable */
LOCAL int setindent(i)
  register int i;
{
  if(i < MININD || i > MAXIND)          /* Out of range */
    return(-1);
  maxindent = indent = i;               /* Set indention variables */
  checkindent();
  return(0);

} /* setindent() */

/* Set video mode variable */
GLOBL int setvideomode(i)
  register int i;
{
  if(i  < VMODE0 || i > VMODE2)         /* Out of range */
    return(-1);
  else if(glitchcap && i > VMODE1)      /* Respect glitches */
    i = VMODE1;
  videomode = i;                        /* Set video mode variables */
  *VARVAL(V_VM) = videomode + '0';
  return(0);

} /* setvideomode() */

/* Set number of file lines on tree screen */
LOCAL int setfilelines(n)
  register int n;
{
  if(n < MINFIL || n > MAXFIL)          /* Out of range */
    return(-1);
  maxnflines = n;
  checklines(1);
  return(0);

} /* setfilelines() */

/*
 *      USER COMMAND ROUTINES
 */

/* Build command line s from user command v with value p */
GLOBL int userformat(s, p, v, w)
  register char *s, *p, *w;
  register int v;
{
  char buf[INPLEN];
  register char *fp, *pp;
  register int c, n;

  pp = p;
  n  = 0;
  while(*p) {                   /* Scan command format */
    if(*p == '\\') {
      *s++ = *++p;
      if(*p)
	++p;
    }
    else if(*p == '%') {
      switch(*++p) {
	case 's':               /* Request for parameter */
	case 'S':
	  puthelp("%s: Parameter %d for %s", w, ++n, pp);
	  c = putecho("Give Parameter %d:", n);
	  if((c = getline(buf, sizeof(buf), c, 0, NULL, GNULL, 0)) < RV_NUL)
	    return(c);
	  fp = buf;
	  goto DOCOPY;
	case 'B':               /* Basename of directory or file */
	case 'b':
	  if(v > V_FC0) {
	    if(CNFIL == 0)
	      return(RV_NUL);
	    (void) strcpy(buf, FFNAM(cdlist, CFCUR));
	  }
	  else
	    (void) strcpy(buf, CFNAM);
	  if(fp = strrchr(buf, '.'))
	    *fp = '\0';
	  fp = buf;
	  goto DOCOPY;
	case 'F':               /* Filename of directory or file */
	case 'f':
	  if(v > V_FC0) {
	    if(CNFIL == 0)
	      return(RV_NUL);
	    fp = FFNAM(cdlist, CFCUR);
	  }
	  else
	    fp = CFNAM;
	  goto DOCOPY;
	case 'R':               /* Root directory */
	case 'r':
	  fp = rootdir;
	  goto DOCOPY;
	case 'H':               /* Home directory */
	case 'h':
	  fp = home;
	  goto DOCOPY;
	case 'P':               /* Full pathname of directory or file */
	case 'p':
	  if(v > V_FC0) {
	    if(CNFIL == 0)
	      return(RV_NUL);
	    fp = CPNAM;
	    while(*fp)
	      *s++ = *fp++;
	    *s++ = '/';
	    fp = FFNAM(cdlist, CFCUR);
	    goto DOCOPY;
	  }
	  /* Directory: fall thru */
	case 'D':               /* Current directory name */
	case 'd':
	  fp = CPNAM;
DOCOPY:   while(*fp)
	    *s++ = *fp++;
	  break;
	case '%':               /* As it is */
	  *s++ = '%';
	  break;
	default:
	  return(RV_NUL);       /* Bad format */
      }
      ++p;
    }
    else
      *s++ = *p++;
  }

  *s = '\0';                    /* Terminate command line */
  return(RV_OK);

} /* userformat() */

/* Execute an user defined command v */
GLOBL int usercommand(v)
  register int v;
{
  char buf[EXECLEN];
  register int c;

  /* Check if usercommand is set */
  if( !VARSET(v))
    return(errequest("User command", "Not defined"));
  /* Build command line */
  else {
    c = userformat(buf, VARVAL(v), v, VARNAM(v));
    if(c == RV_NUL)
      return(errequest("User command", "Bad format"));
    else if(c < 0)
      return(c);
  }

  puthelp("USER COMMAND: %s", buf);
  c = callsystem(buf, 1, 0);
  checkdlist(CPNAM);

  if(c != RV_OK)
    return(errequest("User command", "Error in executing"));
  return(hitakey("Return from user command (Hit a key)", lines-1, DA_REVERSE));

} /* usercommand() */

/*
 *      FILE TYPE COMMAND ROUTINES
 */

/* Set file type command */
GLOBL xlist *setcommand(type, f)
  register char *type;
  register int f;
{
  register xlist *xp, *pp, *p;
  register char *comd, *comm;

  /* Search for filetype - command separator ':' */
  if((comd = strchr(type, ':')) == NULL || comd == type)
    return(XNULL);
  else
    *comd++ = '\0';

  /* Get additional comment lead in by '#' */
  if(comm = strchr(comd, '#')) {
    *comm++ = '\0';
    comm = strclean(comm);
  }
  comd = strclean(comd);

  /* Search for file type to set in command list */
  for(xp = xroot; xp; xp = (xlist *) XNEXT(xp))
    if(EQU(type, XTYPE(xp)))
      break;

  /* Replace or delete an existing file type command */
  if(xp) {
    ufree(XCOMD(xp));
    ufree(XCOMM(xp));
    if(comd == NULL || *comd == '\0') {
      if(xroot && xp == xroot) {
	if(xroot = (xlist *) XNEXT(xp))
	  XPREV(xroot) = GNULL;
	else
	  xroot = XNULL;
      }
      else {
	if(p = (xlist *) XPREV(xp))
	  XNEXT(p) = XNEXT(xp);
	if(p = (xlist *) XNEXT(xp))
	  XPREV(p) = XPREV(xp);
      }
      ufree(XTYPE(xp));
      ufree(xp);
      xp = XUNDEF;
    }
  }
  /* Ignore invalid definition */
  else if(comd == NULL || *comd == '\0')
    return(XUNDEF);
  /* Create and insert a new entry in file type command list */
  else if(xp = (xlist *) ualloc(1, sizeof(xlist))) {
    XTYPE(xp) = strsav(type);
    if(xroot && CMP(type, XTYPE(xroot)) < 0) {
      for(pp = xroot; XNEXT(pp); pp = (xlist *) XNEXT(pp)) {
	p = (xlist *) XNEXT(pp);
	if(CMP(type, XTYPE(pp)) > 0)
	  break;
      }
      XPREV(xp) = (glist *) pp;
      XNEXT(xp) = XNEXT(pp);
      if(p = (xlist *) XNEXT(pp))
	XPREV(p) = (glist *) xp;
      XNEXT(pp) = (glist *) xp;
    }
    else {
      if(xroot)
	XPREV(xroot) = (glist *) xp;
      XPREV(xp) = GNULL;
      XNEXT(xp) = (glist *) xroot;
      xroot = xp;
    }
  }

  /* Insert command and comment and return */
  if(comd && *comd) {
    XCOMD(xp) = strsav(comd);
    XCOMM(xp) = comm && *comm ? strsav(comm) : NULL;
  }
  if(xp) {
    if(f == VC_CHG)
      vcchg = 1;
    return(xp);
  }
  else
    return(XNULL);

} /* setcommand() */

/* Show and set/unset filetype commands */
GLOBL int commands()
{
  char buf[3*INPLEN], typ[INPLEN], cmd[INPLEN], cmt[INPLEN];
  register xlist *xp = XUNDEF;
  register int c, f;

  who = "SET COMMAND";
  f = 1;
  /* Filetype command loop */
  while(1) {
    cmd[0] = cmt[0] = '\0';
    if(f && ((c = showallcmds()) < RV_NUL || c == 'q'))
      break;
    f = 0;
    if(xp == XUNDEF || xp == XNULL)
      xp = xroot;
    puthelp("%s: Give filetype or type:command (CR:quit)", who);
    c = putecho("Set command:");
    if((c = getline(typ, sizeof(typ), c, 'd', NULL, xp ? XLIST(xp) : GNULL, 1)) != RV_OK)
      break;
    if(strchr(typ, ':'))
      (void) strcpy(buf, typ);
    else {
      puthelp("%s: Give command for %s (CR:unset)", who, typ);
      c = putecho("Command for %s:", typ);
      for(xp = xroot; xp; xp = (xlist *) XNEXT(xp))
	if(EQU(typ, XTYPE(xp)))
	  break;
      if((c = getline(cmd, sizeof(cmd), c, 'd', xp ? XCOMD(xp) : NULL, GNULL, 1)) < RV_NUL)
	break;
      if(c == RV_NUL)
	(void) sprintf(buf, "%s:", typ);
      else if(strchr(cmd, '#'))
	(void) sprintf(buf, "%s:%s", typ, cmd);
      else {
	puthelp("%s: Give comment for %s", who, typ);
	c = putecho("Comment for %s:", typ);
	if((c = getline(cmt, sizeof(cmt), c, 'd', xp ? XCOMM(xp) : NULL, GNULL, 1)) < RV_NUL)
	  break;
	else if(c == RV_OK)
	  (void) sprintf(buf, "%s:%s#%s", typ, cmd, cmt);
	else
	  (void) sprintf(buf, "%s:%s", typ, cmd);
      }
    }
    if((xp = setcommand(buf, VC_CHG)) == XNULL) {
      puthelp("%s %s", who, hitkey);
      if((c = errequest(buf, "Error in setting")) < RV_NUL)
	break;
    }
    else
      f = 1;
  }
  return(c);

} /* commands() */

/*
 *      VARIABLE ROUTINES
 */

/* Init variables */
GLOBL VOID initvariables()
{
  char buf[NAMELEN];
  register FILE *fp;
  register char *ep;
  register int i;

  /* Init user commands menu lines for tree and file screen */
  (void) strcpy(utreemenu, UMENUFORM);
  (void) strcpy(ufilemenu, utreemenu);

  /* First: Get and set variables from environment */
  if(ep = getenv("SHELL"))
    VARDEF(V_SH) = ep;
  if(ep = getenv("EDITOR"))
    VARDEF(V_ED) = ep;
  if(ep = getenv("PAGER"))
    VARDEF(V_PG) = ep;

  /* Second: Initialize and link variables table */
  for(i = 0; VARNAM(i); i++) {
    if(VARDEF(i) == NULL)
      VARDEF(i) = novar;
    VARVAL(i) = VARDEF(i);
  }
  VARNXT(0) = VARLST(1);
  for(i = 1; VARNAM(i+1); i++) {
    VARNXT(i) = VARLST(i+1);
    VARPRV(i) = VARLST(i-1);
  }
  VARPRV(i) = VARLST(i-1);

#ifdef  UTSTART
  /* Third: Get and set variables from startup file */
  if(startup(buf, UTSTART) && (fp = fopen(buf, "r"))) {
    while(fgets(buf, sizeof(buf), fp))
      if(VALID(buf[0])) {
	i = strlen(buf) - 1;
	if(i > 0 && buf[i] == '\n')
	  buf[i] = '\0';
	if(strchr(buf, '='))
	  (void) setvariable(buf, VC_SET);
	else if(strchr(buf, ':'))
	  (void) setcommand(buf, VC_SET);
      }
    (void) fclose(fp);
  }
#endif  /* UTSTART */

  /* Last: Check environment variable UTREE */
  if(ep = getenv("UTREE")) {
    for(i = 0; ep[i]; i++)
      switch(ep[i]) {
	default:                /* Do nothing */
	  break;
	case 'b':               /* No bell */
	  (void) setvariable("BL=", VC_SET);
	  break;
	case 'c':               /* No clock */
	  (void) setvariable("CL=", VC_SET);
	  break;
	case 'g':               /* No graphic chars */
	  (void) setvariable("GC=", VC_SET);
	  break;
	case 'i':               /* Tree indention */
	   if(isdigit(ep[i+1])) {
	     ++i;
	     (void) sprintf(buf, "TI=%c", ep[i]);
	     (void) setvariable(buf, VC_SET);
	   }
	   break;
	case 'n':               /* No tree scan for changes */
	  (void) setvariable("ST=", VC_SET);
	  break;
	case 'o':               /* Omit saving definition changes */
	  (void) setvariable("AS=", VC_SET);
	  break;
	case 'p':               /* File lines on tree screen */
	   if(isdigit(ep[i+1])) {
	     ++i;
	     (void) sprintf(buf, "FL=%c", ep[i]);
	     (void) setvariable(buf, VC_SET);
	   }
	   break;
	case 's':               /* No hardware scrolling */
	  (void) setvariable("TS=", VC_SET);
	  break;
	case 'v':               /* Set video mode */
	   if(isdigit(ep[i+1])) {
	     ++i;
	     (void) sprintf(buf, "VM=%c", ep[i]);
	     (void) setvariable(buf, VC_SET);
	   }
	   break;
	case 'w':               /* No warning about unreadable dirs */
	  (void) setvariable("WD=", VC_SET);
	  break;
      }
  }

} /* initvariables() */

/* Save current settings after changes */
GLOBL VOID savevariables()
{
#ifdef UTSTART
  char buf[NAMELEN];
  register xlist *xp;
  register vlist *vp;
  register FILE *fp;
  time_t t;

  if(VARVAL(V_AS) && vcchg) {
   (void) sprintf(buf, ".%s", UTSTART);
   (void) strcpy(buf, pathname(buf, home));
    if(fp = fopen(buf, "w")) {
      t = time((time_t *) 0);
      (void) fprintf(fp, "# %s: ~/.%s, %s", getversion(), UTSTART, ctime(&t));
      /* Save variables */
      (void) fprintf(fp, "# Variables\n");
      for(vp = VARTAB(0); vp; vp = (vlist *) VNEXT(vp)) {
	(void) fprintf(fp, "%s=", VNAME(vp));
	if(VVALE(vp))
	  (void) fprintf(fp, "%s", VTYPE(vp) == VT_B ? "ON" : VVALE(vp));
	if(VCOMM(vp))
	  (void) fprintf(fp, "\t#%s\n", VCOMM(vp));
	else
	  (void) fprintf(fp, "\n");
      }
      /* Save command settings */
      (void) fprintf(fp, "# Commands\n");
      for(xp = xroot; xp; xp = (xlist *) XNEXT(xp)) {
	(void) fprintf(fp, "%s:%s", XTYPE(xp), XCOMD(xp) ? XCOMD(xp) : "");
	if(XCOMM(xp))
	  (void) fprintf(fp, "\t#%s\n", XCOMM(xp));
	else
	  (void) fprintf(fp, "\n");
      }
      (void) fclose(fp);
    }
  }
#endif  /* UTSTART */

} /* savevariables() */

/* Check tree indention */
GLOBL VOID checkindent()
{
  register int i;

  if(maxlevel > 0)                      /* Calculate max possible indention */
    i = (columns - FNAMSZ - 6) / maxlevel;
  else
    i = indent;
  if(i < MININD) i = MININD;
  if(i > MAXIND) i = MAXIND;
  if(maxindent && i > maxindent)        /* Check and set indention */
    i = maxindent;
  indent = i;
  *VARVAL(V_TI) = indent + '0';         /* Set indention variable value */
  treeflag = SF_TREE;

} /* checkindent() */

/* Calculate max possible number of file lines on tree screen */
GLOBL int calculatelines()
{
  register int l;

  if((l = (lines-3) / 2) > MAXFIL)
    l = MAXFIL;
  return(l);

} /* calculatelines() */

/* Check line partitioning on tree screen */
GLOBL VOID checklines(f)
  register int f;
{
  register int m, nf;

  if(f) {                               /* Calculate new line values */
    nf = nflines;
    if(maxnflines < MINFIL)
      nflines = MINFIL;
    else {
      m = calculatelines();
      nflines = maxnflines > m ? m : maxnflines;
    }
    firstfline = lastfline - nflines + 1;
    lastdline  = firstfline - 2;
    ndlines    = lastdline - firstdline;
    if(nf != nflines)                   /* Check directory window */
      calculatetree(nf-nflines);
    treeflag = SF_TREE;
  }
  else                                  /* Update after resizing */
    maxnflines = nflines;
  *VARVAL(V_FL) = maxnflines + '0';     /* Set number of file lines */

} /* checklines() */

/* Set/unset or reset user defined variables */
GLOBL int setvariable(line, f)
  register char *line;
  register int f;
{
  register vlist *vp;
  register char *value, *comm, *dp;
  register int n, i, j;

  /* Search for variable - value separator '=' */
  if(f != VC_TST) {
    if(value = strchr(line, '='))
      *value++ = '\0';
    else
      return(-1);
  }
  line = strclean(line);
  strupper(line);

  /* Search for variable to set in variable list */
  for(vp = VARTAB(0); vp; vp = (vlist *) VNEXT(vp))
    if(*line == *VNAME(vp) && (EQU(line, VSCUT(vp)) || EQU(line, VNAME(vp))))
      break;
  if(vp == VNULL)
    return(-1);
  else if(f == VC_TST)
    return(VNUMB(vp));

  /* Ignore additional comment */
  if(comm = strchr(value, '#')) {
    *comm++ = '\0';
    comm = strclean(comm);
  }
  value = strclean(value);

  /* Set or unset variable */
  n = VNUMB(vp);
  switch(VTYPE(vp)) {
    case VT_B:                  /* Booleans */
      VVALE(vp) = *value ? VB_ON : VB_OFF;
      if(n == V_GC)
	initgraphics((int) VVALE(vp));
      else if(n == V_LS)
	setsortflag((int) VVALE(vp));
#ifdef  USEANSICOLORS
      else if(n == V_UC)
	setcolorflag((int) VVALE(vp));
#endif  /* USEANSICOLORS */
      break;
    case VT_N:                  /* Numbers */
      if(n == V_TI && setindent(atoi(value)))
	n = -1;
      else if(n == V_VM && setvideomode(atoi(value)))
	n = -1;
      else if(n == V_FL && setfilelines(atoi(value)))
	n = -1;
      else if(n == V_HS && (f != VC_SET || sethistorysize(atoi(value))))
	n = -1;
      break;
    case VT_S:                  /* General strings */
      if(VVALE(vp) && VVALE(vp) != VDFLT(vp))
	ufree(VVALE(vp));
      if(*value && *value != '#' && (dp = strsav(value)))
	VVALE(vp) = dp;
      else if(VDFLT(vp))
	VVALE(vp) = VDFLT(vp);
      else
	VVALE(vp) = NULL;
      break;
    case VT_U:                  /* Command strings */
      if(VVALE(vp) && VVALE(vp) != VDFLT(vp))
	ufree(VVALE(vp));
      if(VCOMM(vp))
	ufree(VCOMM(vp));
      VVALE(vp) = VCOMM(vp) = NULL;
      dp = NULL;
      if(*value) {
	if(dp = strsav(value))
	  VVALE(vp) = dp;
	if(comm && *comm)
	  VCOMM(vp) = strsav(comm);
      }
      /* Update user command menulines if needed */
      if(n > V_FC0)
	for(i = (n - V_FC1) * OFFSET + FIRST, j = 0; j < TITLE; i++, j++)
	  ufilemenu[i] = comm && *comm ? *comm++ : ' ';
      else
	for(i = (n - V_TC1) * OFFSET + FIRST, j = 0; j < TITLE; i++, j++)
	  utreemenu[i] = comm && *comm ? *comm++ : ' ';
      break;
    case VT_O:                  /* User defined strings: not yet! */
    default:                    /* ??? */
      n = -1;
  }

  if(n >= 0 && f == VC_CHG)
    vcchg = 1;
  return(n);

} /* setvariable() */

/* Show and set/unset common and user defined variables */
GLOBL int variables()
{
  char buf[3*INPLEN], var[INPLEN], def[INPLEN], cmt[INPLEN];
  register char *dp;
  register int f, c, n = -1;

  who = "SET VARIABLE";
  /* Variable loop */
  f = 1;
  while(1) {
    if(n < 0)
      n = 0;
    if(f && ((c = showallvars()) < RV_NUL || c == 'q'))
      return(c);
    f = 0;
    puthelp("%s: Give variable or variable=value (CR:quit)", who);
    c = putecho("Set variable:");
    if((c = getline(var, sizeof(var), c, 'v', NULL, VARLST(n), 1)) != RV_OK)
      break;
    if(strchr(var, '='))
      (void) strcpy(buf, var);
    else {
      if((n = setvariable(var, VC_TST)) < 0) {
	if((c = errequest(var, "Unknown")) < RV_NUL)
	  break;
	else
	  continue;
      }
      puthelp("%s: Give definition for %s (CR:unset)", who, var);
      c = putecho("Set %s to:", var);
      if(VARTYP(n) == VT_B)
	dp = VARSET(n) ? "ON" : "";
      else
	dp = VARVAL(n);
      if((c = getline(def, sizeof(def), c, 'v', dp, GNULL, 0)) < RV_NUL)
	break;
      if(VARTYP(n) != VT_U || strchr(def, '#'))
	(void) sprintf(buf, "%s=%s", var, def);
      else {
	puthelp("%s: Give comment for user command %s", who, VARNAM(n));
	c = putecho("Comment for %s:", VARNAM(n));
	if((c = getline(cmt, sizeof(cmt), c, 'v', VARCOM(n), GNULL, 0)) < RV_NUL)
	  break;
	(void) sprintf(buf, "%s=%s#%s", var, def, cmt);
      }
    }
    if(setvariable(buf, VC_CHG) < 0) {
      puthelp("%s %s", who, hitkey);
      if((c = errequest(buf, "Error in setting")) < RV_NUL)
	break;
    }
    else
      f = 1;
  }

  treeflag = fileflag = SF_FULL;
  return(c);

} /* variables() */
