/*
 *      HIST.C
 *      UTREE shell escape, shell execution and command history.
 *      3.03-um klin, Tue Feb 11 22:35:55 1992, Derived from comm.c and
 *                                              shell command history added
 *              klin, Sat Feb 15 14:44:52 1992, Video handling changed
 *              klin, Sun Feb 23 18:45:19 1992, Keybindings and variable
 *                                              AUTOSAVE added
 *              klin, Fri Mar  6 07:24:57 1992, Minor changes
 *            a klin, Sun Mar 15 19:08:25 1992, Search in history list added
 *            c klin, Mon Mar 30 14:24:30 1992, Fixes in history list handling
 *            d klin, Thu Apr  2 09:07:41 1992, Time stamps for history added
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03d-um (klin) Apr  2 1992 hist.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

LOCAL jmp_buf intrjump;         /* Buffer for longjump                  */
LOCAL clist *chcurr = CHNULL;   /* Current entry in history list        */
LOCAL int chsiz = 0;            /* Size of history list                 */
LOCAL int chmax = HLDEF;        /* Max size of history list             */
LOCAL int chnum = 0;            /* Event number                         */
LOCAL int hlchg = 0;            /* History list changed                 */

/* ---- External variables and functions ------------------------------ */

EXTRN char *getversion();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Catch signals SIGINT and SIGQUIT */
LOCAL int oninterrupt()
{
  (void) signal(SIGINT,  SIG_IGN);
  (void) signal(SIGQUIT, SIG_IGN);
  longjmp(intrjump, RV_INT);
  /*NOTREACHED*/

} /* oninterrupt() */

/* Insert command cmd into history list */
LOCAL clist *inserthistory(cmd, f)
  register char *cmd;
  register int f;
{
  register clist *cp, *p;

  /* Check if command is already in history list */
  if(cp = chcurr) {
    do {
      if(EQU(cmd, CHCMD(cp))) {
	++CHCNT(cp);
	CHTIM(cp) = time((time_t *) 0);
	if(cp != chcurr) {
	  p = (clist *) CHPRV(cp);
	  CHNXT(p) = CHNXT(cp);
	  p = (clist *) CHNXT(cp);
	  CHPRV(p) = CHPRV(cp);
	  p = (clist *) CHNXT(chcurr);
	  CHPRV(p) = CHNXT(chcurr) = (glist *) cp;
	  CHPRV(cp) = (glist *) chcurr;
	  CHNXT(cp) = (glist *) p;
	  chcurr = cp;
	}
	return(cp);
      }
      cp = (clist *) CHNXT(cp);
    } while(cp != chcurr);
  }

  /* Create root of history list */
  if(chsiz == 0) {
    chcurr = (clist *) ualloc(1, sizeof(clist));
    CHCMD(chcurr) = strsav(cmd);
    CHLEN(chcurr) = strlen(cmd);
    CHPRV(chcurr) = CHNXT(chcurr) = (glist *) chcurr;
    CHNUM(chcurr) = ++chnum;
    CHCNT(chcurr) = f ? 1 : 0;
    CHTIM(chcurr) = f ? time((time_t *) 0) : (time_t) 0;
    ++chsiz;
  }
  /* Create new entry in history list */
  else if(chsiz < chmax) {
    cp = (clist *) ualloc(1, sizeof(clist));
    CHCMD(cp) = strsav(cmd);
    CHLEN(cp) = strlen(cmd);
    CHNUM(cp) = ++chnum;
    CHCNT(cp) = f ? 1 : 0;
    CHTIM(cp) = f ? time((time_t *) 0) : (time_t) 0;
    CHPRV(cp) = (glist *) chcurr;
    CHNXT(cp) = CHNXT(chcurr);
    p = (clist *) CHNXT(chcurr);
    CHPRV(p) = (glist *) cp;
    CHNXT(chcurr) = (glist *) cp;
    chcurr = cp;
    ++chsiz;
  }
  /* Overwrite existing entry = the next in the chain */
  else {
    chcurr = (clist *) CHNXT(chcurr);
    if(CHLEN(chcurr) < strlen(cmd)) {
      ufree(CHCMD(chcurr));
      CHCMD(chcurr) = strsav(cmd);
      CHLEN(chcurr) = strlen(cmd);
    }
    else
      (void) strcpy(CHCMD(chcurr), cmd);
    CHNUM(chcurr) = ++chnum;
    CHCNT(chcurr) = f ? 1 : 0;
    CHTIM(chcurr) = f ? time((time_t *) 0) : (time_t) 0;
  }

  /* Update changed flag and return */
  if(f)
    hlchg = 1;
  return(chcurr);

} /* inserthistory() */

/* Display history list */
LOCAL int showhistory()
{
  register clist *fp, *cp;
  register int l, c, n;

  l = firstline;
  c = RV_OK;
  clearwindow(firstline, lastline);
  fp = cp = (clist *) CHNXT(chcurr);
  do {
    n = putfxy(0, l, 0, "%-5d %s %s", CHNUM(cp), cp == chcurr ? "->" : "  ", CHCMD(cp));
    (void) putfxy(n > columns/2 ? n : columns/2, l, 0, "#%-4d %s", CHCNT(cp), CHTIM(cp) ? ctime(&CHTIM(cp)) : "?");
    cp = (clist *) CHNXT(cp);
    if(++l > lastline && cp != fp) {
      puthelp("COMMAND HISTORY (CR:continue  Q:quit  ELSE:give command)");
      c = hitakey("More commands ?", echoline, DA_NONE);
      if( !(c == ' ' || c == '\n'))
	break;
      clearwindow(firstline, lastline);
      l = firstline;
    }
  } while(cp != fp);

  treeflag = fileflag = SF_FULL;
  return(c);

} /* showhistory() */

/* Execute a shell command */
LOCAL int callcommand(w)
  register int w;
{
  char buf[EXECLEN];
  register clist *cp, *lp;
  register int c, rv, f;

  who = "SHELL";
  if(chcurr)
    puthelp("%s: Give command line or select from history (CR:quit)", who);
  else
    puthelp("%s: Give command line (CR:quit)", who);
  /* Get command loop. Handle command given by history number */
  f = 1;
  buf[0] = '\0';
  lp = chcurr;
  while(1) {
    c = putecho("Command:");
    c = getline(buf, sizeof(buf), c, 'l', buf, lp ? CHLST(lp) : GNULL, f);
    if(c == RV_OK && buf[0] == '!') {
      if(cp = lp) {
	if(buf[1] == '!' && lp) {                       /* Recall last command */
	  (void) strcpy(buf, CHCMD(lp));
	  continue;
	}
	else if(buf[1] >= '1' && buf[1] <= '9') {       /* Search for number */
	  c = atoi(&buf[1]);
	  do {
	    if(CHNUM(cp) == c) {                        /* Found */
	      (void) strcpy(buf, CHCMD(cp));
	      lp = cp;
	      f = 0;
	      break;
	    }
	    cp = (clist *) CHPRV(cp);
	  } while(cp != lp);
	  continue;
	}
	else {                                          /* Search for string */
	  (void) strcat(buf, "*");
	  do {
	    if(match(CHCMD(cp), &buf[1])) {             /* Found */
	      (void) strcpy(buf, CHCMD(cp));
	      lp = cp;
	      f = 0;
	      break;
	    }
	    cp = (clist *) CHPRV(cp);
	  } while(cp != lp);
	  continue;
	}
      }
      continue;
    }
    else
      break;
  }

  /* Command given: insert into history list and execute */
  if(c == RV_OK) {
    cp = inserthistory(buf, 1);
    if(strchr(buf, '%')) {
      c = userformat(buf, CHCMD(cp), w, "SHELL");
      if(c == RV_NUL) {
	puthelp("%s %s", who, hitkey);
	return(errequest("SHELL", "Bad format"));
      }
      else if(c != RV_OK)
	return(c);
    }
    puthelp("%s: %s", who, buf);
    rv = callsystem(buf, 1, 0);
    treeflag = fileflag = SF_FULL;
  }
  else
    return(c);

  if(rv == RV_INT)
    return(errequest("Shell escape", "Interrupted"));
  return(hitakey("Return from shell (Hit a key)", lines-1, DA_REVERSE));

} /* callcommand() */

/* Call shell interactively */
LOCAL int callshell()
{
  register int pid, rpid;
  int status;

  /* Ignore signals SIGINT and SIGQUIT */
  (void) signal(SIGINT,  SIG_IGN);
  (void) signal(SIGQUIT, SIG_IGN);
  /* Clear screen and reset terminal to cooked mode */
  clearscreen();
  setvideo(DA_REVERSE);
#define LINE \
"SHELL ESCAPE: Hit \'EOF character\' or \'exit\' to return to utree"
  bell(VARSET(V_BL));
  (void) putsxy(0, 0, LINE);
  while(putchar(' '))
    ;
#undef  LINE
  setvideo(DA_NORMAL);
  cursorset(CF_VISIBLE);
  terminalreset(0);
  /* Fork child process */
#if     defined(BSD) || defined(HASVFORK)
  if((pid = vfork()) < 0) {
#else   /* SYSV && !HASVFORK */
  if((pid = fork()) < 0) {
#endif  /* BSD || HASVFORK */
    (void) errequest(prgname, "Cannot fork");
    status = 0;
  }
  /* Start the shell in child process */
  else if(pid == 0) {
    /* Reset signal interrupt and quit */
    (void) signal(SIGINT,  SIG_DFL);
    (void) signal(SIGQUIT, SIG_DFL);
    /* Execute interactive shell */
    (void) execlp(VARVAL(V_SH), VARVAL(V_SH), "-i", NULL);
    _exit(1);
  }
  /* Parent: wait for return from shell */
  else {
    while((rpid = wait(&status)) != -1 && rpid != pid)
      ;
    status >>= 8;
  }
  /* Reset terminal to raw mode und set screen flags */
  cursorset(CF_INVISIBLE);
  terminalraw(0);
  treeflag = fileflag = SF_FULL;

  return(status);

} /* callshell() */

/*
 *      COMMON USED COMMAND EXECUTION
 */

/* Execute a command, return exit code */
GLOBL int callsystem(cmd, c, s)
  register char *cmd;
  register int c, s;
{
  register int pid, rpid, rv;
  int status;

  if(c) {
    /* Reset terminal to cooked mode */
    if(s)
      clearscreen();
    else
      clearwindow(helpline + 1, lines - 1);
    cursorset(CF_VISIBLE);
    terminalreset(0);
  }
  else
    /* Turn on signal handling */
    enablesignals();
  flushout();
  /* Fork child process */
#if     defined(BSD) || defined(HASVFORK)
  if((pid = vfork()) < 0) {
#else   /* SYSV && !HASVFORK */
  if((pid = fork()) < 0) {
#endif  /* BSD || HASVFORK */
    (void) errequest(prgname, "Cannot fork");
    status = 0;
  }
  /* Child process */
  else if(pid == 0) {
    /* Reset signals interrupt and quit for child */
    (void) signal(SIGINT,  SIG_DFL);
    (void) signal(SIGQUIT, SIG_DFL);
    if( !c) {
      (void) close(0);          /* Close stdin  */
      (void) close(1);          /*       stdout */
      (void) close(2);          /*   and stderr */
    }
    /* Let the shell execute the command line */
    (void) execlp(SHELL, SHELL, "-c", cmd, NULL);
    _exit(1);
  }
  /* Parent: wait for termination of child */
  else {
    /* Set up here if an interrupt was catched */
    if( !(rv = setjmp(intrjump))) {
      /* Catch signals interrupt and quit */
      (void) signal(SIGINT,  (SIGNL(*)()) oninterrupt);
      (void) signal(SIGQUIT, (SIGNL(*)()) oninterrupt);
    }
    while((rpid = wait(&status)) != -1 && rpid != pid)
      ;
    if(rv != RV_INT)
      rv = status >> 8 ? RV_NUL : RV_OK;
  }
  if(c) {
    /* Set terminal to raw mode and set screen flags */
    terminalraw(0);
    treeflag = fileflag = SF_FULL;
  }
  else
    /* Turn off signal handling */
    disablesignals();

  cursorset(CF_INVISIBLE);
  flushout();
  return(rv);

} /* callsystem() */

/*
 *      COMMAND HISTORY
 */

/* Set size of history list */
GLOBL int sethistorysize(n)
  register int n;
{
  static char buf[4];

  if(n < HLMIN || n > HLMAX)
    return(1);
  chmax = n;
  (void) sprintf(buf, "%d", n);
  VARDEF(V_HS) = VARVAL(V_HS) = buf;
  return(0);

} /* sethistorysize() */

/* Init history list */
GLOBL VOID inithistory()
{
#ifdef  UTHIST
  char name[NAMELEN], cmd[INPLEN];
  register FILE *fp;
  register int l;

  (void) strcpy(name, pathname(UTHIST, home));
  if(fp = fopen(name, "r")) {
    while(fgets(cmd, sizeof(cmd), fp)) {
      if(VALID(cmd[0])) {
	l = strlen(cmd) - 1;
	if(cmd[l] == '\n')
	  cmd[l] = '\0';
	(void) inserthistory(cmd, 0);
      }
    }
    (void) fclose(fp);
  }
#endif  /* UTHIST */

} /* inithistory() */

/* Save history list to history file in $HOME */
GLOBL VOID savehistory()
{
#ifdef  UTHIST
  char name[NAMELEN];
  register FILE *fp;
  register clist *cp;
  time_t t;

  if(VARVAL(V_AS) && chcurr && hlchg) {
    (void) strcpy(name, pathname(UTHIST, home));
    if(fp = fopen(name, "w")) {
      t  = time((time_t *) 0);
      (void) fprintf(fp, "# %s: ~/%s, %s", getversion(), UTHIST, ctime(&t));
      cp = (clist *) CHNXT(chcurr);
      do {
	(void) fprintf(fp, "%s\n", CHCMD(cp));
	 cp = (clist *) CHNXT(cp);
      } while(cp != (clist *) CHNXT(chcurr));
      (void) fclose(fp);
    }
  }
#endif  /* UTHIST */

} /* savehistory() */

/* Display history list, execute shell or command  */
GLOBL int history(c, w)
  register int c, w;
{
  if(c == '$')                  /* Execute interactive shell */
    return(callshell());

  do {                          /* Sehll command loop */
    if(chcurr) {                /* Display history list */
      if((c = showhistory()) < RV_NUL || c == 'q')
	break;
    }
    c = callcommand(w);         /* Execute command */
  } while(c > RV_NUL);
  return(c);

} /* history() */

