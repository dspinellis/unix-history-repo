/*
 *      HELP.C
 *      UTREE help routines.
 *      3.01-um klin, Sat Apr 20 11:02:33 1991
 *      3.03-um klin, Sat Feb 15 18:34:27 1992, Minor changes
 *            c klin, Mon Mar 30 11:10:00 1992, Minor changes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03-um (klin) Mar 30 1992 help.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

LOCAL hlist *hroot = HNULL;     /* Root of help pages                   */
LOCAL char *helpmenu = NULL;    /* Help menuline                        */
LOCAL char helpfile[NAMELEN];   /* Help file name                       */

#define LINELEN 128             /* Max line length for help pages       */

/* ---- External variables and functions ------------------------------ */

EXTRN long ftell();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Insert a help page for topic s into help page list for file fp */
LOCAL VOID inserthelp(s, fp)
  register char *s;
  register FILE *fp;
{
  char buf[LINELEN];
  register hlist *hp, *np;
  register char *i, k;
  register long p;
  register int n;

  p = ftell(fp);                /* Get position of topic in help file */
  i = s;                        /* and menu line item */
  while(*s && !(*s == ' ' || *s == '\t' || *s == '\n'))
    ++s;
  *s = '\0';
  /* Count number of lines for this help page */
  for(n = 0; fgets(buf, sizeof(buf), fp); n++)
    if(buf[0] == '#' && buf[1] == '@')
      break;
  if(*i && p > 0 && n > 0) {    /* Get the hotkey for help menu topic */
    s = i;
    k = *i;
    while(*s) {
      if(*s >= 'A' && *s <= 'Z') {
	k = *s;
	break;
      }
      ++s;
    }
    /* Get space for help page, fill up data and insert into list */
    hp = (hlist *) ualloc(1, sizeof(hlist));
    (void) strncpy(HITEM(hp), i, ITEMLEN-1);
    HHKEY(hp) = isupper(k) ? tolower(k) : k;
    HSPOS(hp) = p;
    HNLIN(hp) = n;
    HNEXT(hp) = HNULL;
    if(hroot == HNULL)
      hroot = hp;
    else {
      for(np = hroot; HNEXT(np); np = HNEXT(np))
	;
      HNEXT(np) = hp;
    }
  }

} /* inserthelp() */

/* Show help page hp from help file fp */
LOCAL int showhelppage(hp, fp)
  register hlist *hp;
  register FILE *fp;
{
  char buf[LINELEN];
  register int n, l, c;

  puthelp("%s: About %s (Q:quit  ELSE:helpmenu)", who, HITEM(hp));
  clearwindow(firstline, lastline);
  flushout();
  c = RV_OK;
  for(n = 0, l = firstline; n < HNLIN(hp); ) {
    if(fgets(buf, sizeof(buf), fp))
      (void) putsxy(0, l, buf);
    else
      break;
    ++n;
    ++l;
    if(l > lastfline && n < HNLIN(hp)) {
      puthelp("%s: About %s (CR:continue  Q:quit  ELSE:helpmenu)", who, HITEM(hp));
      c = hitakey("Help:", echoline, DA_NONE);
      puthelp("%s: About %s (Q:quit  ELSE:help)", who, HITEM(hp));
      if( !(c == ' ' || c == '\n'))
	break;
      l = firstline;
      clearwindow(firstline, lastline);
      flushout();
    }
  }
  if(c == 'q' || c < RV_NUL)
    return(c);
  puthelp("%s: About %s %s", who, HITEM(hp), hitkey);
  (void) putecho("Help about %s done", HITEM(hp));
  return(hitakey(NULL));

} /* showhelppage() */

/* Utree help pages are contained in a help file. Each help page is     */
/* enclosed in a pair of lines '#@item' and '#@' which signal start and */
/* end of the help page to topic 'item'. This item is also copied into  */
/* the help menuline and the first character or the first upper case    */
/* character from item if found is used as hot key for selecting the    */
/* help page to this topic. The initialization routine scans the help   */
/* file and builds up a list of available help pages from this file.    */
GLOBL VOID inithelp()
{
  char buf[NAMELEN];
  register FILE *fp;
  register hlist *hp;
  register int l, i;

#ifdef  UTHELP
  if(startup(buf, UTHELP) && (fp = fopen(buf, "r"))) {
    (void) strcpy(helpfile, buf);
    /* First read help file and insert help pages into list */
    while(fgets(buf, sizeof(buf), fp))
      if(buf[0] == '#' && buf[1] == '@' && buf[2])
	inserthelp(&buf[2], fp);
    /* Second build from help items the help menu line */
    l = columns - 4;
    helpmenu = ualloc((unsigned) l, sizeof(char));
    for(hp = hroot, i = 0; hp; hp = HNEXT(hp)) {
      i += strlen(HITEM(hp)) + 1;
      if(i >= l)
	break;
      (void) strcat(helpmenu, " ");
      (void) strcat(helpmenu, HITEM(hp));
    }
    (void) fclose(fp);
    if((i + 5) < l)
      (void) strcat(helpmenu, " Quit");
  }
#endif

} /* inithelp() */

/* Display help menu and help pages */
GLOBL int showhelp(k)
  register int k;
{
  register FILE *fp;
  register hlist *hp;
  register int c;

  who = "HELP";
  if(hroot == HNULL || !(fp = fopen(helpfile, "r"))) {
    puthelp("%s %s", who, hitkey);
    return(errequest("Help", "Not available"));
  }

  /* Help menu loop */
  do {
    if(k) {
      c = k;
      k = 0;
    }
    else {
      if( !keypressed()) {
	putmenu("HELP:", helpmenu);
	(void) putecho("Help about which topic:");
      }
      if((c = hitakey(NULL)) == 'q' || c < RV_NUL)
	break;
    }
    for(hp = hroot; hp && HHKEY(hp) != c; hp = HNEXT(hp))
      ;
    if(hp == HNULL || fseek(fp, HSPOS(hp), 0))
      break;
    c = showhelppage(hp, fp);
    treeflag = fileflag = SF_FULL;
  } while(c != 'q' && c > RV_NUL);

  /* Close help file and return */
  (void) fclose(fp);
  if(c == 'q' || c < RV_NUL)
    return(c);
  return(putversion(echoline, "HELP: Done"));

} /* showhelp() */
