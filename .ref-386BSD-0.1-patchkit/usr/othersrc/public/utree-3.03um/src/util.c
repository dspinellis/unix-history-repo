/*
 *      UTIL.C
 *      UTREE utility functions.
 *      3.01-um klin, Sat Apr 20 11:02:33 1991
 *              klin, Tue Oct 15 14:02:37 1991, Handling of symlinks changed
 *      3.02-um klin, Fri Nov  1 10:46:14 1991, APOLLO stuff added
 *      3.03-um klin, Tue Feb 11 22:58:03 1992, statusfile() into stat.c
 *                                              getopt() into sup/getopt.c
 *              klin, Fri Mar  6 10:45:49 1992, strclean() added
 *
 *      Copyright (c) 1991/92 Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03-um (klin) Mar  6 1992 util.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

#define TRASH(c) ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '#')

/* ---- Functions and procedures -------------------------------------- */

/*
 *      COMMON USED UTILITIY FUNCTIONS
 *
 */

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/* !  This function is a modified version of adjustname() `stolen'    ! */
/* !  from the public domain editor MICRO GNU EMACS (mg) version 2a.  ! */
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

/* Return pathname of filename fn and current directory cwd */
GLOBL char *pathname(fn, cwd)
  register char *fn, *cwd;
{
  static char pn[NAMELEN];
  register char *cp;

  if(*fn == '/') {
    cp = pn;
#ifdef  APOLLO                  /* Stuff for APOLLO node names: //node */
    if(fn[1] == '/')
      *cp++ = *fn++;
#endif  /* APOLLO */
    while(fn[1] == '/')
      ++fn;
#ifdef  MUNET                   /* Stuff for MUNIX/NET: /../node */
    if(fn[1] == '.' && fn[2] == '.') {
      *cp++ = *fn++;
      *cp++ = *fn++;
    }
#endif  /* MUNET */
    *cp++ = *fn++;
  }
  else {
    (void) strcpy(pn, cwd);
    cp = pn + strlen(pn);
  }
  if(cp != pn && cp[-1] != '/')
    *cp++ = '/';
  while(*fn) {
    switch(*fn) {
      case '.':
	switch(fn[1]) {
	  case '\0':
	    *--cp = '\0';
	    return(pn);
	  case '/':
	    fn += 2;
	    continue;
	  case '.':
	    if(fn[2]=='/' || fn[2] == '\0') {
	      --cp;
	      while(cp > pn && *--cp != '/')
		;
	      ++cp;
	      if(fn[2]=='\0') {
		*--cp = '\0';
		return(pn);
	      }
	      fn += 3;
	      continue;
	    }
	    break;
	  default:
	    break;
	}
	break;
      case '/':
	fn++;
	continue;
      default:
	break;
    }
    while(*fn && (*cp++ = *fn++) != '/')
      ;
  }
#ifdef  APOLLO
  if(cp != &pn[2] && cp[-1] == '/')
#else   /* !APOLLO */
  if(cp != &pn[1] && cp[-1] == '/')
#endif  /* APOLLO */
    --cp;
  *cp = '\0';

  return(pn);

} /* pathname() */

/* Return basename of filename s */
GLOBL char *basename(s)
  register char *s;
{
  register char *sp;

  sp = s + strlen(s);
  while(sp >= s)
    if(*sp == '/')
      return(*(++sp) ? sp : s);
    else
      --sp;
  return(s);

} /* basename() */

/* Save string s */
GLOBL char *strsav(s)
  register char *s;
{
  register char *sp;

  if(sp = ualloc((unsigned) strlen(s) + 1, sizeof(char)))
    (void) strcpy(sp, s);
  return(sp);

} /* strsav() */

/* Convert string from lower to upper case */
GLOBL VOID strupper(s)
  register char *s;
{
  while(*s) {
    if(islower(*s))
      *s = toupper(*s);
    ++s;
  }

} /* strupper() */

/* Delete leading and trailing trash characters from s */
GLOBL char *strclean(s)
  register char *s;
{
  register char *p;

  while(*s && TRASH(*s))
    ++s;
  p = s + strlen(s);
  while(p > s && TRASH(*(p-1)))
    --p;
  *p = '\0';
  return(s);

} /* strclean() */

/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/* !   This function is a modified and tuned version of a wild card   ! */
/* !   pattern matcher written by Rich Salz (mirror!rs, Nov 26 1986)  ! */
/* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */

/* Shell like pattern matching for meta characters *, ?, [class] */
/* Check string s against pattern p and return 1 on match 0 else */
GLOBL int match(s, p)
  register char *s, *p;
{
  register int lc, ok, ex;

  for( ; *p; s++, p++) {
    switch(*p) {
      case '\\':                /* Literal match next char */
	++p;
	/*FALLTHRU*/
      default:                  /* Literal match char */
	if(*s != *p)
	  return(0);
	continue;
      case '?':                 /* Match any char */
	if(*s == '\0')
	  return(0);
	continue;
      case '*':                 /* Match any chars */
	if(*++p == '\0')        /* Matches all */
	  return(1);
	for( ; *s; s++)
	  if(match(s, p))
	    return(1);
	return(0);
      case '[':                 /* Class */
	if(ex = (p[1] == '^' || p[1] == '!'))
	  ++p;
	for(lc = 0400, ok = 0; *++p && *p != ']'; lc = *p)
	  if(*p == '-' ? *s <= *++p && *s >= lc : *s == *p)
	    ok = 1;
	if(ok == ex)
	  return(0);
	continue;
    }
  }
  return(*s == '\0');

} /* match() */

/* Get and return next valid filename entry from directory file d */
GLOBL char *readdname(dp)
  register DIR *dp;
{
#if     defined(BSD) || !defined(NODIRENT)
#ifdef  BSD
  register struct direct *d;
#else   /* SYSV */
  register struct dirent *d;
#endif  /* BSD */
  register char *f;

  /* Get next valid directory entry and return filename */
  while(d = readdir(dp)) {
    f = d->d_name;
    /* Skip "." and ".." */
    if(f[0] == '.' && (f[1] == '\0' || (f[1] == '.' && f[2] == '\0')))
      continue;
    return(f);
#else   /* SYSV && NODIRENT */
  static char n[DIRSIZ+1];
  struct direct d;
  register char *f;

  while(fread(&d, sizeof(struct direct), 1, dp) > 0) {
    f = d.d_name;
    /* Skip removed files, "." and ".." */
    if(d.d_ino == 0 || (f[0] == '.' && (f[1] == '\0' || (f[1] == '.' && f[2] == '\0'))))
      continue;
    (void) strncpy(n, d.d_name, DIRSIZ);
    n[DIRSIZ] = '\0';
    return(n);
#endif  /* BSD || !NODIRENT */
  }
  return(NULL);

} /* readdname() */
