/*
 *      LIST.C
 *      UTREE directory and file list handling routines.
 *      3.01-um klin, Tue Jun  4 14:17:17 1991
 *              klin, Tue Oct 15 14:02:37 1991, Handling of symlinks changed
 *              klin, Sat Oct 26 15:07:06 1991, Print tree list added
 *                                              writedlist() changed
 *      3.02-um klin, Fri Nov  1 13:41:58 1991, Minor changes
 *              klin, Sun Nov 10 19:37:14 1991, buildlist() changed
 *              klin, Sun Nov 24 11:44:49 1991, Some error fixes reported by
 *                                              Rolf Gebhardt (RG 11/22/91)
 *      3.03-um klin, Sat Feb 15 20:09:09 1992, Minor changes
 *            e klin, Sat Apr 11 11:45:01 1992, Bug fix in newflist()
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 list.c";
#endif  /* !lint */

#include "defs.h"

/* ---- External variables and functions ------------------------------ */

EXTRN char *readdname();
EXTRN char *getversion();

/* ---- Local/global functions and procedures --------------------------*/

/*
 *      INTERNAL USED ROUTINES
 */

/* Compare filenames from flist. Called by qsort() */
LOCAL int flistcmp(f1, f2)
  register flist *f1, *f2;
{
  if(sortbytime) {
    if(FPTIM(f1) < FPTIM(f2))
      return(99);
    else if(FPTIM(f1) > FPTIM(f2))
      return(-99);
  }
  return(CMP(FPFIL(f1), FPFIL(f2)));

} /* flistcmp() */

/* Compare filenames from directory name vector. Called by qsort() */
LOCAL int dlistcmp(d1, d2)
  register char **d1, **d2;
{
  return(CMP(*d1, *d2));

} /* dlistcmp() */

/* Count directory level */
LOCAL int countlevl(name)
  register char *name;
{
  register int n;

  if(*name == '/' && *(name+1) == '\0')
    return(0);
  n = 0;
  while(*name)
    if(*name++ == '/')
      ++n;
  return(n);

} /* countlevl() */

/* Align buffer size to avoid too often alloc and free */
LOCAL unsigned alignbsiz(s, n)
  register unsigned s;
  register int n;
{
  register unsigned ns;

  if(s == 0 || n == 0)
    ns = 0;
  else
    for(ns = NBUFSZ; ns < s; ns += NBUFINC)
      ;
  return(ns);

} /* alignbsiz() */

/* Align filevector size to avoid too often alloc and free */
LOCAL int alignnfils(n)
  register int n;
{
  register int nn;

  if(n == 0)
    nn = 0;
  else
    for(nn = NFILES; nn < n; nn += NFILINC)
      ;
  return(nn);

} /* alignnfils() */

/*
 *      FILE LIST ROUTINES
 */

/* Update file f in file list */
LOCAL VOID insertflist(dp, f)
  register dlist *dp;
  register int f;
{
  struct stat st;
  register char *pn;

  /* Get mode flag for file */
  pn = pathname(FFNAM(dp, f), DPNAM(dp));
  if((*statfun)(pn, &st) == 0) {
    switch(STFMT(&st)) {
#ifdef  S_IFLNK
      case S_IFLNK:             /* Symbolic link */
	FMODE(dp, f) = FF_SLNK;
	break;
#endif  /* S_IFLNK */
#ifdef  S_IFSOCK
      case S_IFSOCK:            /* Socket */
	FMODE(dp, f) = FF_SOCK;
	break;
#endif
      case S_IFDIR:             /* Directory */
	FMODE(dp, f) = FF_DIR;
	++DNDIR(dp);
	break;
      default:                  /* Executable or other file */
	FMODE(dp, f) = (st.st_mode & 0111) ? FF_EXEC : FF_NONE;
	break;
    }
    FMTIM(dp, f) = st.st_mtime;
    FSIZE(dp, f) = st.st_size;
  }
  else {
    FMODE(dp, f) = FF_ERR;
    FMTIM(dp, f) = (time_t) 0;
    FSIZE(dp, f) = (off_t) 0;
  }
  FITAG(dp, f) = FF_NONE;

} /* insertflist() */

/* Delete an entry from file list */
GLOBL VOID deleteflist(dp, f)
  register dlist *dp;
  register int f;
{
  register int n;

  /* Do nothing and return if file list is not yet read in (RG 11/22/91) */
  if(DFVEC(dp) == FNULL)
    return;
  /* Move up file name, tag and mode flag in file list */
  n = f + 1;
  if(FITAG(dp, f) && DNTAG(dp) > 0)
    --DNTAG(dp);
  while(n < DNFIL(dp)) {
    FFNAM(dp, f) = FFNAM(dp, n);
    FITAG(dp, f) = FITAG(dp, n);
    FMODE(dp, f) = FMODE(dp, n);
    ++f;
    ++n;
  }
  /* Decrement counters */
  --DNFIL(dp);
  --filecount;

} /* deleteflist() */

/* Sort or resort file list from directory dp */
GLOBL VOID sortflist(dp)
  register dlist *dp;
{
  register int f;

  f = sortbytime;
  sortbytime = DSORT(dp) ? 1 : 0;       /* Set flag for comparing */
  if(DNFIL(dp) > 0)                     /* Sort/resort file list */
    qsort((char *) DFVEC(dp), (unsigned) DNFIL(dp), sizeof(flist), flistcmp);
  sortbytime = f;

} /* sortflist() */

/* Zoom file list of directory dp */
GLOBL VOID zoomflist(dp)
  register dlist *dp;
{
  register int f, n;

  if(DZOOM(dp)) {
    for(f = n = 0; f < DNFIL(dp); f++)
      if(umatch(dp, f, DZOOM(dp))) {
	if(n != f)
	  FLIST(dp, n) = FLIST(dp, f);
	++n;
      }
    DNFIL(dp) = n;
  }

} /* zoomflist() */

/* Rebuild file list in some cases (i.e. cp, mv ..) */
GLOBL int newflist(dp)
  register dlist *dp;
{
  char cfnam[NAMELEN];
  struct stat st;
  register flist *fv;
  register char *fbp, *name, *fb;
  register int i, j, k, nfil, ntag;
  register unsigned size;
  register DIR *dirfp;

  (void) putecho("Building %s", DPNAM(dp));
  flushout();
  /* Set/reset counter */
  nfil = DNFIL(dp);
  ntag = DNTAG(dp);
  filecount -= DNFIL(dp);
  DNFIL(dp) = DNDIR(dp) = DNTAG(dp) = 0;

  /* Get status of directory and read in */
  if((*statfun)(DPNAM(dp), &st) == 0 && (dirfp = opendir(DPNAM(dp)))) {
    /* Save name of current file */
    if(nfil > 0) {
      (void) strcpy(cfnam, FFNAM(dp, DFCUR(dp)));
      /* Save flist array and file buffer */
      if(ntag > 0) {
	fb = DFBUF(dp);
	fv = DFVEC(dp);
	DFBUF(dp) = NULL;
	DBSIZ(dp) = 0;
      }
    }
    /* Count files */
    for(i = 0; readdname(dirfp); i++)
      ;
    rewinddir(dirfp);
    /* Align file count and buffer size */
    i = alignnfils(i);
    size = alignbsiz((unsigned) st.st_size, i);
    /* Release current file list and file name buffer if needed */
    if(DFBUF(dp) && DBSIZ(dp) < size) {
      ufree(DFBUF(dp));
      ufree((char *) DFVEC(dp));
      DFBUF(dp) = NULL;
      DBSIZ(dp) = 0;
    }
    /* Get memory for file list and file name buffer if not yet done */
    if(DFBUF(dp) == NULL && i > 0) {
      DFBUF(dp) = (char *)  ualloc(size, sizeof(char));         /* RG 11/22/91 */
      DFVEC(dp) = (flist *) ualloc((unsigned) i, sizeof(flist));
      DBSIZ(dp) = size;
    }
    /* Fill up file list and file name buffer */
    for(i = 0, fbp = DFBUF(dp); name = readdname(dirfp); i++) {
      FFNAM(dp, i) = strcpy(fbp, name);
      fbp += strlen(name) + 1;
      insertflist(dp, i);
    }
    closedir(dirfp);
    DNFIL(dp) = i;
    sortflist(dp);
    if(DZOOM(dp))
      zoomflist(dp);
    /* Set up other directory data */
    DFLAG(dp) = FL_FIL;
    DMTIM(dp) = st.st_mtime;
    DCTIM(dp) = st.st_ctime;
    DCANC(dp) = CANCD(DPNAM(dp));
    /* Try to restore current file */
    DFTOP(dp) = DFCUR(dp) = 0;
    if(nfil > 0) {
      for(i = 0; i < DNFIL(dp); i++)
	if(cfnam[0] != *FFNAM(dp, i) || CMP(cfnam, FFNAM(dp, i)))
	  (void) gofile(dp, 1);
	else
	  break;
      if(i >= DNFIL(dp))
	while(gofile(dp, -1))
	  ;
      /* Try to restore tagged files */
      if(ntag > 0) {
	i = j = 0;
	while(i < nfil) {
	  if(FVTAG(fv, i) == FF_TAG)
	    for(k = j; k < DNFIL(dp); k++)
	      if(*FVFIL(fv,i) == *FFNAM(dp,k) && EQU(FVFIL(fv,i), FFNAM(dp,k))) {
		FITAG(dp, k) = FF_TAG;
		++DNTAG(dp);
		j = k;
		break;
	      }
	  ++i;
	}
      }
    }
    filecount += DNFIL(dp);
  }
  /* Error in stat: use defaults */
  else {
    ufree(DFBUF(dp));
    ufree((char *) DFVEC(dp));
    DFBUF(dp) = NULL;
    DFVEC(dp) = FNULL;
    DBSIZ(dp) = 0;
    DFLAG(dp) = FL_FIL;
    DMTIM(dp) = st.st_mtime;
    DCTIM(dp) = st.st_ctime;
    DCANC(dp) = 0;
    DFCUR(dp) = DFTOP(dp) = 0;
    fileflag |= SF_TREE;
    if(VARSET(V_WD)) {
      puthelp("BUILD %s (Y:exit  ELSE:continue)", DPNAM(dp));
      if(errequest(CFNAM, "Cannot read directory. Exit ?") == 'y')
	return(RV_END);
    }
  }

  /* Release saved flist array and file buffer */
  if(ntag > 0) {
    ufree(fb);
    ufree((char *) fv);
  }

  /* Set screen flags */
  treeflag |= SF_ECHO;
  fileflag |= SF_ECHO;

  return(RV_OK);

} /* newflist() */

/*
 *      DIRECTORY LIST ROUTINES
 */

/* Build and set tree info flag */
GLOBL VOID infodlist()
{
  register dlist *dp, *np;
  register int i, j;

  /* For each dir in tree check if there are dirs on the same level */
  for(dp = droot; dp; dp = (dlist *) DNEXT(dp)) {
    DINFO(dp) = 0;
    for(i = 1, j = DLEVL(dp) - 1; i < j; i++) {
      for(np = (dlist *) DNEXT(dp); np && DLEVL(np) > (i + 1); np = (dlist *) DNEXT(np))
	;
      if(np && DLEVL(np) == (i + 1))
	DINFO(dp) |= 1 << (i - 1);
    }
    for(np = (dlist *) DNEXT(dp); np && DLEVL(np) > DLEVL(dp); np = (dlist *) DNEXT(np))
      ;
    if(np && DLEVL(np) == DLEVL(dp))
      DINFO(dp) |= 1 << (i - 1);
  }

} /* infodlist() */

/* Insert directory dname into directory tree list */
LOCAL dlist *insertdlist(dname, nfils, fvec)
  register char *dname;
  register int nfils;
  register flist *fvec;
{
  register dlist *dp, *rp;
  register char *np;
  register int f;

  /* Get memory and save directory name */
  dp = (dlist *) ualloc(1, sizeof(dlist));
  np = strsav(pathname(dname, "/"));

  /* Set up pathname/filename and file list */
  DPNAM(dp) = np;
  DFNAM(dp) = basename(np);
  DZOOM(dp) = NULL;
  DNFIL(dp) = nfils;
  DFVEC(dp) = fvec;
  DNEXT(dp) = GNULL;
  DFCUR(dp) = DFTOP(dp) = 0;
  DNTAG(dp) = 0;
  /* For each file in file list create an entry */
  for(f = 0; f < nfils; f++)
    insertflist(dp, f);

  /* Insert dlist into tree list */
  if(droot == DNULL) {
    DPREV(dp) = GNULL;
    droot = dp;
  }
  else {
    for(rp = droot; DNEXT(rp); rp = (dlist *) DNEXT(rp))
      ;
    DNEXT(rp) = (glist *) dp;
    DPREV(dp) = (glist *) rp;
  }

  /* Increment directory and file counters */
  ++dircount;
  filecount += nfils;

  return(dp);

} /* insertdlist() */

/* Rebuild directory list after creating a new directory */
GLOBL dlist *newdlist(name, flag)
  register char *name;
  register int flag;
{
  struct stat st;
  register dlist *dp, *pdp, *ndp, *p;
  register char *np;
  register int levl;

  /* Allocate memory for new dlist entry and for pathname */
  ndp = (dlist *) ualloc(1, sizeof(dlist));
  np  = strsav(pathname(name, CPNAM));

  /* Insert dlist into directory tree */
  levl = CLEVL + 1;
  for(pdp = cdlist, dp = (dlist *) CNEXT; dp && DLEVL(dp) >= levl; dp = (dlist *) DNEXT(dp)) {
    if(DLEVL(dp) == levl && CMP(np, DPNAM(dp)) < 0)
      break;
    pdp = dp;
  }
  if(DNEXT(ndp) = DNEXT(pdp)) {
    p = (dlist *) DNEXT(ndp);
    DPREV(p) = (glist *) ndp;
  }
  DNEXT(pdp) = (glist *) ndp;
  DPREV(ndp) = (glist *) pdp;

  /* Fill up dlist record data */
  DPNAM(ndp) = np;
  DFNAM(ndp) = basename(np);
  DZOOM(ndp) = NULL;
  if((*statfun)(np, &st)) {     /* Bad stat: use defaults */
    DMTIM(ndp) = DCTIM(ndp) = (time_t) -1;
    DFLAG(ndp) = FL_FIL;
    DCANC(ndp) = 0;
  }
  else {
    DMTIM(ndp) = st.st_mtime;
    DCTIM(ndp) = st.st_ctime;
    DFLAG(ndp) = flag;
    DCANC(ndp) = CANCD(np);
  }
  DSORT(ndp) = sortbytime ? 1 : 0;
  DFBUF(ndp) = NULL;
  DFVEC(ndp) = FNULL;
  DBSIZ(ndp) = 0;
  DLEVL(ndp) = levl;
  if(DLEVL(ndp) > maxlevel)
    maxlevel = DLEVL(ndp);
  DNFIL(ndp) = DNDIR(ndp) = 0;
  DDNUM(ndp) = DDNUM(pdp) + 1;
  DFCUR(ndp) = DFTOP(ndp) = 0;
  DNTAG(ndp) = 0;

  /* Increment all following dlist entry numbers */
  for(dp = (dlist *) DNEXT(ndp); dp; dp = (dlist *) DNEXT(dp))
    ++DDNUM(dp);

  /* Update counter, flags and return */
  ++CNDIR;
  ++dircount;
  writeflag = 1;
  checkindent();
  infodlist();
  return(ndp);

} /* newdlist() */

/* Delete current entry from directory list */
GLOBL VOID deletedlist(rp)
  register dlist *rp;
{
  register dlist *dp, *p;
  register int f;

  /* Search for parent directory of directory list entry dp */
  for(dp = rp; dp && DLEVL(dp) >= DLEVL(rp); dp = (dlist *) DPREV(dp))
    ;

  /* If found destroy file list entry in parent directory */
  if(dp) {
    if(DFVEC(dp)) {             /* RG 11/22/91 */
      for(f = 0; f < DNFIL(dp); f++)
	if(EQU(FFNAM(dp, f), DFNAM(rp)))
	  break;
      deleteflist(dp, f);
    }
    --DNDIR(dp);
  }

  /* Close the directory tree list chain */
  if(DNEXT(rp)) {
    for(dp = (dlist *) DNEXT(rp); DNEXT(dp); dp = (dlist *) DNEXT(dp))
      ;
    while(dp != rp) {
      p = (dlist *) DPREV(dp);
      DDNUM(dp) = DDNUM(p);
      dp = (dlist *) DPREV(dp);
    }
    p = (dlist *) DNEXT(rp);
    DPREV(p) = DPREV(rp);
  }
  if(p = (dlist *) DPREV(rp))
    DNEXT(p) = DNEXT(rp);

  /* Release memory */
  ufree(DPNAM(rp));
  ufree(DFBUF(rp));
  ufree((char *) DFVEC(rp));
  if(DZOOM(rp))
    ufree(DZOOM(rp));
  ufree((char *) rp);

  /* Decrement directory counter and update tree info flag */
  --dircount;
  infodlist();

} /* deletedlist() */

/* Write out directory list dependent on w to file name */
GLOBL char *writedlist(name, rp, what, w)
  register dlist *rp;
  register char *name, *what;
  register int w;
{
  char list[NAMELEN];
  register FILE *fp;
  register dlist *dp, *p;
  register unsigned long info;
  register int f, i, levl;
  time_t t;

  (void) strcpy(list, pathname(name, CPNAM));
  if(fp = fopen(list, "w")) {
    t  = time((time_t *) 0);
    if(w == 'm')
      (void) fprintf(fp, "# %s: file list (matching %s)", getversion(), what);
    else
      (void) fprintf(fp, "# %s: %s list", getversion(), what);
    (void) fprintf(fp, ", %s", ctime(&t));
    dp = rp;
    do {
      if(w == 'd')
	(void) fprintf(fp, "%s\n", DPNAM(dp));
      else if(w == 'l') {
	p = (dlist *) DNEXT(dp);
	f = p && DLEVL(p) > DLEVL(rp);
	if(dp == rp) {
	  (void) fprintf(fp, "R%s\n", DPNAM(rp));
	  (void) fprintf(fp, "%cH%s\n", f ? 'U' : 'H', DFNAM(rp));
	}
	else {
	  levl = DLEVL(rp) - 1;
	  info = DINFO(dp) >> levl;
	  (void) fprintf(fp, "%c", f ? 'V' : 'L');
	  for(i = 1; i < (DLEVL(dp) - levl - 1); i++)
	    (void) fprintf(fp, "I%c", info & (1 << (i-1)) ? 'V' : 'S');
	  (void) fprintf(fp, "I%cH%s\n", info & (1 << (i-1)) ? 'T' : 'L', DFNAM(dp));
	}
      }
      else {
	for(f = 0; f < DNFIL(dp); f++)
	  switch(w) {
	    case 'm':
	      if(umatch(dp, f, what))
		goto WRITEFILE;
	      break;
	    case 't':
	      if( !ISTAG(dp, f))
		break;
	      /*Fall thru*/
	    case 'f':
WRITEFILE:    (void) fprintf(fp, "%s\n", pathname(FFNAM(dp, f), DPNAM(dp)));
	      break;
	  }
	}
      dp = (dlist *) DNEXT(dp);
    } while(dp && DLEVL(dp) > DLEVL(rp));
    (void) fclose(fp);
    return(list);
  }
  return(NULL);

} /* writedlist() */

/* Check directory list after file changes in directory name */
GLOBL VOID checkdlist(name)
  register char *name;
{
  char fname[NAMELEN];
  struct stat st;
  register dlist *dp;
  register char *sp;

  if( !ISDIR(name, st)) {
    (void) strcpy(fname, name);
    name = fname;
    if(sp = strrchr(fname, '/'))
      *sp = '\0';
    if((*statfun)(name, &st))   /* Bad stat: do nothing */
      return;
  }

  for(dp = droot; dp; dp = (dlist *) DNEXT(dp)) {
    if(EQU(DPNAM(dp), name) && CHKTIM(dp, st)) {
      DFLAG(dp) = FL_CHG;
      ++buildflag;
      break;
    }
  }

} /* checkdlist() */

/* Scan directory tree for changes in filesystem */
GLOBL int scandlist(rp)
  register dlist *rp;
{
  register dlist *dp;

  dp = rp;
  do {
    if(keypressed() && hitakey(NULL) < RV_NUL)
      return(RV_INT);
    if(DFLAG(dp) == FL_FIL && changedlist(dp)) {
      DFLAG(dp) = FL_CHG;
      ++buildflag;
    }
  } while((dp = (dlist *) DNEXT(dp)) && DLEVL(dp) > DLEVL(rp));
  return(RV_OK);

} /* scandlist() */

/* Check if status of a directory has changed */
GLOBL int changedlist(dp)
  register dlist *dp;
{
  struct stat st;

  return((*statfun)(DPNAM(dp), &st) == 0 && CHKTIM(dp, st));

} /* changedlist() */

/* Update directory list after changes in filesystem tree */
GLOBL int updatedlist()
{
  register dlist *dp;
  register int c;

  c = RV_OK;
  for(dp = droot; dp; dp = (dlist *) DNEXT(dp)) {
    if(DFLAG(dp) == FL_CHG) {
      if((c = newflist(dp)) != RV_OK)
	break;
      if(dp == cdlist)
	fileflag |= SF_TREE;
    }
  }
  buildflag = 0;
  return(c);

} /* updatedlist() */

/*
 *      BUILD UP DIRECTORY TREE
 */

/* Build up directory tree and file lists recursively. Work is done hard. */
/* For each directory starting with directory pn up to level last a       */
/* dlist entry is allocated, the dlist record is filled with all needed   */
/* directory data, the directory file list and the file name buffer, and  */
/* then inserted into the directory tree list.                            */
GLOBL int buildread(pn, levl, last, intr)
  register char *pn;
  register int levl, last, intr;
{
  char name[NAMELEN];
  static int dnum = 0;
  struct stat dst, fst;
  register DIR *fp;
  register dlist *dp;
  register flist *fvec;
  register char *fbuf, *fbufp, *fn;
  register char **dvec;
  register int nfils, ndirs, flag, i, rv;
  register unsigned bsiz;

  /* Is pn a directory? */
  if( !ISDIR(pn, dst))
    return(RV_ERR);

  /* Check for keyboard interrupt if interrupt is allowed */
  if(intr && keypressed() && hitakey(NULL) < RV_NUL)
    return(RV_INT);

  /* Init variables */
  ndirs = nfils = 0;
  fvec  = FNULL;
  fbuf  = NULL;
  bsiz  = 0;
  flag  = FL_NUL;

  /* Read in directory and build dlist entry */
  if(levl < last && (fp = opendir(pn))) {
    /* Count files */
    for(i = 0; readdname(fp); i++)
      ;
    rewinddir(fp);
    (void) putecho("Reading %s (%d files)", pn, i);
    flushout();
    /* Align nfils and bsiz to avoid too often alloc and free */
    i = alignnfils(i);
    bsiz = alignbsiz((unsigned) dst.st_size, i);
    if(i > 0) {
      /* Get memory for file buffer, file list and directory array */
      fbuf = (char *)  ualloc(bsiz, sizeof(char));
      fvec = (flist *) ualloc((unsigned) i, sizeof(flist));
      dvec = (char **) ualloc((unsigned) i, sizeof(char *));
    }
    else {
      fbuf = NULL;
      fvec = FNULL;
    }
    /* Read in directory, fill up file list and directory array */
    for(nfils = 0, fbufp = fbuf; fn = readdname(fp); nfils++) {
      FVFIL(fvec, nfils) = strcpy(fbufp, fn);
      fbufp += strlen(fn) + 1;
      (void) strcpy(name, pathname(fn, pn));
      if((*statfun)(name, &fst)) {
	FVTIM(fvec, nfils) = (time_t) 0;
	FVSIZ(fvec, nfils) = (off_t)  0;
      }
      else {
	FVTIM(fvec, nfils) = fst.st_mtime;
	FVSIZ(fvec, nfils) = fst.st_size;
	if(STFMT(&fst) == S_IFDIR && (hiddendirs || *fn != '.'))
	  dvec[ndirs++] = FVFIL(fvec, nfils);
      }
    }
    closedir(fp);
    /* Sort file list and directory array */
    qsort((char *) fvec, (unsigned) nfils, sizeof(flist),  flistcmp);
    qsort((char *) dvec, (unsigned) ndirs, sizeof(char *), dlistcmp);
    flag = FL_FIL;
  }

  /* Insert dlist into tree list and set up dlist record */
  dp = insertdlist(pn, nfils, fvec);
  DFBUF(dp) = fbuf;
  DFVEC(dp) = fvec;
  DBSIZ(dp) = bsiz;
  DCANC(dp) = CANCD(pn);
  DMTIM(dp) = dst.st_mtime;
  DCTIM(dp) = dst.st_ctime;
  DFLAG(dp) = flag;
  DSORT(dp) = sortbytime ? 1 : 0;
  DLEVL(dp) = levl;
  if(DLEVL(dp) > maxlevel)
    maxlevel = DLEVL(dp);
  DNDIR(dp) = ndirs;
  DDNUM(dp) = dnum++;

  if(nfils > 0) {
    /* Build a dlist entry for each directory found in directory list */
    for(i = 0; i < ndirs; i++) {
      (void) strcpy(name, pathname(dvec[i], pn));
      ++levl;
      rv = buildread(name, levl, last, intr);
      --levl;
      if(intr && rv == RV_INT)
	break;
    }
    /* Release directory array */
    ufree((char *) dvec);
  }

  /* Set up tree info flag and return */
  infodlist();
  return(intr && rv == RV_INT ? RV_INT : RV_OK);

} /* buildread() */

/* Build up directory tree from tree list. File lists are built up later.  */
/* For each directory starting with directory rn read in from tree list    */
/* file lfn a dlist entry is allocated, the dlist record is filled with    */
/* all needed directory data and inserted into the directory tree list.    */
/* File list and file name buffer are initially unset and filled up later. */
GLOBL int buildlist(rn, cwd, lfn)
  register char *rn, *cwd, *lfn;
{
  char name[NAMELEN];
  struct stat st;
  register FILE *fp;
  register dlist *dp;
  register char *pn;
  register int dlen, dlev, dnum, i;

  /* Init variables */
  dlen = strlen(rn);
  dlev = countlevl(rn);
  dnum = 0;

  /* Read in list file fn */
  if(fp = fopen(lfn, "r")) {
    (void) putecho("Building tree from list %s ... ", lfn);
    flushout();
    while(fgets(name, sizeof(name), fp)) {
      if(VALID(name[0])) {
	if(name[i = strlen(name) - 1] == '\n')
	  name[i] = '\0';
	pn = pathname(name, cwd);
	/* Skip bad entries in tree list */
	if(strncmp(rn, pn, dlen) || (*statfun)(pn, &st) || STFMT(&st) != S_IFDIR) {
	  writeflag = 1;
	  continue;
	}
	/* Skip hidden directories if flag is not set */
	if( !hiddendirs && *(basename(pn)) == '.')
	  continue;
	/* Insert into tree list and file up record with directory   */
	/* data. File list and file name buffer are initially unset. */
	dp = insertdlist(pn, 0, FNULL);
	DFBUF(dp) = NULL;
	DFVEC(dp) = FNULL;
	DBSIZ(dp) = 0;
	if(CANCD(pn)) {
	  DCANC(dp) = 1;
	  DMTIM(dp) = st.st_mtime;
	  DCTIM(dp) = st.st_ctime;
	}
	else {
	  DCANC(dp) = 0;
	  DMTIM(dp) = (time_t) -1;
	  DCTIM(dp) = (time_t) -1;
	}
	DFLAG(dp) = FL_NUL;
	DSORT(dp) = sortbytime ? 1 : 0;
	DLEVL(dp) = countlevl(pn) - dlev + 1;
	if(DLEVL(dp) > maxlevel)
	  maxlevel = DLEVL(dp);
	DNDIR(dp) = 0;
	DDNUM(dp) = dnum++;
      }
    }
    (void) fclose(fp);
  }

  /* Set up tree info flag and return */
  if(dnum) {
    infodlist();
    return(RV_OK);
  }
  return(RV_NUL);

} /* buildlist() */
