/*
 *      STAT.C
 *      UTREE file status functions.
 *      3.03-um klin, Tue Feb 11 22:47:06 1992, Splitted from comm.c
 *            c klin, Mon Mar 30 11:02:24 1992, Minor change in fileaccess()
 *
 *      Copyright (c) 1991/92 Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03c-um (klin) Mar 30 1992 stat.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

#ifndef major           /* Major number part of a device */
# define major(x)       ((x >> 8) & 0377)
#endif  /* !major */
#ifndef minor           /* Minor number part of a device */
# define minor(x)       (x & 0377)
#endif  /* !minor */

/* ---- External variables and functions ------------------------------ */

/*EXTRN struct passwd *getpwuid();
EXTRN struct passwd *getpwnam();
EXTRN struct group  *getgrgid();
EXTRN struct group  *getgrnam();*/
EXTRN char *readdname();

/* ---- Functions and procedures -------------------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Return device name for device with device number r from /dev */
LOCAL char *devicename(r)
  dev_t r;
{
  static char pn[NAMELEN];
  struct stat st;
  register DIR *dp;
  register char *fn;

  if(dp = opendir("/dev")) {
    (void) strcpy(pn, "/dev/");
    /* Read in all valid entries in /dev */
    while(fn = readdname(dp)) {
      (void) strcpy(&pn[5], fn);
      if(ISBLK(pn, st) && st.st_rdev == r) {
	/* Match: return pathname of device */
	closedir(dp);
	return(pn);
      }
    }
    closedir(dp);
  }
  return(NULL);

} /* devicename() */

/* Get and return type of file from stat data st */
LOCAL char *filetype(st)
  register struct stat *st;
{
  register int f;

  f = STFMT(st);
  if(f == S_IFDIR)              /* Directory */
    return("directory");
  else if(f == S_IFBLK)         /* Block special */
    return("block special");
  else if(f == S_IFCHR)         /* Character special */
    return("character special");
  else if(f == S_IFREG)         /* Regular file */
    return("regular");
#ifdef  S_IFSOCK
  else if(f == S_IFSOCK)        /* Socket */
    return("socket");
#endif
#ifdef  S_IFIFO
  else if(f == S_IFIFO)         /* Named pipe */
    return("fifo");
#endif
#ifdef  S_IFLAN
  else if(f == S_IFLAN)         /* LAN special */
    return("network lan special");
#endif
#ifdef  S_IFLNK
  else if(f == S_IFLNK)         /* Symbolic link */
    return("symbolic link");
#endif
  return("unkown");             /* Unknown type */

} /* filetype() */

/* Build string containing current permissions */
LOCAL char *currentperms(m)
  int m;
{
  static char ps[4];
  register char *p = ps;

  if(m & 04)
    *p++ = 'r';
  if(m & 02)
    *p++ = 'w';
  if(m & 01)
    *p++ = 'x';
  *p = '\0';
  return(ps);

} /* currentperms() */

/* Parse permission string s */
LOCAL int parseperms(s)
  register char *s;
{
  register int p;

  p = 0;
  while(*s) {
    switch(*s) {
      case '0':                 /* Permission are given octal */
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
	return(*s - '0');
      case 'r':                 /* Read */
	p |= 004; break;
      case 'w':                 /* Write */
	p |= 002; break;
      case 'x':                 /* Execute/change */
	p |= 001; break;
      case '-':                 /* No permission */
	p = 0; break;
      default:
	return(-1);
    }
    ++s;
  }
  return(p);

} /* parseperms() */

/* Get and display status of file pn. Return status record */
LOCAL struct stat *filestatus(pn)
  register char *pn;
{
  static struct stat st;
  char *uid = "???";
  char *gid = "???";
  register struct passwd *pw;
  register struct group *gr;
  register char *dev;
  register int isdir, isdev, l;
#ifdef  S_IFLNK
  char sym[NAMELEN];
  struct stat lst;
  int n, issym = 0;
#endif  /* S_IFLNK */

  if(lstat(pn, &st))            /* Get file status information */
    return((struct stat *) 0);

  clearwindow(firstline, lastline);
  l = firstline;
  isdir = isdev = 0;

  /* Display type of file */
  (void) putfxy(0, ++l, 0, "Path: %s", pn);
  (void) putfxy(0, ++l, 0, "Type: %s (octal %06o)", filetype(&st), STFMT(&st));
  switch(STFMT(&st)) {
    case S_IFDIR:               /* Directory */
      ++isdir;
      break;
    case S_IFBLK:               /* Block special */
    case S_IFCHR:               /* Character special */
#ifdef  S_IFLAN
    case S_IFLAN:               /* LAN special */
#endif
      ++isdev;
      break;
#ifdef  S_IFLNK
    case S_IFLNK:               /* Symbolic link */
      ++issym;
      break;
#endif
  }

  /* Display device number if file is a device */
  if(isdev)
    (void) putfxy(0, ++l, 0, "Device major no: %d, minor no %d",
		  major(st.st_rdev), minor(st.st_rdev));

  /* Display device the file resides on */
  if( !(dev = devicename(st.st_dev)))
    dev = "???";
  (void) putfxy(0, ++l, 0, "Resides on device: %s (major: %d, minor: %d)",
		dev, major(st.st_dev), minor(st.st_dev));

  /* Display inode, # of links and size */
  (void) putfxy(0, ++l, 0, "Inode: %d, Links: %d, Size: %ld bytes",
		st.st_ino, st.st_nlink, st.st_size);
#ifdef  BSD
# ifndef titan
  /* Display blocksize and allocated blocks */
  (void) putfxy(0, ++l, 0, "Optimal IO blocksize: %ld, Allocated blocks: %ld",
		st.st_blksize, st.st_blocks);
# endif /* titan */
#endif  /* BSD */

  /* Display owner and group of file */
  if(pw = getpwuid((int) st.st_uid))
    uid = pw->pw_name;
  (void) putfxy(0, ++l, 0, "Owner: %s (uid: %d)", uid, st.st_uid);
  if(gr = getgrgid((int) st.st_gid))
    gid = gr->gr_name;
  (void) putfxy(0, ++l, 0, "Group: %s (gid: %d)", gid, st.st_gid);

  /* Display file permissions for user, group and others */
  (void) putfxy(0, ++l, 0, "Permissions for user:");
  if(st.st_mode & 0400)
    (void) putfxy(24, l, 0, "read ");
  if(st.st_mode & 0200)
    (void) putfxy(29, l, 0, "write");
  if(st.st_mode & 0100)
    (void) putfxy(35, l, 0, isdir ? "search" : "execute");
  (void) putfxy(16, ++l, 0, "group:");
  if(st.st_mode & 0040)
    (void) putfxy(24, l, 0, "read ");
  if(st.st_mode & 0020)
    (void) putfxy(29, l, 0, "write");
  if(st.st_mode & 0010)
    (void) putfxy(35, l, 0, isdir ? "search" : "execute");
  (void) putfxy(16, ++l, 0, "all:");
  if(st.st_mode & 004)
    (void) putfxy(24, l, 0, "read ");
  if(st.st_mode & 0002)
    (void) putfxy(29, l, 0, "write");
  if(st.st_mode & 0001)
    (void) putfxy(35, l, 0, isdir ? "search" : "execute");

  /* Display special access rights */
  if(st.st_mode & (S_ISUID|S_ISGID))
    (void) putfxy(16, ++l, 0, "Set user and group ID (%s, %s) on execution",
		  uid, gid);
  else if((st.st_mode & S_ISUID) == S_ISUID)
    (void) putfxy(16, ++l, 0, "Set user ID (%s) on execution", uid);
  else if((st.st_mode & S_ISGID) == S_ISGID)
    (void) putfxy(16, ++l, 0, "Set group ID (%s) on execution", gid);
  if((st.st_mode & S_ISVTX) == S_ISVTX)
    (void) putfxy(16, ++l, 0, "Save text image after execution");
  (void) putfxy(16, ++l, 0, "Short form: %s (octal: %04o)",
		fileaccess(&st), st.st_mode & 07777);

  /* Display file dates */
  (void) putfxy(0, ++l, 0, "Last access:        %s", ctime(&st.st_atime));
  (void) putfxy(0, ++l, 0, "Last modification:  %s", ctime(&st.st_mtime));
  (void) putfxy(0, ++l, 0, "Last status change: %s", ctime(&st.st_ctime));

#ifdef  S_IFLNK
  /* Read and display symbolic link */
  if(issym && (n = readlink(pn, sym, sizeof(sym))) > 0) {
    sym[n] = '\0';
    (void) putfxy(0, ++l, 0, "Symbolic link to: %s", sym);
    if((*statfun)(sym, &lst))
      (void) putfxy(0, ++l, 0, "Cannot stat type of file");
    else
      (void) putfxy(0, ++l, 0, "Type: %s (octal %06o)",
		    filetype(&lst), STFMT(&lst));
  }
#endif  /* S_IFLNK */

  return(&st);

} /* filestatus() */

/*
 *      FILE STATUS ROUTINES
 */

/* Get and return file access string (like ls -l) */
GLOBL char *fileaccess(st)
  register struct stat *st;
{
  static char perm[10];

/*perm[0] = STFMT(st) == S_IFDIR ? 'd' : '-';*/
  perm[0] = st->st_mode & 0400   ? 'r' : '-';
  perm[1] = st->st_mode & 0200   ? 'w' : '-';
  if(st->st_mode & S_ISUID)
    perm[2] = 's';
  else
    perm[2] = st->st_mode & 0100 ? 'x' : '-';
  perm[3] = st->st_mode & 0040   ? 'r' : '-';
  perm[4] = st->st_mode & 0020   ? 'w' : '-';
  if(st->st_mode & S_ISGID)
    perm[5] = 's';
  else
    perm[5] = st->st_mode & 0010 ? 'x' : '-';
  perm[6] = st->st_mode & 0004   ? 'r' : '-';
  perm[7] = st->st_mode & 0002   ? 'w' : '-';
  if(st->st_mode & S_ISVTX)
    perm[8] = 't';
  else
    perm[8] = st->st_mode & 0001 ? 'x' : '-';
  perm[9] = '\0';
  return(perm);

} /* fileaccess() */

/* Show all file status information for file fn     */
/* Change owner and/or permissions if flag f is set */
GLOBL int statusfile(fn, f)
  register char *fn;
  register int f;
{
  char pnbuf[NAMELEN], buf[INPLEN];
  register struct passwd *pw;
  register struct group *gr;
  register struct stat *st;
  register char *pn, *ps;
  register int mode;
  register int c;

  /* Build full pathname if needed */
  if(f) {
    (void) strcpy(pnbuf, pathname(fn, CPNAM));
    pn = pnbuf;
  }
  else
    pn = fn;

  who = "STATUS";
  /* File status loop */
  do {
    /* Get and display file status information */
#ifdef  BSD
    enablesignals();
#endif
    st = filestatus(pn);
#ifdef  BSD
    disablesignals();
#endif
    if( !st) {
      c = errequest(fn, "Cannot stat");
      break;
    }
    flushout();

    if(f && STFMT(st) == S_IFDIR) {
      /* Changes for directories from tree menu only */
      puthelp("%s (CR:continue  Q:quit)", who);
      (void) putecho("Status %s: ", fn);
      c = hitakey(NULL);
      break;
    }
    puthelp("%s (CR:continue  P:permissions  O:owner  G:group  ELSE:quit)", who);
    (void) putecho("Change status %s:", fn);
    c = hitakey(NULL);
    if(c == 'p') {              /* Change permissions */
      mode = 0;
      puthelp("CHANGE PERMISSIONS: Octal or string from \'rwx-\' (CR:quit))");
      c = putecho("Set permissions for owner to:");
      ps = currentperms((int) (st->st_mode & 0700) >> 6);
      if((c = getline(buf, sizeof(buf), c, 0, ps, GNULL, 0)) != RV_OK)
	break;
      else if((c = parseperms(buf)) < 0) {
	c = errequest(buf, "Bad permissions");
	break;
      }
      mode = c << 6;
      c = putecho("Set permissions for group to:");
      ps = currentperms((int) (st->st_mode & 0070) >> 3);
      if((c = getline(buf, sizeof(buf), c, 0, ps, GNULL, 0)) != RV_OK)
	break;
      else if((c = parseperms(buf)) < 0) {
	c = errequest(buf, "Bad permissions");
	break;
      }
      mode |= c << 3;
      c = putecho("Set permissions for all to:");
      ps = currentperms((int) (st->st_mode & 0007));
      if((c = getline(buf, sizeof(buf), c, 0, ps, GNULL, 0)) != RV_OK)
	break;
      else if((c = parseperms(buf)) < 0) {
	c = errequest(buf, "Bad permissions");
	break;
      }
      mode |= c;
      if(chmod(pn, mode) < 0) {
	c = errequest(fn, "Cannot change permissions");
	break;
      }
      ++buildflag;
      c = 'p';
    }
    else if(c == 'o') {         /* Change owner */
      puthelp("CHANGE OWNER: Give new owner name (CR:quit)");
      c = putecho("Change owner of %s to:", fn);
      if((c = getline(buf, sizeof(buf), c, 0, NULL, GNULL, 0)) != RV_OK)
	break;
      if( !(pw = getpwnam(buf))) {
	c = errequest(buf, "Unknown user");
	break;
      }
      else if(chown(pn, (int) pw->pw_uid, (int) st->st_gid) < 0) {
	c = errequest(fn, "Cannot change owner");
	break;
      }
      ++buildflag;
      c = 'o';
    }
    else if(c == 'g') {         /* Change group */
      puthelp("CHANGE GROUP: Give new group name (CR:quit)");
      c = putecho("Change group of %s to:", fn);
      if((c = getline(buf, sizeof(buf), c, 0, NULL, GNULL, 0)) != RV_OK)
	break;
      if( !(gr = getgrnam(buf))) {
	c = errequest(buf, "Unknown group");
	break;
      }
      else if(chown(pn, (int) st->st_uid, (int) gr->gr_gid) < 0) {
	c = errequest(fn, "Cannot change group");
	break;
      }
      ++buildflag;
      c = 'g';
    }
  } while(c == 'p' || c == 'o' || c == 'g');

  /* Set screen flags and return */
  treeflag = fileflag = SF_FULL;
  return(c);

} /* statusfile() */
