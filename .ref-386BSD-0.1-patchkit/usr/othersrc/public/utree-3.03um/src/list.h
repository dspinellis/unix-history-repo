/*
 *      LIST.H
 *      UTREE tree and file list definitions.
 *      3.03-um klin, Sat Jan 11 19:53:57 1992, Splitted from defs.h
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_list[] = "@(#) utree 3.03-um (klin) Feb 11 1992 list.h";
#endif  /* _MAIN_ && !lint */

/*
 *      Files in a directory are hold in dynamically allocated array
 *      of a record type flist which is linked into directory type dlist.
 */

typedef struct _flist {
  char *filename;               /*  File name                           */
  time_t modtime;               /*  Modification time                   */
  off_t filesize;               /*  File size                           */
  char istagged;                /*  File is tagged                      */
  char filemode;                /*  File modes                          */
} flist;

#define FNULL   ((flist *) 0)   /* The flist NIL pointer                */

/*
 *      Directories are hold in a double linked list of a record type
 *      dlist which is built up at startup time or later at runtime.
 */

typedef struct _dlist {
  glist list;                   /*  Containing pathname and pointers    */
  char *filename;               /*  Directory basename                  */
  flist *filevec;               /*  Pointer to flist entries            */
  char *zoompattern;            /*  File zooming pattern                */
  char *filebuf;                /*  Buffer of filenames                 */
  unsigned bufsize;             /*  Size of filename buffer             */
  int number;                   /*  Directory number                    */
  int level;                    /*  Subdirectory level                  */
  unsigned long treeinfo;       /*  Tree information flag               */
  int nfils;                    /*  Number of files                     */
  int ndirs;                    /*  Number of subdirectories            */
  int ntags;                    /*  Number of tagged files              */
  int curfile;                  /*  Current file in file list           */
  int topfile;                  /*  First file on screen in file list   */
  char cancd;                   /*  Can change to this directory        */
  char flag;                    /*  Filelist flag                       */
  char sort;                    /*  Sort criteria flag                  */
  time_t modtime;               /*  Modification time of directory      */
  time_t chgtime;               /*  Status change time of directory     */
} dlist;

#define DNULL   ((dlist *) 0)   /* The dlist NIL pointer                */

/*
 *      Access to items of dlist record is done with macros
 *      to hide this record and for abbreviation.
 */

#define DLIST(p)        (&(p)->list)
#define DPNAM(p)        ((p)->list.string)
#define DPREV(p)        ((p)->list.prev)
#define DNEXT(p)        ((p)->list.next)
#define DFNAM(p)        ((p)->filename)
#define DFVEC(p)        ((p)->filevec)
#define DZOOM(p)        ((p)->zoompattern)
#define DFBUF(p)        ((p)->filebuf)
#define DBSIZ(p)        ((p)->bufsize)
#define DDNUM(p)        ((p)->number)
#define DLEVL(p)        ((p)->level)
#define DINFO(p)        ((p)->treeinfo)
#define DNFIL(p)        ((p)->nfils)
#define DNDIR(p)        ((p)->ndirs)
#define DNTAG(p)        ((p)->ntags)
#define DFCUR(p)        ((p)->curfile)
#define DFTOP(p)        ((p)->topfile)
#define DCANC(p)        ((p)->cancd)
#define DFLAG(p)        ((p)->flag)
#define DSORT(p)        ((p)->sort)
#define DMTIM(p)        ((p)->modtime)
#define DCTIM(p)        ((p)->chgtime)

/*
 *      The same access macros as above for current directory entry.
 */

#define CLIST           (&cdlist->list)
#define CPNAM           (cdlist->list.string)
#define CPREV           (cdlist->list.prev)
#define CNEXT           (cdlist->list.next)
#define CFNAM           (cdlist->filename)
#define CFVEC           (cdlist->filevec)
#define CZOOM           (cdlist->zoompattern)
#define CFBUF           (cdlist->filebuf)
#define CBSIZ           (cdlist->bufsize)
#define CDNUM           (cdlist->number)
#define CLEVL           (cdlist->level)
#define CINFO           (cdlist->treeinfo)
#define CNFIL           (cdlist->nfils)
#define CNDIR           (cdlist->ndirs)
#define CNTAG           (cdlist->ntags)
#define CFCUR           (cdlist->curfile)
#define CFTOP           (cdlist->topfile)
#define CCANC           (cdlist->cancd)
#define CFLAG           (cdlist->flag)
#define CSORT           (cdlist->sort)
#define CMTIM           (cdlist->modtime)
#define CCTIM           (cdlist->chgtime)

/* Compare modification and change time from dlist p with status s      */
#define CHKTIM(p, s)    ((p)->modtime<st.st_mtime||(p)->chgtime<st.st_ctime)

/* Column and row of directory p on directory tree screen               */
#define DTCOL(p)        ((p)->level*indent)
#define DTROW(p)        ((p)->number-tdlist->number+firstdline)

/*
 *      Access to items of flist record is done with macros
 *      to hide this record and for abbreviation.
 */

/* Access macros to flist items pointed to by flist ptr f               */
#define FPFIL(f)        (f->filename)
#define FPTIM(f)        (f->modtime)
#define FPSIZ(f)        (f->filesize)
#define FPTAG(f)        (f->istagged)
#define FPMOD(f)        (f->filemode)

/* Access macros to flist items in flist vector v with index n          */
#define FVFIL(v, n)     (v[n].filename)
#define FVTIM(v, n)     (v[n].modtime)
#define FVSIZ(v, n)     (v[n].filesize)
#define FVTAG(v, n)     (v[n].istagged)
#define FVMOD(v, n)     (v[n].filemode)

/* Access macros to flist with index n from directory pointed to by p   */
#define FLIST(p, n)     (p->filevec[n])
#define FFNAM(p, n)     (p->filevec[n].filename)
#define FMTIM(p, n)     (p->filevec[n].modtime)
#define FSIZE(p, n)     (p->filevec[n].filesize)
#define FITAG(p, n)     (p->filevec[n].istagged)
#define FMODE(p, n)     (p->filevec[n].filemode)

/* Column and row of file n from directory p                            */
#define FFCOL(p, n)     (((n-p->topfile)%fperline)*FWINSZ)
#define FFROW(p, n)     ((n-p->topfile)/fperline+firstline)

/* Column and row of file n in file window on tree screen               */
#define FTCOL(n)        ((n%fperline)*FWINSZ)
#define FTROW(n)        (n/fperline+firstfline)

/* Values for directory list file list flag                             */
#define FL_NUL  0x00            /* File list not read in                */
#define FL_CHG  0x01            /* File list changed                    */
#define FL_FIL  0x02            /* File list read in                    */

/* Values for file mode and flags                                       */
#define FF_NONE ' '             /* Ordinary file                        */
#define FF_ERR  '?'             /* Can't stat file                      */
#define FF_EXEC '*'             /* Executable                           */
#define FF_DIR  '/'             /* Directory                            */
#define FF_SLNK '@'             /* Symbolic link                        */
#define FF_SOCK '='             /* Socket AF_UNIX                       */
#define FF_TAG  '+'             /* File is tagged                       */
#define FF_MARK '>'             /* Mark on file                         */

#define ISTAG(p, n)     (FITAG(p, n)  == FF_TAG)
