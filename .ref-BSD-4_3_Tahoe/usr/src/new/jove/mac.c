/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/


/* (C) 1986, 1987, 1988 Ken Mitchum. This code is intended only for use with Jove. */

#include "tune.h"
#ifdef MAC
#define _mac 
#include <MacTypes.h>
#include "jove.h" 
#include <QuickDraw.h>
#include <WindowMgr.h>
#include <FontMgr.h>
#include <ListMgr.h>
#include <EventMgr.h>
#include <ControlMgr.h>
#include <DialogMgr.h>
#include <ResourceMgr.h>
#include <ToolboxUtil.h>
#include <HFS.h>
#include <StdFilePkg.h>
#include <MenuMgr.h>
#include <pascal.h>
#include <errno.h>
#include <SegmentLdr.h>
#include "mac.h"
#include "termcap.h"

/***************************************************/

/* these normally reside in "tune.c" which we don't use */

char *CmdDb;	/* see InitMac() */
char *p_tempfile = ".jrecXXX";	
char *d_tempfile = ".joveXXX";
char *Joverc = ".joverc";	


void putcurs(),curset(),putp(),dellines(),inslines();


static WindowPtr theScreen;

int
	errno,
	EventCmd,
	Keyonly,
	Macmode,
	Bufchange,
	Modechange,
	Windchange;




/* Initialization Routines. */

void InitBinds()
{
	struct cmd *c;
	data_obj **p;
	int i;
	
	p = mainmap;
	for(i= 0; i < NCHARS; i++) {
		c = (struct cmd *) *p;
		c->c_map = F_MAINMAP;
		c->c_key = i;
		p++;
	}
	
	p = pref1map;
	for(i= 0; i < NCHARS; i++) {
		c = (struct cmd *) *p;
		c->c_map = F_PREF1MAP;
		c->c_key = i;
		p++;
	}
	p = pref2map;
	for(i= 0; i < NCHARS; i++) {
		c = (struct cmd *) *p;
		c->c_map = F_PREF2MAP;
		c->c_key = i;
		p++;
	}
	
}

static	WindowPtr window;
static	Rect r;	
static CursHandle cross;

void InitEvents()
{
	void InitSysMenu();
	
	window = theScreen;
	InitSysMenu();		
	SetRect(&r,window->portRect.left,
	window->portRect.top,
	window->portRect.right - SCROLLWIDTH,
	window->portRect.bottom - SCROLLWIDTH);
	cross = GetCursor(crossCursor);
}

void MacInit()
{
	char *gethome();
	void tn_init();

	tn_init();
	getdir();
	gethome();	/* before anyone changes it */
	CmdDb = malloc(strlen(gethome()) + 10);
	strcpy(CmdDb,gethome());
	strcat(CmdDb,"/cmds.doc");
	InitBinds();
}


/* dummy routines. */

void InitCM()
{
}

void ResetTerm(){}

void UnsetTerm(s)
char *s;
{
}



int dummy(){}

int (*signal(sig,func))()
int sig;
int (*func)();
{
	return(&dummy);
}
dorecover() {}


/* Surrogate unix-style file i/o routines for Jove. These replace the
   routines distributed in the libraries. They work with Jove, but may
   not be general enough for other purposes. */

#include <io.h>
#define NFILES 10
/*
#define fsetup(p) {p.ioCompletion = 0; p.ioVRefNum = cur_vol; p.ioDirID = cur_dir;p.ioFVersNum = 0;}
#define isetup(p) {p.ioCompletion = 0; p.ioVRefNum = cur_vol;}
*/

static int cur_vol;	/* Disk or volume number */
static long cur_dir;	/* Directory number */
static int cur_vref;	/* ugh.. Vref for volume + directory */

struct ftab {
	int inuse;	/* 0 = closed 1 = binary 2 = text*/
	int refnum;	/* Mac file reference number */
} ft[NFILES];

fsetup(p)
HParmBlkPtr p;
{
	bzero(p,sizeof(HParamBlockRec));
	p->fileParam.ioVRefNum = cur_vol;
	p->fileParam.ioDirID = cur_dir;
	p->fileParam.ioFVersNum = 0;
}

isetup(p)
HIOParam *p;
{
	bzero(p,sizeof(HIOParam));
	p->ioVRefNum = cur_vol;
}

	
/* Kludge to convert Macintosh error codes to something like Unix. */

static int cvt_err(err)	/* some of these don't make sense... */
{
	switch(err) {
		case noErr: errno = 0; return(0);
		case dirFulErr: errno = ENOSPC; break;
		case dskFulErr: errno = ENOSPC; break;
		case nsvErr: errno = ENOENT; break;
		case ioErr: errno = EIO; break;
		case bdNamErr: errno = EINVAL; break;
		case fnOpnErr: errno = EBADF; break;	/* dubious... */
		case eofErr: errno = ESPIPE; break;		/* ditto */
		case posErr: errno = ESPIPE; break;
		case mFulErr: 
		case tmfoErr:
		case fnfErr: errno = ENOENT; break;
		case wPrErr: errno = EROFS; break;
		case fLckdErr: errno = EACCES; break;
		case fBsyErr: errno = EBUSY; break;
		case dupFNErr: errno = EEXIST; break;
		case opWrErr:
		case paramErr: errno = EINVAL; break;
		case rfNumErr: errno = EBADF; break;
		case gfpErr:
		case volOffLinErr: errno = ENODEV; break;
		case permErr: errno = EACCES; break;
		case volOnLinErr: errno = ENODEV; break;
		case nsDrvErr: errno = ENODEV; break;
		case noMacDskErr: errno = EIO; break;
		case extFSErr: errno = EIO; break;
		case fsRnErr: 
		case badMDBErr:
		case wrPermErr: errno = EPERM; break;
		default: errno = ENOENT;
	}
	return(-1);
}

static char *cvt_fnm(file)
char *file;
{
	static char nm[255];
	char *t;


	if(*file == '/') strcpy(nm,file + 1); /* full path */
	else {
		if(index(file + 1, '/') != NULL)
			strcpy(nm,"/");	/* make a partial pathname */
		else *nm = '\0';
		strcat(nm,file);
	}
	t = nm;
	while(*t) {
		if(*t == '/') *t = ':';
		t++;
	}
	return(nm);
}

int creat(name,perm)	/* permission mode is irrelevant on a Mac */
char *name;
{
	int fd, err;
	char *nm;
	HParamBlockRec p;

	nm = cvt_fnm(name);	/* convert filename to Mac type name */
	CtoPstr(nm);
	for(fd = 0; fd < NFILES && ft[fd].inuse; fd++);
	if(fd == NFILES) {
		errno = EMFILE;
		return(-1);
	}
	fsetup(&p);	/* try to delete it, whether it is there or not. */
	p.fileParam.ioNamePtr = (StringPtr) nm;
	if((err = PBHDelete(&p,0)) != noErr && err != fnfErr) return(cvt_err(err));
	if(do_creat(&p,nm) != 0) return(-1);
	else {
		ft[fd].inuse++;
		ft[fd].refnum = p.ioParam.ioRefNum;
		return(fd + 1);
	}
}

int open(name,mode)
char *name;
{
	int fd, err;
	char *nm;
	HParamBlockRec p;

	nm = cvt_fnm(name);	/* convert filename to Mac type name */
	CtoPstr(nm);
	for(fd = 0; fd < NFILES && ft[fd].inuse; fd++);
	if(fd == NFILES) {
		errno = EMFILE;
		return(-1);
	}
	fsetup(&p);
	if((mode & 3) == O_RDONLY) p.ioParam.ioPermssn = fsRdPerm;
	if((mode & 3) == O_WRONLY) p.ioParam.ioPermssn = fsWrPerm;
	if((mode & 3) == O_RDWR) p.ioParam.ioPermssn = fsRdWrPerm;
	p.ioParam.ioNamePtr = (StringPtr) nm;
	p.ioParam.ioMisc = 0;
	if((err = PBHOpen(&p,0)) != noErr && err != fnfErr) return(cvt_err(err));
	if(err == noErr && mode & O_CREAT && mode & O_EXCL) {
		PBClose(&p,0);
		errno = EEXIST;
		return(-1);
	}
	if(err == fnfErr) {
		if(mode & O_CREAT) {
			if(do_creat(&p,nm) != 0) return(-1);
		} else {
			errno = ENOENT;
			return(-1);
		}
	}
	ft[fd].inuse++;
	ft[fd].refnum = p.ioParam.ioRefNum;
	if(mode & O_APPEND)	p.ioParam.ioPosMode = fsFromLEOF;
	else p.ioParam.ioPosMode = fsFromStart;
	p.ioParam.ioPosOffset = 0;
	if((err = PBSetFPos(&p,0)) != noErr) {
		ft[fd].inuse = 0;
		return(cvt_err(err));
	}
	errno = 0;
	return(fd + 1);
}

static int do_creat(p,nm)
HParmBlkPtr p;
char *nm;
{
	int err;
	
	fsetup(p);
	p->fileParam.ioNamePtr = (StringPtr) nm;
	if((err = PBHCreate(p,0)) != noErr) return(cvt_err(err));
	fsetup(p);
	p->fileParam.ioNamePtr = (StringPtr) nm;
	p->fileParam.ioFDirIndex = 0;
	if((err = PBHGetFInfo(p,0)) != noErr) return(cvt_err(err));
	p->fileParam.ioDirID = cur_dir;
	p->fileParam.ioFlFndrInfo.fdType = 'TEXT';
	p->fileParam.ioFlFndrInfo.fdCreator = 'JV01';
	p->fileParam.ioFlFndrInfo.fdFlags = 0;
	p->fileParam.ioFVersNum = 0;
	if((err = PBHSetFInfo(p,0)) != noErr) return(cvt_err(err));
	fsetup(p);
	p->ioParam.ioNamePtr = (StringPtr) nm;
	p->ioParam.ioPermssn = fsRdWrPerm;
	p->ioParam.ioMisc = 0;
	if(cvt_err(PBHOpen(p,0))) return(-1);
	return(0);
}


int close(fd)
{
	int err;
	HParamBlockRec p;

	fsetup(&p);
	p.ioParam.ioRefNum = ft[--fd].refnum;
	ft[fd].inuse = 0;
/*	if(cvt_err(PBFlushFile(&p,0)) < 0) return(-1);
	fsetup(&p); */
	if(cvt_err(PBClose(&p,0)) < 0) return(-1);
	fsetup(&p);
	p.ioParam.ioNamePtr = 0;
	if(cvt_err(PBFlushVol(&p,0)) < 0) return(-1);
}

int read(fd,buf,n)
char *buf;
unsigned n;
{
	int err;
	IOParam p;
	if(fd == 0) return(con_read(buf,n));
	if(ft[--fd].inuse == 0) {
		errno = EBADF;
		return(-1);
	}
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	p.ioBuffer = buf;
	p.ioReqCount = n;
	p.ioPosMode = fsFromMark;
	p.ioPosOffset = 0;
	if((err = PBRead(&p,0)) != noErr && err != eofErr) {
		cvt_err(err);
		return(-1);
	}
	while(n--) {		
		if(*buf == '\r') *buf = '\n';	/* convert from Mac style */
		buf++;
	}
	errno = 0;
	return(p.ioActCount);
}

int write(fd,buf,n)
char *buf;
unsigned n;
{
	int err;
	IOParam p;
	char *obuf, *s;
	
	if(fd == 0) return(con_write(buf,n));
	
	s = obuf = malloc(n + 1);
	if(obuf == 0)  return(-1);	/* shouldn't happen... */
	if(ft[--fd].inuse == 0) {
		errno = EBADF;
		free(obuf);
		return(-1);
	}
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	p.ioBuffer = obuf;
	p.ioReqCount = (long) n;
	p.ioPosMode = fsFromMark;
	p.ioPosOffset = 0L;
	while(n--) {		
		if(*buf == '\n') *s = '\r';	/* make it look like Mac files */
		else(*s = *buf);
		buf++;
		s++;
	}
	if((err = PBWrite(&p,0)) != noErr) {
		free(obuf);
		return(-1);
	}
	free(obuf);
	return((int) p.ioActCount);
}

long lseek(fd,offset,type)	/* The Mac version of this doesn't allocate new space. */
long offset;
unsigned type;
{
	int err;
	long cur_mark, eof, new_mark;
	IOParam p;

	if(ft[--fd].inuse == 0) {
		errno = EBADF;
		return(-1);
	}

	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	if((err = PBGetFPos(&p,0)) != noErr) {
		cvt_err(err);
		return(-1);
	}
	cur_mark = p.ioPosOffset;
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	if((err = PBGetEOF(&p,0)) != noErr) {
		cvt_err(err);
		return(-1);
	}
	eof = (long) p.ioMisc;
	switch(type) {
		case 0 :
			new_mark = offset;
			break;
		case 1 :
			new_mark = offset + cur_mark;
			break;
		case 2 :
			new_mark = offset + eof;
	}
	if(new_mark > eof) {		/* need more space in file */
		isetup(&p);
		p.ioRefNum = ft[fd].refnum;
		p.ioMisc = (Ptr) new_mark;
		if((err = PBSetEOF(&p,0)) != noErr) {
			cvt_err(err);
			return(-1);
		}
/*		if((err = PBAllocContig(&p,0)) != noErr) {
			cvt_err(err);
			return(-1);
		}*/
	}
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	p.ioPosOffset = new_mark;
	p.ioPosMode = fsFromStart;
	if((err = PBSetFPos(&p,0)) != noErr) {
		cvt_err(err);
		return(-1);
	}
	errno = 0;
	return(p.ioPosOffset);
}

int unlink(name)
char *name;
{	int fd, err;
	char *nm;
	HParamBlockRec p;

	nm = cvt_fnm(name);	/* convert filename to Mac type name */
	CtoPstr(nm);
	fsetup(&p);	/* try to delete it, whether it is there or not. */
	p.fileParam.ioNamePtr = (StringPtr) nm;
	if((err = PBHDelete(&p,0)) != noErr && err != fnfErr) return(cvt_err(err));
	return;
}

/* Console read and write routines */

static int con_write(buf,size)
unsigned  size;
char *buf;
{
	while(size--) putp(*buf++);
	return(size);
}

static int con_read(buf,size)
unsigned size;
char *buf;
{
	unsigned n;
	int p;

	
	n = 0;
	do {
		p = rawgetc();
#ifdef O_META
		if(p & 0x7f) p &= 0x7f;		/* was normal ascii char */
#endif
		*buf++ = p;
		n++;
	} while (rawchkc() && n <= size);
	return(n);
}


/* This didn't seem to be any place else */

int abs(n)
int n;
{
	return(n >= 0 ? n : -n);

}

/* Simplified stat() routine emulates what is needed most. */

int stat(fname,buf)
char *fname;
struct stat *buf;
{
	CInfoPBRec p;
	char *nm;
	
	nm = cvt_fnm(fname);
	CtoPstr(nm);
	bzero(&p,sizeof(CInfoPBRec));
	p.hFileInfo.ioCompletion = 0;
	p.hFileInfo.ioNamePtr = (StringPtr) nm;
	p.hFileInfo.ioFVersNum = 0;
	p.hFileInfo.ioFDirIndex = 0;
	p.hFileInfo.ioVRefNum = cur_vol;
	p.hFileInfo.ioDirID = cur_dir;
	
	switch (PBHGetFInfo(&p,0)) {
		
		case noErr : 	errno = 0;
						break;
		case nsvErr:
		case paramErr:
		case bdNamErr :
		case fnfErr:	errno = ENOENT;
						break;
		case ioErr:		errno = EIO;
						break;
		default :		errno = ENOENT;
						break;
	}
	buf->st_dev = p.hFileInfo.ioVRefNum + 1;	/* don't want 0 */
	buf->st_ino = p.hFileInfo.ioDirID;
	buf->st_size = p.hFileInfo.ioFlLgLen;
	buf->st_mtime = p.hFileInfo.ioFlMdDat;
	buf->st_mode = (p.hFileInfo.ioFlAttrib & 0x10) ? S_IFDIR : 0;
	PtoCstr(nm);
	return(errno == 0 ? 0 : -1);
}

/* Directory related routines. Jove keeps track of the true Volume (disk) number and
   directory number, and avoids "Working Directory Reference Numbers", which are
   confusing. */
   
static int getdir()	/* call this only once, during startup. */
{
	WDPBRec p;

	p.ioCompletion = 0;
	p.ioNamePtr = 0;
	if(PBHGetVol(&p,0) != noErr) return(-1);	/* BIG trouble */
	cur_vol = p.ioWDVRefNum;
	cur_dir = p.ioWDDirID;
	SFSaveDisk = 0 - cur_vol;	/* these are for SF dialogs */
	CurDirStore = cur_dir;
}

static int setdir(vol,dir)
long dir;
{
	WDPBRec p;

	p.ioCompletion = 0;
	p.ioNamePtr = 0;
	p.ioVRefNum = vol;
	p.ioWDDirID = dir;
	if(PBHSetVol(&p,0) != noErr) return(-1);
	cur_vol = vol;
	cur_dir = dir;
	SFSaveDisk = 0 - vol;	/* these are for SF dialogs */
	CurDirStore = dir;


}

int chdir(dir)
char *dir;
{
	DirInfo d;
	WDPBRec p;
	char *t;
	char *nm; 
	
	if(strcmp(dir,"/") == 0) return(-1); /* There is no root... */
	nm = malloc(strlen(dir) + 2);
	if(nm == 0) return(-1);
	
	strcpy(nm,dir);
	t = nm;
	while(*t) {
		if(*t == '/') *t = ':';
		t++;
	}
	t = nm;	
	while(*t == ':') t++;	/*get rid of initial slashes */
	strcat(nm,":");
	CtoPstr(t);

	d.ioCompletion = 0;			/* get the directory number */
	d.ioNamePtr = (StringPtr) t;
	d.ioVRefNum = cur_vol;
	d.ioFDirIndex = 0;
	d.ioDrDirID = 0;
	PBGetCatInfo(&d,0);
	free(nm);
	if(d.ioResult != noErr || ((d.ioFlAttrib & 0x10) == 0)) return(-1);
	if(setdir(d.ioVRefNum,d.ioDrDirID) < 0)return(-1);
	return(0);
}

/* Scandir returns the number of entries or -1 if the directory cannoot
   be opened or malloc fails. */

int scandir(dir, nmptr, qualify, sorter) /* this function has NOT been debugged */
char	*dir;
char	***nmptr;
int	(*qualify)();
int	(*sorter)();
{
	HParamBlockRec fb;
	DirInfo d;
	long DirID;
	char	**ourarray, *nm, *t, buf[50];
	Str255 buf1;
	unsigned int	len, nalloc = 10,
			nentries = 0;

	if(strcmp(dir,"/") == 0) return(-1); /* There is no root... */
	nm = malloc(strlen(dir) + 2);
	if(nm == 0) return(-1);
	
	strcpy(nm,dir);
	t = nm;
	while(*t) {
		if(*t == '/') *t = ':';
		t++;
	}
	t = nm;	
	while(*t == ':') t++;	/*get rid of initial slashes */
	strcat(nm,":");
	CtoPstr(t);

	d.ioCompletion = 0;			/* get the directory number */
	d.ioNamePtr = (StringPtr) t;
	d.ioVRefNum = cur_vol;
	d.ioFDirIndex = 0;
	d.ioDrDirID = 0;
	PBGetCatInfo(&d,0);
	free(nm);
	fb.fileParam.ioDirID = DirID = d.ioDrDirID;
	fb.fileParam.ioCompletion = (long) 0;
	fb.fileParam.ioVRefNum = cur_vol;
	fb.fileParam.ioFVersNum = 0;
	fb.fileParam.ioNamePtr = buf1;

	if ((ourarray = (char **) malloc(nalloc * sizeof (char *))) == 0)
memfail:	complain("[Malloc failed: cannot scandir]");
	while (1) {
		fb.fileParam.ioFDirIndex = nentries;
		fb.fileParam.ioVRefNum = cur_vol;
		fb.fileParam.ioDirID = DirID;
		if(PBHGetFInfo(&fb,0) != noErr) break;	/* we are done, then */
		len = (char) *fb.fileParam.ioNamePtr;	/* pascal style string */
		strncpy(buf,fb.fileParam.ioNamePtr +1,len);
		buf[len] = '\0';

		if (qualify != 0 && (*qualify)(buf) == 0)
			continue;
		if (nentries == nalloc) {
			ourarray = (char **) realloc((char *) ourarray, (nalloc += 10) * sizeof (char *));
			if (ourarray == 0)
				goto memfail;
		}
		ourarray[nentries] = (char *) malloc(strlen(buf)+1);
		null_ncpy(ourarray[nentries], buf, strlen(buf));
		nentries += 1;
	}
	if ((nentries + 1) != nalloc)
		ourarray = (char **) realloc((char *) ourarray,
					((nentries + 1) * sizeof (char *)));
	if (sorter != 0)
		qsort((char *) ourarray, nentries, sizeof (char **), sorter);
	*nmptr = ourarray;
	ourarray[nentries] = 0;		/* guaranteed 0 pointer */
	return nentries;
}


char *getwd()
{
	DirInfo d;
	static char ret[255];
	char nm[50], tmp[255];
	
	ret[0] = '\0';
	d.ioDrDirID = cur_dir;
	for(;;) {
		d.ioCompletion = 0;
		d.ioNamePtr = (StringPtr) nm;
		d.ioVRefNum = cur_vol;
		d.ioFDirIndex = -1;

		PBGetCatInfo(&d,0);
		if(d.ioResult != noErr) return(0);
		PtoCstr((char *) nm);
		strcpy(tmp,ret);
		strcpy(ret,"/");
		strcat(ret,nm);
		strcat(ret,tmp);
		if(d.ioDrDirID == 2) break;	/* home directory */
		d.ioDrDirID = d.ioDrParID;
	} 
	return(ret);
}

static char *gethome()		/* this will be startup directory */
{
	static char *ret = 0;
	

	if(ret == 0) {
		char *item = getwd();
		ret = malloc(strlen(item)+1);
		strcpy(ret,item);
	}
	return(ret);
}	

	

/* Routines that put up and manipulate the "About Jove" dialog. */


/* (ORIGINALLY IN) about_j.c. */


#define DLOGNAME "\pABOUT_JDLOG"

#define DONE_ITEM 1
#define LIST_ITEM 2


#define DWIDTH 460		/* there should be an easy way to get this */
#define DHEIGHT 240		/* from the resource file! */

WindowPtr makedisplay();
ListHandle makelist();


static WindowPtr theWindow;
static ListHandle theList;
static Rect theListRect;
static EventRecord theEvent;



static void about_j()
{
	void do_list(), do_events();
	
	WindowPtr OldWindow;
	
	GetPort(&OldWindow);
	
	if((theWindow = makedisplay()) == 0) return;
	SetPort(theWindow);
	if(theList = makelist()) {
		LActivate(1,theList);
		do_list();
		ShowWindow(theWindow);
		do_events();
	}
	SetPort(OldWindow);
	LDispose(theList);
	DisposDialog(theWindow);
	
	return;
}
		
	
static WindowPtr makedisplay()
{
	static int dlogid = 0;
	DialogPtr theDialog;
	Handle theHandle;
	Handle theResource;
	Str255 buf;
	long itemType;
	Rect theRect;
	short dh,dv;	/* to center dialog on the screen */
	Str255 nostring;
	
	if(dlogid == 0) {
		if((theResource = GetNamedResource('DLOG',DLOGNAME)) == 0)
			return((WindowPtr) 0);
		itemType = 'DLOG';
		GetResInfo(theResource,&dlogid,&itemType,buf);
	}
	
	theDialog = GetNewDialog(dlogid,(long) 0,(WindowPtr) -1);
	strcpy((char *) nostring,"\p");
	ParamText("\pMacJove - Copyright (C) 1986, 1987, 1988 J. Payne, K. Gegenfurtner,",
	"\pK. Mitchum. Portions (C) THINK Technologies, Inc.",nostring,nostring);
	
	dh = screenBits.bounds.left + (screenBits.bounds.right - DWIDTH) / 2;
	dv = screenBits.bounds.top  + (screenBits.bounds.bottom - DHEIGHT) / 2;
	MoveWindow((WindowPtr)theDialog,dh,dv,0);
	ShowWindow((WindowPtr)theDialog);
	
	
	GetDItem(theDialog,LIST_ITEM,&itemType,&theHandle,&theRect);
	theListRect = theRect;
	theListRect.right -= 15;
	((WindowPtr)theDialog)->txFont = FONT;
	((WindowPtr)theDialog)->txSize = TEXTSIZE;
	
	return((WindowPtr) theDialog);
}

static void do_display()		/* draw necessary controls, lines */
{
	Rect rViewF;		/* framing rect for list */
	int offset;
	
	rViewF = theListRect;
	
	rViewF.left--;
	rViewF.top--;
	rViewF.right++;
	rViewF.bottom++;
	FrameRect(&rViewF);
	
	DrawControls(theWindow);

}

static ListHandle makelist()
{
	Point csize;
	Rect dataBounds, rView;	/* list boundaries */	
	
	csize.h = csize.v = 0;
	SetRect(&dataBounds,0,0,1,0);	
	return(LNew(&theListRect,&dataBounds,csize,0,theWindow,0,0,0,1));
}

static void do_list()
{
	void printbind();
	
	int row, col;
	struct cmd *f;
	Str255 buf;
	Point theCell;

	theCell.h = 0;
	
	for(f = commands, row = 0; f->Name; f++, row++) {
		LAddRow(1,row,theList);
		theCell.v = row;

		printbind(f,buf);
		strcat(buf,f->Name);
		LSetCell(buf,strlen((char *)buf),theCell,theList);
		
	}
}
static void printbind(f,buf)
struct cmd *f;
char *buf;
{	
	char c;
	
	if(f->c_map == 0 || (c = f->c_key) == 0x7f) {
		strcpy(buf,"        ");
		return;
	}
	switch(f->c_map) {
		case F_MAINMAP :
			strcpy(buf,"     ");
			break;
		
		case F_PREF1MAP :
			strcpy(buf," ESC ");
			break;
			
		case F_PREF2MAP :
			strcpy(buf,"  ^X ");
			break;
	}
	if(c < ' ') {
		buf[5] = '^';		/* control char */
		c |= 0x40;
	}
	else buf[5] = ' ';
	if(c >= 'a' && c<= 'z') c &= 0x5f;
	buf[6] = c;
	buf[7] = ' ';
	buf[8] = '\0';
}



static pascal Boolean ProcFilter(theDialog,event,itemHit)
DialogPtr theDialog;
EventRecord *event;
int *itemHit;
{
	theEvent = *event;
	if(theEvent.what == keyDown && theEvent.message & charCodeMask == '\r') {
		*itemHit = 1;
		return(TRUE);
	}
	if(theEvent.what == activateEvt && (WindowPtr) theEvent.message == theWindow) {
		LDoDraw(1,theList);
		LActivate(1,theList);
	}
	if(theEvent.what == updateEvt && (WindowPtr) theEvent.message == theWindow) {
		BeginUpdate(theWindow);
		do_display();
		DrawDialog(theWindow);
		LUpdate((GrafPtr) theWindow->visRgn,theList);
		EndUpdate(theWindow);
	}

	return(FALSE);
}
	

void do_events()
{
	int item,done;
	Point p;
	
	done = 0;
	
	while(!done) {
		ModalDialog(ProcFilter,&item);
		switch(item) {
			case DONE_ITEM :
				done = 1;
			case LIST_ITEM :
				p = theEvent.where;
				GlobalToLocal(&p);
				LClick(p,theEvent.modifiers,theList);
		}
	}
}

/* Window and Control related routines. */

/* (ORIGINALLY IN) tcon.c. 
   control handler routines for Jove. K. Mitchum 12/86 */


#define MINC 0
#define MAXC (int)100
#define INITC 0
#define EVENTLIST (mDownMask | keyDownMask )

extern long GetCRefCon();	/* omitted in ControlMgr.h */

static Point p;
static intext;	/* mouse down in jove text */

void docontrols()	/* called from redisplay routines */
{
	void MakeScrollBar(),
		AdjustScrollBar(),
		drawfluff();
	
	Window *w;
	int top;

	w = fwind;
	top = 0;
	do {
		if(w->w_control) HideControl(w->w_control);
		w = w->w_next;
	} while (w != fwind);
	w = fwind;
	do {
		w->w_topline = top;
		if(w->w_control) AdjustScrollBar(w);
		else MakeScrollBar(w);
		ShowControl(w->w_control);
		top += w->w_height;
		w = w->w_next;
	} while(w != fwind);
	Windchange = 0;
	drawfluff();
}	


void MakeScrollBar(w)	/* set up control */
Window *w;
{
	Rect BarRect;
	int wheight, wtop;
	
	WindowPtr window = theScreen;
	wheight = w->w_height;
	wtop = w->w_topline;
	SetRect(&BarRect,window->portRect.right - SCROLLWIDTH + 1,
		window->portRect.top -1 + wtop * HEIGHT,
		window->portRect.right +1,
		window->portRect.top + ((wheight + wtop) * HEIGHT));
		w->w_control = ((char **) NewControl(window,&BarRect,"/psbar",1,INITC,
		MINC,MAXC,scrollBarProc,w));
}

void AdjustScrollBar(w)	/* redo existing control */
Window *w;
{
	int wtop,wheight;
	ControlHandle handle;
	WindowPtr window;
	
	handle = (ControlHandle) w->w_control;
	wtop = w->w_topline;
	wheight = w->w_height;
	window = (*handle)->contrlOwner;

	if(handle == 0) return;

	SizeControl(handle,SCROLLWIDTH,wheight * HEIGHT +1);
	
	MoveControl(handle,window->portRect.right - SCROLLWIDTH + 1,
		window->portRect.top -1 + wtop * HEIGHT);
}

void SetScrollBar(handle)	/* set value of the bar */
ControlHandle handle;
{ 

	SetCtlValue(handle,ltoc());
}



static void dodivider()	/* originally to divide windows, but not enough */
			/* room in between lines, so just put line at bottom */
{
	WindowPtr window;
	PenState pnState;
	
	window = theScreen;
	GetPenState(&pnState);
	MoveTo(0,((MAXROW) * HEIGHT));
	PenSize(1,1);
	LineTo(window->portRect.right,(MAXROW) * HEIGHT);
	SetPenState(&pnState);
	return;
}

static void drawfluff()		/* draw controls and dividers */
{
	WindowPtr window;
	Window *w = fwind;

	window = theScreen;
	DrawControls(window);
	
	dodivider();
}	

void RemoveScrollBar(w)
Window *w;
{
	if(w->w_control) DisposeControl(w->w_control);
	dodivider();		/* erase the divider */
	w->w_control = 0;

}
	
static pascal void DScroll(control,part)
ControlHandle control;
int part;
{
	DownScroll();
	redisplay();
}

static pascal void UScroll(control,part)
ControlHandle control;
int part;
{
	UpScroll();
	redisplay();
}

static pascal void NPage(control,part)
ControlHandle control;
int part;
{	NextPage();
	redisplay();
}

static pascal void PPage(control,part)
ControlHandle control;
int part;
{	PrevPage();
	redisplay();
}

static long npos;	/* number of lines in buffer */

static int ltoc()	/* calculate ctlvalue for line position */
{
	register long ipos;
	register Line	*lp = curbuf->b_first;

	for (npos = 1; lp ; npos++, lp = lp->l_next)  {
		if(lp == curline) ipos = npos;
	}
	return((int) ((ipos * MAXC) / npos));
}

static Line *ctol(ctlv)	/* find buffer line for ctlvalue */
int ctlv;
{
extern char *itoa();
	register long ipos;
	register Line	*lp = curbuf->b_first;

	ipos = (npos * ctlv)/MAXC;
	while (ipos-- && lp->l_next) lp = lp->l_next;
	return(lp);
}

static void doWind(event,window)
EventRecord *event;
WindowPtr window;
{
#define track() TrackControl(whichControl,p,(ProcPtr) 0)

	ControlHandle whichControl;
	Window *jwind, *cwind;
	int notcurwind;
	int cpart;	/* control part */
	int oldval,newval,thumb = 0;
	
	p = event->where;
	intext = 0;
	notcurwind = 0;
	GlobalToLocal(&p);
	
	if(event->what == mouseDown) {
		if((cpart = FindControl(p,window,&whichControl)) == 0) return;
		if((jwind = (Window *) (*whichControl)->contrlRfCon) !=  curwind) {
			notcurwind++;
			cwind = curwind;
			SetWind(jwind);
		}
		switch (cpart) {
			case inUpButton :	TrackControl(whichControl,p,(ProcPtr) DScroll); break;
			case inDownButton :	TrackControl(whichControl,p,(ProcPtr) UScroll); break;
			case inPageUp :		TrackControl(whichControl,p,(ProcPtr) PPage); break;
			case inPageDown :	TrackControl(whichControl,p,(ProcPtr) NPage); break;
			case inThumb :		if(track()) {
									newval = GetCtlValue(whichControl);
										
									if(newval == MAXC) Eof();
									else if(newval == MINC) Bof();
									else SetLine(ctol(newval));
								}
								break;
									
		}
		if(notcurwind) {
			SetWind(cwind);
			redisplay();
		}
		redisplay();	/* again, to set the cursor */
	}
	else {
		if(findtext()) redisplay();
	}
}


static void doGoAway(event,window)
EventRecord *event;
WindowPtr window;
{
	if(TrackGoAway(window,&event->where) == TRUE) Leave();
}

static Window *rtowind(row)	/* return jove window row is in */
int row;
{
	Window *w = fwind;
	
	do {
		if((w->w_topline <= row) && ((w->w_height + w->w_topline) > row))
			return(w);
		w = w->w_next;
	} while(w != fwind);
	return(0);
}

static Line *windtol(w,row)		/* return line for row in window */
Window *w;
int row;
{
	Line *l = w->w_top;
	
	while(row--) if((l = l->l_next) == 0) return(0);
	return(l);
}
	

static int findtext()		/* locate and move the point to match the mouse */
{
	int row,col;
	Window *w;
	Line *l;
	ptoxy(p,&row,&col);
	if((w = rtowind(row)) == 0) return(0);
	if(w != curwind) SetWind(w);
	row -= w->w_topline;		/* now have row number in window */
	if(row >= w->w_height -1) return(0);
	if((l = windtol(w,row)) == 0) return(0);
	if(l->l_dline == 0) return(0);
	this_cmd = LINECMD;
	SetLine(l);		/* Curline is in linebuf now */
	if(w->w_flags & W_NUMLINES) col -= 8;	/* adjust for line numbers */
	if(col < 0) col = 0;
	curchar = how_far(curline, col);
	return(1);
}

	
static int ptoxy(p,row,col)	/* convert Point to terminal x,y coordinate */
Point p;
int *row,*col;
{
	*row = (p.v / HEIGHT);
	*col = (p.h / WIDTH );
	if((*row > MAXROW) || (*col > MAXCOL)) return(ERROR);
	return(0);
}

/* Event-related routines. The Event loop is CheckEvents(), and is called whenever
   a console read occurs or a call to charp(). During certain activities, such as ask(),
   etc. non-keyboard events are ignored. This is set by the variable Keyonly. 
   As an update or activate event generates a call to redisplay(), it is important
   that redisplay() and related routines NOT check for keyboard characters. */
 
/* (ORIGINALLY IN) tevent.c
	event handler for Jove. K Mitchum 12/86 */


#define SYS_ID 100
#define NOFUNC (void (*)()) 0
#define NEVENTS 16
static int firsttime = 0;
extern void doMouse(),dokeyDown(),doUpdate(),doActivate();
static MenuHandle SysMenu;
static void (*eventlist[])() =
{
	NOFUNC, /* nullEvent */
	doMouse,/* mouseDown */
	doMouse, /* mouseUp */
	dokeyDown, /* keyDown */
	NOFUNC, /* keyUp */
	dokeyDown, /* autoKey */
	doUpdate, /* updateEvt */
	NOFUNC, /* diskEvt */
	doActivate, /* activateEvt */
	NOFUNC, /* not  used */
	NOFUNC, /* networkEvt = 10 */
	NOFUNC, /* driverEvt */
	NOFUNC, /* app1Evt */
	NOFUNC, /* app2Evt */
	NOFUNC,	/* app3Evt */
	NOFUNC	/* app4Ev */
};


static void CheckEvents()
{
#define Ticks (long *) 0x16A

	void SetBufMenu(),
		MarkModes();
		
	static EventRecord theEvent;
	static Point Mousep;
	static long time = 0;


	static void (*fptr)();
	
#define HeapEnd (char **) 0x114
#define ApplLimit (char **) 0x130
	static long freesp = 50000;
	
	if(FrontWindow() == window) {
		GetMouse(&Mousep);
		if(PtInRect(Mousep,&r))
			SetCursor(*cross);
		else SetCursor(&arrow);
	}
	
	SystemTask();
	if(EventCmd && !Keyonly) return;
	if(Bufchange != 0) SetBufMenu();
	if(Modechange != 0) MarkModes();
	while(GetNextEvent(everyEvent,&theEvent)) {
		if ((theEvent.what < NEVENTS) && (fptr = eventlist[theEvent.what])) {
			(*fptr)(&theEvent);
		}
		SystemTask();
	}
	if((*ApplLimit - *HeapEnd) < freesp) {
		freesp = (((*ApplLimit - *HeapEnd)/5000) * 5000);
 		SysBeep(120);
 		s_mess("WARNING: Free memory down to %D bytes",freesp);
 		freesp -= 5000;
		if(!Macmode) redisplay();
	}
	if((*Ticks - time) > 3600) {
		time = *Ticks;
		UpdModLine = YES;
		redisplay();
	}

}


static void InitSysMenu()
{
	void InitLocalMenus();
	
	SysMenu = NewMenu(SYS_ID,"\p\24");
	AppendMenu(SysMenu,"\pAbout Jove");
	AddResMenu(SysMenu,'DRVR');
	InsertMenu(SysMenu,0);
	InitLocalMenus();
	DrawMenuBar();
}

extern void doWind(),doGoAway(),doSysMenu(),doSysClick();
#define NMEVENTS 7
static void (*mouselist[])() =
{
	NOFUNC, /* inDesk */
	doSysMenu, /* inMenuBar */
	doSysClick, /* inSysWindow */
	doWind, /* inContent */
	NOFUNC, /* inDrag */
	NOFUNC, /* inGrow */
	doGoAway /* inGoAwa */
};


static void doMouse(event)
EventRecord *event;
{
	WindowPtr theWindow;
	int wpart;
	void (*fptr)();
	
	if(Keyonly) {
		if(event->what == mouseDown) SysBeep(2);
		return;
	}
	wpart = FindWindow(event->where,&theWindow);	
	if ((wpart < NMEVENTS) && (fptr = mouselist[wpart])) {
		(*fptr)(event,theWindow);
	}

}

static void doSysMenu(event,window)
EventRecord *event;
WindowPtr window;
{
	void ProcMenu();
	
	int Menu,Item;
	long result = MenuSelect(event->where);
	Menu = (result >> 16) & 0xffff;
	Item = result & 0xffff;
	if(Item == 0) return;	/* no choice made */
	
	if(Menu == SYS_ID) {			/* apple menu */
		Str255 Name;
		GrafPtr Port;
		
		if(Item == 1) about_j();
		else {		
			GetItem(SysMenu,Item,Name);
			GetPort(&Port);
			OpenDeskAcc(Name);
			SetPort(Port);
		}
	}
	else ProcMenu(Menu,Item);
	HiliteMenu(0);
	EventCmd = 1;

	menus_on();
	return;
	
}
		
static void doSysClick(event,window)
EventRecord *event;
WindowPtr window;
{
	SystemClick(event,window);
}		
		
	
	
static void doUpdate(event)
EventRecord *event;
{
	WindowPtr theWindow, oldPort;
	
	theWindow = (WindowPtr) event->message;

	if(firsttime == 0) {		
		firsttime++;
		BeginUpdate(theWindow);
		EndUpdate(theWindow);
		return;
	}
		
/*	redisplay(); */
	GetPort(&oldPort);
	SetPort(theWindow);
	BeginUpdate(theWindow);
	if(theWindow == theScreen && Windchange == 0
		&& Keyonly == 0) {
		Placur(0,0);
		drawfluff();
		cl_scr(1);
		redisplay();
	}
	EndUpdate(theWindow);

	SetPort(oldPort);
}

static void doActivate(event)
EventRecord *event;
{
	WindowPtr theWindow;
	ControlHandle control;
	int hilite;
	
	theWindow = (WindowPtr) event->message;
	SetPort(theWindow);
	if(event->modifiers & activeFlag) {
		hilite = 0;
	}
	else hilite = 255;
	for(control = (ControlHandle) (((WindowPeek) theWindow)->controlList);
		 (control != 0); control = (*control)->nextControl) {
		 	HiliteControl(control,hilite);
	}
}
		
/* Keyboard routines. The Option key was formerly used as a meta key.
   However, to take advantage of the full (non-ASCII) character set,
   this was removed. The corresponding code is ifdeffed O_META. */
   
/* (ORIGINALLY IN) tkey.c
   keyboard routines for Macintosh. K Mitchum 12/86 */

extern jmp_buf auxjmp;

static nchars = 0;
static char charbuf[MCHARS];
/* the following kludges a meta key out of the option key by
sending an escape sequence back to the dispatch routines. this is
not elegant but it works, and doesn't alter escape sequences for
those that prefer them. to remap the control or meta keys,
see mackeys.h. */

static void dokeyDown(event)
EventRecord *event;
{
	unsigned mods;
	register c;
	static int cptr = 0;
	
	if(MCHARS - nchars < 2) return;
		
	c  = (char)((event->message)&(charCodeMask));

	if(c == '`') c = '\033';	/* for those used to escapes */
	
	mods = event->modifiers;

#ifdef O_META	
	if (mods & (optionKey | cmdKey)) {
#else
	if (mods & (cmdKey)) {
#endif
		if(mods & shiftKey) 
			c  = sh_keycodes[(((event->message)&(keyCodeMask))>>8)];
		else	
			c  = nsh_keycodes[(((event->message)&(keyCodeMask))>>8)];
#ifdef O_META			
		if(mods & optionKey) {		/* make escape sequence */
			if(mods & cmdKey) c &= 0x1f;
			charbuf[cptr++] = '\033';
			cptr &= NMASK;		/* zero if necessary */
			nchars++;
		}
		else 
#endif
		{	/* command key (control key) */
			if((c == '2') || (c == '\\')) c = 0;	/* so we have a null char */
			if(c != '`') c &= 0x1f;		/* make a control char */
		}
	}
	charbuf[cptr++] = c;
	cptr &= NMASK;
	nchars++;
}

static int rawgetc()
{
	static int cptr = 0;
	register c;

	if(EventCmd) longjmp(auxjmp,0);	
	while(nchars <= 0) {
		nchars = 0;
		if(EventCmd) longjmp(auxjmp,0);
		CheckEvents();	/* ugh! WAIT for a character */
	}
	nchars--;
	c = charbuf[cptr++];
	cptr &= NMASK;		/* zero if necessary */
	return(c);
}

int rawchkc()
{
	if(EventCmd) longjmp(auxjmp,0);
	if(nchars == 0) CheckEvents();	/* this should NOT be necessary! */	
	return(nchars > 0);
}

/* Routines for calling the standard file dialogs, when macify is ON. If the user
   changes the directory using the file dialogs, Jove's notion of the current directory
   is updated. */

				
/* (ORIGINALLY IN) tmacf.c. K. Mitchum 12/86.
   Macify routines for jove. */

int CurrentVol;			/* see tfile.c */


#define TYPES  -1

static Point px = {100,100};
static char pmess[] = "\pSave file as: ";

static pascal Boolean Ffilter(p)
FileParam *p;
{
	if(p->ioFlFndrInfo.fdType == 'APPL') return TRUE;
	PtoCstr((char *) p->ioNamePtr);
	if(strcmp(p->ioNamePtr,d_tempfile) == 0) {
		CtoPstr((char *) p->ioNamePtr);
		return TRUE;
	}
	CtoPstr((char *) p->ioNamePtr);
	return FALSE;
}

static void check_dir()
{
	if(cur_vol != 0 - SFSaveDisk || cur_dir != CurDirStore) {
		setdir(0 - SFSaveDisk, CurDirStore);
		UpdModLine = YES;	/* make sure jove knows the change */
		Modechange++;
		setCWD(getwd());
	}
}

char *gfile(namebuf)	/* return a filename to get */
char *namebuf;
{
	SFReply frec;
	char ans[FILESIZE];
	
	SFSaveDisk = 0 - cur_vol;	/* in case a Desk Accessory changed them */
	CurDirStore = cur_dir;
	SFGetFile(px,0L,Ffilter,TYPES,0L,0L,&frec);
	check_dir();	/* see if any change, set if so */
	if(frec.good) {
		EventRecord theEvent;
		while(GetNextEvent(updateMask,&theEvent) == 0);
		doUpdate(&theEvent);
		PtoCstr((char *)frec.fName);
		strcpy(ans,frec.fName);
		CtoPstr((char *)frec.fName);
		PathParse(ans,namebuf);
		return(namebuf);
	}
	return(char *) 0;	
}

char *pfile(namebuf)
char *namebuf;
{
	SFReply frec;
	char *t, *nm;
	SFSaveDisk = 0 - cur_vol;	/* in case a Desk Accessory changed them */
	CurDirStore = cur_dir;
	strncpy(namebuf,filename(curbuf),63);
	nm = cvt_fnm(namebuf);
	CtoPstr(nm);
	SFPutFile(px,pmess,nm,0L,&frec);
	check_dir();	/* see if any change, set if so */
	if(frec.good) {
		EventRecord theEvent;
		while(GetNextEvent(updateMask,&theEvent) == 0);
		doUpdate(&theEvent);
		t = (char *)frec.fName;
		PtoCstr((char *)frec.fName);
		while(*t == ':') t++;	/* convert to unix style */
		nm = t;
		while(*nm) {
			if(*nm == ':') *nm = '/';
			nm++;
		}
		PathParse(t,namebuf);
		return(namebuf);
	}
	return(char *) 0;	
}	


/* getArgs() returns an argument list based on documents clicked on by the user. */

int getArgs(avp)
char ***avp;
{
	int argc, nargs, type, old_vol;
	long old_dir;
	char **argv;
	char *pathname;	
	AppFile p;
	WDPBRec d;
	
	old_vol = cur_vol;
	old_dir = cur_dir;
	
	CountAppFiles(&type,&nargs);
	if(nargs > 0) {	/* files to open... */
		argv = (char **) malloc((nargs + 2) * sizeof(char *)); 
		for(argc = 1; argc <= nargs; argc++) {
			GetAppFiles(argc,&p);
			if(type == 0) {
				PtoCstr((char *)p.fName);
				d.ioCompletion = 0;
				d.ioNamePtr = 0;
				d.ioVRefNum = p.vRefNum;
				d.ioWDIndex = 0;
				PBGetWDInfo(&d,0);
				cur_vol = d.ioWDVRefNum;
				cur_dir = d.ioWDDirID;
				pathname = getwd();
				argv[argc] = malloc(strlen((char *)p.fName) + strlen(pathname) + 2);
				strcpy(argv[argc],pathname);
				strcat(argv[argc],"/");
				strcat(argv[argc],(char *)p.fName);
			}
			ClrAppFiles(argc);
		}
		if(type != 0) argc = 1;
	}
	else {
		argv = (char **) malloc(2 * sizeof(char*));
		argc = 1;
	}
	argv[0] = "jove";

	argv[argc] = 0;
	*avp = argv;
	cur_dir = old_dir;
	cur_vol = old_vol;	
	return(argc);
}

/* Limited version of getenv() */

char *getenv(item)
char *item;
{
	char *ret = 0, *str = 0;
	
	if(strcmp(item,"CWD") == 0) str = getwd();
	if(strcmp(item,"HOME") == 0) str = gethome();
	if(str) {
		ret = malloc(strlen(str) + 1);
		strcpy(ret,str);
	}
	return(ret);
}

char *mktemp(name)
char *name;
{
	return name;
}


/* Menu routines. The menus items are set up in a similar manner as keys, and
   are bound prior to runtime. See menumaps.txt, which must be run through setmaps.
   Unlike keys, menu items may be bound to variables, and to buffers. Buffer binding
   is only done at runtime. */	
		
static void InitLocalMenus()
{
	void InitMenu(),
		make_edits();
	
	int i;
	for(i = 0; i < NMENUS; i++) {
		InitMenu(&Menus[i]);
		if(i == 0) make_edits(Menus[i].menu_id + 1);
	}
}

static void InitMenu(M)
struct menu *M;
{
	int i;
	data_obj *d;
	char *name;

	if(M->menu_id == 0) return;
	M->Mn = NewMenu(M->menu_id,CtoPstr(M->Name));
	PtoCstr(M->Name);

	for(i = 0; i < NMENUITEMS; i++) {
		d = (M->m[i]);
		if(d == 0) break;	/* last item... */ 
		switch (d->Type & TYPEMASK) {
			case (STRING) : 
				AppendMenu(M->Mn,CtoPstr(d->Name));
				PtoCstr(d->Name);
				break;
			case (VARIABLE) :
				SetItemMark(M->Mn,i + 1, 0x12);
			case (FUNCTION) :
				CtoPstr(name = ((data_obj *) d)->Name);
				AppendMenu(M->Mn,name);
				PtoCstr(name);
		}
	}
	InsertMenu(M->Mn,0);
}

static void ProcMenu(menuno,itemno)
int menuno,itemno;
{
	void MacSetVar();
	
	int i;
	data_obj *d;

	for(i = 0; i < NMENUS && Menus[i].menu_id != menuno; i++);
	if(i < NMENUS) {	/* found the menu */
		itemno--;
		d = Menus[i].m[itemno];
		switch(d->Type & TYPEMASK) {
			case FUNCTION :
				ExecCmd((data_obj *) d);
				break;
			case BUFFER :
					SetABuf(curbuf);
					tiewind(curwind,(Buffer *) d);
					SetBuf((Buffer *) d);
				break;
			case VARIABLE :
					MacSetVar((struct variable *) d,i,itemno);
				break;
			default :
				break;
		}
		
	}
}

		

			
	

static void make_edits(menu)	/* add dummy edit menu */
int menu;
{
	MenuHandle M;
	int item;
	char *fname;
	
	M = NewMenu((menu),"\pEdit");
	AppendMenu(M,"\pUndo/Z;(-;Cut/X;Copy/C;Paste/V;Clear;Select All;(-;Show Clipboard");
	InsertMenu(M,0);
	DisableItem(M,0);
}

void menus_off()
{
	int i;
	
	if(Keyonly || EventCmd) return;
	DisableItem(SysMenu,0);
	for(i = 0; i < NMENUS; i++) 
		if(Menus[i].Mn) DisableItem(Menus[i].Mn,0);
	Keyonly = 1;
	DrawMenuBar();
}

void menus_on()
{
	int i;
	
	if(Keyonly == 0) return;
	EnableItem(SysMenu,0);
	for(i = 0; i < NMENUS; i++)
		if(Menus[i].Mn) EnableItem(Menus[i].Mn,0);
	Keyonly = 0;
	DrawMenuBar();
}

static char *BufMPrint(b,i)
Buffer *b;
{
	char *p;
	char *nm = filename(b);
	char t[35];
	
	if(strlen(nm) > 30) { 
		strcpy(t,"...");
		strcat(t,nm + strlen(nm) - 30);
	}
	else strcpy(t,nm);
	nm = t;
	while(*nm) {
		switch(*nm) {	/* ugh... these are metacharacter for Menus */
			case '/' : *nm = ':'; break;
			case '^' :
			case '!' :
			case '<' :
			case '(' :
			case ';' : *nm = '.'; break;	/* that will confuse everybody */
		}
		nm++;
	}
	p = sprint("%-2d %-11s \"%-s\"",i,b->b_name,t);
	return(p);
}

static void SetBufMenu()
{
	register Buffer *b;
	data_obj *d;
	int i,j,stop;
	struct menu *M;

	Bufchange = 0;
	for(i = 0; i < NMENUS && strcmp(Menus[i].Name,"Buffer"); i++);
	if(i < NMENUS) {
		M = &Menus[i];
		for(j = 0; j < NMENUITEMS && (d = Menus[i].m[j]) && (d->Type & TYPEMASK) != BUFFER; j++);
		if(j < NMENUITEMS) {
			for(i = j, b = world; i < NMENUITEMS && b != 0; i++, b = b->b_next) {
			
				if(M->m[i] == 0) 
					AppendMenu(M->Mn,CtoPstr(BufMPrint(b,i-j+1)));	/* add the item */
				else
					SetItem(M->Mn,i + 1,CtoPstr(BufMPrint(b,i-j+1)));	/* or change it */
				M->m[i] = (data_obj *) b;
			}
			stop = i;
			  /* out of buffers? */
			for(;i < NMENUITEMS && M->m[i];i++) {
				DelMenuItem(M->Mn,stop + 1);	/* take off last item */
				M->m[i] = 0;
			}
		}
	}
	return;
}

static void MacSetVar(vp,mnu,itm)	/* Set a variable from the menu */
struct variable *vp;	/* Liberally taken from SetVar() in extend.c */
int mnu,itm;
{
	void MarkVar();
	
	char *prompt;

	prompt = sprint("Set %s: ", vp->Name);
	switch (vp->v_flags & V_TYPEMASK) {
	case V_BASE10:
	case V_BASE8:
	    {
	    	int	value;

		value = ask_int(prompt, ((vp->v_flags & V_TYPEMASK) == V_BASE10)
					  ? 10 : 8);
		*(vp->v_value) = value;
	    	break;
	    }
	case V_BOOL:	/* toggle the value */
		*(vp->v_value) = (*vp->v_value == ON ? OFF : ON);
		MarkVar(vp,mnu,itm);
		break;
	case V_FILENAME:
	case V_STRING:
	    {
		char	*str;

	    	/* Do_ask() so you can set string to "" if you so desire. */
	    	str = do_ask("\r\n", (int (*)()) 0, (char *) vp->v_value, prompt);
	    	if (str == 0)
			str = NullStr;
	    	strcpy((char *) vp->v_value, str);
		/* ... and hope there is enough room. */
	    	break;
	    }
	case V_CHAR:
		f_mess(prompt);
	    	*(vp->v_value) = addgetc();
		break;	    	
	}

	if (vp->v_flags & V_MODELINE)
		UpdModLine = YES;
	if (vp->v_flags & V_CLRSCREEN) ClAndRedraw();
	if (vp->v_flags & V_TTY_RESET) tty_reset(); /* probably none on a Mac */
	return;
}

static void MarkModes()
{
	int mnu,itm,checked;
	data_obj *d;
	
	Modechange = 0;
	for(mnu = 0; mnu < NMENUS; mnu++) 
		for(itm = 0; itm < NMENUITEMS; itm++) {
			if((d = Menus[mnu].m[itm]) == 0) break;
			if((d->Type & (MAJOR_MODE | MINOR_MODE)) ||
				 ((d->Type & TYPEMASK) == BUFFER)){
				if(d->Type & (MAJOR_MODE))
					checked = (curbuf->b_major == (d->Type >> 8)) ? 1 : 0;
				else if(d->Type & (MINOR_MODE))
					checked = (curbuf->b_minor & (d->Type >> 8)) ? 1 : 0;
				else 
					checked = (d == (data_obj *) curbuf) ? 1 : 0;
				CheckItem(Menus[mnu].Mn, itm + 1, checked);
			}
		}
}
	
void MarkVar(vp,mnu,itm)	/* mark a boolean menu item */
struct variable *vp;
int mnu,itm;
{	
	int checked;
	if(mnu == -1) {		/* we don't know the item... slow */
		int found;
		for(mnu = 0, found = 0; (mnu < NMENUS) && !found; mnu++) {
			for(itm = 0; (itm < NMENUITEMS); itm++)
				if((struct variable *) (Menus[mnu].m[itm]) == vp) {
					found++;
					break;
				}
			if(found) break;
		}
		if(!found) return;
	}
	checked = (*(vp->v_value) == ON);
	CheckItem(Menus[mnu].Mn, itm + 1, checked);
}

static void MarkAllVar()	/* slow, but only do it once */
{
	int mnu,itm;
	data_obj *d;
	for(mnu = 0; mnu < NMENUS; mnu++) 
		for(itm = 0; itm < NMENUITEMS; itm++) {
			if((d = Menus[mnu].m[itm]) == 0) break;
			if((d->Type & TYPEMASK) == VARIABLE)
				MarkVar((struct variable *)Menus[mnu].m[itm],mnu,itm);
		}
}


/* Screen routines and driver. The Macinitosh Text Edit routines are not utilized,
   as they are slow and cumbersome for a terminal emulator. Instead, direct QuickDraw
   calls are used. The fastest output is obtained writing a line at a time, rather
   than on a character basis, so the major output routine is writechr(), which takes
   a pascal-style string as an argument. See bufputc() in screen.c. */

void Placur(line,col)
int line, col;
{
	CapCol = col;
	CapLine = line;
	putcurs(line,col,ON);
}

void NPlacur(line,col)
int line, col;
{
	CapCol = col;
	CapLine = line;
	putcurs(line,col,OFF);
}

void i_lines(top, bottom, num)
int top, bottom, num;
{
	Placur(bottom - num + 1, 0);
	dellines(num,bottom);
	Placur(top, 0);
	inslines(num,bottom);
}

void d_lines(top, bottom, num)
int top, bottom, num;
{
	Placur(top, 0);
	dellines(num,bottom);
	Placur(bottom + 1 - num, 0);
	inslines(num,bottom);
}


void clr_page()
{
	void wipescreen();
	
	wipescreen();
}

void clr_eoln()
{
	void wipeline();
	
	wipeline();
}

void SO_on()
{
	void HLmode();
	
	HLmode(1);
}

void SO_off()
{
	void HLmode();
	
	HLmode(0);
}

	
/* (ORIGINALLY IN) tn.c   */
/* window driver for MacIntosh using windows. */
/* K. Mitchum 9/86 */



/*#define VARFONT*/
#ifdef VARFONT
static height,width,theight,twidth,descent;
#else
#define height HEIGHT
#define width WIDTH
#define theight THEIGHT
#define twidth TWIDTH 
#define descent DESCENT
#endif

static trow,tcol, insert, tattr, cursor;
static Rect cursor_rect;


static Rect  vRect;
static WindowRecord myWindowRec;
static Rect myBoundsRect;


#define active() SetPort(theScreen)
/*#define active()*/
#define maxadjust(r) OffsetRect(r,0,2);
static void tn_init()
{
	void INSmode(),
		init_slate();
	
	HLmode(0);
	INSmode(0);
	init_slate();
	ShowPen();
}

static void wipescreen()	/* clear and home function */
{
	Rect r;
	
	active();
	SetRect(&r, 0,0,WINDWIDTH,WINDHEIGHT);
	EraseRect(&r);
	cursor = OFF;
	putcurs(0,0);
	drawfluff();
}

static void putcurs(row,col,vis)
unsigned row, col, vis;
{
/*	if(row > MAXROW || col > MAXCOL) return(ERROR);*/
/*	if(row != trow || col != tcol) */{
		active();
		curset(OFF);
		if(row == MAXROW)
			MoveTo(col * width, (row  +1) * height + 2 -descent );
		else
			MoveTo(col * width, (row  +1) * height - descent);
		trow = row;
		tcol = col;
		curset(vis);
	}
}

static void curset(desired)
{
	if(cursor != desired) {
		SetRect(&cursor_rect, tcol * width, (trow) * height , (tcol + 1) * width - 1, (trow +1) * height -1);
		if(trow == MAXROW) maxadjust(&cursor_rect);
		InvertRect(&cursor_rect);
		cursor = desired;
	}
}


void putp(p)			/* put one character, advance cursor */
int p;
{
	static Rect r;
	static RgnHandle updateRgn;

	active();
	curset(OFF);
	if(insert) {
		updateRgn = NewRgn();
		SetRect(&r, tcol * width, trow * height, WINDWIDTH, (trow +1) * height -1);
		if(trow == MAXROW) maxadjust(&r);
		ScrollRect(&r, width, 0, updateRgn);
		DisposeRgn(updateRgn);
	}
	if(p == '0') p = 0xAF;	/* slashed zero */
	DrawChar(p);
	if(tcol >= MAXCOL) putcurs(trow,MAXCOL);
	else putcurs(trow,tcol +1);
}

static void wipeline()
{
		static Rect r;
		
		active();
		cursor = OFF;
		SetRect(&r, tcol * width, trow * height, WINDWIDTH, (trow +1) * height);
		if(trow == MAXROW) maxadjust(&r);
		EraseRect(&r);
		curset(ON);
}

static void delchars()
{
		static Rect r;
		static RgnHandle updateRgn;
		
		active();
		curset(OFF);
		updateRgn = NewRgn();
		SetRect(&r, tcol * width, trow * height, twidth - width, (trow +1) * height);
		if(trow == MAXROW) maxadjust(&r);
		ScrollRect(&r, 0 - width, 0, updateRgn);
		DisposeRgn(updateRgn);
		curset(ON);
}

static void dellines(n,bot)
int n,bot;
{
	Rect r;
	RgnHandle updateRgn;
	
	updateRgn = NewRgn();
	active();
	curset(OFF);
	SetRect(&r, 0, ((trow) * height), WINDWIDTH, ((bot + 1) * height));
	ScrollRect(&r, 0, 0 - (n * height), updateRgn);
	DisposeRgn(updateRgn);
	putcurs(trow,0);
	
}

static void inslines(n,bot)
int n,bot;
{
	Rect r;
	RgnHandle updateRgn;
	
	updateRgn = NewRgn();
	active();
	curset(OFF);	
	SetRect(&r, 0, trow * height, WINDWIDTH, (bot +1) * height);
	ScrollRect(&r, 0, (n * height), updateRgn);
	DisposeRgn(updateRgn);
	putcurs(trow,0);
}
	 
static void INSmode(new)
int new;
{
	insert = new;
}

static void HLmode(new)
int new;
{
	if(new) tattr = 1;
	else tattr = 0;
}

void writechr(start)
char *start;	/* actually, a Str255 type string */
{
	static Rect r;
	static RgnHandle updateRgn;
	register len;
	register char save;
	
	len = (int) start[0];		/* adjusted 6/86 K. M. in td.c*/

	active();
	curset(OFF);
	if(insert) {
		updateRgn = NewRgn();
		SetRect(&r, tcol * width, trow * height, twidth - width * len, (trow +1) * height -1);
		if(trow == MAXROW) maxadjust(&r);
		ScrollRect(&r, width * len, 0, updateRgn);
		DisposeRgn(updateRgn);
	}
	DrawString(start);
	
	if(tcol >= MAXCOL) putcurs(trow,MAXCOL);
	else putcurs(trow,tcol +len);
}



static void reset(){}
static void blanks(){}
static void cleanup() {}


static void init_slate()
{
	FontInfo f;
	
	extern char *version;
	char *Name = "MacJove ";
	char *Title;
	
	InitGraf(&thePort);
	InitWindows();
	InitCursor();
	InitFonts ();
	InitMenus ();
	InitDialogs ((ProcPtr) 0);		/* no restart proc */
	
	tn_left = screenBits.bounds.left + 3;
	tn_top =  screenBits.bounds.top + 40;

	tn_rows = (screenBits.bounds.bottom - 3 - tn_top) / HEIGHT;
	tn_cols = (screenBits.bounds.right - 3 - tn_left - SCROLLWIDTH) / WIDTH;
	tn_right = tn_left + tn_cols * WIDTH + SCROLLWIDTH;
	tn_bottom = tn_top + tn_rows * HEIGHT + 2;
	tn_cols++;	/* kludge to get jove to use last col */ 

	LI = tn_rows;
	CO = tn_cols;
	MAXROW = tn_rows -1;
	MAXCOL = tn_cols -1;
	
	SetRect(&myBoundsRect,tn_left,tn_top,tn_right,tn_bottom);

	Title = sprint("%s%s",Name,version);
	theScreen = NewWindow(&myWindowRec, &myBoundsRect,CtoPstr(Title),
		1,noGrowDocProc,(WindowPtr) -1, 1, (long) 0);



	SetPort(theScreen);

/*	SetOrigin(-3,-1);*/
	(theScreen)->txFont = FONT;
	(theScreen)->txSize = TEXTSIZE;
	
#ifdef VARFONT	
	GetFontInfo(&f);
		height = f.ascent+f.descent+f.leading;
		width = f.widMax;
		twidth = width * tn_cols;
		theight = height * tn_rows;
		descent = f.descent;
#endif
	
/*	(theScreen)->lineHeight = height;
	(theScreen)->fontAscent = ASCENT;*/
	theScreen->txMode = patCopy;
	theScreen->pnMode = patCopy;
	PenNormal();
	cursor = OFF;
}
#endif /* MAC */



