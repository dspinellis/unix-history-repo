/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/


/* (C) 1986, 1987, 1988 Ken Mitchum. This code is intended only for use with Jove. */

#include "tune.h"
#ifdef	MAC
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

extern struct menu Menus[NMENUS];

private	EventRecord the_Event;

private void SetBounds proto((void));
private void Set_std proto((void));
private void Reset_std proto((void));
private bool is_dir proto((char *));
private bool findtext proto((void));

/* keycodes (from Inside MacIntosh I-251). because of changes with
the MacPlus, there are some duplicate codes between cursor keys and
keypad keys. these can be deciphered by the corresponding character
codes, which are different. this table simply translates a keycode
into a character code that is appropriate. */

#define NOKEY (-1)
#define RET 0x0D
#define TAB 0x09
#define BACKSP 0x08
#define ENTERL NOKEY	/* left enter key absent on MacPlus */
#define COMMAND NOKEY	/* will be no translation anyway for these */
#define SHIFT NOKEY
#define CAPSLOCK NOKEY
#define OPTION NOKEY
#define PADDOT '.'		/* PAD period */
#define PAD0 '0'
#define PAD1 '1'
#define PAD2 '2'
#define PAD3 '3'
#define PAD4 '4'
#define PAD5 '5'
#define PAD6 '6'
#define PAD7 '7'
#define PAD8 '8'
#define PAD9 '9'
#define LEFTCURS 'B'		/* jove only, make like commands */
#define RIGHTCURS 'F'
#define UPCURS 'P'
#define DOWNCURS 'N'
#define PADENTER RET
#define PADMINUS '-'
#define CLEAR 0

private char nsh_keycodes[] = {
	'a','s','d','f','h',						/* 0 - 4 */
	'g','z','x','c','v',						/* 5 - 9 */
	NOKEY,'b','q','w','e',					/* 10 - 14 */
	'r','y','t','1','2',					/* 15 - 19 */
	'3','4','6','5','=',					/* 20 - 24 */
	'9','7','-','8','0',					/* 25 - 29 */
	']','O','u','[','i',					/* 30 - 34 */
	'p',RET,'l','j','\'',					/* 35 - 39 */
	'k',';','\\',',','/',					/* 40 - 44 */
	'n','m','.',TAB,NOKEY,					/* 45 - 49 */
	'`',BACKSP,ENTERL,NOKEY,NOKEY,			/* 50 - 54 */
	COMMAND,SHIFT,CAPSLOCK,OPTION, NOKEY,	/* 55 - 59 */
	NOKEY,NOKEY,NOKEY,NOKEY,NOKEY,			/* 60 - 64 */
	PADDOT,RIGHTCURS,NOKEY,NOKEY,NOKEY,		/* 65 - 69 */
	LEFTCURS,CLEAR,DOWNCURS,NOKEY,NOKEY,	/* 70 - 74 */
	NOKEY,PADENTER,UPCURS,PADMINUS,NOKEY,	/* 75 - 79 */
	NOKEY,NOKEY,PAD0,PAD1,PAD2,				/* 80 - 84 */
	PAD3,PAD4,PAD5,PAD6,PAD7,				/* 85 - 89 */
	NOKEY,PAD8,PAD9
};

private char sh_keycodes[] = {
	'A','S','D','F','H',						/* 0 - 4 */
	'G','Z','X','C','V',						/* 5 - 9 */
	NOKEY,'B','Q','W','E',					/* 10 - 14 */
	'R','Y','T','!','@',					/* 15 - 19 */
	'#','$','^','%','+',					/* 20 - 24 */
	'(','&','_','*',')',					/* 25 - 29 */
	'}','O','U','{','I',					/* 30 - 34 */
	'P',RET,'L','J','\'',					/* 35 - 39 */
	'K',';','|','<','?',					/* 40 - 44 */
	'N','M','>',TAB,NOKEY,					/* 45 - 49 */
	'~',BACKSP,ENTERL,NOKEY,NOKEY,			/* 50 - 54 */
	COMMAND,SHIFT,CAPSLOCK,OPTION, NOKEY,	/* 55 - 59 */
	NOKEY,NOKEY,NOKEY,NOKEY,NOKEY,			/* 60 - 64 */
	PADDOT,RIGHTCURS,NOKEY,NOKEY,NOKEY,		/* 65 - 69 */
	LEFTCURS,CLEAR,DOWNCURS,NOKEY,NOKEY,	/* 70 - 74 */
	NOKEY,PADENTER,UPCURS,PADMINUS,NOKEY,	/* 75 - 79 */
	NOKEY,NOKEY,PAD0,PAD1,PAD2,				/* 80 - 84 */
	PAD3,PAD4,PAD5,PAD6,PAD7,				/* 85 - 89 */
	NOKEY,PAD8,PAD9
};



/* tn.h Modified for variable screen size 11/21/87. K. Mitchum */

#define SCREENSIZE (wc->w_rows * ROWSIZE)
#define FONT monaco
#define TEXTSIZE 9

#define HEIGHT 11
#define WIDTH 6
#define DESCENT 2
#define TWIDTH CO * WIDTH
#define THEIGHT LI * HEIGHT

/* window specs */

#define SCROLLWIDTH 16	/* width of scroll bar control in pixels */
#define WINDWIDTH (wc->w_width - SCROLLWIDTH + 1)	/* local coordinates */
#define WINDHEIGHT (wc->w_height)	/* local coordinates */
#define MAXROW (LI - 1)
#define MAXCOL (CO - 1)


/* for keyboard routines */
#define MCHARS 32	/* must be power of two */
#define NMASK MCHARS (-1)	/* circular buffer */


/***************************************************/

/* these normally reside in "tune.c" which we don't use */

char *CmdDb;	/* see InitMac() */
char *p_tempfile = ".jrecXXX";
char *d_tempfile = ".joveXXX";
char *Joverc = ".joverc";


void putcurs(),curset(),putp(),dellines(),inslines();

private Rect LimitRect;	/* bounds we can't move past */

struct wind_config {
	int w_width;	/* pixel width of the Mac window */
	int	w_height;
	int	w_rows;	/* rows of characters which fit the window */
	int	w_cols;
} wc_std, wc_user, *wc;

private WindowPtr theScreen;

int
	errno;

bool
	Windchange,
	EventCmd,
	Keyonly,
	Bufchange,
	Modechange,
	Macmode = OFF;

/* Initialization Routines. */

void
InitBinds()
{
	struct cmd *c;
	data_obj **p;
	int i;

	p = MainKeys;
	for (i= 0; i < NCHARS; i++) {
		c = (struct cmd *) *p;
		c->c_map = F_MAINMAP;
		c->c_key = i;
		p++;
	}

	p = EscKeys;
	for (i= 0; i < NCHARS; i++) {
		c = (struct cmd *) *p;
		c->c_map = F_PREF1MAP;
		c->c_key = i;
		p++;
	}
	p = CtlxKeys;
	for (i= 0; i < NCHARS; i++) {
		c = (struct cmd *) *p;
		c->c_map = F_PREF2MAP;
		c->c_key = i;
		p++;
	}

}

private	WindowPtr window;
private	Rect r;
private CursHandle cross;

void
InitEvents()
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

void
MacInit()
{
	char *gethome();
	void tn_init();

	tn_init();
	getdir();
	gethome();	/* before anyone changes it */
	CmdDb = malloc(strlen(gethome()) + 10);
	/* ??? better check for CmdDb == NULL -- DHR */
	strcpy(CmdDb,gethome());
	strcat(CmdDb,"/cmds.doc");
	InitBinds();
}


/* dummy routines. */

int dummy() {}

SIGRESULT
(*signal(sig,func)) proto((int))
int sig;
SIGRESULT (*func) proto((int));
{
	return &dummy;
}

void dorecover() {}


/* Surrogate unix-style file i/o routines for Jove. These replace the
   routines distributed in the libraries. They work with Jove, but may
   not be general enough for other purposes. */

#include <io.h>
#define NFILES 10

/* #define fsetup(p) { \
 *	(p).ioCompletion = 0; \
 *	(p).ioVRefNum = cur_vol; \
 *	(p).ioDirID = cur_dir; \
 *	(p).ioFVersNum = 0; \
 * }
 * #define isetup(p) {(p).ioCompletion = 0; (p).ioVRefNum = cur_vol;}
 */

private int cur_vol;	/* Disk or volume number */
private long cur_dir;	/* Directory number */
private int cur_vref;	/* ugh.. Vref for volume + directory */

struct ftab {
	int inuse;	/* 0 = closed 1 = binary 2 = text*/
	int refnum;	/* Mac file reference number */
} ft[NFILES];

private void
fsetup(p)
HParmBlkPtr p;
{
	byte_zero(p,sizeof(HParamBlockRec));
	p->fileParam.ioVRefNum = cur_vol;
	p->fileParam.ioDirID = cur_dir;
	p->fileParam.ioFVersNum = 0;
}

private void
isetup(p)
HIOParam *p;
{
	byte_zero(p,sizeof(HIOParam));
	p->ioVRefNum = cur_vol;
}


/* Kludge to convert Macintosh error codes to something like Unix. */

private int
cvt_err(err)	/* some of these don't make sense... */
int	err;
{
	switch(err) {
	case noErr:	errno = 0; return 0;
	case dirFulErr:
	case dskFulErr:	errno = ENOSPC; break;
	case nsvErr:
	case mFulErr:
	case tmfoErr:
	case fnfErr:
	default:	errno = ENOENT; break;
	case ioErr:	errno = EIO; break;
	case bdNamErr:
	case opWrErr:
	case paramErr:	errno = EINVAL; break;
	case fnOpnErr:				/* dubious... */
	case rfNumErr:	errno = EBADF; break;
	case eofErr:				/* ditto */
	case posErr:	errno = ESPIPE; break;
	case wPrErr:	errno = EROFS; break;
	case fLckdErr:
	case permErr:	errno = EACCES; break;
	case fBsyErr:	errno = EBUSY; break;
	case dupFNErr:	errno = EEXIST; break;
	case gfpErr:
	case volOffLinErr:
	case volOnLinErr:
	case nsDrvErr:	errno = ENODEV; break;
	case noMacDskErr:
	case extFSErr:	errno = EIO; break;
	case fsRnErr:
	case badMDBErr:
	case wrPermErr:	errno = EPERM; break;
	}
	return -1;
}

private char *
cvt_fnm(file)
char *file;
{
	static char nm[255];
	char *t;


	if (*file == '/')
		strcpy(nm,file + 1);	/* full path */
	else {
		if (strchr(file + 1, '/') != NULL)
			strcpy(nm,"/");	/* make a partial pathname */
		else
			*nm = '\0';
		strcat(nm,file);
	}
	t = nm;
	while (*t) {
		if (*t == '/')
			*t = ':';
		t++;
	}
	return nm;
}

int
creat(name,perm)	/* permission mode is irrelevant on a Mac */
char	*name;
int	perm;
{
	int fd, err;
	char *nm;
	HParamBlockRec p;

	if (is_dir(name)) {
		errno = EACCES;
		return -1;
	}
	nm = cvt_fnm(name);	/* convert filename to Mac type name */
	CtoPstr(nm);
	for (fd = 0; fd < NFILES && ft[fd].inuse; fd++)
		;
	if (fd == NFILES) {
		errno = EMFILE;
		return -1;
	}
	fsetup(&p);	/* try to delete it, whether it is there or not. */
	p.fileParam.ioNamePtr = (StringPtr) nm;
	if ((err = PBHDelete(&p,0)) != noErr && err != fnfErr)
		return cvt_err(err);
	if (do_creat(&p,nm) != 0)
		return -1;
	else {
		ft[fd].inuse++;
		ft[fd].refnum = p.ioParam.ioRefNum;
		return fd + 1;
	}
}

int
open(name,mode)
char	*name;
int	mode;
{
	int fd, err;
	char *nm;
	HParamBlockRec p;

	if (is_dir(name)) {
		errno = EACCES;
		return -1;
	}

	nm = cvt_fnm(name);	/* convert filename to Mac type name */
	CtoPstr(nm);
	for (fd = 0; fd < NFILES && ft[fd].inuse; fd++)
		;
	if (fd == NFILES) {
		errno = EMFILE;
		return -1;
	}
	fsetup(&p);
	switch (mode & 3) {
	case O_RDONLY:
		p.ioParam.ioPermssn = fsRdPerm;
		break;
	case O_WRONLY:
		p.ioParam.ioPermssn = fsWrPerm;
		break;
	case O_RDWR:
		p.ioParam.ioPermssn = fsRdWrPerm;
		break;
	}
	p.ioParam.ioNamePtr = (StringPtr) nm;
	p.ioParam.ioMisc = 0;
	if ((err = PBHOpen(&p,0)) != noErr && err != fnfErr)
		return cvt_err(err);
	if (err == noErr && mode & O_CREAT && mode & O_EXCL) {
		PBClose(&p,0);
		errno = EEXIST;
		return -1;
	}
	if (err == fnfErr) {
		if (mode & O_CREAT) {
			if (do_creat(&p,nm) != 0)
				return -1;
		} else {
			errno = ENOENT;
			return -1;
		}
	}
	ft[fd].inuse++;
	ft[fd].refnum = p.ioParam.ioRefNum;
	p.ioParam.ioPosMode =  (mode & O_APPEND)? fsFromLEOF : fsFromStart;
	p.ioParam.ioPosOffset = 0;
	if ((err = PBSetFPos(&p,0)) != noErr) {
		ft[fd].inuse = 0;
		return cvt_err(err);
	}
	errno = 0;
	return fd + 1;
}

private int
do_creat(p,nm)
HParmBlkPtr p;
char *nm;
{
	int err;

	fsetup(p);
	p->fileParam.ioNamePtr = (StringPtr) nm;
	if ((err = PBHCreate(p,0)) != noErr)
		return cvt_err(err);
	fsetup(p);
	p->fileParam.ioNamePtr = (StringPtr) nm;
	p->fileParam.ioFDirIndex = 0;
	if ((err = PBHGetFInfo(p,0)) != noErr)
		return cvt_err(err);
	p->fileParam.ioDirID = cur_dir;
	p->fileParam.ioFlFndrInfo.fdType = 'TEXT';
	p->fileParam.ioFlFndrInfo.fdCreator = 'JV01';
	p->fileParam.ioFlFndrInfo.fdFlags = 0;
	p->fileParam.ioFVersNum = 0;
	if ((err = PBHSetFInfo(p,0)) != noErr)
		return cvt_err(err);
	fsetup(p);
	p->ioParam.ioNamePtr = (StringPtr) nm;
	p->ioParam.ioPermssn = fsRdWrPerm;
	p->ioParam.ioMisc = 0;
	if (cvt_err(PBHOpen(p,0)))
		return -1;
	return 0;
}


int
close(fd)
int	fd;
{
	int err;
	HParamBlockRec p;

	fsetup(&p);
	p.ioParam.ioRefNum = ft[--fd].refnum;
	ft[fd].inuse = 0;
#ifdef	NEVER
	if (cvt_err(PBFlushFile(&p,0)) < 0)
		return -1;
	fsetup(&p);
#endif
	if (cvt_err(PBClose(&p,0)) < 0)
		return -1;
	fsetup(&p);
	p.ioParam.ioNamePtr = NULL;
	if (cvt_err(PBFlushVol(&p,0)) < 0)
		return -1;
	return 0;	/* ??? added by DHR */
}

/* Raw read, except '\n' is translated to '\r'.
 * Surely this could be done better by having '\n' stand for '\015'
 * as it is done in OS-9.
 */
int
read(fd,buf,n)
int	fd;
char	*buf;
unsigned	n;
{
	int err;
	IOParam p;
	if (fd == 0)
		return con_read(buf,n);
	if (ft[--fd].inuse == 0) {
		errno = EBADF;
		return -1;
	}
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	p.ioBuffer = buf;
	p.ioReqCount = n;
	p.ioPosMode = fsFromMark;
	p.ioPosOffset = 0;
	if ((err = PBRead(&p,0)) != noErr && err != eofErr)
		return cvt_err(err);
	while (n--) {
		if (*buf == '\r')
			*buf = '\n';	/* convert from Mac style */
		buf++;
	}
	errno = 0;
	return p.ioActCount;
}

/* Raw write, except '\n' is translated to '\r'.
 * Surely this could be done better by having '\n' stand for '\015'
 * as it is done in OS-9.
 */
int
write(fd,buf,n)
int	fd;
const char	*buf;
unsigned	n;
{
#ifdef	NEVER
	int err;
	IOParam p;
	char *obuf, *s;

	if (fd == 0)
		return con_write(buf,n);

	s = obuf = malloc(n + 1);
	if (obuf == NULL)
		return -1;	/* shouldn't happen... */
	if (ft[--fd].inuse == 0) {
		errno = EBADF;
		free(obuf);
		return -1;
	}
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	p.ioBuffer = obuf;
	p.ioReqCount = (long) n;
	p.ioPosMode = fsFromMark;
	p.ioPosOffset = 0L;
	while (n--) {
		if (*buf == '\n')
			*s = '\r';	/* make it look like Mac files */
		else
			*s = *buf;
		buf++;
		s++;
	}
	if ((err = PBWrite(&p,0)) != noErr) {
		free(obuf);
		return -1;
	}
	free(obuf);
	return (int) p.ioActCount;
#else
	/* ??? This version is untested! -- DHR
	 * It avoids a malloc for every file write!
	 */
	if (fd == 0) {
		return con_write(buf,n);
	} else {
		IOParam p;
		const char	*ebuf = buf + n;

		p.ioRefNum = ft[fd].refnum;
		p.ioPosMode = fsFromMark;
		while (buf != ebuf) {
			int err;

			if (*buf == '\n') {
				p.ioReqCount = 1
				p.ioBuffer = "\r";
			} else {
				const char	*p = buf

				while (p != ebuf && *p != '\n')
					p++;
				p.ioReqCount = p-buf;
				p.ioBuffer = buf;
			}
			p.ioPosOffset = 0L;	/* bidirectional */
			if ((err = PBWrite(&p,0)) != noErr)
				return cvt_err(err);
			buf += p.ioActCount;
		}
		return n;
	}
#endif
}

long
lseek(fd,offset,type)	/* The Mac version of this doesn't allocate new space. */
int	fd;
long	offset;
unsigned	type;
{
	int err;
	long cur_mark, eof, new_mark;
	IOParam p;

	if (ft[--fd].inuse == 0) {
		errno = EBADF;
		return -1;
	}

	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	if ((err = PBGetFPos(&p,0)) != noErr)
		return cvt_err(err);
	cur_mark = p.ioPosOffset;
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	if ((err = PBGetEOF(&p,0)) != noErr)
		return cvt_err(err);
	eof = (long) p.ioMisc;
	switch(type) {
	case 0:
		new_mark = offset;
		break;
	case 1:
		new_mark = offset + cur_mark;
		break;
	case 2:
		new_mark = offset + eof;
		break;
	}
	if (new_mark > eof) {		/* need more space in file */
		isetup(&p);
		p.ioRefNum = ft[fd].refnum;
		p.ioMisc = (Ptr) new_mark;
		if ((err = PBSetEOF(&p,0)) != noErr)
			return cvt_err(err);
#ifdef	NEVER
		if ((err = PBAllocContig(&p,0)) != noErr)
			return cvt_err(err);
#endif
	}
	isetup(&p);
	p.ioRefNum = ft[fd].refnum;
	p.ioPosOffset = new_mark;
	p.ioPosMode = fsFromStart;
	if ((err = PBSetFPos(&p,0)) != noErr)
		return cvt_err(err);
	errno = 0;
	return p.ioPosOffset;
}

int
unlink(name)
char *name;
{	int fd, err;
	char *nm;
	HParamBlockRec p;

	nm = cvt_fnm(name);	/* convert filename to Mac type name */
	CtoPstr(nm);
	fsetup(&p);	/* try to delete it, whether it is there or not. */
	p.fileParam.ioNamePtr = (StringPtr) nm;
	if ((err = PBHDelete(&p,0)) != noErr && err != fnfErr)
		return cvt_err(err);
	return 0;	/* ??? added by DHR */
}

/* Console read and write routines */

private int
con_write(buf,size)
char *buf;
unsigned  size;
{
	while (size--)
		putp(*buf++);
	return size;
}

private int
con_read(buf,size)
char *buf;
unsigned size;
{
	unsigned n;
	int p;


	n = 0;
	do {
		p = rawgetc();
#ifdef	O_META
		if (p & 0x7f)
			p &= 0x7f;		/* was normal ascii char */
#endif
		*buf++ = p;
		n++;
	} while (rawchkc() && n <= size);
	return n;
}


/* This didn't seem to be any place else */

int
abs(n)
int n;
{
	return n >= 0 ? n : -n;

}

/* Simplified stat() routine emulates what is needed most. */

int
stat(fname,buf)
char *fname;
struct stat *buf;
{
	CInfoPBRec p;
	char *nm;

	nm = cvt_fnm(fname);
	CtoPstr(nm);
	byte_zero(&p,sizeof(CInfoPBRec));
	p.hFileInfo.ioCompletion = 0;
	p.hFileInfo.ioNamePtr = (StringPtr) nm;
	p.hFileInfo.ioFVersNum = 0;
	p.hFileInfo.ioFDirIndex = 0;
	p.hFileInfo.ioVRefNum = cur_vol;
	p.hFileInfo.ioDirID = cur_dir;

	switch (PBGetCatInfo(&p,0)) {
	case noErr:
		errno = 0;
		break;
	case nsvErr:
	case paramErr:
	case bdNamErr:
	case fnfErr:
		errno = ENOENT;
		break;
	case ioErr:
		errno = EIO;
		break;
	default:
		errno = ENOENT;
		break;
	}
	buf->st_dev = p.hFileInfo.ioVRefNum + 1;	/* don't want 0 */
	buf->st_ino = p.hFileInfo.ioDirID;
	buf->st_size = p.hFileInfo.ioFlLgLen;
	buf->st_mtime = p.hFileInfo.ioFlMdDat;
	buf->st_mode = (p.hFileInfo.ioFlAttrib & 0x10) ? S_IFDIR : 0;
	PtoCstr(nm);
	return errno == 0 ? 0 : -1;
}

private bool
is_dir(fname)
char *fname;
{
	struct stat s;

	return (stat(fname,&s) == 0) && (s.st_mode & S_IFDIR);
}

/* Directory related routines. Jove keeps track of the true Volume (disk) number and
   directory number, and avoids "Working Directory Reference Numbers", which are
   confusing. */

private int
getdir()	/* call this only once, during startup. */
{
	WDPBRec p;

	p.ioCompletion = 0;
	p.ioNamePtr = NULL;
	if (PBHGetVol(&p,0) != noErr)
		return -1;	/* BIG trouble */
	cur_vol = p.ioWDVRefNum;
	cur_dir = p.ioWDDirID;
	SFSaveDisk = 0 - cur_vol;	/* these are for SF dialogs */
	CurDirStore = cur_dir;
	return 0;	/* ??? added by DHR */
}

private int
setdir(vol,dir)
int	vol;
long	dir;
{
	WDPBRec p;

	p.ioCompletion = 0;
	p.ioNamePtr = NULL;
	p.ioVRefNum = vol;
	p.ioWDDirID = dir;
	if (PBHSetVol(&p,0) != noErr)
		return -1;
	cur_vol = vol;
	cur_dir = dir;
	SFSaveDisk = 0 - vol;	/* these are for SF dialogs */
	CurDirStore = dir;


}

int
chdir(dir)
char *dir;
{
	CInfoPBRec d;
	WDPBRec p;
	char *t;
	char *nm;

	if (strcmp(dir,"/") == 0)
		return -1;	/* There is no root... */
	nm = malloc(strlen(dir) + 2);
	if (nm == NULL)
		return -1;

	strcpy(nm,dir);
	t = nm;
	while (*t) {
		if (*t == '/')
			*t = ':';
		t++;
	}
	t = nm;
	while (*t == ':')
		t++;	/*get rid of initial slashes */
	strcat(nm,":");
	CtoPstr(t);

	d.dirInfo.ioCompletion = 0;			/* get the directory number */
	d.dirInfo.ioNamePtr = (StringPtr) t;
	d.dirInfo.ioVRefNum = cur_vol;
	d.dirInfo.ioFDirIndex = 0;
	d.dirInfo.ioDrDirID = 0;
	PBGetCatInfo(&d,0);
	free(nm);
	if (d.dirInfo.ioResult != noErr
	|| (d.dirInfo.ioFlAttrib & 0x10) == 0
	|| setdir(d.dirInfo.ioVRefNum,d.dirInfo.ioDrDirID) < 0)
		return -1;
	return 0;
}

/* Scandir returns the number of entries or -1 if the directory cannot
   be opened or malloc fails. */

int
jscandir(dir, nmptr, qualify, sorter) /* this function has NOT been debugged */
char	*dir;
char	***nmptr;
int	(*qualify) proto((char *));
int	(*sorter) proto((UnivConstPtr, UnivConstPtr));
{
	CInfoPBRec d;
	Str255 buf;
	long DirID;
	char	**ourarray, *nm, *t;
	unsigned int	nalloc = 10,
			nentries = 0,
			index = 1;
	char *getwd();

	if (strcmp(dir,"/") == 0)
		return -1;	/* There is no root... */
	if (strcmp(dir,".") == 0)
		dir = getwd();
	nm = malloc(strlen(dir) + 2);
	if (nm == NULL)
		return -1;

	strcpy(nm,dir);
	t = nm;
	while (*t) {
		if (*t == '/')
			*t = ':';
		t++;
	}
	t = nm;
	while (*t == ':')
		t++;	/*get rid of initial slashes */
	strcat(nm,":");
	CtoPstr(t);

	byte_zero(&d,sizeof(CInfoPBRec));
	d.dirInfo.ioCompletion = 0;			/* get the directory number */
	d.dirInfo.ioNamePtr = (StringPtr) t;
	d.dirInfo.ioVRefNum = cur_vol;
	d.dirInfo.ioFDirIndex = 0;
	d.dirInfo.ioDrDirID = 0;
	PBGetCatInfo(&d,0);
	PtoCstr(t);
	free(nm);
	if (d.dirInfo.ioResult != noErr
	|| ((d.dirInfo.ioFlAttrib & 0x10) == 0))
		return -1;
	DirID = d.dirInfo.ioDrDirID;
	ourarray = (char **) emalloc(nalloc * sizeof (char *));
	for (;;) {
		byte_zero(&d,sizeof(CInfoPBRec));
		d.dirInfo.ioCompletion = (long) 0;
		d.dirInfo.ioVRefNum = cur_vol;
		d.dirInfo.ioFVersNum = 0;
		d.dirInfo.ioNamePtr = (StringPtr) buf;
		d.dirInfo.ioFDirIndex = index++;
		d.dirInfo.ioVRefNum = cur_vol;
		d.dirInfo.ioDrDirID = DirID;
		if (PBGetCatInfo(&d,0) != noErr)
			break;	/* we are done, then */
		PtoCstr((char *) buf);
#ifdef	NEVER
		if (d.dirInfo.ioFlAttrib & 0x10)
			strcat(buf,"/");
#endif
		if (qualify != NULL && (*qualify)((char *) buf) == 0)
			continue;
		if (nentries == nalloc)
			ourarray = (char **) erealloc((char *) ourarray, (nalloc += 10) * sizeof (char *));
		ourarray[nentries] = (char *) emalloc(strlen(buf)+1);
		null_ncpy(ourarray[nentries], (char *) buf, strlen((char *) buf));
		nentries += 1;
	}
	if ((nentries + 1) != nalloc)
		ourarray = (char **) erealloc((char *) ourarray,
			((nentries + 1) * sizeof (char *)));
	if (sorter != NULL)
		qsort((char *) ourarray, nentries, sizeof (char **), sorter);
	*nmptr = ourarray;
	ourarray[nentries] = NULL;		/* guaranteed NULL pointer */
	return nentries;
}

void
freedir(dir, nentries)
char ***dir;
int nentries;
{
	char **ptr = *dir;
	while (nentries--)
		free(*ptr++);
}

int
alphacomp(a, b)
UnivConstPtr	a,
	b;
{
	return strcmp(*(const char **)a, *(const char **)b);
}

bool
chkCWD(name)	/* eventually, may check validity of cwd */
char *name;
{
	return TRUE;
}


char *
getwd()
{
	CInfoPBRec d;
	static char ret[255];
	char nm[50], tmp[255];

	ret[0] = '\0';
	d.dirInfo.ioDrDirID = cur_dir;
	for (;;) {
		d.dirInfo.ioCompletion = 0;
		d.dirInfo.ioNamePtr = (StringPtr) nm;
		d.dirInfo.ioVRefNum = cur_vol;
		d.dirInfo.ioFDirIndex = -1;

		PBGetCatInfo(&d,0);
		if (d.dirInfo.ioResult != noErr)
			return NULL;
		PtoCstr((char *) nm);
		strcpy(tmp,ret);
		strcpy(ret,"/");
		strcat(ret,nm);
		strcat(ret,tmp);
		if (d.dirInfo.ioDrDirID == 2)
			break;	/* home directory */
		d.dirInfo.ioDrDirID = d.dirInfo.ioDrParID;
	}
	return ret;
}

private char *
gethome()		/* this will be startup directory */
{
	static char *ret = NULL;


	if (ret == NULL) {
		char *item = getwd();

		ret = emalloc(strlen(item)+1);
		strcpy(ret,item);
	}
	return ret;
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


private WindowPtr theWindow;
private ListHandle theList;
private Rect theListRect;
private EventRecord theEvent;



private void
about_j()
{
	void do_list(), do_events();

	WindowPtr OldWindow;

	GetPort(&OldWindow);

	if ((theWindow = makedisplay()) == 0)
		return;
	SetPort(theWindow);
	if (theList = makelist()) {
		LActivate(1,theList);
		do_list();
		ShowWindow(theWindow);
		do_events();
	}
	SetPort(OldWindow);
	LDispose(theList);
	DisposDialog(theWindow);
}


private WindowPtr
makedisplay()
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

	if (dlogid == 0) {
		if ((theResource = GetNamedResource('DLOG',DLOGNAME)) == 0)
			return (WindowPtr)NULL;
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

	return (WindowPtr) theDialog;
}

private void
do_display()		/* draw necessary controls, lines */
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

private ListHandle
makelist()
{
	Point csize;
	Rect dataBounds, rView;	/* list boundaries */

	csize.h = csize.v = 0;
	SetRect(&dataBounds,0,0,1,0);
	return LNew(&theListRect,&dataBounds,csize,0,theWindow,0,0,0,1);
}

private void
do_list()
{
	void printbind();

	int row, col;
	struct cmd *f;
	Str255 buf;
	Point theCell;

	theCell.h = 0;

	for (f = commands, row = 0; f->Name; f++, row++) {
		LAddRow(1,row,theList);
		theCell.v = row;

		printbind(f,buf);
		strcat(buf,f->Name);
		LSetCell(buf,strlen((char *)buf),theCell,theList);

	}
}
private void
printbind(f,buf)
struct cmd *f;
char *buf;
{
	char c;

	if (f->c_map == 0 || (c = f->c_key) == 0x7f) {
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
	if (c < ' ') {
		buf[5] = '^';		/* control char */
		c |= 0x40;
	} else {
		buf[5] = ' ';
	}
	if (c >= 'a' && c<= 'z')
		c &= 0x5f;
	buf[6] = c;
	buf[7] = ' ';
	buf[8] = '\0';
}



private pascal Boolean
ProcFilter(theDialog,event,itemHit)
DialogPtr theDialog;
EventRecord *event;
int *itemHit;
{
	theEvent = *event;
	if (theEvent.what == keyDown && theEvent.message & charCodeMask == '\r') {
		*itemHit = 1;
		return TRUE;
	}
	if (theEvent.what == activateEvt && (WindowPtr) theEvent.message == theWindow) {
		LDoDraw(1,theList);
		LActivate(1,theList);
	}
	if (theEvent.what == updateEvt && (WindowPtr) theEvent.message == theWindow) {
		BeginUpdate(theWindow);
		do_display();
		DrawDialog(theWindow);
		LUpdate((GrafPtr) theWindow->visRgn,theList);
		EndUpdate(theWindow);
	}

	return FALSE;
}


void
do_events()
{
	int item;
	bool done = NO;
	Point p;

	while (!done) {
		ModalDialog(ProcFilter,&item);
		switch(item) {
		case DONE_ITEM :
			done = YES;
			/* ??? fall through? -- DHR */
		case LIST_ITEM :
			p = theEvent.where;
			GlobalToLocal(&p);
			LClick(p,theEvent.modifiers,theList);
			break;
		}
	}
}

/* Window and Control related routines. */

/* (ORIGINALLY IN) tcon.c.
   control handler routines for Jove. K. Mitchum 12/86 */


#define MINC 0
#define MAXC ((int)100)
#define INITC 0
#define EVENTLIST (mDownMask | keyDownMask )

extern long
GetCRefCon();	/* omitted in ControlMgr.h */

private Point p;
private intext;	/* mouse down in jove text */
private bool wc_adjust proto((int, int, struct wind_config *, int));

void
docontrols()	/* called from redisplay routines */
{
	void MakeScrollBar(),
		AdjustScrollBar(),
		drawfluff();

	Window *w;
	int top;

	w = fwind;
	top = 0;
	do {
		if (w->w_control)
			HideControl(w->w_control);
		w = w->w_next;
	} while (w != fwind);
	w = fwind;
	do {
		w->w_topline = top;
		if (w->w_control)
			AdjustScrollBar(w);
		else
			MakeScrollBar(w);
		ShowControl(w->w_control);
		top += w->w_height;
		w = w->w_next;
	} while (w != fwind);
	Windchange = NO;
	drawfluff();
}


void
MakeScrollBar(w)	/* set up control */
Window *w;
{
	Rect BarRect;
	int wheight, wtop;

	WindowPtr window = theScreen;
	wheight = w->w_height;
	wtop = w->w_topline;
	SetRect(&BarRect,window->portRect.right - SCROLLWIDTH + 1,
		window->portRect.top -2 + wtop * HEIGHT,
		window->portRect.right +1,
		window->portRect.top + ((wheight + wtop) * HEIGHT + 1));
		w->w_control = ((char **) NewControl(window,&BarRect,"/psbar",1,INITC,
		MINC,MAXC,scrollBarProc,w));
}

void
AdjustScrollBar(w)	/* redo existing control */
Window *w;
{
	int wtop,wheight;
	ControlHandle handle;
	WindowPtr window;

	handle = (ControlHandle) w->w_control;
	wtop = w->w_topline;
	wheight = w->w_height;
	window = (*handle)->contrlOwner;

	if (handle == 0)
		return;

	SizeControl(handle,SCROLLWIDTH,wheight * HEIGHT + 1);

	MoveControl(handle,window->portRect.right - SCROLLWIDTH + 1,
		window->portRect.top -1 + wtop * HEIGHT);

}

void
SetScrollBar(handle)	/* set value of the bar */
ControlHandle handle;
{

	SetCtlValue(handle,ltoc());
}

private void
drawfluff()		/* draw controls and dividers */
{
	Window *w = fwind;

	DrawControls(theScreen);
	DrawGrowIcon(theScreen);
}

void
RemoveScrollBar(w)
Window *w;
{
	if (w->w_control)
		DisposeControl(w->w_control);
	w->w_control = 0;

}

private pascal void
DScroll(control,part)
ControlHandle control;
int part;
{
	DownScroll();
	redisplay();
}

private pascal void
UScroll(control,part)
ControlHandle control;
int part;
{
	UpScroll();
	redisplay();
}

private pascal void
NPage(control,part)
ControlHandle control;
int part;
{	NextPage();
	redisplay();
}

private pascal void
PPage(control,part)
ControlHandle control;
int part;
{	PrevPage();
	redisplay();
}

private long npos;	/* number of lines in buffer */

private int
ltoc()	/* calculate ctlvalue for line position */
{
	register long ipos;
	register Line	*lp = curbuf->b_first;

	for (npos = 1; lp ; npos++, lp = lp->l_next)  {
		if (lp == curline)
			ipos = npos;
	}
	return (int) ((ipos * MAXC) / npos);
}

private Line *
ctol(ctlv)	/* find buffer line for ctlvalue */
int ctlv;
{
extern char *itoa();
	register long ipos;
	register Line	*lp = curbuf->b_first;

	ipos = (npos * ctlv)/MAXC;
	while (ipos-- && lp->l_next)
		lp = lp->l_next;
	return lp;
}

private void
doWind(event,window)
EventRecord *event;
WindowPtr window;
{
#define track() TrackControl(whichControl,p,(ProcPtr)NULL)

	ControlHandle whichControl;
	Window *jwind, *cwind;
	int notcurwind;
	int cpart;	/* control part */
	int oldval,newval,thumb = 0;

	p = event->where;
	intext = 0;
	notcurwind = 0;
	GlobalToLocal(&p);

	if (event->what == mouseDown) {
		if ((cpart = FindControl(p,window,&whichControl)) == 0)
			return;
		if ((jwind = (Window *) (*whichControl)->contrlRfCon) !=  curwind) {
			notcurwind++;
			cwind = curwind;
			SetWind(jwind);
		}
		switch (cpart) {
		case inUpButton:
			TrackControl(whichControl,p,(ProcPtr) DScroll);
			break;
		case inDownButton:
			TrackControl(whichControl,p,(ProcPtr) UScroll);
			break;
		case inPageUp:
			TrackControl(whichControl,p,(ProcPtr) PPage);
			break;
		case inPageDown:
			TrackControl(whichControl,p,(ProcPtr) NPage);
			break;
		case inThumb:
			if (track()) {
				newval = GetCtlValue(whichControl);
				if (newval == MAXC)
					Eof();
				else if (newval == MINC)
					Bof();
				else
					SetLine(ctol(newval));
			}
			break;
		}
		if (notcurwind) {
			SetWind(cwind);
			redisplay();
		}
		redisplay();	/* again, to set the cursor */
	}
	else {
		if (findtext())
			redisplay();
	}
}

#define std_state(w) (*((WStateData **)((WindowPeek)((w)))->dataHandle))->stdState
#define user_state(w) (*((WStateData **)((WindowPeek)((w)))->dataHandle))->userState

private void
doDrag(event,window)
EventRecord *event;
WindowPtr window;
{
	Rect old_std;

	old_std = std_state(window);

	DragWindow(window, event->where, &LimitRect);
	if (wc == &wc_std) {
		wc_user = wc_std;
		user_state(theScreen) = std_state(theScreen);
		ZoomWindow(window,7,1);
		wc = &wc_user;
		Reset_std();
	}
}

private void
doGrow(event,window)
EventRecord *event;
WindowPtr window;
{
	long size;

	/* zero means user didn't change anything */
	if (size = GrowWindow(window, event->where, &LimitRect)) {
		if (wc == &wc_std) {
			wc_user = wc_std;
			user_state(theScreen) = std_state(theScreen);
			ZoomWindow(window,7,1);
			wc = &wc_user;
			Reset_std();
		}
		if (wc_adjust(LoWord(size),HiWord(size),wc,0)) {
			EraseRect(&window->portRect);
			SizeWindow(window,wc->w_width,wc->w_height,TRUE);
			win_reshape();	/* no signals here... */
		}
	}
}

private void
doZoomIn(event,window)
EventRecord *event;
WindowPtr window;
{
	if (TrackBox(window, event->where, 7)) {
			EraseRect(&window->portRect);
			ZoomWindow(window,7,1);
			wc = &wc_user;
			win_reshape();	/* we do our own toggle, not ZoomWindow() */
		}
}

private void
doZoomOut(event,window)
EventRecord *event;
WindowPtr window;
{
	if (TrackBox(window, event->where, 8)) {
			EraseRect(&window->portRect);
			ZoomWindow(window,8,1);
			wc = &wc_std;
			win_reshape();	/* we do our own toggle, not ZoomWindow() */
		}
}

private void
doGoAway(event,window)
EventRecord *event;
WindowPtr window;
{
	if (TrackGoAway(window, event->where))
		Leave();
}

private Window *
rtowind(row)	/* return jove window row is in */
int row;
{
	Window *w = fwind;

	do {
		if ((w->w_topline <= row) && ((w->w_height + w->w_topline) > row))
			return w;
		w = w->w_next;
	} while (w != fwind);
	return NULL;
}

private Line *
windtol(w,row)		/* return line for row in window */
Window *w;
int row;
{
	Line *l = w->w_top;

	while (row--)
		if ((l = l->l_next) == NULL)
			return NULL;
	return l;
}


private bool
findtext()		/* locate and move the point to match the mouse */
{
	int row,col;
	long ticks;
	EventRecord event;
	Window *w;
	Line *l;

	ticks = Ticks;
	ptoxy(p,&row,&col);
	if ((w = rtowind(row)) == NULL)
		return NO;
	if (w != curwind)
		SetWind(w);
	row -= w->w_topline;		/* now have row number in window */
	if (row >= w->w_height -1)
		return NO;
	if ((l = windtol(w,row)) == NULL)
		return NO;
	if (l->l_dline == NULL_DADDR)
		return NO;
	this_cmd = LINECMD;
	SetLine(l);		/* Curline is in linebuf now */
	if (w->w_flags & W_NUMLINES)
		col -= 8;	/* adjust for line numbers */
	if (col < 0)
		col = 0;
	curchar = how_far(curline, col);
	do {
		if (GetNextEvent(mUpMask,&event) && (event.when < ticks + DoubleTime)) {
			set_mark();
			break;
		}
	} while ((Ticks - ticks) < DoubleTime);
	return YES;
}


private int
ptoxy(p,row,col)	/* convert Point to terminal x,y coordinate */
Point p;
int *row,*col;
{
	*row = (p.v / HEIGHT);
	*col = (p.h / WIDTH );
	if ((*row > MAXROW) || (*col > MAXCOL))
		return ERROR;
	return 0;
}

/* Event-related routines. The Event loop is CheckEvents(), and is called whenever
   a console read occurs or a call to charp(). During certain activities, such as ask(),
   etc. non-keyboard events are ignored. This is set by the variable Keyonly.
   As an update or activate event generates a call to redisplay(), it is important
   that redisplay() and related routines NOT check for keyboard characters. */

/* (ORIGINALLY IN) tevent.c
	event handler for Jove. K Mitchum 12/86 */


#define SYS_ID 100
#define NOFUNC ((void (*)())NULL)
#define NEVENTS 16

extern void doMouse(),dokeyDown(),doUpdate(),doActivate();

private void p_refresh proto((void));

private MenuHandle SysMenu;

private void (*eventlist[])() =
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


private void
CheckEvents()
{
	void SetBufMenu(),
		MarkModes();

	static EventRecord theEvent;
	static Point Mousep;
	static long time = 0;

	static void (*fptr)();


	if (FrontWindow() == window) {
		GetMouse(&Mousep);
		if (PtInRect(Mousep,&r))
			SetCursor(*cross);
		else
			SetCursor(&arrow);
	}

	SystemTask();
	if (EventCmd && !Keyonly)
		return;
	if (Bufchange)
		SetBufMenu();
	if (Modechange)
		MarkModes();
	while (GetNextEvent(everyEvent,&theEvent)) {
		if ((theEvent.what < NEVENTS) && (fptr = eventlist[theEvent.what])) {
			(*fptr)(&theEvent);
		}
		SystemTask();
	}
	if ((Ticks - time) > 3600) {
		time = Ticks;
		UpdModLine = YES;
		redisplay();
	}
}

private void
InitSysMenu()
{
	void InitLocalMenus();

	SysMenu = NewMenu(SYS_ID,"\p\24");
	AppendMenu(SysMenu,"\pAbout Jove");
	AddResMenu(SysMenu,'DRVR');
	InsertMenu(SysMenu,0);
	InitLocalMenus();
	DrawMenuBar();
}

extern void doWind(),doGoAway(),doSysMenu(),doSysClick(),
	 doDrag(), doGrow(), doZoomIn(), doZoomOut();
#define NMEVENTS 9

private void (*mouselist[])() =
{
	NOFUNC, /* inDesk */
	doSysMenu, /* inMenuBar */
	doSysClick, /* inSysWindow */
	doWind, /* inContent */
	doDrag, /* inDrag */
	doGrow, /* inGrow */
	doGoAway, /* inGoAway */
	doZoomIn,	/* inZoomIn */
	doZoomOut	/* inZoomOut */
};


private void
doMouse(event)
EventRecord *event;
{
	WindowPtr theWindow;
	int wpart;
	void (*fptr)();

	if (Keyonly) {
		if (event->what == mouseDown)
			SysBeep(2);
		return;
	}
	wpart = FindWindow(event->where,&theWindow);
	if ((wpart < NMEVENTS) && (fptr = mouselist[wpart])) {
		(*fptr)(event,theWindow);
	}

}

private void
doSysMenu(event,window)
EventRecord *event;
WindowPtr window;
{
	void ProcMenu();

	int Menu,Item;
	long result = MenuSelect(event->where);
	Menu = (result >> 16) & 0xffff;
	Item = result & 0xffff;
	if (Item == 0)
		return;	/* no choice made */

	if (Menu == SYS_ID) {			/* apple menu */
		Str255 Name;
		GrafPtr Port;

		if (Item == 1)
			about_j();
		else {
			GetItem(SysMenu,Item,Name);
			GetPort(&Port);
			OpenDeskAcc(Name);
			SetPort(Port);
		}
	}
	else
		ProcMenu(Menu,Item);
	HiliteMenu(0);
	EventCmd = YES;
	menus_on();
}

private void
doSysClick(event,window)
EventRecord *event;
WindowPtr window;
{
	SystemClick(event,window);
}


private void
doUpdate(event)
EventRecord *event;
{
	WindowPtr theWindow, oldPort;

	theWindow = (WindowPtr) event->message;

	GetPort(&oldPort);
	SetPort(theWindow);
	BeginUpdate(theWindow);
	p_refresh();
	drawfluff();
	EndUpdate(theWindow);
	SetPort(oldPort);
}

private void
doActivate(event)
EventRecord *event;
{
	WindowPtr theWindow;
	ControlHandle control;
	int hilite;

	theWindow = (WindowPtr) event->message;
	SetPort(theWindow);
	hilite = (event->modifiers & activeFlag)? 0 : 255;
	for (control = (ControlHandle) (((WindowPeek) theWindow)->controlList)
	; (control != 0); control = (*control)->nextControl) {
			HiliteControl(control,hilite);
	}
}

/* Keyboard routines. The Option key was formerly used as a meta key.
   However, to take advantage of the full (non-ASCII) character set,
   this was removed. The corresponding code is ifdeffed O_META. */

/* (ORIGINALLY IN) tkey.c
   keyboard routines for Macintosh. K Mitchum 12/86 */

extern jmp_buf auxjmp;

private nchars = 0;
private char charbuf[MCHARS];

/* The following kludges a meta key out of the option key by
   sending an escape sequence back to the dispatch routines. this is
   not elegant but it works, and doesn't alter escape sequences for
   those that prefer them. to remap the control or meta keys,
   see mackeys.h. */

private void
dokeyDown(event)
EventRecord *event;
{
	unsigned mods;
	register c;
	static int cptr = 0;

	if (MCHARS - nchars < 2)
		return;

	c  = (char)((event->message)&(charCodeMask));

	mods = event->modifiers;

#ifdef	O_META
	if (mods & (optionKey | cmdKey | controlKey)) {
#else
	if (mods & (cmdKey | controlKey)) {
#endif
#ifdef	NEVER
		if (mods & shiftKey)
			c  = sh_keycodes[(((event->message)&(keyCodeMask))>>8)];
		else
			c  = nsh_keycodes[(((event->message)&(keyCodeMask))>>8)];
#endif
#ifdef	O_META
		if (mods & optionKey) {		/* make escape sequence */
			if (mods & cmdKey)
				c &= 0x1f;
			charbuf[cptr++] = '\033';
			cptr &= NMASK;		/* zero if necessary */
			nchars++;
		}
		else
#endif
		{	/* command key (control key) */
			if ((c == '2') || (c == '\\') || (c == ' '))
				c = '\0';	/* so we have a null char */
			if (c != '`')
				c &= 0x1f;		/* make a control char */
		}
	}
	else {
		if (c == '`')
			c = '\033';	/* for those used to escapes */
	}

	charbuf[cptr++] = c;
	cptr &= NMASK;
	nchars++;
}

private int
rawgetc()
{
	static int cptr = 0;
	register int c;

	if (EventCmd)
		longjmp(auxjmp,1);
	while (nchars <= 0) {
		nchars = 0;
		if (EventCmd)
			longjmp(auxjmp,1);
		CheckEvents();	/* ugh! WAIT for a character */
	}
	nchars--;
	c = charbuf[cptr++];
	cptr &= NMASK;		/* zero if necessary */
	return c;
}

bool
rawchkc()
{
	if (EventCmd)
		longjmp(auxjmp,1);
	if (nchars == 0)
		CheckEvents();	/* this should NOT be necessary! */
	return nchars > 0;
}

/* Routines for calling the standard file dialogs, when macify is ON. If the user
   changes the directory using the file dialogs, Jove's notion of the current directory
   is updated. */


/* (ORIGINALLY IN) tmacf.c. K. Mitchum 12/86.
   Macify routines for jove. */

int CurrentVol;			/* see tfile.c */


#define TYPES  (-1)

private Point px = {100,100};
private char pmess[] = "\pSave file as: ";

private pascal Boolean
Ffilter(p)
FileParam *p;
{
	if (p->ioFlFndrInfo.fdType == 'APPL')
		return TRUE;
	PtoCstr((char *) p->ioNamePtr);
	if (strcmp(p->ioNamePtr,d_tempfile) == 0) {
		CtoPstr((char *) p->ioNamePtr);
		return TRUE;
	}
	CtoPstr((char *) p->ioNamePtr);
	return FALSE;
}

private void
check_dir()
{
	if (cur_vol != 0 - SFSaveDisk || cur_dir != CurDirStore) {
		setdir(0 - SFSaveDisk, CurDirStore);
		UpdModLine = YES;	/* make sure jove knows the change */
		Modechange = YES;
		setCWD(getwd());
	}
}

char *
gfile(namebuf)	/* return a filename to get */
char *namebuf;
{
	SFReply frec;
	char ans[FILESIZE];

	SFSaveDisk = 0 - cur_vol;	/* in case a Desk Accessory changed them */
	CurDirStore = cur_dir;
	SFGetFile(px,0L,Ffilter,TYPES,0L,0L,&frec);
	check_dir();	/* see if any change, set if so */
	if (frec.good) {
		EventRecord theEvent;
		do; while (GetNextEvent(updateMask,&theEvent) == 0);
		doUpdate(&theEvent);
		PtoCstr((char *)frec.fName);
		strcpy(ans,frec.fName);
		CtoPstr((char *)frec.fName);
		PathParse(ans,namebuf);
		return namebuf;
	}
	return (char *)NULL;
}

char *
pfile(namebuf)
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
	if (frec.good) {
		EventRecord theEvent;
		do; while (GetNextEvent(updateMask,&theEvent) == 0);
		doUpdate(&theEvent);
		t = (char *)frec.fName;
		PtoCstr((char *)frec.fName);
		while (*t == ':')
			t++;	/* convert to unix style */
		nm = t;
		while (*nm) {
			if (*nm == ':')
				*nm = '/';
			nm++;
		}
		PathParse(t,namebuf);
		return namebuf;
	}
	return (char *)NULL;
}


/* getArgs() returns an argument list based on documents clicked on by the user. */

int
getArgs(avp)
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
	if (nargs > 0) {	/* files to open... */
		argv = (char **) emalloc((nargs + 2) * sizeof(char *));
		for (argc = 1; argc <= nargs; argc++) {
			GetAppFiles(argc,&p);
			if (type == 0) {
				PtoCstr((char *)p.fName);
				d.ioCompletion = 0;
				d.ioNamePtr = NULL;
				d.ioVRefNum = p.vRefNum;
				d.ioWDIndex = 0;
				PBGetWDInfo(&d,0);
				cur_vol = d.ioWDVRefNum;
				cur_dir = d.ioWDDirID;
				pathname = getwd();
				argv[argc] = emalloc(strlen((char *)p.fName) + strlen(pathname) + 2);
				strcpy(argv[argc],pathname);
				strcat(argv[argc],"/");
				strcat(argv[argc],(char *)p.fName);
			}
			ClrAppFiles(argc);
		}
		if (type != 0)
			argc = 1;
	}
	else {
		argv = (char **) emalloc(2 * sizeof(char*));
		argc = 1;
	}
	argv[0] = "jove";

	argv[argc] = NULL;
	*avp = argv;
	cur_dir = old_dir;
	cur_vol = old_vol;
	return argc;
}

/* Limited version of getenv() */

char *
getenv(item)
char *item;
{
	char *ret = NULL, *str = NULL;

	if (strcmp(item,"CWD") == 0)
		str = getwd();
	if (strcmp(item,"HOME") == 0)
		str = gethome();
	if (str) {
		ret = emalloc(strlen(str) + 1);
		strcpy(ret,str);
	}
	return ret;
}

char *
mktemp(name)
char *name;
{
	return name;
}


/* Menu routines. The menus items are set up in a similar manner as keys, and
   are bound prior to runtime. See menumaps.txt, which must be run through setmaps.
   Unlike keys, menu items may be bound to variables, and to buffers. Buffer binding
   is only done at runtime. */

private void
InitLocalMenus()
{
	void InitMenu(),
		make_edits();

	int i;
	for (i = 0; i < NMENUS; i++) {
		InitMenu(&Menus[i]);
		if (i == 0)
			make_edits(Menus[i].menu_id + 1);
	}
}

private void
InitMenu(M)
struct menu *M;
{
	int i;
	data_obj *d;
	char *name;

	if (M->menu_id == 0)
		return;
	M->Mn = NewMenu(M->menu_id,CtoPstr(M->Name));
	PtoCstr(M->Name);

	for (i = 0; i < NMENUITEMS; i++) {
		d = (M->m[i]);
		if (d == NULL)
			break;	/* last item... */
		switch (d->Type & TYPEMASK) {
		case (STRING):
			AppendMenu(M->Mn,CtoPstr(d->Name));
			PtoCstr(d->Name);
			break;
		case (VARIABLE):
			SetItemMark(M->Mn,i + 1, 0x12);
			/* ??? fall through? */
		case (FUNCTION):
			CtoPstr(name = ((data_obj *) d)->Name);
			AppendMenu(M->Mn,name);
			PtoCstr(name);
			break;
		}
	}
	InsertMenu(M->Mn,0);
}

private void
ProcMenu(menuno,itemno)
int menuno,itemno;
{
	void MacSetVar();

	int i;
	data_obj *d;

	for (i = 0; i < NMENUS && Menus[i].menu_id != menuno; i++)
		;
	if (i < NMENUS) {	/* found the menu */
		itemno--;
		d = Menus[i].m[itemno];
		switch(d->Type & TYPEMASK) {
		case FUNCTION:
			ExecCmd((data_obj *) d);
			break;
		case BUFFER:
			SetABuf(curbuf);
			tiewind(curwind,(Buffer *) d);
			SetBuf((Buffer *) d);
			break;
		case VARIABLE:
			MacSetVar((struct variable *) d,i,itemno);
			break;
		default:
			break;
		}
	}
}


private void
make_edits(menu)	/* add dummy edit menu */
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

void
menus_off()
{
	int i;
	if (Keyonly || EventCmd)
		return;

#ifdef	MENU_DISABLE		/* NOBODY likes this, but it's here if you want it... */
	DisableItem(SysMenu,0);
	for (i = 0; i < NMENUS; i++)
		if (Menus[i].Mn)
			DisableItem(Menus[i].Mn,0);
	DrawMenuBar();
#endif
	Keyonly = YES;
}

void
menus_on()
{
	int i;

	if (!Keyonly)
		return;
#ifdef	MENU_DISABLE
	EnableItem(SysMenu,0);
	for (i = 0; i < NMENUS; i++)
		if (Menus[i].Mn)
			EnableItem(Menus[i].Mn,0);
	DrawMenuBar();
#endif
	Keyonly = NO;
}

private char *
BufMPrint(b,i)
Buffer	*b;
int	i;
{
	char *p;
	char *nm = filename(b);
	char t[35];

	if (strlen(nm) > 30) {
		strcpy(t,"...");
		strcat(t,nm + strlen(nm) - 30);
	} else
		strcpy(t,nm);
	nm = t;
	while (*nm) {
		switch(*nm) {	/* ugh... these are metacharacter for Menus */
		case '/':
			*nm = ':';
			break;
		case '^':
		case '!':
		case '<':
		case '(':
		case ';':
			*nm = '.';
			break;	/* that will confuse everybody */
		}
		nm++;
	}
	p = sprint("%-2d %-11s \"%-s\"",i,b->b_name,t);
	return p;
}

private void
SetBufMenu()
{
	register Buffer *b;
	data_obj *d;
	int i,j,stop;
	struct menu *M;

	Bufchange = NO;
	for (i = 0; i < NMENUS && strcmp(Menus[i].Name,"Buffer"); i++)
		;
	if (i < NMENUS) {
		M = &Menus[i];
		for (j = 0; j < NMENUITEMS && (d = Menus[i].m[j]) && (d->Type & TYPEMASK) != BUFFER; j++)
			;
		if (j < NMENUITEMS) {
			for (i = j, b = world; i < NMENUITEMS && b != NULL; i++, b = b->b_next) {

				if (M->m[i] == NULL)
					AppendMenu(M->Mn,CtoPstr(BufMPrint(b,i-j+1)));	/* add the item */
				else
					SetItem(M->Mn,i + 1,CtoPstr(BufMPrint(b,i-j+1)));	/* or change it */
				M->m[i] = (data_obj *) b;
			}
			stop = i;
			/* out of buffers? */
			for (;i < NMENUITEMS && M->m[i];i++) {
				DelMenuItem(M->Mn,stop + 1);	/* take off last item */
				M->m[i] = NULL;
			}
		}
	}
}

private void
MacSetVar(vp,mnu,itm)	/* Set a variable from the menu */
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
		str = do_ask("\r\n", (bool (*) ptrproto((int))) NULL, (char *) vp->v_value,
			prompt);
		if (str == NULL)
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
	if (vp->v_flags & V_CLRSCREEN)
		ClAndRedraw();
	if (vp->v_flags & V_TTY_RESET)
		tty_reset();	/* probably none on a Mac */
}

private void
MarkModes()
{
	int mnu,itm;
	data_obj *d;

	Modechange = NO;
	for (mnu = 0; mnu < NMENUS; mnu++)
		for (itm = 0; itm < NMENUITEMS; itm++) {
			if ((d = Menus[mnu].m[itm]) == NULL)
				break;
			if ((d->Type & (MAJOR_MODE | MINOR_MODE))
			|| ((d->Type & TYPEMASK) == BUFFER))
			{
				bool	checked;

				if (d->Type & (MAJOR_MODE))
					checked = curbuf->b_major == (d->Type >> 8);
				else if (d->Type & (MINOR_MODE))
					checked = (curbuf->b_minor & (d->Type >> 8)) != 0;
				else
					checked = d == (data_obj *) curbuf;
				CheckItem(Menus[mnu].Mn, itm + 1, checked);
			}
		}
}

void
MarkVar(vp,mnu,itm)	/* mark a boolean menu item */
struct variable *vp;
int mnu,itm;
{
	if (mnu == -1) {		/* we don't know the item... slow */
		for (mnu = 0; mnu < NMENUS; mnu++) {
			for (itm = 0; (itm < NMENUITEMS); itm++) {
				if ((struct variable *) (Menus[mnu].m[itm]) == vp) {
					CheckItem(Menus[mnu].Mn, itm + 1,
						*(vp->v_value) == ON);
					return;
				}
			}
		}
	}
}

private void
MarkAllVar()	/* slow, but only do it once */
{
	int mnu,itm;
	data_obj *d;
	for (mnu = 0; mnu < NMENUS; mnu++)
		for (itm = 0; itm < NMENUITEMS; itm++) {
			if ((d = Menus[mnu].m[itm]) == NULL)
				break;
			if ((d->Type & TYPEMASK) == VARIABLE)
				MarkVar((struct variable *)Menus[mnu].m[itm],mnu,itm);
		}
}


/* Screen routines and driver. The Macinitosh Text Edit routines are not utilized,
   as they are slow and cumbersome for a terminal emulator. Instead, direct QuickDraw
   calls are used. The fastest output is obtained writing a line at a time, rather
   than on a character basis, so the major output routine is writechr(), which takes
   a pascal-style string as an argument. See do_sputc() in screen.c. */

void
Placur(line,col)
int line, col;
{
	CapCol = col;
	CapLine = line;
	putcurs(line,col,ON);
}

void
NPlacur(line,col)
int line, col;
{
	CapCol = col;
	CapLine = line;
	putcurs(line,col,OFF);
}

void
i_lines(top, bottom, num)
int top, bottom, num;
{
	Placur(bottom - num + 1, 0);
	dellines(num,bottom);
	Placur(top, 0);
	inslines(num,bottom);
}

void
d_lines(top, bottom, num)
int top, bottom, num;
{
	Placur(top, 0);
	dellines(num,bottom);
	Placur(bottom + 1 - num, 0);
	inslines(num,bottom);
}

/* (ORIGINALLY IN) tn.c   */
/* window driver for MacIntosh using windows. */
/* K. Mitchum 9/86 */


/*#define VARFONT*/
#ifdef	VARFONT
private height,width,theight,twidth,descent;
#else
#define height HEIGHT
#define width WIDTH
#define theight THEIGHT
#define twidth TWIDTH
#define descent DESCENT
#endif

private int trow, tcol, insert, cursor;
private bool tattr;	/* ??? never fetched */
private Rect cursor_rect;
private char *p_scr, *p_curs;	/* physical screen and cursor */
private int p_size;

private Rect  vRect;
private WindowRecord myWindowRec;

#define active() SetPort(theScreen)
#define maxadjust(r) OffsetRect((r),0,2);

char *
conv_p_curs(row,col)
int	row,
	col;
{
	return p_scr + (row * (CO)) + col;
}

private void
INSmode(new)
bool new;
{
	insert = new;
}

private void
HLmode(new)
bool new;
{
	tattr = new;
}

void
SO_on()
{
	HLmode(TRUE);
}

void
SO_off()
{
	HLmode(FALSE);
}


private void
tn_init()
{
	void INSmode proto((bool)),
	init_slate();

	HLmode(FALSE);
	INSmode(OFF);
	init_slate();
	ShowPen();
}

void
clr_page()	/* clear and home function */
{
	Rect r;

	setmem(p_scr,p_size,' ');
	active();
	SetRect(&r, 0,0,WINDWIDTH,WINDHEIGHT);
	EraseRect(&r);
	cursor = OFF;
	putcurs(0,0,OFF);	/* ??? "OFF" guess by DHR */
	drawfluff();
}

private void
putcurs(row,col,vis)
unsigned	row, col;
bool	vis;
{
	active();
	curset(OFF);
	trow = row;
	tcol = col;
	curset(vis);
}

private void
curset(desired)
bool	desired;
{
	p_curs = conv_p_curs(trow,tcol);
	if (trow == MAXROW)
		MoveTo(tcol * width, (trow  +1) * height + 2 -descent );
	else
		MoveTo(tcol * width, (trow  +1) * height - descent);

	DrawChar(*p_curs);

	if (desired) {
		SetRect(&cursor_rect, tcol * width, (trow) * height , (tcol + 1) * width - 1, (trow +1) * height -1);
		if (trow == MAXROW)
			maxadjust(&cursor_rect);
		InvertRect(&cursor_rect);
	}
	if (trow == MAXROW)
		MoveTo(tcol * width, (trow  +1) * height + 2 -descent );
	else
		MoveTo(tcol * width, (trow  +1) * height - descent);
}


void
putp(p)			/* put one character, advance cursor */
int p;
{
	static Rect r;
	static RgnHandle updateRgn;

	active();
	curset(OFF);
	if (insert) {
		updateRgn = NewRgn();
		SetRect(&r, tcol * width, trow * height, WINDWIDTH, (trow +1) * height -1);
		if (trow == MAXROW)
			maxadjust(&r);
		ScrollRect(&r, width, 0, updateRgn);
		DisposeRgn(updateRgn);
	}
	if (p == '0')
		p = 0xAF;	/* slashed zero */
	if (insert)
		BlockMove(p_curs, p_curs + 1, (long) (MAXCOL - tcol));
	*p_curs = (char) p;
	DrawChar(p);
	if (tcol >= MAXCOL)
		putcurs(trow,MAXCOL, ON);	/* ??? "ON" guess by DHR */
	else
		putcurs(trow,tcol+1, ON);	/* ??? "ON" guess by DHR */
}

void
clr_eoln()
{
		static Rect r;

		active();
		cursor = OFF;
		SetRect(&r, tcol * width, trow * height, WINDWIDTH, (trow +1) * height);
		if (trow == MAXROW)
			maxadjust(&r);
		EraseRect(&r);
		setmem(p_curs,CO - tcol, ' ');
		curset(ON);
}

private void
delchars()
{
	static Rect r;
	static RgnHandle updateRgn;

	active();
	curset(OFF);
	updateRgn = NewRgn();
	SetRect(&r, tcol * width, trow * height, twidth - width, (trow +1) * height);
	if (trow == MAXROW)
		maxadjust(&r);
	ScrollRect(&r, 0 - width, 0, updateRgn);
	DisposeRgn(updateRgn);
	BlockMove(p_curs + 1, p_curs, (long) (MAXCOL - tcol));
	*(conv_p_curs(trow,MAXCOL)) = ' ';
	curset(ON);
}

private void
dellines(n,bot)
int n,bot;
{
	Rect r;
	RgnHandle updateRgn;
	long len;

	updateRgn = NewRgn();
	active();
	curset(OFF);
	SetRect(&r, 0, ((trow) * height), WINDWIDTH, ((bot + 1) * height));
	ScrollRect(&r, 0, 0 - (n * height), updateRgn);
	DisposeRgn(updateRgn);
	len = ((bot - trow - n + 1) * CO);
	BlockMove(conv_p_curs((trow + n),0), conv_p_curs(trow,0), len);
	setmem(conv_p_curs((bot - n + 1),0),(n * CO),' ');
	putcurs(trow, 0, ON);	/* ??? "ON" guess by DHR */
}

private void
inslines(n,bot)
int n,bot;
{
	Rect r;
	RgnHandle updateRgn;
	long len;

	updateRgn = NewRgn();
	active();
	curset(OFF);
	SetRect(&r, 0, trow * height, WINDWIDTH, (bot +1) * height);
	ScrollRect(&r, 0, (n * height), updateRgn);
	DisposeRgn(updateRgn);
	len = ((bot - trow - n +1) * CO);
	BlockMove(conv_p_curs(trow,0), conv_p_curs((trow + n),0), len);
	setmem(conv_p_curs(trow,0),(n * CO),' ');
	putcurs(trow,0, ON);	/* ??? "ON" guess by DHR */
}

void
writechr(start)
char *start;	/* actually, a Str255 type string */
{
	static Rect r;
	static RgnHandle updateRgn;
	register len;
	register char save;

	len = (int) start[0] & 0xff;		/* adjusted 6/86 K. M. in td.c*/

	active();
	curset(OFF);
	if (insert) {
		updateRgn = NewRgn();
		SetRect(&r, tcol * width, trow * height, twidth - width * len, (trow +1) * height -1);
		if (trow == MAXROW)
			maxadjust(&r);
		ScrollRect(&r, width * len, 0, updateRgn);
		DisposeRgn(updateRgn);
	}
	DrawString(start);
	if (insert)
		BlockMove(p_curs,p_curs + len, (long) (CO - tcol - len));
	strncpy(p_curs,start + 1,len);
	if (tcol >= MAXCOL)
		putcurs(trow, MAXCOL, ON);	/* ??? "ON" guess by DHR */
	else
		putcurs(trow, tcol+len, ON);	/* ??? "ON" guess by DHR */
}

private Rect myBoundsRect;

private void
init_slate()
{
	FontInfo f;

	char *Name = "Jove ";
	char *Title;

	InitGraf(&thePort);
	InitWindows();
	InitCursor();
	InitFonts();
	InitMenus();
	InitDialogs((ProcPtr)NULL);		/* no restart proc */

	/* figure limiting rectangle for window moves */
	SetRect(&LimitRect,
		screenBits.bounds.left + 3,
		screenBits.bounds.top + 20,
		screenBits.bounds.right - 3,
		screenBits.bounds.bottom -3);

	Set_std();
	SetBounds();

	/* initialize char array for updates */
	p_scr = emalloc(p_size = wc_std.w_cols * wc_std.w_rows);	/* only once */
	p_curs = p_scr;

	Title = sprint("%s%s",Name,version);
	theScreen = NewWindow(&myWindowRec, &myBoundsRect,CtoPstr(Title),
		1,8,(WindowPtr) -1, 1, 0L);

	/* figure an initial window configuration and adjust it */
	wc = &wc_std;
	wc_user = wc_std;	/* initially, only one configuration to toggle */
	user_state(theScreen) = std_state(theScreen);
	SetPort(theScreen);

	(theScreen)->txFont = FONT;
	(theScreen)->txSize = TEXTSIZE;

#ifdef	VARFONT
	GetFontInfo(&f);
		height = f.ascent+f.descent+f.leading;
		width = f.widMax;
		twidth = width * wc->w_cols;
		theight = height * wc->w_rows;
		descent = f.descent;
#endif

	theScreen->txMode = patCopy;
	theScreen->pnMode = patCopy;
	PenNormal();
	cursor = OFF;
}

private void
p_refresh()
{
	int lineno;
	char *curs, *buf;

	buf = emalloc(CO + 1);
	for (lineno = 0; lineno < LI; lineno++) {
		curs = conv_p_curs(lineno,0);
		if (lineno == MAXROW)
			MoveTo(0, (lineno  +1) * height + 2 -descent );
		else
			MoveTo(0, (lineno  +1) * height - descent);
		strncpy(buf + 1, curs, CO);
		buf[0] = (char) CO;
		DrawString(buf);
	}
	putcurs(trow,tcol,OFF);
	free(buf);
}


private bool
wc_adjust(w,h,wcf,init)		/* adjust window config to look nice */
int w, h;
struct wind_config *wcf;
int init;
{
	static int LIMIT_R, LIMIT_C;
	int rows, cols;

	if (init) {
		LIMIT_R = (h - 4) / HEIGHT;
		LIMIT_C = (w - SCROLLWIDTH - 1) / WIDTH + 1;
	}
	if ((w < WIDTH * 40) ||(h < HEIGHT * 10)	/* too small */
	|| ((rows = (h - 4) / HEIGHT) > LIMIT_R)	/* too big */
	|| ((cols = (w - SCROLLWIDTH - 1) / WIDTH + 1) > LIMIT_C))
		return NO;

	wcf->w_rows = rows;
	wcf->w_cols = cols;
	wcf->w_width = wcf->w_cols * WIDTH + 1 + SCROLLWIDTH;
	wcf->w_height = wcf->w_rows * HEIGHT + 4;
	return YES;
}

int
getCO()	/* so that jove knows params */
{
	return wc->w_cols;
}

int
getLI()
{
	return wc->w_rows;
}

private void
SetBounds()
{
	SetRect(&myBoundsRect,
		screenBits.bounds.left + 3,
		screenBits.bounds.top + 40,
		screenBits.bounds.left + 3 + wc_std.w_width,
		screenBits.bounds.top + 40 + wc_std.w_height);
}

private void
Set_std()
{
	(void) wc_adjust(screenBits.bounds.right - screenBits.bounds.left - 6,
		screenBits.bounds.bottom - screenBits.bounds.top - 42,
		&wc_std,1);
}

private void
Reset_std()
{
	Set_std();
	std_state(theScreen) = myBoundsRect;
}
#endif	/* MAC */
