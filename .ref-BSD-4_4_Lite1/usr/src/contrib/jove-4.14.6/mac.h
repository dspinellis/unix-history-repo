/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Macintosh related things. K. Mitchum 2/88 */


#define NMENUS 6
#define NMENUITEMS 40	/* This has GOT to be enough! */

typedef data_obj *menumap[NMENUITEMS];
struct menu {
	char *Name;
	int menu_id;
	MenuHandle Mn;
	menumap m;
};

struct stat {
	int st_dev;		/* volume number */
	long st_ino;		/* file number on volume */
	dev_t st_rdev;
	off_t st_size;		/* logical end of file */
	int st_mode;
	time_t st_mtime;	/* last modified */
};

#define S_IFDIR 2

typedef char *va_list;
#define va_dcl va_list va_alist;
#define va_start(l) { (l) = (va_list)&va_alist; }
#define va_arg(l,m) (((m*)((l) += sizeof(m)))[-1])
#define va_end(l) { (l) = NULL; }



