/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * Copyright (c) 1990, 1991 by Sun Microsystems, Inc.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n\
 Copyright (c) 1990, 1991 by Sun Microsystems, Inc.\n";
#endif not lint

#ifndef lint
static        char sccsid[] = "@(#)ld.c 1.135 90/12/18"; /* from UCB 5.4 85/11/26 */
#endif not lint

/*
 * ld - string table version
 */

#include <sys/param.h>
#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include <ar.h>
#include <a.out.h>
#include <ranlib.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stab.h>
#include <sys/dir.h>
#include <link.h>
#include "dynamic.h"
#include "reloc_info.h"
#ifdef SUNPRO
#include <vroot.h>
#include <report.h>
#endif

int nsymwrite;
char *rindex();

extern int errno;
char *errmsg();

/*
 * Basic strategy:
 *
 * The loader takes a number of files and libraries as arguments.
 * A first pass examines each file in turn.  Normal files are
 * unconditionally loaded, and the (external) symbols they define and require
 * are noted in the symbol table.   Libraries are searched, and the
 * library members which define needed symbols are remembered
 * in a special data structure so they can be selected on the second
 * pass.  Symbols defined and required by library members are also
 * recorded.
 *
 * After the first pass, the loader knows the size of the basic text
 * data, and bss segments from the sum of the sizes of the modules which
 * were required.  It has computed, for each ``common'' symbol, the
 * maximum size of any reference to it, and these symbols are then assigned
 * storage locations after their sizes are appropriately rounded.
 * The loader now knows all sizes for the eventual output file, and
 * can determine the final locations of external symbols before it
 * begins a second pass.
 *
 * On the second pass each normal file and required library member
 * is processed again.  The symbol table for each such file is
 * reread and relevant parts of it are placed in the output.  The offsets
 * in the local symbol table for externally defined symbols are recorded
 * since relocation information refers to symbols in this way.
 * Armed with all necessary information, the text and data segments
 * are relocated and the result is placed in the output file, which
 * is pasted together, ``in place'', by writing to it in several
 * different places concurrently.
 */

/*
 * Internal data structures
 *
 * All internal data structures are segmented and dynamically extended.
 * The basic structures hold 1103 (NSYM) symbols, ~~200 (NROUT)
 * referenced library members, and 100 (NSYMPR) private (local) symbols
 * per object module.  For large programs and/or modules, these structures
 * expand to be up to 40 (NSEG) times as large as this as necessary.
 */
#define	NSEG	160		/* Number of segments, each data structure */
#define	NSYM	1103		/* Number of symbols per segment */
#define	NROUT	250		/* Number of library references per segment */
#define	NSYMPR	200		/* Number of private symbols per segment */

/*
 * Structure describing each symbol table segment.
 * Each segment has its own hash table.  We record the first
 * address in and first address beyond both the symbol and hash
 * tables, for use in the routine symx and the lookup routine respectively.
 * The symfree routine also understands this structure well as it used
 * to back out symbols from modules we decide that we don't need in pass 1.
 *
 * cs points to the current symbol table segment;
 * cs->sy_first[cs->sy_used] is the next symbol slot to be allocated,
 * (unless cs->sy_used == NSYM in which case we will allocate another
 * symbol table segment first.)
 */
struct	symseg {
	struct	nlist *sy_first;	/* base of this alloc'ed segment */
	struct	nlist *sy_last;		/* end of this segment, for n_strx */
	int	sy_used;		/* symbols used in this seg */
	struct	nlist **sy_hfirst;	/* base of hash table, this seg */
	struct	nlist **sy_hlast;	/* end of hash table, this seg */
} symseg[NSEG], nsymseg[NSEG];

struct syminfo {
	struct symseg *fs;		/* address of first segment */ 
	struct symseg *cs;		/* address of current segment */ 
	struct nlist *ls;		/* last symbol entered */
	struct nlist *ns;		/* next symbol entered */
} ldsym = { symseg, symseg, 0, 0 },	/* symbol table for a.out */
  shsym = { nsymseg, nsymseg, 0, 0 };	/* symbols from dynamic objects */

/*
 * The lookup routine uses quadratic rehash.  Since a quadratic rehash
 * only probes 1/2 of the buckets in the table, and since the hash
 * table is segmented the same way the symbol table is, we make the
 * hash table have twice as many buckets as there are symbol table slots
 * in the segment.  This guarantees that the quadratic rehash will never
 * fail to find an empty bucket if the segment is not full and the
 * symbol is not there.
 */
#define	HSIZE	(NSYM*2)

#ifndef n_hash
#define	n_hash	n_desc
#endif n_hash

/*
 * Xsym converts symbol table indices (ala x) into symbol table pointers.
 * Symx (harder, but never used in loops) inverts pointers into the symbol
 * table into indices using the symseg[] structure.
 */
#define	xsym(s, x)	(s[(x)/NSYM].sy_first+((x)%NSYM))
/* symx() is a function, defined below */

struct	nlist cursym;		/* current symbol */
struct	nlist *addsym;		/* first sym defined during incr load */
int	nsym;			/* pass2: number of local symbols in a.out */
/* nsym + symx(s, ldsym.ns) is the symbol table size during pass2 */

struct	nlist **lookup(), **slookup();
struct	nlist *p_etext, *p_edata, *p_end, *entrypt;

/*
 * Definitions of segmentation for library member table.
 * For each library we encounter on pass 1 we record pointers to all
 * members which we will load on pass 2.  These are recorded as offsets
 * into the archive in the library member table.  Libraries are
 * separated in the table by the special offset value -1.
 */
off_t	li_init[NROUT];
struct	libseg {
	off_t	*li_first;
	int	li_used;
	int	li_used2;
} libseg[NSEG] = {
	li_init, 0, 0,
}, *clibseg = libseg;

/*
 * In processing each module on pass 2 we must relocate references
 * relative to external symbols.  These references are recorded
 * in the relocation information as relative to local symbol numbers
 * assigned to the external symbols when the module was created.
 * Thus before relocating the module in pass 2 we create a table
 * which maps these internal numbers to symbol table entries.
 * A hash table is constructed, based on the local symbol table indices,
 * for quick lookup of these symbols.
 */
struct	local {
	int	l_index;		/* index to symbol in file */
	struct	nlist *l_symbol;	/* ptr to symbol table */
	struct	local *l_link;		/* hash link */
} *lochash[LHSIZ], lhinit[NSYMPR];
struct	locseg {
	struct	local *lo_first;
	int	lo_used;
} locseg[NSEG] = {
	lhinit, 0
}, *clocseg;

/*
 * These data structures keep track of symbols of the following type
 * stpic for static pic, dpic for data pic, tpic for text pic (or jump)
 * npic for relocation in non pic modules.
 */
struct	nlist *loc_symb;
struct  slsymb {
	int	sl_offset;
	int	sl_lo;		/* offset into the data/jump linkage */
	struct	nlist sl_symbol;
	struct  slsymb *sl_link;
	u_char	sl_new;	
	int	sl_rc;		/* reference count */
} *stpichash[LHSIZ], *dpichash[LHSIZ], *tpichash[LHSIZ],
  *npichash[LHSIZ],
  stpicinit[NSYMPR], dpicinit[NSYMPR], tpicinit[NSYMPR],
  npicinit[NSYMPR];

struct  slsseg {
	struct slsymb	*sls_first;
	int sls_used;
};
struct  slsseg stpicseg[NSEG] = { stpicinit, 0 }, *stpic;
struct  slsseg dpicseg[NSEG] = { dpicinit, 0 }, *dpic;
struct  slsseg tpicseg[NSEG] = { tpicinit, 0 }, *tpic;
struct  slsseg npicseg[NSEG] = { npicinit, 0 }, *npic;

#define	NSAVETAB	8192
char	*savetab;			/* for symbols build by load1 */
int	saveleft;

struct dynamic dynamic;
struct link_dynamic lkd;
struct link_dynamic_2 lkd2;
struct ld_debug ldd;
int  doff;
int  pad;

#define GT "__GLOBAL_OFFSET_TABLE_"
#define ISGT(x) (!strcmp(GT,x))

#define DYNAMIC 	1
#define SYMBOLIC 	2

int forceflag = DYNAMIC;
int entryflag;			
int referonly;		/* load1 use this flag to decide when to bring in any
			   modules from the .sa archive */
int sa_load;		/* ldrand used this flag to skip lookin at the shsym
			   symbol table when checking out the .sa file */
#define ST_BIND 	0x1
#define DN_BIND 	0x2
#define DEFINITIONS	0x1
#define NOSYMBOLIC	0x2
#define PURE_TEXT	0x4

int bindingflag;
int assertflag;
				   
struct runtime rt, *rtp = &rt;

#define ISDYNAMIC ((entryflag == 0 && ((bindingflag & DN_BIND) || !(bindingflag & ST_BIND))) || dynamic.lib != 0) 

#define SHLIBSTR	1000

char shlibstr[SHLIBSTR];
char *shlibtab = shlibstr;
int shlibleft = SHLIBSTR;

struct slsymb *sllookup();

/*
 * Libraries are typically built with a table of contents,
 * which is the first member of a library with special file
 * name __.SYMDEF and contains a list of symbol names
 * and with each symbol the offset of the library member which defines
 * it.  The loader uses this table to quickly tell which library members
 * are (potentially) useful.  The alternative, examining the symbol
 * table of each library member, is painfully slow for large archives.
 *
 * See <ranlib.h> for the definition of the ranlib structure and an
 * explanation of the __.SYMDEF file format.
 */
int	tnum;		/* number of symbols in table of contents */
int	ssiz;		/* size of string table for table of contents */
struct	ranlib *tab;	/* the table of contents (dynamically allocated) */
char	*tabstr;	/* string table for table of contents */

/*
 * We open each input file or library only once, but in pass2 we
 * (historically) read from such a file at 2 different places at the
 * same time.  These structures are remnants from those days,
 * and now serve only to catch ``Premature EOF''.
 * In order to make I/O more efficient, we provide routines which
 * use the optimal block size returned by stat().
 */
#define BLKSIZE 1024
typedef struct {
	short	*fakeptr;
	int	bno;
	int	nibuf;
	int	nuser;
	char	*buff;
	int	bufsize;
} PAGE;

PAGE	page[2];
int	p_blksize;
int	p_blkshift;
int	p_blkmask;

struct {
	short	*fakeptr;
	int	bno;
	int	nibuf;
	int	nuser;
} fpage;

typedef struct {
	char	*ptr;
	int	bno;
	int	nibuf;
	long	size;
	long	pos;
	PAGE	*pno;
} STREAM;

STREAM	text;
STREAM	reloc;

/*
 * Header from the a.out and the archive it is from (if any).
 */
struct	exec filhdr;
struct  exec outfilhdr;
struct	ar_hdr archdr;
#define	OARMAG 0177545

/*
 * Options.
 */
int	trace;
int	xflag;		/* discard local symbols */
int	Xflag;		/* discard locals starting with 'L' */
int	Sflag;		/* discard all except locals and globals*/
int	rflag;		/* preserve relocation bits, don't define common */
int	arflag;		/* original copy of rflag */
int	sflag;		/* discard all symbols */
int	Mflag;		/* print rudimentary load map */
int	nflag;		/* pure procedure */
int	dflag;		/* define common even with rflag */
int	pflag;		/* force definition of procedures */
int	pdflag;		/* pad text to the next page boundary for 410 file */
int	zflag;		/* demand paged  */
long	hsize;		/* size of hole at beginning of data to be squashed */
int	Aflag;		/* doing incremental load */
int	Tflag = 0;
int	Nflag;		/* want impure a.out */
int	funding;	/* reading fundamental file for incremental load */
int	yflag;		/* number of symbols to be traced */
char	**ytab;		/* the symbols */

/*	Alastair's changes to support Sky Warrior	*/

int   Pflag;          /* number of commons to be aligned */
char  **Ptab;         /* the commons */

#if	TARGET==SUN3 || TARGET==SUN2
int	use68020;	/* 68020-specific instructions used */
#endif	/* 680x0 */

/*
 * These are the cumulative sizes, set in pass 1, which
 * appear in the a.out header when the loader is finished.
 */
off_t	tsize, dsize, bsize, trsize, drsize, ssize;

/*
 * Symbol relocation: c?rel is a scale factor which is
 * added to an old relocation to convert it to new units;
 * i.e. it is the difference between segment origins.
 * (Thus if we are loading from a data segment which began at location
 * 4 in a .o file into an a.out where it will be loaded starting at
 * 1024, cdrel will be 1020.)
 */
long	ctrel, cdrel, cbrel;

/*
 * Textbase is the start address of all text, given by textreloc()
 * unless specified by -T, or unless we are still relocating (-r), in
 * which case it is 0.  Database is the base of all data, computed
 * before and used during pass2.
 */
long	textbase = -1, database = -1;

#ifdef	vax
#define	textreloc() 0
#define pagesize() sys_pagesize
#define segsize() sys_pagesize
#endif	vax

#ifdef	sun
#		define textreloc() (zflag ? PAGSIZ+sizeof (struct exec): PAGSIZ)
#		define pagesize() PAGSIZ
#		define segsize()  SEGSIZ
#		define seground() sizeof (double)
#endif	sun

/*
 * Origins of the text and data segments can be regulated by command-
 * line flags -Ttext & -Tdata (just -T will be interpreted as -Ttext).
 */
struct origopts{
    char	*optname;
    char	*whatbase;
    long	*flagptr;
    long	*whatsize;
} origopts[] = {
	"T",		"text",	&textbase,	&tsize,
	"Ttext",	"text",	&textbase,	&tsize,
	"Tdata",	"data",	&database,	&dsize,
	(char *)0 ,
};

/*
 * changes for Sun-3 and subsequent architectures:
 *
 *   1.	(magic numbers) Magic numbers are 16 bits and are preceded by
 *	a 16-bit machine type field, which is 0 for Sun-2 and earlier
 *	architectures.  Sun-3 has a machine type of 1.
 *
 *   2.	(segment relocation bases) Changed from Sun-2:
 *
 *	text(old):	0x8000
 *	text(new):	0x2000 + sizeof(struct exec)
 *	data(old):	begins at a multiple of 0x8000
 *	data(new):	begins at a multiple of 0x20000
 *
 *   3. (location of a.out header) In all Sun-3 object file formats,
 *	the exec structure is at the beginning of the text segment.
 *	In demand-paged (0413) files, this saves about a page of
 *	disk space.
 */

/*
 * The base addresses for the loaded text, data and bss from the
 * current module during pass2 are given by torigin, dorigin and borigin.
 */
long	torigin, dorigin, ndorigin, borigin;

/*
 * Errlev is nonzero when errors have occured.
 * Delarg is an implicit argument to the routine delexit
 * which is called on error.  We do ``delarg = errlev'' before normal
 * exits, and only if delarg is 0 (i.e. errlev was 0) do we make the
 * result file executable.
 */
int	errlev;
int	delarg	= 4;

/*
 * The biobuf structure and associated routines are used to write
 * into one file at several places concurrently.  Calling bopen
 * with a biobuf structure sets it up to write ``biofd'' starting
 * at the specified offset.  You can then use ``bwrite'' and/or ``bputc''
 * to stuff characters in the stream, much like ``fwrite'' and ``fputc''.
 * Calling bflush drains all the buffers and MUST be done before exit.
 */
struct	biobuf {
	int	b_nleft;		/* Number free spaces left in b_buf */
/* Initialize to be less than b_bufsize initially, to boundary align in file */
	char	*b_ptr;			/* Next place to stuff characters */
	char	*b_buf;			/* Pointer to the buffer */
	int	b_bufsize;		/* Size of the buffer */
	off_t	b_off;			/* Current file offset */
	struct	biobuf *b_link;		/* Link in chain for bflush() */
} *biobufs;
#define	bputc(c,b) ((b)->b_nleft ? (--(b)->b_nleft, *(b)->b_ptr++ = (c)) \
		       : bflushc(b, c))
int	biofd;
off_t	boffset;
struct	biobuf *tout, *dout, *trout, *drout, *sout, *strout, *dynout;

/*
 * Offset is the current offset in the string file.
 * Its initial value reflects the fact that we will
 * eventually stuff the size of the string table at the
 * beginning of the string table (i.e. offset itself!).
 */
off_t	offset = sizeof (off_t);

char	*aoutname = "a.out";	/* name of resultant file: -o argument or a.out */
char	ofilename[MAXNAMLEN+1];
int	ofilemode;		/* respect umask even for unsucessful ld's */
int	infil;			/* current input file descriptor */
char	*filname;		/* and its name */
int	header_num;		/* ordinal # of header file (for dbx) */

#define	NDIRS	100		/* programs are getting bigger */

#define NDEFDIRS 3              /* number of default directories in dirs[] */
char    *dirs[NDIRS];           /* directories for library search */
int	ndir;			/* number of directories */
char	*ldpath1;		/* place to hold make's copy of additionals */
char	*ldpath2;		/* place to hold ld's copy of additionals */
char	*defaults_dir[NDEFDIRS] = {
	"/lib",
	"/usr/lib",
	"/usr/local/lib"
};
#ifdef SUNPRO
pathcellpt sp_dirs[NDIRS];	/* search path containing one directory */
#endif

/*
 * Base of the string table of the current module (pass1 and pass2).
 */
char	*curstr;

/*
 * System software page size
 */

int sys_pagesize;

char 	get();
int	delexit();
char	*savestr();
char	*malloc();
char	*calloc();
char	*mymalloc();

/*
 * list of all libraries both dynamic and static one.
 */
#define PLAIN 0
#define ARCH1 1         /* archive without table of contents */
#define ARCH2 2         /* archive with table of contents */
#define ARCH3 3         /* archive with out of date table of contents */
#define SHLIB 4         /* shared library */

/*
 * definition of flag value
 */
#define DOREADME 1	/* when set do read in my symbol table */
#define DOLOADME 2	/* in load2arg when set do load in needed .o */

struct ldlib {
	char	*ll_name;
	int	ll_flag;
	int	ll_type;
	struct	ldlib *ll_next;
} *hldlp, **ldlpp;

/*
 * Debugging logic
 */
extern printf();
null(){};
int (*dp)();
int is_null = 0;

#ifdef BROWSER
char *cb_program_name = "ld";

int cb_ranlib_saw_library_name = 0;

#include <stab.h>
#define	N_BROWS	0x48		/* Zap when 4.1 <stab.h> is installed */

#ifndef cb_executable_tags_h_INCLUDED
#include "cb_executable_tags.h"
#endif

#endif

main(argc, argv)
	int argc;
	char **argv;
{
	register	int c, i; 
	int		num;
	register	char *ap, **p;
	char		*cp;
	char		save;
	char		*ln;
	char		*getenv();
	char		*ld_opts;
	char		**prepend_argv();
	struct nlist    **spp;
	int		doneflag = 1;

	dp = is_null ? printf : null;
	if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
		signal(SIGINT, delexit);
		signal(SIGTERM, delexit);
	}
	if (argc == 1)
		exit(4);
	alloc_inclstack();
	sys_pagesize = getpagesize();

	/* 
	 * some initializations for doing dynamic linking
	 */
	dpic = dpicseg;
	tpic = tpicseg;
	npic = npicseg;
	rtp->dp = &dynamic;
	rtp->libname = shlibstr;
	assertflag |= DEFINITIONS;
#ifdef SUNPRO
	dovroot();
#endif

	ld_opts = getenv("LD_OPTIONS");
	if (ld_opts != NULL)
		argv = prepend_argv(ld_opts, argv, &argc);	


	/*
	 * Scan arguments to determine the correct setting of
	 * zflag.  The text relocation base depends on it, and
	 * cannot change after the first file is processed.
	 * We also scan to pick up any "-L" flags, so we can set the
	 * list of directories to search for libraries in before
	 * we actually process the libraries.
	 */
	zflag = 1;	/* default: on */
	for (c = 1; c < argc; c++) {
		ap = argv[c];
		if (ap[0] != '-') {
			/* not an option string */
			doneflag = 0;
			continue;
		}
		for (i=1; ap[i]; i++) {
			switch (ap[i]) {
			case 'e':
				entryflag++;
#ifdef BROWSER
				c++;
				goto nextarg;
#endif
			case 'o':
#ifdef BROWSER
				if (!cb_ranlib_saw_library_name) {
					cb_ranlib_start_library(argv[c+1],
						CB_CURRENT_LANGUAGE,
						CB_CURRENT_MAJOR_VERSION,
						CB_CURRENT_MINOR_VERSION,
						CB_EX_FOCUS_UNIT_PROGRAM_FORMAT);
					cb_ranlib_saw_library_name = 1;
				}
				/* Fall thru */
#endif
			case 'u':
				doneflag = 0;
			case 'H':
			case 'D':
			case 'A':
			case 'T':
			case 'a':
				/*
				 * skip the next argument string --
				 * but look at the rest of the flags
				 * in the current one (bletch)
				 */
				c++;
				goto nextarg;
			case 'l':
			case 'y':
			case 'B':
				/*
				 * discard the rest of the current argument
				 * string
				 */
				goto nextarg;
			case 'r':
			case 'n':
			case 'N':
				continue;
			case 'L':
				/*
				 * add a directory to the list of directories
				 * to look for libraries in
				 */
				if (ap[i+1] == '\0')
					error(1, "-L: pathname missing");
				if (ndir >= NDIRS - NDEFDIRS)
					error(1, "-L: too many directories");
#ifdef SUNPRO
				add_dir_to_path(&ap[i+1], &sp_dirs[ndir], 0);
#endif
				dirs[ndir++] = &ap[i+1];
				goto nextarg;
			default:
				continue;
			}
		}
nextarg:
		continue;
	}
	if (doneflag)
		exit(0);

	for (i = 0; i < ndir; i++)
		rtp->spthlen += strlen(dirs[i]) + 1; 
	if (rtp->spthlen) {
		rtp->searchpath = calloc(lalign(rtp->spthlen), 1);
		cp = rtp->searchpath;
		for (i = 0; i < ndir; i++) {
			strcat(cp, dirs[i]);
			strcat(cp, ":");
		}
		cp = rindex(cp, ':');
		*cp = '\0';
	}

#ifdef SUNPRO
	ln = getenv(LDPATH);
	i = ndir;

	if (ln != NULL) {
		ldpath1 = malloc(strlen(ln) + 1);
		strcpy(ldpath1, ln);
		ln = ldpath1;
		for (;;) {
			char *cp = ln;
			while (*cp != '\0' && *cp != ':')
				cp++;
			if (*cp == ':') {
				*cp = '\0';
				add_dir_to_path(ln, &sp_dirs[i++], 0);
				ln = cp + 1;
			} else {
				add_dir_to_path(ln, &sp_dirs[i++], 0);
				break;
			}
		}
	}
#endif

	ln = getenv(LDPATH);

	/* add default search directories */
	if (ln != NULL) {
		ldpath2 = malloc(strlen(ln) + 1);
		strcpy(ldpath2, ln);
		ln = ldpath2;
		for (;;) {
			char *cp = ln;
			while (*cp != '\0' && *cp != ':')
				cp++;
			if (*cp == ':') {
				*cp = '\0';
				dirs[ndir++] = ln;
				ln = cp + 1;
			} else {
				dirs[ndir++] = ln;
				break;
			}
		}
	}
	

#ifdef SUNPRO
	i = ndir;

	add_dir_to_path("/lib", &sp_dirs[i++], 0);
	add_dir_to_path("/usr/lib", &sp_dirs[i++], 0);
	add_dir_to_path("/usr/local/lib", &sp_dirs[i++], 0);
#endif
	for (i = 0; i < NDEFDIRS; i++)
		dirs[ndir++] = defaults_dir[i];

	for (c = 1; c < argc; c++) {
		ap = argv[c];
		if (ap[0] != '-') {
			getlibname(ap);
			continue;
		}
		for (i=1; ap[i]; i++) {
			switch (ap[i]) {
			case 'o':
			case 'u':
			case 'H':
			case 'D':
			case 'e':
			case 'T':
			case 'a':
				/*
				 * skip the next argument string --
				 * but look at the rest of the flags
				 * in the current one (bletch)
				 */
				c++;
				goto nextt;
			case 'l':
				getlibname(&ap[--i]);
			case 'y':
				goto nextt;
			case 'B':
				if (ap[i+1] == '\0')
					error(1, "-B: force option missing");
				if (!strcmp(&ap[i+1],"static"))
					forceflag &= ~DYNAMIC;
				else if (!strncmp(&ap[i+1],"symbolic",8)) {
					char *c = &ap[i+1];

					if (*(c+8) == '=')
						getsymb(c+9);
					bindingflag |= DN_BIND;
					forceflag |= SYMBOLIC;
				} else if (!strcmp(&ap[i+1],"dynamic")) {
					bindingflag |= DN_BIND;
					forceflag |= DYNAMIC;
				} else 
					error(1, "-B: unknown force option");
				goto nextt;
			case 'n':
			case 'N':
			case 'A':
				bindingflag |= ST_BIND;
				forceflag &= ~DYNAMIC;
				continue;
			case 'r':
				rflag++;
				continue;
			case 'L':
				goto nextt;
			default:
				continue;
			}
		}
nextt:
		continue;
	}
	
	if (!rflag)
		initss(rtp);
	else
		if (entryflag)
			error(1, "do not mix -e and -r flags.");
	if (!entryflag && bindingflag != ST_BIND && !rflag)
		textbase = sizeof(struct exec);
	p = argv+1;
	/*
	 * Scan files once to find where symbols are defined.
	 */
	forceflag = DYNAMIC;	/* reset to default value */
	for (c=1; c<argc; c++) {
		if (trace)
			printf("%s:\n", *p);
		filname = 0;
		ap = *p++;
		if (*ap != '-') {
			load1arg(ap);
			continue;
		}
		for (i=1; ap[i]; i++) switch (ap[i]) {

		case 'a':	/* align to page size option */
                       	if (strcmp( &ap[i] ,"align")) {
				if (strcmp( &ap[i] ,"assert"))
                        		error(1, "-a??? option not recognized");
				else {
					char *tmp = *p++;

					if (++c >= argc)
						error(1, "-assert argument missing");
					if (!strcmp(tmp, "definitions"))
					    assertflag |= DEFINITIONS;
					else if (!strcmp(tmp, "nodefinitions"))
					    assertflag &= ~DEFINITIONS;
					else if (!strcmp(tmp, "nosymbolic"))
					    assertflag |= NOSYMBOLIC;
					else if (!strcmp(tmp, "pure-text"))
					    assertflag |= PURE_TEXT;
					else
    error(1, "[no]definitions, nosymbolic, pure-text are the only valid assert options");
					goto next;
				}
			} else {
				if (++c >= argc)
					error(1, "-align argument missing");
				if (Pflag == 0) {   
				       Ptab = (char **)calloc(argc, sizeof (char **));
				       if (Ptab == 0)
					       error(1, "ran out of memory (-align)");
				}
				Ptab[Pflag++] = *p++;
				goto next ;
			}

		case 'B':
			if (ap[i+1] == '\0')
				error(1, "-B: force option missing");
			if (!strcmp(&ap[i+1],"static"))
				forceflag &= ~DYNAMIC;
			else if (!strncmp(&ap[i+1],"symbolic", 8)) {
				forceflag |= SYMBOLIC;
			} else if (!strcmp(&ap[i+1],"dynamic")) {
				forceflag |= DYNAMIC;
			} else 
				error(1, "-B: unknown force option");
			goto next;

 		case 'o':
			if (++c >= argc)
				error(1, "-o: arg missing");
			aoutname = *p++;
			continue;
		case 'u':
		case 'e':
			if (++c >= argc)
				error(1, "-u or -c: arg missing");
			spp = slookup(*p, &ldsym); 
			if (*spp == 0) 
				rtp->fsalloc += strlen(*p) + 1;
			enter(&ldsym, spp, &cursym);
			p++;
			if (ap[i]=='e')
				entrypt = ldsym.ls;
			continue;
		case 'H':
			if (++c >= argc)
				error(1, "-H: arg missing");
			if (tsize!=0)
				error(1, "-H: too late, some text already loaded");
			hsize = atoi(*p++);
			continue;
		case 'A':
			if (++c >= argc)
				error(1, "-A: arg missing");
			if (Aflag) 
				error(1, "-A: only one base file allowed");
			Aflag = 1;
			nflag = zflag = 0;
			funding = 1;
			load1arg(*p++);
			trsize = drsize = tsize = dsize = bsize = 0;
			ctrel = cdrel = cbrel = 0;
			funding = 0;
			addsym = ldsym.ns;
			continue;
		case 'D':
			if (++c >= argc)
				error(1, "-D: arg missing");
			num = htoi(*p++);
			if (dsize > num)
				error(1, "-D: too small");
			dsize = num;
			continue;
		case 'T':
			{
			struct origopts *t;
			for (t = origopts; t->optname; t++){
			    if (!strcmp( &ap[i] ,t->optname))
				break;
			}
			if (t->optname==NULL)
			        error(1, "-%s option unrecognized", &ap[i] );
			if (++c >= argc)
				error(1, "-%s: arg missing", t->optname);
			if (*(t->whatsize)!=0)
				error(1, "-%s: too late,some %s already loaded",
					t->optname,  t->whatbase);
			if (*(t->flagptr)>=0 && !Aflag)
				error(-1, "-%s: %s base already given: old value overridden",
					t->optname,  t->whatbase);
			*(t->flagptr) = htoi(*p++);
			}
			goto next;
		case 'l':
			save = ap[--i]; 
			ap[i]='-';
			load1arg(&ap[i]); 
			ap[i]=save;
			goto next;
		case 'M':
			Mflag++;
			continue;
		case 'x':
			xflag++;
			continue;
		case 'X':
			Xflag++;
			continue;
		case 'S':
			Sflag++; 
			continue;
		case 'r':
			rflag++;
			arflag++;
			/* We don't want to relocate the text. */
			if (textbase != 0 && textbase != -1){
				if (tsize!=0)
					error(1, "-r: too late, some text already loaded");
				else
					error(0, "-T & -r are mutually exclusive");
			}
			textbase = 0;
			continue;
		case 's':
			sflag++;
			xflag++;
			continue;
		case 'd':
			if (ap[i+1] == 'c' || ap[i+1] == '\0')
			    dflag++;
			else if (ap[i+1] == 'p')
			    pflag++;
			else
			    error(1, "bad -d flag");
			goto next;
		case 't':
			trace++;
			continue;
		case 'y':
			if (ap[i+1] == 0)
				error(1, "-y: symbol name missing");
			if (yflag == 0) {
				ytab = (char **)calloc(argc, sizeof (char **));
				if (ytab == 0)
					error(1, "ran out of memory (-y)");
			}
			ytab[yflag++] = &ap[i+1];
			goto next;
		case 'N':
			Nflag++;
			nflag = zflag = 0;
			forceflag &= ~DYNAMIC;
			continue;
		case 'n':
			nflag++;
			Nflag = zflag = 0;
			forceflag &= ~DYNAMIC;
			continue;
		case 'p':
			pdflag++;
			continue;
		case 'z':
			zflag++;
			Nflag = nflag = 0;
			continue;
		case 'L':
			goto next;	/* we already did this one */
		default:
			filname = savestr("-x", &savetab, &saveleft); /* kludge */
			filname[1] = ap[i];		/* kludge */
			archdr.ar_name[0] = 0;		/* kludge */
			error(1, "bad flag");
		}
next:
		;
	}
	ap = rindex( aoutname, '/' );
	if ( ap != 0 ){
	    strncpy( ofilename, aoutname, ap-aoutname+1 );
	}
	strcat( ofilename, "l.outXXXXXX");
	mktemp( ofilename );

	header_num = 0;
	merge_headers();
	endload(argc, argv);
#ifdef BROWSER
	cb_ranlib_exit();
#endif
	exit(0);
}

/*
 * This routine set up theses special symbols for the runtime symbol table.
 * It also set up the values here so calcreloc doesn't account any 
 * reference to these symbols
 */
initss(rt)
	register struct runtime *rt;
{
	register struct nlist *sp;
	struct	nlist **slookup();

	enter(&ldsym, slookup("_etext", &ldsym), &cursym);
	enter(&ldsym, slookup("_edata", &ldsym), &cursym);
	enter(&ldsym, slookup("_end", &ldsym), &cursym);
	rt->fsalloc += 19; 	/* lenght of _etext _edata _end plus 3 null */
	sp = *slookup("_etext", &ldsym);
	ldrsym(sp, -1, N_EXT+N_UNDF);
	sp = *slookup("_edata", &ldsym);
	ldrsym(sp, -1, N_EXT+N_UNDF);
	sp = *slookup("_end", &ldsym);
	ldrsym(sp, -1, N_EXT+N_UNDF);
}

getlibname(cp)
	char *cp;
{
	int	kind;
	int	i;
	int	dummy;
	struct	ldlib *llp;
	struct	ldlib *tllp;
	
	kind = getfile(cp, &dummy, &dummy, NULL);
	close(infil);
	if (kind == PLAIN)
		return (0);

	/* 
	 * first time 
	 */
	if (!hldlp)
		ldlpp = &hldlp;
	else {
		for (tllp = hldlp; tllp != NULL; tllp = tllp->ll_next)
			if (!strcmp(tllp->ll_name, cp))
				return (-1);
	}
	*ldlpp = llp =  (struct ldlib *) mymalloc(sizeof(struct ldlib));
	if (!llp)
		error(1, "getlibname: out of memory\n");
	ldlpp = &llp->ll_next;
	llp->ll_next = NULL;
	if ((llp->ll_name = mymalloc(strlen(filname)+1)) == NULL)
		error(1, "getlibname: out of memory\n");
	else
		strcpy(llp->ll_name, filname);
	switch (kind) {
	case ARCH1:
	case ARCH2:
	case ARCH3:
		for (tllp = hldlp; tllp != llp; tllp = tllp->ll_next)
			if (tllp->ll_type == SHLIB) 
				tllp->ll_flag |= DOREADME;
		break;
	case SHLIB:
		bindingflag |= DN_BIND;
		llp->ll_type = kind;
		break;
	default:
		error(1, "getlibname: unexpected type of file\n");
	}
	return (0);
}

/*
 * Convert a ascii string which is a hex number.
 * Used by -T and -D options.
 */
htoi(p)
	register char *p;
{
	register int c, n;

	n = 0;
	while (c = *p++) {
		n <<= 4;
		if (isdigit(c))
			n += c - '0';
		else if (c >= 'a' && c <= 'f')
			n += 10 + (c - 'a');
		else if (c >= 'A' && c <= 'F')
			n += 10 + (c - 'A');
		else
			error(1, "badly formed hex number");
	}
	return (n);
}

delexit()
{
	struct stat stbuf;
	long size;
	char c = 0;
	register int nwritten;

	bflush();
	unlink(ofilename);
	delete_section_temps(); /* for management of extra sections */
	/*
	 * We have to insure that the last block of the data segment
	 * is allocated a full pagesize block. If the underlying
	 * file system allocates frags that are smaller than pagesize,
	 * a full zero filled pagesize block needs to be allocated so 
	 * that when it is demand paged, the paged in block will be 
	 * appropriately filled with zeros.
	 */
	fstat(biofd, &stbuf);
	size = round(stbuf.st_size, pagesize());
	if (!rflag && zflag && size > stbuf.st_size) {
		lseek(biofd, size - 1, 0);
		nwritten = write(biofd, &c, 1);
		if (nwritten != 1) {
			filname = ofilename;		/* kludge */
			archdr.ar_name[0] = 0;		/* kludge */
			if (nwritten < 0)
				error(-1, "output write error: %s",
				    errmsg(errno));
			else
				error(-1, "output write error: premature EOF");
			delarg = 2;
		}
	}
	if (delarg==0 && Aflag==0)
		(void) chmod(aoutname, ofilemode);
#ifdef BROWSER
	cb_ranlib_exit();
#endif
	exit (delarg);
}

#define DISCARDIT 0xff

endload(argc, argv)
	int argc; 
	char **argv;
{
	register int c, i; 
	long dnum;
	register char *ap, **p;
	struct nlist *sp;

	clibseg = libseg;
	filname = 0;
	middle();
	setupout();

	/*
	 * In the case of static linking then symbol __DYNAMIC 
	 * has a value of zero to tell crt0 that there is no need
	 * for any futher relocation.
	 * Also we dont need to increment ssize to account for
	 * this mkfsym since we are not going to write the __DYNAMIC
	 * that is already in the symbol table.
	 */
	if (!rflag) {
		int savexflag;

		cursym.n_un.n_name = D_NAME;
		savexflag = xflag;
		xflag = 0;
		if ((sp = *lookup(&ldsym)) == 0) {
			if (entryflag) {
				if (rtp->dp->lib != 0)
					error(1, "_DYNAMIC bootstrapping not available: use -Bstatic.");
				else
					mkfsym(cursym.n_un.n_name, 0, N_DATA);
			} else {
				/*
				 * when we are building a shared library here
				 * ++++ is this a reasonable assumption???
				 */
				mkfsym(cursym.n_un.n_name, dorigin, N_DATA);
				if (forceflag & SYMBOLIC)
					*(rtp->dt + abs(rtp->dp->got_off)) =
					    dorigin; 	
			}
		} else {
			mkfsym(sp->n_un.n_name, sp->n_value, N_DATA);
			/*
			 * we are setting the first location of the glob 
			 * table to contain the location of the dynamic
			 * structure to assist the runtime loader in 
			 * locating it.
			 */
			if (sp->n_value != 0)
				*(rtp->dt + abs(rtp->dp->got_off)) = dorigin; 	
			sp->n_type = DISCARDIT;
		}
		xflag = savexflag;
		nsym++;
		forceflag |= DYNAMIC; 		/* reset it to default value */
	}

	p = argv+1;
	for (c=1; c<argc; c++) {
		ap = *p++;
		if (trace)
			printf("%s:\n", ap);
		if (*ap != '-') {
			load2arg(ap);
			continue;
		}
		for (i=1; ap[i]; i++) switch (ap[i]) {

		case 'D':
			dnum = htoi(*p);
			if (dorigin < dnum)
				while (dorigin < dnum)
					bputc(0, dout), dorigin++;
			/* fall into ... */
		case 'u':
		case 'e':
		case 'o':
		case 'H':
			++c; 
			++p;
			/* fall into ... */
		default:
			continue;
		case 'A':
			funding = 1;
			load2arg(*p++);
			funding = 0;
			c++;
			continue;
		case 'a':	/* Page align */
		case 'T':
			++c; 
			++p;
			/* fall into ... */
		case 'y':
		case 'L':
			goto next;
		case 'B':
			if (!strcmp(&ap[i+1],"static"))
				forceflag &= ~DYNAMIC;
			else if (!strncmp(&ap[i+1],"symbolic", 8))
				forceflag |= SYMBOLIC;
			else if (!strcmp(&ap[i+1],"dynamic"))
				forceflag |= DYNAMIC;
			goto next;
		case 'l':
			ap[--i]='-'; 
			load2arg(&ap[i]);
			goto next;
		case 'n':
		case 'N':
			forceflag &= ~DYNAMIC;
			continue;
		}
next:
		;
	}
	finishout();
}

struct lslib {
	char	*libname;
	struct lslib *lib_next;
};
struct lslib *rd_shsb();

/*
 * Scan file to find defined symbols.
 */
load1arg(cp)
	register char *cp;
{
	register	struct ranlib *tp;
	off_t	nloc;
	int	kind;
	int	i;
	char	*p, *pp, *tpp;
	int	maj = 0;
	int	min = 0;
	struct	ldlib *tllp;
	struct lslib *lp = 0;
	static	int nosa = 0;

	/* 
	 * If the user hasn't somehow specified a textbase (by -T or -r)
	 * take the default.
	 */
	if (textbase == -1 && !Aflag)
		textbase = textreloc();
	kind = getfile(cp, &maj, &min, NULL);
	if (Mflag)
		printf("%s\n", filname);
	switch (kind) {

	/*
	 * Plain file.
	 */
	case PLAIN:
		load1(0, 0L);
		break;

	/*
	 * Archive without table of contents.
	 * (Slowly) process each member.
	 */
	case ARCH1:
		error(-1,
"warning: archive has no table of contents; add one using ranlib(1)");
		nloc = SARMAG;
		while (step(nloc))
			nloc += sizeof(archdr) +
			    round(atol(archdr.ar_size), sizeof (short));
		break;

	/*
	 * Archive with table of contents.
	 * Read the table of contents and its associated string table.
	 * Pass through the library resolving symbols until nothing changes
	 * for an entire pass (i.e. you can get away with backward references
	 * when there is a table of contents!)
	 */
	case ARCH2:
		nloc = SARMAG + sizeof (archdr);
		dseek(&text, nloc, sizeof (tnum));
		mget((char *)&tnum, sizeof (tnum), &text);
		nloc += sizeof (tnum);
		tab = (struct ranlib *)mymalloc(tnum);
		if (tab == 0)
			error(1, "ran out of memory (toc)");
		dseek(&text, nloc, tnum);
		mget((char *)tab, tnum, &text);
		nloc += tnum;
		tnum /= sizeof (struct ranlib);
		dseek(&text, nloc, sizeof (ssiz));
		mget((char *)&ssiz, sizeof (ssiz), &text);
		nloc += sizeof (ssiz);
		tabstr = (char *)mymalloc(ssiz);
		if (tabstr == 0)
			error(1, "ran out of memory (tocstr)");
		dseek(&text, nloc, ssiz);
		mget((char *)tabstr, ssiz, &text);
		for (tp = &tab[tnum]; --tp >= tab;) {
			if (tp->ran_un.ran_strx < 0 ||
			    tp->ran_un.ran_strx >= ssiz)
				error(1, "mangled archive table of contents");
			tp->ran_un.ran_name = tabstr + tp->ran_un.ran_strx;
		}
		while (ldrand())
			continue;
		free((char *)tab);
		free(tabstr);
		nextlibp(-1);
		break;

	/*
	 * Table of contents is out of date, so search
	 * as a normal library (but skip the __.SYMDEF file).
	 */
	case ARCH3:
		error(-1,
"warning: table of contents for archive is out of date; rerun ranlib(1)");
		nloc = SARMAG;
		do
			nloc += sizeof(archdr) +
			    round(atol(archdr.ar_size), sizeof(short));
		while (step(nloc));
		break;
	/*
	 * shared library file. 
	 */
	case SHLIB:
		/*
		 * look up ldlib table to see if we need to read its symbol
		 * table.
		 */
		for (tllp = hldlp; tllp; tllp = tllp->ll_next) {
			if (!strcmp(tllp->ll_name, filname)) {
				if ((tllp->ll_flag & DOREADME) 
				    || dflag || pflag)
					lp = rd_shsb(filname);
				break;
			}
		}
		p = cp;
		if (p[0] == '-' && p[1] == 'l') {  
			/*
			 * remove version string if one was specified;
			 * save shlib name, then put version string back
			 */
			if ((tpp = rindex(p,'.')) != 0) {
				while (*tpp == '.' || isdigit(*tpp)) {
					if (*tpp == '.')
						pp = tpp;
					tpp--;
				}
				*pp = '\0';
				savestr(p+2, &shlibtab, &shlibleft);
				*pp = '.';
			} else
				savestr(p+2, &shlibtab, &shlibleft);
			rtp->lko[rtp->lko_i].lo_library = 1;
			rtp->lko[rtp->lko_i].lo_major = maj;
			rtp->lko[rtp->lko_i].lo_minor = min;
			rtp->dp->libstr += (strlen(p+2) + 1);
			/*
			 * if we are forced to define common then we need
			 * to bring in the sister library (of the form libx.sa)
			 * which is of an archive form and has all the 
			 * definitions of both initialized and uninitialized
			 * commons for the shared library libx.so.
			 * if it is only -dp then we simply wanted to get the
			 * size information from the sister library (libx.sa) in
			 * order to assist later on the calcution of jump slots
			 * for relocation in non pic code.
			 */
			if ((dflag || pflag) && !nosa) {
				if ((p = rindex(filname, '/')) == 0)
					p = filname;
				while (*p != '.')
					p++;
				strncpy(++p, "sa", 2);
				if (open(filname, O_RDONLY) >= 0) {
					/*
					 * +++ need to check here whether
					 * or not it is of the archive type
					 * of a file.
					 */
					tllp->ll_flag |= DOLOADME;
					sa_load = 1;
					if (pflag && !dflag)
					referonly = 1;
					load1arg(filname);
					referonly = 0;
					sa_load = 0;
				}
			} 
		} else {
			savestr(p, &shlibtab, &shlibleft);
			rtp->lko[rtp->lko_i].lo_library = 0;
			rtp->lko[rtp->lko_i].lo_major = 0;
			rtp->lko[rtp->lko_i].lo_minor = 0;
			rtp->dp->libstr += (strlen(p) + 1);
		}
		if (shlibtab > &shlibstr[SHLIBSTR])
			error(1,"out of memory for shlib strings");
		rtp->dp->lib++;
		rtp->lko_i++;
		nosa = 1;	/* Don't process .sa files for cascaded refs */
		while (lp != 0) {
			getlibname(lp->libname);
			load1arg(lp->libname);
			lp = lp->lib_next;
		}
		nosa = 0;
		break;
	}
	close(infil);
}

/*
 * Read symbols from shared object *name.
 */
#define VERSION2 2

struct lslib *
rd_shsb(name)
	char *name;
{
	struct nlist *sp;		/* Allocated symbol table */
	struct nlist *csp;		/* Current symbol entry */
	char *str;			/* Allocated string area */
	int i;				/* General temporary */
	int fd;				/* fd on *name */
	struct exec exec;		/* For examining header of *name */
	struct link_dynamic dinfo;	/* For examining dynamic structures */
	struct link_dynamic_2 d2;	/* More of the same */
	int ssize;			/* Calculated symbol table size */
	char *buf;			/* buffer for link objects */
	struct lslib *lp = 0, **lpp = &lp;
    
	/*
	 * Open and sanity check *name.
	 */
	if ((fd = open(name, O_RDONLY)) == -1)
		error(1, "rd_shsb: can't open shared library %s", name);
	if (read(fd, &exec, sizeof(struct exec)) !=  sizeof(struct exec))
		error(1, 
		    "rd_shsb: can't read struct exec for shared library %s",
		    name);	

	/*
	 * Object's __DYNAMIC is assumed to be first thing right after
	 * text.  XXX
	 */
	lseek(fd, exec.a_text, L_SET);
	if (read(fd, &dinfo, sizeof(struct link_dynamic)) !=  
	    sizeof (struct link_dynamic))
		error(1, 
		    "rd_shsb: can't read struct dynamic for shared library %s",
		    name);	

	/*
	 * Version check the interface and get the appropriate instantiation
	 * of the __DYNAMIC structure.
	 */
	if (dinfo.ld_version < VERSION2)
		error(1, 
		    "rd_shsb: ld no longer supports this version of %s\n",
		    name);
	lseek(fd, exec.a_text+sizeof(dinfo)+sizeof(struct ld_debug), L_SET);
	if (read(fd, &d2, sizeof(struct link_dynamic_2)) != sizeof(d2))
		error(1, 
		    "rd_shsb: ld can't read link_dynamic_2 of file %s", name);

	/*
	 * Get the symbol table.  Note assumption of layout of dynamic 
	 * information in object, namely that symbol strings immediately
	 * follow the symbol table.
	 */
	dinfo.v2 = &d2;

	if (dinfo.v2->ld_need) {
		struct link_object *lko;
		char tmp[1024];
		char *cp, *cp1;

		/*
		 * get the object name. This is a bad hack... there should
		 * be a field denoting the space needed for this aera.
		 */
#define LO_BUFSIZE 0x2000
		buf = malloc(LO_BUFSIZE);
		lseek(fd, dinfo.v2->ld_need, L_SET);
		if (read(fd, buf, LO_BUFSIZE) == -1)
			error(1, "rd_shsb: can't read link_object stuff\n");
		
		lko = (struct link_object *) buf;
		while (1) {
			*lpp = (struct lslib *)malloc(sizeof(struct lslib)); 
			(*lpp)->lib_next = (struct lslib *)0;
			cp = buf + ((int)lko->lo_name - dinfo.v2->ld_need);
			if (lko->lo_library) {
				strcpy(tmp, "-l");
				strcat(tmp, buf + ((int) lko->lo_name -
				    dinfo.v2->ld_need));
				cp = malloc(strlen(tmp)+1);
				strcpy(cp, tmp);
				(*lpp)->libname = cp;
				lpp = &((*lpp)->lib_next);
			} else {
				cp1 = malloc(strlen(cp) + 1);
				strcpy(cp1, cp);
				(*lpp)->libname = cp1;
				lpp = &((*lpp)->lib_next);
			}
			if (lko->lo_next)
	 			lko = (struct link_object *) (buf + 
				    (int) lko->lo_next - dinfo.v2->ld_need);
			else
				break;
		}
		free(buf);
	}

	ssize = dinfo.v2->ld_symbols - dinfo.v2->ld_stab;
	if ((sp = (struct nlist *) mymalloc(ssize)) == NULL)
		error(1, 
		    "rd_shsb: out of memory for object %s with symbol size %d",
		    name, ssize);
	lseek(fd, dinfo.v2->ld_stab, L_SET);
	if (read(fd, sp, ssize) !=  ssize)
		error(1, 
		    "rd_shsb: can't read symbol table for object %s",
		    name);	

	/*
	 * Get the strings.
	 */
	if ((str = mymalloc(dinfo.v2->ld_symb_size)) == NULL)
		error(1, 
		    "rd_shsb: out of memory for strings for object %s",
		    name);
	lseek(fd, dinfo.v2->ld_symbols, L_SET);
	if (read(fd, str, dinfo.v2->ld_symb_size) != dinfo.v2->ld_symb_size)
		error(1, "rd_shsb: can't read strings for shared library %s",
		    name);	

	/*
	 * Process the symbols in this dynamic object.
	 */
	csp = sp;
	for (i = 0; i < (ssize / sizeof (struct nlist)); i++, csp++) {

		/*
		 * Chuck symbols that are not "extern".
		 */
		if (!(csp->n_type&N_EXT)) 
			continue;

		/*
		 * Fixup pointer to allocated string area, and then
		 * get the "current" symbol.
		 */
		csp->n_un.n_name = str + csp->n_un.n_strx;
		cursym = *csp;

		/*
		 * Add this symbol.  If it is new, we're done.
		 */
		if (enter(&shsym, lookup(&shsym), &cursym))
			continue;

(*dp)("shsb: collision with %s and %s\n", shsym.ls->n_un.n_name, 
    cursym.n_un.n_name);
		/*
		 * Symbol collides.  If what it collided with is already
		 * defined, skip over it.  
		 */
		if (shsym.ls->n_type != N_EXT+N_UNDF)
			continue;

(*dp)("shsb: collision was with N_EXT+N_UNDF %s\n", shsym.ls->n_un.n_name);
		/*
		 * Collision with an undefined symbol.  If the symbol we're
		 * examining is not a definition, and it has a value, then
		 * accumulate that value (e.g., common size) into the symbol
		 * we collided with.  Is this an error?  Shouldn't we check
		 * the type of the collided symbol first?  I bet so...
		 */
		if (cursym.n_type == N_EXT+N_UNDF) {
			if (cursym.n_value > shsym.ls->n_value)
				shsym.ls->n_value = cursym.n_value;
			continue;
		}

(*dp)("shsb: collision because of new definition of %s, type %x, value %x\n",
    cursym.n_un.n_name, cursym.n_type, cursym.n_value);
		/*
		 * Symbol we're adding is a definition of some kind.  If
		 * the symbol we collided with already has a value, and
		 * the addition is in text, then we're done.
		 * XXX Why?
		 */
		if (shsym.ls->n_value != 0 && cursym.n_type == N_EXT+N_TEXT)
			continue;

(*dp)("shsb: collision adds a definition for a reference\n");
		/*
		 * Symbol we're adding is a definition for a symbol we already
		 * have a reference for.  Replace the reference with the
		 * definition and value we're supplying now.
		 */
		shsym.ls->n_type = cursym.n_type;
		shsym.ls->n_value = cursym.n_value;
	}

	/*
	 * Leave the symbol strings intact, but free up the symbol table
	 * scratch space we've allocated and get rid of the descriptor on
	 * the library.
	 */
	free((char *) sp);
	close(fd);
	return(lp);
}

/*
 * Advance to the next archive member, which
 * is at offset nloc in the archive.  If the member
 * is useful, record its location in the liblist structure
 * for use in pass2.  Mark the end of the archive in libilst with a -1.
 */
step(nloc)
	off_t nloc;
{

	dseek(&text, nloc, (long) sizeof archdr);
	if (text.size <= 0) {
		nextlibp(-1);
		return (0);
	}
	getarhdr();
(*dp)("step: %.16s\n", archdr.ar_name);
	if (load1(1, nloc + (sizeof archdr)))
		nextlibp(nloc);
	return (1);
}

/*
 * Record the location of a useful archive member.
 * Recording -1 marks the end of files from an archive.
 * The liblist data structure is dynamically extended here.
 */
nextlibp(val)
	off_t val;
{

	if (clibseg->li_used == NROUT) {
		if (++clibseg == &libseg[NSEG])
			error(1, "too many files loaded from libraries");
		clibseg->li_first = (off_t *)mymalloc(NROUT * sizeof (off_t));
		if (clibseg->li_first == 0)
			error(1, "ran out of memory (nextlibp)");
	}
	clibseg->li_first[clibseg->li_used++] = val;
	if (val != -1 && Mflag)
		printf("\t%s\n", archdr.ar_name);
}

/*
 * One pass over an archive with a table of contents.
 * Remember the number of symbols currently defined,
 * then call step on members which look promising (i.e.
 * that define a symbol which is currently externally undefined).
 * Indicate to our caller whether this process netted any more symbols.
 */
ldrand()
{
	register struct nlist *sp, **hp;
	register struct ranlib *tp, *tplast;
	off_t loc;
	int nsymt = symx(&ldsym, ldsym.ns);

	tplast = &tab[tnum-1];
	for (tp = tab; tp <= tplast; tp++) {
		if ((hp = slookup(tp->ran_un.ran_name, &ldsym)) == 0)
			goto next;
		if ((sp = *hp) == NULL)
			goto next;
		if (sp->n_type != N_EXT+N_UNDF)
			continue;
		else
			goto stepit;
next:
		if (sa_load)
			continue;
		if ((hp = slookup(tp->ran_un.ran_name, &shsym)) == 0)
			continue;
		if ((sp = *hp) == NULL)
			continue;
		if (sp->n_type != N_EXT+N_UNDF)
			continue;
stepit:		
		step(tp->ran_off); 
		loc = tp->ran_off;
		while (tp < tplast && (tp+1)->ran_off == loc)
			tp++;
	}
	return (symx(&ldsym, ldsym.ns) != nsymt);
}

/*
 * Examine a single file or archive member on pass 1.
 */
load1(libflg, loc)
	off_t loc;
{
	register struct local *lp;
	register struct nlist *sp;
	struct nlist *savnext;
	int ndef, nlocal, type, size, nsymt;
	register int i;
	off_t maxoff;
	struct stat stb;
	int symno;
	register struct nlist *csp;
	off_t saveloc = loc;
	struct nlist **hp;
	int lpicflag;
	static int symsize = 0;

	new_obj1();
	readhdr(loc);

	for (i = 0; i < LHSIZ; i++)
		lochash[i] = 0;
	clocseg = locseg;
	clocseg->lo_used = 0;
	symno = -1;

	if (loc_symb == 0) {
		symsize = filhdr.a_syms;
	       	loc_symb = (struct nlist *) calloc(
		   filhdr.a_syms/sizeof(struct nlist), sizeof(struct local));
	       	if (loc_symb == 0)
			error(1, "out of memory for relocation symbols");
	} else {
		/*
		 * check if number of symbols of current file exceed 
		 * the last one, if so free up the last one and allocate
		 * a new one.
		 */
		if (filhdr.a_syms > symsize) {
			symsize = filhdr.a_syms;
			free(loc_symb);
	       		loc_symb = (struct nlist *) calloc(
		   	    symsize/sizeof(struct nlist),sizeof(struct local));
			if (loc_symb == 0)
				error(1, 
				    "out of memory for relocation symbols");
		}
	}
		
	csp = loc_symb;

	if (filhdr.a_syms == 0) {
		if (filhdr.a_text+filhdr.a_data == 0) {
			if (libflg == 0) {
				ssize += sizeof(cursym);
			}
			return(0);
		}
		error(1, "no namelist");
	}
	if (libflg)
		maxoff = atol(archdr.ar_size);
	else {
		fstat(infil, &stb);
		maxoff = stb.st_size;
	}
	if (N_STROFF(filhdr) + sizeof (off_t) >= maxoff)
		error(1, "too small (old format .o?)");
	ctrel = tsize; cdrel += dsize; cbrel += bsize;
	ndef = 0;
	nlocal = sizeof(cursym);
	savnext = ldsym.ns;
	loc += N_SYMOFF(filhdr);
	dseek(&reloc, loc + filhdr.a_syms, sizeof(off_t));
	mget(&size, sizeof (size), &reloc);
	dseek(&reloc, loc + filhdr.a_syms+sizeof (off_t), size-sizeof (off_t));
	curstr = (char *)mymalloc(size);
	if (curstr == NULL)
		error(1, "no space for string table");
	mget(curstr+sizeof(off_t), size-sizeof(off_t), &reloc);

	/*
	 * If we are -r'ing a -pic file, we need to "globalize" local
	 * symbols so they're around when the output gets passed through
	 * us again.
	 */
	lpicflag = 0;
	if (rflag) {
		dseek(&text, loc, filhdr.a_syms);
		while (text.size > 0) {
			mget((char *)&cursym, sizeof (struct nlist), &text);
			if (cursym.n_un.n_strx == 0) 
				continue;
			if (cursym.n_un.n_strx < sizeof (size) ||
			    cursym.n_un.n_strx >= size)
				error(1, "bad string table index (pic pass 1)");
			cursym.n_un.n_name = curstr + cursym.n_un.n_strx;
			if (ISGT(cursym.n_un.n_name)) {
				lpicflag = 1;
				break;
			}
		}
	}

	/* 
	 * Now process each symbol.
	 */
	dseek(&text, loc, filhdr.a_syms);
	while (text.size > 0) {
		symno++;
		mget((char *)&cursym, sizeof(struct nlist), &text);
		if (cursym.n_un.n_strx) {
			if (cursym.n_un.n_strx<sizeof(size) ||
			    cursym.n_un.n_strx>=size)
				error(1, "bad string table index (pass 1)");
			cursym.n_un.n_name = curstr + cursym.n_un.n_strx;
#ifdef BROWSER
			if (cursym.n_type == N_BROWS) {
				if (!cb_ranlib_saw_library_name) {
					cb_ranlib_saw_library_name = 1;
					cb_ranlib_start_library(aoutname,
								CB_CURRENT_LANGUAGE,
								CB_CURRENT_MAJOR_VERSION,
								CB_CURRENT_MINOR_VERSION,
								CB_EX_FOCUS_UNIT_PROGRAM_FORMAT);
				}
				cb_ranlib_symbol(cursym.n_un.n_name,
						 cb_focus_program_unit);
			}
#endif
(*dp)("load1: %s\t%x\t%x\n", cursym.n_un.n_name, cursym.n_type, 
    cursym.n_value);
		}
		type = cursym.n_type;
		if (type & N_STAB) {
			if (type == N_BINCL) {
				start_incl1(&cursym, header_num);
				header_num++;
			} else if (type == N_EINCL) {
				end_incl1();
			} else {
				stab1(&cursym);
			}
		}

		/*
		 * do not keep track of any dbx symbols for load1reloc
		 * +++ is it safe?
		 */
		if (!(type & N_STAB)) {
			if (csp > loc_symb + (symsize/sizeof(struct nlist)))
				error(1,
				   "load1: ran out of local symbol space");
			if (clocseg->lo_used == NSYMPR) {
				if (++clocseg == &locseg[NSEG])
					error(1, "local symbol overflow");
				clocseg->lo_used = 0;
			}
			if (clocseg->lo_first == 0) {
				clocseg->lo_first = (struct local *)
				    calloc(NSYMPR, sizeof (struct local));
				if (clocseg->lo_first == 0)
					error(1, "out of memory (clocseg)");
			}
			lp = &clocseg->lo_first[clocseg->lo_used++];
			lp->l_index = symno;
			*csp = cursym;
			lp->l_symbol = csp++;
			lp->l_link = lochash[symno % LHSIZ];
			lochash[symno % LHSIZ] = lp;
		}

		if ((type&N_EXT)==0) {
			/*
			 * compiling with pic flag generated local symbol
			 * and we dont wanted to write these back out.
			 */
			if (type & N_STAB || cursym.n_un.n_name[0]!='L' ||
				(rflag && cursym.n_un.n_name[0]== 'L'
				 && (lpicflag)))
				nlocal += sizeof cursym;
			continue;
		}
		symreloc();

		/*
		 * Make an entry for this symbol.  If it is new, then
		 * see if a dynamic object has referenced it.  If it has,
		 * then calculate any common contributions or, if this
		 * symbol contributes a definition, increment count of
		 * definitions seen.
		 *
		 * If the symbol is not new, then see if other references
		 * have now been satisfied or calculate this symbol's
		 * common contribution (if appropriate).  If this symbol
		 * satisfies a previously undefined reference, then bump count
		 * of definitions seen.
		 */
		if (enter(&ldsym, lookup(&ldsym), &cursym)) {
			if ((hp = lookup(&shsym)) == 0)
				continue;
			if (*hp == NULL)
				continue;
			if ((*hp)->n_type != N_EXT+N_UNDF)
				continue;
			if (ldsym.ls->n_type == N_EXT+N_UNDF) {
				if ((*hp)->n_value > ldsym.ls->n_value)
					ldsym.ls->n_value = (*hp)->n_value;
			} else
				ndef++;
			continue;
		} else {
			if ((sp = ldsym.ls)->n_type != N_EXT+N_UNDF)
				continue;
			if (cursym.n_type == N_EXT+N_UNDF) {
				if (cursym.n_value > sp->n_value)
					sp->n_value = cursym.n_value;
				continue;
			}
			if (referonly)
			    continue;
			if (sp->n_value != 0 && cursym.n_type == N_EXT+N_TEXT)
				continue;
			ndef++;
			sp->n_type = cursym.n_type;
			sp->n_value = cursym.n_value;
		}
	}
	if (libflg==0 || ndef) {
		tsize += filhdr.a_text;
		dsize += round(filhdr.a_data, seground());
		bsize += round(filhdr.a_bss, seground());
		ssize += nlocal;
		trsize += filhdr.a_trsize;
		drsize += filhdr.a_drsize;
		if (funding) {
			if (textbase == -1 && Tflag == 0) {
				if ((*slookup("_end", &ldsym)) == 0) {
					error(1, "base address is not provided, use option \"-T\" or link the fundamental file with option \"-u _end\"");
				}
				textbase = (*slookup("_end", &ldsym))->n_value;
			} 
		}
		nsymt = symx(&ldsym, ldsym.ns);
		for (i = symx(&ldsym, savnext); i < nsymt; i++) {
			sp = xsym(ldsym.fs, i);
			if (sp->n_un.n_name) {
				rtp->fsalloc += strlen(sp->n_un.n_name) + 1;
				sp->n_un.n_name = savestr(sp->n_un.n_name,
							&savetab, &saveleft);
			}
		}
		load1rel(saveloc);
		free(curstr);
		return (1);
	}
	/*
	 * No symbols defined by this library member.
	 * Rip out the hash table entries and reset the symbol table.
	 */
	incl_free();
	symfree(&ldsym, savnext);
	free(curstr);
	return(0);
}

#define TEXTSEG 0
#define DATASEG 1

struct dslot sl;
struct rl rl;
int relocused;


/*
 * This routine looked all the relocation datums to determine how 
 * much space is needed for pic data, pic static, pic and force allocation
 * of a jump entry.
 */
load1rel(loc)
	off_t loc;
{
	int i;

	/* 
	 * reintialize static pic stuff after each module
	 */
	for (i = 0; i < LHSIZ; i++)
		stpichash[i] = 0;
	stpic = stpicseg;
	stpicseg->sls_used = 0;

	loc += N_TXTOFF(filhdr);
	dseek(&text, loc, filhdr.a_text);
	dseek(&reloc, loc+filhdr.a_text+filhdr.a_data, filhdr.a_trsize);
	load1reltd(TEXTSEG);
	dseek(&text, loc+filhdr.a_text, filhdr.a_data);
	dseek(&reloc, loc+filhdr.a_text+filhdr.a_data+filhdr.a_trsize,
	    filhdr.a_drsize);
	load1reltd(DATASEG);
}

#undef relocation_info
#if	TARGET== SUN4
#	define relocation_info	reloc_info_sparc
#	define r_symbolnum	r_index
#endif
#if	TARGET==SUN2 || TARGET==SUN3
#	define relocation_info	reloc_info_68k
#endif  /* mc68000 */

/*
 * Get symbol local to the object file being relocated.
 */
static
struct nlist *
getlocsymb(rp)
	struct relocation_info *rp;
{
	register struct local *lp;
	
	lp = lochash[rp->r_symbolnum % LHSIZ];
	for (;;) {
		if (lp == 0)
			error(1, 
			    "object file inconsistency: nonexistent symbol");
		if (lp->l_index == rp->r_symbolnum) 
			break;
		lp = lp->l_link;
	}
	return(lp->l_symbol);
}

int	picflag = 0;

load1reltd(seg)
	int seg;
{
	register struct nlist *sp;
	register struct relocation_info *rp, *rpend;
	register struct slsymb *ps;
	struct relocation_info *relp;
	char *codep;
	register char *cp;
	int relsz, codesz;
	int offset;

	relsz = reloc.size;
	relp = (struct relocation_info *)mymalloc(relsz);
	codesz = text.size;
	codep = (char *)mymalloc(codesz);
	if (relp == 0 || codep == 0)
		error(1, "out of memory (load2td)");
	mget((char *)relp, relsz, &reloc);
	rpend = &relp[relsz / sizeof (struct relocation_info)];
	mget(codep, codesz, &text);
	for (rp = relp; rp < rpend; rp++) {
		/*
		 * keep track of the global separeted from the local references
		 */
#if	TARGET==SUN4
		switch (rp->r_type) {
		    /*
		     * skip relocation for __GLOBAL_OFFSET_TABLE_ 
		     */
		case RELOC_PC22:
		case RELOC_PC10:
			continue;
		case RELOC_BASE13:
		case RELOC_BASE10:
		case RELOC_BASE22:
			if (!picflag) {
				if (rp->r_type == RELOC_BASE13)
					picflag = 1;
				else 
					picflag = 2;
			} else {
				if ((picflag == 1 && rp->r_type != RELOC_BASE13)
					|| (picflag == 2 && 
						!(rp->r_type == RELOC_BASE10 ||
						 rp->r_type == RELOC_BASE22)))
					error(1, "can't mixed pic and PIC .o");
					
			}
			sp = getlocsymb(rp);
			if (rp->r_extern) {
				ps = sllookup(&dpic, &dpicseg[NSEG], dpichash,
				    sp, rp->r_addend, 1, -1);
				if (ps->sl_new)
					sl.ds++;
			} else {
				 ps = sllookup(&stpic, &stpicseg[NSEG],
				     stpichash, sp, rp->r_addend, 0, -1);
				 if (ps->sl_new)
					sl.ss++;
			}
			continue;
		case RELOC_JMP_TBL:
			sp = getlocsymb(rp);
			ps = sllookup(&tpic, &tpicseg[NSEG], tpichash, sp,
					0, 1, -1);
			if (ps->sl_new)
			   	 sl.js++;
			continue;
		default:
			break;
		}
#endif
#if	TARGET==SUN2 || TARGET==SUN3
		cp = codep + rp->r_address;
		switch (rp->r_length) {

		case 0:		/* byte */
			offset = *cp;
			break;
		case 1:		/* word */
			offset = *(short *)cp;
			break;
		case 2:		/* long */
			/* "cp" points to an least a 16-bit boundary, but
			 * not necessarily a 32-bit boundary.
			 */
#ifdef mc68000		/* 68k host can do long accesses on 16-bit boundaries */
			offset = *(long *)cp;
#else /*!mc68000*/	/* others can only do long accesses on 32-bit bdy's */
			*((short*)      (&offset)       ) = *((short*)  cp  );
			*((short*)(((char*)(&offset))+2)) = *((short*)(cp+2));
#endif /*mc68000*/
			break;
		default:
			error(1, "load1reltd botch: bad length");
		}
		if (rp->r_baserel || rp->r_jmptable) {
			if (rp->r_jmptable && !rp->r_extern)
				continue;
			sp = getlocsymb(rp);
			if (rp->r_baserel) {
				if (!picflag) {
					if (rp->r_length == 1)
						picflag = 1;
					else 
						picflag = 2;
				} else {
					if ((picflag == 1 && rp->r_length == 2)
					|| (picflag == 2 && rp->r_length == 1))
						error(1, "can't mixed pic and PIC .o");
				}
			}
			if (rp->r_extern) {
				if (rp->r_baserel) {
					 ps = sllookup(&dpic, &dpicseg[NSEG],
					     dpichash, sp, offset, 1, -1);
					if (ps->sl_new)
						sl.ds++;
				} else if (rp->r_jmptable) {
					 ps = sllookup(&tpic, &tpicseg[NSEG],
					     tpichash, sp, 0, 1, -1);
					 if (ps->sl_new)
						sl.js++;
				}
			} else {
				if (rp->r_baserel) {
					 ps = sllookup(&stpic, &stpicseg[NSEG],
					     stpichash, sp, offset, 0, -1);
					 if (ps->sl_new)
						sl.ss++;
				}
			}
			continue;
		}
#endif

		if (seg == DATASEG) {
			rl.rl_d++;
			if (rp->r_extern) {
				sp = getlocsymb(rp);
#if	TARGET==SUN2 || TARGET==SUN3
				if (rp->r_pcrel && ISGT(sp->n_un.n_name))
					rl.rl_d--;
				else {
					rl.rl_de++;
					(void) sllookup(&npic, &npicseg[NSEG],
					    npichash, sp, 0, 1, -1);
				}
#endif
#if	TARGET==SUN4
				rl.rl_de++;
				(void) sllookup(&npic, &npicseg[NSEG], npichash,
				    sp, 0, 1, -1);
#endif
			}
		} else {
			/*
			 * dont count if symbol is "__GLOBAL_OFFSET_TABLE_"
			 */
			rl.rl_t++;
			if (rp->r_extern)  { 
				sp = getlocsymb(rp);
#if	TARGET==SUN4
				rl.rl_te++;
				(void) sllookup(&npic, &npicseg[NSEG], npichash,
				    sp, 0, 1, -1);
#endif
#if	TARGET==SUN2 || TARGET==SUN3
				if (rp->r_pcrel && ISGT(sp->n_un.n_name))
					rl.rl_t--;
				else {
					rl.rl_te++;
					(void) sllookup(&npic, &npicseg[NSEG],
					    npichash, sp, 0, 1, -1);
				}
#endif
			}
		}
	}
	free(relp);
	free(codep);
}

/*
 * this is a generalized routine to do look up on 3 sort of lists:
 * the static, data and jump pic list. The static list is only good per
 * module basis while the data and jump list span to all modules.
 */
char 	*psavetab;	/* for symbols needed by load1rel */ 
int 	psaveleft;

struct slsymb *
sllookup(slsseg, endseg, slslot, sp, offset, save, lo)
	struct slsseg **slsseg;
	struct slsseg *endseg;
	struct slsymb *slslot[];
	struct nlist *sp;
	int offset;
	int save;
	int lo;
{
	register int i;
	register struct slsymb *ps;
	struct slsymb *slfindit();

	i = hashit(sp);
	ps = slfindit(slslot, sp, offset, i);
	if (ps != 0) {
		ps->sl_rc++;
		return(ps);
	}
	if ((*slsseg)->sls_used == NSYMPR) {
		if (++(*slsseg) == endseg) 
			error(1, "pic symbol overflow");
		(*slsseg)->sls_used = 0;
	}
	if ((*slsseg)->sls_first == 0) {
		(*slsseg)->sls_first = ps = (struct slsymb *)
			calloc(NSYMPR, sizeof (struct slsymb));
		if ((*slsseg)->sls_first == 0)
			error(1, "out of memory(slsseg)");
	}
	ps = &(*slsseg)->sls_first[(*slsseg)->sls_used++];
	ps->sl_offset = offset;
	ps->sl_symbol = *sp;
	if (save) {
		ps->sl_symbol.n_un.n_name = 
		    savestr(ps->sl_symbol.n_un.n_name, &psavetab, &psaveleft);
	}
	ps->sl_link = slslot[i];
	/*
	 * a value of -1 for the linkoffset  means that no slot is assigned
	 * for this symbol yet.
	 */
	ps->sl_lo = lo;	
	ps->sl_new = 1;
	ps->sl_rc++;
	slslot[i] = ps;
	return(ps);
}

int
hashit(sp)
	struct nlist *sp;
{
	int i;
	char *cp;

	i = 0;
	for (cp = sp->n_un.n_name; *cp;)
		i = (i<<1) + *cp++;
	return ((i & 0x7fffffff) % LHSIZ);
}


struct slsymb *
slfindit(slslot, sp, offset, slot)
	struct slsymb *slslot[];
	struct nlist *sp;
{
	register struct slsymb *ps;

	/* now compare name,offset pair */
	ps = slslot[slot];
	while (ps != 0) {
		if (!strcmp(ps->sl_symbol.n_un.n_name, sp->n_un.n_name)) {
			if (ps->sl_offset == offset) {
				ps->sl_new = 0;
				return(ps);
			}
		}
		ps = ps->sl_link;
	}
	return(ps);
}

/*
 * How the fast symbol hash table is build:
 * 	- There can be at most (dataslot + jumpslot) number of hash slots
 *	of which the first RTHS slot are the initial buckets. Each
 * 	bucket has 2 items: one denoting the ordinal symbol number
 *	the other one a pointer in the form of an index to the slots
 *	following the first RTHS slots.
 *	- Following right after the hash table for the symbols are the
 *	symbols themselves followed by the strings.
 */


/* 
 * This routine look up to the hash table for dynamic symbols.
 * return the ordinal symbol number if symbol is found
 * else add the symbol and return a -1  
 */
int
fslookup(sp, rt)
	register struct nlist *sp;
	register struct runtime *rt;
{
	register int i;
	register char *cp;
	register struct fshash *p;
	struct nlist *sp1 = rt->sp;
	static int fs = 0;		/* ordinal number for next symbol */

	i = 0;
	for (cp = sp->n_un.n_name; *cp;)
		i = (i<<1) + *cp++;
	i = (i & 0x7fffffff) % rt->buckets;

	p = rt->hp + i; 
	if (p->fssymbno == -1) {
		p->fssymbno = fs++;
		addfs(sp, rt);
	} else {
		do {
		    if (!strcmp(sp->n_un.n_name, rt->fsstr +
				    (sp1+p->fssymbno)->n_un.n_strx))
			    return(p->fssymbno);
		    else if (p->next == 0) {
			    p->next = rt->hp_ind;
			    p = rt->hp + rt->hp_ind;
			    rt->hp_ind++;
			    if (p > rt->hp_last)
				    error(1, "out of hash space");
			    p->fssymbno = fs++;
			    p->next = 0;
			    addfs(sp, rt);
			    break;
		    }
		} while (p = rt->hp + p->next);
	}
	return -1;
}

addfs(sp, rt)
	register struct nlist *sp;
	register struct runtime *rt;
{
	register char *s = sp->n_un.n_name;
	register int len;

	*(rt->spp) = *sp; 
	rt->spp->n_un.n_strx = rt->fsoff;
	len = strlen(s) + 1;
	strcpy(rt->fsstr + rt->fsoff, s);
	rt->fsoff += len;
	rt->spp++;
	if (rt->spp > rt->sp_last)
		error(1, "out of dynamic symbol space");
	if (rt->fsoff > rtp->fsalloc) 
		error(1, "out of string space");
}

static int nund = 0;
static int undbanner = 0;

static int
ck_shs(sp, ps, p)
	register struct nlist *sp;
	register struct slsymb *ps;
	register struct slsymb *p;
{
	int count = 0;
	register struct nlist **hp;

	hp = lookup(&shsym);
	if (*hp == NULL) {
			if (assertflag & DEFINITIONS) {
				if (undbanner == 0) {
					error(0, "Undefined symbol ");
					undbanner = 1;
				}
				error(-2, "   %s ", sp->n_un.n_name);
			}
	} else {
		if ((*hp)->n_type == N_EXT+N_TEXT) {
			count += p->sl_rc - 1;
			ps = sllookup(&tpic,&tpicseg[NSEG],tpichash,
				sp, 0, 1, -1);
			if (ps->sl_new)
				sl.js++;
		}
	}
	return (count);
}

/*
 * this routine check whether a symbol is in the procedure table or 
 * in the shared library symbol table.
 * ++++ the handling of _end and company is ugly here. must search
 * for a better way to do it.
 */
int
calcreloc()
{
	register int i;
	register int count;
	register int tcount = 0;
	register struct slsymb *p;
	register struct slsymb *ps;
	register struct nlist *sp;
	register struct nlist **hp;

	if (entryflag == 0) 	/* needed to do relative to absolute */
		return (sl.ds + sl.ss + sl.js + rl.rl_d + rl.rl_t);

	/*
	 * go through the dataslot, jumpslot symbols and dont count the
	 * symbols that are defined. Also go throught the nonpic
	 * referred symbols and discount symbols that are defined.
	 */
	count = sl.ds;
	for (i = 0; i < LHSIZ; i++) {
		p = dpichash[i];
		while (p != 0) {
			cursym = p->sl_symbol;
			if  ((sp = *lookup(&ldsym)) == 0)
				error(1, "calrelco symb lookup botch");
			/*
			 * if we are not forced to define common and
			 * the symbol is a defined common (non zero
			 * size) then we still have to allocate 
			 * relocation record for that symbol except
			 * the case where the symbol is one of these
			 * special ones _end, _etext, _edata.
			 */
			if (dflag == 0 && sp->n_type == N_EXT+N_UNDF && 
				sp->n_value != 0) {
				if (strcmp("_end", sp->n_un.n_name) &&
				    strcmp("_etext", sp->n_un.n_name) &&
				    strcmp("_edata", sp->n_un.n_name)) {
					p = p->sl_link;
					continue;
				}
			}
			if (sp->n_type != N_EXT+N_UNDF || 
			    sp->n_value != 0)
				count--;
			else {
				ps = slfindit(dpichash, sp, 0, hashit(sp));
				(void) ck_shs(sp, ps, p);
			}
			p = p->sl_link;
		}
	}

	/*
	 * should I allowed people to jump to _end, _edata, _etext
	 * here ++++++++++
	 */
	tcount += count;
	count = sl.js;
	for (i = 0; i < LHSIZ; i++) {
		p = tpichash[i];
		while (p != 0) {
			cursym = p->sl_symbol;
			if  ((sp = *lookup(&ldsym)) == 0)
				error(1, "calrelco symb lookup botch");
			if (sp->n_type != N_EXT+N_UNDF || sp->n_value != 0)
				count--;
			else {
				ps = slfindit(tpichash, sp, 0, hashit(sp));
				(void) ck_shs(sp, ps, p);
			}
			p = p->sl_link;
		}
	}
	tcount += count;
	count = rl.rl_de + rl.rl_te;

	/*
	 * if we are not forced to allocate common and procedure
	 * then what we need to discount are the relocations to defined
	 * symbols that are not of the type N_EXT+N_UNDF with a non zero
	 * size.
	 *
	 * if we are forced to define both then what we needed 
	 * are the relocations for the extra jump slots (at this
	 * time whatever symbols that are left in this table (npichash)
	 * of the type N_EXT+N_UNDF with a size of 0 should have a
	 * definition in the shsym symbol table of type N_EXT+N_TEXT)  
	 *
	 * if we are forced to define only common, then we can only 
	 * discount all relocation to N_EXT+N_UNDF with non zero size.
	 *
	 * if we are forced to define only procedure then we can 
	 * discount a) relocation(s) to defined routine in user program,
	 * b) for relocation(s) to routine already in the jump table
	 * we can discount the reference count to that symbol, 
	 * c) the last step here is to determine if the symbol is
	 * a common in which case we can't discount the references to it.
	 * the algorithm we used to determine whether it is a common is
	 * to check whether the type is N_UNDF+N_EXT with a non zero size.
	 * (we presume that the ".sa" file's symbols are used to update
	 * the size information for any match found in the static symbol
	 * table; however none of the modules are not brought in.) 
	 * if the symbol is not a common then we need then to decide 
	 * whether it is a routine by checking to see if it's type
	 * is  N_EXT+N_TEXT in the shlib symbol table; we can discount
	 * reference count - 1 since we need a relocation datum for that
	 * entry in the jump table.
	 */ 
	for (i = 0; i < LHSIZ; i++) {
		p = npichash[i];
		while (p != 0) {
			cursym = p->sl_symbol;
			if  ((sp = *lookup(&ldsym)) == 0)
				error(1, "calrelco symb lookup botch");
	
			/*
			 * case where we are not forced to define
			 * both commons and procedures
			 */
			if (dflag == 0 && pflag == 0) {
				if (sp->n_type != N_EXT+N_UNDF)
					count -= p->sl_rc;
				else {
					if (!strcmp("_end", sp->n_un.n_name) ||
					    !strcmp("_etext",sp->n_un.n_name) ||
					    !strcmp("_edata", sp->n_un.n_name))
						count -= p->sl_rc;
				}
			} else if (dflag && pflag) {
			/*
			 * case where we are forced to define
			 * both commons and procedures
			 */
				if (sp->n_type != N_EXT+N_UNDF ||
				    sp->n_value != 0) 
					count -= p->sl_rc;
				else {
					if ((ps = slfindit(tpichash, sp,
					    0, hashit(sp))))
						count -= p->sl_rc;
					else
						count -= ck_shs(sp, ps, p);
				}
			} else if (dflag) {
			/*
			 * common are forced to be defined (-dc).
			 */
				if (sp->n_type != N_EXT+N_UNDF || 
				    sp->n_value != 0)
					count -= p->sl_rc;
			} else {
			/*
			 * only procedure are forced to be defined (-dp)
			 */
				if (sp->n_type != N_EXT+N_UNDF)
					count -= p->sl_rc;
				else if (!strcmp("_end", sp->n_un.n_name) ||
					 !strcmp("_etext", sp->n_un.n_name) ||
					 !strcmp("_edata", sp->n_un.n_name))
						count -= p->sl_rc;
				else if (sp->n_value == 0) {
					if ((ps = slfindit(tpichash,
					    sp, 0, hashit(sp))))
						count -= p->sl_rc;
					else
						count -= ck_shs (sp, ps, p);
				}
			}
			p = p->sl_link;
		}
	}
	tcount += count;
	return(tcount);
}

middle()
{
	register struct nlist *sp;
	register struct nlist *sp1;
	long csize, t, corigin, ocsize;
	int rnd;
	char s;
	register int i;
	register int j;
	int nsymt;
	int otsize;

	torigin = 0; 
	dorigin = 0; 
	borigin = 0;
	
	if (!rflag) {
		/*
		 * needed to kludge this here so calcreloc to discount 
		 * the reference to __DYNAMIC from crt0.s.
		 */
		if ((sp = *slookup(D_NAME, &ldsym)) != NULL)
			ldrsym(sp, -1, N_EXT+N_DATA);

		if ((ISDYNAMIC) || forceflag & SYMBOLIC) {
			rt_init(rtp);
			for (i = 0; i < NSEG; i++) 
				if (ldsym.fs[i].sy_first != 0) {
					sp = ldsym.fs[i].sy_first;
					for (j = 0; j<ldsym.fs[i].sy_used;j++) {
						fslookup(sp, rtp);
						sp++;
					}
				} else 
					break;
			dj_init(rtp, &sl, 1);
			dp_init(rtp);
		} else  {
			/*
			 * case of pic code link statically
			 */
			dj_init(rtp, &sl, 0);
		}
	}

	p_etext = *slookup("_etext", &ldsym);
	p_edata = *slookup("_edata", &ldsym);
	p_end = *slookup("_end", &ldsym);

	nsymt = symx(&ldsym, ldsym.ns);

	if (rflag) 
		sflag = zflag = 0;

	/*
	 * Assign common locations.
	 */
	csize = 0;
	if (!Aflag)
		addsym = ldsym.fs[0].sy_first;
#ifdef sun
	if (zflag) {
		/*
		 * in Sun-x demand-paged programs,
		 * the exec structure is in text space
		 */
		tsize += sizeof(struct exec);
	}
#endif sun

	otsize = tsize;		/* original tsize without shared lib stuff */
	if (!rflag && (ISDYNAMIC))
		tsize +=  dynamic.rs + dynamic.hs + dynamic.ss + dynamic.sts +
		    dynamic.libstr + (rtp->spthlen ? lalign(rtp->spthlen) : 0) +
		    (rtp->dp->lib*sizeof(struct link_object));

#if	(TARGET==SUN2) || (TARGET==SUN3) /* i.e. mc68000 */
	/*
	 * prevent programs from ending exactly on a page boundary;
	 * if they do, they will coredump as they prefetch the last
	 * "unlk a6; rts" instruction sequence (bug in unlk instruction)
	 */
	if (tsize > 0 && tsize % pagesize() == 0) {
		tsize += sizeof(long);
	}
#endif	/* mc68000 */
	if (database == -1 ){
		/* compute normal, default data base */
		database = textbase + tsize;
#ifdef sun
		if (zflag) {
			/*
			 * Don't count the header TWICE.  If we do, and
			 * the data segment gets put at the next segment
			 * boundary as a result, we'll disagree with exec,
			 * and data references will end up in no man's land.
			 */
			database -= sizeof(struct exec);
		}
#endif	sun
		if (!Aflag)
			database = round(database, (nflag||zflag||pdflag ?
				segsize() : seground()));
		database += hsize;
	}

	/*
	 * +++ dont allocate common if dflag is not on
	 */
	if (!rflag || dflag) {
		ldrsym(p_etext, tsize, N_EXT+N_TEXT);
		ldrsym(p_edata, dsize, N_EXT+N_DATA);
		ldrsym(p_end, bsize, N_EXT+N_BSS);
		if (dflag || forceflag & SYMBOLIC || bindingflag == ST_BIND ||
		    (entryflag && rtp->dp->lib == 0)) {
			for (i = symx(&ldsym, addsym); i < nsymt; i++) {
				sp = xsym(ldsym.fs, i);
				if ((s=sp->n_type)==N_EXT+N_UNDF &&
				    (t = sp->n_value)!=0) {
					if (t >= sizeof (double))
						rnd = sizeof (double);
					else if (t >= sizeof (long))
						rnd = sizeof (long);
					else
						rnd = sizeof (short);
					/*
					 * check if this common is to
					 * be page aligned 
					 */
					if (Pflag && sp->n_un.n_name) {
						int ii;

						for (ii = 0; ii < Pflag; ii++)
					   		if(Ptab[ii][1] ==
							   sp->n_un.n_name[1] &&
						 	   !strcmp(Ptab[ii],
							   sp->n_un.n_name)) {
						 	rnd = pagesize();
						 	t = round(t,pagesize());
						 	break;
							}
					}
					csize = round(csize, rnd);
					sp->n_value = csize;
					sp->n_type = N_EXT+N_COMM;
					ocsize = csize;	
					csize += t;
				}
				if (s&N_EXT && (s&N_TYPE)==N_UNDF && s&N_STAB) {
					sp->n_value = ocsize;
					sp->n_type =(s&N_STAB) | (N_EXT+N_COMM);
				}
			}
		}
	}

	/*
	 * Now set symbols to their final value 
	 */
	if (!rflag && ((ISDYNAMIC) || (dynamic.ds + dynamic.js))) {
		lkd.v2 = &lkd2;
		lkd.ldd = &ldd;
		init_lkd(&lkd, rtp, otsize, database);
		i = sizeof(struct link_dynamic) + sizeof(struct link_dynamic_2)
		    + sizeof (struct ld_debug) + dynamic.ds + dynamic.js;
		if (i % sizeof(double))
			pad = sizeof(double) - (i % sizeof(double));
		doff = i + pad;
	}

	csize = round(csize, seground());
	torigin = textbase;
	dorigin = database;
	ndorigin = database + doff;
	corigin = ndorigin + dsize;
        if(Pflag) { /* make sure commons start on a page boundary */
        	corigin = round(corigin, pagesize());
        } else 
		corigin = round(corigin, seground()); /* Commons must start on seg bdy. */
 	borigin = corigin + csize;

	nsymt = symx(&ldsym, ldsym.ns);
	for (i = symx(&ldsym, addsym); i<nsymt; i++) {
		sp = xsym(ldsym.fs, i);
		switch (sp->n_type & (N_TYPE+N_EXT)) {
		case N_EXT+N_UNDF:
			if (arflag == 0 && !(ISDYNAMIC) && 
			    !(dynamic.ds + dynamic.js) )
				errlev |= 01;
			if ((arflag==0 || dflag) && sp->n_value==0) {
				if (sp==p_end || sp==p_etext || sp==p_edata)
					continue;
				if (undbanner == 0)
					if (!(ISDYNAMIC) && 
					    !ISGT(sp->n_un.n_name)) {
					    	error(0, "Undefined symbol");
						undbanner = 1;
					}
				if (!rflag && !ISGT(sp->n_un.n_name))
				    nund++;
				if (!(ISDYNAMIC) && !ISGT(sp->n_un.n_name))
				    error(-2, "   %s", sp->n_un.n_name);
			}
			continue;
		case N_EXT+N_ABS:
		default:
			continue;
		case N_EXT+N_TEXT:
			sp->n_value += torigin;
			continue;
		case N_EXT+N_DATA:
			sp->n_value += ndorigin;
			continue;
		case N_EXT+N_BSS:
			sp->n_value += borigin;
			continue;
		case N_EXT+N_COMM:
			/*
			 * ++++ why this in sun4 ld
			 */
			if (dflag || rflag == 0) {
			    sp->n_type = (sp->n_type & N_STAB) | (N_EXT+N_BSS);
			    sp->n_value += corigin;
			}
			continue;
		}
	}
	if (!rflag)
		if ((sp = *slookup(GT, &ldsym)) != NULL)
		    ldrsym(sp, lkd.v2->ld_got, N_EXT+N_DATA);


	rtp->us = nund;
	if (sflag || xflag)
		ssize = 0;

	if ( (sp = *slookup(D_NAME, &ldsym)) != NULL) {
		/*
		 * now load in the real value of __DYNAMIC.
		 * if there is a definition for __DYNAMIC and rflag is on
		 * just leave it alone (i.e ld -x -r crt0.o)
		 */
		if (!rflag) {
			sp->n_type = N_EXT+N_DATA;
			if (ISDYNAMIC || forceflag & SYMBOLIC)
				sp->n_value = dorigin;
			else
				sp->n_value = 0;
		}
	} else {
		/* 
		 * if there is no reference to __DYNAMIC symbol
		 * then we need to account for it here since we will
		 * be writing this symbol in endload.
		 * In the case where it is already in the ldsym table
		 * then we dont need to account for it here since we
		 * wanted to avoid writing out the same symbol twice.
		 */
		if (!rflag)
			ssize += sizeof cursym;
	}

	bsize += csize;
	nsym = ssize / (sizeof cursym);
	if (Aflag) {
		fixspec(p_etext,torigin);
		fixspec(p_edata,dorigin);
		fixspec(p_end,borigin);
	}

	/*
	 * update value field for all the symbols used for dynamic linking.
	 */
	if (!rflag)
		if ((ISDYNAMIC) || forceflag & SYMBOLIC) {
			sp1 = rtp->sp;
			for (i = 0; i < totalsymb(); i++) {
				cursym.n_un.n_name = rtp->fsstr + 
				    sp1->n_un.n_strx;
				if ( (sp = *lookup(&ldsym)) == 0 )
					error(1, "fast symbol not found");
				sp1->n_value = sp->n_value;
				sp1->n_type = sp->n_type;
				sp1++;
			}
		}

	/*
	 * Finally, verify that all .so symbols would be satisfied if we
	 * were to run now.
	 */
	if (!(assertflag & DEFINITIONS))
		return;
	nsymt = symx(&shsym, shsym.ns);
	for (i = symx(&shsym, 0); i < nsymt; i++) {
		sp = xsym(shsym.fs, i);
		if ((sp->n_type == (N_EXT + N_UNDF)) && (sp->n_value == 0)) {
			cursym = *sp;
			sp1 = *lookup(&ldsym);
			if (sp1 == NULL ||
			    ((sp1->n_type == (N_EXT + N_UNDF)) &&
			    (sp1->n_value == 0))) {
				if (undbanner == 0) {
					error(0, "Undefined symbol");
					undbanner = 1;
				}
				error(-2, "   %s", sp->n_un.n_name);
			}
		}
	}
}

fixspec(sym,offset)
	struct nlist *sym;
	long offset;
{

	if (symx(&ldsym, sym) < symx(&ldsym, addsym) && sym!=0)
		sym->n_value += offset;
}

ldrsym(sp, val, type)
	register struct nlist *sp;
	long val;
{

	if (sp == 0)
		return;
	if ((sp->n_type != N_EXT+N_UNDF) && !Aflag) {
		error(0, "%s: user attempt to redefine loader-defined symbol",
			sp->n_un.n_name);
		return;
	}
	sp->n_type = type;
	sp->n_value = val;
}

off_t	wroff;
struct	biobuf toutb;

setupout()
{
	int bss;
	struct stat stbuf;
	int ds;
	int ntsize;

	ofilemode = 0777 & ~umask(0);
	biofd = creat(ofilename, 0666 & ofilemode);
	if (biofd < 0) {
		filname = ofilename;		/* kludge */
		archdr.ar_name[0] = 0;		/* kludge */
		error(1, errmsg(errno));	/* kludge */
	}
	fstat(biofd, &stbuf);		/* suppose file exists, wrong*/
	if (stbuf.st_mode & 0111) {	/* mode, ld fails? */
		chmod(ofilename, stbuf.st_mode & 0666);
		ofilemode = stbuf.st_mode;
	}
#if	TARGET==SUN3 || TARGET==SUN2
	outfilhdr.a_machtype = (use68020 ? M_68020 : M_68010);
#endif	/* mc680x0 */
#if	TARGET == SUN4
	outfilhdr.a_toolversion = TV_SUN4;
	outfilhdr.a_machtype = M_SPARC;
#endif /* sun4 */
	outfilhdr.a_magic = nflag ? NMAGIC : (zflag ? ZMAGIC : OMAGIC);
	outfilhdr.a_text = round(tsize,zflag||pdflag?pagesize():seground());
	ds = dsize;
	if (!rflag && ((ISDYNAMIC) || (dynamic.ds + dynamic.js))) {
		if (ISDYNAMIC)
			outfilhdr.a_dynamic = 1;
		ds += doff;
		lkd.v2->ld_text = outfilhdr.a_text;
	}
	outfilhdr.a_data = round(ds, zflag ? pagesize() : seground());
	bss = bsize - (Pflag ? 0 : (outfilhdr.a_data - ds));
	if (bss < 0)
		bss = 0;
	outfilhdr.a_bss = bss = round(bss, seground());
	outfilhdr.a_trsize = trsize;
	outfilhdr.a_drsize = drsize;
	outfilhdr.a_syms = sflag? 0: 
	    (ssize + (sizeof cursym)*(symx(&ldsym, ldsym.ns)-rtp->us));
	if (entrypt) {
		if (entrypt->n_type!=N_EXT+N_TEXT)
			error(0, "entry point not in text");
		else
			outfilhdr.a_entry = entrypt->n_value;
	} else
		outfilhdr.a_entry = textbase;
	outfilhdr.a_trsize = (rflag ? trsize:0);
	outfilhdr.a_drsize = (rflag ? drsize:0);
	tout = &toutb;
	bopen(tout, 0, stbuf.st_blksize);
	bwrite((char *)&outfilhdr, sizeof (outfilhdr), tout);
#ifndef sun
	if (zflag)
		bseek(tout, pagesize());
#endif sun
	wroff = N_TXTOFF(outfilhdr) + outfilhdr.a_text;
	/*
	 * in case of static linking only space for data and jump linkage
	 * is needed else the dynamic header, relocation datum and the fast
	 * symbol table will be added before the real data segment began.
	 */
	if (!rflag && ((ISDYNAMIC) || (dynamic.ds + dynamic.js))) {
		struct link_dynamic lk;

		outb(&dynout, doff, stbuf.st_blksize);
		lk.ld_version = 3;	
		(int) (lk.ldd) = (int)database + sizeof(struct link_dynamic);
		(int) (lk.v2) =  (int)lk.ldd + sizeof(struct ld_debug);
		lkd.v2->ld_buckets = rtp->buckets;
		bwrite((char *)&lk, sizeof(struct link_dynamic), dynout);
		bwrite((char *)(lkd.ldd), sizeof(struct ld_debug), dynout);
		bwrite((char *)(lkd.v2), sizeof(struct link_dynamic_2), dynout);
	}

	outb(&dout, outfilhdr.a_data - doff, stbuf.st_blksize);
	if (rflag) {
		outb(&trout, outfilhdr.a_trsize, stbuf.st_blksize);
		outb(&drout, outfilhdr.a_drsize, stbuf.st_blksize);
	}
	if (sflag==0 || xflag==0) {
		outb(&sout, outfilhdr.a_syms, stbuf.st_blksize);
		wroff += sizeof (offset);
		outb(&strout, 0, stbuf.st_blksize);
	}
}

outb(bp, inc, bufsize)
	register struct biobuf **bp;
{
	*bp = (struct biobuf *)mymalloc(sizeof (struct biobuf));
	if (*bp == 0)
		error(1, "ran out of memory (outb)");
	bopen(*bp, wroff, bufsize);
	wroff += inc;
}

int	localsymbolno = 0;

load2arg(acp)
char *acp;
{
	register char *cp;
	register int i;
	register int j;
	register char *p;
	off_t loc;
	int dummy;
	struct	ldlib *tllp;
	long maxoff;

	cp = acp;
	if ((i = getfile(cp, &dummy, &dummy, &maxoff)) == 0) {
		while (*cp)
			cp++;
		while (cp >= acp && *--cp != '/');
		mkfsym(++cp, torigin, N_TEXT);
		localsymbolno++;
		load2(0L, maxoff);
	} else {	/* scan archive members referenced */
		if (i != SHLIB) {
			for (;;) {
				if (clibseg->li_used2 == clibseg->li_used) {
					if (clibseg->li_used < NROUT)
						error(1, "libseg botch");
					clibseg++;
				}
				loc = clibseg->li_first[clibseg->li_used2++];
				if (loc == -1)
					break;
				dseek(&text, loc, (long)sizeof(archdr));
				getarhdr();
				mkfsym(archdr.ar_name, torigin, N_TEXT);
				load2(loc + (long)sizeof(archdr), atol(archdr.ar_size));
			}
		} else {
			for (tllp = hldlp; tllp; tllp = tllp->ll_next) {
				if (!strcmp(tllp->ll_name, filname)) {
					if (tllp->ll_flag & DOLOADME) {
						if (!(p = rindex(filname, '/')))
							p = filname;
						while (*p != '.')
							p++;
						strncpy(++p, "sa", 2);
						load2arg(filname);
					}
					break;
				}
			}
		}
	}
	close(infil);
}

load2(loc, maxoff)
int loc;
long maxoff;
{
	int size;
	register struct nlist *sp;
	register struct local *lp;
	register int symno, i;
	int type;
	int exclude;
	register struct nlist *csp;
	int lpicflag;
	int lsymbno;

	exclude = 0;
	readhdr(loc);
	if (!funding) {
		ctrel = torigin;
		cdrel += ndorigin;
		cbrel += borigin;
	}
	/*
	 * Reread the symbol table, recording the numbering
	 * of symbols for fixing external references.
	 */
	for (i = 0; i < LHSIZ; i++)
		lochash[i] = 0;
	clocseg = locseg;
	clocseg->lo_used = 0;
	symno = -1;
	csp = loc_symb;
	/* 
	 * reintialize static pic stuff after each module
	 */
	for (i = 0; i < LHSIZ; i++)
		stpichash[i] = 0;
	stpic = stpicseg;
	stpicseg->sls_used = 0;

	loc += N_TXTOFF(filhdr);
	dseek(&text, loc+filhdr.a_text+filhdr.a_data+
		filhdr.a_trsize+filhdr.a_drsize+filhdr.a_syms, sizeof(off_t));
	mget(&size, sizeof(size), &text);
	dseek(&text, loc+filhdr.a_text+filhdr.a_data+
		filhdr.a_trsize+filhdr.a_drsize+filhdr.a_syms+sizeof(off_t),
		size - sizeof(off_t));
	curstr = (char *)mymalloc(size);
	if (curstr == NULL)
		error(1, "out of space reading string table (pass 2)");
	mget(curstr+sizeof(off_t), size-sizeof(off_t), &text);
	/*
	 * detect the existance of the extra sections.
	 * go blat them to temp files.
	 */
	if ( maxoff > N_STROFF(filhdr)+size ){
		/* it should be out there. go get it */
		collect_extra_sections( &text, loc,
		    filhdr.a_text+filhdr.a_data+
		    filhdr.a_trsize+filhdr.a_drsize+
		    filhdr.a_syms+size, maxoff );
	}

	/*
	 * If we are -r'ing a -pic file, we need to "globalize" local
	 * symbols so they're around when the output gets passed through
	 * us again.
	 */
	lpicflag = 0;
	if (rflag) {
		dseek(&text, loc+filhdr.a_text+filhdr.a_data+
		    filhdr.a_trsize+filhdr.a_drsize, filhdr.a_syms);
		while (text.size > 0) {
			mget((char *)&cursym, sizeof (struct nlist), &text);
			if (cursym.n_un.n_strx == 0) 
				continue;
			if (cursym.n_un.n_strx < sizeof (size) ||
			    cursym.n_un.n_strx >= size)
				error(1, "bad string table index (pic pass 1)");
			cursym.n_un.n_name = curstr + cursym.n_un.n_strx;
			if (ISGT(cursym.n_un.n_name)) {
				lpicflag = 1;
				break;
			}
		}
	}

	/* 
	 * Now process each symbol.
	 */
	dseek(&text, loc+filhdr.a_text+filhdr.a_data+
		filhdr.a_trsize+filhdr.a_drsize, filhdr.a_syms);
	lsymbno = 0;
	while (text.size > 0) {
		symno++;
		mget((char *)&cursym, sizeof(struct nlist), &text);
		if (cursym.n_un.n_strx) {
			if (cursym.n_un.n_strx<sizeof(size) ||
			    cursym.n_un.n_strx>=size)
				error(1, "bad string table index (pass 2)");
			cursym.n_un.n_name = curstr + cursym.n_un.n_strx;
		}
		if (cursym.n_type & N_STAB) {
			if (cursym.n_type == N_BINCL) {
				exclude = start_incl2(&cursym, header_num);
				header_num++;
			} else if (cursym.n_type == N_EINCL) {
				if (exclude) {
					exclude = end_incl2();
					continue;
				} else {
					exclude = end_incl2();
				}
			} else if (exclude) {
				continue;
			}
		} else {
			/*
			 * do not keep track of any dbx symbols for load2td
			 * +++ is it safe?
			 */
			if (clocseg->lo_used == NSYMPR) {
				if (++clocseg == &locseg[NSEG])
					error(1, "local symbol overflow");
				clocseg->lo_used = 0;
			}
			if (clocseg->lo_first == 0) {
				clocseg->lo_first = (struct local *)
				    calloc(NSYMPR, sizeof (struct local));
				if (clocseg->lo_first == 0)
					error(1, "out of memory (clocseg)");
			}
			lp = &clocseg->lo_first[clocseg->lo_used++];
			lp->l_index = symno;
			if (rflag && lpicflag && cursym.n_un.n_name[0]=='L') {
				char *cp;

				cp = malloc(strlen(filname) +
				       strlen(cursym.n_un.n_name) + 2);
				strcpy(cp, filname);
				strcat(cp, ".");
				strcat(cp, cursym.n_un.n_name);
				cursym.n_un.n_name = cp;
			}
			*csp = cursym;
			lp->l_symbol = csp++;
			lp->l_link = lochash[symno % LHSIZ];
			lochash[symno % LHSIZ] = lp;
		}
/* inline expansion of symreloc() */
		if (!funding) {
			/*
			 * Do not relocate symbols in fundamental file.
			 * This turns out only to matter for local
			 * static and debugging symbols, which
			 * are written out here.
			 */
			switch (cursym.n_type & (N_TYPE+N_EXT)) {
			case N_TEXT:
			case N_EXT+N_TEXT:
				cursym.n_value += ctrel;
				break;
			case N_DATA:
			case N_EXT+N_DATA:
				cursym.n_value += cdrel;
				break;
			case N_BSS:
			case N_EXT+N_BSS:
				cursym.n_value += cbrel;
				break;
			case N_EXT+N_UNDF:
				break;
			default:
				if (cursym.n_type&N_EXT)
					cursym.n_type = N_EXT+N_ABS;
			}
		}
/* end inline expansion of symreloc() */

		type = cursym.n_type;
		if (yflag && cursym.n_un.n_name)
			for (i = 0; i < yflag; i++)
				/* fast check for 2d character! */
				if (ytab[i][1] == cursym.n_un.n_name[1] &&
				    !strcmp(ytab[i], cursym.n_un.n_name)) {
					tracesym();
					break;
				}
		if ((type&N_EXT) == 0) {
			if ( (!sflag&&!xflag&& (type&N_STAB||
				cursym.n_un.n_name[0]!='L')) || 
				(rflag && (lpicflag) && 
				cursym.n_un.n_name[0] == 'L') ) {
			    /*
			     * if symbol is for debugging then we 
			     * dont need to keep track of it.
			     */
			    if (!(type&N_STAB)) {
				    lp->l_symbol->n_desc = lsymbno;
				    lsymbno++;
			    }
			    symwrite(&cursym, sout);
			}
			continue;
		}
		if (funding)
			continue;
		if ((sp = *lookup(&ldsym)) == 0)
			error(1, "internal error: symbol not found");
		if (cursym.n_type & N_STAB || cursym.n_type == N_EXT+N_UNDF)
			continue;
		if (cursym.n_type!=sp->n_type || cursym.n_value!=sp->n_value) {
			if (!ISGT(cursym.n_un.n_name)) {
				error(0, "%s: multiply defined", 
				     cursym.n_un.n_name);
			}
		}
	}
	if (funding)
		return;
	dseek(&text, loc, filhdr.a_text);
	dseek(&reloc, loc+filhdr.a_text+filhdr.a_data, filhdr.a_trsize);
	load2td(ctrel, torigin - textbase, tout, trout);
	dseek(&text, loc+filhdr.a_text, filhdr.a_data);
	dseek(&reloc, loc+filhdr.a_text+filhdr.a_data+filhdr.a_trsize,
	    filhdr.a_drsize);
	load2td(cdrel, ndorigin - database, dout, drout);
	while (filhdr.a_data & (seground()-1)) {
		bputc(0, dout);
		filhdr.a_data++;
	}
	localsymbolno += lsymbno;
	torigin += filhdr.a_text;
	ndorigin += round(filhdr.a_data, seground());
	borigin += round(filhdr.a_bss, seground());
	free(curstr);
}

struct tynames {
	int	ty_value;
	char	*ty_name;
} tynames[] = {
	N_UNDF,	"undefined",
	N_ABS,	"absolute",
	N_TEXT,	"text",
	N_DATA,	"data",
	N_BSS,	"bss",
	N_COMM,	"common",
	0,	0,
};

tracesym()
{
	register struct tynames *tp;

	if (cursym.n_type & N_STAB)
		return;
	printf("%s", filname);
	if (archdr.ar_name[0])
		printf("(%s)", archdr.ar_name);
	printf(": ");
	if ((cursym.n_type&N_TYPE) == N_UNDF && cursym.n_value) {
		printf("definition of common %s size %d\n",
		    cursym.n_un.n_name, cursym.n_value);
		return;
	}
	for (tp = tynames; tp->ty_name; tp++)
		if (tp->ty_value == (cursym.n_type&N_TYPE))
			break;
	printf((cursym.n_type&N_TYPE) ? "definition of" : "reference to");
	if (cursym.n_type&N_EXT)
		printf(" external");
	if (tp->ty_name)
		printf(" %s", tp->ty_name);
	printf(" %s\n", cursym.n_un.n_name);
}

extern struct ssymbol *ssymbol_p;

static int
forcesymbolic(flag, sp)
	int		flag;
	struct nlist	*sp;
{
	struct ssymbol *ssp = ssymbol_p;

	if (!(flag & SYMBOLIC))
		return(0);
	if (!ssp)
		return(1);
	if (!sp->n_un.n_name)
		return(0);
	while (ssp)
		if (!strcmp(sp->n_un.n_name, ssp->ssp))
			return(1);
		else
			ssp = ssp->ss_next;
	return(0);
}

/* 
 * these bits denote a relocation type for bldreloc routine 
 */
#define REL_RP 1		/* relative */
#define EXT_RP 2		/* extern */
#define BSR_RP 4		/* base relative */
#define JMP_RP 8		/* jump */
#define PCREL_RP 0x10		/* pc relative */
#define DONE_RP 0x20		/* done */

#define dloff (lkd.v2->ld_got)
#define jloff (lkd.v2->ld_plt)

static
ext_pic_got(rp, sp, sp1, ps, where, b1)
	register	struct relocation_info *rp;
	register	struct nlist *sp;
	register	struct nlist *sp1;
	struct		slsymb *ps;
	char		*where;
	struct		biobuf *b1;
{
	int rf = 0;
	static char *errmsg = "can't reduce symbolic to relative:"; 

#if	TARGET==SUN4
	ps = sllookup(&dpic, &dpicseg[NSEG], dpichash, sp, rp->r_addend, 0, 0);
#endif
#if	TARGET==SUN3 || TARGET==SUN2
	ps = sllookup(&dpic, &dpicseg[NSEG], dpichash, sp, 0, 0, 0);
#endif
	if (ps->sl_new)
		error(1, "data linkage botch");
	else {
		if (ps->sl_lo == -1) {
			ps->sl_lo = rtp->dto;
			relocate(rp, where, rtp->dto, b1);
			if (forcesymbolic(forceflag, sp1) || entryflag)  {
				/*
				 * case where ld is asked to do
				 * symbolic to relative.
				 */
				if ((sp1->n_type & (N_TYPE+N_EXT)) !=
				    N_EXT+N_UNDF) {
					/*
					 * this ps->offset is bogus here since 
					 * for now we are ignoring offset+++
					 */
					if (sp1->n_type == N_TEXT+N_EXT)
						*(rtp->dtp)++ = sp1->n_value;
					else
						*(rtp->dtp)++ = sp1->n_value +
						    ps->sl_offset;
					if (entryflag) {
						rtp->dto += sizeof(int);
						return;
					}
					rf |= REL_RP;
				} else {
					rtp->dtp++;
					if (forcesymbolic(forceflag, sp1))
						if (assertflag & NOSYMBOLIC) {
							error(0,
							    "%s %s\n", errmsg,
							    sp1->n_un.n_name);
							errlev |= 01;
						}
				}
			} else
				*(rtp->dtp)++ = ps->sl_offset;
			/*
			 * cooking up new relocation datum
			 */
			rf |= EXT_RP + BSR_RP;
	 		bldreloc(rtp, dloff+rtp->dto, rf, sp, rp);
			rtp->dto += sizeof(int);
		} else
			relocate(rp, where, ps->sl_lo, b1);
	}
}
 
static 
jmp_slot(rp, sp, sp1, ps, where, b1, tw, creloc)
	register	struct relocation_info *rp;
	register	struct nlist *sp;
	register	struct nlist *sp1;
	struct		slsymb *ps;
	char		*where;
	struct		biobuf *b1;
	long		tw;
	long		creloc;
{
	int rf = 0;

#if	TARGET==SUN4
	ps = sllookup(&tpic, &tpicseg[NSEG], tpichash, sp, 
				0, 0, 0);
#endif
#if	TARGET==SUN2 || TARGET==SUN3
	ps = sllookup(&tpic, &tpicseg[NSEG], tpichash, sp, 0, 0, 0);
#endif
	if (ps->sl_new)
		error(1, "jump linkage botch");
	else {
		if (ps->sl_lo == -1) {
			tw += (jloff + rtp->jto) - creloc;
			relocate (rp, where, tw, b1);

			if (((sp1->n_type & (N_TYPE+N_EXT)) == N_EXT+N_UNDF) ||
			     !(forcesymbolic(forceflag, sp) || entryflag)) {
				if (forcesymbolic(forceflag, sp)) {
					if (assertflag & NOSYMBOLIC) {
						error(0, "can't reduce symbolic to relative: %s", sp1->n_un.n_name);
						errlev |= 01;
					}
				} else {
#if	TARGET==SUN4
#define MASK(n) ((1<<(n))-1)
#define jmpoff ( (unsigned long)(-4 - rtp->jto) )
					rtp->jtp->jb_inst[0] = SAVE;
					rtp->jtp->jb_inst[1] =
					    CALL | ((jmpoff>>2) & MASK(30));
					rtp->jtp->jb_inst[2] = SETHIG0 |
					    (rtp->rpp - rtp->rp);
#endif
#if	TARGET==SUN2
#define jmpoff ( (unsigned long)(-4 - rtp->jto) )
					rtp->jtp->code = NOP;
					rtp->jtp->cl_hi = JBSR;
					rtp->jtp->cl_low = jmpoff;
#endif
#if	TARGET==SUN3
#define jmpoff ( (unsigned long)(-2 - rtp->jto) )
					rtp->jtp->code = JBSR;
					rtp->jtp->cl_hi = jmpoff >> 16;
					rtp->jtp->cl_low = jmpoff & 0xffff;
#endif
				}
			} else {
				/*
				 * case where ld is asked to do
				 * symbolic to relative.
				 */
#if	TARGET==SUN4
				setupjs(rtp->jtp, sp1->n_value);
#endif
#if	TARGET==SUN2 || TARGET==SUN3
				rtp->jtp->code = JUMP;
				rtp->jtp->cl_hi = sp1->n_value >> 16;
				rtp->jtp->cl_low = sp1->n_value & 0xffff;
#endif
				if (entryflag) {
					ps->sl_lo = rtp->jto;
					rtp->jto += sizeof(struct jbind);
					rtp->jtp++; 
					return;
				}
				rf |= REL_RP;
			}

			/*
			 * cook up new relocation info
			 */
#if	TARGET==SUN2 || TARGET==SUN3
			rtp->jtp->reloc_index = rtp->rpp - rtp->rp;
#endif
			ps->sl_lo = rtp->jto;
			rtp->jtp++;
			rf |= EXT_RP + JMP_RP;
#if	TARGET==SUN4
			bldreloc(rtp, jloff + rtp->jto, rf, sp, rp);
#endif
#if	TARGET==SUN2 || TARGET==SUN3
			bldreloc(rtp, jloff + rtp->jto + 2, rf, sp, rp);
#endif
			rtp->jto += sizeof(struct jbind);
		} else {
			tw += (jloff + ps->sl_lo) - creloc;
			relocate(rp, where, tw, b1);
		}
	}
}

/*
 * This routine relocates the single text or data segment argument.
 * Offsets from external symbols are resolved by adding the value
 * of the external symbols.  Non-external reference are updated to account
 * for the relative motion of the segments (ctrel, cdrel, ...).  If
 * a relocation was pc-relative, then we update it to reflect the
 * change in the positioning of the segments by adding the displacement
 * of the referenced segment and subtracting the displacement of the
 * current segment (creloc).
 *
 * If we are saving the relocation information, then we increase
 * each relocation datum address by our base position in the new segment.
 */
load2td(creloc, position, b1, b2)
	long creloc, position;
	struct biobuf *b1, *b2;
{
	register struct nlist *sp;
	register struct nlist *sp1;
	register int rf;
	long tw;
	register struct relocation_info *rp, *rpend;
	struct relocation_info *relp;
	char *codep;
	register char *cp;
	int relsz, codesz;
	struct slsymb *ps;
	int piccode;

	relsz = reloc.size;
	relp = (struct relocation_info *)mymalloc(relsz);
	codesz = text.size;
	codep = (char *)mymalloc(codesz);
	if (relp == 0 || codep == 0)
		error(1, "out of memory (load2td)");
	mget((char *)relp, relsz, &reloc);
	rpend = &relp[relsz / sizeof (struct relocation_info)];
	mget(codep, codesz, &text);
	for (rp = relp, piccode = 0; rp < rpend; rp++) {
		if (rp->r_extern == 0)
			continue;
		sp = getlocsymb(rp);
#if	TARGET==SUN4
#               define  IN_RANGE(v,n)   ((-(1<<((n)-1))) <=(v) && (v) < (1<<((n)-1)))
		/*
		 * the peephole optimizer on sun4 figured out that
		 * sometimes the __GLOBAL_OFFSET_TABLE_ relocation is not
		 * needed (i.e only jmp_table reloc are in the routine)
		 * so we have to test here for both conditions.
		 */
		if (ISGT(sp->n_un.n_name) || rp->r_type == RELOC_JMP_TBL) {
#endif
#if	TARGET==SUN3 || TARGET==SUN2
		if (ISGT(sp->n_un.n_name) || rp->r_jmptable) {
#endif
			piccode = 1;
			break;
		}

	}

	for (rp = relp; rp < rpend; rp++) {
	    rf = 0;
	    cp = codep + rp->r_address;

	    /*
	     * Search the hash table which maps local
	     * symbol numbers to symbol tables entries
	     * in the new a.out file.
	     */
	    if (piccode || rp->r_extern)
		sp = getlocsymb(rp);

	    /*
	     * Pick up previous value at location to be relocated.
	     */
#if	TARGET== SUN4
		/*
		 * Pick up addend.
		 */
		tw = rp->r_addend;
#endif
#if	TARGET==SUN3 || TARGET==SUN2
	    switch (rp->r_length) {
	    case 0:		/* byte */
		    tw = *cp;
		    break;
	    case 1:		/* word */
		    tw = *(short *)cp;
		    break;
	    case 2:		/* long */
		    /* "cp" points to an least a 16-bit boundary, but
		     * not necessarily a 32-bit boundary.
		     */
#ifdef mc68000	    /* 68k host can do long accesses on 16-bit boundaries */
		    tw = *(long *)cp;
#else /*!mc68000*/  /* others can only do long accesses on 32-bit bdy's */
		    *((short*)      (&tw)       ) = *((short*)  cp  );
		    *((short*)(((char*)(&tw))+2)) = *((short*)(cp+2));
#endif /*mc68000*/
		    break;
	    default:
		    error(1, "load2td botch: bad length");
	    }
#endif

	    if (rp->r_extern) {
		    cursym.n_un.n_name = sp->n_un.n_name;
		    if ( (sp1 = *lookup(&ldsym)) == 0 )
			error(1, "can't find symbol");
	    }

		/*
		 * The 0'th entry of the GOT (which may not be its base
		 * address) is reserved to hold the location of __DYNAMIC.
		 */
		if (rtp->dto == 0) {
			rtp->dto += sizeof (int);
			rtp->dtp++;
		}

#if	TARGET==SUN4
	    if (rp->r_type == RELOC_BASE10 || rp->r_type == RELOC_BASE13 ||
			rp->r_type == RELOC_BASE22) {
#endif
#if	TARGET==SUN3 || TARGET==SUN2
	    if (rp->r_baserel) {
#endif
	        if (rflag) {
			if (rp->r_extern)
				rp->r_symbolnum = nsym+symx(&ldsym, sp1);
			else
				rp->r_symbolnum = sp->n_desc + localsymbolno;
			goto dorflag;
		}
		
		if (rp->r_extern) {
			ext_pic_got(rp, sp, sp1, ps, cp, b1);
		} else {
#if	TARGET==SUN4
		    ps = sllookup(&stpic, &stpicseg[NSEG], stpichash, sp,
					rp->r_addend, 0, (rtp->dtp - rtp->dt));
#endif
#if	TARGET==SUN3 || TARGET==SUN2
		    ps = sllookup(&stpic, &stpicseg[NSEG], stpichash, sp,
					0, 0, (rtp->dtp - rtp->dt));
#endif
		    if (ps->sl_new) {
			ps->sl_lo = rtp->dto;
			relocate(rp, cp, rtp->dto, b1);
			switch (sp->n_type) {
			case N_TEXT:
			    /* tw += ctrel; */
			    tw += ctrel + sp->n_value;
			    break;
			case N_DATA:
			    /* tw += cdrel; */
			    tw += cdrel + sp->n_value;
			    break;
			case N_BSS:
			    /* tw += cbrel; */
			    tw += cbrel + sp->n_value;
			    break;
			default:
			    error(1, "base relative static symbol(%s) botch",
				sp->n_un.n_name);
			}
			if (!entryflag || forcesymbolic(forceflag, sp)) {
 			    rf |= REL_RP+BSR_RP;
 			    bldreloc(rtp, dloff+rtp->dto, rf, 0, rp);
			}
			rtp->dto += sizeof(int);
			*(rtp->dtp)++ = tw;
		    } else
			relocate(rp, cp, ps->sl_lo, b1);
		}
		continue;
	    }

#if	TARGET==SUN4
	    if (rp->r_type == RELOC_JMP_TBL) {
#endif
#if	TARGET==SUN2 || TARGET==SUN3
	    if (rp->r_jmptable) {
#endif
	        if (rflag) {
			rp->r_symbolnum = nsym+symx(&ldsym, sp1);
			goto dorflag;
		}
		/*
		 * pc relative call to a symbol in the data segment
		 */
		if (rp->r_extern == 0)  {
			u_int	dsoff;		/* offset into data segment */

			dsoff = tw - (filhdr.a_text - rp->r_address);
			tw = cdrel + filhdr.a_text - (ctrel + rp->r_address) +
			    dsoff;
			relocate(rp, cp, tw, b1);
			continue;
		}
		jmp_slot(rp, sp, sp1, ps, cp, b1, tw, creloc);
		continue;
	    }

#if	TARGET==SUN4
	    if (rp->r_type == RELOC_PC10 || rp->r_type == RELOC_PC22) {
		if (!ISGT(sp1->n_un.n_name))
		    error(1, "load2td: expect __GLOBAL_OFFSET_TABLE_");
	        if (rflag) {
			rp->r_symbolnum = nsym+symx(&ldsym, sp1);
			goto dorflag;
		}
		/*
		 * pc relative reference that used 4 instructions since
		 * sparc doesn't have one pc relative instruction to
		 * access a symbol.
		 */
		tw = sp1->n_value - creloc - rp->r_address + rp->r_addend; 
		relocate(rp, cp, tw, b1);
		continue;
	    }
#endif

	    /*
	     * If relative to an external which is defined,
	     * resolve to a simpler kind of reference in the
	     * result file.  If the external is undefined, just
	     * convert the symbol number to the number of the
	     * symbol in the result file and leave it undefined.
	     */
#define r_addr rp->r_address + position + (b1 == dout ? database : textbase)
#if	TARGET==SUN4
#ifdef _SUN4_DEVELOPMENT
#define isitpcrel(rp) (rp->r_type == RELOC_DISP8 || rp->r_type == RELOC_DISP16\
	    || rp->r_type == RELOC_DISP32 || rp->r_type == RELOC_WDISP30 \
	    || rp->r_type == oRELOC_WDISP23 || rp->r_type == RELOC_WDISP22)
#else
#define isitpcrel(rp) (rp->r_type == RELOC_DISP8 || rp->r_type == RELOC_DISP16\
	    || rp->r_type == RELOC_DISP32 || rp->r_type == RELOC_WDISP30 \
	    || rp->r_type == RELOC_WDISP22)
#endif _SUN4_DEVELOPMENT
#endif

	    if (rp->r_extern) {
		if (sp1->n_type == N_EXT+N_UNDF) {
		    rp->r_symbolnum = nsym+symx(&ldsym, sp1);
		    if ((ISDYNAMIC) && !rflag) {
			/*
			 * if we are forced to allocate both common and
			 * procedure then all that are left here should
			 * be procedure (all the common should be turn 
			 * into BSS type by now).
			 *
			 * if we are forced to declare only common then
			 * what is left here are normal relocation to 
			 * undefined routines.
			 *
			 * if we are forced to allocate only procedure
			 * then what we are concerned here are the symbols
			 * with a value of 0.
			 */ 
			if (pflag) {
			    if ((ps = slfindit(tpichash, sp1, 0, hashit(sp1)))) {
				if (ps->sl_lo == -1) {
#if	TARGET==SUN4
				    if (isitpcrel(rp))
#endif
#if	TARGET==SUN3 || TARGET==SUN2
				    if (rp->r_pcrel)
#endif
					tw += (jloff+rtp->jto) - creloc;
				    else
					tw += (jloff+rtp->jto);
				    relocate(rp, cp, tw, b1);
				    if (forceflag & SYMBOLIC)
					error(1, "symbolic flag botch");
#if	TARGET==SUN4
				    rtp->jtp->jb_inst[0] = SAVE;
				    rtp->jtp->jb_inst[1] = CALL | ((jmpoff>>2) & MASK(30));
				    rtp->jtp->jb_inst[2] = SETHIG0 | 
					(rtp->rpp - rtp->rp);
					
#endif
#if	TARGET==SUN2
				    rtp->jtp->code = NOP;
				    rtp->jtp->cl_hi = JBSR;
				    rtp->jtp->cl_low = jmpoff;
#endif
#if	TARGET==SUN3
				    rtp->jtp->code = JBSR;
				    rtp->jtp->cl_hi = jmpoff >> 16;
				    rtp->jtp->cl_low = jmpoff & 0xffff;
#endif
#if	TARGET==SUN2 || TARGET==SUN3
				    rtp->jtp->reloc_index = rtp->rpp - rtp->rp;
#endif
				    ps->sl_lo = rtp->jto;
				    rtp->jtp++;
				    rf |= EXT_RP + JMP_RP;
#if	TARGET==SUN4
	 			    bldreloc(rtp, jloff + rtp->jto, rf, sp, rp);
#endif
#if	TARGET==SUN3 || TARGET==SUN2
				    bldreloc(rtp, jloff + rtp->jto + 2, rf, sp, rp);
#endif
				    rtp->jto += sizeof(struct jbind);
				} else {
#if	TARGET==SUN4
				    if (isitpcrel(rp))
#endif
#if	TARGET==SUN3 || TARGET==SUN2
				    if (rp->r_pcrel)
#endif
					tw += (jloff+ps->sl_lo) - creloc;
				    else
					tw += (jloff+ps->sl_lo);
				    relocate(rp, cp, tw, b1);
				}
				continue;
			    }
			}
#if	TARGET==SUN4
			if (isitpcrel(rp))
#endif
#if	TARGET==SUN3 || TARGET==SUN2
			if (rp->r_pcrel)
#endif
			    if (b1 != tout)
				error(1, "no pc rel from data");
			    else {
				rf |= PCREL_RP;
				tw -= creloc;
#if	TARGET==SUN4
				rp->r_addend = tw;
#endif
#if	TARGET==SUN3 || TARGET==SUN2
				relocate(rp, cp, tw, b1);
#endif
			    }
			rf |= EXT_RP;
			bldreloc(rtp, r_addr, rf, sp, rp);
			continue;
		    }
		} else {
		    /* what a kludge here */
#if	(TARGET==SUN3) || (TARGET==SUN2)
		    if (ISGT(sp1->n_un.n_name)) {
			if (rflag)
				goto dorflag;
			rp->r_symbolnum = sp1->n_type & N_TYPE;
			tw += sp1->n_value;
			rp->r_extern = 0;
			tw -= creloc;	/* ++++ assumes that this is pcrel */
			relocate(rp, cp, tw, b1, rp);
			continue;
		    }
#endif
		    /*
		     * This is the case of a non pic module referencing 
		     * a defined symbol.
		     */
		    if (!rflag && !entryflag && (bindingflag != ST_BIND) 
			&& !(forcesymbolic(forceflag, sp1)))  {
#if	TARGET==SUN4
			if (isitpcrel(rp))
#endif
#if	TARGET==SUN3 || TARGET==SUN2
			if (rp->r_pcrel)
#endif
			    if (b1 != tout)
				error(1, "no pc rel from data");
			    else {
				rf |= PCREL_RP;
				tw -= creloc;
#if	TARGET==SUN4
				rp->r_addend = tw;
#endif
#if	TARGET==SUN3 || TARGET==SUN2
				relocate(rp, cp, tw, b1);
#endif
			    }
			rf |= EXT_RP;
			bldreloc(rtp, r_addr, rf, sp, rp);
			continue;
		    }
		    rp->r_symbolnum = sp1->n_type & N_TYPE;
		    tw += sp1->n_value;
		    rp->r_extern = 0;
		    if ((ISDYNAMIC) && !rflag) {
			if (forcesymbolic(forceflag, sp1)) {
#if	TARGET==SUN4
			    if (isitpcrel(rp))
#endif
#if	TARGET==SUN2 || TARGET==SUN3
			    if (rp->r_pcrel)
#endif
				if (b1 != tout)
				    error(1, "no pc rel from data");
				else {
				    tw -= creloc;
				    rf |= DONE_RP;
				}
			    relocate(rp, cp, tw, b1);
			    rf |= REL_RP;
			    bldreloc(rtp, r_addr, rf, 0, rp);
			    continue;
			}
		    }
		}
	    } else switch (rp->r_symbolnum & N_TYPE) {
	    /*
	     * Relocation is relative to the loaded position
	     * of another segment.  Update by the change in position
	     * of that segment.
	     */
	    case N_TEXT:
		    tw += ctrel;
		    if (rflag)
			break;
		    if (!entryflag && ((bindingflag & DN_BIND) ||
						!(bindingflag & ST_BIND))) {
			relocate(rp, cp, tw, b1);
			rf |= REL_RP;
			bldreloc(rtp, r_addr, rf, 0, rp);
			continue;
		    }
		    break;
	    case N_DATA:
		    tw += cdrel;
		    if (rflag)
			break;
		    if (!entryflag && ((bindingflag & DN_BIND) ||
						!(bindingflag & ST_BIND))) {
			relocate(rp, cp, tw, b1);
			rf |= REL_RP;
			bldreloc(rtp, r_addr, rf, 0, rp);
			continue;
		    }
		    break;
	    case N_BSS:
		    tw += cbrel;
		    if (rflag)
			break;
		    if (!entryflag && ((bindingflag & DN_BIND) ||
						!(bindingflag & ST_BIND))) {
			relocate(rp, cp, tw, b1);
			rf |= REL_RP;
			bldreloc(rtp, r_addr, rf, 0, rp);
			continue;
		    }
		    break;
	    case N_ABS:
		    break;
	    default:
		    error(1,"relocation format botch (symbol type))");
	    }

	    /*
	     * Relocation is pc relative, so decrease the relocation
	     * by the amount the current segment is displaced.
	     * (E.g if we are a relative reference to a text location
	     * from data space, we added the increase in the text address
	     * above, and subtract the increase in our (data) address
	     * here, leaving the net change the relative change in the
	     * positioning of our text and data segments.)
	     */
dorflag:
#if	TARGET==SUN4
	    switch (rp->r_type) {
	    case RELOC_DISP8:
	    case RELOC_DISP16:
	    case RELOC_DISP32:
	    case RELOC_WDISP30:
#ifdef _SUN4_DEVELOPMENT
	    case oRELOC_WDISP23:
#endif _SUN4_DEVELOPMENT
	    case RELOC_WDISP22:
	    case RELOC_JMP_TBL:
		    tw -= creloc;
	    }

	    /*
	     * If we're saving the relocation record, just stuff the
	     * value back into it. Otherwise,
	     * Put the value back in the segment,
	     * while checking for overflow.
	     */
	    if (rflag){
		    rp->r_addend = tw;
		    rp->r_address += position;
	    } else 
		    relocate(rp, cp, tw, b1);
#endif
#if	TARGET==SUN2 || TARGET==SUN3
	    if (rp->r_pcrel)
		    tw -= creloc;

	    relocate(rp, cp, tw, b1);

	    /*
	     * If we are saving relocation information,
	     * we must convert the address in the segment from
	     * the old .o file into an address in the segment in
	     * the new a.out, by adding the position of our
	     * segment in the new larger segment.
	     */
	    if (rflag)
		    rp->r_address += position;
#endif
	}
	bwrite(codep, codesz, b1);
	if (rflag)
		bwrite(relp, relsz, b2);
	free((char *)relp);
	free(codep);
}

#if	TARGET==SUN4
setupjs(jtp, val)
	register struct jbind *jtp;
	int val;
{
	jtp->jb_inst[0] = SETHI | ((val >> (32-22)) & MASK(22));
	jtp->jb_inst[1] = JMPI | (val & MASK(10));
	jtp->jb_inst[2] = NOP;
}
#endif

/* 
 *  This routine build a relocation record for runtime linking
 */
bldreloc(rt, addr, flag, sp, rp)
	register struct runtime *rt;
	register int addr;
	register int flag;
	register struct nlist *sp;
	register struct relocation_info *rp;
{

	/*
	 * If we are building a fully statically linked program, and we
	 * are doing a pic relocation against undefined symbols, it is
	 * possible for us to get asked to build a relocation entry
	 * for the undefined reference.  Of course, because we aren't
	 * doing any dynamic linking, we won't have a table to build
	 * it against.  Check for this here, and just pretend success
	 * in such a circumstance.
	 */
	if (rt->rpp == NULL) {
		if (!rflag && ((ISDYNAMIC) || forceflag & SYMBOLIC))
			error(1, "bldreloc: relocation table missing");
		return;
	}

	/*
	 * Verify arguments and proceed to build a new relocation entry.
	 */
	if (flag == 0)
		error(1, "Illegal flag");
	rt->rpp->r_address = addr;
	if ((assertflag & PURE_TEXT) && !(rflag || Nflag))
		if (addr < database) 
			error(0, "assert pure-text failed: reference to %s at %x in %s\n",
			    (sp == 0 ? "[offset]" : sp->n_un.n_name), 
			    addr, filname);
#if	TARGET==SUN4
	rt->rpp->r_addend = rp->r_addend;
	if (flag & REL_RP) {
		if (flag & JMP_RP)
			rt->rpp->r_type = RELOC_RELATIVE;
		else if (flag & BSR_RP)
			rt->rpp->r_type = RELOC_32;
		else
			rt->rpp->r_type = rp->r_type;
	} else if (flag & BSR_RP)
		rt->rpp->r_type = RELOC_GLOB_DAT;
	else if (flag & JMP_RP)
		rt->rpp->r_type = RELOC_JMP_SLOT;
	else
		rt->rpp->r_type = rp->r_type;
#endif
#if	TARGET==SUN2 || TARGET==SUN3
	if (flag & REL_RP) 
		rt->rpp->r_relative = 1;
	if (flag & JMP_RP)
		rt->rpp->r_jmptable = 1;
	if (flag & BSR_RP)
		rt->rpp->r_baserel = 1;
	if (flag & PCREL_RP)
		rt->rpp->r_pcrel = 1;
#endif
	if (flag & EXT_RP) {
		if ((rt->rpp->r_symbolnum = fslookup(sp, rt)) == -1) 
			error(1, "fast symbol botch");
		if (flag & REL_RP)
			rt->rpp->r_extern = 0;
		else
			rt->rpp->r_extern = 1;
	}
	rt->rpp++;
	relocused++;
}

relocate (rp, where, what, b1)
	struct relocation_info *rp;
	char *where;
	long what;
	struct biobuf *b1;
{
#if	TARGET==SUN4
	switch (rp->r_type) {
	case RELOC_8:
	case RELOC_DISP8:
		if (!IN_RANGE(what,8))
			error(0, "byte displacement overflow at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		*where = what;
		break;
#ifdef _SUN4_DEVELOPMENT
	case oRELOC_LO9:
		*(long *)where = (*(long *)where & ~MASK(9)) | (what & MASK(9));
		break;
#endif _SUN4_DEVELOPMENT
	case RELOC_LO10:
	case RELOC_PC10:
	case RELOC_BASE10:
		*(long *)where = (*(long *)where & ~MASK(10)) | (what & MASK(10));
		break;
	case RELOC_BASE13:
	case RELOC_13:
		*(long *)where = (*(long *)where & ~MASK(13)) | (what & MASK(13));
		break;

	case RELOC_16:
	case RELOC_DISP16:
		if (!IN_RANGE(what,16))
			error(0, "word displacement overflow at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		*(short *)where = what;
		break;
#ifdef _SUN4_DEVELOPMENT
	case oRELOC_23:
		if (!IN_RANGE(what,23))
			error(0, "sethi displacement overflow at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		*(long *)where = (*(long *)where & ~MASK(23)) | (what & MASK(23));
		break;
#endif _SUN4_DEVELOPMENT
	case RELOC_22:
		if (!IN_RANGE(what,22))
			error(0, "sethi displacement overflow at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		*(long *)where = (*(long *)where & ~MASK(22)) | (what & MASK(22));
		break;
#ifdef _SUN4_DEVELOPMENT
	case oRELOC_HI23:
		*(long *)where = (*(long *)where & ~MASK(23)) 
			| ((what>>(32-23)) & MASK(23));
		break;
#endif _SUN4_DEVELOPMENT
	case RELOC_HI22:
	case RELOC_BASE22:
	case RELOC_PC22:
		*(long *)where = (*(long *)where & ~MASK(22)) 
			| ((what>>(32-22)) & MASK(22));
		break;
#ifdef _SUN4_DEVELOPMENT
	case oRELOC_WDISP23:
		if (what & MASK(2) )
			error(0, "odd word displacement at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		what >>= 2;
		if (!IN_RANGE(what,23))
			error(0, "branch displacement overflow at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		*(long *)where = (*(long *)where & ~MASK(23)) | (what & MASK(23));
		break;
#endif _SUN4_DEVELOPMENT
	case RELOC_WDISP22:
		if (what & MASK(2) )
			error(0, "odd word displacement at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		what >>= 2;
		if (!IN_RANGE(what,22))
			error(0, "branch displacement overflow at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		*(long *)where = (*(long *)where & ~MASK(22)) | (what & MASK(22));
		break;
		
	case RELOC_JMP_TBL:
	case RELOC_WDISP30:
		if (what & MASK(2) )
			error(0, "odd word displacement at %s+%#x",
				b1==tout?"text":"data", rp->r_address);
		what >>= 2;
		*(long *)where = (*(long *)where & ~MASK(30)) | (what&MASK(30));
		break;
	case RELOC_32:
	case RELOC_DISP32:
		*(long *)where = what;
		break;

	}
#endif
#if	TARGET==SUN2 || TARGET==SUN3
	/*
	 * Put the value back in the segment,
	 * while checking for overflow.
	 */
	switch (rp->r_length) {

	case 0:		/* byte */
		if (what < -128 || what > 127)
			error(0, "byte displacement overflow at %s+%#x",
			    b1==tout?"text":"data", rp->r_address);
		*where = what;
		break;
	case 1:		/* word */
		if (what < -32768 || what > 32767)
			error(0, "word displacement overflow at %s+%#x",
			    b1==tout?"text":"data", rp->r_address);
		*(short *)where = what;
		break;
	case 2:		/* long */
		/* "where" points to an least a 16-bit boundary, but
		 * not necessarily a 32-bit boundary.
		 */
#ifdef mc68000		/* 68k host can do long accesses on 16-bit boundaries */
		*(long *)where = what;
#else /*!mc68000*/	/* others can only do long accesses on 32-bit bdy's */
		*((short*)  where  ) = *((short*)      (&what)       );
		*((short*)(where+2)) = *((short*)(((char*)(&what))+2));
#endif /*mc68000*/
		break;
	}
#endif
}

finishout()
{
	register int i;
	register struct nlist *sp;
	int nsymt;

	/*
	 * if dynamic linking then flush out the data, jump linkage table plus
	 * the relocation datum followed by the hash table for the fast symbols
	 * and the symbols themselves. 
	 */
	if (!rflag) {
		int *j = rtp->dt;
		struct jbind *k = rtp->jt;

		if (ISDYNAMIC) {
			if ((i = lalign(rtp->fsoff)) != rtp->fsalloc) {
				error(1, "allocated %d used %d for fast symb",
				    rtp->fsalloc, i);
			}
			if (relocused != rtp->rl) {
				error(1, "no of reloc used %d != no alloc %d",
					relocused, rtp->rl);
			}
			{
				struct relocation_info *l = rtp->rp;
				struct fshash *m = rtp->hp;
				struct nlist *n = rtp->sp;
				char *x = (char *) calloc(8,1);
	
				for (i = 0; i < sl.ds+sl.ss; i++)
					bwrite((char *)j++, sizeof(int),
					    dynout);
				for (i = 0; i < sl.js; i++)
					bwrite((char *)k++,
					    sizeof(struct jbind), dynout);
				if (pad != 0)
					bwrite((char *)x, pad, dynout);
				for (i = 0; i < rtp->rl; i++)
					bwrite((char *)l++,
					    sizeof(struct relocation_info),
					    tout);
				for (i = 0; i < rtp->hp_ind; i++)
					bwrite((char *)m++, 
					    sizeof(struct fshash), tout);
				for (i = 0; i < totalsymb(); i++)
					bwrite((char *)n++,
					    sizeof(struct nlist), tout);
				bwrite((char *)rtp->fsstr, rtp->fsalloc, tout);
				if (rtp->searchpath)
					bwrite(rtp->searchpath, 
					    lalign(rtp->spthlen), tout);
				bwrite((char *) rtp->lko, dynamic.lib *
				    sizeof(struct link_object), tout);
				bwrite(shlibstr, dynamic.libstr, tout); 
			}
		} else if (dynamic.ds + dynamic.js) {
			for (i = 0; i < sl.ds+sl.ss; i++)
				bwrite((char *)j++, sizeof(int), dynout);
			for (i = 0; i < sl.js; i++)
				bwrite((char *)k++, sizeof(struct jbind),
				    dynout);
		}
	}
	if (sflag==0) {
		nsymt = symx(&ldsym, ldsym.ns);
		for (i = 0; i < nsymt; i++) {
			sp = xsym(ldsym.fs, i);
			if (sp->n_type == DISCARDIT)
			    continue;
			/*
			 * ++++ -g option for the symbolic debugger
			 * generated symbol types N_UNDF and N_ABS
			 * that can have value 0. Is this correct 
			 * to say here that if we see such symbol the
			 * we can write it out. Checking for external
			 * text symbol here to deal with flag -N -T 0.
			 */
			if (!rflag) {
				if (sp->n_value != 0 || sp->n_type == N_UNDF ||
				    sp->n_type == N_EXT+N_TEXT ||
				    sp->n_type == N_ABS ||
				    sp->n_type == N_ABS+N_EXT)
					symwrite(sp, sout);
			} else
				symwrite(sp, sout);
		}
		bwrite(&offset, sizeof offset, sout);
		/*
		 * if appropriate, write out extra sections 
		 * following the string table
		 */
		write_extra_sections( strout );
	}

	filname = aoutname;
	archdr.ar_name[0] = '\0';
	if (rename(ofilename, aoutname) < 0) {
		filname = NULL;	/* kludge */
		error(1, "cannot move temp file %s to %s: %s", ofilename,
		    aoutname, errmsg(errno));
	}
	delarg = errlev;
	delexit();
}

mkfsym(s, value, type)
	char *s;
	int value;
	int type;
{
	static struct nlist fsym;

	if (sflag || xflag)
		return;
	fsym.n_un.n_name = s;
	fsym.n_type = type;
	fsym.n_value = value;
	symwrite(&fsym, sout);
}

getarhdr()
{
	register char *cp;

	mget((char *)&archdr, sizeof archdr, &text);
	for (cp=archdr.ar_name; cp<&archdr.ar_name[sizeof(archdr.ar_name)];)
		if (*cp++ == ' ') {
			cp[-1] = 0;
			return;
		}
}

mget(loc, n, sp)
	register STREAM *sp;
	register char *loc;
{
	register char *p;
	register int take;
	register int nread;

top:
	if (n == 0)
		return;
	if (sp->size && sp->nibuf) {
		p = sp->ptr;
		take = sp->size;
		if (take > sp->nibuf)
			take = sp->nibuf;
		if (take > n)
			take = n;
		n -= take;
		sp->size -= take;
		sp->nibuf -= take;
		sp->pos += take;
		do
			*loc++ = *p++;
		while (--take > 0);
		sp->ptr = p;
		goto top;
	}
	if (n > p_blksize) {
		take = n - n % p_blksize;
		lseek(infil, (sp->bno+1)<<p_blkshift, 0);
		if (take > sp->size)
			error(1, "premature EOF");
		if (nread = read(infil, loc, take) != take) {
			if (nread < 0)
				error(1, errmsg(errno));
			else
				error(1, "premature EOF");
		}
		loc += take;
		n -= take;
		sp->size -= take;
		sp->pos += take;
		dseek(sp, (sp->bno+1+(take>>p_blkshift))<<p_blkshift, -1);
		goto top;
	}
	*loc++ = get(sp);
	--n;
	goto top;
}

symwrite(sp, bp)
	struct nlist *sp;
	struct biobuf *bp;
{
	register int len;
	register char *str;

	str = sp->n_un.n_name;
	if (str) {
		sp->n_un.n_strx = offset;
		len = strlen(str) + 1;
		bwrite(str, len, strout);
		offset += len;
	}
	bwrite(sp, sizeof (*sp), bp);
	sp->n_un.n_name = str;
	nsymwrite++;
}

dseek(sp, loc, s)
	register STREAM *sp;
	long loc, s;
{
	register o;

	o = loc&p_blkmask;
	if (o&01)
		error(1, "loader error; odd offset");
	dseek1(sp, loc, s);
}

dseek1(sp, loc, s)
	register STREAM *sp;
	long loc, s;
{
	register PAGE *p;
	register b, o;
	int n;

	b = loc>>p_blkshift;
	o = loc&p_blkmask;
	--sp->pno->nuser;
	if ((p = &page[0])->bno!=b && (p = &page[1])->bno!=b)
		if (p->nuser==0 || (p = &page[0])->nuser==0) {
			if (page[0].nuser==0 && page[1].nuser==0)
				if (page[0].bno < page[1].bno)
					p = &page[0];
			p->bno = b;
			lseek(infil, loc & ~(long)p_blkmask, 0);
			if ((n = read(infil, p->buff, p_blksize)) < 0)
				error(1, errmsg(errno));
			p->nibuf = n;
		} else
			error(1, "botch: no pages");
	++p->nuser;
	sp->bno = b;
	sp->pno = p;
	if (s != -1) {sp->size = s; sp->pos = 0;}
	sp->ptr = (char *)(p->buff + o);
	if ((sp->nibuf = p->nibuf-o) <= 0)
		sp->size = 0;
}

char
get(asp)
	STREAM *asp;
{
	register STREAM *sp;

	sp = asp;
	if ((sp->nibuf -= sizeof(char)) < 0) {
		dseek(sp, ((long)(sp->bno+1)<<p_blkshift), (long)-1);
		sp->nibuf -= sizeof(char);
	}
	if ((sp->size -= sizeof(char)) <= 0) {
		if (sp->size < 0)
			error(1, "premature EOF");
		++fpage.nuser;
		--sp->pno->nuser;
		sp->pno = (PAGE *) &fpage;
	}
	sp->pos += sizeof(char);
	return(*sp->ptr++);
}

getfile(acp, mj, mn, sizep)
	char *acp;
	int *mj;
	int *mn;
	long *sizep;
{
	register char *cp;
	register int c;
	union {
	    char arcmag[SARMAG+1];
	    struct exec ex;
	} u;
	struct stat stb;
#ifdef SUNPRO
	char *path;
#endif

	cp = acp; 
	archdr.ar_name[0] = '\0';
	filname = cp;
	if (cp[0]=='-' && cp[1]=='l')
		infil = libopen(filname + 2, O_RDONLY, mj, mn);
	else {
#ifdef SUNPRO
		infil = open_vroot(filname, O_RDONLY, 0, NULL, VROOT_DEFAULT);
		if (trace) {
			char *full_path;
			get_vroot_path(&full_path, NULL, NULL);
			(void)printf("\t%s\n", full_path);
		}
#else
		infil = open(filname, O_RDONLY);
#endif
	}

	if (infil < 0)
		error(1, errmsg(errno));
	fstat(infil, &stb);
	page[0].bno = page[1].bno = -1;
	page[0].nuser = page[1].nuser = 0;
	c = stb.st_blksize;
	if (c == 0 || (c & (c - 1)) != 0) {
		/* use default size if not a power of two */
		c = BLKSIZE;
	}
	if (p_blksize != c) {
		p_blksize = c;
		p_blkmask = c - 1;
		for (p_blkshift = 0; c > 1 ; p_blkshift++)
			c >>= 1;
		if (page[0].buff != NULL)
			free(page[0].buff);
		page[0].buff = (char *)mymalloc(p_blksize);
		if (page[0].buff == NULL)
			error(1, "ran out of memory (getfile)");
		if (page[1].buff != NULL)
			free(page[1].buff);
		page[1].buff = (char *)mymalloc(p_blksize);
		if (page[1].buff == NULL)
			error(1, "ran out of memory (getfile)");
	}
	text.pno = reloc.pno = (PAGE *) &fpage;
	fpage.nuser = 2;
	dseek(&text, 0L, SARMAG);
	if (text.size <= 0)
		error(1, "premature EOF");
	mget((char *)u.arcmag, SARMAG, &text);
	u.arcmag[SARMAG] = 0;
	if (strcmp(u.arcmag, ARMAG)) {
		if (u.ex.a_dynamic) 
			return(SHLIB);
		else {
			 if (sizep != NULL)
				if (u.ex.a_magic != ZMAGIC)
					*sizep = stb.st_size;
				else
					*sizep = 0;
			return(PLAIN);
		}
	}
	dseek(&text, SARMAG, sizeof archdr);
	if (text.size <= 0)
		return (ARCH1);
	getarhdr();
	if (strncmp(archdr.ar_name, "__.SYMDEF", sizeof(archdr.ar_name)) != 0)
		return (ARCH1);
	/*
	 * +++++++ kludge. have to get rid of this stuff later
	 */
	return(stb.st_mtime > atol(archdr.ar_date) + 60 ? ARCH3 : ARCH2);
}

/*
 * Search for a library with given name
 * using the directory search array.
 */
libopen(name, oflags, mj, mn)
	char *name;
	int oflags;
	int *mj;
	int *mn;
{
	register char *p, *cp, *q;
	char *pp, *tpp;
	register int i;
	static char buf[MAXPATHLEN+1];
	char lib[MAXPATHLEN+1];
	int fd = -1;
#ifdef SUNPRO
	char *path;
#endif
	/*
	 * for shared library we are not reporting make dependency
	 * since it could be changed at runtime. ++++ will need
	 * to revisit this area at some future date
	 */

	if (*name == '\0')			/* backwards compat */
	    name = "a";

	for (i = 0; i < ndir && fd == -1; i++) {
		(void) strcpy(buf, dirs[i]);
		(void) strcpy(lib, "lib");
		/*
		 * Fix up library name if verision number was specifed
		 */
		 if ((tpp = rindex(name,'.')) != 0) {
			while (*tpp == '.' || isdigit(*tpp)) {
				if (*tpp == '.')
					pp = tpp;
				tpp--;
			}
			(void) strncat(lib,name,pp-name);
			(void) strcat(lib, ".so");
			(void) strcat(lib, pp);
		 } else
			(void) strcat(lib, name);
		if ((forceflag & DYNAMIC) && getshlib(buf, lib, mj, mn) != -1)
			fd = open(buf, oflags);
		else {
#ifdef SUNPRO
			(void)strcpy(buf, "lib");
			(void)strcat(buf, name);
			(void)strcat(buf, ".a");
			fd = open_vroot(buf, oflags, 0, sp_dirs[i],
			    VROOT_DEFAULT);
			get_vroot_path(NULL, &path, NULL);
			if (fd != -1) {
				if (trace)
					(void)printf("\t%s\n", path);
				(void)strcpy(buf, path);
				report_dependency(stripvroot(buf));
				do_report_libdep(buf, "LD");
			}
#else
			(void)strcpy(buf, dirs[i]);
			(void)strcat(buf, "/lib");
			(void)strcat(buf, name);
			(void)strcat(buf, ".a");
			fd = open(buf, oflags);
#endif
		}
	}

	if (fd != -1)
	    filname = buf;
	return (fd);
}

struct nlist **
lookup(st)
	struct syminfo *st;
{
	register int sh; 
	register struct nlist **hp;
	register char *cp, *cp1;
	register struct symseg *gp;
	register int i;

	sh = 0;
	if (!(cp = cursym.n_un.n_name))
		error(1, "object file inconsistency: symbol has no string");
	while (*cp)
		sh = (sh<<1) + *cp++;
	sh = (sh & 0x7fffffff) % HSIZE;
	for (gp = st->fs; gp < &(st->fs[NSEG]); gp++) {
		if (gp->sy_first == 0) {
			gp->sy_first = (struct nlist *)
			    calloc(NSYM, sizeof (struct nlist));
			gp->sy_hfirst = (struct nlist **)
			    calloc(HSIZE, sizeof (struct nlist *));
			if (gp->sy_first == 0 || gp->sy_hfirst == 0)
				error(1, "ran out of space for symbol table");
			gp->sy_last = gp->sy_first + NSYM;
			gp->sy_hlast = gp->sy_hfirst + HSIZE;
		}
		if (gp > st->cs)
			st->cs = gp;
		hp = gp->sy_hfirst + sh;
		i = 1;
		do {
			if (*hp == 0) {
				if (gp->sy_used == NSYM)
					break;
				return (hp);
			}
			cp1 = (*hp)->n_un.n_name; 
			for (cp = cursym.n_un.n_name; *cp == *cp1++;)
				if (*cp++ == 0)
					return (hp);
			hp += i;
			i += 2;
			if (hp >= gp->sy_hlast)
				hp -= HSIZE;
		} while (i < HSIZE);
		if (i > HSIZE)
			error(1, "hash table botch");
	}
	error(1, "symbol table overflow");
	/*NOTREACHED*/
}

symfree(st, saved)
	struct syminfo *st;
	struct nlist *saved;
{
	register struct symseg *gp;
	register struct nlist *sp;

	for (gp = st->cs; gp >= st->fs; gp--, st->cs--) {
		sp = gp->sy_first + gp->sy_used;
		if (sp == saved) {
			st->ns = sp;
			return;
		}
		for (sp--; sp >= gp->sy_first; sp--) {
			gp->sy_hfirst[sp->n_hash] = 0;
			gp->sy_used--;
			if (sp == saved) {
				st->ns = sp;
				return;
			}
		}
	}
	if (saved == 0)
		return;
	error(1, "symfree botch");
}

struct nlist **
slookup(s, st)
	char *s;
	struct syminfo *st;
{

	cursym.n_un.n_name = s;
	cursym.n_type = N_EXT+N_UNDF;
	cursym.n_value = 0;
	return (lookup(st));
}

enter(st, hp, nsp)
	register struct syminfo *st;
	register struct nlist **hp;
	register struct nlist *nsp;
{
	register struct nlist *sp;

	if (*hp==0) {
		if (hp < st->cs->sy_hfirst || hp >= st->cs->sy_hlast)
			error(1, "enter botch");
		*hp = st->ls = sp = st->cs->sy_first + st->cs->sy_used;
		st->cs->sy_used++;
		sp->n_un.n_name = nsp->n_un.n_name;
		sp->n_type = nsp->n_type;
		sp->n_hash = hp - st->cs->sy_hfirst;
		sp->n_value = nsp->n_value;
		st->ns = st->ls + 1;
(*dp)("enter: %s\t%s\t%x\t%x\n", st == &shsym ? "shsym" : "ldsym",
    nsp->n_un.n_name, nsp->n_type, nsp->n_value);
		return(1);
	} else {
		st->ls = *hp;
		return(0);
	}
}

symx(st, sp)
	struct syminfo *st;
	struct nlist *sp;
{
	register struct symseg *gp;

	if (sp == 0)
		return (0);
	for (gp = st->cs; gp >= st->fs; gp--)
		/* <= is sloppy so ldsym.ns will always work */
		if (sp >= gp->sy_first && sp <= gp->sy_last)
			return ((gp - st->fs) * NSYM + sp - gp->sy_first);
	error(1, "symx botch");
	/*NOTREACHED*/
}

symreloc()
{
	if (funding)
		return;
	switch (cursym.n_type & (N_TYPE+N_EXT)) {

	case N_TEXT:
	case N_EXT+N_TEXT:
		cursym.n_value += ctrel;
		return;

	case N_DATA:
	case N_EXT+N_DATA:
		cursym.n_value += cdrel;
		return;

	case N_BSS:
	case N_EXT+N_BSS:
		cursym.n_value += cbrel;
		return;

	case N_EXT+N_UNDF:
		return;

	default:
		if (cursym.n_type&N_EXT)
			cursym.n_type = N_EXT+N_ABS;
		return;
	}
}

/*VARARGS 2*/
error(n, s, w, x, y, z)
char *s;
{

	if (n == -2)
		n = 0;
	else
		fprintf(stderr, "ld: ");
	if (filname) {
		fprintf(stderr, "%s", filname);
		if (n != -1 && archdr.ar_name[0])
			fprintf(stderr, "(%s)", archdr.ar_name);
		fprintf(stderr, ": ");
	}
	fprintf(stderr, s, w, x, y, z);
	fprintf(stderr, "\n");
	if (n == -1)
		return;
	if (n)
		delexit();
	errlev = 2;
}

char *
errmsg(errnum)
	int errnum;
{
	extern int sys_nerr;
	extern char *sys_errlist[];
	static char buf[6+10+1];	/* "Error " + "int" + '\0' */

	if (errnum < 0 || errnum > sys_nerr) {
		(void) sprintf(buf, "Error %d", errnum);
		return (buf);
	} else
		return (sys_errlist[errnum]);
}

readhdr(loc)
off_t loc;
{

	dseek(&text, loc, (long)sizeof(filhdr));
	mget((short *)&filhdr, sizeof(filhdr), &text);
	if (N_BADMAG(filhdr)) {
		if (filhdr.a_magic == OARMAG)
			error(1, "old archive");
		error(1, "bad magic number");
	}
#if	TARGET==SUN3 || TARGET==SUN2
	if (filhdr.a_machtype == M_68020) {
		use68020 = 1;
	} else if ( !(filhdr.a_machtype == M_68010
	    || filhdr.a_machtype == M_OLDSUN2))
		error(1, "wrong machine type");
#endif	/* sun3 */
	if (filhdr.a_text&01 || filhdr.a_data&01)
		error(1, "text/data size odd");
#if	TARGET== SUN4
	if (filhdr.a_machtype != M_SPARC)
		error(1, "wrong machine type");
	if (filhdr.a_toolversion != TV_SUN4){
		error(1,"linker expected toolversion number 0x%x and got 0x%x",
				TV_SUN4, filhdr.a_toolversion);
	}
#endif /* sun4 */
	if (filhdr.a_magic == NMAGIC || filhdr.a_magic == ZMAGIC) {
		cdrel = -round(filhdr.a_text, segsize());
		cbrel = cdrel - filhdr.a_data;
	} else if (filhdr.a_magic == OMAGIC) {
		cdrel = -filhdr.a_text;
		cbrel = cdrel - filhdr.a_data;
	} else
		error(1, "bad format");
}

round(v, r)
	int v;
	u_long r;
{

	r--;
	v += r;
	v &= ~(long)r;
	return(v);
}


char *
savestr(cp, stab, sleft)
	register char *cp;
	char **stab;
	int *sleft;
{
	register int len;

	len = strlen(cp) + 1;
	if (len > *sleft) {
		*sleft = NSAVETAB;
		if (len > *sleft)
			saveleft = *sleft;
		*stab = mymalloc(*sleft);
		if (*stab == 0)
			error(1, "ran out of memory (savestr)");
	}
	strncpy(*stab, cp, len);
	cp = *stab;
	*stab += len;
	*sleft -= len;
	return (cp);
}

bopen(bp, off, bufsize)
	register struct biobuf *bp;
{

	bp->b_ptr = bp->b_buf = mymalloc(bufsize);
	if (bp->b_ptr == (char *)0)
		error(1, "ran out of memory (bopen)");
	bp->b_bufsize = bufsize;
	bp->b_nleft = bufsize - (off % bufsize);
	bp->b_off = off;
	bp->b_link = biobufs;
	biobufs = bp;
}

int	bwrerror;

bwrite(p, cnt, bp)
	register char *p;
	register int cnt;
	register struct biobuf *bp;
{
	register int put;
	register char *to;
	register int nwritten;

top:
	if (cnt == 0)
		return;
	if (bp->b_nleft) {
		put = bp->b_nleft;
		if (put > cnt)
			put = cnt;
		bp->b_nleft -= put;
		to = bp->b_ptr;
		bcopy(p, to, put);
		bp->b_ptr += put;
		p += put;
		cnt -= put;
		goto top;
	}
	if (cnt >= bp->b_bufsize) {
		if (bp->b_ptr != bp->b_buf)
			bflush1(bp);
		put = cnt - cnt % bp->b_bufsize;
		if (boffset != bp->b_off)
			lseek(biofd, bp->b_off, 0);
		nwritten = write(biofd, p, put);
		if (nwritten != put) {
			bwrerror = 1;
			filname = ofilename;		/* kludge */
			archdr.ar_name[0] = 0;		/* kludge */
			if (nwritten < 0)
				error(1, "output write error: %s",
				    errmsg(errno));
			else
				error(1, "output write error: premature EOF");
		}
		bp->b_off += put;
		boffset = bp->b_off;
		p += put;
		cnt -= put;
		goto top;
	}
	bflush1(bp);
	goto top;
}

bflush()
{
	register struct biobuf *bp;

	if (bwrerror)
		return;
	for (bp = biobufs; bp; bp = bp->b_link)
		bflush1(bp);
}

bflush1(bp)
	register struct biobuf *bp;
{
	register int cnt = bp->b_ptr - bp->b_buf;
	register int nwritten;

	if (cnt == 0)
		return;
	if (boffset != bp->b_off)
		lseek(biofd, bp->b_off, 0);
	nwritten = write(biofd, bp->b_buf, cnt);
	if (nwritten != cnt) {
		bwrerror = 1;
		filname = ofilename;		/* kludge */
		archdr.ar_name[0] = 0;		/* kludge */
		if (nwritten < 0)
			error(1, "output write error: %s", errmsg(errno));
		else
			error(1, "output write error: premature EOF");
	}
	bp->b_off += cnt;
	boffset = bp->b_off;
	bp->b_ptr = bp->b_buf;
	bp->b_nleft = bp->b_bufsize;
}

bflushc(bp, c)
	register struct biobuf *bp;
{

	bflush1(bp);
	bputc(c, bp);
}

bseek(bp, off)
	register struct biobuf *bp;
	register off_t off;
{
	bflush1(bp);
	
	bp->b_nleft = bp->b_bufsize - (off % bp->b_bufsize);
	bp->b_off = off;
}

/*
 * total the symbols found
 */
int
totalsymb()
{
	register int i;
	register int j = 0;

	for (i = 0; i < NSEG; i++) 
		if (ldsym.fs[i].sy_first != 0)
		    j += ldsym.fs[i].sy_used;
		 else 
		    break;
	return(j);
}

/*
 * old malloc where if you wanted to allocated 0 byte it will returned
 * you a address instead of null
 */
char *
mymalloc(cc)
	int cc;
{
	if (cc == 0)
	    return(malloc(1));
	else
	    return(malloc(cc));
}

char 	*WhiteSp = " 	|\\\n";
char
**prepend_argv(ld_opts, argv, argc)
	char 	*ld_opts;
	char	**argv;
	int	*argc;
{
	char 	*new, *s;
	char	**tmp_argv = NULL;
	int	tmp_argc = 0;
	int 	i, nochars = 0;

	while(isspace(*ld_opts)) ld_opts++;
 	s = ld_opts;
	nochars = strlen(ld_opts) + 1;
	while(strlen(s) != 0) {
		tmp_argc++;
		if((new = (char *)strpbrk(s, WhiteSp)) == NULL)
			break;
		else {
			*new++ = '\0';
			while(isspace(*new)) new++;
			s = new;
		};
	};
	tmp_argv = (char **)calloc(tmp_argc + *argc, sizeof(char *));
	*tmp_argv = *argv;	
	s = ld_opts;
	for(i=1; (i < (tmp_argc + *argc)); i++){
		if (i < (tmp_argc +1)){
			if((strlen(s) <= nochars) && (s != NULL)){
				while(isspace(*s)){ s++; nochars--;}
				tmp_argv[i] = s;
				if ((nochars -= (strlen(s)+1)) > 1)
					s = s + strlen(s) + 1;
				else s = s + strlen(s);
			};
		} else tmp_argv[i] = argv[i-tmp_argc];
	};
	*argc += tmp_argc;
	return(tmp_argv);
}

#ifdef BROWSER

cb_callback_write_stab()
{
}

#endif

#ifdef SUNPRO
struct	slc {				/* SUNPRO libdep cell */
	char 	*slc_name;		/* name of dependent library */
	struct	slc *slc_next;		/* next cell */
};

do_report_libdep(path)
	char	*path;
{
	struct slc *p;
	static struct slc *sp = 0;
	static struct slc **spp = &sp;

	for (p = sp; p; p = p->slc_next) {
		if (strcmp(path, p->slc_name) == 0)
			return;
	}
	*spp = (struct slc *)mymalloc(sizeof (struct slc));
	(*spp)->slc_name = (char *)strcpy(mymalloc(strlen(path) + 1), path);
	(*spp)->slc_next = (struct slc *)0;
	spp = &(*spp)->slc_next;
	report_libdep(path, "LD");
}
#endif NSE
