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

/* @(#)rtld.c 1.60 91/04/02 SMI */

/*
 * Run-time link editor.
 */

/*
 * Copyright (c) 1989, 1990, 1991 by Sun Microsystems, Inc.
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/mman.h>
#include <sys/errno.h>
#include <a.out.h>
#include <link.h>
#include "dynamic.h"
#include "reloc_info.h"
#include "rtld.h"
#include <strings.h>
#include "cache.h"
#include <sysexits.h>

#define VERSION2 2

/*
 * Code collapsing macros.
 */
#define JMPOFF(x)	(x)->v2->ld_plt
#define RELOCOFF(x)	(x)->v2->ld_rel
#define HASHOFF(x)	(x)->v2->ld_hash
#define SYMOFF(x)	(x)->v2->ld_stab
#define STROFF(x) 	(x)->v2->ld_symbols

#define DONTAPPEND 0
#define APPEND 1

#if	TARGET==SUN4
#               define  MASK(n) ((1<<(n))-1)
#               define  IN_RANGE(v,n)   ((-(1<<((n)-1))) <=(v) && (v) < (1<<((n)-1)))
#define isitpcrel(rp) (rp->r_type == RELOC_DISP8 || rp->r_type == RELOC_DISP16\
	    || rp->r_type == RELOC_DISP32 || rp->r_type == RELOC_WDISP30 \
	    || rp->r_type == RELOC_WDISP22)
#endif

/*
 * What we do:
 *	- crt0 will pass in the pointer to dynamic structure of main prog.
 *	- relocate ourselves.
 *	- version checking: get the libraries, 
 *		check the version and mmap them in.
 *	- relocate the _GOT_ for both user and shared libraries.
 *	- relocate _PLT_ to the binder.
 *	- go back to crt0.
 */

/*
 * Interface between crt0 & ld.so.
 */
struct crt_i1 {
	int	crt_baseaddr;		/* Address ld.so is at */
	int	crt_dzfd;		/* /dev/zero file descriptor */
	int	crt_rlfd;		/* ld.so file descriptor */
	struct	link_dynamic *crt_udp;	/* "main_" dynamic */
	char	**crt_ep;		/* environment strings */
	caddr_t	crt_breakp;		/* place to put initial breakpoint */
};

/*
 * Global declarations.
 */
int	devzero_fd;			/* cache for file descriptor */
struct	rtc_symb *rtcp;			/* allocated commons */
struct	link_map *hlmp = NULL;		/* head of link_map chain */
struct	link_map *plmp = NULL;		/* first public link_map */
struct	link_map *mlmp;			/* link map for "main" */
struct	link_map *ld_lmp;		/* link map for runtime linker */
struct	link_object ld_lo;		/* dummy link object for linker */
struct	link_map **plmpp;		/* insertion point for new lm's */
struct	link_map *flmp = NULL;		/* head of link_map free chain */
struct	dl_object *hdlp = NULL;		/* head of dl_object chain */
struct	dl_object **pdlpp;		/* insertion point for new dl's */
struct	dl_object *fdlp = NULL;		/* head of dl_object free chain */
caddr_t	top_of_stack;			/* top of stack (+1) */
char	*dl_error;			/* last dynamic linking error */
char	*tracing;			/* tracing or running? */
char	*preload;			/* other shared object to preload */
char	*profile;			/* activate profiling */
char	*symbols_public;		/* show ld.so symbols to debuggers */
char	*library_path;			/* alternate path for library search */
char	**environ;			/* root of environment strings */
char	*main_program;			/* string identifying lmp of "main" */
char	*ldso_program;			/* string identifying lmp of "ld.so" */
char	*cached_symbol = NULL;		/* last string cached by lookup */
int	use_cache = 1;			/* tell lo_lookup ok to use cache */
int	version_no;			/* link dynamic version number */

caddr_t rtmalloc();
static	struct nlist *lookup();
static	struct nlist *findsb();
static	struct nlist *ldsofindsb();
static	struct link_map *have_we_got_it();
static	struct link_map *new_lmp();
static	struct link_map *map_so();
static	void map_error();
static	struct dl_object *new_dlp();
static	void relocate();
static	void upd_reloc();
static	struct link_map *pc_to_lmp();
static	void free_dlp();
static	struct dl_object *dlopen_worker();

extern	int errno;
extern	void rtbinder();		/* declare the binding function */
extern	caddr_t caller();		/* function to return our caller's pc */

/*
 * Option storage.
 */
#ifdef PROF
int prof_fd;
char *prof_buf;
#endif

#ifdef WHOAMI
char  *progname;
#endif

#ifdef HRC_TIME
extern void hrc_init();
extern int hrc_time();

int ftime;
int stime;
int etime;
int time1;
int time2;
int time3;
int time4;
int time5;
#endif

/*
 * Dummies for LD_PRELOAD.
 */
static	struct link_dynamic_2 pld_ld2;	/* dummy structure for preloads */
static	struct link_dynamic pld_ld = 	/* dummy __DYNAMIC */
		{2, (struct ld_debug *)0};

/*
 * Run-time link editor bootstrap entry point.  Called by program requiring
 * initial link editing at startup (generally, crt0).
 */
rtld(version, iptr, dp)
	int version;			/* interface version */
	caddr_t iptr;			/* interface passed from program */
	register struct link_dynamic *dp; /* ld.so dynamic pointer */
{
	register struct crt_i1 *ip;	/* crt0 version 1 interface structure */
	register int reloc;		/* count of internal relocations */
	register			/* scratch relocation entry pointer */
	    struct relocation_info *rp;
	int i;				/* integer temporaries */
	char *bp, *cp;			/* character pointer temporaries */
	char *buf;			/* temporary storage pointers */
	struct	link_object *lop;	/* work pointer */
	struct	link_object **lopp; 	/* preloaded object insertion point */
	struct	link_map *lmp;		/* work pointer */
	struct	link_map *nlmp;		/* newly allocated link_map */

	/*
	 * Determine interface format.  A prototype interface once existed
	 * that passed an address as the first argument.  We assume this
	 * address is greater than the size of a page, and thus use this
	 * to test for it.  Eventually, support for this will be deleted.
	 */
	if (version < PAGSIZ) {	/* version 1 of new interface */
		ip =  (struct crt_i1 *) iptr;
		if (version != 1)
			panic(
			    "ld.so: unsupported crt interface version of %d\n",
			    version);
	} else 		/* old interface */
		panic("ld.so: obsolete crt interface\n");

	/*
	 * ld.so must itself be relocated, take care of this now.
	 * We can not refer to global data before this step is
	 * complete.  Perform the relocation by stepping over all
	 * entries in the relocation table and turn them into
	 * absolute addresses.  
	 *
	 * N.B. We have the assumption here that there are no 
	 * symbolic relocations which need to be performed.
	 */
	(int) dp->v2 += ip->crt_baseaddr;

#if	TARGET==SUN4
	/*
	 * SPARC does not have a divide instruction, but we can not
	 * call support routine until we have finished relocating the
	 * loader.
	 */
#define RELOC_SIZE sizeof (struct relocation_info)
	reloc = 0;
	i = GETRELSZ(dp);
	while (i != 0) {
		i -= RELOC_SIZE;
		reloc++;
	}
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
	reloc = GETRELSZ(dp) / sizeof (struct relocation_info);
#endif

	rp = (struct relocation_info *) (RELOCOFF(dp) + 
		(dp->ld_version < VERSION2 ? (int) dp : ip->crt_baseaddr));
	for (i = 0; i < reloc; i++) {
#if	TARGET==SUN4
	        upd_reloc(rp, (long *)(rp->r_address + ip->crt_baseaddr),
				ip->crt_baseaddr);
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
		*(long *)(rp->r_address + ip->crt_baseaddr) = 
			*(long *)(rp->r_address + ip->crt_baseaddr) + ip->crt_baseaddr;
#endif
		rp++;
	}

	/*
	 * Relocation is completed.  Update any global data necessary
	 * to continue.
	 */
	environ = ip->crt_ep;
	rtgetenv();
	devzero_fd = ip->crt_dzfd;
	pld_ld.ld_un.ld_2 = &pld_ld2;	/* C bug with initializing unions */

	/*
	 * Initialize link maps.  Create a link map for ld.so -- however
	 * do not put it on the "public list" unless we are making its
	 * symbols available to the debugger.
	 */
	pdlpp = &hdlp;
	plmpp = &hlmp;
	ldso_program = "/usr/lib/ld.so";
	ld_lo.lo_name = (long)ldso_program - ip->crt_baseaddr;
	ld_lmp = new_lmp(ldso_program, &ld_lo, ip->crt_baseaddr, 
	    ip->crt_baseaddr, dp, findsb);
	if (!symbols_public)
		hlmp = NULL;
	plmpp = &hlmp;
	db_malloc = heap_malloc = rtmalloc;
	is_secure = secure_objects;
	main_program = "main_$main_";

#ifdef PROF
	/*
	 * If profiling features are turned on, then see if the profiling
	 * file exists.  If so, map in the file and use it as our profile
	 * buffer.
	 */
	if ((prof_fd = open("/tmp/ld.so.profile_buffer", O_RDWR)) != -1) {
		if ((prof_buf = mmap(0, 0x2000, PROT_READ | PROT_WRITE, 
		    MAP_SHARED, prof_fd, 0)) == (caddr_t)-1)
			panic("ld.so: mmap failure (%d) for profile buffer\n",
			    errno);
		profil(prof_buf, 0x2000, ip->crt_baseaddr, 0x4000);
		close(prof_fd);
	}
#endif PROF
#ifdef HRC_TIME
	hrc_init();
	ftime = stime = hrc_time();
#endif

	/*
	 * Free descriptor on ld.so.
	 */
	close(ip->crt_rlfd);

	/*
	 * If we're letting the user freelance, then see if the user has
	 * specified any files to be dynamically loaded.  If so, build
	 * link-object data structures for them and get them.
	 * N.B. THIS IS FOR DEBUGGING ONLY.  IT IS NOT SUPPORTED NOR 
	 * IS IT TO BE DOCUMENTED!
	 */
	if ((cp = preload))
		if (!secure_objects()) {
			int f = 0;	/* flag for scanning */

			lopp = &(struct link_object *)pld_ld2.ld_need;
			bp = buf = rtmalloc(strlen(cp) + 1);
			do {
				*bp = *cp;
				if (isspace(*bp) || (*bp == '\0')) {
					*bp = '\0';
					if (f) {
						f = 0;
						lop = (struct link_object *)
						    rtmalloc(sizeof 
						    (struct link_object));
						lop->lo_name = (long)buf;
						lop->lo_library = 0;
						lop->lo_next = NULL;
						*lopp = lop;
						lopp = &(struct link_object *)
						    lop->lo_next;
					}
				} else 
					if (!f) {
						f = 1;
						buf = bp;
					}
				bp++;
			} while (*cp++);

			/*
			 * Have list of objects.  Go add them.
			 */
			(void) new_lmp("ld_$preload_", NULL, (caddr_t)0,
			    NULL, &pld_ld, findsb);
		}

	/*
	 * Insert link_map for "main".
	 */
	mlmp = new_lmp(main_program, NULL, MAIN_BASE, (caddr_t)0, ip->crt_udp,
	    findsb);

#ifdef HRC_TIME
	etime = hrc_time();
	time1 = etime - stime;
	stime = etime;
#endif

	/*
	 * Walk map of loaded objects, loading their requisite objects as
	 * necessary.  Note that the first one we load in this loop is the
	 * first one we hand back to the debugger (see use of plmp).
	 */
	for (lmp = hlmp; lmp; lmp = lmp->lm_next)
		if (lmp->lm_ld) {
			for (lop = (struct link_object *)
				&TEXTBASE(lmp)[lmp->lm_ld->v2->ld_need]; 
			    lop != (struct link_object *)&TEXTBASE(lmp)[0];
			    lop = (struct link_object *)
				      &TEXTBASE(lmp)[lop->lo_next])
				if (!have_we_got_it(lop, TEXTBASE(lmp)))
					if (nlmp = map_so(lmp, lop))
						if (!plmp)
							plmp = nlmp;
		}
	
	/*
	 * If we've just been asked to describe what we would load,
	 * describe what we did load, and terminate the process.
	 */
	if (tracing) {
		for (lmp = plmp; lmp; lmp = lmp->lm_next)
			if (lmp->lm_lop->lo_library)
				printf("\t-l%s.%d => %s\n",
				    &lmp->lm_lob[lmp->lm_lop->lo_name],
				    lmp->lm_lop->lo_major, 
				    lmp->lm_name ? lmp->lm_name : 
					"not found");
			else
				printf("\t%s%s\n", 
				    &lmp->lm_lob[lmp->lm_lop->lo_name], 
				    lmp->lm_name ? "" : " (not found)");
		_exit(0);
	}

#ifdef HRC_TIME
	etime = hrc_time();
	time2 = etime - stime;
	stime = etime;
	/* +++ time in mmap loop */
#endif
	/* 
	 * Relocate all loaded objects.
	 */
	for (lmp = hlmp; lmp; lmp = lmp->lm_next)
		if (lmp != ld_lmp)
			relocate(lmp, mlmp);

#ifdef HRC_TIME
	etime = hrc_time();
	time3 = etime - stime;
	stime = etime;
	/* +++ time in relocate loop */
#endif

	/*
	 * Store in the "main" link_dynamic the list of objects we
	 * loaded.  Also return any commons that were allocated.
	 */
	ip->crt_udp->v2->ld_loaded = plmp;
	if (rtcp)
		ip->crt_udp->ldd->ldd_cp = rtcp;

	/*
	 * If we're running under a debugger, let it know we've
	 * done our loader thing.
	 */
	if (ip->crt_udp->ldd->ldd_in_debugger) {
		ip->crt_udp->ldd->ldd_bp_addr = ip->crt_breakp;
		if (!mlmp->lm_rwt)
			if (mprotect(PAGEMASK & 
			    (int)ip->crt_breakp, PAGESIZE, 
			    PROT_READ | PROT_WRITE | PROT_EXEC) == -1)
				map_error("text write-enable", "main");
		ip->crt_udp->ldd->ldd_sym_loaded = 1;
		ip->crt_udp->ldd->ldd_bp_inst = *(int *)(ip->crt_breakp);
#if	TARGET==SUN4
		*(long *)(ip->crt_breakp) = TRAP;
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
		*(short *)(ip->crt_breakp) = TRAP;
#endif
		if (!mlmp->lm_rwt)
			if (mprotect(PAGEMASK & 
			    (int)ip->crt_breakp, PAGESIZE,
			    PROT_READ | PROT_EXEC) == -1)
				panic(
			    "ld.so: error %d on main text write-protect\n",
				    errno);
	}

	/*
	 * Turn write-protect back on for text segments.
	 */
	for (lmp = hlmp; lmp; lmp = lmp->lm_next) 
		if (lmp->lm_rwt) {
			if (mprotect(TEXTBASE(lmp), 
			    PROUND(lmp->lm_ld->v2->ld_text),
			    PROT_READ | PROT_EXEC) == -1)
				panic(
				    "ld.so: write protect error %d\n", errno);
			lmp->lm_rwt = 0;
		}

	/*
	 * Get rid of /dev/zero
	 */
	(void) close(devzero_fd);
	devzero_fd = -1;

#ifdef HRC_TIME
	etime = hrc_time();
	time4 = etime - stime;
	stime = etime;
	/* +++ time in misc stuff after relocate loop */
	fprintf(stderr, "before mainloop %x main loop %x relocate %x misc %x\n",
			time1, time2, time3, time4);
	fprintf(stderr, " total %x\n", etime - ftime);
#endif
}

/*
 * Indicate whether a given link_object has already been mapped into the
 * address space.
 */
static struct link_map *
have_we_got_it(lop, ba)
	struct link_object *lop;
	caddr_t ba;
{
	struct link_map *lmp;

	for (lmp = hlmp; lmp; lmp = lmp->lm_next)
		if (lmp->lm_lop)
			if (!strcmp(&ba[lop->lo_name], 
			    &lmp->lm_lob[lmp->lm_lop->lo_name]))
				return (lmp); 
	return (0);
}

/*
 * Handle mapping error.
 */
static void
map_error(s, cp)
	char *s;			/* type of mapping */
	char *cp;			/* object being mapped */
{

	if (errno == ENOMEM) {
		fprintf(stderr,
		    "ld.so: swap space exhausted for %s of %s\n", s, cp);
		_exit(EX_TEMPFAIL);
	} else
		panic("ld.so: %s error (%d) for %s\n", s, errno, cp);
}

static struct link_map *
mapit(s, lop, lob, intp)
	char *s;			/* object name */
	struct link_object *lop;	/* link object that derived file */
	caddr_t lob;			/* base address for link object */
	struct nlist *(*intp)();	/* interpreter for link map */
{
	struct exec exec;		/* working area for object headers */
	int fd;				/* file descriptor temporary */
	caddr_t	 addr;			/* mmap result temporary */
	struct link_dynamic *dp;	/* dynamic pointer of object mapped */
	int size;			/* size of object */ 

	if ((fd = open(s, O_RDONLY)) == -1)
		return(0);

	/*
	 * Verify the object's header.
	 */
	if (read(fd, (char *)&exec, sizeof (struct exec)) != 
	    sizeof (struct exec))
		panic("ld.so: can't read struct exec for %s\n", s);
	if 
#if TARGET == SUN2
	    (exec.a_machtype != M_68010) 
#endif
#if TARGET == SUN3
	    ((exec.a_machtype != M_68010) && (exec.a_machtype != M_68020)) 
#endif
#if TARGET == SUN4
	    (exec.a_machtype != M_SPARC) 
#endif
		panic("ld.so: %s is not for this machine type\n", s);

	/*
	 * Map text and allocate enough address space to fit the whole 
	 * library.  Note that we map enough to catch the first symbol
	 * in the symbol table and thereby avoid an "lseek" & "read"
	 * pair to pick it up.
	 */
	/* XXX need to fail in ways besides "panic". */
	size = max(SIZE(exec), N_SYMOFF(exec) + sizeof (struct nlist));
	if ((addr = mmap(0, size, PROT_READ | PROT_EXEC, MAP_PRIVATE,
	    fd, 0)) == (caddr_t)-1)
		map_error("mmap", s);

	/*
	 * Grab the first symbol entry while we've got it mapped aligned
	 * to file addresses.  We assume that this symbol describes the
	 * object's link_dynamic.
	 */
	dp = (struct link_dynamic *)&addr[
	   ((struct nlist *)&addr[N_SYMOFF(exec)])->n_value];

	/*
	 * Map the initialized data portion of the file to the correct
	 * point in the range of allocated addresses.  This will leave
	 * some portion of the data segment "doubly mapped" on machines
	 * where the text/data relocation alignment is not on a page
	 * boundaries.  However, leaving the file mapped has the double
	 * advantage of both saving the munmap system call and of leaving
	 * us a contiguous chunk of address space devoted to the object --
	 * in case we need to unmap it all later.
	 */
	if (mmap((caddr_t)(addr+SROUND(exec.a_text)), (int) exec.a_data, 
	    PROT_READ | PROT_WRITE | PROT_EXEC, MAP_FIXED | MAP_PRIVATE,
	    fd, (off_t) exec.a_text) == (caddr_t)-1)
		map_error("mmap data", s);

	/*
	 * Allocate pages for the object's bss, if necessary.
	 */
	if (exec.a_bss != 0) {
		if (!get_zero_object()) 
			map_error("mmap bss", s);
		if (mmap(addr + SROUND(exec.a_text) + exec.a_data,
		    (int) exec.a_bss,
		    PROT_READ | PROT_WRITE | PROT_EXEC, MAP_FIXED | MAP_PRIVATE,
		    devzero_fd, (off_t) 0) == (caddr_t)-1)
			map_error("mmap bss", s);
	}

	/*
	 * Finished with shared object mapping, get rid of descriptor.
	 */
	dp->v2 = (struct link_dynamic_2 *)((int)dp->v2 + (int)addr);
	(void) close(fd);
	return(new_lmp(s, lop, lob, addr, dp, intp));
}

/*
 * Map in a link object, return its link_map.
 */
static struct link_map *
map_so(lmp, lop)
	struct link_map *lmp;		/* link_map from which lop came */
	struct link_object *lop;	/* object to be mapped */
{
	caddr_t	 cp;			/* work pointer */
	static int flushed = 0;		/* only flush cache once */
	struct link_map *nlmp;		/* new link map for mapped object */

	/*
	 * Determine the absolute name of the object to be mapped.
	 */
	for (;;) {
		cp = lop->lo_library ? lo_lookup(lop, lmp) : 
		    &TEXTBASE(lmp)[lop->lo_name];
		if (!cp) {
			if (!tracing) 
				panic("ld.so: lib%s.so.%d: not found\n",
	 			    &TEXTBASE(lmp)[lop->lo_name],
				    lop->lo_major);
			return (new_lmp(NULL, lop, TEXTBASE(lmp), 0, 0, findsb));
		}
	
		/*
		 * Open object. If we can not, see if flushing the cache
		 * and retrying helps.  There is special handling for the
		 * "programmer's interface", embodied in libdl -- which
		 * is really just an alias for some of our own symbols.
		 * Note that this allows for special handling ONLY for
		 * a libdl installed with /usr/lib/ld.so -- i.e., the
		 * one *in* /usr/lib.
		 */
		if ((nlmp = mapit(cp, lop, TEXTBASE(lmp), 
		    (!strcmp(cp, "/usr/lib/libdl.so.1.0") ? 
		    ldsofindsb : findsb))) == 0) {
			if (flushed) {
				if (!tracing)
					panic("ld.so: open error %d for %s\n",
					    errno, cp);
				return (new_lmp(NULL, lop, TEXTBASE(lmp), 
				    0, 0, findsb));
			} else {
				lo_flush();
				flushed++;
			}
		} else
			break;
	}

	/*
	 * Process dynamic interface we just loaded, including
	 * list of objects on which this one depends.
	 */
	return (nlmp);
}

/*
 * Allocate a new link map for file "f", loaded at "addr" from link_object
 * "lop", with a dynamic at "dp".  Manage a free list of "link maps" -- as
 * we have no way to reclaim one into a "heap" when one is made free.
 */
static struct link_map *
new_lmp(f, lop, lob, addr, dp, intp)
	char *f;			/* file name */
	struct link_object *lop;	/* link object that derived file */
	caddr_t lob;			/* base address for link object */
	caddr_t addr;			/* address where mapped */
	struct link_dynamic *dp;	/* link_dynamic for file */
	struct nlist *(*intp)();	/* interpreter for link map */
{
	caddr_t	offset;			/* hack for "main" */
	struct link_map *lmp;		/* link map we built */

	/*
	 * For the "main" program, addresses in the link_dynamic structure
	 * are "broken" (text relocated to "0" rather than MAIN_BASE).  
	 * We "know" which these are, and deal with them especially here.
	 * XXX
	 */
	offset = f == main_program ? (caddr_t)MAIN_BASE : addr;

	/*
	 * Allocate link_map structure, and private data structure.
	 * Add the new link_map to the list of link_map's.
	 */
	if (lmp = flmp)
		flmp = lmp->lm_next;
	else {
		lmp = (struct link_map *)rtmalloc(sizeof (struct link_map));
		lmp->lm_lpd = rtmalloc(sizeof (struct ld_private));
	}
	lmp->lm_next = *plmpp;
	*plmpp = lmp;
	plmpp = &lmp->lm_next;

	/*
	 * Fill in the fields in the new link_map.  Load the private
	 * data cache with pre-relocated information from the object's
	 * unrelocated link_dynamic.
	 */
	LM2LP(lmp)->lp_symbol_base = lmp->lm_addr = addr;
	if (f) {
		lmp->lm_name = rtmalloc(strlen(f) + 1);
		strcpy(lmp->lm_name, f);
	} else
		lmp->lm_name = f;
	lmp->lm_lop = lop;
	lmp->lm_lob = lob;
	lmp->lm_ld = dp;

	/*
	 * Fill in fields from target's dynamic structure -- if there is
	 * one.  (There might not be because we are "tracing.")
	 */
	if (dp) {
		if (dp->ld_version < VERSION2)
			panic("ld.so: __DYNAMIC version %d not supported\n",
			    dp->ld_version);
		LM2LP(lmp)->lp_plt = (struct jbind *)(&addr[JMPOFF(dp)]);
		LM2LP(lmp)->lp_rp = 
		    (struct relocation_info *)(&offset[RELOCOFF(dp)]);
		LM2LP(lmp)->lp_hash = (struct fshash *)(&offset[HASHOFF(dp)]);
		LM2LP(lmp)->lp_symtab = (struct nlist *)(&offset[SYMOFF(dp)]);
		LM2LP(lmp)->lp_symstr = &offset[STROFF(dp)];
		LM2LP(lmp)->lp_textbase = offset;
		LM2LP(lmp)->lp_interp = intp;
		LM2LP(lmp)->lp_refcnt++;
		LM2LP(lmp)->lp_dlh = LM2LP(lmp)->lp_dlp = NULL;
	}
	return (lmp);
}

static void
free_lmp(lmp)
	struct link_map *lmp;
{
	struct	link_map *tlmp;		/* temporary */
	struct	link_map **plmp;	/* previous pointer */

	tlmp = hlmp, plmp = &hlmp;
	while (tlmp) {
		if (tlmp == lmp) {
			cached_symbol = NULL;
			(void) munmap(lmp->lm_addr, 
			    max(SIZE(*(struct exec *)lmp->lm_addr),
			    N_SYMOFF((*(struct exec *)lmp->lm_addr)) + 
			    sizeof (struct nlist)));
			*plmp = lmp->lm_next;
			if (plmpp == &lmp->lm_next)
				plmpp = plmp;
			lmp->lm_next = flmp;
			flmp = lmp;
			return;
		}
		plmp = &tlmp->lm_next;
		tlmp = tlmp->lm_next;
	}
	panic("ld.so: mangled lm object list.\n");
	/*NOTREACHED*/
}

/*
 * Relocate an object.
 */
static void
relocate(lmp, clmp)
	register struct link_map *lmp;	/* object to be relocated */
	register struct link_map *clmp;	/* calling link map */
{
	int k;				/* loop temporary */
	int nr;				/* number of relocations */
	char *symbol;			/* symbol being searched for */
	caddr_t et;			/* cached _etext of object */
	register long j;		/* relocation temporary */
	register caddr_t ra;		/* cached relocation address */
	register struct 		/* current relocation */
		relocation_info *rp;
	struct nlist *sp;		/* symbol table of "symbol" */
	struct link_map *llmp;		/* lmp of source of "symbol" */

	/*
	 * Cache some invariants.
	 */
	rp = LM2LP(lmp)->lp_rp;
	et = &TEXTBASE(lmp)[lmp->lm_ld->v2->ld_text];
	nr = GETRELSZ(lmp->lm_ld) / sizeof (struct relocation_info); 

	/*
	 * Initialize _PLT_, if any.
	 */
	if (lmp->lm_ld->v2->ld_plt_sz) {
#if	TARGET==SUN4
		int *inst = LM2LP(lmp)->lp_plt->jb_inst;
		int tmp = (int) rtbinder;

		*inst++ |= ((tmp >> (32-22)) & MASK(22));
		*inst |= (tmp & MASK(10)); 
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
		LM2LP(lmp)->lp_plt->cl_hi = (int) rtbinder >> 16;
		LM2LP(lmp)->lp_plt->cl_low = (int) rtbinder & 0xffff;
#endif
	}

	/*
	 * Loop over all relocations.
	 */
	for (k = 0; k < nr; k++, rp++) {
		/*
		 * Check to see if we're relocating in the text segment
		 * and turn off the write protect if necessary.
		 */
		if ((ra = &lmp->lm_addr[rp->r_address]) < et)
			if (lmp->lm_rwt == 0) {
				if (mprotect(TEXTBASE(lmp), 
				    PROUND((long)(et - TEXTBASE(lmp))),
				    PROT_READ | PROT_WRITE | PROT_EXEC) == -1)
					map_error("text write-enable", 
					    lmp->lm_name);
				lmp->lm_rwt = 1;
			}

		/*
		 * Perform the relocation.
		 */
		if (rp->r_extern == 0) {
#if	TARGET==SUN4
			upd_reloc(rp, (long *)ra, (long) lmp->lm_addr);
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
			*(long *)(ra) += (int)lmp->lm_addr;
#endif
		} else {
#if	TARGET==SUN4
			if (rp->r_type == RELOC_JMP_SLOT)
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
			if (rp->r_jmptable)
#endif
				continue;
			sp = &LM2LP(lmp)->lp_symtab[rp->r_symbolnum];
			symbol = &LM2LP(lmp)->lp_symstr[sp->n_un.n_strx];
			if ((sp = lookup(symbol, &llmp, clmp)) == NULL)
					panic("ld.so: Undefined symbol: %s\n",
						symbol); 
#if	TARGET==SUN4
	    		j = sp->n_value + (long) (sp->n_type==N_COMM ||
				sp->n_type==N_ABS+N_EXT ? 0 : llmp->lm_addr)
				+ rp->r_addend;
			if (isitpcrel(rp))
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
			j = sp->n_value + (long) (sp->n_type==N_COMM ||
				sp->n_type==N_ABS+N_EXT ? 0 : llmp->lm_addr);
			if (rp->r_pcrel)
#endif
				j -= (long)lmp->lm_addr;
#if	TARGET==SUN4
			upd_reloc(rp, (long *)ra, j);
#endif	TARGET==SUN4
#if	TARGET==SUN3 || TARGET==SUN2
			/*
			 * Optimize out the call to upd_reloc.  Note
			 * that upd_reloc has unimplemented sanity checking
			 * that, when implemented, may require removing
			 * or otherwise altering this optimization.
			 */
			*(long *)ra += j;
#endif	TARGET==SUN3 || TARGET==SUN2
		}
	}
}

/*
 * Perform a specific relocation operation.
 */
static void
upd_reloc(rp, where, what)
	struct relocation_info *rp;
	long *where;
	long what;
{
#if	TARGET==SUN4
	switch (rp->r_type) {
	case RELOC_RELATIVE:
		what += *where << (32-22);
		*(long *)where = (*(long *)where & ~MASK(22)) | 
					((what >> (32-22)) & MASK(22));
		where++;
		what += (*where & MASK(10));
		*(long *)where = (*(long *)where & ~MASK(10)) | (what & MASK(10));
		break;
	case RELOC_8:
	case RELOC_DISP8:
		what += *where & MASK(8);
		if (!IN_RANGE(what,8))
			fprintf(stderr, "byte displacement overflow at %#x",
				 rp->r_address);
		*where = what;
		break;
	case RELOC_LO10:
	case RELOC_BASE10:
		what += *where & MASK(10);
		*(long *)where = (*(long *)where & ~MASK(10)) | (what & MASK(10));
		break;
	case RELOC_BASE13:
	case RELOC_13:
		what += *where & MASK(13);
		*(long *)where = (*(long *)where & ~MASK(13)) | (what & MASK(13));
		break;

	case RELOC_16:
	case RELOC_DISP16:
		what += *where & MASK(16);
		if (!IN_RANGE(what,16))
			fprintf(stderr, "word displacement overflow at %#x",
				 rp->r_address);
		*(short *)where = what;
		break;
	case RELOC_22:
	case RELOC_BASE22:
		what += *where & MASK(22);
		if (!IN_RANGE(what,22))
			fprintf(stderr, "sethi displacement overflow at %#x",
				 rp->r_address);
		*(long *)where = (*(long *)where & ~MASK(22)) | (what & MASK(22));
		break;
	case RELOC_HI22:
		what += (*where<<32-22) & MASK(22);
		*(long *)where = (*(long *)where & ~MASK(22)) 
			| ((what>>(32-22)) & MASK(22));
		break;
	case RELOC_WDISP22:
		what += *where & MASK(22);
		if (what & MASK(2) )
			fprintf(stderr, "odd word displacement at %#x",
				 rp->r_address);
		what >>= 2;
		if (!IN_RANGE(what,22))
			fprintf(stderr, "branch displacement overflow at %#x",
				 rp->r_address);
		*(long *)where = (*(long *)where & ~MASK(22)) | (what & MASK(22));
		break;
		
	case RELOC_WDISP30:
		what += *where & MASK(30);
		if (what & MASK(2) )
			fprintf(stderr, "odd word displacement at %#x",
				 rp->r_address);
		what >>= 2;
		*(long *)where = (*(long *)where & ~MASK(30)) | (what&MASK(30));
		break;
	case RELOC_32:
	case RELOC_GLOB_DAT:
	case RELOC_DISP32:
		what += *where;
		*(long *)where = what;
		break;
	default:
		fprintf(stderr, "unknown relocation type %d", rp->r_type);
		break;
	}
#endif 
#if 	TARGET==SUN3 || TARGET==SUN2
	/*
	 * Put the value back in the segment,
	 * while checking for overflow.
	 */
	what += *where;
	*(long *)where = what;
	/* ++++ need to add this
	switch (rp->r_length) {
	case 0:	
		if (what < -128 || what > 127)
			fprintf(stderr, "byte displacement overflow at %#x",
				 rp->r_address);
		*where = what;
		break;
	case 1:
		if (what < -32768 || what > 32767)
			fprintf(stderr, "word displacement overflow at %#x",
				 rp->r_address);
		*(short *)where = what;
		break;
	case 2:		
		*(long *)where = what;
		break;
	}
	*/
#endif
}

/*
 * Calculate hash value for symbol being looked up.
 */
static int
gethashv(sn)
	char *sn;
{
	int val;			/* accumulator for hash value */
	/*
	 * Calculate hash value for symbol being looked up.
	 */
	for (val = 0; *sn;)
		val = (val<<1) + *sn++;
	return(val);
}

static struct nlist *
ldsofindsb(lmp, sn, clmp)
	struct link_map *lmp;
	char *sn;
	struct link_map *clmp;
{
	struct nlist *sp;

	if ((sp = findsb(lmp, sn, clmp)) == 0)
		return (0);
	if ((sp->n_value == 0) || (sp->n_type == N_EXT+N_UNDF))
		return (0);
	LM2LP(lmp)->lp_symbol_base = ld_lmp->lm_addr;
	return (findsb(ld_lmp, sn, clmp));
}

#define HASHMASK 0x7fffffff

static struct nlist *
findsb(lmp, sn, clmp)
	struct link_map *lmp;
	char *sn;
	struct link_map *clmp;
{
	register char *cp;		/* temporary */
	register char *s1;		/* string handling pointers */
	struct fshash *p;		/* working pointer to .so symbols */
	int i;				/* temporary */
	struct nlist *sp;		/* symbol entry pointer */
	static int hashval;		/* cache hash value of last symbol */
	static char *lp;		/* cache last symbol pointer */

	/* 
	 * this is a questionable cache since the same pointer may
	 * pointed to a different symbol. ++++
	 */
	if (lp != sn)
		hashval = gethashv(sn);

	if (LM2LP(lmp)->lp_hash == (struct fshash *)&lmp->lm_addr[0])
		return(0); 	/* not found */
	i = (hashval & HASHMASK) % (lmp->lm_ld->v2->ld_buckets == 0 
	    ? RTHS : lmp->lm_ld->v2->ld_buckets);
	p = LM2LP(lmp)->lp_hash + i; 
	if (p->fssymbno != -1)
		do {
			sp = &LM2LP(lmp)->lp_symtab[p->fssymbno];
			s1 = sn;
			cp = &LM2LP(lmp)->lp_symstr[sp->n_un.n_strx];
			while (*s1 == *cp++)
				if (*s1++=='\0') {
					return(sp);	/* found */
				}
			if (p->next == 0) 
				return(0);		/* not found */
			else 
				continue;		/* next symbol */
		} while (p = &LM2LP(lmp)->lp_hash[p->next]);
	return(0);
}

/*
 * Lookup symbol with name (s).  Return pointer to symbol table entry
 * representing it as well as index to dynamic object which defines it.
 */
static
struct nlist *
lookup(s, lmpp, clmp)
	char *s;
	struct link_map **lmpp;
	struct link_map *clmp;
{
	register struct link_map *lmp;	/* working pointer */
	register char *cp;		/* string handling pointers */
	register char *s1;		/* string handling pointers */
	struct nlist *sp;		/* symbol table entry */
	struct nlist *savesp;		/* save common data symbol entry */
	int msize = 0;			/* common maximum size */
	struct rtc_symb *rs;		/* new common */
	struct rtc_symb *trs;		/* temporary for new common */
	static struct nlist *lsp;	/* cached copy of last symbol found */
	static struct link_map *lmpc;	/* cached link map pointer */

	/*
	 * Heuristic: if this symbol is the same as the last one, then
	 * just skip all this work.
	 * XXX should count this to see how effective it is.
	 */
	if (cached_symbol)
		if (!strcmp(cached_symbol, s)) {
			*lmpp = lmpc;
			return(lsp);
		}
	cached_symbol = s;

	/*
	 * Over all loaded objects, determine find the hash entry corresponding
	 * to this value, and search for a definition for this symbol.
	 */
	for (lmp = hlmp; lmp; lmp = lmp->lm_next) {
		lmpc = lmp;
		if (lsp = sp = (*(LM2LP(lmp)->lp_interp))(lmp, s, clmp)) {
			/*
			 * We found a match.  If it is simply a reference 
			 * and not a definition we proceed to the next object.
			 * If it is a definition, and not a common then we 
			 * have found what we're looking for so return it. 
			 * Otherwise, determine whether the common has already 
			 * been allocated, and if so, return that pointer.
			 */
			if (sp->n_value == 0)
				continue;
			if (sp->n_type != N_EXT+N_UNDF) {
				if (msize == 0) {
					*lmpp = lmp;
					return (sp);
				}
			} else {
				/* 
				 * look up the runtime allocated commom
				 * symbol table 
				 */
				savesp = sp;
				trs = rtcp;
				while (trs) {
					s1 = s;
					cp = trs->rtc_sp->n_un.n_name;
					while (*s1 == *cp++)
						if (*s1++=='\0')
						    return (lsp = trs->rtc_sp);
					trs = trs->rtc_next;
				}

				/*
				 * It's an unallocated common, 
				 * accumulate size information.
				 */
				if (msize < sp->n_value)
					msize = sp->n_value;
				continue;
			}
		}
	}

	/*
	 * If we got this far, we either have found nothing suitable or
	 * else have a common.  In the former, punt, otherwise we now
	 * know enough to allocate the common so do so.
	 */
	if (msize == 0) {
		return (0);
	} else {
		rs = (struct rtc_symb *) rtmalloc(sizeof (struct rtc_symb));
		rs->rtc_sp = (struct nlist *)rtmalloc(sizeof (struct nlist));
		trs = rtcp;
		rtcp = rs;
		rs->rtc_next = trs;
		*(rs->rtc_sp) = *savesp;
		rs->rtc_sp->n_un.n_name = rtmalloc(strlen(s) + 1);
		strcpy(rs->rtc_sp->n_un.n_name, s);
		rs->rtc_sp->n_type = N_COMM;
		rs->rtc_sp->n_value = (long)rtmalloc(msize);
		*lmpp = lmpc = hlmp;
		return (lsp = rs->rtc_sp);
	}
}

/*
 * Procedure call binding.  Called on initial call reference to a global
 * unbound symbol.  
 */
int
binder(pc, relocindex)
	caddr_t pc;
	int relocindex;
{
	int address;			/* target address */
	register struct link_map *lmp;	/* link_map describing calling obj */
	register struct 		/* working relocation pointer */
	    relocation_info *rp;
	register struct nlist *sp;	/* entry for symbol being referenced */
	register char *symbol;		/* symbol being searched for */
	struct link_map *llmp;		/* link_map in which symbol is found */

	/*
	 * Find the object that our caller came from.
	 */
	for (lmp = hlmp; lmp; lmp = lmp->lm_next)
		if (pc > (caddr_t)(LM2LP(lmp)->lp_plt) && 
		    pc < (caddr_t)((int)LM2LP(lmp)->lp_plt + 
			lmp->lm_ld->v2->ld_plt_sz))
			goto gotit;
	/*
	 * If no object can be found, we can't understand how we got
	 * here, so we panic.
	 */
	if (lmp == NULL)
		panic("ld.so: unidentifiable procedure reference at 0x%x\n", 
		    pc);

gotit:
	/*
	 * Need to know which link_dynamic version for plt handling.
	 * This interface will be revised post 4.0 ++++++
	 */
#if	TARGET==SUN4
#define	LAST22BITS	0x3fffff
	version_no = lmp->lm_ld->ld_version;
	if (version_no == 3)
		relocindex = *(int *)(pc + 4) & LAST22BITS;
#endif
	/*
	 * Find the symbol being referenced, and then find a definition for it.
	 * If the latter can not be found, then terminate the program.
	 */
	/*
	 * N.B. Different error handling possibilities are a CASE
	 * opportunity: namely, incremental program construction and
	 * calls to debuggers.
	 */
	rp = &LM2LP(lmp)->lp_rp[relocindex];
	sp = &LM2LP(lmp)->lp_symtab[rp->r_symbolnum];
	symbol = &LM2LP(lmp)->lp_symstr[sp->n_un.n_strx];
	if ((sp = lookup(symbol, &llmp, lmp)) == NULL)
		panic("ld.so: call to undefined procedure %s from 0x%x\n",
		    symbol, pc);

	/*
	 * Rebuild the relocation entry to direct future calls directly
	 * to the target procedure.  Return the address to which this call
	 * should ultimately return.
	 */
	/*
	 * N.B. Much opportunity here for:
	 *	- call graph profiling.
	 *	- alternative procedure call implementation (RPC).
	 *	- value-added interposing.
	 */
	address = (int)&LM2LP(llmp)->lp_symbol_base[sp->n_value];
	stuffit((long *)&lmp->lm_addr[rp->r_address], address);
	return (address);
}

/*
 * Rebuild a _PLT_ entry after initial binding.  
 */
stuffit(where, what)
	long *where;
	int what;
{
#if	TARGET==SUN4
	long *i = where;
	switch (version_no) {
	case 3:
		*i = SETHI;
		*i = (*i & ~MASK(22)) | ((what>>(32-22)) & MASK(22));
		i++;
		*i = JMPI;
		*i = (*i & ~MASK(10)) | (what & MASK(10));
		break;
	default:
		*i = SETHI;
		*i = (*i & ~MASK(22)) | ((what>>(32-22)) & MASK(22));
		i++;
		*i = ORIT;
		*i = (*i & ~MASK(10)) | (what & MASK(10));
		*i++;
		*i++ = JMPI;
		*i = NOP;
		break;
	}
#endif
#if 	TARGET==SUN3 || TARGET==SUN2
	char *i = (char *)where;
	
	*where = what;
	*(short *)(i - 2) = JUMP;
#endif
}

/*
 * Utility function to round "v" to the next "r" boundary.
 */
round(v, r)
	u_int v;
	u_int r;
{
	r--;
	v += r;
	v &= ~(long)r;
	return (v);
}

/*
 * Make sure we have a source of heap.
 */
static
get_zero_object()
{
	int zfd;

	if (devzero_fd == -1)
		devzero_fd = open("/dev/zero", O_RDONLY);
	return (devzero_fd != -1);
}

/*
 * Local heap allocator.  Very simple, does not support storage freeing.
 * XXX Should be upgraded to support multiple heaps.  Should also handle
 * /dev/zero allocation errors.
 */
caddr_t
rtmalloc(nb)
	int nb;
{
	static caddr_t cp = 0;
	static caddr_t sp = 0;
	caddr_t tp;
	struct rlimit rlimit;
	int sl;

	if (cp == 0) {
		getrlimit(RLIMIT_STACK, &rlimit);
		sl = rlimit.rlim_cur - (rlimit.rlim_cur % PAGSIZ);
		sp = cp = (caddr_t)(top_of_stack - sl);
	} 
	if (cp + round(nb, (long)sizeof (double)) >= sp) {
		if (!get_zero_object())
			map_error("map heap", "/dev/zero");
		if ((sp = mmap(sp, round(nb, PAGSIZ), PROT_READ | PROT_WRITE,
		    MAP_FIXED | MAP_PRIVATE, devzero_fd, 0)) == (caddr_t)-1)
			    map_error("map heap", "/dev/zero");
		sp += round(nb,PAGSIZ);
	}
	tp = cp;
	cp += round(nb, sizeof (double));
	return (tp);
}
 
/*
 * Run-time link editor private getenv system call.  Scans for our 
 * special strings, and also calculates top of stack.
 */
char	ld_library_path[] =	"LD_LIBRARY_PATH";
char	ld_trace[] =		"LD_TRACE_LOADED_OBJECTS";
char	ld_profile[] =		"LD_PROFILE";
char	ld_preload[] =		"LD_PRELOAD";
char	ld_symbols_public[]=	"LD_SYMBOLS_PUBLIC";

static
rtgetenv()
{
	register char **p = environ;
	register char *v;

	/*
	 * Guard against no environment.  This seems practically impossible,
	 * as the variable "environ" is initialized in crt0 with an address
	 * on the stack.  However, if it actually does happen, we need to
	 * fake out the top of user_stack.
	 */
	if ((p == NULL) || (*p == NULL)) {
		top_of_stack = (caddr_t)(((int)&p + PAGSIZ - 1) 
		    & ~(PAGSIZ - 1));
		return;
	}

	/*
	 * Scan for LD_ environment variables that affect our behavior.
	 */
	while (v = *p++) {
		if (strncmp(v, "LD_", 3)) 
			continue;
		if (!strncmp(v, ld_library_path, sizeof (ld_library_path) - 1))
			library_path = v + sizeof (ld_library_path);
		else if (!strncmp(v, ld_preload, sizeof (ld_preload) - 1))
			preload = v + sizeof (ld_preload);
		else if (!strncmp(v, ld_trace, sizeof (ld_trace) - 1))
			tracing = v + sizeof (ld_trace);
		else if (!strncmp(v, ld_profile, sizeof (ld_profile) - 1))
			profile = v + sizeof (ld_profile);
		else if (!strncmp(v, ld_symbols_public, 
		    sizeof (ld_symbols_public) - 1))
			symbols_public = v + sizeof (ld_symbols_public);
	}

	/*
	 * Calculate the top of stack -- avoid dependence upon USRSTACK,
	 * but add dependence upon knowing that the stack ends with environment
	 * strings.  Note: program bootstraps already depend upon this
	 * knowledge, and since we're sort of kind of part of the bootstrap
	 * this isn't too much of a hack.
	 */
	for (v = *(p - 2); *v; v++)
		;;
	top_of_stack = (caddr_t)(((int)v + PAGSIZ - 1) & ~(PAGSIZ - 1));
}

/*
 * Simple programmatic interface to the dynamic linker.
 *
 * These functions constitute a simple interface that permits programs to
 * add shared objects, lookup symbols in such objects, and remove those
 * objects -- all under program control.  Such objects have special
 * symbol lookup interpretations, implemented by interpreter functions
 * such as the one immediately following.
 */
struct nlist *
dl_interpreter(lmp, s, clmp)
	struct link_map *lmp;
	char *s;
	struct link_map *clmp;
{
	if (LM2LP(lmp)->lp_dlh != LM2LP(clmp)->lp_dlh)
		return(0);
	else
		return(findsb(lmp, s, clmp));
}

/*
 * Add the shared object in "path" to the program.  Reference count the
 * objects so that multiple calls actually load only one instance of
 * the object -- also account for objects that might have been automatically
 * loaded at program start-up.
 */
struct dl_object *
dlopen(path, mode)
	char *path;			/* path name of object */
	int mode;			/* open mode */
{
	struct dl_object *dlp;		/* temporary return */

	/*
	 * Jacket function for real worker routine.  Establishes and
	 * cleans up general environment.
	 */
	dlp = dlopen_worker(path, mode);
	if (devzero_fd != -1) {
		(void) close(devzero_fd);
		devzero_fd = -1;
	}
	return (dlp);
}

static struct dl_object *
dlopen_worker(path, mode)
	char *path;			/* path name of object */
	int mode;			/* open mode */
{
	struct	link_map *lmp;		/* link map temporary */
	struct	link_map *nlmp;		/* newly created link maps */	
	struct	dl_object *dlp;		/* dl object temporary */
	struct	dl_object *wdlp;	/* working dl temporary */
	struct	dl_object **dlpp;	/* insertion pointer for dependents */
	struct	link_object *lop;	/* dependent link objects */
	char	*save_error;		/* error saving */

	/*
	 * Initialize error state.
	 */
	dl_error = DLE_none;

	/*
	 * Currently, no modes supported.
	 */
	if (mode != 1) {
		dl_error = DLE_mode_error;
		return(NULL);
	}

	/*
	 * If a null path, then we're operating on "main."
	 */
	if (!path)
		path = "main_$main_";

	/*
	 * Scan link map list looking for this object.  If found,
	 * then get a dl_object descriptor for it (creating the
	 * descriptor if necessary.)  Note that the situation in
	 * which we find an object with a descriptor, but where
	 * that object is either not on a dl "chain" (because
	 * it's part of "main") or is not the head of that chain,
	 * then we have a conflict.  The conflict is the result
	 * of having two "libraries" be depended upon by two different
	 * shared objects, leading to confusion over how the resolution
	 * of symbols should be handled.  We simply prohibit this.
	 */
	for (lmp = hlmp; lmp; lmp = lmp->lm_next)
		if (!strcmp(path, lmp->lm_name)) {
			dlp = LM2LP(lmp)->lp_dlp;
			if (dlp) {
				if ((LM2LP(lmp)->lp_dlh != NULL) &&
				    (LM2LP(lmp)->lp_dlh != dlp)) {
					dl_error = DLE_conflict;
					return (NULL);
				}
				dlp->dl_refcnt++;
			} else {
				LM2LP(lmp)->lp_refcnt++;
				dlp = new_dlp(lmp);
			}
			return (dlp);
		}

	/*
	 * No link map for it.  Make one, and then make a
	 * dl_object descriptor for it.  Add it to the
	 * address space, set it as the head of a chain.
	 */
	if (!(lmp = mapit(path, 0, 0, dl_interpreter))) {
		dl_error = DLE_can_not_open;
		return (NULL);
	}
	dlp = new_dlp(lmp);
	LM2LP(lmp)->lp_dlh = dlp;
	dlpp = &dlp->dl_dep;

	/*
	 * Now, see if there are any dependent objects.  For each,
	 * load it up, get a dlp, etc. and add it to the list of
	 * dependencies.
	 */
	for (wdlp = dlp; wdlp; wdlp = wdlp->dl_dep) {
		lmp = wdlp->dl_lmp;
		if (lmp->lm_ld) {
			for (lop = (struct link_object *)
			    &TEXTBASE(lmp)[lmp->lm_ld->v2->ld_need];
			    lop != (struct link_object *)&TEXTBASE(lmp)[0];
			    lop = (struct link_object *)
			    &TEXTBASE(lmp)[lop->lo_next])
				if ((nlmp =
				    have_we_got_it(lop, TEXTBASE(lmp))) ==
				    NULL) {
					if (nlmp = map_so(lmp, lop)) {
						*dlpp = new_dlp(nlmp);
						LM2LP(nlmp)->lp_interp =
						    dl_interpreter;
						LM2LP(nlmp)->lp_dlh = dlp;
						dlpp = &(*dlpp)->dl_dep;
					} else {
						dl_error = DLE_can_not_open;
						break;
					}
				} else 
					if (LM2LP(lmp)->lp_dlh) {
						dl_error = DLE_conflict;
						break;
					}
		}
		if (dl_error != DLE_none)
			break;
	}

	/*
	 * If an error occurred, we're hosed.  Close off everything we
	 * opened including ourselves.
	 */
	if (dl_error != DLE_none) {
		save_error = dl_error;
		(void) dlclose(dlp);
		dl_error = save_error;
		return (NULL);
	}
	
	/*
	 * Now, walk the list and relocate everything.
	 */
	for (wdlp = dlp; wdlp; wdlp = wdlp->dl_dep) {
		relocate(wdlp->dl_lmp, wdlp->dl_lmp);
		if (wdlp->dl_lmp->lm_rwt) {
			(void) mprotect(TEXTBASE(wdlp->dl_lmp),
			    PROUND(wdlp->dl_lmp->lm_ld->v2->ld_text),
			    PROT_READ | PROT_EXEC);
			wdlp->dl_lmp->lm_rwt = 0;
		}
	}
	return (dlp);
}

/*
 * Convert dlsym responses to appropriate address based on type of
 * symbol.  Note that in the case of dlsym references to an unallocated
 * common, we have to "simulate" a reference to it in order to stimulate
 * the allocation process.
 */
static caddr_t
dl_absolute(sp, lmp)
	struct	nlist *sp;		/* symbol table entry of symbol */
	struct	link_map *lmp;		/* link map that defined it */
{

	if (sp->n_type == N_UNDF+N_EXT && sp->n_value != 0) {
		sp = lookup(&LM2LP(lmp)->lp_symstr[sp->n_un.n_strx], &lmp, lmp);
		if (sp == NULL)
			return (0);
	}
	if (sp->n_type == N_COMM || sp->n_type == N_ABS+N_EXT)
		return ((caddr_t)sp->n_value);
	else
		return ((caddr_t)(sp->n_value + lmp->lm_addr));
}

#define	MAX_DLSYM_SYMBOL 1024		/* maximum size of symbol we match */
caddr_t
dlsym(dlp, symbol)
	struct dl_object *dlp;
	char *symbol;
{
	int	i;			/* temporary */
	caddr_t	pc;			/* caller's pc */
	struct	dl_object *wdlp;	/* working dlp */
	struct	link_map *lmp;		/* link map of symbol table */
	struct	nlist *sp;		/* symbol table entry */
	char	*cp;			/* temporary */
	char	buffer[MAX_DLSYM_SYMBOL + 2];
					/* "_" assembly, including null pad */

	/*
	 * Initialize error.
	 */
	dl_error = DLE_none;

	/*
	 * Check dl object handle -- if valid, then retrieve
	 * symbol from appropriate object.  Note that dlp of
	 * NULL is valid, and refers to the "caller" of dlsym.
	 */
	if (dlp) {
		if (!valid_dl_object(dlp))
			return (NULL);
	} else {
#ifdef notdef
		pc = caller();
		if ((lmp = pc_to_lmp(pc)) == NULL) {
			dl_error = DLE_bad_handle;
			return (NULL);
		}
#else notdef
		dl_error = DLE_bad_handle; /* XXX */
		return (NULL);
#endif notdef
	}
	for (wdlp = dlp; wdlp; wdlp = wdlp->dl_dep)
		if (sp = findsb(wdlp->dl_lmp, symbol, wdlp->dl_lmp))
			return (dl_absolute(sp, wdlp->dl_lmp));

	/*
	 * Symbol not found as supplied.  However, most of our symbols
	 * will be in the "C" name space, where the implementation prepends
	 * a "_" to the symbol as it emits it.  Therefore, attempt to find
	 * the symbol with the "_" prepend.
	 */
	buffer[0] = '_';
	cp = &buffer[1];
	i = 1;
	while (i < MAX_DLSYM_SYMBOL) {
		if ((*cp++ = *symbol++) == '\0') {
			for (wdlp = dlp; wdlp; wdlp = wdlp->dl_dep)
				if (sp = findsb(wdlp->dl_lmp, buffer,
				    wdlp->dl_lmp)) 
					return (dl_absolute(sp,
					    wdlp->dl_lmp));
			break;
		}
		i++;
	}
	dl_error = DLE_undefined;
	return (0);
}

/*
 * Functions to retrieve a symbol from a dlopen'ed object.
 */
static struct link_map *
pc_to_lmp(pc)
	caddr_t pc;			/* pc needing an lmp */
{
	int	size;			/* size of program */
	struct	link_map *lmp;		/* temporary */

	size = max(SIZE(*(struct exec *)lmp->lm_addr),
	    N_SYMOFF((*(struct exec *)lmp->lm_addr)) + 
	    sizeof (struct nlist));
	for (lmp = hlmp; lmp; lmp = lmp->lm_next)
		if (pc > lmp->lm_addr &&
		    pc < (lmp->lm_addr + size))
			break;
	return (lmp);
}

/*
 * Remove a dynamically loaded object from the program.  If it is
 * the last reference, then *really* remove the name -- and if the
 * name is the last reference to the object, then *really* remove
 * the object.
 */
int
dlclose(dlp)
	struct dl_object *dlp;
{
	struct	link_map *lmp;		/* temporary */
	struct	dl_object *wdlp;	/* temporary */
	struct	dl_object *ndlp;	/* where we're going next */

	/*
	 * Clear error.
	 */
	dl_error = DLE_none;

	/*
	 * Validity check object handle.
	 */
	if (!valid_dl_object(dlp)) {
		dl_error = DLE_bad_handle;
		return (-1);
	}

	/*
	 * Valid object: decrement reference count.  If we've still
	 * got any references then just leave.
	 */
	if (--dlp->dl_refcnt)
		return (0);

	/*
	 * Reference count went to zero.  By definition, that means
	 * that the reference count on everything we loaded also
	 * went to zero.  So free them all as well.
	 */
	for (wdlp = dlp; wdlp; wdlp = ndlp) {
		lmp = wdlp->dl_lmp;
		ndlp = wdlp->dl_dep;
		wdlp->dl_refcnt = 0;
		free_dlp(wdlp);
		if (--LM2LP(lmp)->lp_refcnt)
			continue;
		free_lmp(lmp);
	}
	return (0);
}

char *
dlerror() 
{
	char *error = dl_error;

	if (dl_error == DLE_none)
		return (0);
	dl_error = DLE_none;
	return (error);
}

/*
 * dl_object allocation and management.  "free'ed" dl_objects are not
 * returned to the "heap" (because we don't have a general heap manager
 * at present.)  So, they are kept on a list in case we need to use them
 * and requests for new dl_object's are allocated from this list rather
 * than from the heap.
 */
static struct dl_object *
new_dlp(lmp)
	struct link_map *lmp;
{
	struct dl_object *dlp;

	if (dlp = fdlp) 
		fdlp = dlp->dl_next;
	else
		dlp = (struct dl_object *) 
		    rtmalloc(sizeof (struct dl_object));
	dlp->dl_magic = DL_MAGIC;
	dlp->dl_cigam = DL_CIGAM;
	LM2LP(lmp)->lp_dlp = dlp;
	dlp->dl_lmp = lmp;
	dlp->dl_refcnt++;
	dlp->dl_next = NULL;
	dlp->dl_dep = NULL;
	*pdlpp = dlp;
	pdlpp = &dlp->dl_next;
	return (dlp);
}

static void
free_dlp(dlp)
	struct dl_object *dlp;
{
	struct	dl_object *tdlp;	/* temporary */
	struct	dl_object **pdlp;	/* previous pointer */

	tdlp = hdlp, pdlp = &hdlp;
	while (tdlp) {
		if (tdlp == dlp) {
			LM2LP(dlp->dl_lmp)->lp_dlp = NULL;
			LM2LP(dlp->dl_lmp)->lp_dlh = NULL;
			dlp->dl_magic = 0;
			dlp->dl_cigam = 0;
			*pdlp = dlp->dl_next;
			if (pdlpp == &dlp->dl_next)
				pdlpp = pdlp;
			dlp->dl_next = fdlp;
			fdlp = dlp;
			return;
		}
		pdlp = &tdlp->dl_next;
		tdlp = tdlp->dl_next;
	}
	panic("ld.so: mangled dl object list.\n");
	/*NOTREACHED*/
}

/*
 * Sanity check a program-provided dl_object handle.
 */
static
valid_dl_object(dlp)
	struct dl_object *dlp;
{
	if (dlp)
		if (dlp->dl_magic == DL_MAGIC &&
		    dlp->dl_cigam == DL_CIGAM &&
		    dlp->dl_refcnt != 0 &&
		    LM2LP(dlp->dl_lmp)->lp_dlh == dlp)
			return (1);
	dl_error = DLE_bad_handle;
	return (0);
}

#ifdef WHOAMI
/*
 * This routine is only available in-house.  It returns the 
 * program name of the running process.
 */
char *
whoami()
{
      return(progname);
}
#endif

