/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)bsd_openprom.h	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: bsd_openprom.h,v 1.3 92/09/09 00:41:33 leres Exp $
 */

/*
 * This file defines the interface between the kernel and the Openboot PROM.
 * N.B.: this has been tested only on interface versions 0 and 2 (we have
 * never seen interface version 1).
 */

/*
 * The v0 interface tells us what virtual memory to scan to avoid PMEG
 * conflicts, but the v2 interface fails to do so, and we must `magically'
 * know where the OPENPROM lives in virtual space.
 */
#define	OPENPROM_STARTVADDR	0xffd00000
#define	OPENPROM_ENDVADDR	0xfff00000

#define	OPENPROM_MAGIC 0x10010407

/*
 * Version 0 PROM vector device operations (collected here to emphasise that
 * they are deprecated).  Open and close are obvious.  Read and write are
 * segregated according to the device type (block, network, or character);
 * this is unnecessary and was eliminated from the v2 device operations, but
 * we are stuck with it.
 *
 * Seek is probably only useful on tape devices, since the only character
 * devices are the serial ports.
 *
 * Note that a v0 device name is always exactly two characters ("sd", "le",
 * and so forth).
 */
struct v0devops {
	int	(*v0_open)(char *dev);
	int	(*v0_close)(int d);
	int	(*v0_rbdev)(int d, int nblks, int blkno, caddr_t addr);	
	int	(*v0_wbdev)(int d, int nblks, int blkno, caddr_t addr);	
	int	(*v0_wnet)(int d, int nbytes, caddr_t addr);
	int	(*v0_rnet)(int d, int nbytes, caddr_t addr);
	int	(*v0_rcdev)(int d, int nbytes, int, caddr_t addr);
	int	(*v0_wcdev)(int d, int nbytes, int, caddr_t addr);
	int	(*v0_seek)(int d, long offset, int whence);
};

/*
 * Version 2 device operations.  Open takes a device `path' such as
 * /sbus/le@0,c00000,0 or /sbus/esp@.../sd@0,0, which means it can open
 * anything anywhere, without any magic translation.
 *
 * The memory allocator and map functions are included here even though
 * they relate only indirectly to devices (e.g., mmap is good for mapping
 * device memory, and drivers need to allocate space in which to record
 * the device state).
 */
struct v2devops {
	int	(*v2_xxx1)(int d);	/* ??? convert fd to something */

	/* Memory allocation and release. */
	caddr_t	(*v2_malloc)(caddr_t va, u_int sz);
	void	(*v2_free)(caddr_t va, u_int sz);

	/* Device memory mapper. */
	caddr_t	(*v2_mmap)(caddr_t va, int asi, u_int pa, u_int sz);
	void	(*v2_munmap)(caddr_t va, u_int sz);

	/* Device open, close, etc. */
	int	(*v2_open)(char *devpath);
	void	(*v2_close)(int d);
	int	(*v2_read)(int d, caddr_t buf, int nbytes);
	int	(*v2_write)(int d, caddr_t buf, int nbytes);
	void	(*v2_seek)(int d, int hi, int lo);

	void	(*v2_xxx2)();		/* ??? */
	void	(*v2_xxx3)();		/* ??? */
};

/*
 * The v0 interface describes memory regions with these linked lists.
 * (The !$&@#+ v2 interface reformats these as properties, so that we
 * have to extract them into local temporary memory and reinterpret them.)
 */
struct v0mlist {
	struct	v0mlist *next;
	caddr_t	addr;
	u_int	nbytes;
};

/*
 * V0 gives us three memory lists:  Total physical memory, VM reserved to
 * the PROM, and available physical memory (which, presumably, is just the
 * total minus any pages mapped in the PROM's VM region).  We can find the
 * reserved PMEGs by scanning the taken VM.  Unfortunately, the V2 prom
 * forgot to provide taken VM, and we are stuck with scanning ``magic''
 * addresses.
 */
struct v0mem {
	struct	v0mlist **v0_phystot;	/* physical memory */
	struct	v0mlist **v0_vmprom;	/* VM used by PROM */
	struct	v0mlist **v0_physavail;	/* available physical memory */
};

/*
 * The version 0 PROM breaks up the string given to the boot command and
 * leaves the decoded version behind.
 */
struct v0bootargs {
	char	*ba_argv[8];		/* argv format for boot string */
	char	ba_args[100];		/* string space */
	char	ba_bootdev[2];		/* e.g., "sd" for `b sd(...' */
	int	ba_ctlr;		/* controller # */
	int	ba_unit;		/* unit # */
	int	ba_part;		/* partition # */
	char	*ba_kernel;		/* kernel to boot, e.g., "vmunix" */
	void	*ba_spare0;		/* not decoded here	XXX */
};

/*
 * The version 2 PROM interface uses the more general, if less convenient,
 * approach of passing the boot strings unchanged.  We also get open file
 * numbers for stdin and stdout (keyboard and screen, or whatever), for use
 * with the v2 device ops.
 */
struct v2bootargs {
	char	**v2_bootpath;		/* V2: Path to boot device */
	char	**v2_bootargs;		/* V2: Boot args */
	int	*v2_fd0;		/* V2: Stdin descriptor */
	int	*v2_fd1;		/* V2: Stdout descriptor */
};

/*
 * The following structure defines the primary PROM vector interface.
 * The Boot PROM hands the kernel a pointer to this structure in %o0.
 * There are numerous substructures defined below.
 */
struct promvec {
	/* Version numbers. */
	u_int	pv_magic;		/* Magic number */
	u_int	pv_romvec_vers;		/* interface version (0, 2) */
	u_int	pv_plugin_vers;		/* ??? */
	u_int	pv_printrev;		/* PROM rev # (* 10, e.g 1.9 = 19) */

	/* Version 0 memory descriptors (see below). */
	struct	v0mem pv_v0mem;		/* V0: Memory description lists. */

	/* Node operations (see below). */
	struct	nodeops *pv_nodeops;	/* node functions */

	char	**pv_bootstr;		/* Boot command, eg sd(0,0,0)vmunix */

	struct	v0devops pv_v0devops;	/* V0: device ops */

	/*
	 * PROMDEV_* cookies.  I fear these may vanish in lieu of fd0/fd1
	 * (see below) in future PROMs, but for now they work fine.
	 */
	char	*pv_stdin;		/* stdin cookie */
	char	*pv_stdout;		/* stdout cookie */
#define	PROMDEV_KBD	0		/* input from keyboard */
#define	PROMDEV_SCREEN	0		/* output to screen */
#define	PROMDEV_TTYA	1		/* in/out to ttya */
#define	PROMDEV_TTYB	2		/* in/out to ttyb */

	/* Blocking getchar/putchar.  NOT REENTRANT! (grr) */
	int	(*pv_getchar)(void);
	void	(*pv_putchar)(int ch);

	/* Non-blocking variants that return -1 on error. */
	int	(*pv_nbgetchar)(void);
	int	(*pv_nbputchar)(int ch);

	/* Put counted string (can be very slow). */
	void	(*pv_putstr)(char *str, int len);

	/* Miscellany. */
	void	(*pv_reboot)(char *bootstr);
	void	(*pv_printf)(const char *fmt, ...);
	void	(*pv_abort)(void);	/* L1-A abort */
	int	*pv_ticks;		/* Ticks since last reset */
	__dead void (*pv_halt)(void);	/* Halt! */
	void	(**pv_synchook)(void);	/* "sync" command hook */

	/*
	 * This eval's a FORTH string.  Unfortunately, its interface
	 * changed between V0 and V2, which gave us much pain.
	 */
	union {
		void	(*v0_eval)(int len, char *str);
		void	(*v2_eval)(char *str);
	} pv_fortheval;

	struct	v0bootargs **pv_v0bootargs;	/* V0: Boot args */

	/* Extract Ethernet address from network device. */
	u_int	(*pv_enaddr)(int d, char *enaddr);

	struct	v2bootargs pv_v2bootargs;	/* V2: Boot args + std in/out */
	struct	v2devops pv_v2devops;	/* V2: device operations */

	int	pv_spare[15];

	/*
	 * The following is machine-dependent.
	 *
	 * The sun4c needs a PROM function to set a PMEG for another
	 * context, so that the kernel can map itself in all contexts.
	 * It is not possible simply to set the context register, because
	 * contexts 1 through N may have invalid translations for the
	 * current program counter.  The hardware has a mode in which
	 * all memory references go to the PROM, so the PROM can do it
	 * easily.
	 */
	void	(*pv_setctxt)(int ctxt, caddr_t va, int pmeg);
};

/*
 * In addition to the global stuff defined in the PROM vectors above,
 * the PROM has quite a collection of `nodes'.  A node is described by
 * an integer---these seem to be internal pointers, actually---and the
 * nodes are arranged into an N-ary tree.  Each node implements a fixed
 * set of functions, as described below.  The first two deal with the tree
 * structure, allowing traversals in either breadth- or depth-first fashion.
 * The rest deal with `properties'.
 *
 * A node property is simply a name/value pair.  The names are C strings
 * (NUL-terminated); the values are arbitrary byte strings (counted strings).
 * Many values are really just C strings.  Sometimes these are NUL-terminated,
 * sometimes not, depending on the the interface version; v0 seems to
 * terminate and v2 not.  Many others are simply integers stored as four
 * bytes in machine order: you just get them and go.  The third popular
 * format is an `address', which is made up of one or more sets of three
 * integers as defined below.
 *
 * N.B.: for the `next' functions, next(0) = first, and next(last) = 0.
 * Whoever designed this part had good taste.  On the other hand, these
 * operation vectors are global, rather than per-node, yet the pointers
 * are not in the openprom vectors but rather found by indirection from
 * there.  So the taste balances out.
 */
struct openprom_addr {
	int	oa_space;		/* address space (may be relative) */
	u_int	oa_base;		/* address within space */
	u_int	oa_size;		/* extent (number of bytes) */
};

struct nodeops {
	/*
	 * Tree traversal.
	 */
	int	(*no_nextnode)(int node);	/* next(node) */
	int	(*no_child)(int node);	/* first child */

	/*
	 * Property functions.  Proper use of getprop requires calling
	 * proplen first to make sure it fits.  Kind of a pain, but no
	 * doubt more convenient for the PROM coder.
	 */
	int	(*no_proplen)(int node, caddr_t name);
	int	(*no_getprop)(int node, caddr_t name, caddr_t val);
	int	(*no_setprop)(int node, caddr_t name, caddr_t val, int len);
	caddr_t	(*no_nextprop)(int node, caddr_t name);
};
