/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)kvm.c	5.9 (Berkeley) 6/27/90";
#endif /* LIBC_SCCS and not lint */

#include <machine/pte.h>
#include <machine/vmparam.h>
#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/text.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/vmmac.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <kvm.h>
#include <ctype.h>
#include <vis.h>
#include <nlist.h>
#include <pwd.h>
#include <string.h>
#include <ndbm.h>
#include <limits.h>
#include <paths.h>
#include <stdio.h>

/*
 * files
 */
static	char *unixf, *memf, *kmemf, *swapf;
static	int unixx, mem, kmem, swap;
static	DBM *db;
/*
 * flags
 */
static	int deadkernel;
static	int kvminit = 0;
static	int kvmfilesopen = 0;
/*
 * state
 */
static	struct kinfo_proc *kvmprocbase, *kvmprocptr;
static	int kvmnprocs;
/*
 * u. buffer
 */
static union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;
/*
 * random other stuff
 */
static	struct pte *Usrptmap, *usrpt;
static	int	dmmin, dmmax;
static	struct	pte *Sysmap;
static	int	Syssize;
static	int	pcbpf;
static	int	argaddr0;	/* XXX */
static	int	argaddr1;
static	int	nswap;
static	char	*tmp;
#if defined(hp300)
static	int	lowram;
#endif

#define basename(cp)	((tmp=rindex((cp), '/')) ? tmp+1 : (cp))
#define	MAXSYMSIZE	256

#if defined(hp300)
#define pftoc(f)	((f) - lowram)
#define iskva(v)	(1)
#endif

#ifndef pftoc
#define pftoc(f)	(f)
#endif
#ifndef iskva
#define iskva(v)	((v) & KERNBASE)
#endif

static struct nlist nl[] = {
	{ "_Usrptmap" },
#define	X_USRPTMAP	0
	{ "_usrpt" },
#define	X_USRPT		1
	{ "_nswap" },
#define	X_NSWAP		2
	{ "_dmmin" },
#define	X_DMMIN		3
	{ "_dmmax" },
#define	X_DMMAX		4
	/*
	 * everything here and down, only if a dead kernel
	 */
	{ "_Sysmap" },
#define	X_SYSMAP	5
#define	X_DEADKERNEL	X_SYSMAP
	{ "_Syssize" },
#define	X_SYSSIZE	6
	{ "_allproc" },
#define X_ALLPROC	7
	{ "_zombproc" },
#define X_ZOMBPROC	8
	{ "_nproc" },
#define	X_NPROC		9
#define	X_LAST		9
#if defined(hp300)
	{ "_lowram" },
#define	X_LOWRAM	(X_LAST+1)
#endif
	{ "" },
};

/*
 * returns 	0 if files were opened now,
 * 		1 if files were already opened,
 *		-1 if files could not be opened.
 */
kvm_openfiles(uf, mf, sf)
	char *uf, *mf, *sf; 
{
	if (kvmfilesopen)
		return (1);
	unixx = mem = kmem = swap = -1;
	unixf = (uf == NULL) ? _PATH_UNIX : uf; 
	memf = (mf == NULL) ? _PATH_MEM : mf;

	if ((unixx = open(unixf, O_RDONLY, 0)) == -1) {
		setsyserr("can't open %s", unixf);
		goto failed;
	}
	if ((mem = open(memf, O_RDONLY, 0)) == -1) {
		setsyserr("can't open %s", memf);
		goto failed;
	}
	if (sf != NULL)
		swapf = sf;
	if (mf != NULL) {
		deadkernel++;
		kmemf = mf;
		kmem = mem;
		swap = -1;
	} else {
		kmemf = _PATH_KMEM;
		if ((kmem = open(kmemf, O_RDONLY, 0)) == -1) {
			setsyserr("can't open %s", kmemf);
			goto failed;
		}
		swapf = (sf == NULL) ?  _PATH_DRUM : sf;
		/*
		 * live kernel - avoid looking up nlist entries
		 * past X_DEADKERNEL.
		 */
		nl[X_DEADKERNEL].n_name = "";
	}
	if (swapf != NULL && ((swap = open(swapf, O_RDONLY, 0)) == -1)) {
		seterr("can't open %s", swapf);
		goto failed;
	}
	kvmfilesopen++;
	if (kvminit == 0 && kvm_init(NULL, NULL, NULL, 0) == -1) /*XXX*/
		return (-1);
	return (0);
failed:
	kvm_close();
	return (-1);
}

static
kvm_init(uf, mf, sf)
	char *uf, *mf, *sf;
{
	if (kvmfilesopen == 0 && kvm_openfiles(NULL, NULL, NULL) == -1)
		return (-1);
	if (getkvars() == -1)
		return (-1);
	kvminit = 1;

	return (0);
}

kvm_close()
{
	if (unixx != -1) {
		close(unixx);
		unixx = -1;
	}
	if (kmem != -1) {
		if (kmem != mem)
			close(kmem);
		/* otherwise kmem is a copy of mem, and will be closed below */
		kmem = -1;
	}
	if (mem != -1) {
		close(mem);
		mem = -1;
	}
	if (swap != -1) {
		close(swap);
		swap = -1;
	}
	if (db != NULL) {
		dbm_close(db);
		db = NULL;
	}
	kvminit = 0;
	kvmfilesopen = 0;
	deadkernel = 0;
	if (Sysmap) {
		free(Sysmap);
		Sysmap = NULL;
	}
}

kvm_nlist(nl)
	struct nlist *nl;
{
	datum key, data;
	char dbname[MAXPATHLEN];
	char dbversion[_BSD_LINE_MAX];
	char kversion[_BSD_LINE_MAX];
	int dbversionlen;
	char symbuf[MAXSYMSIZE+1];
	struct nlist nbuf, *n;
	int num, did;

	if (kvmfilesopen == 0 && kvm_openfiles(NULL, NULL, NULL) == -1)
		return (-1);
	if (deadkernel)
		goto hard2;
	/*
	 * initialize key datum
	 */
	key.dptr = symbuf;
	symbuf[0] = KVMDB_NLIST;

	if (db != NULL)
		goto win;	/* off to the races */
	/*
	 * open database
	 */
	sprintf(dbname, "%s/kvm_%s", KVMDBDIR, basename(unixf));
	if ((db = dbm_open(dbname, O_RDONLY, 0)) == NULL)
		goto hard2;
	/*
	 * read version out of database
	 */
	bcopy("VERSION", symbuf+1, sizeof ("VERSION")-1);
	key.dsize = (sizeof ("VERSION") - 1) + 1;
	data = dbm_fetch(db, key);
	if (data.dptr == NULL)
		goto hard1;
	bcopy(data.dptr, dbversion, data.dsize);
	dbversionlen = data.dsize;
	/*
	 * read version string from kernel memory
	 */
	bcopy("_version", symbuf+1, sizeof ("_version")-1);
	key.dsize = (sizeof ("_version")-1) + 1;
	data = dbm_fetch(db, key);
	if (data.dptr == NULL)
		goto hard1;
	if (data.dsize != sizeof (struct nlist))
		goto hard1;
	bcopy(data.dptr, &nbuf, sizeof (struct nlist));
	lseek(kmem, nbuf.n_value, 0);
	if (read(kmem, kversion, dbversionlen) != dbversionlen)
		goto hard1;
	/*
	 * if they match, we win - otherwise do it the hard way
	 */
	if (bcmp(dbversion, kversion, dbversionlen) != 0)
		goto hard1;
	/*
	 * getem from the database.
	 */
win:
	num = did = 0;
	for (n = nl; n->n_name && n->n_name[0]; n++, num++) {
		int len;
		/*
		 * clear out fields from users buffer
		 */
		n->n_type = 0;
		n->n_other = 0;
		n->n_desc = 0;
		n->n_value = 0;
		/*
		 * query db
		 */
		if ((len = strlen(n->n_name)) > MAXSYMSIZE) {
			seterr("kvm_nlist: symbol too large");
			return (-1);
		}
		strcpy(symbuf+1, n->n_name);
		key.dsize = len + 1;
		data = dbm_fetch(db, key);
		if (data.dptr == NULL || data.dsize != sizeof (struct nlist))
			continue;
		bcopy(data.dptr, &nbuf, sizeof (struct nlist));
		n->n_value = nbuf.n_value;
		n->n_type = nbuf.n_type;
		n->n_desc = nbuf.n_desc;
		n->n_other = nbuf.n_other;
		did++;
	}
	return (num - did);
hard1:
	dbm_close(db);
	db = NULL;
hard2:
	return (nlist(unixf, nl));	/* XXX seterr if -1 */
}

kvm_getprocs(what, arg)
{
	if (kvminit == 0 && kvm_init(NULL, NULL, NULL, 0) == -1)
		return (NULL);
	if (!deadkernel) {
		int ret, copysize;

		if ((ret = getkerninfo(what, NULL, NULL, arg)) == -1) {
			setsyserr("can't get estimate for kerninfo");
			return (-1);
		}
		copysize = ret;
		if ((kvmprocbase = (struct kinfo_proc *)malloc(copysize)) 
		     == NULL) {
			seterr("out of memory");
			return (-1);
		}
		if ((ret = getkerninfo(what, kvmprocbase, &copysize, 
		     arg)) == -1) {
			setsyserr("can't get proc list");
			return (-1);
		}
		if (copysize % sizeof (struct kinfo_proc)) {
			seterr("proc size mismatch (kinfo_proc: %d)",
				sizeof (struct kinfo_proc));
			return (-1);
		}
		kvmnprocs = copysize / sizeof (struct kinfo_proc);
	} else {
		int nproc;

		if (kvm_read(nl[X_NPROC].n_value, &nproc, sizeof (int)) !=
		    sizeof (int)) {
			seterr("can't read nproc");
			return (-1);
		}
		if ((kvmprocbase = (struct kinfo_proc *)
		     malloc(nproc * sizeof (struct kinfo_proc))) == NULL) {
			seterr("out of memory (addr: %x nproc = %d)",
				nl[X_NPROC].n_value, nproc);
			return (-1);
		}
		kvmnprocs = kvm_doprocs(what, arg, kvmprocbase);
		realloc(kvmprocbase, kvmnprocs * sizeof (struct kinfo_proc));
	}
	kvmprocptr = kvmprocbase;

	return (kvmnprocs);
}

/*
 * XXX - should NOT give up so easily - especially since the kernel
 * may be corrupt (it died).  Should gather as much information as possible.
 * Follows proc ptrs instead of reading table since table may go
 * away soon.
 */
static
kvm_doprocs(what, arg, buff)
	int what, arg;
	char *buff;
{
	struct proc *p, proc;
	register char *bp = buff;
	int i = 0;
	int doingzomb = 0;
	struct eproc eproc;
	struct pgrp pgrp;
	struct session sess;
	struct tty tty;
	struct text text;

	/* allproc */
	if (kvm_read(nl[X_ALLPROC].n_value, &p, 
	    sizeof (struct proc *)) != sizeof (struct proc *)) {
		seterr("can't read allproc");
		return (-1);
	}

again:
	for (; p; p = proc.p_nxt) {
		if (kvm_read(p, &proc, sizeof (struct proc)) !=
		    sizeof (struct proc)) {
			seterr("can't read proc at %x", p);
			return (-1);
		}
		switch(ki_op(what)) {
			
		case KINFO_PROC_PID:
			if (proc.p_pid != (pid_t)arg)
				continue;
			break;


		case KINFO_PROC_UID:
			if (proc.p_uid != (uid_t)arg)
				continue;
			break;

		case KINFO_PROC_RUID:
			if (proc.p_ruid != (uid_t)arg)
				continue;
			break;
		}
		/*
		 * gather eproc
		 */
		eproc.e_paddr = p;
		if (kvm_read(proc.p_pgrp, &pgrp, sizeof (struct pgrp)) !=
	            sizeof (struct pgrp)) {
			seterr("can't read pgrp at %x", proc.p_pgrp);
			return (-1);
		}
		eproc.e_sess = pgrp.pg_session;
		eproc.e_pgid = pgrp.pg_id;
		eproc.e_jobc = pgrp.pg_jobc;
		if (kvm_read(pgrp.pg_session, &sess, sizeof (struct session))
		   != sizeof (struct session)) {
			seterr("can't read session at %x", pgrp.pg_session);
			return (-1);
		}
		if ((proc.p_flag&SCTTY) && sess.s_ttyp != NULL) {
			if (kvm_read(sess.s_ttyp, &tty, sizeof (struct tty))
			    != sizeof (struct tty)) {
				seterr("can't read tty at %x", sess.s_ttyp);
				return (-1);
			}
			eproc.e_tdev = tty.t_dev;
			eproc.e_tsess = tty.t_session;
			if (tty.t_pgrp != NULL) {
				if (kvm_read(tty.t_pgrp, &pgrp, sizeof (struct
				    pgrp)) != sizeof (struct pgrp)) {
					seterr("can't read tpgrp at &x", 
						tty.t_pgrp);
					return (-1);
				}
				eproc.e_tpgid = pgrp.pg_id;
			} else
				eproc.e_tpgid = -1;
		} else
			eproc.e_tdev = NODEV;
		if (proc.p_wmesg)
			kvm_read(proc.p_wmesg, eproc.e_wmesg, WMESGLEN);
		if (proc.p_textp) {
			kvm_read(proc.p_textp, &text, sizeof (text));
			eproc.e_xsize = text.x_size;
			eproc.e_xrssize = text.x_rssize;
			eproc.e_xccount = text.x_ccount;
			eproc.e_xswrss = text.x_swrss;
		} else {
			eproc.e_xsize = eproc.e_xrssize =
			  eproc.e_xccount = eproc.e_xswrss = 0;
		}

		switch(ki_op(what)) {

		case KINFO_PROC_PGRP:
			if (eproc.e_pgid != (pid_t)arg)
				continue;
			break;

		case KINFO_PROC_TTY:
			if ((proc.p_flag&SCTTY) == 0 || 
			     eproc.e_tdev != (dev_t)arg)
				continue;
			break;
		}

		i++;
		bcopy(&proc, bp, sizeof (struct proc));
		bp += sizeof (struct proc);
		bcopy(&eproc, bp, sizeof (struct eproc));
		bp+= sizeof (struct eproc);
	}
	if (!doingzomb) {
		/* zombproc */
		if (kvm_read(nl[X_ZOMBPROC].n_value, &p, 
		    sizeof (struct proc *)) != sizeof (struct proc *)) {
			seterr("can't read zombproc");
			return (-1);
		}
		doingzomb = 1;
		goto again;
	}

	return (i);
}

struct proc *
kvm_nextproc()
{

	if (!kvmprocbase && kvm_getprocs(0, 0) == -1)
		return (NULL);
	if (kvmprocptr >= (kvmprocbase + kvmnprocs)) {
		seterr("end of proc list");
		return (NULL);
	}
	return((struct proc *)(kvmprocptr++));
}

struct eproc *
kvm_geteproc(p)
	struct proc *p;
{
	return ((struct eproc *)(((char *)p) + sizeof (struct proc)));
}

kvm_setproc()
{

	kvmprocptr = kvmprocbase;
}

kvm_freeprocs()
{

	if (kvmprocbase) {
		free(kvmprocbase);
		kvmprocbase = NULL;
	}
}

struct user *
kvm_getu(p)
	struct proc *p;
{
	struct pte *pteaddr, apte;
	struct pte arguutl[HIGHPAGES+(CLSIZE*2)];
	register int i;
	int ncl;

	if (kvminit == 0 && kvm_init(NULL, NULL, NULL, 0) == -1)
		return (NULL);
	if (p->p_stat == SZOMB) {
		seterr("zombie process");
		return (NULL);
	}
	if ((p->p_flag & SLOAD) == 0) {
		if (swap < 0) {
			seterr("no swap");
			return (NULL);
		}
		(void) lseek(swap, (long)dtob(p->p_swaddr), 0);
		if (read(swap, (char *)&user.user, sizeof (struct user)) != 
		    sizeof (struct user)) {
			seterr("can't read u for pid %d from %s\n",
			    p->p_pid, swapf);
			return (NULL);
		}
		pcbpf = 0;
		argaddr0 = 0;
		argaddr1 = 0;
		return (&user.user);
	}
	pteaddr = &Usrptmap[btokmx(p->p_p0br) + p->p_szpt - 1];
	klseek(kmem, (long)pteaddr, 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		seterr("can't read indir pte to get u for pid %d from %s",
		    p->p_pid, kmemf);
		return (NULL);
	}
	lseek(mem, (long)ctob(pftoc(apte.pg_pfnum+1)) - sizeof(arguutl), 0);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		seterr("can't read page table for u of pid %d from %s",
		    p->p_pid, memf);
		return (NULL);
	}
	if (arguutl[0].pg_fod == 0 && arguutl[0].pg_pfnum)
		argaddr0 = ctob(pftoc(arguutl[0].pg_pfnum));
	else
		argaddr0 = 0;
	if (arguutl[CLSIZE*1].pg_fod == 0 && arguutl[CLSIZE*1].pg_pfnum)
		argaddr1 = ctob(pftoc(arguutl[CLSIZE*1].pg_pfnum));
	else
		argaddr1 = 0;
	pcbpf = arguutl[CLSIZE*2].pg_pfnum;
	ncl = (sizeof (struct user) + CLBYTES - 1) / CLBYTES;
	while (--ncl >= 0) {
		i = ncl * CLSIZE;
		lseek(mem,
		      (long)ctob(pftoc(arguutl[(CLSIZE*2)+i].pg_pfnum)), 0);
		if (read(mem, user.upages[i], CLBYTES) != CLBYTES) {
			seterr("can't read page %d of u of pid %d from %s",
			    arguutl[(CLSIZE*2)+i].pg_pfnum, p->p_pid, memf);
			return(NULL);
		}
	}
	return (&user.user);
}

char *
kvm_getargs(p, up)
	struct proc *p;
	struct user *up;
{
	char cmdbuf[CLBYTES*2];
	union {
		char	argc[CLBYTES*2];
		int	argi[CLBYTES*2/sizeof (int)];
	} argspac;
	register char *cp;
	register int *ip;
	char c;
	int nbad;
	struct dblock db;
	char *file;

	if (up == NULL || p->p_pid == 0 || p->p_pid == 2)
		goto retucomm;
	if ((p->p_flag & SLOAD) == 0 || argaddr1 == 0) {
		if (swap < 0 || p->p_ssize == 0)
			goto retucomm;
		vstodb(0, CLSIZE, &up->u_smap, &db, 1);
		(void) lseek(swap, (long)dtob(db.db_base), 0);
		if (read(swap, (char *)&argspac.argc[CLBYTES], CLBYTES)
			!= CLBYTES)
			goto bad;
		vstodb(1, CLSIZE, &up->u_smap, &db, 1);
		(void) lseek(swap, (long)dtob(db.db_base), 0);
		if (read(swap, (char *)&argspac.argc[0], CLBYTES) != CLBYTES)
			goto bad;
		file = swapf;
	} else {
		if (argaddr0) {
			lseek(mem, (long)argaddr0, 0);
			if (read(mem, (char *)&argspac, CLBYTES) != CLBYTES)
				goto bad;
		} else
			bzero(&argspac, CLBYTES);
		lseek(mem, (long)argaddr1, 0);
		if (read(mem, &argspac.argc[CLBYTES], CLBYTES) != CLBYTES)
			goto bad;
		file = memf;
	}
	ip = &argspac.argi[CLBYTES*2/sizeof (int)];
	ip -= 2;		/* last arg word and .long 0 */
	while (*--ip) {
		if (ip == argspac.argi)
			goto retucomm;
	}
	*(char *)ip = ' ';
	ip++;
	nbad = 0;
	for (cp = (char *)ip; cp < &argspac.argc[CLBYTES*2]; cp++) {
		c = *cp & 0177;
		if (c == 0)
			*cp = ' ';
		else if (c < ' ' || c > 0176) {
			if (++nbad >= 5*(0+1)) {	/* eflg -> 0 XXX */
				*cp++ = ' ';
				break;
			}
			*cp = '?';
		} else if (0 == 0 && c == '=') {	/* eflg -> 0 XXX */
			while (*--cp != ' ')
				if (cp <= (char *)ip)
					break;
			break;
		}
	}
	*cp = 0;
	while (*--cp == ' ')
		*cp = 0;
	cp = (char *)ip;
	(void) strncpy(cmdbuf, cp, &argspac.argc[CLBYTES*2] - cp);
	if (cp[0] == '-' || cp[0] == '?' || cp[0] <= ' ') {
		(void) strcat(cmdbuf, " (");
		(void) strncat(cmdbuf, p->p_comm, sizeof(p->p_comm));
		(void) strcat(cmdbuf, ")");
	}
	return (cmdbuf);

bad:
	seterr("error locating command name for pid %d from %s\n",
	    p->p_pid, file);
retucomm:
	(void) strcpy(cmdbuf, " (");
	(void) strncat(cmdbuf, p->p_comm, sizeof (p->p_comm));
	(void) strcat(cmdbuf, ")");
	return (cmdbuf);
}


static
getkvars()
{

	if (kvm_nlist(nl) == -1)
		return (-1);
	if (deadkernel) {
		/* We must do the sys map first because klseek uses it */
		long	addr;

		Syssize = nl[X_SYSSIZE].n_value;
		Sysmap = (struct pte *)
			calloc((unsigned) Syssize, sizeof (struct pte));
		if (Sysmap == NULL) {
			seterr("out of space for Sysmap");
			return (-1);
		}
		addr = (long) nl[X_SYSMAP].n_value;
		addr &= ~KERNBASE;
		(void) lseek(kmem, addr, 0);
		if (read(kmem, (char *) Sysmap, Syssize * sizeof (struct pte))
		    != Syssize * sizeof (struct pte)) {
			seterr("can't read Sysmap");
			return (-1);
		}
#if defined(hp300)
		addr = (long) nl[X_LOWRAM].n_value;
		(void) lseek(kmem, addr, 0);
		if (read(kmem, (char *) &lowram, sizeof (lowram))
		    != sizeof (lowram)) {
			seterr("can't read lowram");
			return (-1);
		}
		lowram = btop(lowram);
#endif
	}
	usrpt = (struct pte *)nl[X_USRPT].n_value;
	Usrptmap = (struct pte *)nl[X_USRPTMAP].n_value;
	if (kvm_read((long)nl[X_NSWAP].n_value, &nswap, sizeof (long)) !=
	    sizeof (long)) {
		seterr("can't read nswap");
		return (-1);
	}
	if (kvm_read((long)nl[X_DMMIN].n_value, &dmmin, sizeof (long)) !=
	    sizeof (long)) {
		seterr("can't read dmmin");
		return (-1);
	}
	if (kvm_read((long)nl[X_DMMAX].n_value, &dmmax, sizeof (long)) !=
	    sizeof (long)) {
		seterr("can't read dmmax");
		return (-1);
	}
	return (0);
}

kvm_read(loc, buf, len)
	unsigned long loc;
	char *buf;
{
	if (kvmfilesopen == 0 && kvm_openfiles(NULL, NULL, NULL) == -1)
		return (-1);
	if (iskva(loc)) {
		klseek(kmem, loc, 0);
		if (read(kmem, buf, len) != len) {
			seterr("error reading kmem at %x\n", loc);
			return (-1);
		}
	} else {
		lseek(mem, loc, 0);
		if (read(mem, buf, len) != len) {
			seterr("error reading mem at %x\n", loc);
			return (-1);
		}
	}
	return (len);
}

static
klseek(fd, loc, off)
	int fd;
	off_t loc;
	int off;
{

	if (deadkernel) {
		off_t vtophys();

		if ((loc = vtophys(loc)) == -1)
			return;
	}
	(void) lseek(fd, (off_t)loc, off);
}

/*
 * Given a base/size pair in virtual swap area,
 * return a physical base/size pair which is the
 * (largest) initial, physically contiguous block.
 */
static
vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase;
	int vssize;
	struct dmap *dmp;
	register struct dblock *dbp;
{
	register int blk = dmmin;
	register swblk_t *ip = dmp->dm_map;

	vsbase = ctod(vsbase);
	vssize = ctod(vssize);
	if (vsbase < 0 || vsbase + vssize > dmp->dm_size)
		/*panic("vstodb")*/;
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < dmmax)
			blk *= 2;
		ip++;
	}
	if (*ip <= 0 || *ip + blk > nswap)
		/*panic("vstodb")*/;
	dbp->db_size = MIN(vssize, blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + dbp->db_size) : vsbase);
}

static off_t
vtophys(loc)
	long loc;
{
	int p;
	off_t newloc;
	register struct pte *pte;

	newloc = loc & ~KERNBASE;
	p = btop(newloc);
#if defined(vax) || defined(tahoe)
	if ((loc & KERNBASE) == 0) {
		seterr("vtophys: translating non-kernel address");
		return((off_t) -1);
	}
#endif
	if (p >= Syssize) {
		seterr("vtophys: page out of bound (%d>=%d)", p, Syssize);
		return((off_t) -1);
	}
	pte = &Sysmap[p];
	if (pte->pg_v == 0 && (pte->pg_fod || pte->pg_pfnum == 0)) {
		seterr("vtophys: page not valid");
		return((off_t) -1);
	}
#if defined(hp300)
	if (pte->pg_pfnum < lowram) {
		seterr("vtophys: non-RAM page (%d<%d)", pte->pg_pfnum, lowram);
		return((off_t) -1);
	}
#endif
	loc = (long) (ptob(pftoc(pte->pg_pfnum)) + (loc & PGOFSET));
	return(loc);
}

#include <varargs.h>
static char errbuf[_BSD_LINE_MAX];

static
seterr(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;

	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vsprintf(errbuf, fmt, ap);
	va_end(ap);
}

static
setsyserr(va_alist)
	va_dcl
{
	char *fmt, *cp;
	va_list ap;
	extern int errno;

	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vsprintf(errbuf, fmt, ap);
	for (cp=errbuf; *cp; cp++)
		;
	sprintf(cp, ": %s", strerror(errno));
	va_end(ap);
}

char *
kvm_geterr()
{
	return (errbuf);
}
