/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)kvm.c	5.24 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <machine/vmparam.h>
#include <fcntl.h>
#include <nlist.h>
#include <kvm.h>
#include <ndbm.h>
#include <paths.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <vm/vm.h>	/* ??? kinfo_proc currently includes this*/
#include <vm/vm_param.h>
#include <vm/swap_pager.h>
#include <sys/kinfo_proc.h>

#include <limits.h>

#include "kvm_private.h"

#include <stdarg.h>

static int kvm_dbopen(kvm_t *, const char *);

char *
kvm_geterr(kvm_t *kd)
{
	return (kd->errbuf);
}

/*
 * Report an error using printf style arguments.  "program" is kd->program
 * on hard errors, and 0 on soft errors, so that under sun error emulation,
 * only hard errors are printed out (otherwise, programs like gdb will
 * generate tons of error messages when trying to access bogus pointers).
 */
void
_kvm_err(kvm_t *kd, const char *program, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	if (program != 0) {
		fprintf(stderr, "%s: ", program);
		vfprintf(stderr, fmt, ap);
		fputc('\n', stderr);
	} else
		vsnprintf(kd->errbuf, sizeof(kd->errbuf), (char *)fmt, ap);

	va_end(ap);
}

void
_kvm_syserr(kvm_t *kd, const char *program, const char *fmt, ...)
{
	va_list ap;
	register int n;

	va_start(ap, fmt);
	if (program != 0) {
		fprintf(stderr, "%s: ", program);
		vfprintf(stderr, fmt, ap);
		fprintf(stderr, ": ");
		perror((char *)0);
	} else {
		register char *cp = kd->errbuf;

		vsnprintf(cp, sizeof(kd->errbuf), (char *)fmt, ap);
		n = strlen(cp);
		snprintf(&cp[n], sizeof(kd->errbuf) - n, ": %s",
			 strerror(errno));
	}
	va_end(ap);
}

void *
_kvm_malloc(kd, n)
	register kvm_t *kd;
	register size_t n;
{
	void *p = (void *)malloc(n);

	if (p == 0)
		_kvm_err(kd, kd->program, "out of memory");
	return (p);
}

static kvm_t *
_kvm_open(kd, uf, mf, sf, flag, errout)
	register kvm_t *kd;
	const char *uf;
	const char *mf;
	const char *sf;
	int flag;
	char *errout;
{
	struct stat st;

	kd->vmfd = -1;
	kd->pmfd = -1;
	kd->swfd = -1;
	kd->nlfd = -1;
	kd->vmst = 0;
	kd->db = 0;
	kd->procbase = 0;
	kd->argspc = 0;
	kd->argv = 0;

	if (uf == 0)
		uf = _PATH_UNIX;
	else if (strlen(uf) >= MAXPATHLEN) {
		_kvm_err(kd, kd->program, "exec file name too long");
		goto failed;
	}
	if (flag & ~O_RDWR) {
		_kvm_err(kd, kd->program, "bad flags arg");
		goto failed;
	}
	if (mf == 0)
		mf = _PATH_MEM;
	if (sf == 0)
		sf = _PATH_DRUM;

	if ((kd->pmfd = open(mf, flag, 0)) < 0) {
		_kvm_syserr(kd, kd->program, "%s", mf);
		goto failed;
	}
	if (fstat(kd->pmfd, &st) < 0) {
		_kvm_syserr(kd, kd->program, "%s", mf);
		goto failed;
	}
	if (S_ISCHR(st.st_mode)) {
		/*
		 * If this is a character special device, then check that
		 * it's /dev/mem.  If so, open kmem too.  (Maybe we should
		 * make it work for either /dev/mem or /dev/kmem -- in either
		 * case you're working with a live kernel.)
		 */
		if (strcmp(mf, _PATH_MEM) != 0) {	/* XXX */
			_kvm_err(kd, kd->program,
				 "%s: not physical memory device", mf);
			goto failed;
		}
		if ((kd->vmfd = open(_PATH_KMEM, flag)) < 0) {
			_kvm_syserr(kd, kd->program, "%s", _PATH_KMEM);
			goto failed;
		}
		if ((kd->swfd = open(sf, flag, 0)) < 0) {
			_kvm_syserr(kd, kd->program, "%s", sf);
			goto failed;
		}
		/*
		 * Open kvm nlist database.  We go ahead and do this
		 * here so that we don't have to hold on to the vmunix
		 * path name.  Since a kvm application will surely do
		 * a kvm_nlist(), this probably won't be a wasted effort.
		 * If the database cannot be opened, open the namelist
		 * argument so we revert to slow nlist() calls.
		 */
		if (kvm_dbopen(kd, uf) < 0 && 
		    (kd->nlfd = open(uf, O_RDONLY, 0)) < 0) {
			_kvm_syserr(kd, kd->program, "%s", uf);
			goto failed;
		}
	} else {
		/*
		 * This is a crash dump.
		 * Initalize the virtual address translation machinery,
		 * but first setup the namelist fd.
		 */
		if ((kd->nlfd = open(uf, O_RDONLY, 0)) < 0) {
			_kvm_syserr(kd, kd->program, "%s", uf);
			goto failed;
		}
		if (_kvm_initvtop(kd) < 0)
			goto failed;
	}
	return (kd);
failed:
	/*
	 * Copy out the error if doing sane error semantics.
	 */
	if (errout != 0)
		strcpy(errout, kd->errbuf);
	(void)kvm_close(kd);
	return (0);
}

kvm_t *
kvm_openfiles(uf, mf, sf, flag, errout)
	const char *uf;
	const char *mf;
	const char *sf;
	int flag;
	char *errout;
{
	register kvm_t *kd = (kvm_t *)malloc(sizeof(*kd));

	if (kd == 0) {
		strcpy(errout, "out of memory");
		return (0);
	}
	kd->program = 0;
	return _kvm_open(kd, uf, mf, sf, flag, errout);
}

kvm_t *
kvm_open(uf, mf, sf, flag, program)
	const char *uf;
	const char *mf;
	const char *sf;
	int flag;
	const char *program;
{
	register kvm_t *kd = (kvm_t *)malloc(sizeof(*kd));

	if (kd == 0 && program != 0) {
		fprintf(stderr, "%s: out of memory", program);
		return (0);
	}
	kd->program = program;
	return _kvm_open(kd, uf, mf, sf, flag, (char *)0);
}

int
kvm_close(kd)
	kvm_t *kd;
{
	register int error = 0;

	if (kd->pmfd >= 0)
		error |= close(kd->pmfd);
	if (kd->vmfd >= 0)
		error |= close(kd->vmfd);
	if (kd->nlfd >= 0)
		error |= close(kd->nlfd);
	if (kd->swfd >= 0)
		error |= close(kd->swfd);
	if (kd->db != 0)
		dbm_close(kd->db);
	if (kd->vmst)
		_kvm_freevtop(kd);
	if (kd->procbase != 0)
		free((void *)kd->procbase);
	if (kd->argv != 0)
		free((void *)kd->argv);
	free((void *)kd);

	return (0);
}

/*
 * Set up state necessary to do queries on the kernel namelist
 * data base.  If the data base is out-of-data/incompatible with 
 * given executable, set up things so we revert to standard nlist call.
 * Only called for live kernels.  Return 0 on success, -1 on failure.
 */
static int
kvm_dbopen(kd, uf)
	kvm_t *kd;
	const char *uf;
{
	char *cp;
	datum rec;
	int dbversionlen;
	struct nlist nitem;
	char dbversion[_POSIX2_LINE_MAX];
	char kversion[_POSIX2_LINE_MAX];
	char dbname[MAXPATHLEN];

	if ((cp = rindex(uf, '/')) != 0)
		uf = cp + 1;

	sprintf(dbname, "%skvm_%s", _PATH_VARRUN, uf);
	kd->db = dbm_open(dbname, O_RDONLY, 0);
	if (kd->db == 0)
		return (-1);
	/*
	 * read version out of database
	 */
	rec.dptr = VRS_KEY;
	rec.dsize = sizeof(VRS_KEY) - 1;
	rec = dbm_fetch(kd->db, rec);
	if (rec.dptr == 0 || rec.dsize > sizeof(dbversion))
		goto close;

	bcopy(rec.dptr, dbversion, rec.dsize);
	dbversionlen = rec.dsize;
	/*
	 * Read version string from kernel memory.
	 * Since we are dealing with a live kernel, we can call kvm_read()
	 * at this point.
	 */
	rec.dptr = VRS_SYM;
	rec.dsize = sizeof(VRS_SYM) - 1;
	rec = dbm_fetch(kd->db, rec);
	if (rec.dptr == 0 || rec.dsize != sizeof(struct nlist))
		goto close;
	bcopy((char *)rec.dptr, (char *)&nitem, sizeof(nitem));
	if (kvm_read(kd, (u_long)nitem.n_value, kversion, dbversionlen) != 
	    dbversionlen)
		goto close;
	/*
	 * If they match, we win - otherwise clear out kd->db so
	 * we revert to slow nlist().
	 */
	if (bcmp(dbversion, kversion, dbversionlen) == 0)
		return (0);
close:
	dbm_close(kd->db);
	kd->db = 0;

	return (-1);
}

int
kvm_nlist(kd, nl)
	kvm_t *kd;
	struct nlist *nl;
{
	register struct nlist *p;
	register int nvalid;

	/*
	 * If we can't use the data base, revert to the 
	 * slow library call.
	 */
	if (kd->db == 0)
		return (__fdnlist(kd->nlfd, nl));

	/*
	 * We can use the kvm data base.  Go through each nlist entry
	 * and look it up with a dbm query.
	 */
	nvalid = 0;
	for (p = nl; p->n_name && p->n_name[0]; ++p) {
		register int len;
		datum rec;

		if ((len = strlen(p->n_name)) > 4096) {
			/* sanity */
			_kvm_err(kd, kd->program, "symbol too large");
			return (-1);
		}
		rec.dptr = p->n_name;
		rec.dsize = len;
		rec = dbm_fetch(kd->db, rec);
		if (rec.dptr == 0 || rec.dsize != sizeof(struct nlist))
			continue;
		++nvalid;
		/*
		 * Avoid alignment issues.
		 */
		bcopy((char *)&((struct nlist *)rec.dptr)->n_type,
		      (char *)&p->n_type, 
		      sizeof(p->n_type));
		bcopy((char *)&((struct nlist *)rec.dptr)->n_value,
		      (char *)&p->n_value, 
		      sizeof(p->n_value));
	}
	/*
	 * Return the number of entries that weren't found.
	 */
	return ((p - nl) - nvalid);
}

ssize_t
kvm_write(kd, kva, buf, len)
	kvm_t *kd;
	register u_long kva;
	register const char *buf;
	register size_t len;
{
	_kvm_err(kd, kd->program, "kvm_write not implemented");
	return (ssize_t)(0);
}

ssize_t
kvm_read(kd, kva, buf, len)
	kvm_t *kd;
	register u_long kva;
	register char *buf;
	register size_t len;
{
	register int cc;
	register char *cp;

	if (ISALIVE(kd)) {
		/*
		 * We're using /dev/kmem.  Just read straight from the
		 * device and let the active kernel do the address translation.
		 */
		errno = 0;
		if (lseek(kd->vmfd, (off_t)kva, 0) == -1 && errno != 0) {
			_kvm_err(kd, 0, "invalid address (%x)", kva);
			return (0);
		}
		cc = read(kd->vmfd, buf, len);
		if (cc < 0) {
			_kvm_syserr(kd, 0, "kvm_read");
			return (0);
		} else if (cc < len)
			_kvm_err(kd, kd->program, "short read");
		return (ssize_t)(cc);
	} else {
		cp = buf;
		while (len > 0) {
			u_long pa;
		
			cc = _kvm_kvatop(kd, kva, &pa);
			if (cc == 0)
				return (0);
			if (cc > len)
				cc = len;
			errno = 0;
			if (lseek(kd->pmfd, (off_t)pa, 0) == -1 && errno != 0) {
				_kvm_syserr(kd, 0, _PATH_MEM);
				break;
			}
			cc = read(kd->pmfd, cp, cc);
			if (cc < 0) {
				_kvm_syserr(kd, kd->program, "kvm_read");
				break;
			}
			cp += cc;
			kva += cc;
			len -= cc;
		}
		return (cp - buf);
	}
	/* NOTREACHED */
}
