static char sccsid[] = "@(#)alterfile.c	4.4 (Melbourne) 82/02/27";

/*
 * userprivs file conversion program
 *
 *	This utility converts the userprivs file
 *	from one form to another, after a modification to
 *	the basic structure definition
 *
 * method
 *	put the new structure in "./udata.h"
 *	and the old defn in "/usr/include/udata.h"
 *
 *	add/remove code below so that all fields in the old structure
 *	that still exist in the new one are copied
 *
 *	run the program
 */

#include <stdio.h>
#include <arrays.h>
#include <sys/types.h>
#define udata Udata
#include <udata.h>
#undef	udata
#include "udata.h"

struct udata new, zero;
struct Udata old;

main()
{
	register fd, nfd, i;

	if ((fd = open(UPRIVFILE, 0)) < 0) {
		perror(UPRIVFILE);
		exit(1);
	}
	if (link(UPRIVFILE, "/usr/adm/oldprivs") < 0) {
		perror("/usr/adm/oldprivs");
		exit(1);
	}
	if (unlink(UPRIVFILE) < 0) {
		perror("unlink");
		exit(1);
	}
	if ((nfd = creat(UPRIVFILE, 0444)) < 0) {
		perror(UPRIVFILE);
		link("/usr/adm/oldprivs", UPRIVFILE);
		exit(1);
	}
	while (read(fd, &old, sizeof old) == sizeof old) {
		if (allzero(&old, sizeof old)) {
			lseek(nfd, (long)sizeof old, 1);
			continue;
		}
		new = zero;	/* not necessary, but it doesn't hurt either */

		for (i = 0; i < elmts(old.ud_flags); i++)
			if (i < elmts(new.ud_flags))
				new.ud_flags[i] = old.ud_flags[i];
		new.ud_maxrss = old.ud_maxrss;
		new.ud_maxfile = old.ud_maxfile;
		new.ud_maxcore = old.ud_maxcore;
		new.ud_maxstack = old.ud_maxstack;
		new.ud_maxdata = old.ud_maxdata;
		new.ud_maxcpu = old.ud_maxcpu;
		new.ud_raise = old.ud_raise;
		new.ud_ttys = old.ud_ttys;
		new.ud_maxlogin = old.ud_maxlogin;
		new.ud_umask = old.ud_umask;
		new.ud_expires = old.ud_expires;

		new.ud_logon[0].tr_low.xt_ok = 0;
		new.ud_logon[0].tr_high.xt_ok = 0;

		if (write(nfd, &new, sizeof new) != sizeof new) {
			perror("write");
			exit(1);
		}
	}
	exit(0);
}

allzero(addr, size)
	register char *addr;
	register size;
{
	while (size-- > 0)
		if (*addr++)
			return(0);
	return(1);
}
