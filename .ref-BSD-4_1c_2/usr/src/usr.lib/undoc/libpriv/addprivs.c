static char Sccsid[] = "@(#)addprivs.c	4.1	(Melbourne)	82/02/21";

#include <stdio.h>
#include <sys/types.h>
#include <sys/quota.h>
#include <sys/mush.h>
#include <udata.h>
#include <lpdquota.h>
#include <mushmuck.h>

struct lpquota l1, l2;
struct udata u1, u2;
struct mushmuck m1, m2;

struct dquot d1[16];
struct dquot d2[16];

char	f1[16][32];
char	f2[16][32];

main(c, v)
char **v;
{
	register i;

	if (c < 3) {
		fprintf(stderr, "Usage: %s in ... out\n", *v);
		exit(1);
	}

	if (!rdprivf(v[1], &m1, &l1, d1, f1, &u1))
		err(v[1]);

	for (i = 2; i < c-1; i++) {
		if (!rdprivf(v[i], &m2, &l2, d2, f2, &u2))
			err(v[i]);
		addmush(&m1, &m2);
		addudata(&u1, &u2);
		addlpdq(&l1, &l2);
		adddq(d1, f1, d2, f2);
	}

	if (wrtprivf(v[c-1], &m1, &l1, d1, f1, &u1))
		err(v[c-1]);

	exit(0);
}

err(s)
char *s;
{
	fprintf(stderr, "%s: cannot open\n", s);
	exit(1);
}
