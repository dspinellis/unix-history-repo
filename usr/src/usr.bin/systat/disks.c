/*-
 * Copyright (c) 1980, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)disks.c	5.14 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/buf.h>

#include <nlist.h>
#include <ctype.h>
#include <paths.h>
#include <string.h>
#include <stdlib.h>
#include "systat.h"
#include "extern.h"

static void dkselect __P((char *, int, int []));
static int read_names __P((void));

static struct nlist namelist[] = {
#define	X_DK_NDRIVE	0
	{ "_dk_ndrive" },
#define	X_DK_WPMS	1
	{ "_dk_wpms" },
#ifdef vax
#define	X_MBDINIT	(X_DK_WPMS+1)
	{ "_mbdinit" },
#define	X_UBDINIT	(X_DK_WPMS+2)
	{ "_ubdinit" },
#endif
#ifdef sun
#define	X_MBDINIT	(X_DK_WPMS+1)
	{ "_mbdinit" },
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_DK_WPMS+1)
	{ "_vbdinit" },
#endif
#ifdef hp300
#define X_HPDINIT       (X_DK_WPMS+1)
        { "_hp_dinit" }, 
#endif
	{ "" },
};

float *dk_mspw;
int dk_ndrive, *dk_select;
char **dr_name;

#include "names.c"					/* XXX */

int
dkinit()
{
	register int i;
	register char *cp;
	static int once = 0;
	static char buf[1024];

	if (once)
		return(1);

	if (kvm_nlist(kd, namelist)) {
		nlisterr(namelist);
		return(0);
	}
	if (namelist[X_DK_NDRIVE].n_value == 0) {
		error("dk_ndrive undefined in kernel");
		return(0);
	}
	NREAD(X_DK_NDRIVE, &dk_ndrive, LONG);
	if (dk_ndrive <= 0) {
		error("dk_ndrive=%d according to %s", dk_ndrive, _PATH_UNIX);
		return(0);
	}
	dk_mspw = (float *)calloc(dk_ndrive, sizeof (float));
	{
		long *wpms = (long *)calloc(dk_ndrive, sizeof(long));
		KREAD(NPTR(X_DK_WPMS), wpms, dk_ndrive * sizeof (long));
		for (i = 0; i < dk_ndrive; i++)
			*(dk_mspw + i) = (*(wpms + i) == 0)? 0.0:
			                 (float) 1.0 / *(wpms + i);
		free(wpms);
	}
	dr_name = (char **)calloc(dk_ndrive, sizeof (char *));
	dk_select = (int *)calloc(dk_ndrive, sizeof (int));
	for (cp = buf, i = 0; i < dk_ndrive; i++) {
		dr_name[i] = cp;
		sprintf(dr_name[i], "dk%d", i);
		cp += strlen(dr_name[i]) + 1;
		if (dk_mspw[i] != 0.0)
			dk_select[i] = 1;
	}
	if (!read_names()) {
		free(dr_name);
		free(dk_select);
		free(dk_mspw);
		return(0);
	}
	once = 1;
	return(1);
}

int
dkcmd(cmd, args)
	char *cmd, *args;
{
	if (prefix(cmd, "display") || prefix(cmd, "add")) {
		dkselect(args, 1, dk_select);
		return (1);
	}
	if (prefix(cmd, "ignore") || prefix(cmd, "delete")) {
		dkselect(args, 0, dk_select);
		return (1);
	}
	if (prefix(cmd, "drives")) {
		register int i;

		move(CMDLINE, 0); clrtoeol();
		for (i = 0; i < dk_ndrive; i++)
			if (dk_mspw[i] != 0.0)
				printw("%s ", dr_name[i]);
		return (1);
	}
	return (0);
}

static void
dkselect(args, truefalse, selections)
	char *args;
	int truefalse, selections[];
{
	register char *cp;
	register int i;
	char *index();

	cp = index(args, '\n');
	if (cp)
		*cp = '\0';
	for (;;) {
		for (cp = args; *cp && isspace(*cp); cp++)
			;
		args = cp;
		for (; *cp && !isspace(*cp); cp++)
			;
		if (*cp)
			*cp++ = '\0';
		if (cp - args == 0)
			break;
		for (i = 0; i < dk_ndrive; i++)
			if (strcmp(args, dr_name[i]) == 0) {
				if (dk_mspw[i] != 0.0)
					selections[i] = truefalse;
				else
					error("%s: drive not configured",
					    dr_name[i]);
				break;
			}
		if (i >= dk_ndrive)
			error("%s: unknown drive", args);
		args = cp;
	}
}
