/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)disks.c	5.1 (Berkeley) %G%";
#endif not lint

#include "systat.h"
#include <sys/buf.h>
#include <ctype.h>

static struct nlist nlst[] = {
#define	X_DK_NDRIVE	0
	{ "_dk_ndrive" },
#define	X_DK_MSPW	1
	{ "_dk_mspw" },
#ifdef vax
#define	X_MBDINIT	2
	{ "_mbdinit" },
#define	X_UBDINIT	3
	{ "_ubdinit" },
#endif
#ifdef sun
#define	X_MBDINIT	2
	{ "_mbdinit" },
#endif
	{ "" },
};

dkinit()
{
	register int i;
	register char *cp;
	static int once = 0;
	static char buf[1024];

	if (once)
		return;
	once = 1;
	nlist("/vmunix", nlst);
	if (nlst[X_DK_NDRIVE].n_value == 0) {
		error("dk_ndrive undefined in kernel");
		return;
	}
	dk_ndrive = getw(nlst[X_DK_NDRIVE].n_value);
	if (dk_ndrive <= 0) {
		error("dk_ndrive=%d according to /vmunix", dk_ndrive);
		return;
	}
	dk_mspw = (float *)calloc(dk_ndrive, sizeof (float));
	lseek(kmem, nlst[X_DK_MSPW].n_value, L_SET);
	read(kmem, dk_mspw, dk_ndrive * sizeof (float));
	dr_name = (char **)calloc(dk_ndrive, sizeof (char *));
	dk_select = (int *)calloc(dk_ndrive, sizeof (int));
	for (cp = buf, i = 0; i < dk_ndrive; i++) {
		dr_name[i] = cp;
		sprintf(dr_name[i], "dk%d", i);
		cp += strlen(dr_name[i]) + 1;
		if (dk_mspw[i] != 0.0)
			dk_select[i] = 1;
	}
	read_names();
}

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

#define steal(where, var) \
	lseek(kmem, where, L_SET); read(kmem, &var, sizeof var);

#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>

read_names()
{
	static int once = 0;
	struct mba_device mdev;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *)&two_char;
	struct uba_device udev;
	struct uba_driver udrv;
	register struct mba_device *mp;
	register struct uba_device *up;

	mp = (struct mba_device *)nlst[X_MBDINIT].n_value;
	up = (struct uba_device *)nlst[X_UBDINIT].n_value;
	if (mp == 0 && up == 0) {
		error("Disk init info not in namelist\n");
		return;
	}
	if (mp) for (;;) {
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		sprintf(dr_name[mdev.mi_dk], "%c%c%d",
		    cp[0], cp[1], mdev.mi_unit);
	}
	if (up) for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		    cp[0], cp[1], udev.ui_unit);
	}
}
#endif

#ifdef sun
#include <sundev/mbvar.h>

read_names()
{
	static int once = 0;
	struct mb_device mdev;
	struct mb_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	register struct mb_device *mp;

	mp = (struct mb_device *)nlst[X_MBDINIT].n_value;
	if (mp == 0) {
		error("Disk init info not in namelist\n");
		return;
	}
	for (;;) {
		steal(mp++, mdev);
		if (mdev.md_driver == 0)
			break;
		if (mdev.md_dk < 0 || mdev.md_alive == 0)
			continue;
		steal(mdev.md_driver, mdrv);
		steal(mdrv.mdr_dname, two_char);
		sprintf(dr_name[mdev.md_dk], "%c%c%d",
		    cp[0], cp[1], mdev.md_unit);
	}
}
#endif

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
