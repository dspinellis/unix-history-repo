#ifndef lint
static char sccsid[] = "@(#)disks.c	1.1 (Lucasfilm) %G%";
#endif

#include "systat.h"

#include <sys/buf.h>
#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>
#endif
#ifdef sun
#include <sundev/mbvar.h>
#endif

char dr_name[DK_NDRIVE][10];

#define steal(where, var) \
	lseek(kmem, where, L_SET); read(kmem, &var, sizeof var);

#ifdef vax
read_names(mp, up)
	register struct mba_device *mp;
	register struct uba_device *up;
{
	static int once = 0;
	struct mba_device mdev;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *)&two_char;
	struct uba_device udev;
	struct uba_driver udrv;

	if (once)
		return;
	once++;
	if (up == 0) {
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
/*VARARGS*/
read_names(mp)
	register struct mb_device *mp;
{
	static int once = 0;
	struct mb_device mdev;
	struct mb_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;

	if (once)
		return;
	once++;
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
		for (i = 0; i < DK_NDRIVE; i++)
			if (strcmp(args, dr_name[i]) == 0) {
				selections[i] = truefalse;
				if (dk_mspw[i] != 0.0)
					if (truefalse == 0)
						ndrives--;
					else
						ndrives++;
				break;
			}
		if (i >= DK_NDRIVE)
			error("%s: unknown drive", args);
		args = cp;
	}
}
