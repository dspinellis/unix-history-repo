#ifndef lint
static char sccsid[] = "@(#)disklabel.c	1.2 (Berkeley/CCI) 6/7/88";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

int	lab_help();

/*
**
*/

get_drive_type(ctlr, drive, op_mask)
int	ctlr, drive, op_mask;
{
	int	tokens[20];
	char	line[132];
	int	savedevflags = 0;
	register struct disklabel *lp;
	struct disklabel *plp, *getdiskbyname(), *promptfordisk(), *findproto();

	lp = lab;
	if (lp->d_typename[0] == 0) {
		print("Read label from drive %d, controller %d? ", drive, ctlr);
		get_string_cmd(line, lab_help);
		if (kill_processes == true)
			return;
		if (line[0] == 'y' || line[0] == 'Y') {
			lp->d_secsize = 512;
			lp->d_nsectors = 66;
			lp->d_ntracks = 23;
			lp->d_ncylinders = 850;
			lp->d_secpercyl = 66*23;
			if (D_INFO->alive != u_true)
				spin_up_drive();
			savedevflags = lab->d_devflags;
			if (readlabel()) {
				lp->d_devflags = savedevflags;
				lp->d_pat = 0;	/* this can't be what we want */
				goto check;
			}
			lab->d_devflags = savedevflags;
			lp->d_typename[0] = 0;
		}
	}
	for (;;) {
		print("Drive type for controller %d, drive %d? ", ctlr, drive);
		if (lp->d_typename[0] != 0)
			printf("(%s) ", lp->d_typename);
		get_string_cmd(line, lab_help);
		if (kill_processes == true)
			return;
		if (lp->d_typename[0] != 0 &&
		    (line[0] == 0 || strcmp(lp->d_typename, line) == 0))
			break;
		if (lp = findproto(line))
			break;;
		if (lp = getdiskbyname(line))
			break;
		if (lp = promptfordisk(line))
			break;
		if (kill_processes == true)
			return;
		lp = lab;
	}
check:
	plp = findproto(lp->d_typename);
	while (op_mask & FORMAT_OP && lp->d_traksize == 0) {
		print("number of bytes per track");
		if (plp && plp->d_traksize)
			printf(" (%d)", plp->d_traksize);
		printf(": ");
		get_string_cmd(line, lab_help);
		if (kill_processes == true)
			return;
		if (line[0] == 0) {
			if (plp->d_traksize == 0)
				print("no default value\n");
			lp->d_traksize = plp->d_traksize;
		} else
			lp->d_traksize = atol(line);
	}
	print("Drive geometry for controller %d, drive %d (%s):\n",
	    ctlr, drive, lp->d_typename);
	print("  sector size %d; %d sectors, %d tracks, %d cylinders\n",
	    lp->d_secsize, lp->d_nsectors, lp->d_ntracks, lp->d_ncylinders);
	if (lp->d_pat == 0 && op_mask & (FORMAT_OP | VERIFY_OP)) {
		extern struct flawpat defpats, cdcpats;

		print("media patterns for verify (default or cdc): ");
		get_string_cmd(line, lab_help);
		if (kill_processes == true)
			return;
		if (strcmp(line, "cdc") == 0)
			lp->d_pat = (long) &cdcpats;
		else
			lp->d_pat = (long) &defpats;
	}
	if (lab->d_cylskew == -1)
		lab->d_cylskew = 0;
	if (lab->d_trackskew == -1)
		lab->d_trackskew = 0;
	if (lab->d_sparespertrack == -1)
		lab->d_sparespertrack = 0;
	if (lp != lab) {
		*lab = *lp;
		if (savedevflags)
			lab->d_devflags = savedevflags;
	}
	configure_drive(1);		/* set new parameters */
}

struct disklabel *
findproto(name)
	char *name;
{
	int count;

	if (C_INFO->type == VDTYPE_VDDC)
		count = smddrives;
	else
		count = 0;
	for (; count < ndrives; count++)
		if (strcmp(vdproto[count].d_typename, name) == 0)
			return (&vdproto[count]);
	return ((struct disklabel *) 0);
}

struct disklabel disk;

struct	field {
	char	*f_name;
	char	*f_defaults;
	u_long	*f_location;
} fields[] = {
	{ "sector size",		"512",	&disk.d_secsize },
	{ "#sectors/track",		0,	&disk.d_nsectors },
	{ "#tracks/cylinder",		0,	&disk.d_ntracks },
	{ "#cylinders",			0,	&disk.d_ncylinders },
	{ "#bytes/track",		0,	&disk.d_traksize },
	{ 0, 0, 0 },
};

struct disklabel *
promptfordisk(name)
	char *name;
{
	register struct disklabel *dp = &disk;
	register struct field *fp;
	register i;
	char buf[132], *cp;

	print("%s: unknown drive type\n", name);
	if (get_yes_no("Enter drive parameters") == false)
		return ((struct disklabel *)0);

	strncpy(dp->d_typename, name, sizeof(dp->d_typename));
	dp->d_type = DTYPE_SMD;
	dp->d_flags = 0;

	print("(type <cr> to get default value, if only one)\n");
	for (fp = fields; fp->f_name != NULL; fp++) {
again:
		print("%s ", fp->f_name);
		if (fp->f_defaults != NULL)
			printf("(%s)", fp->f_defaults);
		printf("? ");
		get_string_cmd(buf, lab_help);
		if (kill_processes == true)
			return ((struct disklabel *)0);
		cp = buf;
		if (*cp == '\0') {
			if (fp->f_defaults == NULL) {
				print("no default value\n");
				goto again;
			}
			cp = fp->f_defaults;
		}
		*fp->f_location = atol(cp);
		if (*fp->f_location == 0) {
			print("%s: bad value\n", cp);
			goto again;
		}
	}
	print("sectors/cylinder (%d)? ", dp->d_nsectors * dp->d_ntracks);
	get_string_cmd(buf, lab_help);
	if (kill_processes == true)
		return ((struct disklabel *)0);
	if (buf[0] == 0)
		dp->d_secpercyl = dp->d_nsectors * dp->d_ntracks;
	else
		dp->d_secpercyl = atol(buf);
	return (dp);
}

lab_help()
{
	indent();
	print("Entering drive type and parameters:\n");
	indent();
	print("Answer each question with a number or name, as appropriate.\n");
	print("Questions with defaults show them in (parentheses);\n");
	print("press return to accept the default.\n\n");
	exdent(1);
	print("Other commands available:\n");
	indent();
	print("QUIT     - abort current operation\n");
	exdent(2);
}

static char labelsector[VD_MAXSECSIZE];
/*
 * Fetch disklabel for disk.
 */
readlabel()
{
	register struct disklabel *lp;

	bzero(labelsector, sizeof(labelsector));
	if (vread(LABELSECTOR, labelsector, 1) < 1)
		return (0);
	for (lp = (struct disklabel *)labelsector;
	    lp <= (struct disklabel *)(labelsector+VD_MAXSECSIZE - sizeof(*lp));
	    lp = (struct disklabel *)((char *)lp + 16))
		if (lp->d_magic == DISKMAGIC &&
		    lp->d_magic2 == DISKMAGIC)
			break;
	if (lp > (struct disklabel *)(labelsector+VD_MAXSECSIZE-sizeof(*lp)) ||
	    lp->d_magic != DISKMAGIC || lp->d_magic2 != DISKMAGIC ||
	    dkcksum(lp) != 0) {
		print("Disk is unlabeled.\n");
		return (0);
	}
	*lab = *lp;
	return (1);
}

writelabel()
{
	register struct disklabel *lp = lab;

	lp->d_magic = DISKMAGIC;
	lp->d_magic2 = DISKMAGIC;
	lp->d_checksum = 0;
	lp->d_checksum = dkcksum(lp);
	if (vwrite(LABELSECTOR, labelsector, 1) != 1)
		printf("error writing disk label\n");
}
