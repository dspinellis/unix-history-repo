/*
 * Copyright (c) 1980 Regents of the University of California.
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

#ifndef lint
static char sccsid[] = "@(#)mkioconf.c	5.14 (Berkeley) 6/1/90";
#endif /* not lint */

#include <stdio.h>
#include "y.tab.h"
#include "config.h"

/*
 * build the ioconf.c file
 */
char	*qu();
char	*intv();

#if MACHINE_VAX
vax_ioconf()
{
	register struct device *dp, *mp, *np;
	register int uba_n, slave;
	FILE *fp;

	fp = fopen(path("ioconf.c"), "w");
	if (fp == 0) {
		perror(path("ioconf.c"));
		exit(1);
	}
	fprintf(fp, "#include \"machine/pte.h\"\n");
	fprintf(fp, "#include \"../sys/param.h\"\n");
	fprintf(fp, "#include \"../sys/buf.h\"\n");
	fprintf(fp, "#include \"../sys/map.h\"\n");
	fprintf(fp, "#include \"../sys/vm.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#include \"../vaxmba/mbavar.h\"\n");
	fprintf(fp, "#include \"../vaxuba/ubavar.h\"\n\n");
	fprintf(fp, "\n");
	fprintf(fp, "#define C (caddr_t)\n\n");
	/*
	 * First print the mba initialization structures
	 */
	if (seen_mba) {
		for (dp = dtab; dp != 0; dp = dp->d_next) {
			mp = dp->d_conn;
			if (mp == 0 || mp == TO_NEXUS ||
			    !eq(mp->d_name, "mba"))
				continue;
			fprintf(fp, "extern struct mba_driver %sdriver;\n",
			    dp->d_name);
		}
		fprintf(fp, "\nstruct mba_device mbdinit[] = {\n");
		fprintf(fp, "\t/* Device,  Unit, Mba, Drive, Dk */\n");
		for (dp = dtab; dp != 0; dp = dp->d_next) {
			mp = dp->d_conn;
			if (dp->d_unit == QUES || mp == 0 ||
			    mp == TO_NEXUS || !eq(mp->d_name, "mba"))
				continue;
			if (dp->d_addr) {
				printf("can't specify csr address on mba for %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_vec != 0) {
				printf("can't specify vector for %s%d on mba\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_drive == UNKNOWN) {
				printf("drive not specified for %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_slave != UNKNOWN) {
				printf("can't specify slave number for %s%d\n", 
				    dp->d_name, dp->d_unit);
				continue;
			}
			fprintf(fp, "\t{ &%sdriver, %d,   %s,",
				dp->d_name, dp->d_unit, qu(mp->d_unit));
			fprintf(fp, "  %s,  %d },\n",
				qu(dp->d_drive), dp->d_dk);
		}
		fprintf(fp, "\t0\n};\n\n");
		/*
		 * Print the mbsinit structure
		 * Driver Controller Unit Slave
		 */
		fprintf(fp, "struct mba_slave mbsinit [] = {\n");
		fprintf(fp, "\t/* Driver,  Ctlr, Unit, Slave */\n");
		for (dp = dtab; dp != 0; dp = dp->d_next) {
			/*
			 * All slaves are connected to something which
			 * is connected to the massbus.
			 */
			if ((mp = dp->d_conn) == 0 || mp == TO_NEXUS)
				continue;
			np = mp->d_conn;
			if (np == 0 || np == TO_NEXUS ||
			    !eq(np->d_name, "mba"))
				continue;
			fprintf(fp, "\t{ &%sdriver, %s",
			    mp->d_name, qu(mp->d_unit));
			fprintf(fp, ",  %2d,    %s },\n",
			    dp->d_unit, qu(dp->d_slave));
		}
		fprintf(fp, "\t0\n};\n\n");
	}
	/*
	 * Now generate interrupt vectors for the unibus
	 */
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		if (dp->d_vec != 0) {
			struct idlst *ip;
			mp = dp->d_conn;
			if (mp == 0 || mp == TO_NEXUS ||
			    (!eq(mp->d_name, "uba") && !eq(mp->d_name, "bi")))
				continue;
			fprintf(fp,
			    "extern struct uba_driver %sdriver;\n",
			    dp->d_name);
			fprintf(fp, "extern ");
			ip = dp->d_vec;
			for (;;) {
				fprintf(fp, "X%s%d()", ip->id, dp->d_unit);
				ip = ip->id_next;
				if (ip == 0)
					break;
				fprintf(fp, ", ");
			}
			fprintf(fp, ";\n");
			fprintf(fp, "int\t (*%sint%d[])() = { ", dp->d_name,
			    dp->d_unit);
			ip = dp->d_vec;
			for (;;) {
				fprintf(fp, "X%s%d", ip->id, dp->d_unit);
				ip = ip->id_next;
				if (ip == 0)
					break;
				fprintf(fp, ", ");
			}
			fprintf(fp, ", 0 } ;\n");
		}
	}
	fprintf(fp, "\nstruct uba_ctlr ubminit[] = {\n");
	fprintf(fp, "/*\t driver,\tctlr,\tubanum,\talive,\tintr,\taddr */\n");
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_type != CONTROLLER || mp == TO_NEXUS || mp == 0 ||
		    !eq(mp->d_name, "uba"))
			continue;
		if (dp->d_vec == 0) {
			printf("must specify vector for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_addr == 0) {
			printf("must specify csr address for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
			printf("drives need their own entries; dont ");
			printf("specify drive or slave for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_flags) {
			printf("controllers (e.g. %s%d) ",
			    dp->d_name, dp->d_unit);
			printf("don't have flags, only devices do\n");
			continue;
		}
		fprintf(fp,
		    "\t{ &%sdriver,\t%d,\t%s,\t0,\t%sint%d, C 0%o },\n",
		    dp->d_name, dp->d_unit, qu(mp->d_unit),
		    dp->d_name, dp->d_unit, dp->d_addr);
	}
	fprintf(fp, "\t0\n};\n");
/* unibus devices */
	fprintf(fp, "\nstruct uba_device ubdinit[] = {\n");
	fprintf(fp,
"\t/* driver,  unit, ctlr,  ubanum, slave,   intr,    addr,    dk, flags*/\n");
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_unit == QUES || dp->d_type != DEVICE || mp == 0 ||
		    mp == TO_NEXUS || mp->d_type == MASTER ||
		    eq(mp->d_name, "mba"))
			continue;
		np = mp->d_conn;
		if (np != 0 && np != TO_NEXUS && eq(np->d_name, "mba"))
			continue;
		np = 0;
		if (eq(mp->d_name, "uba")) {
			if (dp->d_vec == 0) {
				printf("must specify vector for device %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_addr == 0) {
				printf("must specify csr for device %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
				printf("drives/slaves can be specified ");
				printf("only for controllers, ");
				printf("not for device %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			uba_n = mp->d_unit;
			slave = QUES;
		} else {
			if ((np = mp->d_conn) == 0) {
				printf("%s%d isn't connected to anything ",
				    mp->d_name, mp->d_unit);
				printf(", so %s%d is unattached\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			uba_n = np->d_unit;
			if (dp->d_drive == UNKNOWN) {
				printf("must specify ``drive number'' ");
				printf("for %s%d\n", dp->d_name, dp->d_unit);
				continue;
			}
			/* NOTE THAT ON THE UNIBUS ``drive'' IS STORED IN */
			/* ``SLAVE'' AND WE DON'T WANT A SLAVE SPECIFIED */
			if (dp->d_slave != UNKNOWN) {
				printf("slave numbers should be given only ");
				printf("for massbus tapes, not for %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_vec != 0) {
				printf("interrupt vectors should not be ");
				printf("given for drive %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_addr != 0) {
				printf("csr addresses should be given only ");
				printf("on controllers, not on %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			slave = dp->d_drive;
		}
		fprintf(fp, "\t{ &%sdriver,  %2d,   %s,",
		    eq(mp->d_name, "uba") ? dp->d_name : mp->d_name, dp->d_unit,
		    eq(mp->d_name, "uba") ? " -1" : qu(mp->d_unit));
		fprintf(fp, "  %s,    %2d,   %s, C 0%-6o,  %d,  0x%x },\n",
		    qu(uba_n), slave, intv(dp), dp->d_addr, dp->d_dk,
		    dp->d_flags);
	}
	fprintf(fp, "\t0\n};\n");
	(void) fclose(fp);
}
#endif

#if MACHINE_TAHOE
tahoe_ioconf()
{
	register struct device *dp, *mp, *np;
	register int vba_n, slave;
	FILE *fp;

	fp = fopen(path("ioconf.c"), "w");
	if (fp == 0) {
		perror(path("ioconf.c"));
		exit(1);
	}
	fprintf(fp, "#include \"../sys/param.h\"\n");
	fprintf(fp, "#include \"machine/pte.h\"\n");
	fprintf(fp, "#include \"../sys/buf.h\"\n");
	fprintf(fp, "#include \"../sys/map.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#include \"../tahoevba/vbavar.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#define C (caddr_t)\n\n");
	/*
	 * Now generate interrupt vectors for the versabus
	 */
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (mp == 0 || mp == TO_NEXUS || !eq(mp->d_name, "vba"))
			continue;
		if (dp->d_vec != 0) {
			struct idlst *ip;
			fprintf(fp,
			    "extern struct vba_driver %sdriver;\n",
			    dp->d_name);
			fprintf(fp, "extern ");
			ip = dp->d_vec;
			for (;;) {
				fprintf(fp, "X%s%d()", ip->id, dp->d_unit);
				ip = ip->id_next;
				if (ip == 0)
					break;
				fprintf(fp, ", ");
			}
			fprintf(fp, ";\n");
			fprintf(fp, "int\t (*%sint%d[])() = { ", dp->d_name,
			    dp->d_unit);
			ip = dp->d_vec;
			for (;;) {
				fprintf(fp, "X%s%d", ip->id, dp->d_unit);
				ip = ip->id_next;
				if (ip == 0)
					break;
				fprintf(fp, ", ");
			}
			fprintf(fp, ", 0 } ;\n");
		} else if (dp->d_type == DRIVER)  /* devices w/o interrupts */
			fprintf(fp,
			    "extern struct vba_driver %sdriver;\n",
			    dp->d_name);
	}
	fprintf(fp, "\nstruct vba_ctlr vbminit[] = {\n");
	fprintf(fp, "/*\t driver,\tctlr,\tvbanum,\talive,\tintr,\taddr */\n");
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_type != CONTROLLER || mp == TO_NEXUS || mp == 0 ||
		    !eq(mp->d_name, "vba"))
			continue;
		if (dp->d_vec == 0) {
			printf("must specify vector for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_addr == 0) {
			printf("must specify csr address for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
			printf("drives need their own entries; dont ");
			printf("specify drive or slave for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_flags) {
			printf("controllers (e.g. %s%d) ",
			    dp->d_name, dp->d_unit);
			printf("don't have flags, only devices do\n");
			continue;
		}
		fprintf(fp,
		    "\t{ &%sdriver,\t%d,\t%s,\t0,\t%sint%d, C 0x%x },\n",
		    dp->d_name, dp->d_unit, qu(mp->d_unit),
		    dp->d_name, dp->d_unit, dp->d_addr);
	}
	fprintf(fp, "\t0\n};\n");
/* versabus devices */
	fprintf(fp, "\nstruct vba_device vbdinit[] = {\n");
	fprintf(fp,
"\t/* driver,  unit, ctlr,  vbanum, slave,   intr,    addr,    dk, flags*/\n");
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_unit == QUES || dp->d_type != DEVICE || mp == 0 ||
		    mp == TO_NEXUS || mp->d_type == MASTER ||
		    eq(mp->d_name, "mba"))
			continue;
		np = mp->d_conn;
		if (np != 0 && np != TO_NEXUS && eq(np->d_name, "mba"))
			continue;
		np = 0;
		if (eq(mp->d_name, "vba")) {
			if (dp->d_vec == 0)
				printf(
		"Warning, no interrupt vector specified for device %s%d\n",
				    dp->d_name, dp->d_unit);
			if (dp->d_addr == 0) {
				printf("must specify csr for device %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
				printf("drives/slaves can be specified ");
				printf("only for controllers, ");
				printf("not for device %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			vba_n = mp->d_unit;
			slave = QUES;
		} else {
			if ((np = mp->d_conn) == 0) {
				printf("%s%d isn't connected to anything ",
				    mp->d_name, mp->d_unit);
				printf(", so %s%d is unattached\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			vba_n = np->d_unit;
			if (dp->d_drive == UNKNOWN) {
				printf("must specify ``drive number'' ");
				printf("for %s%d\n", dp->d_name, dp->d_unit);
				continue;
			}
			/* NOTE THAT ON THE UNIBUS ``drive'' IS STORED IN */
			/* ``SLAVE'' AND WE DON'T WANT A SLAVE SPECIFIED */
			if (dp->d_slave != UNKNOWN) {
				printf("slave numbers should be given only ");
				printf("for massbus tapes, not for %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_vec != 0) {
				printf("interrupt vectors should not be ");
				printf("given for drive %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_addr != 0) {
				printf("csr addresses should be given only ");
				printf("on controllers, not on %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			slave = dp->d_drive;
		}
		fprintf(fp, "\t{ &%sdriver,  %2d,   %s,",
		    eq(mp->d_name, "vba") ? dp->d_name : mp->d_name, dp->d_unit,
		    eq(mp->d_name, "vba") ? " -1" : qu(mp->d_unit));
		fprintf(fp, "  %s,    %2d,   %s, C 0x%-6x,  %d,  0x%x },\n",
		    qu(vba_n), slave, intv(dp), dp->d_addr, dp->d_dk,
		    dp->d_flags);
	}
	fprintf(fp, "\t0\n};\n");
	(void) fclose(fp);
}
#endif

#if MACHINE_HP300
hp300_ioconf()
{
	register struct device *dp, *mp, *np;
	register int hpib, slave;
	FILE *fp;
	extern char *wnum();

	fp = fopen(path("ioconf.c"), "w");
	if (fp == 0) {
		perror(path("ioconf.c"));
		exit(1);
	}
	fprintf(fp, "#include \"machine/pte.h\"\n");
	fprintf(fp, "#include \"../sys/param.h\"\n");
	fprintf(fp, "#include \"../sys/buf.h\"\n");
	fprintf(fp, "#include \"../sys/map.h\"\n");
	fprintf(fp, "#include \"../sys/vm.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#include \"../hpdev/device.h\"\n\n");
	fprintf(fp, "\n");
	fprintf(fp, "#define C (caddr_t)\n");
	fprintf(fp, "#define D (struct driver *)\n\n");
	/*
	 * First print the hpib controller initialization structures
	 */
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_unit == QUES || mp == 0)
			continue;
		fprintf(fp, "extern struct driver %sdriver;\n", dp->d_name);
	}
	fprintf(fp, "\nstruct hp_ctlr hp_cinit[] = {\n");
	fprintf(fp, "/*\tdriver,\t\tunit,\talive,\taddr,\tflags */\n");
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_unit == QUES ||
			dp->d_type != MASTER && dp->d_type != CONTROLLER)
			continue;
		if (mp != TO_NEXUS) {
			printf("%s%s must be attached to an sc (nexus)\n",
				dp->d_name, wnum(dp->d_unit));
			continue;
		}
		if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
			printf("can't specify drive/slave for %s%s\n",
				dp->d_name, wnum(dp->d_unit));
			continue;
		}
		fprintf(fp,
			"\t{ &%sdriver,\t%d,\t0,\tC 0x%x,\t0x%x },\n",
			dp->d_name, dp->d_unit, dp->d_addr, dp->d_flags);
	}
	fprintf(fp, "\t0\n};\n");
/* devices */
	fprintf(fp, "\nstruct hp_device hp_dinit[] = {\n");
	fprintf(fp,
	   "/*driver,\tcdriver,\tunit,\tctlr,\tslave,\taddr,\tdk,\tflags*/\n");
	for (dp = dtab; dp != 0; dp = dp->d_next) {
		mp = dp->d_conn;
		if (mp == 0 || dp->d_type != DEVICE || hpbadslave(mp, dp))
			continue;
		if (mp == TO_NEXUS) {
			if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
				printf("can't specify drive/slave for %s%s\n",
					dp->d_name, wnum(dp->d_unit));
				continue;
			}
			slave = QUES;
			hpib = QUES;
		} else {
			if (dp->d_addr != 0) {
				printf("can't specify sc for device %s%s\n",
					dp->d_name, wnum(dp->d_unit));
				continue;
			}
			if (mp->d_type == CONTROLLER) {
				if (dp->d_drive == UNKNOWN) {
					printf("must specify drive for %s%s\n",
						dp->d_name, wnum(dp->d_unit));
					continue;
				}
				slave = dp->d_drive;
			} else {
				if (dp->d_slave == UNKNOWN) {
					printf("must specify slave for %s%s\n",
						dp->d_name, wnum(dp->d_unit));
					continue;
				}
				slave = dp->d_slave;
			}
			hpib = mp->d_unit;
		}
		fprintf(fp, "{ &%sdriver,\t", dp->d_name);
		if (mp == TO_NEXUS)
			fprintf(fp, "D 0x0,\t");
		else
			fprintf(fp, "&%sdriver,", mp->d_name);
		fprintf(fp, "\t%d,\t%d,\t%d,\tC 0x%x,\t%d,\t0x%x },\n",
			dp->d_unit, hpib, slave,
			dp->d_addr, dp->d_dk, dp->d_flags);
	}
	fprintf(fp, "0\n};\n");
	(void) fclose(fp);
}

#define ishpibdev(n) (eq(n,"rd") || eq(n,"ct") || eq(n,"mt") || eq(n,"ppi"))
#define isscsidev(n) (eq(n,"sd") || eq(n,"st"))

hpbadslave(mp, dp)
	register struct device *dp, *mp;
{
	extern char *wnum();

	if (mp == TO_NEXUS && ishpibdev(dp->d_name) ||
	    mp != TO_NEXUS && eq(mp->d_name, "hpib") &&
	    !ishpibdev(dp->d_name)) {
		printf("%s%s must be attached to an hpib\n",
		       dp->d_name, wnum(dp->d_unit));
		return (1);
	}
	if (mp == TO_NEXUS && isscsidev(dp->d_name) ||
	    mp != TO_NEXUS && eq(mp->d_name, "scsi") &&
	    !isscsidev(dp->d_name)) {
		printf("%s%s must be attached to a scsi\n",
		       dp->d_name, wnum(dp->d_unit));
		return (1);
	}
	return (0);
}

char *
wnum(num)
{

	if (num == QUES || num == UNKNOWN)
		return ("?");
	(void) sprintf(errbuf, "%d", num);
	return (errbuf);
}
#endif

char *
intv(dev)
	register struct device *dev;
{
	static char buf[20];

	if (dev->d_vec == 0)
		return ("     0");
	(void) sprintf(buf, "%sint%d", dev->d_name, dev->d_unit);
	return (buf);
}

char *
qu(num)
{

	if (num == QUES)
		return ("'?'");
	if (num == UNKNOWN)
		return (" -1");
	(void) sprintf(errbuf, "%3d", num);
	return (errbuf);
}
