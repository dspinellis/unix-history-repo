/*	mkioconf.c	2.4	82/10/24	*/

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
	if (fp == NULL) {
		perror(path("ioconf.c"));
		exit(1);
	}
	fprintf(fp, "#include \"../h/param.h\"\n");
	fprintf(fp, "#include \"../h/pte.h\"\n");
	fprintf(fp, "#include \"../h/buf.h\"\n");
	fprintf(fp, "#include \"../h/map.h\"\n");
	fprintf(fp, "#include \"../h/vm.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#include \"../vaxmba/mbavar.h\"\n");
	fprintf(fp, "#include \"../vaxuba/ubavar.h\"\n\n");
	fprintf(fp, "\n");
	fprintf(fp, "#define C (caddr_t)\n\n");
	/*
	 * First print the mba initialization structures
	 */
	if (seen_mba) {
		for (dp = dtab; dp != NULL; dp = dp->d_next) {
			mp = dp->d_conn;
			if (mp == NULL || mp == TO_NEXUS ||
			    !eq(mp->d_name, "mba"))
				continue;
			fprintf(fp, "extern struct mba_driver %sdriver;\n",
			    dp->d_name);
		}
		fprintf(fp, "\nstruct mba_device mbdinit[] = {\n");
		fprintf(fp, "\t/* Device,  Unit, Mba, Drive, Dk */\n");
		for (dp = dtab; dp != NULL; dp = dp->d_next) {
			mp = dp->d_conn;
			if (dp->d_unit == QUES || mp == NULL ||
			    mp == TO_NEXUS || !eq(mp->d_name, "mba"))
				continue;
			if (dp->d_addr) {
				printf("can't specify csr address on mba for %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_vec != NULL) {
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
			fprintf(fp,
				"\t{ &%sdriver, %d,   %s,  %s,    %d },\n",
				dp->d_name, dp->d_unit, qu(mp->d_unit),
				qu(dp->d_drive), dp->d_dk);
		}
		fprintf(fp, "\t0\n};\n\n");
		/*
		 * Print the mbsinit structure
		 * Driver Controller Unit Slave
		 */
		fprintf(fp, "struct mba_slave mbsinit [] = {\n");
		fprintf(fp, "\t/* Driver,  Ctlr, Unit, Slave */\n");
		for (dp = dtab; dp != NULL; dp = dp->d_next) {
			/*
			 * All slaves are connected to something which
			 * is connected to the massbus.
			 */
			if ((mp = dp->d_conn) == NULL || mp == TO_NEXUS)
				continue;
			np = mp->d_conn;
			if (np == NULL || np == TO_NEXUS ||
			    !eq(np->d_name, "mba"))
				continue;
			fprintf(fp,
			    "\t{ &%sdriver, %s,  %2d,    %s },\n",
			    mp->d_name, qu(mp->d_unit), dp->d_unit,
			    qu(dp->d_slave));
		}
		fprintf(fp, "\t0\n};\n\n");
	}
	/*
	 * Now generate interrupt vectors for the unibus
	 */
	for (dp = dtab; dp != NULL; dp = dp->d_next) {
		if (dp->d_vec != NULL) {
			struct idlst *ip;
			mp = dp->d_conn;
			if (mp == NULL || mp == TO_NEXUS ||
			    !eq(mp->d_name, "uba"))
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
			    dp->d_unit, dp->d_unit);
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
	for (dp = dtab; dp != NULL; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_type != CONTROLLER || mp == TO_NEXUS || mp == NULL ||
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
	for (dp = dtab; dp != NULL; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_unit == QUES || dp->d_type != DEVICE || mp == NULL ||
		    mp == TO_NEXUS || mp->d_type == MASTER ||
		    eq(mp->d_name, "mba"))
			continue;
		np = mp->d_conn;
		if (np != NULL && np != TO_NEXUS && eq(np->d_name, "mba"))
			continue;
		np = NULL;
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
			if ((np = mp->d_conn) == NULL) {
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
		fprintf(fp,
"\t{ &%sdriver,  %2d,   %s,  %s,    %2d,   %s, C 0%-6o,  %d,  0x%x },\n",
		    eq(mp->d_name, "uba") ? dp->d_name : mp->d_name, dp->d_unit,
		    eq(mp->d_name, "uba") ? " -1" : qu(mp->d_unit), qu(uba_n),
		    slave, intv(dp), dp->d_addr, dp->d_dk, dp->d_flags);
	}
	fprintf(fp, "\t0\n};\n");
	fclose(fp);
}
#endif

#if MACHINE_SUN
sun_ioconf()
{
	register struct device *dp, *mp, *np;
	register int uba_n, slave;
	FILE *fp;

	fp = fopen(path("ioconf.c"), "w");
	if (fp == NULL) {
		perror(path("ioconf.c"));
		exit(1);
	}
	fprintf(fp, "#include \"../h/param.h\"\n");
	fprintf(fp, "#include \"../h/buf.h\"\n");
	fprintf(fp, "#include \"../h/map.h\"\n");
	fprintf(fp, "#include \"../h/vm.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#include \"../sunmb/mbvar.h\"\n");
	fprintf(fp, "\n");
	fprintf(fp, "#define C (caddr_t)\n\n");
	fprintf(fp, "\n");
	/*
	 * Now generate interrupt vectors for the Multibus
	 */
	for (dp = dtab; dp != NULL; dp = dp->d_next) {
		if (dp->d_pri != NULL) {
			mp = dp->d_conn;
			if (mp == NULL || mp == TO_NEXUS ||
			    !eq(mp->d_name, "mb"))
				continue;
			fprintf(fp, "extern struct mb_driver %sdriver;\n",
			    dp->d_name);
		}
	}
	/*
	 * Now spew forth the mb_cinfo structure
	 */
	fprintf(fp, "\nstruct mb_ctlr mbcinit[] = {\n");
	fprintf(fp, "/*\t driver,\tctlr,\talive,\taddr,\tintpri */\n");
	for (dp = dtab; dp != NULL; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_type != CONTROLLER || mp == TO_NEXUS || mp == NULL ||
		    !eq(mp->d_name, "mb"))
			continue;
		if (dp->d_pri == 0) {
			printf("must specify priority for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_addr == 0) {
			printf("must specify csr address for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_drive != UNKNOWN || dp->d_slave != UNKNOWN) {
			printf("drives need their own entries; ");
			printf("dont specify drive or slave for %s%d\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		if (dp->d_flags) {
			printf("controllers (e.g. %s%d) don't have flags, ");
			printf("only devices do\n",
			    dp->d_name, dp->d_unit);
			continue;
		}
		fprintf(fp, "\t{ &%sdriver,\t%d,\t0,\tC 0x%x,\t%d },\n",
		    dp->d_name, dp->d_unit, dp->d_addr, dp->d_pri);
	}
	fprintf(fp, "\t0\n};\n");
	/*
	 * Now we go for the mb_device stuff
	 */
	fprintf(fp, "\nstruct mb_device mbdinit[] = {\n");
	fprintf(fp,
"\t/* driver,  unit, ctlr,  slave,   addr,    pri,    dk, flags*/\n");
	for (dp = dtab; dp != NULL; dp = dp->d_next) {
		mp = dp->d_conn;
		if (dp->d_unit == QUES || dp->d_type != DEVICE || mp == NULL ||
		    mp == TO_NEXUS || mp->d_type == MASTER ||
		    eq(mp->d_name, "mba"))
			continue;
		np = NULL;
		if (eq(mp->d_name, "mb")) {
			if (dp->d_pri == 0) {
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
				printf("drives/slaves can be specified only ");
				printf("for controllers, not for device %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			slave = QUES;
		} else {
			if ((np = mp->d_conn) == NULL) {
				printf("%s%d isn't connected to anything, ",
				    mp->d_name, mp->d_unit);
				printf("so %s%d is unattached\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_drive == UNKNOWN) {
				printf("must specify ``drive number'' for %s%d\n",
				   dp->d_name, dp->d_unit);
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
			if (dp->d_pri != 0) {
				printf("interrupt priority should not be ");
				printf("given for drive %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			if (dp->d_addr != 0) {
				printf("csr addresses should be given only");
				printf("on controllers, not on %s%d\n",
				    dp->d_name, dp->d_unit);
				continue;
			}
			slave = dp->d_drive;
		}
		fprintf(fp,
"\t{ &%sdriver,  %2d,   %s,    %2d,   C 0x%x, %d,  %d,  0x%x },\n",
		    eq(mp->d_name, "mb") ? dp->d_name : mp->d_name, dp->d_unit,
		    eq(mp->d_name, "mb") ? " -1" : qu(mp->d_unit),
		    slave, dp->d_addr, dp->d_pri, dp->d_dk, dp->d_flags);
	}
	fprintf(fp, "\t0\n};\n");
	fclose(fp);
}
#endif

char *intv(dev)
	register struct device *dev;
{
	static char buf[20];

	if (dev->d_vec == 0)
		return ("     0");
	else
		return (sprintf(buf, "%sint%d", dev->d_name, dev->d_unit));
}

char *
qu(num)
{

	if (num == QUES)
		return ("'?'");
	else if (num == UNKNOWN)
		return (" -1");
	else
		return (ns(sprintf(errbuf, "%3d", num)));
}
