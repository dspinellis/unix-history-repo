/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fsinfo.c	5.4 (Berkeley) %G%
 *
 * $Id: fsinfo.c,v 5.2.2.1 1992/02/09 15:09:33 jsp beta $
 *
 */

/*
 * fsinfo
 */

#include "../fsinfo/fsinfo.h"
#include "fsi_gram.h"
#include <pwd.h>

qelem *list_of_hosts;
qelem *list_of_automounts;
dict *dict_of_volnames;
dict *dict_of_hosts;
char *autodir = "/a";
char hostname[MAXHOSTNAMELEN+1];
char *username;
int file_io_errors;
int parse_errors;
int errors;
int verbose;
char idvbuf[1024];

char **g_argv;
char *progname;

/*
 * Output file prefixes
 */
char *exportfs_pref;
char *fstab_pref;
char *dumpset_pref;
char *mount_pref;
char *bootparams_pref;

/*
 * Argument cracking...
 */
static void get_args(c, v)
int c;
char *v[];
{
	extern char *optarg;
	extern int optind;
	int ch;
	int usage = 0;
	char *iptr = idvbuf;

	/*
	 * Determine program name
	 */
	if (v[0]) {
		progname = strrchr(v[0], '/');
		if (progname && progname[1])
			progname++;
		else
			progname = v[0];
	}
	if (!progname)
		progname = "fsinfo";

	while ((ch = getopt(c, v, "a:b:d:e:f:h:m:D:U:I:qv")) != EOF)
	switch (ch) {
	case 'a':
		autodir = optarg;
		break;
	case 'b':
		if (bootparams_pref)
			fatal("-b option specified twice");
		bootparams_pref = optarg;
		break;
	case 'd':
		if (dumpset_pref)
			fatal("-d option specified twice");
		dumpset_pref = optarg;
		break;
	case 'h':
		strncpy(hostname, optarg, sizeof(hostname)-1);
		break;
	case 'e':
		if (exportfs_pref)
			fatal("-e option specified twice");
		exportfs_pref = optarg;
		break;
	case 'f':
		if (fstab_pref)
			fatal("-f option specified twice");
		fstab_pref = optarg;
		break;
	case 'm':
		if (mount_pref)
			fatal("-m option specified twice");
		mount_pref = optarg;
		break;
	case 'q':
		verbose = -1;
		break;
	case 'v':
		verbose = 1;
		break;
	case 'I': case 'D': case 'U':
		sprintf(iptr, "-%c%s ", ch, optarg);
		iptr += strlen(iptr);
		break;
	default:
		usage++;
		break;
	}

	if (c != optind) {
		g_argv = v + optind - 1;
		if (yywrap())
			fatal("Cannot read any input files");
	} else {
		usage++;
	}

	if (usage) {
		fprintf(stderr,
"\
Usage: %s [-v] [-a autodir] [-h hostname] [-b bootparams] [-d dumpsets]\n\
\t[-e exports] [-f fstabs] [-m automounts]\n\
\t[-I dir] [-D|-U string[=string]] config ...\n", progname);
		exit(1);
	}


	if (g_argv[0])
		log("g_argv[0] = %s", g_argv[0]);
	else
		log("g_argv[0] = (nil)");
}

/*
 * Determine username of caller
 */
static char *find_username()
{
	extern char *getlogin();
	extern char *getenv();
	char *u = getlogin();
	if (!u) {
		struct passwd *pw = getpwuid(getuid());
		if (pw)
			u = pw->pw_name;
	}
	if (!u)
		u = getenv("USER");
	if (!u)
		u = getenv("LOGNAME");
	if (!u)
		u = "root";

	return strdup(u);
}

/*
 * MAIN
 */
main(argc, argv)
int argc;
char *argv[];
{
	/*
	 * Process arguments
	 */
	get_args(argc, argv);

	/*
	 * If no hostname given then use the local name
	 */
	if (!*hostname && gethostname(hostname, sizeof(hostname)) < 0) {
		perror("gethostname");
		exit(1);
	}

	/*
	 * Get the username
	 */
	username = find_username();

	/*
	 * New hosts and automounts
	 */
	list_of_hosts = new_que();
	list_of_automounts = new_que();

	/*
	 * New dictionaries
	 */
	dict_of_volnames = new_dict();
	dict_of_hosts = new_dict();

	/*
	 * Parse input
	 */
	show_area_being_processed("read config", 11);
	if (yyparse())
		errors = 1;
	errors += file_io_errors + parse_errors;

	if (errors == 0) {
		/*
		 * Do semantic analysis of input
		 */
		analyze_hosts(list_of_hosts);
		analyze_automounts(list_of_automounts);
	}

	/*
	 * Give up if errors
	 */
	if (errors == 0) {
		/*
		 * Output data files
		 */

		write_atab(list_of_automounts);
		write_bootparams(list_of_hosts);
		write_dumpset(list_of_hosts);
		write_exportfs(list_of_hosts);
		write_fstab(list_of_hosts);
	}

	col_cleanup(1);

	exit(errors);
}
