#include "uucp.h"
#include "uucpdefs.h"
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

int Stop = 0;

/*******
 *
 *	uulog  -  this program will append all update files in
 *	directory (LOGDIR) to the log file (logf) and remove the
 *	update files.
 *
 *	options:
 *		-n  -  nominal time for delete of lock file
 *		-s  -  system name for search
 *		-u  -  user name for search
 *		-x  -  turn on debug outputs
 *
 *	exit codes:
 *		0  -  normal
 *		1  -  lock file problems
 *
 */


#define NOMTIME 3600L


main(argc, argv)
char *argv[];
{
	FILE *plogf, *lsp;
	char filename[NAMESIZE];
	time_t nomtime;
	char *system, *user;

	extern int onintr(), intr1();
	char buf[BUFSIZ], u[20], s[20];

	nomtime = NOMTIME;
	system = user = NULL;


	while (argc>1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'd':
			printf("-d option removed\n");
			break;
		case 'n':
			nomtime = atoi(&argv[1][2]); break;
		case 's':
			system = &argv[1][2];
			break;
		case 'u':
			user = &argv[1][2];
			break;
		case 'x':
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			break;
		default:
			printf("unknown flag %s\n", argv[1]); break;
		}
		--argc;  argv++;
	}

	DEBUG(4, "%s\n", "START");
	chdir(LOGDIR);
	if (ulockf(LOGLOCK, nomtime) != 0)
		exit(0);
	signal(SIGHUP, intr1);
	signal(SIGINT,intr1);
	signal(SIGQUIT, intr1);

	if ((plogf = fopen(LOGFILE, "a")) == NULL) {
		rmlock(LOGLOCK);
		printf("can't open %s\n", LOGFILE);
		exit(0);
	}
	lsp = fopen(LOGDIR, "r");
	ASSERT(lsp != NULL, "CAN NOT OPEN %s", LOGDIR);
	while ((gnamef(lsp, filename)) != 0) {
		DEBUG(4, "file-%s\n", filename);
		if (prefix(LOGPREFIX, filename)) {
			DEBUG(4, "copy file %s\n", filename);
			if (appendf(plogf, filename) == SUCCESS) {
				unlink(filename);
			}
		}
	}
	fclose(lsp);
	fclose(plogf);
	chmod(LOGFILE, 0666);
	rmlock(NULL);
	if (user == NULL && system == NULL)
		exit(0);
	if (Stop)
		exit(0);
	signal(SIGHUP, onintr);
	signal(SIGINT, onintr);
	signal(SIGQUIT, onintr);

	plogf = fopen(LOGFILE, "r");
	ASSERT(plogf != NULL, "CAN NOT OPEN %s", LOGFILE);
	while (fgets(buf, BUFSIZ, plogf) != NULL) {
		sscanf(buf, "%s%s", u, s);
		DEBUG(4, "u s %s ", u);
		DEBUG(4, "%s  ", s);
		DEBUG(4, "%s", buf);
		if (user != NULL && !prefix(user, u))
			continue;
		if (system != NULL && !prefix(system, s))
			continue;
		fputs(buf, stdout);
	}
	exit(0);
}




/***
 *	onintr()
 *
 *	onintr  -  interrupt routine
 *		remove lock file
 *
 */

onintr()
{
	rmlock(NULL);
	exit(0);
}


intr1()
{
	signal(SIGINT, intr1);
	signal(SIGHUP, intr1);
	signal(SIGQUIT, intr1);
	Stop = 1;
	return;
}

cleanup(code)
int code;
{
	exit(code);
}


/*******
 *	appendf(fp, entryf)	append file (entryf) to fp file
 *	FILE *fp;
 *	char *entryf;
 *
 *	return codes:
		SUCCESS - ok
 *		FAIL - file not readable
 */

appendf(fp, entryf)
FILE *fp;
char *entryf;
{
	FILE *pentryf;
	char ltext[513];

	if ((pentryf = fopen(entryf, "r")) == NULL) {
		/* file entryf not readable */
		return(FAIL);
	}
	while (fgets(ltext, 512, pentryf)) fputs(ltext, fp);
	fclose(pentryf);
	return(SUCCESS);
}
