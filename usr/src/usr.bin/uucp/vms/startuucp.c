/*
 * A program to start a uucp batch job (uucico, uuxqt) if it is not
 * already running and pass it the command line.
 *
 * Requires WORLD privilege.
 */
#include <stdio.h>

#define	WORKDIR	"/usr/lib/uucp"		/* where id files are kept */
/* primitive error logging - will have to do for now */
#define	LOGFILE	"/usr/lib/uucp/startuucp.err"
#define	JOBID_LINE_FORMAT	"  Job %d entered on queue %s"
#define	FMASK		0111

#define	SMR$K_ALTER	0xD	/* Alter Job Attributes SYMBIONT msg */
#define	SMO$K_RLSTIM	0x20	/* Release-time option		    */

struct	SYMBIONT_MESSAGE {		/* Message for SYMBIONT Manager */
	unsigned short int Request;		/* Request number */
	unsigned char      Queue[16];		/* Queue Name     */
	unsigned short int JobID;		/* JOB ID number  */
	unsigned char	   Option;		/* RLSTIM Option  */
	unsigned char      Time[8];		/* RLSTIM Value   */
	unsigned char	   End_Options;		/* End of Options */
} Symbiont_Message;

main(argc,argv)
char *argv[];
{
	FILE *f;
	struct {int Size; struct SYMBIONT_MESSAGE *Ptr;} Message_Descr;
	int i, fd;
	int JobID;
	char Queue[64];
	char jidfile[64];
	char cmdfile[64];

	if (argc < 2) {
		fprintf(stderr, "Usage: %s batch-command [args]\n", argv[0]);
		exit(1);
	}
	sprintf(jidfile, "%s/%s.jid", WORKDIR, argv[1]);
	sprintf(cmdfile, "%s/%s.dat", WORKDIR, argv[1]);
	umask(FMASK);
	/*
	 * Open the command file and write the command line to it.
	 */
	if (argc > 2) {
		if ((fd = creat(cmdfile, 0777, "txt")) > 0) {
			write(fd, argv[2], strlen(argv[2]));
			for (i = 3; i < argc; i++) {
				write(fd, " ", 1);
				write(fd, argv[i], strlen(argv[i]));
			}
			write(fd, "\n", 1);
			close(fd);
		} else
			logerr("%s: can not create\n", cmdfile);
	}
	/*
	 * Open the JOB ID file and extract the Job and QUEUE
	 */
	f = fopen(jidfile, "r");
	if (f == NULL)
		exit(0);		/* No file, UUCICO running or dead */
	i = fscanf(f, JOBID_LINE_FORMAT, &JobID, Queue);
	fclose(f);
	if (i != 2) {
		logerr("%s: bad format\n", jidfile);
		exit(1);		/* No Job, PUNT! */
	}
	/*
	 * Construct the SYMBIONT MANAGER message
	 */
	Symbiont_Message.Request = SMR$K_ALTER;	/* Alter Job Attrs. */
	Symbiont_Message.Queue[0] = strlen(Queue);
	strcpy(&Symbiont_Message.Queue[1],Queue);	/* In this Queue    */
	Symbiont_Message.JobID = JobID;		/* This Job	    */
	Symbiont_Message.Option = SMO$K_RLSTIM;	/* Mod Release Time */
	sys$gettim(Symbiont_Message.Time);		/*     to NOW	    */
	Symbiont_Message.End_Options = 0;
	/*
	 * Send message to Symbiont Manager
	 */
	Message_Descr.Size = sizeof(Symbiont_Message);
	Message_Descr.Ptr  = &Symbiont_Message;
	i = sys$sndsmb(&Message_Descr,0);
	if (!(i & 1)) {
		logerr("Symbiont error 0x%x Jobid %d Queue %s\n", i, JobID,
			Queue);
		exit(1);
	}
	/*
	 * DONE:
	 */
	exit(0);
}

logerr(fmt, a, b, c, d)
	char *fmt;
{
	long t;
	char *p, *ctime();
	FILE *f;

	fprintf(stderr, fmt, a, b, c, d);
	if ((f = fopen(LOGFILE, "a")) == NULL)
		return;
	time(&t);
	p = ctime(&t);
	p[24] = '\0';
	fputs(&p[4], f);
	fprintf(f, fmt, a, b, c, d);
	fclose(f);
}
