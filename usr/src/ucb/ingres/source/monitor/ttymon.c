# include	"monitor.h"
# include	<ingres.h>
# include	<version.h>
# include	<opsys.h>
# include	<pv.h>
# include	<func.h>
# include	<signal.h>
# include	<pipes.h>
# include	<setjmp.h>
# include	<sccs.h>

SCCSID(@(#)ttymon.c	7.1	2/5/81)



# define	ERRDELIM	'~'

/*
**  INTERACTIVE TERMINAL MONITOR
**
**	The monitor gathers text from the standard input and performs
**	a variety of rudimentary editting functions.  This program
**	is the main setup.  Monitor() is then called, which does the
**	real work.
**
**	variables:
**	Nodayfile -- zero prints all messages; positive one suppresses
**		dayfile and logout but not prompts; negative one
**		suppresses all printed material except results from \p.
**	Newline -- set when the last character in the query buffer
**		is a newline.
**	Prompt -- set when a prompt character is needed.
**	Autoclear -- set when the query buffer should be cleared before
**		putting another character in.
**	Nautoclear -- if set, suppresses the autoclear function
**		entirely.
**
**	flags:
**	-M -- trace flag
**	-d -- suppress dayfile
**	-s -- suppress prompt (sets -d)
**	-a -- disable autoclear function
**
**	The last three options can be set by stating "+x".
**
**	Trace Flags:
**		9
**		11 (proc_err)
*/

extern char	*Usercode;
extern		tm_mon();
extern		tm_init();
extern		tm_intr();
short		tTttymon[100];

struct fn_def	TtyMonFn =
{
	"MONITOR",
	tm_mon,
	tm_init,
	tm_intr,
	NULL,
	0,
	tTttymon,
	100,
	'M',
	0
};

tm_init(argc, argv)
int	argc;
char	*argv[];
{
	register int	ndx;
	register char	*p;
	extern		quit();
	extern int	(*ExitFn)();
	extern int	Equel;
	char		buff[100];
	extern char	*getufield();
	extern jmp_buf	CmReset;
	extern char	SysIdent[];


	/* insure that permissions are ok */
	setuid(getuid());
#	ifndef xB_UNIX
	setgid(getgid());
#	endif

	setjmp(CmReset);
	signal(SIGPIPE, quit);

	ExitFn = quit;
	set_si_buf();

	/* process arguments */
	if (!setflag(argv, 'd', 1))
		Nodayfile = 1;
	if (!setflag(argv, 's', 1))
		Nodayfile = -1;
	Nautoclear = !setflag(argv, 'a', 1);

	/* preinitialize macros */
	macinit(0, 0, 0);
	macdefine("{pathname}", Pathname, 1);

	/* print the dayfile */
	if (Nodayfile >= 0)
	{
		time(buff);
		printf("%s login\n%s", SysIdent, ctime(buff));
	}
	if (Nodayfile == 0 && (Qryiop = fopen(ztack(ztack(Pathname, "/files/dayfile"), VERSION), "r")) != NULL)
	{
		while ((ndx = getc(Qryiop)) > 0)
			putchar(ndx);
		fclose(Qryiop);
	}

	/* SET UP LOGICAL QUERY-BUFFER FILE */
	concat("/tmp/INGQ", Fileset, Qbname);
	if ((Qryiop = fopen(Qbname, "w")) == NULL)
		syserr("main: open(%s)", Qbname);

	/* GO TO IT ... */
	Prompt = Newline = TRUE;
	Userdflag = Nodayfile;
	Nodayfile = -1;

	/* run the system initialization file */
	setjmp(CmReset);
	Phase++;
	include(ztack(Pathname, "/files/startup"));

	/* find out what the user initialization file is */
	setjmp(CmReset);
	if (getuser(Usercode, buff) == 0)
	{
		p = getufield(buff, 7);
		if (*p != 0)
			include(p);
	}
	getuser(0, 0);

	Nodayfile = Userdflag;

	/*
	**  Get user input from terminal
	**
	**	THIS CODE IS A CLUDGE!!!
	**
	**	This code should return right after the setbuf call,
	**	but it doesn't because we want the monitor to be in
	**	control initially.  The way the control module is
	**	written, this will work.  But we are definitely
	**	cheating....
	*/

	Input = stdin;
	setbuf(stdin, NULL);
	monitor();
	quit();
}
/*
**  CATCH SIGNALS
**
**	clear out pipes and respond to user
**
**	Uses trace flag 10
*/

tm_intr(typ)
int	typ;
{
	register int	i;

	if (typ != 2)
		syserr("tm_intr: typ %d", typ);

	if (Xwaitpid == 0)
		printf("\nInterrupt\n");

	lseek(stdin->_file, 0L, 2);
	Newline = Prompt = TRUE;
	Nodayfile = Userdflag;
	Oneline = FALSE;
	Idepth = 0;
	setbuf(stdin, NULL);
	Input = stdin;
	xwait();
}
/*
**  PROCESS ERROR MESSAGE
**
**	This routine takes an error message off of the pipe and
**	processes it for output to the terminal.  This involves doing
**	a lookup in the .../files/error? files, where ? is the thous-
**	ands digit of the error number.  The associated error message
**	then goes through parameter substitution and is printed.
**
**	In the current version, the error message is just printed.
**
**	We unquestionably cheat, by doing a longjmp rather than a
**	return here -- this is so that the synchronization works right.
**
**	Trace Flags:
**		30
*/

proc_err(ppb, pc, pv)
pb_t	*ppb;
int	pc;
PARM	pv[];
{
	register char	c;
	register char	*p;
	int		i;
	char		buf[512];
	int		err;
	FILE		*iop;
	char		*errfilen();
	extern char	*mcall();
	bool		fatal;
	extern jmp_buf	GoJmpBuf;

	if (pc <= 0 || pv[0].pv_type != PV_INT)
		syserr("proc_err: pc %d pv0type %d", pc, pv[0].pv_type);
	err = pv[0].pv_val.pv_int;
	Error_id = err;
	fatal = !bitset(PB_INFO, ppb->pb_stat);

	/* try calling the {catcherror} macro -- maybe not print */
	p = buf;
	p += smove("{catcherror; ", p);
	p += smove(iocv(err), p);
	p += smove("}", p);

	p = mcall(buf);
	if (sequal(p, "0"))
		return (1);

	/* open the appropriate error file */
	p = errfilen(err / 1000);

#	ifdef xMTR3
	if (tTf(30, -1))
		printf("proc_error: ");
	if (tTf(30, 0))
		printf("%d, %s", err, p);
#	endif

	if ((iop = fopen(p, "r")) == NULL)
		syserr("proc_error: open(%s)", p);

	/* read in the code and check for correct */
	for (;;)
	{
		p = buf;
		while ((c = getc(iop)) != '\t')
		{
			if (c <= 0)
			{
				/* no code exists, print the args */
				printf("%d:", err);
				for (i = 0; i < pc; i++)
					printf(" `%s'", pv[i].pv_val.pv_str);
				printf("\n");
				fclose(iop);
				if (fatal)
					longjmp(CmReset, 1);
				else
					longjmp(GoJmpBuf, 1);
			}
			*p++ = c;
		}
		*p = 0;
		if (atoi(buf, &i))
			syserr("proc_error: bad error file %d\n%s",
				err, buf);
		if (i != err)
		{
			while ((c = getc(iop)) != ERRDELIM)
				if (c <= 0)
					syserr("proc_error: format err %d", err);
			getc(iop);	/* throw out the newline */
			continue;
		}

		/* got the correct line, print it doing parameter substitution */
		printf("%d: ", err);
		c = '\n';
		for (;;)
		{
			c = getc(iop);
			if (c <= 0 || c == ERRDELIM)
			{
				printf("\n");
				fclose(iop);
				if (fatal)
					longjmp(CmReset, 1);
				else
					longjmp(GoJmpBuf, 1);
			}
			if (c == '%')
			{
				c = getc(iop) - '0' + 1;
				if (c >= pc)
					syserr("proc_err: parm %d", c - 1);
				switch (pv[c].pv_type)
				{
				  case PV_STR:
					for (p = pv[c].pv_val.pv_str; c = *p; p++)
						xputchar(c);
					continue;

				  case PV_INT:
					printf("%d", pv[c].pv_val.pv_int);
					continue;

				  default:
					syserr("proc_err: arg %d type %d", c, pv[c].pv_type);
				}
			}
			printf("%c", c);
		}
	}
}
/*
**  TM_MON -- "function to implement this module"
**
**	Since we have cludged up this module to work, and hence
**	the init routine should never return, this routine just
**	syserr's.
*/

tm_mon()
{
	syserr("tm_mon");
}
/*
**  ACC_INIT, PAGEFLUSH -- dummy access method routines
**
**	Since the CM wants to do some basic access method functions,
**	we will let it.
*/

acc_init()
{
}

pageflush(x)
char	*x;
{
	return (0);
}
/*
**  CLOSECATALOG -- dummy catalog close routine.
**
**	To keep from loading access methods.
*/

closecatalog()
{
}
