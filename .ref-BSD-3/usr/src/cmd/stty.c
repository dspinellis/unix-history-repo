/*
 * set teletype modes
 */

#include <stdio.h>
#include <sgtty.h>

struct
{
	char	*string;
	int	speed;
} speeds[] = {
	"0",	B0,
	"50",	B50,
	"75",	B75,
	"110",	B110,
	"134",	B134,
	"134.5",B134,
	"150",	B150,
	"200",	B200,
	"300",	B300,
	"600",	B600,
	"1200",	B1200,
	"1800",	B1800,
	"2400",	B2400,
	"4800",	B4800,
	"9600",	B9600,
	"exta",	EXTA,
	"extb",	EXTB,
	0,
};
struct
{
	char	*string;
	int	set;
	int	reset;
} modes[] = {
	"even",
	EVENP, 0,

	"-even",
	0, EVENP,

	"odd",
	ODDP, 0,

	"-odd",
	0, ODDP,

	"raw",
	RAW, 0,

	"-raw",
	0, RAW,

	"cooked",
	0, RAW,

	"-nl",
	CRMOD, 0,

	"nl",
	0, CRMOD,

	"echo",
	ECHO, 0,

	"-echo",
	0, ECHO,

	"LCASE",
	LCASE, 0,

	"lcase",
	LCASE, 0,

	"-LCASE",
	0, LCASE,

	"-lcase",
	0, LCASE,

	"-tabs",
	XTABS, 0,

	"tabs",
	0, XTABS,


	"cbreak",
	CBREAK, 0,

	"-cbreak",
	0, CBREAK,

	"cr0",
	CR0, CR3,

	"cr1",
	CR1, CR3,

	"cr2",
	CR2, CR3,

	"cr3",
	CR3, CR3,

	"tab0",
	TAB0, XTABS,

	"tab1",
	TAB1, XTABS,

	"tab2",
	TAB2, XTABS,

	"nl0",
	NL0, NL3,

	"nl1",
	NL1, NL3,

	"nl2",
	NL2, NL3,

	"nl3",
	NL3, NL3,

	"ff0",
	FF0, FF1,

	"ff1",
	FF1, FF1,

	"bs0",
	BS0, BS1,

	"bs1",
	BS1, BS1,

	"33",
	CR1, ALLDELAY,

	"tty33",
	CR1, ALLDELAY,

	"37",
	FF1+CR2+TAB1+NL1, ALLDELAY,

	"tty37",
	FF1+CR2+TAB1+NL1, ALLDELAY,

	"05",
	NL2, ALLDELAY,

	"vt05",
	NL2, ALLDELAY,

	"tn",
	CR1, ALLDELAY,

	"tn300",
	CR1, ALLDELAY,

	"ti",
	CR2, ALLDELAY,

	"ti700",
	CR2, ALLDELAY,

	"tek",
	FF1, ALLDELAY,

	0,
	};

char	*arg;
struct tchars tc;
struct sgttyb mode;

main(argc, argv)
char	*argv[];
{
	int i;

	gtty(1, &mode);
	ioctl(1, TIOCGETC, &tc);
	if(argc == 1) {
		prmodes();
		exit(0);
	}
	while(--argc > 0) {

		arg = *++argv;
		if (eq("ek")){
			mode.sg_erase = '#';
			mode.sg_kill = '@';
		}
		if (eq("erase") && --argc) {
			if (**++argv == '^')
				mode.sg_erase = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				mode.sg_erase = **argv;
		}
		if (eq("intr") && --argc) {
			if (**++argv == '^')
				tc.t_intrc = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				tc.t_intrc = **argv;
		}
		if (eq("quit") && --argc) {
			if (**++argv == '^')
				tc.t_quitc = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				tc.t_quitc = **argv;
		}
		if (eq("start") && --argc) {
			if (**++argv == '^')
				tc.t_startc = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				tc.t_startc = **argv;
		}
		if (eq("stop") && --argc) {
			if (**++argv == '^')
				tc.t_stopc = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				tc.t_stopc = **argv;
		}
		if (eq("eof") && --argc) {
			if (**++argv == '^')
				tc.t_eofc = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				tc.t_eofc = **argv;
		}
		if (eq("brk") && --argc) {
			if (**++argv == '^')
				tc.t_brkc = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				tc.t_brkc = **argv;
		}
		if (eq("kill") && --argc) {
			if (**++argv == '^')
				mode.sg_kill = (*(argv[1]) == '?') ? 0177 : (*argv)[1] & 037;
			else
				mode.sg_kill = **argv;
		}
		if (eq("gspeed")) {
			mode.sg_ispeed = B300;
			mode.sg_ospeed = B9600;
		}
		if (eq("hup")) {
			ioctl(1, TIOCHPCL, NULL);
		} else
		for(i=0; speeds[i].string; i++)
			if(eq(speeds[i].string))
				mode.sg_ispeed = mode.sg_ospeed = speeds[i].speed;
		for(i=0; modes[i].string; i++)
			if(eq(modes[i].string)) {
				mode.sg_flags &= ~modes[i].reset;
				mode.sg_flags |= modes[i].set;
			}
		if(arg)
			fprintf(stderr,"unknown mode: %s\n", arg);
	}
	stty(1,&mode);
	ioctl(1, TIOCSETC, &tc);
}

eq(string)
char *string;
{
	int i;

	if(!arg)
		return(0);
	i = 0;
loop:
	if(arg[i] != string[i])
		return(0);
	if(arg[i++] != '\0')
		goto loop;
	arg = 0;
	return(1);
}

prmodes()
{
	register m;

	if(mode.sg_ispeed != mode.sg_ospeed) {
		prspeed("input speed  ", mode.sg_ispeed);
		prspeed("output speed ", mode.sg_ospeed);
	} else
		prspeed("speed ", mode.sg_ispeed);
	pit(mode.sg_erase, "erase", "; ");
	pit(mode.sg_kill, "kill", "; ");
	pit(tc.t_intrc, "intr", "; ");
	pit(tc.t_quitc, "quit", "\n");
	pit(tc.t_startc, "start", "; ");
	pit(tc.t_stopc, "stop", "; ");
	pit(tc.t_eofc, "eof", "; ");
	pit(tc.t_brkc, "brk", "\n");
	m = mode.sg_flags;
	if(m & EVENP)	fprintf(stderr,"even ");
	if(m & ODDP)	fprintf(stderr,"odd ");
	fprintf(stderr,"-raw "+((m&RAW)!=0));
	fprintf(stderr,"-nl "+((m&CRMOD)==0));
	fprintf(stderr,"-echo "+((m&ECHO)!=0));
	fprintf(stderr,"-lcase "+((m&LCASE)!=0));
	fprintf(stderr,"-tabs "+((m&XTABS)!=XTABS));
	fprintf(stderr,"-cbreak "+((m&CBREAK)!=0));
	delay((m&NLDELAY)/NL1,	"nl");
	if ((m&TBDELAY)!=XTABS)
		delay((m&TBDELAY)/TAB1,	"tab");
	delay((m&CRDELAY)/CR1,	"cr");
	delay((m&VTDELAY)/FF1,	"ff");
	delay((m&BSDELAY)/BS1,	"bs");
	fprintf(stderr,"\n");
}

pit(what, itsname, sep)
	unsigned char what;
	char *itsname, *sep;
{

	fprintf(stderr, "%s", itsname);
	if (what == 0377) {
		fprintf(stderr, " <undef>%s", sep);
		return;
	}
	fprintf(stderr, " = ");
	if (what & 0200) {
		fprintf(stderr, "M-");
		what &= ~ 0200;
	}
	if (what == 0177) {
		fprintf(stderr, "^");
		what = '?';
	} else if (what < ' ') {
		fprintf(stderr, "^");
		what += '@';
	}
	fprintf(stderr, "%c%s", what, sep);
}

delay(m, s)
char *s;
{

	if(m)
		fprintf(stderr,"%s%d ", s, m);
}

int	speed[] = {
	0,50,75,110,134,150,200,300,600,1200,1800,2400,4800,9600,0,0
};

prspeed(c, s)
char *c;
{

	fprintf(stderr,"%s%d baud\n", c, speed[s]);
}
