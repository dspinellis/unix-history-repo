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
struct sgttyb mode;

main(argc, argv)
char	*argv[];
{
	int i;

	gtty(1, &mode);
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
		if (eq("erase")) {
			if (**++argv == '^')
				mode.sg_erase = (*argv)[1] & 037;
			else
				mode.sg_erase = **argv;
			argc--;
		}
		if (eq("kill")) {
			if (**++argv == '^')
				mode.sg_kill = (*argv)[1] & 037;
			else
				mode.sg_kill = **argv;
			argc--;
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
	if (mode.sg_erase < ' ')
		fprintf(stderr, "erase = '^%c'; ", '@' + mode.sg_erase);
	else
		fprintf(stderr, "erase = '%c'; ", mode.sg_erase);
	if (mode.sg_kill < ' ')
		fprintf(stderr, "kill = '^%c'\n", '@' + mode.sg_kill);
	else
		fprintf(stderr, "kill = '%c'\n", mode.sg_kill);
	m = mode.sg_flags;
	if(m & EVENP)	fprintf(stderr,"even ");
	if(m & ODDP)	fprintf(stderr,"odd ");
	if(m & RAW)	fprintf(stderr,"raw ");
	if(m & CRMOD)	fprintf(stderr,"-nl ");
	if(m & ECHO)	fprintf(stderr,"echo ");
	if(m & LCASE)	fprintf(stderr,"lcase ");
	if((m & XTABS)==XTABS)	fprintf(stderr,"-tabs ");
	if (m & CBREAK)	fprintf(stderr,"cbreak ");
	delay((m&NLDELAY)/NL1,	"nl");
	if ((m&TBDELAY)!=XTABS)
		delay((m&TBDELAY)/TAB1,	"tab");
	delay((m&CRDELAY)/CR1,	"cr");
	delay((m&VTDELAY)/FF1,	"ff");
	delay((m&BSDELAY)/BS1,	"bs");
	fprintf(stderr,"\n");
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
