static char *sccsid = "@(#)tabs.c	4.1 (Berkeley) %G%";
#include <stdio.h>
#include <sgtty.h>

#define SP	' '
#define TB	'\t'
#define NL	'\n'

# define ESC 033
# define RHM 060
# define SI 017
# define DEL 0177
# define SET '1'
# define CLR '2'
# define MGN '9'
# define CR '\r'
# define BS '\b'

struct sysnod {
	char	*sysnam;
	int	sysval;
};

#define	DASI300 1
#define	DASI300S 2
#define DASI450 3
#define TN300 4
#define TTY37 5
#define HP	6
struct sysnod tty[] = {
	{"dasi300", DASI300},
	{"300", DASI300},
	{"dasi300s", DASI300S},
	{"300s", DASI300S},
	{"dasi450", DASI450},
	{"450", DASI450},
	{"37", TTY37},
	{"tty37", TTY37},
	{"tn300", TN300},
	{"terminet", TN300},
	{"tn", TN300},
	{"hp",	HP},
	{0, 0},
};
int	margset = 1;

syslook(w)
char *w;
{
	register struct sysnod *sp;

	for (sp = tty; sp->sysnam!=NULL; sp++)
		if (strcmp(sp->sysnam, w)==0)
			return(sp->sysval);
	return(0);
}

main(argc,argv)
int argc; char **argv;
{
	struct sgttyb tb;
	int type;
	char *getenv();

	type=0;
	if (argc>=2 && strcmp(argv[1],"-n")==0) {
		margset--; argc--; argv++;
	}
	if (argc>=2) {
		type=syslook(argv[1]);
	} else {
		type=syslook(getenv("TERM"));
	}

	switch(type) {

		case DASI300:	dasi300(); break;

		case DASI300S:	dasi300(); break;

		case DASI450:	dasi450(); break;

		case TN300:	tn300(); break;

		case TTY37:	tty37(); break;

		case HP:	hp2645(); break;

		default:
				gtty (0, &tb);
				if ( (tb.sg_flags & (LCASE|CRMOD)) == CRMOD) {
					/* test for CR map on, upper case off, i.e. terminet but not 33 */
					if ((tb.sg_ispeed) == B300) /* test for 300 baud */
						misc();
				}
				else if ((tb.sg_flags & (CRMOD|LCASE)) == 0 && (tb.sg_ispeed ) == B150) {
					/* apparent model 37 */
					tty37();
				}
	}
}

clear(n)
{
	escape(CLR); 
	delay(n);
	putchar(CR); nl();
}

delay(n)
{
	while (n--) putchar(DEL);
}

tabs(n)
{
	int i,j;

	if(margset) n--;

	for( i=0; i<n; ++i ){
		for( j=0; j<8; ++j ) {
			putchar(SP);
		}
		escape(SET);
	}
}

margin(n)
{
	int i;

	if(margset) {
		for( i=0; i<n; ++i) putchar(SP);
	}
}

escape(c)
{
	putchar(ESC); putchar(c);
}

bs(n)
{
	while (n--) putchar(BS);
}

nl()
{
	putchar(NL);
}



/* ======== terminal types ======== */

dasi450()
{
	struct sgttyb t;
	gtty(0,&t);
	t.sg_flags &= ~ALLDELAY;
	stty(0,&t);
	clear(8); bs(16); margin(8); escape(MGN); nl(); tabs(16);
	escape(RHM); nl();
}

tty37()
{
	putchar(SI); clear(40); bs(8); tabs(9); nl();
}

dasi300()
{
	clear(8); tabs(15); nl();
}

tn300()
{
	struct sgttyb t;
	gtty(0,&t);
	t.sg_flags &= ~ALLDELAY;
	t.sg_flags |= CR1|BS1;
	stty(0,&t);
	clear(8); margin(8); escape(SET); tabs(14); nl();
}

hp2645()
{
	escape('3'); /*clr*/
	putchar(CR);
	tabs(10);
	nl();
}

misc()
{
	tabs(14); nl();
}
