/* Rewrite for VAX: BSB 9/9/79 */
/* Compiled with V7 cc: BSB 6/2/79 */

#include <stdio.h>
#include <sgtty.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <stat.h>

#define CLEARPAGE write(2,"\014\000",2);
#define PAUSE  0
#define NOTIFY 1
#define CLEAR  2
#define NOTICLEAR 3

int spage = 37;
int page;
short int width = 79;
short int noclr = 0;
short int first = 0;
short int numop = 0;
short int flagctl = 1;
short int ontty = 1;    /* assume we're outputting to a tty     */
short int delflg;       /* a <DEL> was typed.                   */

FILE *fin;
jmp_buf envir;

main(argc, argv)
int argc;
char *argv[];
{
	register int i,k,n;
	register char *c1, *c2;
	int j, clrflag;
	int int2(), getout();
	struct sgttyb sg;
	struct stat st;
	extern char _sibuf[], _sobuf[];

	if(gtty(1, &sg) == -1) {
		ontty = 0;
		flagctl = 0;
	}
	c1 = c2 = argv[0];
	do
		if(*c1++ == '/')
			c2 = c1;
	while(*c1);
	if(*c2 == 'c')
		noclr++;
	k=0;
	for( i=1; i<argc; i++ ) {/* look for - args */
		if(argv[i][0] == '-' ) {
			switch( argv[i][1] ) {
				case 'c': noclr++; break;
				case 'f': first=num(&argv[i][2]); /* set first line */
					  break;
				case 'n': numop=1; /* set number option */
					  break;
				case 'l':
				case 'p': spage =num(&argv[i][2]); /* set page size */
					  break;
				case 'w': width =num(&argv[i][2]);
					  break;
				case 'x': flagctl = flagctl? 0 : 1;
					  break;
				default:  printf("Unknown switch: %s\n", argv[i]);
					  return;
			}
		} else
			argv[k++] = argv[i];
	}

	if (spage<=0)
		spage = 0x07fffffff;    /* largest positive number! */
	setbuf(stdout, _sobuf);
	if(ontty)
		signal(SIGINT,int2);
	signal(SIGQUIT,getout);
	page = spage;
	if(k == 0) {                    /* filter */
		if(!setjmp(envir)) {
			fin = stdin;
			setbuf(fin, _sibuf);
			pfile(0);
		}
		getout();
	}
	j = 0;
	setjmp(envir);
	while((i=j++) < k) {
		page = spage;
		clrflag = 0;
		if(fin != NULL) { fclose(fin); fin = NULL; }
		if(stat(argv[i], &st) == 0 && (st.st_mode&S_IFMT) == S_IFDIR){
			printf("%s: Is a directory!\n", argv[i]);
			continue;
		}
		if((fin = fopen(argv[i],"r")) == NULL) {
			printf("Cannot open \"%s\" for reading!\n", argv[i]);
			continue;
		}
		if(st.st_size == 0) {
			printf("File \"%s\" is empty.\n", argv[i]);
			continue;
		}
		if(k>1) {
		    if(i && delflg <= 0)
			printf("\n\n");
		    delflg = 0;
		    if(ontty) {
			printf("Press <RETURN> to list \"%s\"\n", argv[i]);
			clrflag = 1;
		    } else
			printf(">>>>> File \"%s\"\n", argv[i]);
		}

		pfile(clrflag);
	}
	getout();
}

int  linpos, line, ct;

pfile(flg)
{
	register int c;

	if(flg) nextpage(NOTICLEAR);
	else if(!noclr && ontty)
		CLEARPAGE;
	delflg = -1;
	line = 1;
	ct = page;
	while (line < first)  /* Dcrocker: skip to first line */
		if((c = getch()) == EOF)
			return;
		else if(c == '\n')
			line++;
	linpos = 0;

	while ((c = getch()) != EOF)
		putch(c);

	fflush(stdout);
}

num(s)	 /* computes the internal form of a number */
register char *s;       /* bad chars are ignored */
{
	register int c, i, sign;

	sign=1;  i=0;
	while(c = *s++) {
		if(c=='-' && sign==1) sign = -1;
		c -= '0';
		if(c>=0 && c<=9) i=i*10+c;
	}
	return(i*sign);
}

nextpage (clearpage)
{       char c;

	if(!ontty)
		return;
	if (clearpage & NOTIFY)
		putchar('\007');
	fflush(stdout);
	c = 0;
	while(read(2, &c, 1) && c != '\n') ;
	if (clearpage & CLEAR && c) {
		CLEARPAGE;
		page = spage;
	} else {
		page = (spage>>1) + (spage>>3);
/***            page = spage * .6;              ***/
	}
	return;
}

int2()
{
	signal(SIGINT,int2);
	stdout->_cnt = BUFSIZ;
	stdout->_ptr = stdout->_base;
	if(delflg)
		putchar('\n');
	delflg++;
	longjmp(envir, 1);
}

int     peekc = -2;

peekch()
{
	return(peekc = getch());
}

getch()
{
	register int c;
	static word;

	if(peekc != -2) {
		c = peekc;
		peekc = -2;
		return(c);
	};
	c = getc(fin);
	if(c != EOF)
		c &= 0177;
	return(c);
}

putch(c)
register int c;
{
		if(linpos == 0 && numop) {
			printf("%-5d | ");
			linpos += 8;
		}
		if(c < 040 || c == 0177) switch(c) {
			case '\n': line++;
				   linpos = -1;
				   break;
			case 014:  goto npage;
			case 011:  linpos += 8;
				   linpos &= ~07;
				   linpos--;
				   break;
			default:   if(flagctl) {
					if(c == '\7')
					    putchar(c);
					putch('^');
					if(c != 0177)
					    c += '@';
					else
					    c = 'd';
				   }
		}
		putchar(c);
		linpos++;
		if(width && linpos >= width && peekch() != '\n') {
			putchar('\n'); linpos = 0;
		}
		if(linpos == 0 && --ct <= 0) {
	   npage:       nextpage(NOTICLEAR);
			ct = page;
		}
}


getout()
{
	exit(0);
}
