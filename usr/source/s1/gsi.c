#define	PLOT	006	/* ack */
#define BEL    007     /* exit plot mode */
#define	ESC	033	/* escape */
#define	HFWD	'9'
#define	HREV	'8'
#define	FREV	'7'
#define	SO	016	/* shift out - enter greek */
#define	SI	017	/* shift in */
#define	UP	013
#define	DN	012
#define	RT	' '
#define	LF	'\b'

int	restore();
int	svmode, mode[3];

main(argc,argv) int argc; char **argv; {
	int c, textmode;
	extern int fin,fout;
	fin = dup(0);
	fout = dup(1);
	if ((signal(2, 1) & 01) == 0)
		signal(2, &restore);
	gtty(1, mode);
	svmode = mode[2];
	mode[2] =& 0777757;	/* turn off cr-lf  */
	mode[2] =| 03;	/* turn off delays, tabs */
	stty(1, mode);
	textmode = 1;
	while( (c=getchar()) != '\0' ){
		if( c==SO ){
			special();
			continue;
		}
		if (c== PLOT) textmode = 1-textmode;
		if (c==BEL) textmode = 1;
		if( c=='\n' && textmode )
			putchar(015);	/* CR */
		if( c!=ESC ){
			putchar(c);
			continue;
		}
		putchar(PLOT);
		c = getchar();
		if( c == HREV )
			nplot(4,UP);
		else if( c == HFWD )
			nplot(4,DN);
		else if( c == FREV )
			nplot(8,UP);
		putchar(PLOT);
	}
	flush();
	restore();
}

restore(){
	mode[2] = svmode;
	stty(1, mode);
	exit();
}

int	tab[]{
	'A',	/* alpha */
	'B',	/* beta */
	'D',	/* delta */
	'W',	/* DELTA */
	'S',	/* epsilon */
	'N',	/* eta */
	'\\',	/* gamma */
	'G',	/* GAMMA */
	'o',	/* infinity - not in M37 */
	'^',	/* integral */
	'L',	/* lambda */
	'E',	/* LAMBDA */
	'M',	/* mu */
	'[',	/* nabla (del) */
	'_',	/* not */
	'@',	/* nu */
	'C',	/* omega */
	'Z',	/* OMEGA */
	']',	/* partial */
	'U',	/* phi */
	'F',	/* PHI */
	'V',	/* psi */
	'H',	/* PSI */
	'J',	/* pi */
	'P',	/* PI */
	'K',	/* rho */
	'Y',	/* sigma */
	'R',	/* SIGMA */
	'I',	/* tau */
	'T',	/* theta */
	'O',	/* THETA */
	'X',	/* xi */
	'Q',	/* zeta */
	0
};
int	trans[]{
	alpha,
	beta,
	delta,
	DELTA,
	epsilon,
	eta,
	gamma,
	GAMMA,
	infinity,
	integral,
	lambda,
	LAMBDA,
	mu,
	nabla,
	not,
	nu,
	omega,
	OMEGA,
	partial,
	phi,
	PHI,
	psi,
	PSI,
	pi,
	PI,
	rho,
	sigma,
	SIGMA,
	tau,
	theta,
	THETA,
	xi,
	zeta,
	0
};

int alpha[]	{LF,'c',RT,RT,'(',LF,0};
int beta[]	{'B',LF,LF,DN,DN,'|',RT,RT,UP,UP,0};
int delta[]	{'o',UP,UP,'<',DN,DN,0};
int DELTA[]	{LF,LF,'/',-3,DN,'-',-4,RT,'-',-3,UP,'\\',LF,LF,0};
int epsilon[]	{'<','-',0};
int eta[]	{'n',RT,RT,DN,DN,'|',LF,LF,UP,UP,0};
int gamma[]	{')',RT,'/',LF,0};
int GAMMA[]	{LF,LF,'|',RT,RT,-3,UP,'-',-3,DN,RT,RT,'`',LF,LF,0};
int infinity[]	{LF,LF,'c',-4,RT,'o',LF,LF,0};
int integral[]	{'|','\'',RT,RT,'`',-3,LF,-6,DN,'\'',LF,'`',RT,RT,-6,UP,0};
int lambda[]	{'\\',-4,DN,LF,'\'',DN,LF,'\'',-5,UP,RT,RT,0};
int LAMBDA[]	{LF,LF,'/',-4,RT,'\\',LF,LF,0};
int mu[]	{'u',LF,LF,',',RT,RT,0};
int nabla[]	{LF,LF,'\\',-3,UP,'-',-4,RT,'-',-3,DN,'/',LF,LF,0};
int not[]	{'-',-2,RT,UP,',',DN,-2,LF,0};
int nu[]	{LF,'(',-3,RT,'/',LF,LF,0};
int omega[]	{LF,'u',-3,RT,'u',LF,LF,0};
int OMEGA[]	{'O',DN,DN,LF,'-',RT,RT,'-',LF,UP,UP,0};
int partial[]	{'o',RT,DN,'`',LF,UP,'`',LF,UP,'`',RT,DN,0};
int phi[]	{'o','/',0};
int PHI[]	{'o','[',']',0};
int psi[]	{'/','-',DN,DN,RT,RT,'\'',-4,LF,'\'',RT,RT,UP,UP,0};
int PSI[]	{'[',']','-',DN,DN,RT,RT,'\'',-4,LF,'`',RT,RT,UP,UP,0};
int pi[]	{UP,'-',-3,DN,'"',DN,'"',-3,UP,0};
int PI[]	{LF,LF,'[',']',-4,RT,'[',']',LF,LF,-3,UP,'-',-3,DN,0};
int rho[]	{'o',LF,LF,DN,DN,'|',UP,UP,RT,RT,0};
int sigma[]	{'o',DN,RT,RT,'~',UP,LF,LF,0};
int SIGMA[]	{'>',-2,DN,'-',-5,UP,'-',-3,DN,0};
int tau[]	{'t',DN,RT,RT,'~',LF,LF,LF,'~',RT,UP,0};
int theta[]	{'O','-',0};
int THETA[]	{'O','=',0};
int xi[]	{'c',RT,DN,',',LF,-3,UP,'c',LF,DN,'`',RT,DN,0};
int zeta[]	{'c',RT,DN,',',LF,-3,UP,'<',DN,DN,0};

special(){
	int c,i,j,t;
   loop:
	if( (c=getchar()) == SI )
		return;
	for( i=0; tab[i]!=0; i++)
		if( c==tab[i] ){
			plot(trans[i]);
			goto loop;
		}
	putchar(c);
	goto loop;
}

plot(s) int *s; {
	int i,c;
	putchar(PLOT);
	for( i=0; (c=s[i])!=0; i++ )
		if( c<0 )
			nplot(-c,s[++i]);
		else
			putchar(c);
	putchar(PLOT);
	putchar(' ');
}

nplot(n,c) int n,c; {
	while(n--)
		putchar(c);
}
