#define nchars 64	/*number of chars in char set*/
#define nlines  6	/*number of lines in a banner character*/
#define pposs  85	/*number of print positions on a line (must be multiple of 4)*/
			/*followed by end of string character*/
#define pospch 8	/*number of char positions per banner char*/
#define chpln  10	/*number of banner characters per line*/

struct bann{
	  char alpha[nlines][pposs];
};
struct bann buffer,*bp buffer;
char ctbl[nchars][nlines]{
	036,041,046,051,046,035,	/*@*/
	014,022,041,077,041,041,	/*A*/
	076,041,076,041,041,076,	/*B*/
	036,041,040,040,041,036,	/*C*/
	076,041,041,041,041,076,	/*D*/
	077,040,076,040,040,077,	/*E*/
	077,040,076,040,040,040,	/*F*/
	036,041,040,047,041,036,	/*G*/
	041,041,077,041,041,041,	/*H*/
	004,004,004,004,004,004,	/*I*/
	001,001,001,001,041,036,	/*J*/
	041,042,074,044,042,041,	/*K*/
	040,040,040,040,040,077,	/*L*/
	041,063,055,041,041,041,	/*M*/
	041,061,051,045,043,041,	/*N*/
	036,041,041,041,041,036,	/*O*/
	076,041,041,076,040,040,	/*P*/
	036,041,041,045,042,035,	/*Q*/
	076,041,041,076,042,041,	/*R*/
	036,040,036,001,041,036,	/*S*/
	037,004,004,004,004,004,	/*T*/
	041,041,041,041,041,036,	/*U*/
	041,041,041,041,022,014,	/*V*/
	041,041,041,055,063,041,	/*W*/
	041,022,014,014,022,041,	/*X*/
	021,012,004,004,004,004,	/*Y*/
	077,002,004,010,020,077,	/*Z*/
	016,010,010,010,010,016,	/*[*/
	040,020,010,004,002,001,	/*\*/
	034,004,004,004,004,034,	/*]*/
	004,012,000,000,000,000,	/*^*/
	000,000,000,000,000,077,	/*_*/
	000,000,000,000,000,000,	/* */
	010,010,010,010,000,010,	/*!*/
	022,022,000,000,000,000,	/*"*/
	022,077,022,022,077,022,	/*#*/
	036,054,036,015,055,036,	/*$*/
	001,062,064,013,023,040,	/*%*/
	014,022,014,024,042,035,	/*&*/
	010,010,000,000,000,000,	/*'*/
	004,010,010,010,010,004,	/*(*/
	010,004,004,004,004,010,	/*)*/
	000,022,014,014,022,000	/***/
};
char blank ' ';
char plot 'X';
int msk 040;	/*mask at sixth bit*/

main(argc,argp)
char **argp;int argc;
{
	int i;

	/*if invoked with no arguments, prints error comment;
	  if invoked with an argument, prints it in banner form.
	*/

	if(argc<2){
	  printf("missing argument\n");
	  exit();
	}
	banner(argp[1],bp);
	banprt(bp);
}

banner(s,bufp)
char *s;struct bann *bufp;
{
	char c,*p,*q,*r;
	p=s;
	r=bufp;
	banset(blank,bufp);

	while((c= *s++)!=0){
	  if((s-p)>chpln)return(s-p);
	  if(c>='`')c =- ' ';	/*map lower to upper case*/
	  if(c<' ')c='#';
	  if(c>'?')c=- 0100;
	  q=ctbl[c];
	  banfil(q,r);
	  r=+pospch;
	}
}

banfil(c,p)
char *c;
struct bann *p;
{
	int i,j;
	for(i=0;i<nlines;i++){
	  for(j=0;j<pospch;j++){
	    if(((c[i]<<j)&msk)!=0)p->alpha[i][j] = plot;
	  }
	}
	return(0);
}

banset(c,p)
char c;
struct bann *p;
{
	int i,j;
	for(i=0;i<nlines;i++)
	  for(j=0;j<pposs-1;j++)
	    p->alpha[i][j] = c;
}

banprt(ptr)
struct bann *ptr;
{
	int i,j;
	for(i=0;i<nlines;i++){
	  ptr->alpha[i][pposs-1]='\0';
	  for(j=pposs-2;j>=0;j--){
	    if(ptr->alpha[i][j]!=blank)break;
	    ptr->alpha[i][j]='\0';
	  }
	printf("%s\n",ptr->alpha[i]);
	}
}
