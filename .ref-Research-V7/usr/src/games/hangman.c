#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#define DICT "/usr/dict/words"
#define EDICT "/crp/dict/web2"
#define MAXERR 7
#define MINSCORE 0
#define MINLEN 7
char *dictfile;
int alive,lost;
FILE *dict;
long int dictlen;
float errors=0, words=0;
main(argc,argv) char **argv;
{
	if(argc==1) dictfile=DICT;
	else if(*argv[1]=='-') dictfile=EDICT;
	else dictfile=argv[1];
	setup();
	for(;;)
	{	startnew();
		while(alive>0)
		{	stateout();
			getguess();
		}
		words=words+1;
		if(lost) wordout();
		else youwon();
	}
}
setup()
{	int tvec[2];
	struct stat statb;
	time(tvec);
	srand(tvec[1]+tvec[2]);
	if((dict=fopen(dictfile,"r"))==NULL) fatal("no dictionary");
	if(stat(dictfile,&statb)<0) fatal("can't stat");
	dictlen=statb.st_size;
}
double frand()
{
	return(rand()/32768.);
}
char word[26],alph[26],realword[26];
startnew()
{	int i;
	long int pos;
	char buf[128];
	for(i=0;i<26;i++) word[i]=alph[i]=realword[i]=0;
	pos=frand()*dictlen;
	fseek(dict,pos,0);
	fscanf(dict,"%s\n",buf);
	getword();
	alive=MAXERR;
	lost=0;
}
stateout()
{	int i;
	printf("guesses: ");
	for(i=0;i<26;i++)
		if(alph[i]!=0) putchar(alph[i]);
	printf(" word: %s ",word);
	printf("errors: %d/%d\n",MAXERR-alive,MAXERR);
}
getguess()
{	char gbuf[128],c;
	int ok=0,i;
loop:
	printf("guess: ");
	if(gets(gbuf)==NULL)
	{	putchar('\n');
		exit(0);
	}
	if((c=gbuf[0])<'a' || c>'z')
	{	printf("lower case\n");
		goto loop;
	}
	if(alph[c-'a']!=0)
	{	printf("you guessed that\n");
		goto loop;
	}
	else alph[c-'a']=c;
	for(i=0;realword[i]!=0;i++)
		if(realword[i]==c)
		{	word[i]=c;
			ok=1;
		}
	if(ok==0)
	{	alive--;
		errors=errors+1;
		if(alive<=0) lost=1;
		return;
	}
	for(i=0;word[i]!=0;i++)
		if(word[i]=='.') return;
	alive=0;
	lost=0;
	return;
}
wordout()
{
	errors=errors+2;
	printf("the answer was %s, you blew it\n",realword);
}
youwon()
{
	printf("you win, the word is %s\n",realword);
}
fatal(s) char *s;
{
	fprintf(stderr,"%s\n",s);
	exit(1);
}
getword()
{	char wbuf[128],c;
	int i,j;
loop:
	if(fscanf(dict,"%s\n",wbuf)==EOF)
	{	fseek(dict,0L,0);
		goto loop;
	}
	if((c=wbuf[0])>'z' || c<'a') goto loop;
	for(i=j=0;wbuf[j]!=0;i++,j++)
	{	if(wbuf[j]=='-') j++;
		wbuf[i]=wbuf[j];
	}
	wbuf[i]=0;
	if(i<MINLEN) goto loop;
	for(j=0;j<i;j++)
		if((c=wbuf[j])<'a' || c>'z') goto loop;
	pscore();
	strcpy(realword,wbuf);
	for(j=0;j<i;word[j++]='.');
}
long int freq[]
{	42066,	9228,	24412,	14500,	55162,
	6098,	11992,	12648,	48241,	639,
	2944,	33351,	15545,	35618,	36211,
	16033,	937,	36686,	34957,	37544,
	17621,	5453,	3028,	1556,	12875,
	1743
};
pscore()
{
	if(words!=0) printf("(%4.2f/%.0f) ",errors/words,words);
}
