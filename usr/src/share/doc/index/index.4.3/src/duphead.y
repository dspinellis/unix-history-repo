/* this program (modified from reduce.y)
should duplicate headwords on duplicate lines,
so we can sort and grep on them without losing information.
run prior to running reduce (or after modifying reduce to compare 
adjacent lines for duplicate headwords). */
%{
#include "reduce.h"
#include <strings.h>
#include "pagesizes.h"  /* contains last page information for each doc */
int npages;	/* current number of pages in group */
#define MAXPAGES 300
#define MAXHDLEN 200
#define MAXSTRING 255
Page pages[MAXPAGES];  /* contains an entire group's page references */
char text[MAXHDLEN]="";	/* headword text */
int initial=1;
%}
%union {
	char *sptr;  /* pointer to string */
	Page *pptr;  /* pointer to page entry */
}
%token <sptr> HEADWORD
%token <sptr> WORD
%token <sptr> DUP
%token <pptr> PAGE
%token <sptr> WHITESPACE
%token <sptr> ELLIPSIS

%%
index:	
	| index group {
		debugpr("group\n",10);
		npages=0;
		}
	;

group: 	  hdsentence '\n' {
			debugpr("newhead\n",10);
		}
	| group dup '\n'
	;

dup: 	DUP PAGE 	{
		outputhead();
		outputpage($2,++npages);
		debugpr("dup",10);
		}
	;
hdsentence :
	| HEADWORD	{
			debugpr("head",10);
			sprintf(text,"%s",$1);
		}
	| hdsentence WORD {
			debugpr("addword",10);
			strcat(text,$2);
		}
	| hdsentence WHITESPACE {
			debugpr("addwhite",10);
			strcat(text,"\t");
		}
	| hdsentence ELLIPSIS {
			debugpr("ellipsis",10);
			strcat(text,$2);
		}
	| hdsentence PAGE {
			debugpr("headpage",10);
			outputhead();
			outputpage($2,npages);
		}
	;

%%
#include <stdio.h>
#include <setjmp.h>
extern int lineno;
#define YYDEBUG
char *progname;
jmp_buf reparse;
int debug=0;

main(argc,argv)
	char *argv[];
{
	progname = argv[0];
	debug=0;
	setjmp(reparse);
	yyparse();
}

outputpage(p,position)
	char *p;
	int position;
	{
	if (debug>8) fprintf(stderr, "\nppage %s position %d ", p, position);
	pages[position].filename[0]='\0';
	pages[position].percent[0]='\0';
	strcpy(pages[position].pageentry,"unknown");
	sscanf(p,"%s%s%s",pages[position].filename,
		pages[position].percent,
		pages[position].pageentry);
	parsepageentry(&pages[position]);
	if (debug>8)
	fprintf(stderr, 
		"\naddpage posn %d file %s %s page %s \nsort %d vol %s section %d docnum %d docname %s pagenum %d\n", 
		position,
		pages[position].filename,
		pages[position].percent,
		pages[position].pageentry,
		pages[position].sortkey,
		pages[position].volname,
		pages[position].section,
		pages[position].docnum,
		pages[position].docname,
		pages[position].pagenum);
		printf("%s",p);
	}

parsepageentry(p) 
	Page *p;
	{
	char *colon, *minus, *lparen, *rparen;
	char tmp[MAXSTRING];
	char *t;

	if (debug>8)
		fprintf(stderr, "parsepageentry %s", p->pageentry);
	if(strcmp(p->pageentry,"unknown")==0) {
		intuitpageentry(p);
	}
	strcpy(tmp,p->pageentry);  /* we work on it here */
	if (colon = index(tmp,':')) {
		/* found a supplementary docname */
		*colon='\0';
		strcpy(p->volname,tmp); t=colon+1;

		if(strcmp(p->volname,"USD")==0) {
			p->sortkey=4;
		}
		else if(strcmp(p->volname,"PS1")==0) {
			p->sortkey=9;
		}
		else if(strcmp(p->volname,"PS2")==0) {
			p->sortkey=10;
		}
		else if(strcmp(p->volname,"SMM")==0) {
			p->sortkey=12;
		}
		else parseerror("bad supp volume name",p);

		if(minus=index(t,'-')) {
			/* break out docnum and pagenum */
			*minus='\0';
			p->docnum=atoi(t); t=minus+1;
			p->pagenum=atoi(t);
			strcpy(p->docname,"");
		} else parseerror("bogus supp",p);

	} else 	if (lparen = index(tmp,'(')) {
		/* found a man page entry */
		*lparen='\0';
		strcpy(p->docname,tmp); t=lparen+1;
		p->section=atoi(t);
		p->docnum=0;
		switch(p->section) {
			case 1:
				p->sortkey = 1;
				strcpy(p->volname,"URM");
				break;
			case 2:
				p->sortkey = 5;
				strcpy(p->volname,"PRM");
				break;
			case 3:
				p->sortkey = 6;
				strcpy(p->volname,"PRM");
				break;
			case 4:
				p->sortkey = 7;
				strcpy(p->volname,"PRM");
				break;
			case 5:
				p->sortkey = 8;
				strcpy(p->volname,"PRM");
				break;
			case 6:
				p->sortkey = 2;
				strcpy(p->volname,"URM");
				break;
			case 7:
				p->sortkey = 3;
				strcpy(p->volname,"URM");
				break;
			case 8:
				p->sortkey = 11;
				strcpy(p->volname,"SMM");
				break;
			default:
				parseerror("bad section number",p);
			}
		if (rparen = index(t,')')) 
			if(minus=index(rparen,'-')) {
				t=minus+1;
				p->pagenum=atoi(t);
			}
		else p->pagenum=0;
	} else parseerror("neither man nor supp entry",p);
}
	
	
intuitpageentry(p)
	Page *p;
	{
	char *q;
	int docnum;

	if((q=index(p->filename,'.'))==0) parseerror("cant intuit",p);
	q++;
	docnum=atoi(q);
	if(strncmp(p->filename,"USD.",4)==0) {
		sprintf(p->pageentry,"USD:%d-0",docnum);
		return;
	}
	else if(strncmp(p->filename,"PS1.",4)==0) {
		sprintf(p->pageentry,"PS1:%d-0",docnum);
		return;
	}
	else if(strncmp(p->filename,"PS2.",4)==0) {
		sprintf(p->pageentry,"PS2:%d-0",docnum);
		return;
	}
	else if(strncmp(p->filename,"SMM.",4)==0) {
		sprintf(p->pageentry,"SMM:%d-0",docnum);
		return;
	} else  { /* must be a man page */
	sprintf(p->pageentry,"%s",p->filename);
	if((q=index(p->pageentry,'.'))==0) parseerror("cant intuit man",p);
	else {
		*q='(';
		if (*(++q)=='N') *q='1';	/* correct man(n) to man(1) */
	}

	if((q=index(p->pageentry,':'))==0) parseerror("cant intuit man",p);
	else *q=')';
	strcat(p->pageentry,"-0");
	}
}

int compar(p1, p2)
	Page *p1, *p2;
{
	int ret;

	if (p1->sortkey < p2->sortkey) return -1; 
	else if (p1->sortkey == p2->sortkey) {
		if (p1->docnum < p2->docnum) return -1;
		else if (p1->docnum == p2->docnum) {
			if ((ret=strcmp(p1->docname, p2->docname))==0) {
				return (p1->pagenum - p2->pagenum);
			} else return ret;
		} else return 1;
	} else return 1;
}

int samedoc(i, j)
	int i,j;
{
	if (pages[i].sortkey != pages[j].sortkey) return 0; 
	if (pages[i].docnum != pages[j].docnum) return 0;
	if ((strcmp(pages[i].docname, pages[j].docname)==0) &&
		(pages[i].docnum == pages[j].docnum))
		return 1;
	else return 0;
}

outputhead()
{
	printf("\n%s	",text);
}

yyerror(s) /* syntax error routine */
	char *s;
{
	prerror(s, (char *) 0);
}

prerror(s1,s2)
	char *s1, *s2;
{
	fprintf(stderr, "%s", s1);	
	if (s2) fprintf(stderr, "%s", s2);	
	fprintf(stderr, " at line %d, token %d\n", lineno, yychar);
	
}

debugpr(s1,level)
	char *s1;
	int level;
{
	if (debug>level) fprintf(stderr, "%s", s1);	
}

parseerror(s1,p)
	char *s1;
	Page *p;
{
	fprintf(stderr, "%s:%s\n", s1, p->pageentry);	
}
