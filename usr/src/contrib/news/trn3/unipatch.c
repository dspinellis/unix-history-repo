/*
A filter to turn a unified diff into a degenerate context diff (no '!'s)
for patch. Version 1.1. Author: davison@borland.com
*/
#include <stdio.h>
#define ERR(a) {fputs(a,stderr);exit(1);}
#define NUM(x) {for(x=0;*cp<='9'&&*cp>='0';)x=x*10+*cp++-'0';ch= *cp++;}
struct Ln {struct Ln *lk; char t; char s[1];} r,*h,*ln;
char bf[2048],*cp,ch,*malloc();
long os,ol,ns,nl,ne,lncnt;
main()
{
 for(;;){
  for(;;){
   if(!fgets(bf,sizeof bf,stdin)) exit(0);
   lncnt++;
   if(!strncmp(bf,"@@ -",4)) break;
   fputs(bf,stdout);
  }
  ol=nl=1, cp=bf+4;
  NUM(os)
  if(ch==',') NUM(ol)
  if(*cp++!='+') goto bad;
  NUM(ns)
  if(ch==',') NUM(nl)
  if(*cp!='@') goto bad;
  r.lk=0, h= &r, ne=ns+nl-1;
  printf("***************\n*** %ld,%ld ****\n",os,os+ol-(os>0));
  while(ol||nl){
   if(!fgets(bf,sizeof bf,stdin)){
    if(nl>2) ERR("Unexpected end of file.\n")
    strcpy(bf," \n");
   }
   lncnt++;
   if(*bf=='\t'||*bf=='\n') ch=' ', cp=bf;
   else ch= *bf, cp=bf+1;
   switch(ch){
   case'-':if(!ol--) goto bad;
	printf("- %s",cp);
	break;
   case'=':ch=' ';
   case' ':if(!ol--) goto bad;
	printf("  %s",cp);
   case'+':if(!nl--) goto bad;
	ln = (struct Ln*)malloc(sizeof(*ln)+strlen(cp));
	if(!ln) ERR("Out of memory!\n")
	ln->lk=0, ln->t=ch, strcpy(ln->s,cp);
	h->lk=ln, h=ln;
	break;
   default:
bad:	fprintf(stderr,"Malformed unified diff at line %ld: ",lncnt);
	ERR(bf)
   }
  }
  printf("--- %ld,%ld ----\n",ns,ne);
  for(ln=r.lk;ln;ln=h){
   printf("%c %s",ln->t,ln->s);
   h=ln->lk; free(ln);
  }
 }
}
