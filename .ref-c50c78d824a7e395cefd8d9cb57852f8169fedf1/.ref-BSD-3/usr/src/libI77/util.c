#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include "fio.h"
#define DIRSIZE	14
g_char(a,alen,b) char *a,*b; ftnlen alen;
{	char *x=a+alen-1,*y=b+alen-1;
	*(y+1)=0;
	for(;x>=a && *x==' ';x--) *y--=0;
	for(;x>=a;*y--= *x--);
}
b_char(a,b,blen) char *a,*b; ftnlen blen;
{	int i;
	for(i=0;i<blen && *a!=0;i++) *b++= *a++;
	for(;i<blen;i++) *b++=' ';
}
inode(a) char *a;
{	struct stat x;
	if(stat(a,&x)<0) return(-1);
	return(x.st_ino);
}
#define DONE {*bufpos++=0; close(file); return;}
#define INTBOUND sizeof(int)-1
#define register 
mvgbt(n,len,a,b) char *a,*b;
{	register int num=n*len;
	if( ((int)a&INTBOUND)==0 && ((int)b&INTBOUND)==0 && (num&INTBOUND)==0 )
	{	register int *x=(int *)a,*y=(int *)b;
		num /= sizeof(int);
		if(x>y) for(;num>0;num--) *y++= *x++;
		else for(num--;num>=0;num--) *(y+num)= *(x+num);
	}
	else
	{	register char *x=a,*y=b;
		if(x>y) for(;num>0;num--) *y++= *x++;
		else for(num--;num>=0;num--) *(y+num)= *(x+num);
	}
}
char *curdir()
{	char name[256],*bufpos = name;
	struct stat x;
	struct direct y;
	int file,i;
	*bufpos++ = 0;
loop:	stat(".",&x);
	if((file=open("..",0))<0) goto done;
	do
	{	if(read(file,&y,sizeof(y))<sizeof(y)) goto done;
	} while(y.d_ino!=x.st_ino);
	close(file);
	if(y.d_ino!=2)
	{	dcat(name,y.d_name);
		chdir("..");
		goto loop;
	}
	if(stat(y.d_name,&x)<0 || chdir("/")<0
		|| (file=open("/",0))<0) goto done;
	i=x.st_dev;
	do
	{	if(read(file,&y,sizeof(y))<sizeof(y)) goto done;
		if(y.d_ino==0) continue;
		if(stat(y.d_name,&x)<0) goto done;
	} while(x.st_dev!=i || (x.st_mode&S_IFMT)!=S_IFDIR);
	if(strcmp(".",y.d_name) || strcmp("..",y.d_name))
		dcat(name,y.d_name);
	dcat(name,"/");
done:
	bufpos=calloc(strlen(name)+1,1);
	strcpy(bufpos,name);
	chdir(name);
	close(file);
	return(bufpos);
}
dcat(a,b) char *a,*b;
{
	int i,j;
	i=strlen(b);
	j=strlen(a);
	mvgbt(1,j+1,a,a+i+1);
	mvgbt(1,i,b,a);
	a[i]='/';
}
fullpath(a,b,errflag) char *a,*b;
{
	char *a1,*a2,*npart,*dpart,*p;
	a1=curdir();
	npart=NULL;
	for(p=a;*p!=0;p++)
		if(*p=='/') npart=p;
	if(npart==NULL)
	{	dpart=NULL;
		npart=a;
	}
	else
	{	dpart=a;
		*npart++ = 0;
	}
	if(dpart!=NULL)
	{	chdir(dpart);
		a2=curdir();
		strcpy(b,a2);
	}
	else
	{	a2=NULL;
		strcpy(b, a1);
	}
	strcat(b,npart);
	chdir(a1);
	if(a1!=NULL)
	{	free(a1);
		a1=NULL;
	}
	if(a2!=NULL)
	{	free(a2);
	}
	return(0);
}
