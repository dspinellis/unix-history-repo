#define numeric(c) (c>='0' && c<='9')
#define max(a,b) (a<b ? b : a)
#define min(a,b) (a>b ? b : a)
#define abs(x) (x<0 ? -x : x)

#define copy(src,dest) strcpy(dest,src)
#define compare(str1,str2) strcmp(str1,str2)
#define equal(str1,str2) !strcmp(str1,str2)
#define length(str) strlen(str)
#define size(str) (1+strlen(str))

#include "sys/types.h"
#include "sys/stat.h"
struct stat Statbuf;
#define exists(file) (stat(file,&Statbuf)<0 ? 0:Statbuf.st_mode)

#define xfopen(file,mode) fdopen(xopen(file,mode),mode==0?"r":mode==1?"w":"r+w")
#define xfcreat(file,mode) fdopen(xcreat(file,mode),"w")
#define remove(file) xunlink(file)

#define SCCSID(arg) static char Sccsid[] = "arg"

#define USXALLOC()

#define NONBLANK(p) while (*p==' ' || *p=='\t') p++

char Null[1];
char Error[128];
