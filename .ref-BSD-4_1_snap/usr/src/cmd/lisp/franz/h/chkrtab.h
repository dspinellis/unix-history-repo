/* sccs id  @(#)chkrtab.h	35.2  6/17/81  */

#define chkrtab(p);	\
	if(p!=lastrtab){ if(TYPE(p)!=ARRAY && TYPE(p->ar.data)!=INT) rtaberr();\
			else {lastrtab=p;ctable=(unsigned char*)p->ar.data;}}
extern lispval lastrtab;
