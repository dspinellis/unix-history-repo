/* sccs id  @(#)chkrtab.h	34.1  10/3/80  */

#define chkrtab(p);	\
	if(p!=lastrtab){ if(TYPE(p)!=ARRAY && TYPE(p->ar.data)!=INT) rtaberr();\
			 else { lastrtab = p; ctable = p->ar.data; } }
extern lispval lastrtab;
