/*					-[Sat Jan 29 13:53:19 1983 by jkf]-
 * 	chkrtab.h			$Locker:  $
 * check if read table valid 
 *
 * $Header: /na/franz/franz/h/chkrtab.h,v 1.1 83/01/29 14:05:24 jkf Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */
 
#define chkrtab(p);	\
	if(p!=lastrtab){ if(TYPE(p)!=ARRAY && TYPE(p->ar.data)!=INT) rtaberr();\
			else {lastrtab=p;ctable=(unsigned char*)p->ar.data;}}
extern lispval lastrtab;
