/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)nhash.c	4.4 (Berkeley) %G%";
#endif /* not lint */

struct dict {
	char *entry;
	char val;
};
extern struct dict ary_d[], cy_d[], ery_d[], fy_d[],gy_d[];
extern struct dict ity_d[],ly_d[],ory_d[],ry_d[],ty_d[];
extern struct dict dict[];
extern struct dict abbrev_d[];
char aahash();
char lookup();
char abbrev();
char ary();
char cy();
char ery();
char fy();
char gy();
char ity();
char ly();
char ory();
char ry();
char ty();

struct hnode {
	char *aakey;
	struct dict *aadata;
};
char 
aahash(s,ex,aatsize,aapr1,aapr2,tbl,data)
char *s;
struct hnode tbl[];
struct dict *data;
{
	char *cp;
	int ii, key, c, p1, p2;
	cp = s;
	key =0;
	while (c = *cp++)
		key = key + (key<<5) + c;
	key &= 077777;
	p1 = key%aapr1;
	p2 = key%aapr2;
	if (p2==0) p2=17;
	for(ii=0; ii<aatsize; ii++)
	{
		if (tbl[p1].aakey==0)
		{ /* new item */
			if (ex ==0)
			{
				tbl[p1].aakey = s;
				tbl[p1].aadata= data;
				return(tbl[p1].aadata->val);
			}
			else
				return(0);
		}
		else
			if(strcmp(tbl[p1].aakey, s)== 0)
			{
				return(tbl[p1].aadata->val);
			}
		p1 = (p1+p2)%aatsize;
	}
	fprintf(stderr, "hash table full:size %d\n",aatsize);
	exit(1);
}
getd(){
	struct dict *ptr;
	ptr = dict;
	while(ptr->entry != 0){
		lookup(ptr->entry,0,ptr);
		ptr++;
	}
}
getab(){
	struct dict *ptr;
	ptr = abbrev_d;
	while(ptr->entry != 0){
		abbrev(ptr->entry,0,ptr);
		ptr++;
	}
}

struct hnode aa1root[499];
#define aa1tsize 499
#define aa1p1 487
#define aa1p2 491
char 
lookup(a0,a1,ptr)
char *a0;
struct dict *ptr;
{
	return(aahash(a0,a1,aa1tsize,aa1p1,aa1p2,aa1root,ptr));
}
struct hnode aa6root[113];
#define aa6tsize 113
#define aa6p1 107
#define aa6p2 109
char
ary(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa6tsize,aa6p1,aa6p2,aa6root,ptr));
}
struct hnode aa9root[13];
#define aa9tsize 13
#define aa9p1 7
#define aa9p2 1
char
cy(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa9tsize,aa9p1,aa9p2,aa9root,ptr));
}
struct hnode aa12root[59];
#define aa12tsize 59
#define aa12p1 47
#define aa12p2 43
char
ery(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa12tsize,aa12p1,aa12p2,aa12root,ptr));
}
struct hnode aa16root[23];
#define aa16tsize 23
#define aa16p1 17
#define aa16p2 19
char
fy(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa16tsize,aa16p1,aa16p2,aa16root,ptr));
}
struct hnode aa17root[29];
#define aa17tsize 29
#define aa17p1 19
#define aa17p2 23
char
gy(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa17tsize,aa17p1,aa17p2,aa17root,ptr));
}
struct hnode aa27root[11];
#define aa27tsize 11
#define aa27p1 5
#define aa27p2 7
char
ity(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa27tsize,aa27p1,aa27p2,aa27root,ptr));
}
struct hnode aa32root[281];
#define aa32tsize 281
#define aa32p1 271
#define aa32p2 277
char
ly(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa32tsize,aa32p1,aa32p2,aa32root,ptr));
}
struct hnode aa36root[59];
#define aa36tsize 59
#define aa36p1 47
#define aa36p2 43
char
ory(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa36tsize,aa36p1,aa36p2,aa36root,ptr));
}
struct hnode aa38root[59];
#define aa38tsize 59
#define aa38p1 47
#define aa38p2 53
char
ry(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa38tsize,aa38p1,aa38p2,aa38root,ptr));
}
struct hnode aa41root[127];
#define aa41tsize 127
#define aa41p1 109
#define aa41p2 113
char
ty(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa41tsize,aa41p1,aa41p2,aa41root,ptr));
}
struct fandd {
	char (*fun)();
	struct dict *yd;
} arr[] = {
	ary,	ary_d,
	cy,	cy_d,
	ery,	ery_d,
	fy,	fy_d,
	gy,	gy_d,
	ity,	ity_d,
	ly,	ly_d,
	ory,	ory_d,
	ry,	ry_d,
	ty,	ty_d,
	0,	0
};

ygetd(){
	struct fandd *ptr;
	struct dict *pp;
	ptr=arr;
	while(ptr->fun != 0){
		pp = ptr->yd;
		while(pp->entry != 0){
			(*ptr->fun)(pp->entry,0,pp);
			pp++;
		}
		ptr++;
	}
}
struct hnode aa42root[71];
#define aa42tsize 71
#define aa42p1 61
#define aa42p2 67
char
abbrev(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa42tsize,aa42p1,aa42p2,aa42root,ptr));
}
