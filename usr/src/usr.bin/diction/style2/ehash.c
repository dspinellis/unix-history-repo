/*-
 * %sccs.include.proprietary.c%
 *
 *	@(#)ehash.c	4.5 (Berkeley) %G%
 */

struct hnode {
	char *aakey;
	struct dict *aadata;
};
struct dict {
	char *entry;
	char val;
};
char able();
struct dict able_d[];
char ace();
struct dict ace_d[];
char age();
struct dict age_d[];
char ance();
struct dict ance_d[];
char ant();
struct dict ant_d[];
char cal();
struct dict cal_d[];
char cle();
struct dict cle_d[];
char ee();
struct dict ee_d[];
char ence();
struct dict ence_d[];
char ess();
struct dict ess_d[];
char est();
struct dict est_d[];
char ful();
struct dict ful_d[];
char ible();
struct dict ible_d[];
char ic();
struct dict ic_d[];
char ice();
struct dict ice_d[];
char ion();
struct dict ion_d[];
char ional();
struct dict ional_d[];
char is();
struct dict is_d[];
char ish();
struct dict ish_d[];
char ist();
struct dict ist_d[];
char ite();
struct dict ite_d[];
char ive();
struct dict ive_d[];
char ize();
struct dict ize_d[];
char lar();
struct dict lar_d[];
char less();
struct dict less_d[];
char man();
struct dict man_d[];
char ment();
struct dict ment_d[];
char ness();
struct dict ness_d[];
char ous();
struct dict ous_d[];
char ship();
struct dict ship_d[];
char ss();
struct dict ss_d[];
char ure();
struct dict ure_d[];
char us();
struct dict us_d[];
char ing();
struct dict ing_d[];
char ed();
struct dict ed_d[];
struct fandd {
	char (*fun)();
	struct dict *yd;
} arr[] = {
able,	able_d,
ace,	ace_d,
age,	age_d,
ance,	ance_d,
ant,	ant_d,
cal,	cal_d,
cle,	cle_d,
ee,	ee_d,
ence,	ence_d,
ess,	ess_d,
est,	est_d,
ful,	ful_d,
ible,	ible_d,
ic,	ic_d,
ice,	ice_d,
ion,	ion_d,
ional,	ional_d,
is,	is_d,
ish,	ish_d,
ist,	ist_d,
ite,	ite_d,
ive,	ive_d,
ize,	ize_d,
lar,	lar_d,
less,	less_d,
man,	man_d,
ment,	ment_d,
ness,	ness_d,
ous,	ous_d,
ship,	ship_d,
ss,	ss_d,
ure,	ure_d,
us,	us_d,
ing,	ing_d,
ed,	ed_d,
0,	0
};
egetd(){
	struct dict *pp;
	struct fandd *ptr;
	ptr = arr;
	while(ptr->fun != 0){
		pp = ptr->yd;
		while(pp->entry != 0){
			(*ptr->fun)(pp->entry,0,pp);
			pp++;
		}
		ptr++;
	}
}
char 
aahash(s,ex,aatsize,aapr1,aapr2,tbl,data)
char *s;
struct hnode tbl[];
struct dict *data;
{
	char *cp;
	int i, key, c, p1, p2;
	cp = s;
	key =0;
	while (c = *cp++)
		key = key + (key<<5) + c;
	key &= 077777;
	p1 = key%aapr1;
	p2 = key%aapr2;
	if (p2==0) p2=17;
	for(i=0; i<aatsize; i++)
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
	fprintf(stderr, "hash table full\n");
	exit(1);
}
struct hnode aa1root[43];
#define aa1tsize 43
#define aa1p1 37
#define aa1p2 41
char
able(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa1tsize,aa1p1,aa1p2,aa1root,ptr));
}
struct hnode aa2root[11];
#define aa2tsize 11
#define aa2p1 5
#define aa2p2 7
char
ace(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa2tsize,aa2p1,aa2p2,aa2root,ptr));
}
struct hnode aa3root[61];
#define aa3tsize 61
#define aa3p1 53
#define aa3p2 59
char
age(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa3tsize,aa3p1,aa3p2,aa3root,ptr));
}
struct hnode aa4root[37];
#define aa4tsize 37
#define aa4p1 29
#define aa4p2 31
char
ance(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa4tsize,aa4p1,aa4p2,aa4root,ptr));
}
struct hnode aa5root[31];
#define aa5tsize 31
#define aa5p1 23
#define aa5p2 29
char
ant(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa5tsize,aa5p1,aa5p2,aa5root,ptr));
}
struct hnode aa7root[19];
#define aa7tsize 19
#define aa7p1 13
#define aa7p2 17
char
cal(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa7tsize,aa7p1,aa7p2,aa7root,ptr));
}
struct hnode aa8root[13];
#define aa8tsize 13
#define aa8p1 7
#define aa8p2 11
char
cle(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa8tsize,aa8p1,aa8p2,aa8root,ptr));
}
struct hnode aa10root[31];
#define aa10tsize 31
#define aa10p1 23
#define aa10p2 29
char
ee(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa10tsize,aa10p1,aa10p2,aa10root,ptr));
}
struct hnode aa11root[31];
#define aa11tsize 31
#define aa11p1 23
#define aa11p2 29
char
ence(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa11tsize,aa11p1,aa11p2,aa11root,ptr));
}
struct hnode aa13root[47];
#define aa13tsize 47
#define aa13p1 41
#define aa13p2 43
char
ess(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa13tsize,aa13p1,aa13p2,aa13root,ptr));
}
struct hnode aa14root[67];
#define aa14tsize 67
#define aa14p1 59
#define aa14p2 61
char
est(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa14tsize,aa14p1,aa14p2,aa14root,ptr));
}
struct hnode aa15root[43];
#define aa15tsize 43
#define aa15p1 37
#define aa15p2 41
char
ful(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa15tsize,aa15p1,aa15p2,aa15root,ptr));
}
struct hnode aa18root[19];
#define aa18tsize 19
#define aa18p1 13
#define aa18p2 17
char
ible(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa18tsize,aa18p1,aa18p2,aa18root,ptr));
}
struct hnode aa19root[151];
#define aa19tsize 151
#define aa19p1 139
#define aa19p2 149
char
ic(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa19tsize,aa19p1,aa19p2,aa19root,ptr));
}
struct hnode aa20root[11];
#define aa20tsize 11
#define aa20p1 5
#define aa20p2 7
char
ice(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa20tsize,aa20p1,aa20p2,aa20root,ptr));
}
struct hnode aa21root[89];
#define aa21tsize 89
#define aa21p1 79
#define aa21p2 83
char
ion(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa21tsize,aa21p1,aa21p2,aa21root,ptr));
}
struct hnode aa22root[13];
#define aa22tsize 13
#define aa22p1 7
#define aa22p2 11
char
ional(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa22tsize,aa22p1,aa22p2,aa22root,ptr));
}
struct hnode aa23root[11];
#define aa23tsize 11
#define aa23p1 5
#define aa23p2 7
char
is(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa23tsize,aa23p1,aa23p2,aa23root,ptr));
}
struct hnode aa24root[71];
#define aa24tsize 71
#define aa24p1 61
#define aa24p2 67
char
ish(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa24tsize,aa24p1,aa24p2,aa24root,ptr));
}
struct hnode aa25root[151];
#define aa25tsize 151
#define aa25p1 139
#define aa25p2 149
char
ist(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa25tsize,aa25p1,aa25p2,aa25root,ptr));
}
struct hnode aa26root[79];
#define aa26tsize 79
#define aa26p1 71
#define aa26p2 73
char
ite(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa26tsize,aa26p1,aa26p2,aa26root,ptr));
}
struct hnode aa28root[151];
#define aa28tsize 151
#define aa28p1 139
#define aa28p2 149
char
ive(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa28tsize,aa28p1,aa28p2,aa28root,ptr));
}
struct hnode aa29root[11];
#define aa29tsize 11
#define aa29p1 5
#define aa29p2 7
char
ize(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa29tsize,aa29p1,aa29p2,aa29root,ptr));
}
struct hnode aa30root[37];
#define aa30tsize 37
#define aa30p1 29
#define aa30p2 31
char
lar(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa30tsize,aa30p1,aa30p2,aa30root,ptr));
}
struct hnode aa31root[11];
#define aa31tsize 11
#define aa31p1 5
#define aa31p2 7
char
less(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa31tsize,aa31p1,aa31p2,aa31root,ptr));
}
struct hnode aa33root[11];
#define aa33tsize 11
#define aa33p1 5
#define aa33p2 7
char
man(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa33tsize,aa33p1,aa33p2,aa33root,ptr));
}
struct hnode aa34root[37];
#define aa34tsize 37
#define aa34p1 29
#define aa34p2 31
char
ment(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa34tsize,aa34p1,aa34p2,aa34root,ptr));
}
struct hnode aa35root[11];
#define aa35tsize 11
#define aa35p1 5
#define aa35p2 7
char
ness(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa35tsize,aa35p1,aa35p2,aa35root,ptr));
}
struct hnode aa37root[11];
#define aa37tsize 11
#define aa37p1 5
#define aa37p2 7
char
ous(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa37tsize,aa37p1,aa37p2,aa37root,ptr));
}
struct hnode aa39root[11];
#define aa39tsize 11
#define aa39p1 5
#define aa39p2 7
char
ship(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa39tsize,aa39p1,aa39p2,aa39root,ptr));
}
struct hnode aa40root[61];
#define aa40tsize 61
#define aa40p1 53
#define aa40p2 59
char
ss(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa40tsize,aa40p1,aa40p2,aa40root,ptr));
}
struct hnode aa42root[89];
#define aa42tsize 89
#define aa42p1 79
#define aa42p2 83
char
ure(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa42tsize,aa42p1,aa42p2,aa42root,ptr));
}
struct hnode aa43root[19];
#define aa43tsize 19
#define aa43p1 13
#define aa43p2 17
char
us(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa43tsize,aa43p1,aa43p2,aa43root,ptr));
}
struct hnode aa44root[23];
#define aa44tsize 23
#define aa44p1 17
#define aa44p2 19
char
ing(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa44tsize,aa44p1,aa44p2,aa44root,ptr));
}
struct hnode aa45root[47];
#define aa45tsize 47
#define aa45p1 41
#define aa45p2 43
char
ed(a0,a1,ptr)
	char *a0;
	struct dict *ptr;
{
	return(aahash(a0,a1,aa45tsize,aa45p1,aa45p2,aa45root,ptr));
}
