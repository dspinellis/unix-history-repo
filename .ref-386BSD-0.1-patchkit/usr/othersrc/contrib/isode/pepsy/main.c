/* main.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepsy/RCS/main.c,v 7.4 91/02/22 09:49:05 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/pepsy/RCS/main.c,v 7.4 91/02/22 09:49:05 mrose Interim $
 *
 *
 * $Log:	main.c,v $
 * Revision 7.4  91/02/22  09:49:05  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/23  17:24:34  mrose
 * patches
 * 
 * Revision 7.2  90/11/04  19:18:40  mrose
 * update
 * 
 * Revision 7.1  90/10/17  11:59:49  mrose
 * sync
 * 
 * Revision 7.0  90/07/01  19:54:21  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include	<stdio.h>
#include	<ctype.h>
#include "config.h"
#ifdef	PEPSY_REALS
#include	<math.h>
#define F_SMALL	(1.0e-10)	/* How accurate should reals agree */
#endif
#include	"psap.h"
#include	"T1-types.h"
#include	"T2-types.h"
#include	"T3-types.h"



#include "test_table.h"
	/* compactly generate the new item of data */
#define new(x)	((x *)calloc(1, sizeof (x)))

#define NULLQB	(struct qbuf *)0
char	*fill();
static char   *idname();
static char   *clname();
static int prntbits();
static int pclen();
static int prntos();
static int prntstr();

extern int print_pe();
extern	modtyp	_ZBIT_1;
extern	char	*bitstr2strb();
extern PE mkpelist(), mkpe();
extern OID	mkoid();
extern struct type_UNIV_EXTERNAL *mkext();

extern struct rep_elem	*mkrep_elem();
extern struct rep_int	*mkrep_int();

extern void	exit();

static int	t_test = 1;	/* Iteration of the test */

#define NullParm	((char	*) 0)
/*ARGSUSED*/
main(argc, argv)
int	argc;
char	**argv;
{
    int	i;
    int succ, fail;

    succ = fail = 0;
    if (argc <= 1) {
	for (i = MAXTCASE - 1; i >= 0; i--) {
	    if (t_case[i].tst_entry < 0)
		continue;
	    for (t_test = t_case[i].tst_tests; t_test > 0; t_test--) {
		if (ed_tst(i) != OK) {
		    (void) printf("Failed\n");
		    fail++;
		} else
		    succ++;
	    }
	}
	if (fail > 0)
	    (void) printf("Failed\n");
	(void) printf("Total %d  = %d successes %d failures\n", fail + succ, succ, fail);
#ifdef	PEP_TEST
	tailorfree();
#endif
    } else {
	i = atoi(argv[1]);
	if (argc > 2)
	    t_test = atoi(argv[2]);
	(void) printf("Test %d\n", i);
	if (ed_tst(i) != OK)
	    (void) printf("Failed\n");
#ifdef	PEP_TEST
	tailorfree();
#endif
    }
    return (fail);

}

/*
 * Perform a test of encoding/decoding on type number tynum
 */
ed_tst(tynum)
int	tynum;
{
    PE	pe;
    char *parm1, *parm2;

    if (tynum < 0 || tynum >= MAXTCASE)
	    ferrd(1, "ed_tst illegal type %d\n", tynum);
    

    if ((parm1 = fill(tynum)) == NULL) {
	ferr(1, "calloc did not work\n");
    }

#define encode(tynum, x, parm)	\
	enc_f(t_case[tynum].tst_entry, &_ZT1_mod, (x), 1, 0, NULLCP, (parm))

    if (encode(tynum, &pe, parm1) == NOTOK) {
	(void) printf("encode:failed on %s: %s\n", t_case[tynum].tst_name,
	     PY_pepy);
	return(NOTOK);
    }
	

#define decode(tynum, x, parm)	\
	dec_f(t_case[tynum].tst_entry, &_ZT1_mod, (x), 1, (int *) 0, (char **)NULL, (parm))

    if (decode(tynum, pe, &parm2) == NOTOK) {
	    (void) printf("decode:failed on %s: %s\n", t_case[tynum].tst_name,
		 PY_pepy);
	    return (NOTOK);
    }
	
    if (tcmp(tynum, parm1, parm2)) {
	(void) printf("%s: not transfered properly\n", t_case[tynum].tst_name);
	return (NOTOK);
    }

#if PRNT
#define print(tynum, x)	\
    prnt_f(t_case[tynum].tst_entry, &_ZT1_mod, (x), 1, (int *) 0, (char **)0)

    (void) printf("\n\"%s\" t_test = %d\n", t_case[tynum].tst_name, t_test);
    if (print(tynum, pe) == NOTOK) {
	    (void) printf("Print:failed on %s: %s\n", t_case[tynum].tst_name,
		 PY_pepy);
	    exit(2);
    }
#endif
	


#define fre_space(tynum, parm)	\
	fre_obj(parm, _ZT1_mod.md_dtab[t_case[tynum].tst_entry], &_ZT1_mod, 1)

    if (fre_space(tynum, parm1) != OK)
	return (NOTOK);
    if (fre_space(tynum, parm2) != OK)
	return (NOTOK);

    if (pe)
	pe_free(pe);

    return (OK);
}

    /*SUPPRESS 218*/
/*
 * fill in some test data for the given type
 */
char	*
fill(tynum)
int	tynum;
{
    char	*parm;
    static int count;
    int		i;
    char	*p;

    if ((parm = calloc(1, t_case[tynum].tst_size)) == NULL) {
	ferr(1, "calloc did not work\n");
    }

    switch (tynum) {
    case TY_MPDU:
#define Xparm ((struct type_T1_MPDU *)parm)
	if ((Xparm->a__seq = new(struct element_T1_1)) == NULL) {
		(void) printf("calloc did not work\n");
		return NULL;
	}
	Xparm->a__seq->fred = 10;
	Xparm->a__seq->george = 1;
	break;
#undef Xparm
    
    case TY_EMBEDDED:
#define Xparm ((struct type_T1_Embedded *)parm)
	Xparm->anMPDU = (struct type_T1_MPDU *)fill(TY_MPDU);
	Xparm->ei = 6966;
	break;
#undef Xparm

    case TY_STRINGS:
#define Xparm	((struct type_T1_Strings *)parm)
	Xparm->ostring = str2qb("12\376\0\377a6", 7, 1);
	Xparm->bstring = strb2bitstr("\270\017\010\020\040\100", 50, 0, 0);
	Xparm->nstring = str2qb("123456", 6, 1);
	Xparm->pstring = str2qb("hello, world", 12, 1);
	Xparm->tstring = str2qb("teletex", 7, 1);
	Xparm->t61string = str2qb("T.61", 4, 1);
	Xparm->vstring = str2qb("visible", 7, 1);
	Xparm->vis__string = str2qb("visible again", 13, 1);
	Xparm->i646string = str2qb("who knows what this is", 22, 1);
	Xparm->ia5string = str2qb("This is ASCII\n\r", 15, 1);
	Xparm->graphstring = str2qb("This is a graphic string", 24, 1);
	Xparm->genstring = str2qb("This is a general string", 24, 1);
	break;
#undef Xparm

    case TY_EMB_STRINGS:
#define Xparm	((struct type_T1_Emb__Strings *)parm)
	Xparm->atest = 2001;
	Xparm->ctest = strb2bitstr("\03\010\252\125", 24, 0, 0);
	Xparm->btest = str2qb("Good bye", 8, 1);
	Xparm->big__test = (struct type_T1_Strings *)fill(TY_STRINGS);
	if ((Xparm->emb__test = new(struct element_T1_0)) == NULL) {
		(void) printf("calloc did not work\n");
		return NULL;
	}
	Xparm->emb__test->em__int = -101;
	Xparm->emb__test->em__oct = str2qb("Embedded octet string", 21, 1);
	Xparm->emb__test->em__bit = strb2bitstr("\377\252\125\370\01", 40, 0,0);
	break;
#undef Xparm

    case TY_IMPLICIT:
#define Xparm	((struct type_T1_Impl__Tags *)parm)
	Xparm->i__impl = -1;
	Xparm->o__impl = str2qb("I'm an implicit tagged octet string", 36, 1);
	Xparm->b__impl = strb2bitstr("\0\0\01\230\01", 40, 0, 0);
	Xparm->f__impl = 0xff; /* True */
	Xparm->obj__impl = (struct type_T1_Emb__Strings *)fill(TY_EMB_STRINGS);
	if ((Xparm->i__emb__test = new(struct element_T1_2)) == NULL) {
		(void) printf("calloc did not work\n");
		return NULL;
	}
	Xparm->i__emb__test->i__em__int = -101;
	Xparm->i__emb__test->i__em__oct = str2qb("Implicit Embedded", 18, 1);
	Xparm->i__emb__test->i__em__bit = strb2bitstr("\200\200\200\200", 32, 0, 0);
#undef Xparm
	break;

    case TY_EXPLICIT:
#define Xparm	((struct type_T1_Expl__Tags *)parm)
	Xparm->i__expl = 35051;
	Xparm->o__expl = str2qb("explicit tagged octet string", 28, 1);
	Xparm->b__expl = strb2bitstr("\070\070\070\077", 32, 0, 0);
	Xparm->f__expl = 0xf0; /* True */
	Xparm->obj__expl = (struct type_T1_Emb__Strings *)fill(TY_EMB_STRINGS);
	if ((Xparm->i__exp__test = new(struct element_T1_3)) == NULL) {
		(void) printf("calloc did not work\n");
		return NULL;
	}
	Xparm->i__exp__test->i__ex__int = -9;
	Xparm->i__exp__test->i__ex__oct = str2qb("Explicit Embedded", 18, 1);
	Xparm->i__exp__test->i__ex__bit = strb2bitstr("\03\03\03\03\277", 40, 0, 0);
#undef Xparm
	break;
	
    case TY_SEQOF:
#define Xparm	((struct type_T1_Seqof__Test *)parm)
    count = 2;
    Xparm->sqof__test1 = (struct element_T1_4 *) fill(TY_ELEMENT4);
    count = 2;
    Xparm->stof__test1 = (struct member_T1_2 *) fill(TY_MEMBER2);
    Xparm->i__test1 = 33;
    count = 2;
    Xparm->sqof__test2 = (struct element_T1_6 *) fill(TY_ELEMENT6);
    count = 2;
    Xparm->stof__test2 = (struct member_T1_4 *) fill(TY_MEMBER4);
    Xparm->i__test2 = 99;
    break;
#undef Xparm
	
    case TY_ELEMENT4:
#define Xparm	((struct element_T1_4 *)parm)
    Xparm->element_T1_5 = (struct type_T1_Expl__Tags *)fill(TY_EXPLICIT);
    if (count-- > 0)
	Xparm->next = (struct element_T1_4 *) fill(TY_ELEMENT4);
    else
	Xparm->next = NULL;
    break;
#undef Xparm
	
    case TY_MEMBER2:
#define Xparm	((struct member_T1_2 *)parm)
    Xparm->member_T1_3 = (struct type_T1_Expl__Tags *)fill(TY_EXPLICIT);
    if (count-- > 0)
	Xparm->next = (struct member_T1_2 *) fill(TY_MEMBER2);
    else
	Xparm->next = NULL;
    break;
#undef Xparm

    case TY_ELEMENT6:
#define Xparm	((struct element_T1_6 *)parm)
    Xparm->element_T1_7 = (struct element_T1_8 *)fill(TY_ELEMENT8);
    if (count-- > 0)
	Xparm->next = (struct element_T1_6 *) fill(TY_ELEMENT6);
    else
	Xparm->next = NULL;
    break;
#undef Xparm

    case TY_ELEMENT8:
#define Xparm	((struct element_T1_8 *)parm)
    Xparm->sqof__in = (struct type_T1_Expl__Tags *)fill(TY_EXPLICIT);
    Xparm->sqof__i = 212121;
    Xparm->sqof__o = str2qb("Element8 Embedded", 18, 1);
    break;
#undef Xparm

    case TY_MEMBER4:
#define Xparm	((struct member_T1_4 *)parm)
    Xparm->member_T1_5 = (struct element_T1_9 *)fill(TY_ELEMENT9);
    if (count-- > 0)
	Xparm->next = (struct member_T1_4 *) fill(TY_MEMBER4);
    else
	Xparm->next = NULL;
    break;
#undef Xparm

    case TY_ELEMENT9:
#define Xparm	((struct element_T1_9 *)parm)
	Xparm->stof__in = (struct type_T1_Expl__Tags *)fill(TY_EXPLICIT);
	Xparm->stof__i = -12345;
	Xparm->stof__o = str2qb("XYZabcde Embedded", 18, 1);
    break;
#undef Xparm

    case TY_CHOICE:
#define Xparm	((struct type_T1_Choice__Test *)parm)
	Xparm->c1 = (struct choice_T1_0 *) fill(TY_CHOICE0);
	Xparm->c2 = (struct choice_T1_1 *) fill(TY_CHOICE1);
	Xparm->c3 = (struct choice_T1_2 *) fill(TY_CHOICE2);
	Xparm->c4 = (struct element_T1_11 *) fill(TY_ELEMENT11);
	break;
#undef Xparm

    case TY_CHOICE0:
#define Xparm	((struct choice_T1_0 *)parm)
	
	switch (Xparm->offset = (t_test + 10)%choice_T1_0_obj__c1 + 1) {
	case choice_T1_0_i__c1:
	    Xparm->un.i__c1 = 10101;
	    break;
	
	case choice_T1_0_o__c1:
	    Xparm->un.o__c1 = str2qb("Andrew Worsley!!!", 18, 1);
	    break;
	
	case choice_T1_0_b__c1:
	    Xparm->un.b__c1 = strb2bitstr("\02\02\02\07\077", 40, 0, 0);
	    break;
	
	case choice_T1_0_f__c1:
	    Xparm->un.f__c1 = 0x11;
	    break;
	
	case choice_T1_0_obj__c1:
	    Xparm->un.obj__c1 = (struct type_T1_Emb__Strings *) fill(TY_EMB_STRINGS);
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE0:illegal offset %d\n", Xparm->offset);
	}
	break;
#undef Xparm

    case TY_CHOICE1:
#define Xparm	((struct choice_T1_1 *)parm)
	
	switch (Xparm->offset = (t_test + 11)%choice_T1_1_obj__c2 + 1) {
	case choice_T1_1_i__c2:
	    Xparm->un.i__c2 = 1212;
	    break;
	
	case choice_T1_1_o__c2:
	    Xparm->un.o__c2 = str2qb("Richard Worsley!!", 18, 1);
	    break;
	
	case choice_T1_1_b__c2:
	    Xparm->un.b__c2 = strb2bitstr("\02\01\01\07\077", 40, 0, 0);
	    break;
	
	case choice_T1_1_f__c2:
	    Xparm->un.f__c2 = 0x12;
	    break;
	
	case choice_T1_1_obj__c2:
	    Xparm->un.obj__c2 = (struct type_T1_Emb__Strings *) fill(TY_EMB_STRINGS);
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE1:illegal offset %d\n", Xparm->offset);
	}
	break;
#undef Xparm

    case TY_CHOICE2:
#define Xparm	((struct choice_T1_2 *)parm)
	
	switch (Xparm->offset = (t_test + 12)%choice_T1_2_i__c3 + 1) {
	case choice_T1_2_i__c3:
	    Xparm->un.i__c3 = 689364;
	    break;
	
	case choice_T1_2_seq__c3:
	    Xparm->un.seq__c3 = (struct element_T1_10 *) fill(TY_ELEMENT10);
	    break;
	
	case choice_T1_2_set__c3:
	    Xparm->un.set__c3 = (struct member_T1_6 *) fill(TY_MEMBER6);
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE2:illegal offset %d\n", Xparm->offset);
	}
	break;
#undef Xparm

    case TY_CHOICE3:
#define Xparm	((struct choice_T1_3 *)parm)
	
	switch (Xparm->offset = (t_test + 13)%choice_T1_3_sc__b__i + 1) {
	case choice_T1_3_sc__a__i:
	    Xparm->un.sc__a__i = 16891;
	    break;
	
	case choice_T1_3_sc__b__i:
	    Xparm->un.sc__b__i = 13151;
	    break;
	
	case choice_T1_3_c4__i:
	    Xparm->un.c4__i = 10938;
	    break;
	
	case choice_T1_3_c4__obj:
	    Xparm->un.c4__obj = (struct type_T1_Expl__Tags *) fill(TY_EXPLICIT);
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE3:illegal offset %d\n", Xparm->offset);
	}
	break;
#undef Xparm

    case TY_ELEMENT10:
#define Xparm	((struct element_T1_10 *)parm)
	Xparm->seq__c3__in = (struct type_T1_Expl__Tags *) fill(TY_EXPLICIT);
	Xparm->seq__c3__i = 40938;
	Xparm->seq__c3__o = str2qb("James Worsley!!!!", 18, 1);
	break;
#undef Xparm

    case TY_MEMBER6:
#define Xparm	((struct member_T1_6 *)parm)
	Xparm->set__c3__in = (struct type_T1_Expl__Tags *) fill(TY_EXPLICIT);
	Xparm->set__c3__i = 0x40938;
	Xparm->set__c3__o = str2qb("Patrick Worsley!!", 18, 1);
	break;
#undef Xparm

    case TY_ELEMENT11:
#define Xparm	((struct element_T1_11 *)parm)
	Xparm->c4__choice = (struct choice_T1_3 *) fill(TY_CHOICE3);
	break;
#undef Xparm

    case TY_OPTIONAL:
#define Xparm	((struct type_T1_Opt__Strings *)parm)
        if (t_test & opt_T1_Opt__Strings_a__opt) {
	    Xparm->optionals |= opt_T1_Opt__Strings_a__opt;
	    Xparm->a__opt = 192837;
	}
        if (t_test & opt_T1_Opt__Strings_d__opt) {
	    Xparm->optionals |= opt_T1_Opt__Strings_d__opt;
	    Xparm->d__opt = 1;
	}
	Xparm->b__opt = str2qb("Susan Hannah Sibel", 19, 1);
        if (t_test*(t_test + 1) & opt_T1_Opt__Strings_d__opt) {
	    Xparm->c__opt = strb2bitstr("\012\017\02\07\077", 40, 0, 0);
	}
        if (t_test & opt_T1_Opt__Strings_e__opt) {
	    Xparm->optionals |= opt_T1_Opt__Strings_e__opt;
	    Xparm->e__opt = 0;
	}
        if ((t_test + 12)*t_test & opt_T1_Opt__Strings_a__opt) {
	    Xparm->big__opt = (struct type_T1_Strings *) fill(TY_STRINGS);
	}
        if ((t_test + 2)*t_test & opt_T1_Opt__Strings_a__opt) {
	    Xparm->emb__opt = (struct element_T1_12 *) fill(TY_ELEMENT12);
	}
        if ((t_test + 4)*t_test & opt_T1_Opt__Strings_a__opt) {
	    Xparm->st__opt = (struct member_T1_7 *) fill(TY_MEMBER7);
	}
        if ((t_test + 8)*t_test & opt_T1_Opt__Strings_a__opt) {
	    Xparm->obj__opt = (struct type_T1_MPDU *) fill(TY_MPDU);
	}
        if (t_test & opt_T1_Opt__Strings_etag__opt) {
	    Xparm->optionals |= opt_T1_Opt__Strings_etag__opt;
	    Xparm->etag__opt = 2983461;
	}
        if ((t_test + 6)*t_test & opt_T1_Opt__Strings_a__opt) {
	    Xparm->ch__opt = (struct choice_T1_4 *) fill(TY_CHOICE4);
	}
	break;
#undef Xparm

     case TY_ELEMENT12:
#define Xparm	((struct element_T1_12 *)parm)
        if ((t_test + 10)*t_test & opt_T1_element_T1_12_oem__int) {
	    Xparm->optionals |= opt_T1_element_T1_12_oem__int;
	    Xparm->oem__int = 197336;
	}
        if (t_test*(t_test + 22) & opt_T1_Opt__Strings_a__opt) {
	    Xparm->oem__oct = str2qb("Ling Worsley", 13, 1);
	}
        if (t_test*(t_test + 16) & opt_T1_Opt__Strings_d__opt) {
	    Xparm->oem__bit = strb2bitstr("\0142\0117\02\017\07", 40, 0, 0);
	}
        break;
#undef Xparm

     case TY_MEMBER7:
#define Xparm	((struct member_T1_7 *)parm)
        if ((t_test + 12)*t_test & opt_T1_member_T1_7_st__int0) {
	    Xparm->optionals |= opt_T1_member_T1_7_st__int0;
	    Xparm->st__int0 = 85659;
	}
        if ((t_test + 12)*t_test & opt_T1_member_T1_7_st__int1) {
	    Xparm->optionals |= opt_T1_member_T1_7_st__int1;
	    Xparm->st__int1 = 664388;
	}
        if ((t_test + 12)*t_test & opt_T1_member_T1_7_st__int2) {
	    Xparm->optionals |= opt_T1_member_T1_7_st__int2;
	    Xparm->st__int2 = 967768;
	}
        break;
#undef Xparm

    case TY_CHOICE4:
#define Xparm	((struct choice_T1_4 *)parm)
	
	switch (Xparm->offset = (t_test + 14)%choice_T1_4_ch__2 + 1) {
	case choice_T1_4_ch__1:
	    Xparm->un.ch__1 = 74576;
	    break;
	
	case choice_T1_4_ch__2:
	    Xparm->un.ch__2 = 28828;
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE4:illegal offset %d\n", Xparm->offset);
	}
	break;
#undef Xparm

     case TY_EXTREF:
#define Xparm	((struct type_T1_E__ref *)parm)
	Xparm->a__ref = (struct type_T2_Info *) fill(TY_T2_INFO);
	Xparm->b__ref = (struct type_T2_Info *) fill(TY_T2_INFO);
	Xparm->c__ref = (struct type_T1_Choice__Test *) fill(TY_CHOICE);
        if ((t_test + 20)*t_test & 0x2) {
	    Xparm->d__ref = (struct type_T2_Info *) fill(TY_T2_INFO);
	}
        if ((t_test + 20)*t_test & 0x1) {
	    Xparm->e__ref = (struct type_T2_Info *) fill(TY_T2_INFO);
	}
        break;
#undef Xparm

     case TY_T2_INFO:
#define Xparm	((struct type_T2_Info *)parm)
	Xparm->a1 = 101910;
	Xparm->a2 = -304985;
	Xparm->a3 = 13;
	Xparm->a4 = (struct type_T2_MPDU *) fill(TY_T2_MPDU);
        break;
#undef Xparm

     case TY_T2_MPDU:
#define Xparm	((struct type_T2_MPDU *)parm)
	Xparm->a__seq = (struct element_T2_0 *) fill(TY_T2_ELEM0);
        break;
#undef Xparm

     case TY_T2_ELEM0:
#define Xparm	((struct element_T2_0 *)parm)
	Xparm->fred = 998877;
        break;
#undef Xparm

     case TY_OPTIMISED:
#define Xparm	((struct type_T1_Optimised *)parm)
	Xparm->o1 = strb2bitstr("\0241\0227\032\011\0237", 40, 0, 0);
	Xparm->o2 = str2qb("Peckling Worsley!!!!", 20, 1);
	Xparm->o3 = (struct type_T1_MPDU *) fill(TY_MPDU);
	Xparm->o4 = (PE ) mkpelist(t_test);
	Xparm->element_T1_14  = (struct member_T1_9 *) fill(TY_MEMBER9);
	break;
#undef Xparm

     case TY_MEMBER9:
#define Xparm	((struct member_T1_9 *)parm)
	Xparm->o5 = mkpelist(t_test);
	Xparm->o6 = mkpelist(t_test);
	Xparm->o7 = mkoid(t_test);
	break;
#undef Xparm

    case TY_EXTERNAL:
#define Xparm	((struct type_T1_Ext__typ *)parm)
        Xparm->ext = mkext(t_test*8 + 1);
        Xparm->a__ny = mkpe(t_test*8 + 1);
        Xparm->ext__impl = mkext(t_test*6 + 1);
        Xparm->any__impl = mkpe(t_test*8 + 3);
        Xparm->ext__expl = mkext(t_test*4 + 1);
        Xparm->any__expl = mkpe(t_test*8 + 5);
	break;
#undef Xparm

    case TY_SEXTERNAL:
#define Xparm	((struct type_T1_SExt *)parm)
	free(parm);	/* don't need it */
	Xparm = mkext(21);
	break;
#undef Xparm

    case TY_ETAGOBJ:
#define Xparm	((struct type_T1_Etags *)parm)
	switch (Xparm->offset = (t_test + 4)%type_T1_Etags_bE + 1) {
	case type_T1_Etags_aE:
	    Xparm->un.aE = 10283;
	    break;
	
	case type_T1_Etags_bE:
	    Xparm->un.bE = 40986;
	    break;
	
	default:
	    ferrd(1, "TY_ETAGOBJ:illegal offset %d\n", Xparm->offset);
	}
	break;
#undef Xparm

/* This has to be changed when posy is upgraded to handle DEFAULTS properly */
    case TY_DEFAULT:
#define Xparm	((struct type_T1_Def__Strings *)parm)
	if (t_test*t_test & 2)
	    Xparm->a__def = int_T1_a__def_a__def__0;
	else
	    Xparm->a__def = int_T1_a__def_a__def__1;
	if (t_test*t_test & 4)
	    Xparm->b__def = str2qb("Susan Sibel !!!!", 17, 1);
	else if (t_test*t_test & 8)
	    Xparm->b__def = str2qb("hello, world", 12, 1);
	else
	    Xparm->b__def = NULLQB;
	if (t_test*t_test & 8)
	    Xparm->c__def = strb2bitstr(int2strb(bit_T1_c__def_c__def__two, 9),
		9, 0, 0);
	else
	    Xparm->c__def = NULLPE;
	if (t_test*t_test & 0x10)
	    Xparm->okay = 0;
	else
	    Xparm->okay = 1;
	if (t_test*t_test & 0x20)
	    Xparm->e__def = 0;
	else
	    Xparm->e__def = 1;
	Xparm->big__def = (struct type_T1_Strings *) fill(TY_STRINGS);
	if (t_test*t_test*t_test & 0x10)
	    Xparm->emb__def = NULL;
	else
	    Xparm->emb__def = (struct element_T1_13 *) fill(TY_ELEMENT13);
	if (t_test*t_test*t_test & 0x20)
	    Xparm->st__def = NULL;
	else
	    Xparm->st__def = (struct member_T1_8 *) fill(TY_MEMBER8);
	break;
#undef Xparm

    case TY_ELEMENT13:
#define Xparm ((struct element_T1_13 *)parm)
	if (t_test*t_test*t_test & 1)
	    Xparm->colour = int_T1_colour_green;
	else
	    Xparm->colour = int_T1_colour_red;
	if (t_test*t_test & 040)
	    Xparm->oem__oct = str2qb("Julia Dzuikas !!!!", 19, 1);
	else
	    Xparm->oem__oct = NULLQB;
	if (t_test*t_test*t_test & 2)
	    Xparm->version = strb2bitstr(int2strb((1 << bit_T1_version_basic)
					     |(1 << bit_T1_version_patch1), 3),
					     3, 0, 0);
	else
	    Xparm->version = strb2bitstr(int2strb((1 << bit_T1_version_basic),
						    3), 3, 0, 0);
	break;
#undef Xparm

    case TY_MEMBER8:
#define Xparm ((struct member_T1_8 *)parm)
	if (t_test*t_test*t_test & 4)
	    Xparm->wine = int_T1_wine_burgundy;
	else
	    Xparm->wine = int_T1_wine_claret;
	if (t_test*t_test*t_test & 010)
	    Xparm->beer = int_T1_beer_vb;
	else
	    Xparm->beer = int_T1_beer_hieneken;
	if (t_test*t_test*t_test & 020)
	    Xparm->spirit = int_T1_spirit_vodka;
	else
	    Xparm->spirit = int_T1_spirit_brandy;
	break;
#undef Xparm

    case TY_STEST:
#define Xparm ((struct type_T1_Stest *)parm)
	if (t_test*(t_test + 2)*t_test & 4)
	    Xparm->st1 = (struct type_T1_Sint *)fill(TY_SINT);
	else
	    Xparm->st1 = (struct type_T1_Sint *)NULL;
	if (t_test*(t_test + 2)*t_test & 010)
	    Xparm->st2 = str2qb("goodbye, world", 14, 1);
	else
	    Xparm->st2 = str2qb("xxxxxxx, world", 14, 1);
	break;
#undef Xparm

    case TY_SINT:
#define Xparm ((struct type_T1_Sint *)parm)
	if (t_test*(t_test + 4)*t_test & 4)
	    Xparm->parm = 33;
	else
	    Xparm->parm = 44;
	break;
#undef Xparm

    case TY_ETYPE:
#define Xparm ((struct type_T1_Enum__type *)parm)
	switch (t_test & 3) {
	case 0:
	    Xparm->parm = int_T1_Enum__type_pork;
	    break;

	case 1:
	    Xparm->parm = int_T1_Enum__type_beef;
	    break;


	case 2:
	    Xparm->parm = int_T1_Enum__type_chicken;
	    break;


	case 3:
	    Xparm->parm = int_T1_Enum__type_lamb;
	    break;

	default:
	    ferrd(1, "fill:ETYPE: Self consistency failure\n", t_test);
	}
	break;
#undef Xparm

    case TY_ENUM_TEST:
#define Xparm ((struct type_T1_T__enum *)parm)
	i = t_test;
	Xparm->ae1 = (struct type_T1_Enum__type *)fill(TY_ETYPE);
	t_test++;
	Xparm->ae2 = (struct type_T1_Enum__type *)fill(TY_ETYPE);
	t_test++;
	Xparm->ae3 = (struct type_T1_Enum__type *)fill(TY_ETYPE);
	t_test++;
	if (t_test & 1)
	    Xparm->ae5 = (struct type_T1_Enum__type *)fill(TY_ETYPE);
	Xparm->ae4 = (struct type_T1_Enum__type *)fill(TY_ETYPE);
	t_test++;
	if (t_test & 2)
	    Xparm->ae4->parm = int_T1_Enum__type_chicken; /* Default */
	t_test = i;
	break;
#undef Xparm

#ifdef	PEPSY_REALS
    case TY_REAL:
#define Xparm	((struct type_T1_Real *)parm)
	switch (t_test % 3) {
	case 0:
	    Xparm->parm = -2.28789;
	    break;
	
	case 1:
	    Xparm->parm = 927639.98009;
	    break;

	case 2:
	    Xparm->parm = 0.0;
	    break;

	default:
	ferrd(1, "fill:TY_REAL:Internal error %d\n", t_test);
        }
        break;
#undef Xparm

    case TY_REAL_TEST:
#define Xparm	((struct type_T1_T__real *)parm)
	i = t_test;
        Xparm->r1 = (struct type_T1_Real *) fill(TY_REAL);
	t_test++;
        Xparm->r2 = (struct type_T1_Real *) fill(TY_REAL);
	t_test++;
        Xparm->r3 = (struct type_T1_Real *) fill(TY_REAL);
	t_test++;

	Xparm->r4 = (struct type_T1_Real *) fill(TY_REAL);
	t_test++;
	if (i & 1)	/* Default */
	    Xparm->r4->parm = 3.1415962;

	if (i & 2) {	/* Optional */
	    Xparm->r5 = (struct type_T1_Real *) fill(TY_REAL);
	}
	t_test = i;
	break;
#undef	Xparm

/* all strings have to be malloc'ed out ! */
    case TY_OPTPEPY:
    case TY_DEFPEPY:
#define Xparm	((struct pepy_refs1 *)parm)
        Xparm->t_int = 12354 + t_test;
        Xparm->t_int1 = 12354 + t_test;
        Xparm->t_enum = t_test + 1;
        Xparm->t_enum1 = t_test + 1;
        Xparm->t_qbuf = str2qb("Some is out there!", 18, 1);
        Xparm->t_qbuf1 = str2qb("Some is out there!", 18, 1);
        Xparm->t_string = strdup("hello, world\n");
        Xparm->t_string1 = strdup("hello, world\n");
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_ostring = p;
        Xparm->t_olen = t_test + 4;
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_ostring1 = p;
        Xparm->t_olen1 = t_test + 4;
        Xparm->t_bool = t_test % 2;
        Xparm->t_bool1 = t_test % 2;
        Xparm->t_real = sqrt(t_test * 3.1415962);
        Xparm->t_real1 = sqrt(t_test * 3.1415962);
        Xparm->t_oid = mkoid(t_test);
    	if (tynum != TY_OPTPEPY)
	    Xparm->t_oid1 = mkoid(t_test);
	Xparm->t_pe = strb2bitstr("\0221\0327\052\211\0237\200", 41, 0, 0);
	Xparm->t_pe1 = strb2bitstr("\0221\0327\052\211\0237\200", 41, 0, 0);
        Xparm->t_any = mkpe(t_test*8 + 3);
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_bstring = p;
        Xparm->t_blen = t_test*8 + 8;
	break;
#undef	Xparm

    case TY_PEPY:
#define Xparm	((struct pepy_refs *)parm)
        Xparm->t_int = 12354 + t_test;
        Xparm->t_enum = t_test + 1;
        Xparm->t_qbuf = str2qb("Some is out there!", 18, 1);
        Xparm->t_string = strdup("hello, world\n");
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_ostring = p;
        Xparm->t_olen = t_test + 4;
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_bstring = p;
        Xparm->t_blen = t_test*8 + 4;
        Xparm->t_bool = t_test % 2;
        Xparm->t_real = sqrt(t_test * 3.1415962);
        Xparm->t_oid = mkoid(t_test);
	Xparm->t_pe = strb2bitstr("\0221\0327\052\011\0237\200", 41, 0, 0);
        Xparm->t_any = mkpe(t_test*8 + 3);
        Xparm->t_def = (struct pepy_refs1 *)fill(TY_DEFPEPY);
        Xparm->t_opt = (struct pepy_refs1 *)fill(TY_OPTPEPY);
	for (i = NUMOPT - 1; i >= 0; i--)
	     if ((t_test + i) % 2 == 1)
		 BITSET(Xparm->t_opt->opt_set, i);
	     else
		 BITCLR(Xparm->t_opt->opt_set, i);
	break;
#undef	Xparm

    case TY_S_COMPD:
#define Xparm	((struct pepy_refs1 *)parm)
        Xparm->t_int = 12354 + t_test;
        Xparm->t_enum = t_test + 1;
        Xparm->t_int1 = 12354 + t_test;
        Xparm->t_enum1 = t_test + 1;
	for (i = NUMOPT - 1; i >= 0; i--) {
	    if (i != OPT_INT1 && i != OPT_INT2
	    && i != OPT_ENUM1 && i != OPT_ENUM2)
		continue;

	     if ((t_test + i) % 2 == 1)
		 BITSET(Xparm->opt_set, i);
	     else
		 BITCLR(Xparm->opt_set, i);
	}
	break;
#undef	Xparm

#endif

    case TY_ACTION:
    case TY_REPEAT:
#define Xparm	((struct repeats *)parm)
	Xparm->rp_sq1 = mkrep_int(t_test - 1);
	Xparm->rp_sq2 = mkrep_elem(t_test - 1);
	Xparm->rp_st1 = mkrep_int(t_test - 1);
	Xparm->rp_st2 = mkrep_elem(2*t_test - 1);
	switch (t_test % 3) {
	case 0:
	    Xparm->rp_choice = RP_INT;
	    Xparm->rp_int = t_test*t_test - 10;
	    break;

	case 1:
	    Xparm->rp_choice = RP_BOOL;
	    Xparm->rp_bool = (t_test + 1) % 2;
	    break;

	case 2:
	    Xparm->rp_choice = RP_OSTRING;
	    Xparm->rp_ostring = strdup("lets go home");
	    Xparm->rp_int = strlen(Xparm->rp_ostring);
	    break;

	default:
	    ferrd(1, "fill:TY_REPEAT:Internal error %d\n", t_test);
	}
	break;
#undef	Xparm

    case TY_VPDEFINED:
#define Xparm	((struct pepy_refs *)parm)
        Xparm->t_int = 9354 + t_test;
        Xparm->t_enum = t_test + 1;
        Xparm->t_qbuf = str2qb("Some one is out there!", 22, 1);
        Xparm->t_string = strdup("hello there, world\n");
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_ostring = p;
        Xparm->t_olen = t_test + 4;
	p = malloc(30);
	bcopy("\0001234567890", p, 30);
        Xparm->t_bstring = p;
        Xparm->t_blen = t_test*8 + 4;
	Xparm->t_pe = strb2bitstr("\0221\0327\052\211\0237\200", t_test + 33, 0, 0);
        Xparm->t_bool = t_test % 2;
        Xparm->t_real = sqrt((float )t_test);
        Xparm->t_oid = mkoid(t_test);
        Xparm->t_any = mkpe(t_test*7 + 3);
	break;
#undef	Xparm

    case TY_FUNC:
#define Xparm	((struct codedata *)parm)
        Xparm->cd_a = mkpe(t_test + 2);
        Xparm->cd_b = mkpe(t_test + 3);
        Xparm->cd_c = mkpe(t_test + 5);
        Xparm->cd_d = mkpe(t_test + 7);
        Xparm->cd_int = t_test - 1;
	if (t_test % 2)
	    Xparm->cd_string = strdup("hello there, world\n");
	else
	    Xparm->cd_string = strdup("hello world\n");
	for (i = NCD_OPT - 1; i >= 0; i--) {
	     if ((t_test + i) % 2 == 1)
		 BITSET(Xparm->cd_opt_set, i);
	     else
		 BITCLR(Xparm->cd_opt_set, i);
	}
	Xparm->cd_left = (struct codedata *)fill(TY_OPTFUNC);
	Xparm->cd_right = (struct codedata *)fill(TY_DFTFUNC);
	break;
#undef	Xparm

    case TY_OPTFUNC:
#define Xparm	((struct codedata *)parm)
	if ((t_test + 2) % 3)
	    Xparm->cd_a = mkpe(t_test + 2);
	if ((t_test + 1) % 3)
	    Xparm->cd_b = mkpe(t_test + 3);
	Xparm->cd_c = mkpe(t_test + 5);
	Xparm->cd_d = mkpe(t_test + 7);
        Xparm->cd_int = t_test - 1;
	if (t_test % 2)
	    Xparm->cd_string = strdup("hello there, world\n");
	else
	    Xparm->cd_string = NULLCP;
	for (i = NCD_OPT - 1; i >= 0; i--) {
	     if ((t_test + i) % 2 == 1)
		 BITSET(Xparm->cd_opt_set, i);
	     else
		 BITCLR(Xparm->cd_opt_set, i);
	}
	break;
#undef	Xparm

    case TY_DFTFUNC:
#define Xparm	((struct codedata *)parm)
	if ((t_test + 2) % 3)
	    Xparm->cd_a = mkpe(t_test*2 + 1);
	if ((t_test + 1) % 3)
	    Xparm->cd_b = mkpe(t_test*2 + 2);
        Xparm->cd_c = mkpe(t_test*2 + 3);
        Xparm->cd_d = mkpe(t_test*2 + 4);
        Xparm->cd_int = t_test - 2;
	if (t_test % 2)
	    Xparm->cd_string = strdup("hello there, world\n");
	else
	    Xparm->cd_string = strdup("Some is out");
	for (i = NCD_OPT - 1; i >= 0; i--) {
	     if ((t_test + i) % 2 == 1)
		 BITSET(Xparm->cd_opt_set, i);
	     else
		 BITCLR(Xparm->cd_opt_set, i);
	}
	break;
#undef	Xparm

#ifdef PEPSY_REALS
    case TY_ASIMPLE:
#define Xparm	((struct codedata *)parm)
        Xparm->cd_a = mkpe(t_test + 2);
        Xparm->cd_int = 2*t_test - 1;
        Xparm->cd_int1 = 2*t_test - 1;
        Xparm->cd_int2 = 2*t_test - 1;
	if (t_test % 2)
	    Xparm->cd_string = strdup("hello there, world\n");
	else
	    Xparm->cd_string = strdup("hello world\n");
	Xparm->cd_string1 = strdup("hello world\n");
	Xparm->cd_string2 = strdup("hello world\n");
	Xparm->cd_bool = t_test % 2;
	Xparm->cd_real = t_test * 3.1415962;
	Xparm->cd_oid = mkoid(t_test*2);
	Xparm->cd_oid1 = mkoid(t_test*2);
	Xparm->cd_oid2 = mkoid(t_test*2);
	Xparm->cd_bit = strb2bitstr("\0221\0327\052\211\0237\200", t_test + 30,
				    0, 0);
	Xparm->cd_right = (struct codedata *)fill(TY_DFTFUNC);
	break;
#undef	Xparm
#endif

    default:
	ferrd(1, "fill:unknown type %d\n", tynum);
    }

    return (parm);
}

/*
 * compare two structures for differences of fields indicating an
 * error
 */
tcmp(tynum, parm1, parm2)
int	tynum;
char	*parm1, *parm2;
{
    int	d;
    int	i;

    d = 0;

    switch (tynum) {
    case TY_MPDU:
#define Xparm1	((struct type_T1_MPDU *)parm1)
#define Xparm2	((struct type_T1_MPDU *)parm2)
	if (Xparm1->a__seq && !Xparm2->a__seq
	  || !Xparm1->a__seq && Xparm2->a__seq) {
	    (void) printf("a__seq missing/present\n");
	    d++;
	}
	if (Xparm1->a__seq && Xparm2->a__seq) {
	    if (Xparm1->a__seq->fred != Xparm2->a__seq->fred) {
		(void) printf("%s->a__seq->fred %d != %d\n",  
		    Xparm1->a__seq->fred, Xparm2->a__seq->fred);
		d++;
	    }
	    if (Xparm1->a__seq->george != Xparm2->a__seq->george) {
		(void) printf("%s a__seq->george %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->a__seq->george, Xparm2->a__seq->george);
		d++;
	    }
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_EMBEDDED:
#define Xparm1	((struct type_T1_Embedded *)parm1)
#define Xparm2	((struct type_T1_Embedded *)parm2)
	if (Xparm1->anMPDU && !Xparm2->anMPDU ||!Xparm1->anMPDU && Xparm2->anMPDU) {
	    (void) printf("anMPDU missing/present\n");
	    d++;
	}
	if (Xparm1->anMPDU && Xparm2->anMPDU) {
		d += tcmp(TY_MPDU, (char *)Xparm1->anMPDU, (char *)Xparm2->anMPDU);
	}
	if (Xparm1->ei != Xparm2->ei) {
	    (void) printf("%s ei %d != %d\n",  
		t_case[tynum].tst_name, Xparm1->ei, Xparm2->ei);
	    d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_STRINGS:
#define Xparm1	((struct type_T1_Strings *)parm1)
#define Xparm2	((struct type_T1_Strings *)parm2)
	if (qb_cmp(Xparm1->ostring, Xparm2->ostring)) {
		(void) printf("ostring octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->bstring, Xparm2->bstring)) {
	    (void) printf("bstring bitstring different\n");
	    d++;
	}
	if (qb_cmp(Xparm1->nstring, Xparm2->nstring)) {
		(void) printf("nstring octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->pstring, Xparm2->pstring)) {
		(void) printf("pstring octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->tstring, Xparm2->tstring)) {
		(void) printf("tstring octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->t61string, Xparm2->t61string)) {
		(void) printf("t61string octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->vstring, Xparm2->vstring)) {
		(void) printf("vstring octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->vis__string, Xparm2->vis__string)) {
		(void) printf("vis__string octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->i646string, Xparm2->i646string)) {
		(void) printf("i646string octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->ia5string, Xparm2->ia5string)) {
		(void) printf("ia5string octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->graphstring, Xparm2->graphstring)) {
		(void) printf("graphstring octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->genstring, Xparm2->genstring)) {
		(void) printf("genstring octet string different\n");
		d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_EMB_STRINGS:
#define Xparm1	((struct type_T1_Emb__Strings *)parm1)
#define Xparm2	((struct type_T1_Emb__Strings *)parm2)
	if (qb_cmp(Xparm1->btest, Xparm2->btest)) {
		(void) printf("btest octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->ctest, Xparm2->ctest)) {
	    (void) printf("ctest bitstring different\n");
	    d++;
	}
	if (Xparm1->atest != Xparm2->atest) {
		(void) printf("atest integers different\n");
		d++;
	}
	if (Xparm1->big__test && Xparm2->big__test) {
		d += tcmp(TY_STRINGS, (char *)Xparm1->big__test, (char *)Xparm2->big__test);
	} else if (Xparm1->big__test || Xparm2->big__test) {
		(void) printf("big__test one Strings missing!\n");
		d++;
	}
	if (qb_cmp(Xparm1->emb__test->em__oct, Xparm2->emb__test->em__oct)) {
		(void) printf("emb__test->em__oct octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->emb__test->em__bit, Xparm2->emb__test->em__bit)) {
	    (void) printf("emb__test->em__bit bitstring different\n");
	    d++;
	}
	if (Xparm1->emb__test->em__int != Xparm2->emb__test->em__int) {
		(void) printf("emb__test->em__int integers different\n");
		d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_IMPLICIT:
#define Xparm1	((struct type_T1_Impl__Tags *)parm1)
#define Xparm2	((struct type_T1_Impl__Tags *)parm2)
	if (qb_cmp(Xparm1->o__impl, Xparm2->o__impl)) {
		(void) printf("o__impl octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->b__impl, Xparm2->b__impl)) {
	    (void) printf("b__impl bitstring different\n");
	    d++;
	}
	if (Xparm1->i__impl != Xparm2->i__impl) {
		(void) printf("i__impl integers different\n");
		d++;
	}
	if (Xparm1->obj__impl && Xparm2->obj__impl) {
		d += tcmp(TY_EMB_STRINGS, (char *)Xparm1->obj__impl, (char *)Xparm2->obj__impl);
	} else if (Xparm1->obj__impl || Xparm2->obj__impl) {
		(void) printf("obj__impl one Embedded Strings missing!\n");
		d++;
	}
	if (qb_cmp(Xparm1->i__emb__test->i__em__oct, Xparm2->i__emb__test->i__em__oct)) {
		(void) printf("i__emb__test->i__em__oct octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->i__emb__test->i__em__bit, Xparm2->i__emb__test->i__em__bit)) {
	    (void) printf("i__emb__test->i__em__bit bitstring different\n");
	    d++;
	}
	if (Xparm1->i__emb__test->i__em__int != Xparm2->i__emb__test->i__em__int) {
		(void) printf("i__emb__test->i__em__int integers different\n");
		d++;
	}
#undef Xparm1
#undef Xparm2
	break;
	
    case TY_EXPLICIT:
#define Xparm1	((struct type_T1_Expl__Tags *)parm1)
#define Xparm2	((struct type_T1_Expl__Tags *)parm2)
	if (qb_cmp(Xparm1->o__expl, Xparm2->o__expl)) {
		(void) printf("o__expl octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->b__expl, Xparm2->b__expl)) {
	    (void) printf("b__expl bitstring different\n");
	    d++;
	}
	if (Xparm1->i__expl != Xparm2->i__expl) {
		(void) printf("i__expl integers different\n");
		d++;
	}
	if (Xparm1->obj__expl && Xparm2->obj__expl) {
		d += tcmp(TY_EMB_STRINGS, (char *)Xparm1->obj__expl, (char *)Xparm2->obj__expl);
	} else if (Xparm1->obj__expl || Xparm2->obj__expl) {
		(void) printf("obj__expl one Embedded Strings missing!\n");
		d++;
	}
	if (qb_cmp(Xparm1->i__exp__test->i__ex__oct, Xparm2->i__exp__test->i__ex__oct)) {
		(void) printf("i__exp__test->i__ex__oct octet string different\n");
		d++;
	}
	if (bit_cmp(Xparm1->i__exp__test->i__ex__bit, Xparm2->i__exp__test->i__ex__bit)) {
	    (void) printf("i__exp__test->i__ex__bit bitstring different\n");
	    d++;
	}
	if (Xparm1->i__exp__test->i__ex__int != Xparm2->i__exp__test->i__ex__int) {
		(void) printf("i__exp__test->i__ex__int integers different\n");
		d++;
	}
#undef Xparm1
#undef Xparm2
	break;
	

    case TY_SEQOF:
#define Xparm1	((struct type_T1_Seqof__Test *)parm1)
#define Xparm2	((struct type_T1_Seqof__Test *)parm2)
    if (Xparm1->sqof__test1 && Xparm2->sqof__test1) {
	d += tcmp(TY_ELEMENT4, (char *)Xparm1->sqof__test1, (char *)Xparm2->sqof__test1);
    } else if (Xparm1->sqof__test1 || Xparm2->sqof__test1) {
	(void) printf("sqof__test1 one missing");
	d++;
    }
    if (Xparm1->stof__test1 && Xparm2->stof__test1) {
	d += tcmp(TY_MEMBER2, (char *)Xparm1->stof__test1, (char *)Xparm2->stof__test1);
    } else if (Xparm1->stof__test1 || Xparm2->stof__test1) {
	(void) printf("stof__test1 one missing");
	d++;
    }
    if (Xparm1->i__test1 != Xparm2->i__test1) {
	(void) printf("i__test1 integers different\n");
	d++;
    }
    if (Xparm1->sqof__test2 && Xparm2->sqof__test2) {
	d += tcmp(TY_ELEMENT6, (char *)Xparm1->sqof__test2, (char *)Xparm2->sqof__test2);
    } else if (Xparm1->sqof__test2 || Xparm2->sqof__test2) {
	(void) printf("sqof__test2 one missing");
	d++;
    }
    if (Xparm1->stof__test2 && Xparm2->stof__test2) {
	d += tcmp(TY_MEMBER4, (char *)Xparm1->stof__test2, (char *)Xparm2->stof__test2);
    } else if (Xparm1->stof__test2 || Xparm2->stof__test2) {
	(void) printf("stof__test2 one missing");
	d++;
    }
    if (Xparm1->i__test2 != Xparm2->i__test2) {
	(void) printf("i__test2 integers different\n");
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2
	
    case TY_ELEMENT4:
#define Xparm1	((struct element_T1_4 *)parm1)
#define Xparm2	((struct element_T1_4 *)parm2)
    if (Xparm1->element_T1_5 && Xparm2->element_T1_5) {
	d += tcmp(TY_EXPLICIT, (char *)Xparm1->element_T1_5, (char *)Xparm2->element_T1_5);
    } else if (Xparm1->element_T1_5 || Xparm2->element_T1_5) {
	(void) printf("element_T1_5 one missing");
	d++;
    }
    if (Xparm1->next && Xparm2->next) {
	d += tcmp(TY_ELEMENT4, (char *)Xparm1->next, (char *)Xparm2->next);
    } else if (Xparm1->next || Xparm2->next) {
	(void) printf("%s: next one missing", t_case[tynum].tst_name);
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2
	
    case TY_MEMBER2:
#define Xparm1	((struct member_T1_2 *)parm1)
#define Xparm2	((struct member_T1_2 *)parm2)
    if (Xparm1->member_T1_3 && Xparm2->member_T1_3) {
	d += tcmp(TY_EXPLICIT, (char *)Xparm1->member_T1_3, (char *)Xparm2->member_T1_3);
    } else if (Xparm1->member_T1_3 || Xparm2->member_T1_3) {
	(void) printf("%s: member_T1_3 one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->next && Xparm2->next) {
	d += tcmp(TY_MEMBER2, (char *)Xparm1->next, (char *)Xparm2->next);
    } else if (Xparm1->next || Xparm2->next) {
	(void) printf("%s: next one missing", t_case[tynum].tst_name);
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2

    case TY_ELEMENT6:
#define Xparm1	((struct element_T1_6 *)parm1)
#define Xparm2	((struct element_T1_6 *)parm2)
    if (Xparm1->element_T1_7 && Xparm2->element_T1_7) {
	d += tcmp(TY_ELEMENT8, (char *)Xparm1->element_T1_7, (char *)Xparm2->element_T1_7);
    } else if (Xparm1->element_T1_7 || Xparm2->element_T1_7) {
	(void) printf("%s: element_T1_7 one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->next && Xparm2->next) {
	d += tcmp(TY_ELEMENT6, (char *)Xparm1->next, (char *)Xparm2->next);
    } else if (Xparm1->next || Xparm2->next) {
	(void) printf("%s: next one missing", t_case[tynum].tst_name);
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2

    case TY_ELEMENT8:
#define Xparm1	((struct element_T1_8 *)parm1)
#define Xparm2	((struct element_T1_8 *)parm2)
    if (Xparm1->sqof__in && Xparm2->sqof__in) {
	d += tcmp(TY_EXPLICIT, (char *)Xparm1->sqof__in, (char *)Xparm2->sqof__in);
    } else if (Xparm1->sqof__in || Xparm2->sqof__in) {
	(void) printf("%s: sqof__in one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->sqof__i != Xparm2->sqof__i) {
	(void) printf("sqof__i integers different\n");
	d++;
    }
    if (qb_cmp(Xparm1->sqof__o, Xparm2->sqof__o)) {
	(void) printf("sqof__o octet string different\n");
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2

    case TY_MEMBER4:
#define Xparm1	((struct member_T1_4 *)parm1)
#define Xparm2	((struct member_T1_4 *)parm2)
    if (Xparm1->member_T1_5 && Xparm2->member_T1_5) {
	d += tcmp(TY_ELEMENT9, (char *)Xparm1->member_T1_5, (char *)Xparm2->member_T1_5);
    } else if (Xparm1->member_T1_5 || Xparm2->member_T1_5) {
	(void) printf("%s: member_T1_5 one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->next && Xparm2->next) {
	d += tcmp(TY_MEMBER4, (char *)Xparm1->next, (char *)Xparm2->next);
    } else if (Xparm1->next || Xparm2->next) {
	(void) printf("%s: next one missing", t_case[tynum].tst_name);
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2

    case TY_ELEMENT9:
#define Xparm1	((struct element_T1_9 *)parm1)
#define Xparm2	((struct element_T1_9 *)parm2)
    if (Xparm1->stof__in && Xparm2->stof__in) {
	d += tcmp(TY_EXPLICIT, (char *)Xparm1->stof__in, (char *)Xparm2->stof__in);
    } else if (Xparm1->stof__in || Xparm2->stof__in) {
	(void) printf("%s: stof__in one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->stof__i != Xparm2->stof__i) {
	(void) printf("stof__i integers different\n");
	d++;
    }
    if (qb_cmp(Xparm1->stof__o, Xparm2->stof__o)) {
	(void) printf("stof__o octet string different\n");
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2

    case TY_CHOICE:
#define Xparm1	((struct type_T1_Choice__Test *)parm1)
#define Xparm2	((struct type_T1_Choice__Test *)parm2)
    if (Xparm1->c1 && Xparm2->c1) {
	d += tcmp(TY_CHOICE0, (char *)Xparm1->c1, (char *)Xparm2->c1);
    } else if (Xparm1->c1 || Xparm2->c1) {
	(void) printf("%s: c1 one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->c2 && Xparm2->c2) {
	d += tcmp(TY_CHOICE1, (char *)Xparm1->c2, (char *)Xparm2->c2);
    } else if (Xparm1->c2 || Xparm2->c2) {
	(void) printf("%s: c2 one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->c3 && Xparm2->c3) {
	d += tcmp(TY_CHOICE2, (char *)Xparm1->c3, (char *)Xparm2->c3);
    } else if (Xparm1->c3 || Xparm2->c3) {
	(void) printf("%s: c3 one missing", t_case[tynum].tst_name);
	d++;
    }
    if (Xparm1->c4 && Xparm2->c4) {
	d += tcmp(TY_ELEMENT11, (char *)Xparm1->c4, (char *)Xparm2->c4);
    } else if (Xparm1->c4 || Xparm2->c4) {
	(void) printf("%s: c4 one missing", t_case[tynum].tst_name);
	d++;
    }
    break;
#undef Xparm1
#undef Xparm2

    case TY_CHOICE0:
#define Xparm1	((struct choice_T1_0 *)parm1)
#define Xparm2	((struct choice_T1_0 *)parm2)
	if (Xparm1->offset != Xparm2->offset) {
	    d++;
	    (void) printf("%s: offset mismatch %d != %d\n", t_case[tynum].tst_name,
		Xparm1->offset, Xparm2->offset);
	    break;
	}
	switch (Xparm1->offset) {
	case choice_T1_0_i__c1:
	    if (Xparm1->un.i__c1 != Xparm2->un.i__c1) {
		d++;
		(void) printf("%s: i__c1 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.i__c1, Xparm2->un.i__c1);
	    }
	    break;
	
	case choice_T1_0_o__c1:
	    if (qb_cmp(Xparm1->un.o__c1, Xparm2->un.o__c1)) {
		(void) printf("o__c1 octet string different\n");
		d++;
	    }
	    break;
	
	case choice_T1_0_b__c1:
	    if (bit_cmp(Xparm1->un.b__c1, Xparm2->un.b__c1)) {
		(void) printf("un.b__c1 bitstring different\n");
		d++;
	    }
	    break;
	
	case choice_T1_0_f__c1:
	    if (Xparm1->un.f__c1 && !Xparm2->un.f__c1
	      || !Xparm1->un.f__c1 && Xparm2->un.f__c1) {
		(void) printf("f__c1 Boolean different\n");
		d++;
	    }
	    break;
	
	case choice_T1_0_obj__c1:
	    if (Xparm1->un.obj__c1 && Xparm2->un.obj__c1) {
		d += tcmp(TY_EMB_STRINGS, (char *)Xparm1->un.obj__c1,
		  (char *)Xparm2->un.obj__c1);
	    } else if (Xparm1->un.obj__c1 || Xparm2->un.obj__c1) {
		(void) printf("%s: un.obj__c1 one missing", t_case[tynum].tst_name);
		d++;
	    }
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE0:illegal offset %d\n", Xparm1->offset);
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_CHOICE1:
#define Xparm1	((struct choice_T1_1 *)parm1)
#define Xparm2	((struct choice_T1_1 *)parm2)
	
	if (Xparm1->offset != Xparm2->offset) {
	    d++;
	    (void) printf("%s: offset mismatch %d != %d\n", t_case[tynum].tst_name,
		Xparm1->offset, Xparm2->offset);
	    break;
	}
	switch (Xparm1->offset) {
	case choice_T1_1_i__c2:
	    if (Xparm1->un.i__c2 != Xparm2->un.i__c2) {
		d++;
		(void) printf("%s: i__c2 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.i__c2, Xparm2->un.i__c2);
	    }
	    break;
	
	case choice_T1_1_o__c2:
	    if (qb_cmp(Xparm1->un.o__c2, Xparm2->un.o__c2)) {
		(void) printf("o__c2 octet string different\n");
		d++;
	    }
	    break;
	
	case choice_T1_1_b__c2:
	    if (bit_cmp(Xparm1->un.b__c2, Xparm2->un.b__c2)) {
		(void) printf("un.b__c2 bitstring different\n");
		d++;
	    }
	    break;
	
	case choice_T1_1_f__c2:
	    if (Xparm1->un.f__c2 && !Xparm2->un.f__c2
	      || !Xparm1->un.f__c2 && Xparm2->un.f__c2) {
		d++;
		(void) printf("%s: f__c2 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.f__c2, Xparm2->un.f__c2);
	    }
	    break;
	
	case choice_T1_1_obj__c2:
	    if (Xparm1->un.obj__c2 && Xparm2->un.obj__c2) {
		d += tcmp(TY_EMB_STRINGS, (char *)Xparm1->un.obj__c2,
		  (char *)Xparm2->un.obj__c2);
	    } else if (Xparm1->un.obj__c2 || Xparm2->un.obj__c2) {
		(void) printf("%s: un.obj__c2 one missing", t_case[tynum].tst_name);
		d++;
	    }
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE1:illegal offset %d\n", Xparm1->offset);
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_CHOICE2:
#define Xparm1	((struct choice_T1_2 *)parm1)
#define Xparm2	((struct choice_T1_2 *)parm2)
	
	switch (Xparm1->offset) {
	case choice_T1_2_i__c3:
	    if (Xparm1->un.i__c3 != Xparm2->un.i__c3) {
		d++;
		(void) printf("%s: i__c3 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.i__c3, Xparm2->un.i__c3);
	    }
	    break;
	
	case choice_T1_2_seq__c3:
	    if (Xparm1->un.seq__c3 && Xparm2->un.seq__c3) {
		d += tcmp(TY_ELEMENT10, (char *)Xparm1->un.seq__c3, (char *)Xparm2->un.seq__c3);
	    } else if (Xparm1->un.seq__c3 || Xparm2->un.seq__c3) {
		(void) printf("%s: un.seq__c3 one missing", t_case[tynum].tst_name);
		d++;
	    }
	    break;
	
	case choice_T1_2_set__c3:
	    if (Xparm1->un.set__c3 && Xparm2->un.set__c3) {
		d += tcmp(TY_MEMBER6, (char *)Xparm1->un.set__c3, (char *)Xparm2->un.set__c3);
	    } else if (Xparm1->un.set__c3 || Xparm2->un.set__c3) {
		(void) printf("%s: un.set__c3 one missing", t_case[tynum].tst_name);
		d++;
	    }
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE2:illegal offset %d\n", Xparm1->offset);
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_CHOICE3:
#define Xparm1	((struct choice_T1_3 *)parm1)
#define Xparm2	((struct choice_T1_3 *)parm2)
	
	switch (Xparm1->offset) {
	case choice_T1_3_sc__a__i:
	    if (Xparm1->un.sc__a__i != Xparm2->un.sc__a__i) {
		d++;
		(void) printf("%s: sc__a__i mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.sc__a__i, Xparm2->un.sc__a__i);
	    }
	    break;
	
	case choice_T1_3_sc__b__i:
	    if (Xparm1->un.sc__b__i != Xparm2->un.sc__b__i) {
		d++;
		(void) printf("%s: sc__b__i mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.sc__b__i, Xparm2->un.sc__b__i);
	    }
	    break;
	
	case choice_T1_3_c4__i:
	    if (Xparm1->un.c4__i != Xparm2->un.c4__i) {
		d++;
		(void) printf("%s: c4__i mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.c4__i, Xparm2->un.c4__i);
	    }
	    break;
	
	case choice_T1_3_c4__obj:
	    if (Xparm1->un.c4__obj && Xparm2->un.c4__obj) {
		d += tcmp(TY_EXPLICIT, (char *)Xparm1->un.c4__obj, (char *)Xparm2->un.c4__obj);
	    } else if (Xparm1->un.c4__obj || Xparm2->un.c4__obj) {
		(void) printf("%s: un.c4__obj one missing", t_case[tynum].tst_name);
		d++;
	    }
	    break;
	
	default:
	    ferrd(1, "TY_CHOICE3:illegal offset %d\n", Xparm1->offset);
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_ELEMENT10:
#define Xparm1	((struct element_T1_10 *)parm1)
#define Xparm2	((struct element_T1_10 *)parm2)
	if (Xparm1->seq__c3__in && Xparm2->seq__c3__in) {
	    d += tcmp(TY_EXPLICIT, (char *)Xparm1->seq__c3__in, (char *)Xparm2->seq__c3__in);
	} else if (Xparm1->seq__c3__in || Xparm2->seq__c3__in) {
	    (void) printf("%s: seq__c3__in one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->seq__c3__i != Xparm2->seq__c3__i) {
	    d++;
	    (void) printf("%s: seq__c3__i mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->seq__c3__i, Xparm2->seq__c3__i);
	}
	if (qb_cmp(Xparm1->seq__c3__o, Xparm2->seq__c3__o)) {
	    (void) printf("seq__c3__o octet string different\n");
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_MEMBER6:
#define Xparm1	((struct member_T1_6 *)parm1)
#define Xparm2	((struct member_T1_6 *)parm2)
	if (Xparm1->set__c3__in && Xparm2->set__c3__in) {
	    d += tcmp(TY_EXPLICIT, (char *)Xparm1->set__c3__in, (char *)Xparm2->set__c3__in);
	} else if (Xparm1->set__c3__in || Xparm2->set__c3__in) {
	    (void) printf("%s: set__c3__in one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->set__c3__i != Xparm2->set__c3__i) {
	    d++;
	    (void) printf("%s: set__c3__i mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->set__c3__i, Xparm2->set__c3__i);
	}
	if (qb_cmp(Xparm1->set__c3__o, Xparm2->set__c3__o)) {
	    (void) printf("set__c3__o octet string different\n");
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_ELEMENT11:
#define Xparm1	((struct element_T1_11 *)parm1)
#define Xparm2	((struct element_T1_11 *)parm2)
	if (Xparm1->c4__choice && Xparm2->c4__choice) {
	    d += tcmp(TY_CHOICE3, (char *)Xparm1->c4__choice, (char *)Xparm2->c4__choice);
	} else if (Xparm1->c4__choice || Xparm2->c4__choice) {
	    (void) printf("%s: c4__choice one missing", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_OPTIONAL:
#define Xparm1	((struct type_T1_Opt__Strings *)parm1)
#define Xparm2	((struct type_T1_Opt__Strings *)parm2)
        if (Xparm1->optionals & opt_T1_Opt__Strings_a__opt) {
	    if (Xparm1->a__opt != Xparm2->a__opt) {
		d++;
		(void) printf("%s: a__opt mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->a__opt, Xparm2->a__opt);
	    }
	}
        if (Xparm1->optionals & opt_T1_Opt__Strings_d__opt) {
	    if (Xparm1->d__opt && !Xparm2->d__opt
	      || !Xparm1->d__opt && Xparm2->d__opt) {
		d++;
		(void) printf("%s: d__opt mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->d__opt, Xparm2->d__opt);
	    }
	}
	if (Xparm1->b__opt != NULLQB && Xparm2->b__opt != NULLQB) {
	    if (qb_cmp(Xparm1->b__opt, Xparm2->b__opt)) {
		(void) printf("b__opt octet string different\n");
		d++;
	    }
	}
	if (Xparm1->b__opt != NULLQB && Xparm2->b__opt == NULLQB
	 || Xparm1->b__opt == NULLQB && Xparm2->b__opt != NULLQB) {
	    (void) printf("%s: b__opt one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->c__opt != NULLPE && Xparm2->c__opt != NULLPE) {
	    if (bit_cmp(Xparm1->c__opt, Xparm2->c__opt)) {
		(void) printf("%s:c__opt bitstring different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}
        if (Xparm1->optionals & opt_T1_Opt__Strings_e__opt) {
	    if (Xparm1->e__opt && !Xparm2->e__opt
	    || !Xparm1->e__opt && Xparm2->e__opt) {
		d++;
		(void) printf("%s: e__opt mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->e__opt, Xparm2->e__opt);
	    }
	}
	if (Xparm1->big__opt && Xparm2->big__opt) {
	    d += tcmp(TY_STRINGS, (char *)Xparm1->big__opt,
	       (char *)Xparm2->big__opt);
	} else if (Xparm1->big__opt && !Xparm2->big__opt
	       || !Xparm1->big__opt && Xparm2->big__opt) {
	    (void) printf("%s: big__opt one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->emb__opt && Xparm2->emb__opt) {
	    d += tcmp(TY_ELEMENT12, (char *)Xparm1->emb__opt,
	       (char *)Xparm2->emb__opt);
	} else if (Xparm1->emb__opt && !Xparm2->emb__opt
	       || !Xparm1->emb__opt && Xparm2->emb__opt) {
	    (void) printf("%s: emb__opt one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->st__opt && Xparm2->st__opt) {
	    d += tcmp(TY_MEMBER7, (char *)Xparm1->st__opt,
	       (char *)Xparm2->st__opt);
	} else if (Xparm1->st__opt && !Xparm2->st__opt
	       || !Xparm1->st__opt && Xparm2->st__opt) {
	    (void) printf("%s: st__opt one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->obj__opt && Xparm2->obj__opt) {
	    d += tcmp(TY_MPDU, (char *)Xparm1->obj__opt,
	       (char *)Xparm2->obj__opt);
	} else if (Xparm1->obj__opt && !Xparm2->obj__opt
	       || !Xparm1->obj__opt && Xparm2->obj__opt) {
	    (void) printf("%s: obj__opt one missing", t_case[tynum].tst_name);
	    d++;
	}
        if (Xparm1->optionals & opt_T1_Opt__Strings_etag__opt) {
	    if (Xparm1->etag__opt != Xparm2->etag__opt) {
		d++;
		(void) printf("%s: etag__opt mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->etag__opt, Xparm2->etag__opt);
	    }
	}
	if (Xparm1->ch__opt && Xparm2->ch__opt) {
	    d += tcmp(TY_CHOICE4, (char *)Xparm1->ch__opt,
	       (char *)Xparm2->ch__opt);
	} else if (Xparm1->ch__opt && !Xparm2->ch__opt
	       || !Xparm1->ch__opt && Xparm2->ch__opt) {
	    (void) printf("%s: ch__opt one missing", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

     case TY_ELEMENT12:
#define Xparm1	((struct element_T1_12 *)parm1)
#define Xparm2	((struct element_T1_12 *)parm2)
        if (Xparm1->optionals & opt_T1_element_T1_12_oem__int) {
	    if (Xparm1->oem__int != Xparm2->oem__int) {
		d++;
		(void) printf("%s: oem__int mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->oem__int, Xparm2->oem__int);
	    }
	}
	if (Xparm1->oem__oct != NULLQB && Xparm2->oem__oct != NULLQB) {
	    if (qb_cmp(Xparm1->oem__oct, Xparm2->oem__oct)) {
		(void) printf("oem__oct octet string different\n");
		d++;
	    }
	}
	if (Xparm1->oem__oct != NULLQB && Xparm2->oem__oct == NULLQB
	 || Xparm1->oem__oct == NULLQB && Xparm2->oem__oct != NULLQB) {
	    (void) printf("%s: oem__oct one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->oem__bit != NULLPE && Xparm2->oem__bit != NULLPE) {
	    if (bit_cmp(Xparm1->oem__bit, Xparm2->oem__bit)) {
		(void) printf("%s:oem__bit bitstring different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}
	break;
#undef Xparm1
#undef Xparm2

     case TY_MEMBER7:
#define Xparm1	((struct member_T1_7 *)parm1)
#define Xparm2	((struct member_T1_7 *)parm2)
        if (Xparm1->optionals & opt_T1_member_T1_7_st__int0) {
	    if (Xparm1->st__int0 != Xparm2->st__int0) {
		d++;
		(void) printf("%s: st__int0 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->st__int0, Xparm2->st__int0);
	    }
	}
        if (Xparm1->optionals & opt_T1_member_T1_7_st__int1) {
	    if (Xparm1->st__int1 != Xparm2->st__int1) {
		d++;
		(void) printf("%s: st__int1 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->st__int1, Xparm2->st__int1);
	    }
	}
        if (Xparm1->optionals & opt_T1_member_T1_7_st__int2) {
	    if (Xparm1->st__int2 != Xparm2->st__int2) {
		d++;
		(void) printf("%s: st__int2 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->st__int2, Xparm2->st__int2);
	    }
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_CHOICE4:
#define Xparm1	((struct choice_T1_4 *)parm1)
#define Xparm2	((struct choice_T1_4 *)parm2)
	if (Xparm1->offset != Xparm2->offset) {
	    d++;
	    (void) printf("%s: offset mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->offset, Xparm2->offset);
	    break;
	}
	switch (Xparm1->offset) {
	case choice_T1_4_ch__1:
	    if (Xparm1->un.ch__1 != Xparm2->un.ch__1) {
		d++;
		(void) printf("%s: ch__1 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.ch__1, Xparm2->un.ch__1);
	    }
	    break;
	
	case choice_T1_4_ch__2:
	    if (Xparm1->un.ch__2 != Xparm2->un.ch__2) {
		d++;
		(void) printf("%s: ch__2 mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.ch__2, Xparm2->un.ch__2);
	    }
	    break;
	
	default:
	    ferrd(1, "tcmp:TY_CHOICE4:illegal offset %d\n", Xparm1->offset);
	}
	break;
#undef Xparm1
#undef Xparm2

     case TY_EXTREF:
#define Xparm1	((struct type_T1_E__ref *)parm1)
#define Xparm2	((struct type_T1_E__ref *)parm2)
	if (Xparm1->a__ref && Xparm2->a__ref) {
	    d += tcmp(TY_T2_INFO, (char *)Xparm1->a__ref,
	       (char *)Xparm2->a__ref);
	} else if (Xparm1->a__ref == NULL || Xparm2->a__ref == NULL) {
	    (void) printf("%s: a__ref one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->b__ref && Xparm2->b__ref) {
	    d += tcmp(TY_T2_INFO, (char *)Xparm1->b__ref,
	       (char *)Xparm2->b__ref);
	} else if (Xparm1->b__ref == NULL || Xparm2->b__ref == NULL) {
	    (void) printf("%s: b__ref one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->c__ref && Xparm2->c__ref) {
	    d += tcmp(TY_CHOICE, (char *)Xparm1->c__ref,
	       (char *)Xparm2->c__ref);
	} else if (Xparm1->c__ref == NULL || Xparm2->c__ref == NULL) {
	    (void) printf("%s: c__ref one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->d__ref && Xparm2->d__ref) {
	    d += tcmp(TY_T2_INFO, (char *)Xparm1->d__ref,
	       (char *)Xparm2->d__ref);
	} else if (Xparm1->d__ref && !Xparm2->d__ref
	       || !Xparm1->d__ref && Xparm2->d__ref) {
	    (void) printf("%s: d__ref one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->e__ref && Xparm2->e__ref) {
	    d += tcmp(TY_T2_INFO, (char *)Xparm1->e__ref,
	       (char *)Xparm2->e__ref);
	} else if (Xparm1->e__ref && !Xparm2->e__ref
	       || !Xparm1->e__ref && Xparm2->e__ref) {
	    (void) printf("%s: e__ref one missing", t_case[tynum].tst_name);
	    d++;
	}
        break;
#undef Xparm1
#undef Xparm2

     case TY_T2_INFO:
#define Xparm1	((struct type_T2_Info *)parm1)
#define Xparm2	((struct type_T2_Info *)parm2)
	if (Xparm1->a1 != Xparm2->a1) {
	    d++;
	    (void) printf("%s: a1 mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->a1, Xparm2->a1);
	}
	if (Xparm1->a2 != Xparm2->a2) {
	    d++;
	    (void) printf("%s: a2 mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->a2, Xparm2->a2);
	}
	if (Xparm1->a3 != Xparm2->a3) {
	    d++;
	    (void) printf("%s: a3 mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->a3, Xparm2->a3);
	}
	if (Xparm1->a4 && Xparm2->a4) {
	    d += tcmp(TY_T2_MPDU, (char *)Xparm1->a4, (char *)Xparm2->a4);
	} else if (Xparm1->a4 == NULL || Xparm2->a4 == NULL) {
	    (void) printf("%s: a4 one missing", t_case[tynum].tst_name);
	    d++;
	}
        break;
#undef Xparm1
#undef Xparm2

     case TY_T2_MPDU:
#define Xparm1	((struct type_T2_MPDU *)parm1)
#define Xparm2	((struct type_T2_MPDU *)parm2)
	if (Xparm1->a__seq && Xparm2->a__seq) {
	    d += tcmp(TY_T2_ELEM0, (char *)Xparm1->a__seq,
	       (char *)Xparm2->a__seq);
	} else if (Xparm1->a__seq == NULL || Xparm2->a__seq == NULL) {
	    (void) printf("%s: a__seq one missing", t_case[tynum].tst_name);
	    d++;
	}
        break;
#undef Xparm1
#undef Xparm2

     case TY_T2_ELEM0:
#define Xparm1	((struct element_T2_0 *)parm1)
#define Xparm2	((struct element_T2_0 *)parm2)
	if (Xparm1->fred != Xparm2->fred) {
	    d++;
	    (void) printf("%s: fred mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->fred, Xparm2->fred);
	}
        break;
#undef Xparm1
#undef Xparm2

     case TY_OPTIMISED:
#define Xparm1	((struct type_T1_Optimised *)parm1)
#define Xparm2	((struct type_T1_Optimised *)parm2)
	if (Xparm1->o1 == NULLPE || Xparm2->o1 == NULLPE) {
	    (void) printf("%s: o1 one missing\n", t_case[tynum].tst_name);
	    d++;
	} else if (bit_cmp(Xparm1->o1, Xparm2->o1)) {
		(void) printf("%s:o1 bitstring different\n",
		    t_case[tynum].tst_name);
		d++;
	}
	if (Xparm1->o2 != NULLQB && Xparm2->o2 != NULLQB) {
	    if (qb_cmp(Xparm1->o2, Xparm2->o2)) {
		(void) printf("o2 octet string different\n");
		d++;
	    }
	} else {
	    (void) printf("%s: o2 one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->o3 && Xparm2->o3) {
	    d += tcmp(TY_MPDU, (char *)Xparm1->o3, (char *)Xparm2->o3);
	} else if (Xparm1->o3 == NULL || Xparm2->o3 == NULL) {
	    (void) printf("%s: o3 one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->o4 != NULLPE && Xparm2->o4 == NULLPE
	 || Xparm1->o4 == NULLPE && Xparm2->o4 != NULLPE) {
	    (void) printf("%s: o4 one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->o4 != NULLPE && Xparm2->o4 != NULLPE) {
	    if (pe_cmp(Xparm1->o4, Xparm2->o4)) {
		(void) printf("%s:o4 SET of ANY different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}
	if (Xparm1->element_T1_14 && Xparm2->element_T1_14) {
	    d += tcmp(TY_MEMBER9, (char *)Xparm1->element_T1_14,
	    (char *)Xparm2->element_T1_14);
	} else if (Xparm1->element_T1_14 == NULL
 	         || Xparm2->element_T1_14 == NULL) {
	    (void) printf("%s: element_T1_14 one missing", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

     case TY_MEMBER9:
#define Xparm1	((struct member_T1_9 *)parm1)
#define Xparm2	((struct member_T1_9 *)parm2)
	if (Xparm1->o5 != NULLPE && Xparm2->o5 != NULLPE) {
	    if (pe_cmp(Xparm1->o5, Xparm2->o5)) {
		(void) printf("%s:o5 SET of ANY different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: o5 one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->o6 != NULLPE && Xparm2->o6 != NULLPE) {
	    if (pe_cmp(Xparm1->o6, Xparm2->o6)) {
		(void) printf("%s:o6 SET of ANY different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: o6 one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->o7 != NULLOID && Xparm2->o7 != NULLOID) {
	    if (oid_cmp(Xparm1->o7, Xparm2->o7)) {
		(void) printf("%s:o7 OID different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: o7 one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_EXTERNAL:
#define Xparm1	((struct type_T1_Ext__typ *)parm1)
#define Xparm2	((struct type_T1_Ext__typ *)parm2)
	if (Xparm1->ext != NULL && Xparm2->ext != NULL) {
	    if (ext_cmp(Xparm1->ext, Xparm2->ext)) {
		(void) printf("%s:ext EXTERNAL different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: ext one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->a__ny != NULLPE && Xparm2->a__ny != NULLPE) {
	    if (pe_cmp(Xparm1->a__ny, Xparm2->a__ny)) {
		(void) printf("%s:a__ny ANY different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: a__ny one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->ext__impl != NULL && Xparm2->ext__impl != NULL) {
	    if (ext_cmp(Xparm1->ext__impl, Xparm2->ext__impl)) {
		(void) printf("%s:ext__impl EXTERNAL different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: ext__impl one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->any__impl != NULLPE && Xparm2->any__impl != NULLPE) {
	    if (pe_cmp(Xparm1->any__impl, Xparm2->any__impl)) {
		(void) printf("%s:any__impl ANY different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: any__impl one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->ext__expl != NULL && Xparm2->ext__expl != NULL) {
	    if (ext_cmp(Xparm1->ext__expl, Xparm2->ext__expl)) {
		(void) printf("%s:ext__expl EXTERNAL different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: ext__expl one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->any__expl != NULLPE && Xparm2->any__expl != NULLPE) {
	    if (pe_cmp(Xparm1->any__expl, Xparm2->any__expl)) {
		(void) printf("%s:any__expl ANY different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: any__expl one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_SEXTERNAL:
#define Xparm1	((struct type_T1_SExt *)parm1)
#define Xparm2	((struct type_T1_SExt *)parm2)
	if (Xparm1 != NULL && Xparm2 != NULL) {
	    if (ext_cmp(Xparm1, Xparm2)) {
		(void) printf("%s:ext EXTERNAL different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: ext one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2


    case TY_ETAGOBJ:
#define Xparm1	((struct type_T1_Etags *)parm1)
#define Xparm2	((struct type_T1_Etags *)parm2)
	if (Xparm1->offset != Xparm2->offset) {
	    d++;
	    (void) printf("%s: offset mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->offset, Xparm2->offset);
	    break;
	}
	switch (Xparm1->offset) {
	case type_T1_Etags_aE:
	    if (Xparm1->un.aE != Xparm2->un.aE) {
		d++;
		(void) printf("%s: un.aE mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.aE, Xparm2->un.aE);
	    }
	    break;
	
	case type_T1_Etags_bE:
	    if (Xparm1->un.bE != Xparm2->un.bE) {
		d++;
		(void) printf("%s: un.bE mismatch %d != %d", t_case[tynum].tst_name,
		    Xparm1->un.bE, Xparm2->un.bE);
	    }
	    break;
	
	default:
	    ferrd(1, "TY_ETAGOBJ:illegal offset %d\n", Xparm1->offset);
	}
	break;
#undef Xparm1
#undef Xparm2

/* This has to be changed when posy is upgraded to handle DEFAULTS properly */
    case TY_DEFAULT:
#define Xparm1	((struct type_T1_Def__Strings *)parm1)
#define Xparm2	((struct type_T1_Def__Strings *)parm2)
	if (Xparm1->a__def != Xparm2->a__def) {
	    d++;
	    (void) printf("%s: a__def mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->a__def, Xparm2->a__def);
	}
	if (Xparm1->b__def != NULLQB && Xparm2->b__def != NULLQB) {
	    if (qb_cmp(Xparm1->b__def, Xparm2->b__def)) {
		(void) printf("b__def octet string different\n");
		d++;
	    }
	} else if (Xparm2->b__def == NULLQB) {
	    (void) printf("%s: b__def one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->c__def != NULLPE && Xparm2->c__def != NULLPE) {
	    if (bit_cmp(Xparm1->c__def, Xparm2->c__def)) {
		    (void) printf("%s:c__def bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->c__def == NULLPE) {
	    (void) printf("%s: c__def restored version missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->okay != Xparm2->okay) {
	    d++;
	    (void) printf("%s: okay mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->okay, Xparm2->okay);
	}
	/* Can't test NULL ....
	if (Xparm1->e__opt != Xparm2->e__opt) {
	    d++;
	    (void) printf("%s: e__opt mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->e__opt, Xparm2->e__opt);
	}
	*/
	if (Xparm1->big__def && Xparm2->big__def) {
	    d += tcmp(TY_STRINGS, (char *)Xparm1->big__def,
	    (char *)Xparm2->big__def);
	} else {
	    (void) printf("%s: big__def one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->emb__def && Xparm2->emb__def) {
	    d += tcmp(TY_ELEMENT13, (char *)Xparm1->emb__def,
	    (char *)Xparm2->emb__def);
	} else if (Xparm1->emb__def != NULL || Xparm2->emb__def != NULL) {
	    (void) printf("%s: emb__def one missing", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->st__def && Xparm2->st__def) {
	    d += tcmp(TY_MEMBER8, (char *)Xparm1->st__def,
	    (char *)Xparm2->st__def);
	} else if (Xparm1->st__def != NULL || Xparm2->st__def != NULL) {
	    (void) printf("%s: st__def one missing", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_ELEMENT13:
#define Xparm1 ((struct element_T1_13 *)parm1)
#define Xparm2 ((struct element_T1_13 *)parm2)
	if (Xparm1->colour != Xparm2->colour) {
	    d++;
	    (void) printf("%s: colour mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->colour, Xparm2->colour);
	}
	if (Xparm1->oem__oct != NULLQB && Xparm2->oem__oct != NULLQB) {
	    if (qb_cmp(Xparm1->oem__oct, Xparm2->oem__oct)) {
		(void) printf("oem__oct octet string different\n");
		d++;
	    }
	} else if (Xparm1->oem__oct != NULLQB || Xparm2->oem__oct != NULLQB) {
	    (void) printf("oem__oct: one missing 0x%x, 0x%x\n", Xparm1->oem__oct,
	    Xparm1->oem__oct);
	    d++;
	}
	if (Xparm1->version != NULLPE && Xparm2->version != NULLPE) {
	    if (bit_cmp(Xparm1->version, Xparm2->version)) {
		    (void) printf("%s:version bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->version == NULLPE) {
	    (void) printf("%s: version decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_MEMBER8:
#define Xparm1 ((struct member_T1_8 *)parm1)
#define Xparm2 ((struct member_T1_8 *)parm2)
	if (Xparm1->wine != Xparm2->wine) {
	    d++;
	    (void) printf("%s: wine mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->wine, Xparm2->wine);
	}
	if (Xparm1->beer != Xparm2->beer) {
	    d++;
	    (void) printf("%s: beer mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->beer, Xparm2->beer);
	}
	if (Xparm1->spirit != Xparm2->spirit) {
	    d++;
	    (void) printf("%s: spirit mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->spirit, Xparm2->spirit);
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_STEST:
#define Xparm1 ((struct type_T1_Stest *)parm1)
#define Xparm2 ((struct type_T1_Stest *)parm2)
	if (Xparm1->st1 != NULL && Xparm2->st1 != NULL) {
	    d += tcmp(TY_SINT, (char *)Xparm1->st1, (char *)Xparm2->st1);
	} else if (Xparm2->st1 == NULL) {
	    d++;
	    (void) printf("%s: missing", t_case[tynum].tst_name);
	}
	if (Xparm1->st2 != NULL && Xparm2->st2 != NULL) {
	    if (qb_cmp(Xparm1->st2, Xparm2->st2)) {
		(void) printf("%s:st2 octet string different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else if (Xparm2->st2 == NULL) {
	    d++;
	    (void) printf("%s: missing", t_case[tynum].tst_name);
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_SINT:
#define Xparm1 ((struct type_T1_Sint *)parm1)
#define Xparm2 ((struct type_T1_Sint *)parm2)
	if (Xparm1->parm != Xparm2->parm) {
	    (void) printf("%s:parm %d != %d\n", t_case[tynum].tst_name, Xparm1->parm,
		Xparm2->parm);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_ETYPE:
#define Xparm1 ((struct type_T1_Enum__type *)parm1)
#define Xparm2 ((struct type_T1_Enum__type *)parm2)
	if (Xparm1->parm != Xparm2->parm) {
	    (void) printf("%s:parm %d != %d\n", t_case[tynum].tst_name, Xparm1->parm,
		Xparm2->parm);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_ENUM_TEST:
#define Xparm1 ((struct type_T1_T__enum *)parm1)
#define Xparm2 ((struct type_T1_T__enum *)parm2)
	if (Xparm1->ae1 && Xparm2->ae1 ) {
	    d += tcmp(TY_ETYPE, (char *)Xparm1->ae1, (char *)Xparm2->ae1);
	} else {
	    d++;
	    (void) printf("%s:ae1 missing\n", t_case[tynum].tst_name);
	}
	if (Xparm1->ae2 && Xparm2->ae2 ) {
	    d += tcmp(TY_ETYPE, (char *)Xparm1->ae2, (char *)Xparm2->ae2);
	} else {
	    d++;
	    (void) printf("%s:ae2 missing\n", t_case[tynum].tst_name);
	}
	if (Xparm1->ae3 && Xparm2->ae3 ) {
	    d += tcmp(TY_ETYPE, (char *)Xparm1->ae3, (char *)Xparm2->ae3);
	} else {
	    d++;
	    (void) printf("%s:ae3 missing\n", t_case[tynum].tst_name);
	}
	/* Default */
	if (Xparm1->ae4 && Xparm2->ae4 ) {
	    d += tcmp(TY_ETYPE, (char *)Xparm1->ae4, (char *)Xparm2->ae4);
	} else {
	    d++;
	    (void) printf("%s:ae4 missing\n", t_case[tynum].tst_name);
	}
	/* Optional */
	if (Xparm1->ae5 && Xparm2->ae5 ) {
	    d += tcmp(TY_ETYPE, (char *)Xparm1->ae5, (char *)Xparm2->ae5);
	} else if (Xparm1->ae5 || Xparm2->ae5) {
	    d++;
	    (void) printf("%s:ae5 missing\n", t_case[tynum].tst_name);
	}
	break;
#undef Xparm1
#undef Xparm2

#ifdef	PEPSY_REALS
    case TY_REAL:
#define Xparm1	((struct type_T1_Real *)parm1)
#define Xparm2	((struct type_T1_Real *)parm2)
	/* Horrible floating point test for roughly equal */
	if (fabs(Xparm1->parm) < F_SMALL/2) {
	     if (fabs(Xparm1->parm - Xparm2->parm) > F_SMALL) {
		(void) printf("%s:parm %f != %f\n", t_case[tynum].tst_name,
		    Xparm1->parm, Xparm2->parm);
		d++;
	    }
	} else if (fabs((Xparm1->parm - Xparm2->parm)/Xparm1->parm) > F_SMALL) {
	    (void) printf("%s:parm %f != %f\n", t_case[tynum].tst_name, Xparm1->parm,
		Xparm2->parm);
	    d++;
	}
	break;
#undef Xparm1
#undef Xparm2

    case TY_REAL_TEST:
#define Xparm1	((struct type_T1_T__real *)parm1)
#define Xparm2	((struct type_T1_T__real *)parm2)
	if (Xparm1->r1 && Xparm2->r1 ) {
	    d += tcmp(TY_REAL, (char *)Xparm1->r1, (char *)Xparm2->r1);
	} else {
	    d++;
	    (void) printf("%s:r1 missing\n", t_case[tynum].tst_name);
	}
	if (Xparm1->r2 && Xparm2->r2 ) {
	    d += tcmp(TY_REAL, (char *)Xparm1->r2, (char *)Xparm2->r2);
	} else {
	    d++;
	    (void) printf("%s:r2 missing\n", t_case[tynum].tst_name);
	}
	if (Xparm1->r3 && Xparm2->r3 ) {
	    d += tcmp(TY_REAL, (char *)Xparm1->r3, (char *)Xparm2->r3);
	} else {
	    d++;
	    (void) printf("%s:r3 missing\n", t_case[tynum].tst_name);
	}
	/* Default */
	if (Xparm1->r4 && Xparm2->r4 ) {
	    d += tcmp(TY_REAL, (char *)Xparm1->r4, (char *)Xparm2->r4);
	} else {
	    d++;
	    (void) printf("%s:r4 missing\n", t_case[tynum].tst_name);
	}
	/* Optional */
	if (Xparm1->r5 && Xparm2->r5 ) {
	    d += tcmp(TY_REAL, (char *)Xparm1->r5, (char *)Xparm2->r5);
	} else if (Xparm1->r5 || Xparm2->r5) {
	    d++;
	    (void) printf("%s:r5 missing\n", t_case[tynum].tst_name);
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_PEPY:
#define Xparm1	((struct pepy_refs *)parm1)
#define Xparm2	((struct pepy_refs *)parm2)
	if (Xparm1->t_int != Xparm2->t_int) {
	    d++;
	    (void) printf("%s: t_int mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->t_int, Xparm2->t_int);
	}

	if (Xparm1->t_enum != Xparm2->t_enum) {
	    d++;
	    (void) printf("%s:t_enum mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->t_enum, Xparm2->t_enum);
	}

	if (qb_cmp(Xparm1->t_qbuf, Xparm2->t_qbuf)) {
		(void) printf("t_qbuf octet string different\n");
		d++;
	}

	if (Xparm1->t_bool != Xparm2->t_bool) {
	    d++;
	    (void) printf("%s:t_bool mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->t_bool, Xparm2->t_bool);
	}

#ifdef PEPSY_REALS
	/* Horrible floating point test for roughly equal */
	if (fabs(Xparm1->t_real) < F_SMALL/2) {
	     if (fabs(Xparm1->t_real - Xparm2->t_real) > F_SMALL) {
		(void) printf("%s:t_real %f != %f\n", t_case[tynum].tst_name,
		    Xparm1->t_real, Xparm2->t_real);
		d++;
	    }
	} else if (fabs((Xparm1->t_real - Xparm2->t_real)/Xparm1->t_real) > F_SMALL) {
	    (void) printf("%s:t_real %f != %f\n", t_case[tynum].tst_name,
		Xparm1->t_real, Xparm2->t_real);
	    d++;
	}
#endif
	if (Xparm1->t_any != NULLPE && Xparm2->t_any != NULLPE) {
	    if (pe_cmp(Xparm1->t_any, Xparm2->t_any)) {
		(void) printf("%s:t_any different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}
	if (Xparm1->t_oid != NULLOID && Xparm2->t_oid != NULLOID) {
	    if (oid_cmp(Xparm1->t_oid, Xparm2->t_oid)) {
		(void) printf("%s:t_oid OID different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: t_oid one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_bstring != NULLCP && Xparm2->t_bstring != NULLCP) {
	    if (bitstr_cmp(Xparm1->t_bstring, Xparm1->t_blen,
		Xparm2->t_bstring, Xparm2->t_blen)) {
		    (void) printf("%s:t_blen string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_bstring == NULLCP) {
	    (void) printf("%s: t_bstring decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_ostring != NULLCP && Xparm2->t_ostring != NULLCP) {
	    if (Xparm1->t_olen != Xparm2->t_olen) {
		    (void) printf("%s:t_olen string different\n",
			t_case[tynum].tst_name);
		    d++;
	    } else if (bcmp(Xparm1->t_ostring, Xparm2->t_ostring,
		Xparm1->t_olen)) {
		    (void) printf("%s:t_ostring string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_ostring == NULLCP) {
	    (void) printf("%s: t_ostring decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_string != NULLCP && Xparm2->t_string != NULLCP) {
	    if (strcmp(Xparm1->t_string, Xparm2->t_string)) {
		    (void) printf("%s:t_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_string == NULLCP) {
	    (void) printf("%s: t_string decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_pe != NULLPE && Xparm2->t_pe != NULLPE) {
	    if (bit_cmp(Xparm1->t_pe, Xparm2->t_pe)) {
		    (void) printf("%s:t_pe bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_pe == NULLPE) {
	    (void) printf("%s: t_pe decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_def && Xparm2->t_def ) {
	    d += tcmp(TY_DEFPEPY, (char *)Xparm1->t_def, (char *)Xparm2->t_def);
	} else if (Xparm1->t_def || Xparm2->t_def) {
	    d++;
	    (void) printf("%s:t_def missing\n", t_case[tynum].tst_name);
	}
	if (Xparm1->t_opt && Xparm2->t_opt ) {
	    d += tcmp(TY_OPTPEPY, (char *)Xparm1->t_opt, (char *)Xparm2->t_opt);
	    for (i = NUMOPT - 1; i >= 0; i--)
		 if (BITTEST(Xparm1->t_opt->opt_set, i)
		     != BITTEST(Xparm2->t_opt->opt_set, i)) {
		    d++;
		    (void) printf("%s:t_opt missing optional %d\n",
			t_case[tynum].tst_name, i);
		 }
	} else if (Xparm1->t_opt || Xparm2->t_opt) {
	    d++;
	    (void) printf("%s:t_opt missing\n", t_case[tynum].tst_name);
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_DEFPEPY:
    case TY_OPTPEPY:
#define Xparm1	((struct pepy_refs1 *)parm1)
#define Xparm2	((struct pepy_refs1 *)parm2)
	if (tynum == TY_DEFPEPY || (BITTEST(Xparm1->opt_set, OPT_INT1) != 0
	     && BITTEST(Xparm2->opt_set, OPT_INT1) != 0)) {
	    if (Xparm1->t_int != Xparm2->t_int) {
		d++;
		(void) printf("%s: t_int mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_int, Xparm2->t_int);
	    }
	}
	if (tynum == TY_DEFPEPY || (BITTEST(Xparm1->opt_set, OPT_INT2) != 0
	     && BITTEST(Xparm2->opt_set, OPT_INT2) != 0)) {
	    if (Xparm1->t_int1 != Xparm2->t_int1) {
		d++;
		(void) printf("%s: t_int1 mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_int1, Xparm2->t_int1);
	    }
	}

	if (tynum == TY_DEFPEPY || (BITTEST(Xparm1->opt_set, OPT_ENUM1) != 0
	     && BITTEST(Xparm2->opt_set, OPT_ENUM1) != 0)) {
	    if (Xparm1->t_enum != Xparm2->t_enum) {
		d++;
		(void) printf("%s:t_enum mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_enum, Xparm2->t_enum);
	    }
	}
	if (tynum == TY_DEFPEPY || (BITTEST(Xparm1->opt_set, OPT_ENUM2) != 0
	     && BITTEST(Xparm2->opt_set, OPT_ENUM2) != 0)) {
	    if (Xparm1->t_enum1 != Xparm2->t_enum1) {
		d++;
		(void) printf("%s:t_enum1 mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_enum1, Xparm2->t_enum1);
	    }
	}

	if (qb_cmp(Xparm1->t_qbuf, Xparm2->t_qbuf)) {
		(void) printf("t_qbuf octet string different\n");
		d++;
	}
	if (qb_cmp(Xparm1->t_qbuf1, Xparm2->t_qbuf1)) {
		(void) printf("t_qbuf1 octet string different\n");
		d++;
	}

	if (tynum == TY_DEFPEPY || (BITTEST(Xparm1->opt_set, OPT_BOOL1) != 0
	     && BITTEST(Xparm2->opt_set, OPT_BOOL1) != 0)) {
	    if (Xparm1->t_bool != Xparm2->t_bool) {
		d++;
		(void) printf("%s:t_bool mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_bool, Xparm2->t_bool);
	    }
	}
	if (tynum == TY_OPTPEPY) {
	    if (BITTEST(Xparm1->opt_set, OPT_BOOL2) != 0
	     && BITTEST(Xparm2->opt_set, OPT_BOOL2) != 0) {
		if (Xparm1->t_bool1 != Xparm2->t_bool1) {
		    d++;
		    (void) printf("%s:t_bool1 mismatch %d != %d\n", t_case[tynum].tst_name,
			Xparm1->t_bool1, Xparm2->t_bool1);
		}
	    } else if (BITTEST(Xparm1->opt_set, OPT_BOOL2) != 0
		    || BITTEST(Xparm2->opt_set, OPT_BOOL2) != 0) {
		d++;
		(void) printf("%s: t_bool1 missing %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->opt_set, Xparm2->opt_set);
	    }
	}

#ifdef PEPSY_REALS
	/* Horrible floating point test for roughly equal */
	if (fabs(Xparm1->t_real) < F_SMALL/2) {
	     if (fabs(Xparm1->t_real - Xparm2->t_real) > F_SMALL) {
		(void) printf("%s:t_real %f != %f\n", t_case[tynum].tst_name,
		    Xparm1->t_real, Xparm2->t_real);
		d++;
	    }
	} else if (tynum == TY_DEFPEPY 
	    && fabs((Xparm1->t_real - Xparm2->t_real)/Xparm1->t_real) > F_SMALL) {
	    (void) printf("%s:t_real %f != %f\n", t_case[tynum].tst_name,
		Xparm1->t_real, Xparm2->t_real);
	    d++;
	}
	if (fabs(Xparm1->t_real1) < F_SMALL/2) {
	     if (fabs(Xparm1->t_real1 - Xparm2->t_real1) > F_SMALL) {
		(void) printf("%s:t_real1 %f != %f\n", t_case[tynum].tst_name,
		    Xparm1->t_real1, Xparm2->t_real1);
		d++;
	    }
	} else if (tynum == TY_DEFPEPY 
	    && fabs((Xparm1->t_real1 - Xparm2->t_real1)/Xparm1->t_real1) > F_SMALL) {
	    (void) printf("%s:t_real1 %f != %f\n", t_case[tynum].tst_name,
		Xparm1->t_real1, Xparm2->t_real1);
	    d++;
	}
#endif
	if (Xparm1->t_any != NULLPE && Xparm2->t_any != NULLPE) {
	    if (pe_cmp(Xparm1->t_any, Xparm2->t_any)) {
		(void) printf("%s:t_any different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}

	if (Xparm1->t_oid != NULLOID && Xparm2->t_oid != NULLOID) {
	    if (oid_cmp(Xparm1->t_oid, Xparm2->t_oid)) {
		(void) printf("%s:t_oid OID different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: t_oid one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_oid1 != NULLOID && Xparm2->t_oid1 != NULLOID) {
	    if (oid_cmp(Xparm1->t_oid1, Xparm2->t_oid1)) {
		(void) printf("%s:t_oid1 OID different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}

	if (Xparm1->t_bstring != NULLCP && Xparm2->t_bstring != NULLCP) {
	    if (bitstr_cmp(Xparm1->t_bstring, Xparm1->t_blen,
		Xparm2->t_bstring, Xparm2->t_blen)) {
		    (void) printf("%s:t_blen string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_bstring == NULLCP) {
	    (void) printf("%s: t_bstring decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_bstring1 != NULLCP && Xparm2->t_bstring1 != NULLCP) {
	    if (bitstr_cmp(Xparm1->t_bstring1, Xparm1->t_blen1,
		Xparm2->t_bstring1, Xparm2->t_blen1)) {
		    (void) printf("%s:t_blen1 string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (tynum == TY_DEFPEPY && Xparm2->t_bstring1 == NULLCP) {
	    (void) printf("%s: t_bstring1 decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}

	if (Xparm1->t_ostring != NULLCP && Xparm2->t_ostring != NULLCP) {
	    if (Xparm1->t_olen != Xparm2->t_olen) {
		    (void) printf("%s:t_olen string different\n",
			t_case[tynum].tst_name);
		    d++;
	    } else if (bcmp(Xparm1->t_ostring, Xparm2->t_ostring,
		Xparm1->t_olen)) {
		    (void) printf("%s:t_ostring string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_ostring == NULLCP) {
	    (void) printf("%s: t_ostring decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_ostring1 != NULLCP && Xparm2->t_ostring1 != NULLCP) {
	    if (Xparm1->t_olen1 != Xparm2->t_olen1) {
		    (void) printf("%s:t_olen1 string different\n",
			t_case[tynum].tst_name);
		    d++;
	    } else if (bcmp(Xparm1->t_ostring1, Xparm2->t_ostring1,
		Xparm1->t_olen1)) {
		    (void) printf("%s:t_ostring string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_ostring1 == NULLCP) {
	    (void) printf("%s: t_ostring1 decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}

	if (Xparm1->t_string != NULLCP && Xparm2->t_string != NULLCP) {
	    if (strcmp(Xparm1->t_string, Xparm2->t_string)) {
		    (void) printf("%s:t_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_string == NULLCP) {
	    (void) printf("%s: t_string decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_string1 != NULLCP && Xparm2->t_string1 != NULLCP) {
	    if (strcmp(Xparm1->t_string1, Xparm2->t_string1)) {
		    (void) printf("%s:t_string1 string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_string1 == NULLCP) {
	    (void) printf("%s: t_string1 decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}

	if (Xparm1->t_pe != NULLPE && Xparm2->t_pe != NULLPE) {
	    if (bit_cmp(Xparm1->t_pe, Xparm2->t_pe)) {
		    (void) printf("%s:t_pe bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_pe == NULLPE) {
	    (void) printf("%s: t_pe decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_pe1 != NULLPE && Xparm2->t_pe1 != NULLPE) {
	    if (bit_cmp(Xparm1->t_pe1, Xparm2->t_pe1)) {
		    (void) printf("%s:t_pe1 bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_pe1 == NULLPE) {
	    (void) printf("%s: t_pe1 decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_S_COMPD:
#define Xparm1	((struct pepy_refs1 *)parm1)
#define Xparm2	((struct pepy_refs1 *)parm2)
	if (BITTEST(Xparm1->opt_set, OPT_INT1) != 0
	 && BITTEST(Xparm2->opt_set, OPT_INT1) != 0) {
	    if (Xparm1->t_int != Xparm2->t_int) {
		d++;
		(void) printf("%s: t_int mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_int, Xparm2->t_int);
	    }
	} else if (BITTEST(Xparm1->opt_set, OPT_INT1) != 0
	        || BITTEST(Xparm2->opt_set, OPT_INT1) != 0) {
	    d++;
	    (void) printf("%s: t_int missing %d != %d\n", t_case[tynum].tst_name,
		Xparm1->opt_set, Xparm2->opt_set);
	}

	if (BITTEST(Xparm1->opt_set, OPT_INT2) != 0
	 && BITTEST(Xparm2->opt_set, OPT_INT2) != 0) {
	    if (Xparm1->t_int1 != Xparm2->t_int1) {
		d++;
		(void) printf("%s: t_int1 mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_int1, Xparm2->t_int1);
	    }
	} else if (BITTEST(Xparm1->opt_set, OPT_INT2) != 0
	        || BITTEST(Xparm2->opt_set, OPT_INT2) != 0) {
	    d++;
	    (void) printf("%s: t_int1 missing %d != %d\n", t_case[tynum].tst_name,
		Xparm1->opt_set, Xparm2->opt_set);
	}

	if (BITTEST(Xparm1->opt_set, OPT_ENUM1) != 0
	 && BITTEST(Xparm2->opt_set, OPT_ENUM1) != 0) {
	    if (Xparm1->t_enum != Xparm2->t_enum) {
		d++;
		(void) printf("%s:t_enum mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_enum, Xparm2->t_enum);
	    }
	} else if (BITTEST(Xparm1->opt_set, OPT_ENUM1) != 0
	        || BITTEST(Xparm2->opt_set, OPT_ENUM1) != 0) {
	    d++;
	    (void) printf("%s: t_int missing %d != %d\n", t_case[tynum].tst_name,
		Xparm1->opt_set, Xparm2->opt_set);
	}

	if (BITTEST(Xparm1->opt_set, OPT_ENUM2) != 0
	 && BITTEST(Xparm2->opt_set, OPT_ENUM2) != 0) {
	    if (Xparm1->t_enum1 != Xparm2->t_enum1) {
		d++;
		(void) printf("%s:t_enum1 mismatch %d != %d\n", t_case[tynum].tst_name,
		    Xparm1->t_enum1, Xparm2->t_enum1);
	    }
	} else if (BITTEST(Xparm1->opt_set, OPT_ENUM2) != 0
	        || BITTEST(Xparm2->opt_set, OPT_ENUM2) != 0) {
	    d++;
	    (void) printf("%s: t_int missing %d != %d\n", t_case[tynum].tst_name,
		Xparm1->opt_set, Xparm2->opt_set);
	}

	for (i = NUMOPT - 1; i >= 0; i--) {
	    if (i != OPT_INT1 && i != OPT_INT2
	    && i != OPT_ENUM1 && i != OPT_ENUM2)
		continue;

	     if (BITTEST(Xparm1->opt_set, i) != BITTEST(Xparm2->opt_set, i)) {
		d++;
		(void) printf("%s:t_opt missing optional %d\n",
		    t_case[tynum].tst_name, i);
	     }
	}
	break;
#undef	Xparm1
#undef	Xparm2
#endif

    case TY_ACTION:
    case TY_REPEAT:
#define Xparm1	((struct repeats *)parm1)
#define Xparm2	((struct repeats *)parm2)
	if (ri_cmp(Xparm1->rp_sq1, Xparm2->rp_sq1)) {
	    d++;
	    (void) printf("%s:rp_sq1 mangled\n", t_case[tynum].tst_name);
	}
	if (re_cmp(Xparm1->rp_sq2, Xparm2->rp_sq2)) {
	    d++;
	    (void) printf("%s:rp_sq2 mangled\n", t_case[tynum].tst_name);
	}
	if (ri_cmp(Xparm1->rp_st1, Xparm2->rp_st1)) {
	    d++;
	    (void) printf("%s:rp_st1 mangled\n", t_case[tynum].tst_name);
	}
	if (re_cmp(Xparm1->rp_st2, Xparm2->rp_st2)) {
	    d++;
	    (void) printf("%s:rp_st2 mangled\n", t_case[tynum].tst_name);
	}
	if (Xparm1->rp_choice != Xparm2->rp_choice) {
	    d++;
	    (void) printf("%s:rp_choice wrong %d != %d\n",
	    t_case[tynum].tst_name, Xparm1->rp_choice, Xparm2->rp_choice);
	}
	switch (Xparm1->rp_choice) {
	case RP_INT:
	    if (Xparm1->rp_int != Xparm2->rp_int) {
		d++;
		(void) printf("%s:rp_int wrong %d != %d\n",
		t_case[tynum].tst_name, Xparm1->rp_int, Xparm2->rp_int);
	    }
	    break;

	case RP_BOOL:
	    if (Xparm1->rp_bool && !Xparm2->rp_bool
	     || !Xparm1->rp_bool && Xparm2->rp_bool) {
		d++;
		(void) printf("%s:RP_BOOL wrong %d != %d\n",
		t_case[tynum].tst_name, Xparm1->rp_bool, Xparm2->rp_bool);
	    }
	    break;

	case RP_OSTRING:
	    if (!Xparm1->rp_ostring) {
		d++;
		(void) printf("%s:initial rp_ostring missing\n",
		    t_case[tynum].tst_name);
	    }
	    if (!Xparm2->rp_ostring) {
		d++;
		(void) printf("%s:final rp_ostring missing\n",
		    t_case[tynum].tst_name);
	    }
	    if (strcmp(Xparm1->rp_ostring, Xparm2->rp_ostring)) {
		d++;
		(void) printf("%s:rp_ostring not equal %s != %s\n",
		t_case[tynum].tst_name, Xparm1->rp_ostring, Xparm2->rp_ostring);
	    }
	    break;

	default:
		d++;
		(void) printf("%s:bad choice found\n", t_case[tynum].tst_name);
		break;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_VPDEFINED:
#define Xparm1	((struct pepy_refs *)parm1)
#define Xparm2	((struct pepy_refs *)parm2)
	if (Xparm1->t_int != Xparm2->t_int) {
	    d++;
	    (void) printf("%s: t_int mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->t_int, Xparm2->t_int);
	}

	if (Xparm1->t_enum != Xparm2->t_enum) {
	    d++;
	    (void) printf("%s:t_enum mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->t_enum, Xparm2->t_enum);
	}

	if (qb_cmp(Xparm1->t_qbuf, Xparm2->t_qbuf)) {
		(void) printf("t_qbuf octet string different\n");
		d++;
	}

	if (Xparm1->t_bool != Xparm2->t_bool) {
	    d++;
	    (void) printf("%s:t_bool mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->t_bool, Xparm2->t_bool);
	}

#ifdef PEPSY_REALS
	/* Horrible floating point test for roughly equal */
	if (fabs(Xparm1->t_real) < F_SMALL/2) {
	     if (fabs(Xparm1->t_real - Xparm2->t_real) > F_SMALL) {
		(void) printf("%s:t_real %f != %f\n", t_case[tynum].tst_name,
		    Xparm1->t_real, Xparm2->t_real);
		d++;
	    }
	} else if (fabs((Xparm1->t_real - Xparm2->t_real)/Xparm1->t_real) > F_SMALL) {
	    (void) printf("%s:t_real %f != %f\n", t_case[tynum].tst_name,
		Xparm1->t_real, Xparm2->t_real);
	    d++;
	}
#endif
	if (Xparm1->t_any != NULLPE && Xparm2->t_any != NULLPE) {
	    if (pe_cmp(Xparm1->t_any, Xparm2->t_any)) {
		(void) printf("%s:t_any different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	}
	if (Xparm1->t_oid != NULLOID && Xparm2->t_oid != NULLOID) {
	    if (oid_cmp(Xparm1->t_oid, Xparm2->t_oid)) {
		(void) printf("%s:t_oid OID different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: t_oid one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_bstring != NULLCP && Xparm2->t_bstring != NULLCP) {
	    if (bitstr_cmp(Xparm1->t_bstring, Xparm1->t_blen,
		Xparm2->t_bstring, Xparm2->t_blen)) {
		    (void) printf("%s:t_blen string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_bstring == NULLCP) {
	    (void) printf("%s: t_bstring decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_pe != NULLPE && Xparm2->t_pe != NULLPE) {
	    if (bit_cmp(Xparm1->t_pe, Xparm2->t_pe)) {
		    (void) printf("%s:t_pe bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->t_pe == NULLPE) {
	    (void) printf("%s: t_pe decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_ostring != NULLCP && Xparm2->t_ostring != NULLCP) {
	    if (Xparm1->t_olen != Xparm2->t_olen) {
		    (void) printf("%s:t_olen string different\n",
			t_case[tynum].tst_name);
		    d++;
	    } else if (bcmp(Xparm1->t_ostring, Xparm2->t_ostring,
		Xparm1->t_olen)) {
		    (void) printf("%s:t_ostring string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else {
	    (void) printf("%s: t_ostring missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->t_string != NULLCP && Xparm2->t_string != NULLCP) {
	    if (strcmp(Xparm1->t_string, Xparm2->t_string)) {
		    (void) printf("%s:t_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else {
	    (void) printf("%s: t_string decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_FUNC:
#define Xparm1	((struct codedata *)parm1)
#define Xparm2	((struct codedata *)parm2)
	if (Xparm1->cd_a != NULLPE && Xparm2->cd_a != NULLPE) {
	    if (pe_cmp(Xparm1->cd_a, Xparm2->cd_a)) {
		(void) printf("%s:cd_a different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: cd_a missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_b != NULLPE && Xparm2->cd_b != NULLPE) {
	    if (pe_cmp(Xparm1->cd_b, Xparm2->cd_b)) {
		(void) printf("%s:cd_b different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: cd_b missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_c != NULLPE && Xparm2->cd_c != NULLPE) {
	    if (pe_cmp(Xparm1->cd_c, Xparm2->cd_c)) {
		(void) printf("%s:cd_c different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: cd_c missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_d != NULLPE && Xparm2->cd_d != NULLPE) {
	    if (pe_cmp(Xparm1->cd_d, Xparm2->cd_d)) {
		(void) printf("%s:cd_d different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: cd_d missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_int != Xparm2->cd_int) {
	    d++;
	    (void) printf("%s:cd_int mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->cd_int, Xparm2->cd_int);
	}

	if (Xparm1->cd_string != NULLCP && Xparm2->cd_string != NULLCP) {
	    if (strcmp(Xparm1->cd_string, Xparm2->cd_string)) {
		    (void) printf("%s:cd_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else {
	    (void) printf("%s: cd_string missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_left && Xparm2->cd_left ) {
	    d += tcmp(TY_OPTFUNC, (char *)Xparm1->cd_left,
		(char *)Xparm2->cd_left);
	} else if (Xparm1->cd_left || Xparm2->cd_left) {
	    d++;
	    (void) printf("%s:cd_left missing\n", t_case[tynum].tst_name);
	}
	if (Xparm1->cd_right && Xparm2->cd_right ) {
	    d += tcmp(TY_DFTFUNC, (char *)Xparm1->cd_right,
		(char *)Xparm2->cd_right);
	} else if (Xparm1->cd_right || Xparm2->cd_right) {
	    d++;
	    (void) printf("%s:cd_right missing\n", t_case[tynum].tst_name);
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_OPTFUNC:
#define Xparm1	((struct codedata *)parm1)
#define Xparm2	((struct codedata *)parm2)
	if (Xparm1->cd_a != NULLPE && Xparm2->cd_a != NULLPE) {
	    if (pe_cmp(Xparm1->cd_a, Xparm2->cd_a)) {
		(void) printf("%s:cd_a different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else if (Xparm1->cd_a != NULLPE || Xparm2->cd_a != NULLPE) {
	    (void) printf("%s: cd_a missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_b != NULLPE && Xparm2->cd_b != NULLPE) {
	    if (pe_cmp(Xparm1->cd_b, Xparm2->cd_b)) {
		(void) printf("%s:cd_b different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else if (Xparm1->cd_b != NULLPE || Xparm2->cd_b != NULLPE) {
	    (void) printf("%s: cd_b missing\n", t_case[tynum].tst_name);
	    d++;
	}
	for (i = NCD_OPT - 1; i >= 0; i--) {
	     if (BITTEST(Xparm1->cd_opt_set, i)
	       != BITTEST(Xparm2->cd_opt_set, i)) {
		d++;
		(void) printf("%s: flag bit %d differs\n",
			t_case[tynum].tst_name, i);
	     }
	}
	if (BITTEST(Xparm1->cd_opt_set, CD_C)) {
	    if (Xparm1->cd_c != NULLPE && Xparm2->cd_c != NULLPE) {
		if (pe_cmp(Xparm1->cd_c, Xparm2->cd_c)) {
		    (void) printf("%s:cd_c different\n",t_case[tynum].tst_name);
		    d++;
		}
	    } else {
		(void) printf("%s: cd_c missing\n", t_case[tynum].tst_name);
		d++;
	    }
	}
	if (BITTEST(Xparm1->cd_opt_set, CD_D)) {
	    if (Xparm1->cd_d != NULLPE && Xparm2->cd_d != NULLPE) {
		if (pe_cmp(Xparm1->cd_d, Xparm2->cd_d)) {
		    (void) printf("%s:cd_d different\n",t_case[tynum].tst_name);
		    d++;
		}
	    } else {
		(void) printf("%s: cd_d missing\n", t_case[tynum].tst_name);
		d++;
	    }
	}

	if (BITTEST(Xparm1->cd_opt_set, CD_INT)) {
	    if (Xparm1->cd_int != Xparm2->cd_int) {
		d++;
		(void) printf("%s:cd_int mismatch %d != %d",
		    t_case[tynum].tst_name, Xparm1->cd_int, Xparm2->cd_int);
	    }
	}

	if (Xparm1->cd_string != NULLCP && Xparm2->cd_string != NULLCP) {
	    if (strcmp(Xparm1->cd_string, Xparm2->cd_string)) {
		    (void) printf("%s:cd_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm1->cd_string != NULLCP || Xparm2->cd_string != NULLCP) {
	    (void) printf("%s: cd_string missing\n", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

    case TY_DFTFUNC:
#define Xparm1	((struct codedata *)parm1)
#define Xparm2	((struct codedata *)parm2)
	if (Xparm1->cd_a != NULLPE && Xparm2->cd_a != NULLPE) {
	    if (pe_cmp(Xparm1->cd_a, Xparm2->cd_a)) {
		(void) printf("%s:cd_a different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else if (Xparm1->cd_a != NULLPE || Xparm2->cd_a != NULLPE) {
	    (void) printf("%s: cd_a missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_b != NULLPE && Xparm2->cd_b != NULLPE) {
	    if (pe_cmp(Xparm1->cd_b, Xparm2->cd_b)) {
		(void) printf("%s:cd_b different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else if (Xparm1->cd_b != NULLPE || Xparm2->cd_b != NULLPE) {
	    (void) printf("%s: cd_b missing\n", t_case[tynum].tst_name);
	    d++;
	}
	for (i = NCD_OPT - 1; i >= 0; i--) {
	    if (i == CD_INT)
	        continue;

	    if (BITTEST(Xparm1->cd_opt_set,i) !=BITTEST(Xparm2->cd_opt_set,i)) {
	        d++;
	        (void) printf("%s: flag bit %d differs\n",
		    t_case[tynum].tst_name,i);
	    }
	}
	if (BITTEST(Xparm1->cd_opt_set, CD_C)) {
	    if (Xparm1->cd_c != NULLPE && Xparm2->cd_c != NULLPE) {
		if (pe_cmp(Xparm1->cd_c, Xparm2->cd_c)) {
		    (void) printf("%s:cd_c different\n",t_case[tynum].tst_name);
		    d++;
		}
	    } else {
		(void) printf("%s: cd_c missing\n", t_case[tynum].tst_name);
		d++;
	    }
	}
	if (BITTEST(Xparm1->cd_opt_set, CD_D)) {
	    if (Xparm1->cd_d != NULLPE && Xparm2->cd_d != NULLPE) {
		if (pe_cmp(Xparm1->cd_d, Xparm2->cd_d)) {
		    (void) printf("%s:cd_d different\n",t_case[tynum].tst_name);
		    d++;
		}
	    } else {
		(void) printf("%s: cd_d missing\n", t_case[tynum].tst_name);
		d++;
	    }
	}

	if (Xparm1->cd_int != Xparm2->cd_int) {
	    d++;
	    (void) printf("%s:cd_int mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->cd_int, Xparm2->cd_int);
	}

	if (Xparm1->cd_string != NULLCP && Xparm2->cd_string != NULLCP) {
	    if (strcmp(Xparm1->cd_string, Xparm2->cd_string)) {
		    (void) printf("%s:cd_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else {
	    (void) printf("%s: cd_string missing\n", t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2

#ifdef PEPSY_REALS
    case TY_ASIMPLE:
#define Xparm1	((struct codedata *)parm1)
#define Xparm2	((struct codedata *)parm2)
	if (Xparm1->cd_a != NULLPE && Xparm2->cd_a != NULLPE) {
	    if (pe_cmp(Xparm1->cd_a, Xparm2->cd_a)) {
		(void) printf("%s:cd_a different\n", t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: cd_a missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_int != Xparm2->cd_int) {
	    d++;
	    (void) printf("%s:cd_int mismatch %d != %d", t_case[tynum].tst_name,
		Xparm1->cd_int, Xparm2->cd_int);
	}

	if (Xparm1->cd_string != NULLCP && Xparm2->cd_string != NULLCP) {
	    if (strcmp(Xparm1->cd_string, Xparm2->cd_string)) {
		    (void) printf("%s:cd_string string different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else {
	    (void) printf("%s: cd_string missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_bool && !Xparm2->cd_bool
	 || !Xparm1->cd_bool && Xparm2->cd_bool) {
	    d++;
	    (void) printf("%s:cd_bool wrong %d != %d\n",
		t_case[tynum].tst_name, Xparm1->cd_bool, Xparm2->cd_bool);
	}
	/* Horrible floating point test for roughly equal */
	if (fabs(Xparm1->cd_real) < F_SMALL/2) {
	     if (fabs(Xparm1->cd_real - Xparm2->cd_real) > F_SMALL) {
		(void) printf("%s:cd_real %f != %f\n", t_case[tynum].tst_name,
		    Xparm1->cd_real, Xparm2->cd_real);
		d++;
	    }
	} else if (fabs((Xparm1->cd_real - Xparm2->cd_real)/Xparm1->cd_real)
		> F_SMALL) {
	    (void) printf("%s:cd_real %f != %f\n", t_case[tynum].tst_name,
		Xparm1->cd_real, Xparm2->cd_real);
	    d++;
	}
	if (Xparm1->cd_oid != NULLOID && Xparm2->cd_oid != NULLOID) {
	    if (oid_cmp(Xparm1->cd_oid, Xparm2->cd_oid)) {
		(void) printf("%s:cd_oid OID different\n",
		    t_case[tynum].tst_name);
		d++;
	    }
	} else {
	    (void) printf("%s: cd_oid one missing\n", t_case[tynum].tst_name);
	    d++;
	}
	if (Xparm1->cd_bit != NULLPE && Xparm2->cd_bit != NULLPE) {
	    if (bit_cmp(Xparm1->cd_bit, Xparm2->cd_bit)) {
		    (void) printf("%s:cd_bit bitstring different\n",
			t_case[tynum].tst_name);
		    d++;
	    }
	} else if (Xparm2->cd_bit == NULLPE) {
	    (void) printf("%s: cd_bit decoded version missing\n",
		t_case[tynum].tst_name);
	    d++;
	}
	break;
#undef	Xparm1
#undef	Xparm2
#endif

    default:
	ferrd(1, "tcmp:unknown type %d\n", tynum);
    }
    if (d) {
	    (void) printf("tcmp:failed on %s\n", t_case[tynum].tst_name);
    }
    return (d);
}

/*
 * Compare two possible null qbuf lists and return non zero if they are
 * different
 * Pretty complex to allow for all sorts of weird cases
 * Doesn't work for a qbuf which doesn't have a head ! Don't really know what
 * is the proper form of a queue buf. MArshall's doco doesn't say
 */
qb_cmp(qb1, qb2)
struct	qbuf	*qb1, *qb2;
{
    struct	qbuf	*qp1, *qp2;
    char	*po1, *po2;
    int	len1, len2;

    if (qb1 == NULL && qb2 == NULL)
	return (0);
    
    if (qb1 == NULL || qb2 == NULL)
	return (1);

    qp1 = qb1->qb_forw;
    qp2 = qb2->qb_forw;
    po1 = qp1->qb_data;
    po2 = qp2->qb_data;
    len1 = qp1->qb_len;
    len2 = qp2->qb_len;

    while (qp1 != qb1 && qp2 != qb2) {
	if (len1 < len2) {
	    if (bcmp(po1, po2, len1))
		    return (1);
	    len2 -= len1;
	    po2 += len1;
	    qp1 = qp1->qb_forw;
	    po1 = qp1->qb_data;
	    len1 = qp1->qb_len;
	} else {
	    if (bcmp(po1, po2, len1))
		    return (1);
	    len1 -= len2;
	    po1 += len2;
	    qp2 = qp2->qb_forw;
	    po2 = qp2->qb_data;
	    len2 = qp2->qb_len;
	}
    }

    if (len1 == 0)
	    qp1 = qp1->qb_forw;
    if (len2 == 0)
	    qp2 = qp2->qb_forw;
    while (qp1 != qb1 && qp1->qb_len == 0)
	    qp1 = qp1->qb_forw;
    while (qp2 != qb2 && qp2->qb_len == 0)
	    qp2 = qp2->qb_forw;
    if (qp1 == qb1 && qp2 == qb2)
	return (0);	/* perfect match */

    return (1);
}

/*
 * compare two bitstrings. Including the little bits left at the end but
 * not the bits not in the strings
 */
bit_cmp(b1, b2)
PE	b1, b2;
{
       int	len1, len2;
       register char	*cp1, *cp2;

	if (b1 == NULLPE && b2 == NULLPE)
	    return (0);
	if (b1 == NULLPE || b2 == NULLPE)
	    goto fail;

	if ((cp1 = bitstr2strb(b1, &len1)) == NULL)
	    goto fail;
	if ((cp2 = bitstr2strb(b2, &len2)) == NULL) {
	    free(cp1);
	    goto fail;
	}

	if (len1 != len2 || bcmp(cp1, cp2, len1/8)) {
	    free(cp1);
	    free(cp2);
	    goto fail;
	}

	if (len1 % 8) {
		int i;
		int	mask;

		/* Check those last few bits */
		i = len1/8;
		mask = (0xff00 >> len1 % 8) & 0xff;
		if ((cp1[i] & mask) != (cp2[i] & mask)) {
		    free(cp1);
		    free(cp2);
		    goto fail;
		}
	}

	free(cp1);
	free(cp2);
	return (0);

    fail:
	return (1);
}

/*
 * compare to strings of given number of bits for equality
 */
bitstr_cmp(cp1, len1, cp2, len2)
char	*cp1, *cp2;
int	len1, len2;
{
    int i;
    int	mask;

    if (len1 != len2 || bcmp(cp1, cp2, len1/8))
	return (1);
    
    if (len1 % 8) {

	/* Check those last few bits */
	i = len1/8;
	mask = (0xff00 >> len1 % 8) & 0xff;
	if ((cp1[i] & mask) != (cp2[i] & mask)) {
	    return (1);
	}
    }
    return (0);
}

#define MKMASK 0x7
#define MKSHIFT 6
/*
 * Generate a randomish list of PElement s for use as ANY or SET  OF ANY ....
 */
PE
mkpelist(i)
{
    PE pe, fpe = NULL;

    fpe = pe_alloc(PE_CLASS_PRIV, PE_FORM_CONS, (PElementID ) i);
    while (i > 0) {
	pe = mkpe(i);
	pe->pe_next = fpe->pe_cons;
	fpe->pe_cons = pe;
	i--;
    }
    return (fpe);
}

/*
 * generate a randomish PElement
 */
PE
mkpe(i)
int	i;
{
    int	id, class;
    PE 	pe;

    id = i * i + 1;
    class = PE_CLASS_PRIV;
    switch ((i*i >> MKSHIFT) & MKMASK) {
    case 5:
    case 0:
	pe = flag2prim(i & 0x1, (PElementClass ) class, (PElementID ) id);
	break;
    
    case 6:
    case 1:
	pe = num2prim((integer )i, (PElementClass ) class, (PElementID ) id);
	break;

    case 7:
    case 2:
	pe = str2prim("mkpelist:testdata", 17, (PElementClass ) class,
	(PElementID ) id);
	break;

    case 3:
	pe = strb2bitstr("\021\0245\375\0124", 4,
	    (PElementClass ) class, (PElementID ) id);
	break;
    
    case 4:
	pe = mkpelist(i - 1);
	break;

    default:
	ferrd(1, "mkpelist:internal error %d case not handled\n",
	    (i*i >> MKSHIFT) & MKMASK);
    }

    return (pe);
}

#define OID_SIZE	6
/*
 * make an OID for testing
 */
OID
mkoid(i)
int	i;
{
    OID	oid;
    int	oid_cnt = i % OID_SIZE;

    if ((oid = new(struct OIDentifier)) == NULL) {
	(void) printf("mkoid:calloc did not work\n");
	return NULL;
    }
    if (oid_cnt < 2)
	oid_cnt = 2;	/* At least two integers long */
    oid->oid_nelem = oid_cnt;
    if ((oid->oid_elements =
	 (unsigned int *)calloc((unsigned ) oid_cnt, sizeof (int))) == NULL) {
	(void) printf("mkoid:calloc 2 did not work\n");
	return NULL;
    }
    while (oid_cnt > 2) {
	oid->oid_elements[oid_cnt - 1] = oid_cnt*i + 33;
        oid_cnt--;
    }

    oid->oid_elements[0] = 1;
    oid->oid_elements[1] = 17;


    return (oid);
}

/*
 * Produce an External structure initialised to test values
 * for testing EXTERNAL encoding routines
 */
struct type_UNIV_EXTERNAL *
mkext(i)
int	i;
{
    int	k;
    struct type_UNIV_EXTERNAL *p;

    k = i + 2;
    k *= k;	/* generate a more random looking number */
    k %= 51; /* Keep it in a reasonable bounds to avoid overflow */
    if ((p = new(struct type_UNIV_EXTERNAL)) == NULL
    || (p->encoding = new(struct choice_UNIV_0)) == NULL)
	ferr(1, "mkext:malloc:out of memory\n");
    
    if (i & 0x1)
	p->direct__reference = mkoid(i*3);
    
    p->indirect__reference = k & 0x7c;

    p->data__value__descriptor = str2qb("A very wild type of data", 25, 1);

    switch (p->encoding->offset = (k % choice_UNIV_0_arbitrary) + 1) {

    case choice_UNIV_0_arbitrary:
	p->encoding->un.single__ASN1__type = mkpe(k % 7);
	break;

    case choice_UNIV_0_single__ASN1__type:
	p->encoding->un.single__ASN1__type = mkpe(k % 5);
	break;

    case choice_UNIV_0_octet__aligned:
	p->encoding->un.octet__aligned = str2qb("Some test data", 15, 1);
	break;

    default:
	ferrd(1, "mkext:internal error: bad offset %d\n", p->encoding->offset);

    }

    return (p);
}

/*
 * compare two external types to see if they are identical - return zero if
 * they are and non zero if they are different
 */
ext_cmp(e1, e2)
register struct      type_UNIV_EXTERNAL *e1, *e2;
{
     if (e1->direct__reference != NULLOID && e2->direct__reference != NULLOID) {
	 if (oid_cmp(e1->direct__reference, e2->direct__reference))
		return (1);
    } else {
	if (e1->direct__reference != NULLOID || e2->direct__reference != NULLOID)
	    return (1);
	if (e1->indirect__reference != e2->indirect__reference)
	    return (1);
    }
     if (e1->data__value__descriptor != NULLQB
     && e2->data__value__descriptor != NULLQB) {
	 if (qb_cmp(e1->data__value__descriptor, e2->data__value__descriptor))
		return (1);
    } else if (e1->data__value__descriptor != NULLQB
	|| e2->data__value__descriptor != NULLQB)
	    return (1);
    if (e1->encoding == NULL || e2->encoding == NULL)
	return (1);
    if (e1->encoding->offset != e2->encoding->offset)
	return (1);
    switch (e1->encoding->offset) {
    case choice_UNIV_0_single__ASN1__type:
	 if (e1->encoding->un.single__ASN1__type == NULLPE
	 || e2->encoding->un.single__ASN1__type == NULLPE)
	     return (1);
	 if (pe_cmp(e1->encoding->un.single__ASN1__type,
	     e2->encoding->un.single__ASN1__type))
	    return (1);
	break;

    case choice_UNIV_0_octet__aligned:
	 if (e1->encoding->un.octet__aligned == NULLQB
	 || e2->encoding->un.octet__aligned == NULLQB)
	     return (1);
	 if (qb_cmp(e1->encoding->un.octet__aligned,
	     e2->encoding->un.octet__aligned))
	    return (1);
	break;

    case choice_UNIV_0_arbitrary:
	 if (e1->encoding->un.arbitrary == NULLPE
	 || e2->encoding->un.arbitrary == NULLPE)
	     return (1);
	 if (pe_cmp(e1->encoding->un.arbitrary,
	     e2->encoding->un.arbitrary))
	    return (1);
	break;

    default:
	ferrd(1, "ext_cmp:illegal offset value %d\n", e1->encoding->offset);
    }

    return (0);
}
/*
 * print the PE structure pointed to by pe
 */
print_pe(pe, n)
PE      pe;
int     n;
{

    if (pe == NULL)
	return;
    (void) printf("%*s", 4 * n, "");
    if (pe->pe_errno)
	(void) printf(" errno = %d", pe->pe_errno);
    if (pe->pe_class == PE_CLASS_UNIV)
	(void) printf(" %s", idname( (int )pe->pe_id));
    else if (pe->pe_class == PE_CLASS_CONT)
	(void) printf("[%d]", pe->pe_id);
    else
	(void) printf("[%s %d]", clname( (int )pe->pe_class), pe->pe_id);

    (void) printf("\n");

    if (pe->pe_form != 0x0) {
	if (pe->pe_cons != NULLPE)
	    print_pe(pe->pe_cons, n + 1);
    } else {
	(void) printf("%*s", 4 * n, "");
	switch (pe->pe_id) {
	case PE_PRIM_BOOL:
	    (void) printf("%d", prim2flag(pe));
	    break;

	case PE_PRIM_INT:
	    (void) printf(" %d", prim2num(pe));
	    break;
	case PE_PRIM_BITS:
	    prntbits(pe);
	    break;

	case PE_PRIM_OCTS:
	    (void) prntos(pe);
	    break;

	case PE_PRIM_NULL:
	    break;


	case PE_DEFN_NUMS:
	case PE_DEFN_PRTS:
	case PE_DEFN_T61S:
	case PE_DEFN_VTXS:
	case PE_DEFN_IA5S:
	case PE_DEFN_GFXS:
	case PE_DEFN_VISS:
	case PE_DEFN_GENS:
	case PE_DEFN_CHRS:
	    (void) prntstr(pe);
	    break;


	case PE_PRIM_OID:
	case PE_CONS_EXTN:
	case PE_PRIM_REAL:
	case PE_PRIM_ENUM:
	case PE_PRIM_ENCR:

	case PE_CONS_SEQ:
	case PE_CONS_SET:

	case PE_DEFN_UTCT:
	case PE_DEFN_GENT:
	default:
	    (void) printf("Unimplemented %d ", pe->pe_id);
	    break;
	}
	(void) printf("\n");
    }
    if (pe->pe_next != NULLPE) {
	(void) printf("%*s", 4 * n, "pe_next:\n");
	print_pe(pe->pe_next, n);
    }
}

/*
 * return the string describing that class
 */
static char   *
clname(cl)
int     cl;
{
    char   *p;
    static char buf[30];

    switch (cl) {
    case PE_CLASS_UNIV:
	p = "Universal";
	break;

    case PE_CLASS_APPL:
	p = "Application";
	break;

    case PE_CLASS_CONT:
	p = "Context";
	break;

    case PE_CLASS_PRIV:
	p = "Private";
	break;

    default:
	(void) sprintf(buf, "Unknown Class %d", cl);
	p = buf;
	break;
    }
    return (p);
}


/*
 * return the string describing that identity or the number itself
 * Assuming a Universal class
 */
static char   *
idname(id)
int     id;
{
    char   *p;
    static char buf[40];

    switch (id) {
    case PE_PRIM_BOOL:
	p = "Boolean";
	break;

    case PE_PRIM_INT:
	p = "Integer";
	break;

    case PE_PRIM_BITS:
	p = "Bit String";
	break;

    case PE_PRIM_OCTS:
	p = "Octet String";
	break;

    case PE_PRIM_NULL:
	p = "Null";
	break;

    case PE_PRIM_OID:
	p = "Object Descriptor";
	break;

    case PE_CONS_EXTN:
	p = "External";
	break;

    case PE_PRIM_REAL:
	p = "Real";
	break;

    case PE_PRIM_ENUM:
	p = "Enumerated Type";
	break;

    case PE_PRIM_ENCR:
	p = "Encrypted Type";
	break;

    case PE_CONS_SEQ:
	p = "Sequence";
	break;

    case PE_CONS_SET:
	p = "Set";
	break;

    case PE_DEFN_NUMS:
	p = "Numeric String";
	break;

    case PE_DEFN_PRTS:
	p = "Printable String";
	break;

    case PE_DEFN_T61S:
	p = "T.61 String";
	break;

    case PE_DEFN_VTXS:
	p = "Videotex String";
	break;

    case PE_DEFN_IA5S:
	p = "IA5 String";
	break;

    case PE_DEFN_UTCT:
	p = "UTC Time";
	break;

    case PE_DEFN_GENT:
	p = "Generalised Time";
	break;

    case PE_DEFN_GFXS:
	p = "Graphics String";
	break;

    case PE_DEFN_VISS:
	p = "Visable String";
	break;

    case PE_DEFN_GENS:
	p = "General String";
	break;

    case PE_DEFN_CHRS:
	p = "Character String";
	break;

    default:
	(void) sprintf(buf, "Unknown Universal %d", id);
	p = buf;
	break;
    }
    return (p);
}
/*
 * Print out the value of a bits string
 */
static prntbits(pe)
PE      pe;
{
    int     len, i;

    if ((len = pe->pe_nbits) < 0) {
	(void) printf("prntbits:Bad bistring\n");
	return;
    }
    (void) printf("Bits:");
    for (i = 0; i < len; i++)
	if (bit_test(pe, i))
	    (void) printf(" %d", i);

    (void) putchar('\n');
}

/*
 * Dump a bunch of hex digits printing out those that are printable
 * Print out a given length of octets as hex (with the ASCII
 * characters given if they have any
 */
static pclen(s, len)
register char *s;
register int len;
{
    register int cnt = 0;

    while (len-- > 0) {
	if (cnt % 8 == 0)
	    (void) printf("\n%d:", cnt / 8 + 1);
	if (isprint(*s & 0x7f))
	    (void) printf("\t%02x(%c)", *s & 0xff, *s & 0x7f);
	else
	    (void) printf("\t%02x", *s & 0xff);
	s++;
	cnt++;
    }
    (void) putchar('\n');
}

/*
 * print out an octet string
 */
static prntos(pe)
PE      pe;
{
    struct qbuf *qb;

    if ((qb = prim2qb(pe)) == NULL) {
bad:
	(void) printf("prntos:bad octet string\n");
	return (NOTOK);
    }
    if (qb_pullup(qb) == NOTOK)
	goto bad;

    if (qb->qb_forw->qb_data == NULL || qb->qb_forw->qb_len < 0)
	goto bad;

    pclen(qb->qb_forw->qb_data, qb->qb_forw->qb_len);
    return (OK);
}

/*
 * print out a string which should be printable
 */
static prntstr(pe)
PE      pe;
{
    struct qbuf *qb;

    if ((qb = prim2qb(pe)) == NULL) {
bad:
	(void) printf("prntstr:bad string\n");
	return (NOTOK);
    }
    if (qb_pullup(qb) == NOTOK)
	goto bad;

    if (qb->qb_forw->qb_data == NULL || qb->qb_forw->qb_len < 0)
	goto bad;

    (void) printf("\"%s\"", qb->qb_forw->qb_data);
    return (OK);
}

/*
 * build a link list of struct rep_int containing the speicified number of
 * elements
 */
struct rep_int	*
mkrep_int(cnt)
int	cnt;
{
    struct rep_int	*hd, *tl, *p;

    for (hd = NULLREP_INT, tl = NULL; cnt-- > 0; ) {
	if ((p = new(struct rep_int)) == NULLREP_INT)
	    ferr(1, "mkrep_int:malloc failed\n");
	
	p->i = t_test*cnt + 3;
	if (tl) {
	    tl->r = p;
	    tl = p;
	} else
	    hd = tl = p;
    }

    return (hd);
}

static char *test_str[] = { "The quick", "brown", "fox jumps over",
			    "The Lazy", "dog", 0 };

/*
 * build a link list of struct rep_elem containing the speicified number of
 * elements
 */
struct rep_elem	*
mkrep_elem(cnt)
int	cnt;
{
    struct rep_elem	*hd, *tl, *p;
    char	**str;

    for (str = test_str, hd = NULLREP_ELEM, tl = NULL; cnt-- > 0; ) {
	if ((p = new(struct rep_elem)) == NULLREP_ELEM)
	    ferr(1, "mkrep_elem:malloc failed\n");
	
	if (*str == NULLCP)
	    str = test_str;
	
	p->r_int = t_test + cnt + 3;
	p->r_ostring = strdup(*str++);
	p->r_bstring = strdup("1234567890abcdefghijklmnopqrstuvwxyz");
	if (tl) {
	    tl->r_next = p;
	    tl = p;
	} else
	    hd = tl = p;
    }

    return (hd);
}

/*
 * return non zero if the to lists are different - also
 * prints a message about the difference found
 */
ri_cmp(p1, p2)
struct rep_int	*p1, *p2;
{
    int	cnt;


    for (cnt = 1; p1 && p2; cnt++) {
	if (p1->i != p2->i) {
	    (void) printf("ri_cmp: Integers differ in %d item (%d != %d)\n",
		cnt, p1->i, p2->i);
	    return (1);
	}
	p1 = p1->r;
	p2 = p2->r;
    }
    if (p1) {
	(void) printf("ri_cmp: 1st list has more items (> %d)\n", cnt);
       return(1);
    }
    if (p2) {
	(void) printf("ri_cmp: 2nd list has more items (> %d)\n", cnt);
       return(1);
    }

    return (0);
}

/*
 * return non zero if the to lists are different - also
 * prints a message about the difference found
 */
re_cmp(p1, p2)
struct rep_elem	*p1, *p2;
{
    int	cnt;


    for (cnt = 1; p1 && p2; cnt++) {
	if (p1->r_int != p2->r_int) {
	    (void) printf("re_cmp: Integers differ in %d item (%d != %d)\n",
		cnt, p1->r_int, p2->r_int);
	    return (1);
	}
	if (strcmp(p1->r_ostring, p2->r_ostring)) {
	    (void) printf("re_cmp: strings differ in %d item (%s != %s)\n",
		cnt, p1->r_ostring, p2->r_ostring);
	    return (1);
	}
	if (bitscmp(p1->r_bstring, p2->r_bstring, p1->r_int)) {
	    (void) printf("re_cmp: bit strings differ in %d item\n", cnt);
	    return (1);
	}
	p1 = p1->r_next;
	p2 = p2->r_next;
    }
    if (p1) {
	(void) printf("re_cmp: 1st list has more items (> %d)\n", cnt);
       return(1);
    }
    if (p2) {
	(void) printf("re_cmp: 2nd list has more items (> %d)\n", cnt);
       return(1);
    }

    return (0);
}
