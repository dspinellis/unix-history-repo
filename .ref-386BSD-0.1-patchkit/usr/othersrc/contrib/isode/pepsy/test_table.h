/* test_table.h */

/* 
 * $Header: /f/osi/pepsy/RCS/test_table.h,v 7.2 91/02/22 09:50:10 mrose Interim $
 *
 *
 * $Log:	test_table.h,v $
 * Revision 7.2  91/02/22  09:50:10  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/04  19:21:15  mrose
 * update
 * 
 * Revision 7.0  90/07/01  19:54:46  mrose
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


/*
 * This contains the information about each test case necessary to test it
 * e.g. size, name
 */

 struct tst_typ {
	char	*tst_name;	/* Name for error messages */
	unsigned int	tst_size; /* Size of its data structure in bytes */
	int	tst_entry;	/* entry for decoding/encoding */
	int	tst_tests;	/* How many values of the t_test variable
				 * to try it with
				 */
#define NOENTRY		-1
	}  t_case[] = {
	/* MPDU */
	{ "MPDU", sizeof (struct type_T1_MPDU), _ZMPDUT1, 1, },
#define TY_MPDU	0	
	{ "Embedded", sizeof (struct type_T1_Embedded), _ZEmbeddedT1, 1, },
#define TY_EMBEDDED	1	
	{ "Strings", sizeof (struct type_T1_Strings), _ZStringsT1, 1, },
#define TY_STRINGS	2	
	{ "Embedded Strings", sizeof (struct type_T1_Emb__Strings),
	    _ZEmb_StringsT1, 1, },
#define TY_EMB_STRINGS	3	
	{ "Implicit tags", sizeof (struct type_T1_Impl__Tags),
	    _ZImpl_TagsT1, 1, },
#define TY_IMPLICIT	4	
	{ "Explicit tags", sizeof (struct type_T1_Expl__Tags),
	    _ZExpl_TagsT1, 1, },
#define TY_EXPLICIT	5	
	{ "SEQ OF and SET OF", sizeof (struct type_T1_Seqof__Test),
	    _ZSeqof_TestT1, 10, },
#define TY_SEQOF	6	
	{ "Seqof Test 1", sizeof (struct element_T1_4), NOENTRY, 0, },
#define TY_ELEMENT4	7	
	{ "Set of Test 1", sizeof (struct member_T1_2), NOENTRY, 0, },
#define TY_MEMBER2	8	
	{ "Seq of Test 2", sizeof (struct element_T1_6), NOENTRY, 0, },
#define TY_ELEMENT6	9	
	{ "Seq of Sequence test", sizeof (struct element_T1_8), NOENTRY, 0, },
#define TY_ELEMENT8	10	
	{ "Set of Test 2", sizeof (struct member_T1_4), NOENTRY, 0, },
#define TY_MEMBER4	11	
	{ "Set of Sequence", sizeof (struct element_T1_9), NOENTRY, 0, },
#define TY_ELEMENT9	12	
	{ "Choice", sizeof (struct type_T1_Choice__Test), _ZChoice_TestT1, 7, },
#define TY_CHOICE	13	
	{ "Choice test 0", sizeof (struct choice_T1_0), NOENTRY, 0, },
#define TY_CHOICE0	14	
	{ "Choice test 1", sizeof (struct choice_T1_1), NOENTRY, 0, },
#define TY_CHOICE1	15
	{ "Choice test 2", sizeof (struct choice_T1_2), NOENTRY, 0, },
#define TY_CHOICE2	16
	{ "Element 10", sizeof (struct element_T1_10), NOENTRY, 0, },
#define TY_ELEMENT10	17
	{ "Member 6", sizeof (struct member_T1_6), NOENTRY, 0, },
#define TY_MEMBER6	18
	{ "Element 11", sizeof (struct element_T1_11), NOENTRY, 0, },
#define TY_ELEMENT11	19
	{ "Choice test 3", sizeof (struct choice_T1_3), NOENTRY, 0, },
#define TY_CHOICE3	20
	{ "Optional test", sizeof (struct type_T1_Opt__Strings), _ZOpt_StringsT1,
	8, },
#define TY_OPTIONAL	21
	{ "Element 12", sizeof (struct element_T1_12), NOENTRY, 0, },
#define TY_ELEMENT12	22
	{ "Member 7", sizeof (struct member_T1_7), NOENTRY, 0, },
#define TY_MEMBER7	23
	{ "Choice test 4", sizeof (struct choice_T1_4), NOENTRY, 0, },
#define TY_CHOICE4	24
	{ "Default test", sizeof (struct type_T1_Def__Strings), _ZDef_StringsT1,
	12, },
#define TY_DEFAULT	25
	{ "Element 13", sizeof (struct element_T1_13), NOENTRY, 0, },
#define TY_ELEMENT13	26
	{ "Member 8", sizeof (struct member_T1_8), NOENTRY, 0, },
#define TY_MEMBER8	27
	{ "External References", sizeof (struct type_T1_E__ref), _ZE_refT1, 6,},
#define TY_EXTREF	28
	{ "T2 Info", sizeof (struct type_T2_Info), NOENTRY, 0, },
#define TY_T2_INFO	29
	{ "T2 MPDU", sizeof (struct type_T2_MPDU), NOENTRY, 0, },
#define TY_T2_MPDU	30
	{ "T2 ELEMENT 0", sizeof (struct element_T2_0), NOENTRY, 0, },
#define TY_T2_ELEM0	31
	{ "Optimised", sizeof (struct type_T1_Optimised), _ZOptimisedT1, 8, },
#define TY_OPTIMISED	32
	{ "MEMBER 9", sizeof (struct member_T1_9), NOENTRY, 0, },
#define TY_MEMBER9	33
	{ "EXTERNAL", sizeof (struct type_T1_Ext__typ), _ZExt_typT1, 6, },
#define TY_EXTERNAL	34
	{ "Single EXTERNAL", sizeof (struct type_T1_SExt), _ZSExtT1, 1, },
#define TY_SEXTERNAL	35
	{ "Explicit Tagged Objects", sizeof (struct type_T1_Etags), _ZEtagsT1, 3, },
#define TY_ETAGOBJ	36
	{ "Single Objects", sizeof (struct type_T1_Stest), _ZStestT1, 4, },
#define TY_STEST	37
	{ "Single Integer", sizeof (struct type_T1_Sint), NOENTRY, 0, },
#define TY_SINT		38
	{ "Enumerated Type", sizeof (struct type_T1_Enum__type), _ZEnum_typeT1,
	4, },
#define TY_ETYPE	39
	{ "Tests of Enumerated type", sizeof (struct type_T1_T__enum),
	_ZT_enumT1, 4, },
#define TY_ENUM_TEST	40
#define TY_REAL		41
#ifdef	PEPSY_REALS
	{ "Real", sizeof (struct type_T1_Real), _ZRealT1, 3, },

	{ "Tests of Real type", sizeof (struct type_T1_T__real),
	_ZT_realT1, 4, },

	{ "Pepy Stuff", sizeof (struct pepy_refs), _ZT_pepyT1, 3, },

	{ "Default Pepy", sizeof (struct pepy_refs1), NOENTRY, 0, },

	{ "Optional Pepy", sizeof (struct pepy_refs1), NOENTRY, 0, },

	{ "Compound S-types", sizeof (struct pepy_refs1), _ZT3_SingleT1, 3, },

#else
	{ NULLCP, 0, NOENTRY, 3, },

	{ NULLCP, 0, NOENTRY, 4, },

	{ NULLCP, 0, NOENTRY, 3, },

	{ NULLCP, 0, NOENTRY, 3, },

	{ NULLCP, 0, NOENTRY, 0, },

	{ NULLCP, 0, NOENTRY, 3, },
#endif
#define TY_REAL_TEST	42
#define TY_PEPY		43
#define TY_DEFPEPY	44
#define TY_OPTPEPY	45
#define TY_S_COMPD	46

	{"Repeating pepy elements",sizeof (struct repeats), _ZT3_RepeatT1, 3, },
#define TY_REPEAT	47

#ifdef PEPSY_REALS

	{"Value Passing Defined types",sizeof (struct pepy_refs),
	    _ZT3_DefinedT1, 3, },

#else
	{ NULLCP, 0, NOENTRY, 3, },

#endif

#define TY_VPDEFINED	48

	{"function calling code",sizeof (struct codedata), _ZT3_CodeTestT1, 3,},
#define TY_FUNC	49

	{"optional function calling",sizeof (struct codedata), NOENTRY, 3,},
#define TY_OPTFUNC	50

	{"default function calling",sizeof (struct codedata), NOENTRY, 3,},
#define TY_DFTFUNC	51

#ifdef PEPSY_REALS

	{"All Simple Types",sizeof (struct codedata), _ZT3_AllSimplesT1, 3,},

#else
	{ NULLCP, 0, NOENTRY, 3, },

#endif

#define TY_ASIMPLE	52

	{"Action Statements",sizeof (struct repeats), _ZT3_UActionT1, 3, },
#define TY_ACTION	53
	};

#define MAXTCASE	(sizeof (t_case)/sizeof (t_case[0]))
