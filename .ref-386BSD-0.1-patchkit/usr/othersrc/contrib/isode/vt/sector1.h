/* sector1.h - VTPM: sector 1 definitions */

/* 
 * $Header: /f/osi/vt/RCS/sector1.h,v 7.1 91/02/22 09:48:08 mrose Interim $
 *
 *
 * $Log:	sector1.h,v $
 * Revision 7.1  91/02/22  09:48:08  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:39  mrose
 * Release 6.0
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


#define MAXSPARGS	2	/*Max Special Profile Arguments (2 for TLENET)*/
#define MAXCDSOBJ	2	/*Max Display Objects (2 for TRANSPARENT)*/
#define MAXCSSOBJ	1	/*Max Control Objects to negotiate*/
#define MAXDEVOBJ	1	/*Max Device Objects*/
#define MAXFONTS	1
#define MAXREPS		1	/*Maximum Repertoires*/
#define MAXEMPS		1	/*Max Background Emphasis*/
#define MAXCOLORS	1	/*Aw C'mon*/


/* ASQ PDU Fields*/
#define ASQ_basic	0
#define ASQ_Imp_Ident	1
#define ASQ_Func_Units	2
#define ASQ_Profile	3
#define ASQ_P_Version	4
#define ASQ_Coll_Win	5

/* ASR PDU Fields */
#define ASR_Fail_String	0
#define ASR_Fail_Reason	1
#define ASR_Result	2
#define ASR_Imp_Ident	3
#define ASR_P_Version	4
#define ASR_Arg_List	5
#define ASR_Func_Units	6
#define ASR_Coll_Win	7

/* Functional Units Bit Map*/
#define	profileSwitch	0x01
#define profileMIN	0x02
#define negRelease	0x04
#define urgData		0x08
#define destBreak	0x10

/* NDQ ASN.1 Types */
#define ECHO_ON		0
#define ECHO_OFF	1
#define DISPLAY_OBJ	0
#define CTRL_OBJ	1
#define DO_NEXT_X	0
#define DO_NEXT_Y	1
#define DO_PTR_REL	2
#define DO_PTR_ABS	3
#define DO_TEXT		4
#define DO_RPT_TEXT	5
#define DO_ATTR		6
#define DO_ERASE	7
#define DO_PREV_X	8
#define DO_PREV_Y	9

/*DI/KB Control Objects*/
#define KB_SIZE		5	/* network bit ordering */
#define IP_OBJ		0x80
#define AO_OBJ		0x40
#define AYT_OBJ		0x20
#define DM_OBJ		0x10
#define BRK_OBJ		0x08

/*NI/NA Control Objects*/
#define NA_SIZE		4	/* network bit ordering */
#define ECHO_OBJ	0x80	/*0 is Local; 1 is Remote*/
#define SUP_GA		0x40	/*0 is Use Go Ahead; 1 is Suppress Go Ahead*/
#define DISP_BIN	0x20	/*1 = WACA is Binary; 0 = WACA is ASCII*/
#define KBD_BIN		0x10	/*1 = WACI is Binary; 0 = WACI is ASCII*/

/*Go Ahead Control Object*/
#define	GA_SIZE		1	/* network bit ordering */
#define GO_AHEAD	0x80

/*Synch Control Object*/
#define	SYNC_SIZE	1	/* network bit ordering */
#define SYNC		0x80

/*Default Profile Control Object*/
#define	DEF_SIZE	1	/* network bit ordering */
#define DEF_ECHO	0x80	/*True for local echo*/

#define FULL_ASCII	"ASCII"	/*TEMP repertoire ID*/
#define ASCII_GO	"GO"
#define TRANSPARENT	"TRANS"

typedef struct trans_args	/*Arguments for transparent profile*/
{
	int num_reps;
	char *rep_ptr[MAXREPS];
	char *cur_rep;		/*Currently active repertoire*/
} TRANS_ARGS;

typedef struct telnet_args	/*Arguments for telnet profile*/
{
	int x_window;
	char full_ascii;	/*If 1, Full ASCII.  If 0, graphics only*/
} TELNET_ARGS;

typedef struct vt_profile 	/*Structure for profile parameters*/
{
	char *profile_name;
	union
	{
		TRANS_ARGS	tr_arg_list;
		TELNET_ARGS	tel_arg_list;
	} arg_val;
} VT_PROFILE;

/* Data Structures for PDU's */

typedef struct ex_pointer	/*Explicit Pointer*/
{
	int x_true;
	int x_value;
	int y_true;
	int y_value;
	int z_true;
	int z_value;
} EX_POINTER;

typedef struct pointer	/*General Pointer*/
{
	int ptr_type;	/*Values 0 - 6 */
	EX_POINTER e_ptr;
} POINTER;

typedef struct rpt_text		/*Repeat text*/
{
	POINTER fin_addr;
	int text_count;
	char *text;
} RPT_TEXT;

typedef struct erase_text	/*Erase*/
{
	POINTER start_erase;
	POINTER end_erase;
	int erase_attr;		/*Boolean*/
} ERASE_TEXT;

typedef struct text
{
	int text_count;
	char *text_ptr;
} TEXT_CONTENT;

typedef struct attrib
{
	int attr_id;		/* 0 - 4*/
	int attr_val;
	int attr_ext;
	POINTER beg_p;
	POINTER end_p;
} ATTRIB;

typedef struct bool_u
{
	int val_count;
	char *value;
	int mask_count;
	char *mask;
} BOOL_U;

typedef struct bit_str
{
	int bitcount;
	int bitstring;
} BIT_STR;

typedef struct do_update	/*Display Object Update*/
{
	char *do_name;
	int do_type;		/* 0 - 9 */
	union
	{
		EX_POINTER ptr_rel;
		POINTER ptr_abs;
		TEXT_CONTENT text_ud;
		RPT_TEXT rpt_seq;
		ATTRIB wrt_attrib;
		ERASE_TEXT erase;
	} do_cmd;
} DO_UPDATE;

typedef struct co_update	/*Control Object Update*/
{
	char *co_name;
	int co_type;		/* 0 - 4 */
	union
	{
		char *char_update;
		BOOL_U bool_update;
		int sym_update;
		int int_update;
		BIT_STR bit_update;
	} co_cmd;
} CO_UPDATE;

typedef struct text_update
{
	struct text_update *ndq_elem;	/*Pointer to next one in queue*/
	int echo_sw;		/*0 = Echo Now; 1 = Not Echo Now*/
	int type_sw;		/*0 = display; 1 = control*/
	union
	{
		DO_UPDATE do_list;
		CO_UPDATE co_list;
	} updates;
} TEXT_UPDATE;

typedef struct implem_id
{
	int oid_true;
	OID imp_oid;		/*Optional*/
	int name_true;
	char *name;		/*Optional*/
	int version_true;
	char *version;		/*Optional*/
} IMPLEM_ID;

typedef struct int_offer
{
	int type;		/*0 for single value, 1 for range*/
	int value;
	int min_val;
	int max_val;
} INT_OFFER;

typedef struct rep_font		/*Repertoire Font Offer*/
{
	int rep_type;		/*1 = NULL; 2 = SEQUENCE....*/
	char *rep_assign;	/*0 value for pointer means not used*/
	int valid_font_cap;
	INT_OFFER capability;
	int num_fonts;
	char *font_names[MAXFONTS];
} REP_FONT;

typedef struct dimen_param
{
	int bound_type;		/*0 for no bound, 1 for unbounded, 2 for
				  INT_OFFER */
	INT_OFFER bound;
	BIT_STR addressing;	/*NOT optional according to 9041*/
	BIT_STR absolute;	/*Optional*/
	int window_type;	/*0 for not used, 1 for unbounded, 2 for
				  INT_OFFER */
	INT_OFFER window;
} DIMEN_PARAM;

typedef struct rep_list		/*Repertoire list*/
{
	int valid_cap;
	INT_OFFER capability;	/*Listed as optional but seems you should
				  have it. */
	int num_reps;		/*Number of repertoires -- seems it should
				  usually equal capability. */
	REP_FONT repertoire[MAXREPS];
} REP_LIST;

typedef struct emp_list
{
	int valid_cap;
	INT_OFFER capability;	/*Technically Optional*/
	int num_emps;
	char *emp_string[MAXEMPS];
} EMP_LIST;

typedef struct color_list
{
	int valid_cap;
	INT_OFFER capability;
	int num_colors;
	char *color_string[MAXCOLORS];
} COLOR_LIST;

typedef struct cds_offer
{
	char *obj_name;
	BIT_STR dimensions;
	int valid_x_dim;
	DIMEN_PARAM x_dim;
	int valid_y_dim;
	DIMEN_PARAM y_dim;
	int valid_z_dim;
	DIMEN_PARAM z_dim;
	BIT_STR erasure;
	int valid_rep_list;
	REP_LIST rep_offer;
	int valid_emp_list;
	EMP_LIST emp_offer;
	int valid_fore_color;
	COLOR_LIST fore_color_list;
	int valid_back_color;
	COLOR_LIST back_color_list;
	BIT_STR access_right;
} CDS_OFFER;

typedef struct css_offer	/*Unused in TELNET (and hopefully Forms)*/
{
	int i;			/*For compiler*/
} CSS_OFFER;

typedef struct dev_offer	/*Also unused*/
{
	int i;			/*For compiler*/
} DEV_OFFER;

typedef struct special_offer
{
	int param_num;
	int param_type;		/*0,1,or2*/
	union
	{
		char *bool_arg;	/*Turns into bitstring = 0 or 1*/
		INT_OFFER int_arg;
		char *string_arg;
	} args;
} SPECIAL_OFFER;

typedef struct arg_offer_list
{
	int oid_true;	/*Optional--Use Default Profile if not specified*/
	OID prof_oid;
	int num_sp_param;	/*Number of special profile arguments*/
	int num_cds_objects;	/*Number of Conceptual Data Store objects*/
	int num_css_objects;	/*Number of Control Signal Status objects*/
	int num_dev_objects;	/*Number of Device Object identifiers*/
	SPECIAL_OFFER sp_offer_list[MAXSPARGS];
	CDS_OFFER cds_offer_list[MAXCDSOBJ];
	CSS_OFFER css_offer_list[MAXCSSOBJ];
	DEV_OFFER dev_offer_list[MAXDEVOBJ];
	BIT_STR del_ctrl;	/*Delivery Control*/
} ARG_OFFER_LIST;

typedef struct asq_msg
{
	int class;		/*Basic only (=1)*/
	int valid_imp;
	IMPLEM_ID imp_id;	/*Optional*/
	BIT_STR func_units;
	int valid_prof;
	ARG_OFFER_LIST asq_profile;	/*Profile is optional*/
	BIT_STR version;	/*Default = '1'B*/
	int valid_coll;
	int coll_winner;	/*Optional*/
} ASQ_MSG;

typedef struct fail_reason
{
	int type;		/*0 or 1*/
	char *usr_reason;
	int provider_reason;	/* 1,2,3,or 4*/
} FAIL_REASON;


typedef struct font_value		/*Repertoire Font Value*/
{
	int rep_type;		/*1 = NULL; 2 = SEQUENCE....*/
	char *rep_assign;	/*0 value for pointer means not used*/
	int valid_font_cap;
	int capability;
	int num_fonts;
	char *font_names[MAXFONTS];
} FONT_VALUE;

typedef struct dimen_value
{
	int bound_type;		/*0 for no bound, 1 for unbounded, 2 for
				  integer */
	int bound;
	int valid_addr;
	int addressing;	/*NOT optional according to 9041*/
	int valid_abs;
	int absolute;	/*Optional*/
	int window_type;	/*0 for not used, 1 for unbounded, 2 for
				  integer */
	int window;
} DIMEN_VALUE;

typedef struct rep_val_list		/*Repertoire value list*/
{
	int valid_cap;
	int capability;
	int num_reps;		/*Number of repertoires */
	FONT_VALUE repertoire[MAXREPS];
} REP_VALUE;

typedef struct emp_value
{
	int valid_cap;
	int capability;
	int num_emps;
	char *emp_string[MAXEMPS];
} EMP_VALUE;

typedef struct color_value
{
	int valid_cap;
	int capability;
	int num_colors;
	char *color_string[MAXCOLORS];
} COLOR_VALUE;

typedef struct cds_value
{
	char *obj_name;
	int dimensions;		/*0 if not valid*/
	int valid_x_dim;
	DIMEN_VALUE x_dim;
	int valid_y_dim;
	DIMEN_VALUE y_dim;
	int valid_z_dim;
	DIMEN_VALUE z_dim;
	int valid_erasure;
	int erasure;
	int valid_rep_list;
	REP_VALUE rep_value;
	int valid_emp_list;
	EMP_VALUE emp_value;
	int valid_fore_color;
	COLOR_VALUE fore_color_list;
	int valid_back_color;
	COLOR_VALUE back_color_list;
	int valid_access_right;
	int access_right;
} CDS_VALUE;

typedef struct css_value	/*Unused in TELNET */
{
	int i;			/*For compiler*/
} CSS_VALUE;

typedef struct dev_value	/*Also unused*/
{
	int i;			/*For compiler*/
} DEV_VALUE;

typedef struct special_value
{
	int param_num;
	int param_type;		/*0,1,or2*/
	union
	{
		int bool_arg;	/*Turns into bitstring = 0 or 1*/
		int int_arg;
		char *string_arg;
	} args;
} SPECIAL_VALUE;

typedef struct arg_val_list
{
	int num_sp_param;	/*Number of special profile arguments*/
	int num_cds_objects;	/*Number of Conceptual Data Store objects*/
	int num_css_objects;	/*Number of Control Signal Status objects*/
	int num_dev_objects;	/*Number of Device Object identifiers*/
	SPECIAL_VALUE sp_val[MAXSPARGS];
	CDS_VALUE cds_val[MAXCDSOBJ];
	CSS_VALUE css_val[MAXCSSOBJ];
	DEV_VALUE dev_val[MAXDEVOBJ];
	int del_ctrl;	/*Delivery Control*/
} ARG_VAL_LIST;

typedef struct asr_msg
{
	int valid_reason;	/*1 if reason is supplied*/
	FAIL_REASON reason;	/*Optional*/
	int result;		/*0,1, or 2*/
	int valid_imp;		/*1 if implementation i.d is supplied*/
	IMPLEM_ID imp_id;	/*Optional*/
	BIT_STR version;	/*Only '1'B now valid*/
	int valid_arg_list;
	ARG_VAL_LIST arg_list;
	BIT_STR func_units;
	int valid_coll;		/*Is collision_winner valid?*/
	int coll_winner;	/*Optional (0,1,2)*/
} ASR_MSG;	
	

