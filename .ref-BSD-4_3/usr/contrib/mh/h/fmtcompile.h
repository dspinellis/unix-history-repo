
/* Format Types */
/* -------------*/

/* types that output text */
#define FT_COMP		1	/* the text of a component */
#define FT_COMPF	2	/* comp text, filled */
#define FT_LIT		3	/* literal text */
#define FT_LITF		4	/* literal text, filled */
#define FT_CHAR		5	/* a single ascii character */
#define FT_NUM		6	/* "value" as decimal number */
#define FT_NUMF		7	/* "value" as filled dec number */
#define FT_STR		8	/* "str" as text */
#define FT_STRF		9	/* "str" as text, filled */
#define FT_STRFW	10	/* "str" as text, filled, width in "value" */
#define FT_PUTADDR	11	/* split and print address line */

/* types that modify the "str" or "value" registers */
#define FT_LS_COMP	12	/* set "str" to component text */
#define FT_LS_LIT	13	/* set "str" to literal text */
#define FT_LV_COMP	14	/* set "value" to comp (as dec. num) */
#define FT_LV_COMPFLAG	15	/* set "value" to comp flag word */
#define FT_LV_LIT	16	/* set "value" to literal num */
#define FT_LV_DAT	17	/* set "value" to dat[n] */
#define FT_LV_STRLEN	18	/* set "value" to length of "str" */
#define FT_LV_PLUS_L	19	/* set "value" += literal */
#define FT_LV_MINUS_L	20	/* set "value" -= literal */
#define FT_LV_CHAR_LEFT	21	/* set "value" to char left in output */

#define FT_LS_MONTH	22	/* set "str" to tws month */
#define FT_LS_LMONTH	23	/* set "str" to long tws month */
#define FT_LS_ZONE	24	/* set "str" to tws timezone */
#define FT_LS_DAY	25	/* set "str" to tws weekday */
#define FT_LS_WEEKDAY	26	/* set "str" to long tws weekday */
#define FT_LS_822DATE	27	/* set "str" to 822 date str */
#define FT_LS_PRETTY	28	/* set "str" to pretty (?) date str */
#define FT_LV_SEC	29	/* set "value" to tws second */
#define FT_LV_MIN	30	/* set "value" to tws minute */
#define FT_LV_HOUR	31	/* set "value" to tws hour */
#define FT_LV_MDAY	32	/* set "value" to tws day of month */
#define FT_LV_MON	33	/* set "value" to tws month */
#define FT_LV_YEAR	34	/* set "value" to tws year */
#define FT_LV_YDAY	35	/* set "value" to tws day of year */
#define FT_LV_WDAY	36	/* set "value" to tws weekday */
#define FT_LV_ZONE	37	/* set "value" to tws timezone */
#define FT_LV_CLOCK	38	/* set "value" to tws clock */
#define FT_LV_RCLOCK	39	/* set "value" to now - tws clock */
#define FT_LV_DAYF	40	/* set "value" to tws day flag */
#define FT_LV_TZONEF	41	/* set "value" to tws timezone flag */

#define FT_LS_PERS	42	/* set "str" to person part of addr */
#define FT_LS_MBOX	43	/* set "str" to mbox part of addr */
#define FT_LS_HOST	44	/* set "str" to host part of addr */
#define FT_LS_PATH	45	/* set "str" to route part of addr */
#define FT_LS_GNAME	46	/* set "str" to group part of addr */
#define FT_LS_NOTE	47	/* set "str" to comment part of addr */
#define FT_LS_822ADDR	48	/* set "str" to 822 format addr */
#define FT_LS_FRIENDLY	49	/* set "str" to "friendly" format addr */
#define FT_LV_HOSTTYPE	50	/* set "value" to addr host type */
#define FT_LV_INGRPF	51	/* set "value" to addr in-group flag */
#define FT_LV_NOHOSTF	52	/* set "value" to addr no-host flag */

/* pre-format processing */
#define FT_PARSEDATE	53	/* parse comp into a date (tws) struct */
#define FT_PARSEADDR	54	/* parse comp into a mailaddr struct */
#define FT_FORMATADDR	55	/* let external routine format addr */
#define FT_MYMBOX	56	/* do "mymbox" test on comp */

/* conditionals & control flow (must be last) */
#define FT_SAVESTR	57	/* save current str reg */
#define FT_DONE		58	/* stop formatting */
#define FT_NOP		59	/* nop */
#define FT_GOTO		60	/* (relative) goto */
#define FT_IF_S_NULL	61	/* test if "str" null */
#define FT_IF_S		62	/* test if "str" non-null */
#define FT_IF_V_EQ	63	/* test if "value" = literal */
#define FT_IF_V_NE	64	/* test if "value" != literal */
#define FT_IF_V_GT	65	/* test if "value" > literal */
#define FT_IF_MATCH	66	/* test if "str" contains literal */
#define FT_IF_AMATCH	67	/* test if "str" starts with literal */
#define FT_S_NULL	68	/* V = 1 if "str" null */
#define FT_S_NONNULL	69	/* V = 1 if "str" non-null */
#define FT_V_EQ		70	/* V = 1 if "value" = literal */
#define FT_V_NE		71	/* V = 1 if "value" != literal */
#define FT_V_GT		72	/* V = 1 if "value" > literal */
#define FT_V_MATCH	73	/* V = 1 if "str" contains literal */
#define FT_V_AMATCH	74	/* V = 1 if "str" starts with literal */

#define IF_FUNCS FT_S_NULL	/* start of "if" functions */
