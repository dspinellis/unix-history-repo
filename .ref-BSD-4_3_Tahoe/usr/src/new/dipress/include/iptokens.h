/*
 *  Interpress utilities
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  Written for Xerox Corporation by William LeFebvre
 *  24-May-1984
 *
 * HISTORY
 * 07-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Changed the IP Header to be Interpress 2.1 instead of 3.0 .
 *	The 8044 services 9.0 doesn't like masters that begin
 *	"Interpress/Xerox/3.0 ".
 *
 * 03-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Updated to Interpress 3.0 .  Also fixed some typos and marked those
 *	operators which appear here which aren't in the Interpress manual.
 *	Some are old operators that have been dropped but many aren't documented
 *	at all.  How they got here is a mystery.  Perhaps it is part of research
 *	Interpress.
 *
 * 18-Mar-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added definition for RES_Header.
 *
 *
 */

/*  This header appears at the front of every Interpress file  */

# define  IP_Header	"Interpress/Xerox/2.1 "

# define  RES_Header	"Interpress/Xerox/2.1/RasterEncoding/1.0 "

/* For convenience -- POINT is the size of a point in meters */
# define	POINT	(0.00035278)

/*
 *  Definitions pertaining to InterPress tokes:
 */

# define	SHORT_OP	0200
# define	LONG_OP		0240
# define	SHORT_INTEGER	0000
# define	SHORT_SEQUENCE	0300
# define	LONG_SEQUENCE	0340

/* these help us cope with short integer tokens: */
# define	INTEGER_ZERO	4000
# define	INTEGER_MAX	(32767 - INTEGER_ZERO)
# define	INTEGER_MIN	(0 - INTEGER_ZERO)

/* the long and short of sequences: */
# define	SHORT_SEQUENCE_LIMIT	255		/* (8 bits) */
# define	LONG_SEQUENCE_LIMIT	16777215	/* (24 bits) */

/* Sequence types: */
# define	sequenceAdaptivePixelVector	12
# define	sequenceAPV			sequenceAdaptivePixelVector
# define	sequenceCCITT_4PixelVector	13
# define	sequenceC4PV			sequenceCCITT_4PixelVector
# define	sequenceComment			6
# define	sequenceCompressedPixelVector	10
# define	sequenceCPV			sequenceCompressedPixelVector
# define	sequenceContinued		7
# define	sequenceIdentifier		5
# define	sequenceInsertFile		11
# define	sequenceInsertMaster		3
# define	sequenceInteger			2
# define	sequenceLargeVector		8
# define	sequencePackedPixelVector	9
# define	sequencePPV			sequencePackedPixelVector
# define	sequenceRational		4
# define	sequenceString			1

/* imager variables: */
# define	I_DCScpx			0
# define	I_DCScpy			1
# define	I_correctMX			2
# define	I_correctMY			3
# define	I_T				4
# define	I_priorityImportant		5
# define	I_mediumXSize			6
# define	I_mediumYSize			7
# define	I_fieldXMin			8
# define	I_fieldYMin			9
# define	I_fieldXMax			10
# define	I_fieldYMax			11
# define	I_showVec			12	/* old: version 2.1 */
# define	I_font				12
# define	I_color				13
# define	I_noImage			14
# define	I_strokeWidth			15
# define	I_strokeEnd			16
# define	I_underlineStart		17
# define	I_amplifySpace			18
# define	I_correctPass			19
# define	I_correctShrink			20
# define	I_correctTX			21
# define	I_correctTY			22
# define	I_strokeJoint			23
# define	I_clipper			24

# define	I_LIMIT				I_clipper

/* operation codes: */
# define	OP_nil				0
# define	OP_nop				1

# define	OP_setxy			10
# define	OP_setxyrel			11
# define	OP_setxrel			12
# define	OP_setyrel			13
# define	OP_linetox			14
# define	OP_linetoy			15
# define	OP_space			16
# define	OP_get				17
# define	OP_iget				18
# define	OP_iset				19
# define	OP_fget				20
# define	OP_fset				21
# define	OP_show				22
# define	OP_lineto			23
# define	OP_maskstroke			24
# define	OP_moveto			25

# define	SHORT_OP_LIMIT			31

# define	OP_metricMaster			100	/* ?? */
# define	OP_environmentMaster		101	/* ?? */
# define	OP_beginBlock			102
# define	OP_endBlock			103
# define	OP_noPages			104	/* ?? */
# define	OP_pageInstructions		105	/* old: version 2.1 */
# define	OP_contentInstructions		105
# define	OP_beginBody			106
# define	OP_endBody			107

# define	OP_correct			110

# define	OP_makesimpleco			114
# define	OP_makeco			115	/* ?? */
# define	OP_makecompiledimage		116	/* ?? */
# define	OP_findoperator			116

# define	OP_dosavesimplebody		120
# define	OP_dobody			121	/* ?? */
# define	OP_dosavebody			122	/* ?? */
# define	OP_dosaveallbody		123	/* ?? */

# define	OP_maskchar			140

# define	OP_showandfixedxrel		145
# define	OP_showandxrel			146
# define	OP_findfont			147
# define	OP_modifyfont			148
# define	OP_finddecompressor		149
# define	OP_findfontvec			150	/* old: ?? */
# define	OP_setfont			151

# define	OP_setcorrectmeasure		154
# define	OP_setcorrecttolerance		155
# define	OP_correctmask			156	/* ?? */
# define	OP_correctspace			157	/* ?? */

# define	OP_getcp			159
# define	OP_maket			160
# define	OP_opent			161	/* ?? */
# define	OP_translate			162
# define	OP_rotate			163
# define	OP_scale			164
# define	OP_concat			165
# define	OP_scale2			166
# define	OP_invert			167	/* ?? */
# define	OP_concatt			168
# define	OP_move				169
# define	OP_trans			170

# define	OP_transform			174	/* ?? */
# define	OP_transformvewc		175	/* ?? */
# define	OP_roundxy			176	/* ?? */
# define	OP_roundxyvec			177	/* ?? */

# define	OP_pop				180
# define	OP_dup				181

# define	OP_copy				183
# define	OP_roll				184
# define	OP_exch				185
# define	OP_mark				186
# define	OP_unmark			187
# define	OP_count			188

# define	OP_unmark0			192

# define	OP_abs				200
# define	OP_add				201
# define	OP_and				202
# define	OP_ceiling			203
# define	OP_div				204
# define	OP_eq				205
# define	OP_floor			206
# define	OP_ge				207
# define	OP_gt				208
# define	OP_mod				209
# define	OP_mul				210
# define	OP_neg				211
# define	OP_not				212
# define	OP_or				213
# define	OP_sub				214
# define	OP_trunc			215
# define	OP_rem				216
# define	OP_round			217
# define	OP_eqn				218	/* internal */

# define	OP_type				220
# define	OP_atan				221	/* ?? */
# define	OP_cos				222	/* ?? */
# define	OP_exp				223	/* ?? */
# define	OP_log				224	/* ?? */
# define	OP_sin				225	/* ?? */
# define	OP_sqrt				226	/* ?? */
# define	OP_max				227	/* ?? */
# define	OP_min				228	/* ?? */

# define	OP_do				231
# define	OP_dosave			232
# define	OP_dosaveall			233

# define	OP_if				239
# define	OP_ifcopy			240
# define	OP_ifelse			241
# define	OP_loop				242	/* ?? */

# define	OP_frame			250	/* ?? */

# define	OP_poolop			253	/* ?? */
# define	OP_pool				254	/* ?? */
# define	OP_pget				255	/* internal */
# define	OP_pset				256	/* internal */
# define	OP_makepool			257	/* ?? */
# define	OP_nopool			258	/* ?? */

# define	OP_env				260	/* ?? */

# define	OP_makeveclu			282
# define	OP_makevec			283

# define	OP_shape			285
# define	OP_openvec			286	/* ?? */
# define	OP_getp				286

# define	OP_getprop			287
# define	OP_mergeprop			288

# define	OP_dround			300	/* internal */
# define	OP_getcprounded			301	/* ?? */

# define	OP_curveto			402
# define	OP_arcto			403
# define	OP_conicto			404

# define	OP_maskfill			409
# define	OP_maskrectangle		410
# define	OP_masktrapezoidx		411
# define	OP_masktrapezoidy		412
# define	OP_startunderline		413
# define	OP_maskunderline		414

# define	OP_makeoutlineodd		416
# define	OP_makeoutline			417
# define	OP_clipoutline			418
# define	OP_cliprectangle		419

# define	OP_findcoloroperator		421
# define	OP_findcolormodeloperator	422
# define	OP_findcolor			423
# define	OP_setgray			424
# define	OP_makegray			425
# define	OP_makesampledblack		426
# define	OP_makesampledcolor		427
# define	OP_setsampledblack		428
# define	OP_setsampledcolor		429

# define	OP_maskstrokeclosed		440
# define	OP_maskvector			441

# define	OP_maskdashedstroke		442

# define	OP_makepixelarray		450
# define	OP_extractpixelarray		451
# define	OP_maskpixel			452

# define	OP_error			600

# define	OP_LIMIT			OP_error
