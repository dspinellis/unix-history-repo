/* $XConsortium: phigs.h,v 5.10 91/08/23 17:16:47 hersh Exp $ */

/***********************************************************
Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. and the X Consortium.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Sun Microsystems,
the X Consortium, and MIT not be used in advertising or publicity 
pertaining to distribution of the software without specific, written 
prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef PHIGS_H_INCLUDED
#define PHIGS_H_INCLUDED

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <stdio.h>
/* #include <sys/types.h> Xlib.h does this */
#include "attr.h"
#include "phigscfunc.h"
#include "phigsextrn.h"
#include "phigserr.h"

#ifdef min
#undef min
#endif

#ifdef max
#undef max
#endif

#ifdef major
#undef major
#endif

#ifdef minor
#undef minor
#endif

#ifndef FALSE
#define FALSE	0
#endif
#ifndef TRUE
#define TRUE	1
#endif

#if NeedFunctionPrototypes
typedef void * Pconnid;
#else
typedef char * Pconnid;
#endif


typedef struct {
    Display	*display;
    XID		drawable_id;
} Pconnid_x_drawable;

typedef int Pint;

typedef long Plong;

typedef float Pfloat;

#if NeedFunctionPrototypes
typedef void *Pstore;
#else
typedef char *Pstore;
#endif

typedef Pfloat Pmatrix3[4][4];

typedef Pfloat Pmatrix[3][3];

/* These are the base ws types */
extern	Pint		phigs_ws_type_x_tool;
extern	Pint		phigs_ws_type_x_drawable;

typedef enum {
    PWS_INDEP,
    PWS_DEP
} Pws_dep_ind;

typedef enum {
    PSYS_ST_PHCL,
    PSYS_ST_PHOP
} Psys_st;

typedef enum {
    PWS_ST_WSCL,
    PWS_ST_WSOP
} Pws_st;

typedef enum {
    PSTRUCT_ST_STCL,
    PSTRUCT_ST_STOP
} Pstruct_st;

typedef enum {
    PSTRUCT_STATUS_NON_EXISTENT,
    PSTRUCT_STATUS_EMPTY,
    PSTRUCT_STATUS_NOT_EMPTY
} Pstruct_status;

typedef enum {
    PST_ARCL,
    PST_AROP
} Par_st;

typedef enum {
    PCLASS_VEC,
    PCLASS_RASTER,
    PCLASS_OTHER
} Pws_class;

typedef enum {
    PCAT_OUT,
    PCAT_IN,
    PCAT_OUTIN,
    PCAT_MO,
    PCAT_MI
} Pws_cat;

typedef enum {
    PFLAG_COND,
    PFLAG_ALWAYS
} Pctrl_flag;

typedef enum {
    PFLAG_POSTPONE,
    PFLAG_PERFORM
} Pregen_flag;

typedef enum {
    PDEFER_ASAP,
    PDEFER_BNIG,
    PDEFER_BNIL,
    PDEFER_ASTI,
    PDEFER_WAIT
} Pdefer_mode;

typedef enum {
    PDISTING_NO,
    PDISTING_YES
} Pdisting_mode;

typedef enum {
    PCULL_NONE,
    PCULL_BACKFACE,
    PCULL_FRONTFACE
} Pcull_mode;

typedef enum {
    PMODE_NIVE,
    PMODE_UWOR,
    PMODE_UQUM
} Pmod_mode;

typedef enum {
    PSIMULT_NO_MORE,
    PSIMULT_MORE
} Pmore_simult_events;

typedef enum {
    PNET_CSS,
    PNET_AR
} Pstruct_net_source;

typedef enum {
    PSURF_NOT_EMPTY,
    PSURF_EMPTY
} Pdisp_surf_empty;

typedef enum {
    PVISUAL_ST_CORRECT,
    PVISUAL_ST_DEFER,
    PVISUAL_ST_SIMULATED
} Pvisual_st;

typedef enum {
    PPREC_STRING,
    PPREC_CHAR,
    PPREC_STROKE
} Ptext_prec;

typedef enum {
    PPATH_RIGHT,
    PPATH_LEFT,
    PPATH_UP,
    PPATH_DOWN
} Ptext_path;

typedef enum {
    PHOR_NORM,
    PHOR_LEFT,
    PHOR_CTR,
    PHOR_RIGHT
} Phor_text_align;

typedef enum {
    PVERT_NORM,
    PVERT_TOP,
    PVERT_CAP,
    PVERT_HALF,
    PVERT_BASE,
    PVERT_BOTTOM
} Pvert_text_align;

typedef enum {
    PSTYLE_HOLLOW,
    PSTYLE_SOLID,
    PSTYLE_PAT,
    PSTYLE_HATCH,
    PSTYLE_EMPTY
} Pint_style;

typedef enum {
    PEDGE_OFF,
    PEDGE_ON
} Pedge_flag;

typedef enum {
    PASPECT_LINETYPE,
    PASPECT_LINEWIDTH,
    PASPECT_LINE_COLR_IND,
    PASPECT_MARKER_TYPE,
    PASPECT_MARKER_SIZE,
    PASPECT_MARKER_COLR_IND,
    PASPECT_TEXT_FONT,
    PASPECT_TEXT_PREC,
    PASPECT_CHAR_EXPAN,
    PASPECT_CHAR_SPACE,
    PASPECT_TEXT_COLR_IND,
    PASPECT_INT_STYLE,
    PASPECT_INT_STYLE_IND,
    PASPECT_INT_COLR_IND,
    PASPECT_EDGE_FLAG,
    PASPECT_EDGETYPE,
    PASPECT_EDGEWIDTH,
    PASPECT_EDGE_COLR_IND,
    PASPECT_LINE_SHAD_METH,
    PASPECT_INT_SHAD_METH,
    PASPECT_REFL_PROPS,
    PASPECT_INT_REFL_EQN,
    PASPECT_BACK_INT_STYLE,
    PASPECT_BACK_INT_STYLE_IND,
    PASPECT_BACK_INT_COLR,
    PASPECT_BACK_INT_SHAD_METH,
    PASPECT_BACK_REFL_PROPS,
    PASPECT_BACK_INT_REFL_EQN,
    PASPECT_CURVE_APPROX_CRIT,
    PASPECT_SURF_APPROX_CRIT
} Paspect;

typedef enum {
    PASF_BUNDLED,
    PASF_INDIV
} Pasf;

typedef enum {
    PAVAIL_MONOCHR,
    PAVAIL_COLR
} Pcolr_avail;

typedef enum {
    PTYPE_PRECONCAT,
    PTYPE_POSTCONCAT,
    PTYPE_REPLACE
} Pcompose_type;

typedef enum {
    PTYPE_PARAL,
    PTYPE_PERSPECT
} Pproj_type;

typedef enum {
    PIND_NO_CLIP,
    PIND_CLIP
} Pclip_ind;

typedef enum {
    PPRI_HIGHER,
    PPRI_LOWER
} Prel_pri;

typedef enum {
    PRES_MAINTAIN,
    PRES_ABANDON,
    PRES_UPD
} Pconf_res;

typedef enum {
    PFLAG_LINE,
    PFLAG_FILL,
    PFLAG_FILL_SET
} Pline_fill_ctrl_flag;

typedef enum {
    PORDER_TOP_FIRST,
    PORDER_BOTTOM_FIRST
} Ppath_order;

typedef enum {
    POP_REQ,
    POP_SAMPLE,
    POP_EVENT
} Pop_mode;

typedef enum {
    PSWITCH_NO_ECHO,
    PSWITCH_ECHO
} Pecho_switch;

typedef enum {
    PIN_STATUS_NONE,
    PIN_STATUS_OK,
    PIN_STATUS_NO_IN
} Pin_status;

typedef enum {
    PSTRUCT_NONE,
    PSTRUCT_OPEN
} Popen_struct_status;

typedef enum {
    PIN_NONE,
    PIN_LOC,
    PIN_STROKE,
    PIN_VAL,
    PIN_CHOICE,
    PIN_PICK,
    PIN_STRING
} Pin_class;

typedef enum {
    PPR_OFF,
    PPR_ON
} Ppr_switch;

typedef enum {
    PINQ_SET,
    PINQ_REALIZED
} Pinq_type;

typedef enum {
    PUPD_NOT_PEND,
    PUPD_PEND
} Pupd_st;

typedef enum {
    PDC_METRES,
    PDC_OTHER
} Pdc_units;

typedef enum {
    PDYN_IRG,
    PDYN_IMM,
    PDYN_CBS
} Pdyn_mod;

typedef enum {
    PATTR_LINE,
    PATTR_MARKER,
    PATTR_TEXT,
    PATTR_INT,
    PATTR_EDGE
} Pattrs;

typedef enum {
    PELEM_ALL,
    PELEM_NIL,
    PELEM_POLYLINE3,
    PELEM_POLYLINE,
    PELEM_POLYMARKER3,
    PELEM_POLYMARKER,
    PELEM_TEXT3,
    PELEM_TEXT,
    PELEM_ANNO_TEXT_REL3,
    PELEM_ANNO_TEXT_REL,
    PELEM_FILL_AREA3,
    PELEM_FILL_AREA,
    PELEM_FILL_AREA_SET3,
    PELEM_FILL_AREA_SET,
    PELEM_CELL_ARRAY3,
    PELEM_CELL_ARRAY,
    PELEM_GDP3,
    PELEM_GDP,
    PELEM_LINE_IND,
    PELEM_MARKER_IND,
    PELEM_TEXT_IND,
    PELEM_INT_IND,
    PELEM_EDGE_IND,
    PELEM_LINETYPE,
    PELEM_LINEWIDTH,
    PELEM_LINE_COLR_IND,
    PELEM_MARKER_TYPE,
    PELEM_MARKER_SIZE,
    PELEM_MARKER_COLR_IND,
    PELEM_TEXT_FONT,
    PELEM_TEXT_PREC,
    PELEM_CHAR_EXPAN,
    PELEM_CHAR_SPACE,
    PELEM_TEXT_COLR_IND,
    PELEM_CHAR_HT,
    PELEM_CHAR_UP_VEC,
    PELEM_TEXT_PATH,
    PELEM_TEXT_ALIGN,
    PELEM_ANNO_CHAR_HT,
    PELEM_ANNO_CHAR_UP_VEC,
    PELEM_ANNO_PATH,
    PELEM_ANNO_ALIGN,
    PELEM_ANNO_STYLE,
    PELEM_INT_STYLE,
    PELEM_INT_STYLE_IND,
    PELEM_INT_COLR_IND,
    PELEM_EDGE_FLAG,
    PELEM_EDGETYPE,
    PELEM_EDGEWIDTH,
    PELEM_EDGE_COLR_IND,
    PELEM_PAT_SIZE,
    PELEM_PAT_REF_POINT_VECS,
    PELEM_PAT_REF_POINT,
    PELEM_ADD_NAMES_SET,
    PELEM_REMOVE_NAMES_SET,
    PELEM_INDIV_ASF,
    PELEM_HLHSR_ID,
    PELEM_LOCAL_MODEL_TRAN3,
    PELEM_LOCAL_MODEL_TRAN,
    PELEM_GLOBAL_MODEL_TRAN3,
    PELEM_GLOBAL_MODEL_TRAN,
    PELEM_MODEL_CLIP_VOL3,
    PELEM_MODEL_CLIP_VOL,
    PELEM_MODEL_CLIP_IND,
    PELEM_RESTORE_MODEL_CLIP_VOL,
    PELEM_VIEW_IND,
    PELEM_EXEC_STRUCT,
    PELEM_LABEL,
    PELEM_APPL_DATA,
    PELEM_GSE,
    PELEM_PICK_ID,
    PELEM_POLYLINE_SET3_DATA,
    PELEM_FILL_AREA_SET3_DATA,
    PELEM_TRI_STRIP3_DATA,
    PELEM_QUAD_MESH3_DATA,
    PELEM_SET_OF_FILL_AREA_SET3_DATA,
    PELEM_NUNI_BSP_CURVE,
    PELEM_NUNI_BSP_SURF,
    PELEM_CELL_ARRAY3_PLUS,
    PELEM_TEXT_COLR,
    PELEM_MARKER_COLR,
    PELEM_EDGE_COLR,
    PELEM_LINE_COLR,
    PELEM_CURVE_APPROX_CRIT,
    PELEM_LINE_SHAD_METH,
    PELEM_INT_COLR,
    PELEM_BACK_INT_COLR,
    PELEM_BACK_INT_STYLE,
    PELEM_BACK_INT_STYLE_IND,
    PELEM_REFL_PROPS,
    PELEM_BACK_REFL_PROPS,
    PELEM_INT_SHAD_METH,
    PELEM_BACK_INT_SHAD_METH,
    PELEM_INT_REFL_EQN,
    PELEM_BACK_INT_REFL_EQN,
    PELEM_SURF_APPROX_CRIT,
    PELEM_PARA_SURF_CHARACS,
    PELEM_FACE_DISTING_MODE,
    PELEM_FACE_CULL_MODE,
    PELEM_LIGHT_SRC_STATE,
    PELEM_DCUE_IND,
    PELEM_COLR_MAP_IND,
    PELEM_RENDERING_COLR_MODEL,
    PELEM_NUM_EL_TYPES
} Pelem_type;

typedef enum {
    PEDIT_INSERT,
    PEDIT_REPLACE
} Pedit_mode;

typedef enum {
    PFLAG_DEL,
    PFLAG_KEEP
} Pref_flag;

typedef enum {
    PERR_OFF,
    PERR_ON
} Perr_mode;

typedef enum {
    PDIR_BACKWARD,
    PDIR_FORWARD
} Psearch_dir;

typedef enum {
    PSEARCH_STATUS_FAILURE,
    PSEARCH_STATUS_SUCCESS
} Psearch_status;

typedef enum {
	PNON_RATIONAL = 0,
	PRATIONAL = 1
} Prational;

typedef struct {
    size_t	size;	/* sizeof data */
#if NeedFunctionPrototypes
    void	*data;	/* pointer to data */
#else
    char	*data;	/* pointer to data */
#endif
} Pdata;

typedef struct {
    Pfloat	red;	/* red, hue, etc */
    Pfloat	green;	/* green, saturation, lightness, etc */
    Pfloat	blue;	/* blue, value, saturation, etc */
 } Prgb;

typedef struct {
     Pfloat     cieluv_x;  /* x coefficient */
     Pfloat     cieluv_y;  /* y coefficient */
     Pfloat     cieluv_y_lum;  /* y luminance */
 } Pcieluv;

typedef struct {
     Pfloat     hue;    /* hue */
     Pfloat     satur;  /* saturation */
     Pfloat     value;  /* value */
 } Phsv;

typedef struct {
     Pfloat     hue;        /* hue */
     Pfloat     lightness;  /* lightness; */
     Pfloat     satur;      /* saturation */
 } Phls;

typedef union {
     Prgb	rgb;
     Pcieluv	cieluv;
     Phls	hls;
     Phsv	hsv;
     Pdata	unsupp;
} Pcolr_rep;

typedef struct {
    Pint	size_x;
    Pint	size_y;
} Pint_size;

typedef struct {
    Pint	size_x;
    Pint	size_y;
    Pint	size_z;
} Pint_size3;

typedef struct {
    Pfloat	size_x;
    Pfloat	size_y;
} Pfloat_size;

typedef struct {
    Pfloat	size_x;
    Pfloat	size_y;
    Pfloat	size_z;
} Pfloat_size3;

typedef struct {
    Pint        num_ints; /* number of Pints in list */
    Pint        *ints;    /* list of integers        */
} Pint_list;

typedef struct {
    Pint	num_lists;	/* number of Pintlists in list */
    Pint_list	*lists;	        /* list of integer lists */
} Pint_list_list;

typedef struct {
    Pint	num_floats;	/* number of Pfloats in list */
    Pfloat	*floats;	/* list of floats */
} Pfloat_list;

typedef struct {
    Pint	num_lists;	/* number of lists in list */
    Pfloat_list	*lists;		/* list of float lists */
} Pfloat_list_list;

typedef struct {
    Pint	id;	/* GSE identifier */
    Pws_dep_ind	ind;	/* WS independent/dependent ind */
} Pgse_id_dep;

typedef struct {
    Pint	num_id_facs;	/* number of identifiers/dependency element */
    Pgse_id_dep	*id_facs;	/* list */
} Pgse_id_dep_list;

typedef struct {
    Pint        id;	/* archive file identifier */
    char       *name;	/* archive file name       */
} Par_file;

typedef struct {
    Pint	num_ar_files;	/* number of archive files */
    Par_file	*ar_files;	/* list of archive files */
} Par_file_list;

typedef struct {
    Pint	struct_id;	/* structure identifier */
    Pint	elem_pos;	/* element number */
} Pelem_ref;

typedef struct {
    Pint	num_elem_refs;	/* number of execute refs */
    Pelem_ref	*elem_refs;	/* list of execute refs */
} Pelem_ref_list;

typedef struct {
    Pint	        num_elem_ref_lists; /* number of execute refs */
    Pelem_ref_list	*elem_ref_lists;	/* list of execute ref lists */
} Pelem_ref_list_list;

typedef struct {
    Pint_list	incl_set;	/* inclusion set */
    Pint_list	excl_set;	/* exclusion set */
} Pfilter;

typedef struct {
    Pint	num_filters;	/* number of filters */
    Pfilter	*filters;	/* list of filters */
} Pfilter_list;

typedef struct {
    Pint	id;	/* structure id */
    Pfloat	disp_pri;	/* structure disp_pri */
} Pposted_struct;

typedef struct {
    Pint	    num_postings;	/* number of structure postings */
    Pposted_struct  *postings;  	/* list of postings */
} Pposted_struct_list;

typedef struct {
    Pint	num_strings;	/* number of strings */
    char	**strings;	/* list of strings */
} Pstring_list;

typedef struct {
    Pfloat	x;	/* x coordinate */
    Pfloat	y;	/* y coordinate */
} Ppoint;

typedef struct {
    Pfloat	x;	/* x coordinate */
    Pfloat	y;	/* y coordinate */
    Pfloat	z;	/* z coordinate */
} Ppoint3;

typedef struct {
    Pfloat	x;	/* x coordinate */
    Pfloat	y;	/* y coordinate */
    Pfloat	z;	/* z coordinate */
    Pfloat	w;	/* w coordinate */
} Ppoint4;

typedef struct {
    Pfloat	delta_x;	/* x magnitude */
    Pfloat	delta_y;	/* y magnitude */
} Pvec;

typedef struct {
    Pfloat	delta_x;	/* x magnitude */
    Pfloat	delta_y;	/* y magnitude */
    Pfloat	delta_z;	/* z magnitude */
} Pvec3;

typedef struct {
    Ppoint	point;	/* point */
    Pvec	norm;	/* normal */
} Phalf_space;

typedef struct {
    Ppoint3	point;	/* point */
    Pvec3	norm;	/* normal */
} Phalf_space3;

typedef struct {
    Ppoint	p;	/* lower left */
    Ppoint	q;	/* upper right */
} Prect;

typedef struct {
    Ppoint3	p;	/* point p */
    Ppoint3	q;	/* point q */
    Ppoint3	r;	/* point r */
} Pparal;

typedef struct {
    Pfloat	x_min;	/* x min */
    Pfloat	x_max;	/* x max */
    Pfloat	y_min;	/* y min */
    Pfloat	y_max;	/* y max */
    Pfloat	z_min;	/* z min */
    Pfloat	z_max;	/* z max */
} Plimit3;

typedef struct {
    Pfloat	x_min;	/* x min */
    Pfloat	x_max;	/* x max */
    Pfloat	y_min;	/* y min */
    Pfloat	y_max;	/* y max */
} Plimit;

typedef struct {
    Pint	u_dim;	/* dimension (number of divisions) along U */
    Pint	v_dim;	/* dimension (number of divisions) along V */
} Ppcs_dims;

typedef struct {
    Pint	num_points;	/* number of Ppoints in the list */
    Ppoint	*points;	/* list of points */
} Ppoint_list;

typedef struct {
    Pint	num_points;	/* number of Ppoint3s in the list */
    Ppoint3	*points;	/* list of points */
} Ppoint_list3;

typedef struct {
    Pint	num_points;	/* number of Ppoint4s in the list */
    Ppoint4	*points;	/* list of points */
} Ppoint_list4;

typedef struct {		/* list of 2D or 3D  points */
    Pint	num_points;	/* number of points */
    union {
	Ppoint	*point2d;	/* array of 2D points */
	Ppoint3	*point3d;	/* array of 3D points */
    } points;
} Ppoint_list23;

typedef struct {			/* list of 3D or 4D  points */
    Pint		num_points;
    union {
	Ppoint3	*point3d;		/* array of 3D points */
	Ppoint4	*point4d;		/* array of 4D points */
    } 			points;
} Ppoint_list34;

typedef struct {		/* grid of 3D or 4D points, [u_dim][v_dim] */
    Ppcs_dims	num_points;	/* number of points in each dimension */
    union {
	Ppoint3	*point3d;	/* array of 3D points */
	Ppoint4	*point4d;	/* array of 4D points */
    } points;
} Ppoint_grid34;

typedef struct {                        /* list of 2d point lists */
    Pint        num_point_lists;        /* number of point lists  */
    Ppoint_list *point_lists;           /* list of point lists    */
} Ppoint_list_list;

typedef struct {                        /* list of 3d point lists */
    Pint         num_point_lists;       /* number of point lists  */
    Ppoint_list3 *point_lists;          /* list of point lists    */
} Ppoint_list_list3;

typedef struct {
    Pint        	num_half_spaces;	/* number of half-spaces */
    Phalf_space3	*half_spaces;   	/* list of half-spaces */
} Phalf_space_list3;

typedef struct {
    Pint	num_half_spaces;/* number of half spaces */
    Phalf_space	*half_spaces;	/* list of half-spaces */
} Phalf_space_list;

typedef struct {
    Pedge_flag		visible;	/* curve visibility flag */
    Prational		rationality;	/* rationality */
    Pint		order;		/* curve order */
    Pint		approx_type;	/* approximation type */
    Pfloat		approx_val;	/* approximation value */
    Pfloat_list		knots;		/* curve knot vector */
    Pfloat		tmin, tmax;	/* curve parameter range */
    Ppoint_list23	cpts;		/* control points */
} Ptrimcurve;

typedef struct {
    Pint	num_curves;	/* number of trim curve in list */
    Ptrimcurve	*curves;	/* list of curves */
} Ptrimcurve_list;

typedef struct {
    Phor_text_align	hor;	/* horizontal component */
    Pvert_text_align	vert;	/* vertical component */
} Ptext_align;

typedef union {
    Pint        	ind;	/* index in workstation colour bundle table */
    Pcolr_rep	direct;	/* direct colour components */
} Pcoval;

typedef struct {
    Pint       	num_colr_reps;	/* number of colours */
    Pcolr_rep	*colr_reps;	/* array of colours */
} Pcolr_rep_list;

typedef struct {
    Pcoval	colr;		/* colour */
    Pvec3	norm;		/* normal */
} Pconorm3;			/* colour and normal */

typedef struct {
    Ppoint3	point;		/* point coordinates */
    Pcoval	colr;		/* colour */
} Pptco3;			/* point with colour */

typedef struct {
    Ppoint3     point;  	/* point coordinates */
    Pvec3       norm;		/* normal */
} Pptnorm3;

typedef struct {
    Ppoint3     point;  	/* point coordinates */
    Pcoval      colr;		/* colour */
    Pvec3       norm;		/* normal */
} Pptconorm3;			/* point with colour and normal */

typedef union {
    Ppoint3	*points;	/* point */
    Pptco3	*ptcolrs;	/* point and color */
    Pptnorm3	*ptnorms;	/* point and normal */
    Pptconorm3	*ptconorms;	/* point, color and normal */
    /* implementation dependent data pointer */
} Pfacet_vdata_arr3;		/* facet vertex data array */

typedef struct {
    Pint        	num_vertices;	/* number of vertices */
    Pfacet_vdata_arr3	vertex_data;	/* array of facet vertex data */
} Pfacet_vdata_list3;			/* facet vertex data list */

typedef union {
    Ppoint3      *points;		/* points */
    Pptco3       *ptcolrs;		/* points & colours */
    /* implementation dependent data */
} Pline_vdata_arr3;			/* line vertex data array */	

typedef struct {
    Pint	num_vertices;			/* number of vertices */
    Pline_vdata_arr3	vertex_data;	/* array of line vertex data */
} Pline_vdata_list3;               	/* polyline vertex data */

typedef union {
    Pedge_flag      *edges;     		/* edge flags */
    /* implementation dependent data */
} Pedge_data_arr;

typedef struct {
    Pint        	num_edges;     	/* number of edges */
    Pedge_data_arr	edgedata;	/* array of edge data */
} Pedge_data_list;             		/* edge data list */

typedef struct {
    Pint        	num_lists;     	/* number of edges */
    Pedge_data_list	*edgelist;	/* list of edge data list */
} Pedge_data_list_list;	/* edge data list list */

typedef union {
    Pcoval      colr;			/* colour */
    Pvec3       norm;			/* normal */
    Pconorm3    conorm;			/* colour and normal */
    /* implementation dependent data pointer */
} Pfacet_data3;	/* array of facet data */

typedef union {
    Pcoval      *colrs;			/* colour */
    Pvec3       *norms;			/* normal */
    Pconorm3    *conorms;		/* colour and normal */
    /* implementation dependent data pointer */
} Pfacet_data_arr3;	/* array of facet data */

typedef struct {
    Pint	type;		/* indirect, RGB, CIE, HSV, HLS */
    union {
	Pint	ind;		/* index in workstation colour bundle table */
	struct {
	    Pfloat	x;	/* red, hue, etc */
	    Pfloat	y;	/* green, saturation, lightness, etc */
	    Pfloat	z;	/* blue, value, saturation, etc */
	} general;
    } val;
} Pgcolr;

typedef struct {
    Pint	type;		/* line type */
    Pfloat	width;		/* linewidth scale factor */
    Pint	colr_ind;	/* colour index */
} Pline_bundle;

typedef struct {
    Pint	type;		/* line type */
    Pfloat	width;		/* linewidth scale factor */
    Pgcolr	colr;		/* polyline colour */
    Pint	shad_meth;	/* polyline shading method */
    Pint	approx_type;	/* curve approximation criteria */
    Pfloat	approx_val;	/* curve approximation criteria */
} Pline_bundle_plus;

typedef struct {
    Pint	type;		/* marker type */
    Pfloat	size;		/* marker size scale factor */
    Pint	colr_ind;	/* colour index */
} Pmarker_bundle;

typedef struct {
    Pint	type;	/* marker type */
    Pfloat	size;	/* marker size scale factor */
    Pgcolr	colr;	/* marker colour */
} Pmarker_bundle_plus;

typedef struct {
    Pint	font;		/* text font */
    Ptext_prec	prec;		/* text precision */
    Pfloat	char_expan;	/* character char_expansion factor */
    Pfloat	char_space;	/* character spacing */
    Pint	colr_ind;	/* text colour index */
} Ptext_bundle;

typedef struct {
    Pint	font;		/* text font */
    Ptext_prec	prec;   	/* text precision */
    Pfloat	char_expan;	/* character char_expansion factor */
    Pfloat	char_space;	/* character spacing */
    Pgcolr	colr;		/* text colour */
} Ptext_bundle_plus;

typedef struct {
    Pint_style	style;	/* interior style */
    Pint	style_ind;	/* interior style index */
    Pint	colr_ind;	/* interior colour index */
} Pint_bundle;

typedef struct {
    Pfloat	ambient_coef;	/* ambient reflectance coefficient */
    Pfloat	diffuse_coef;	/* diffuse reflectance coefficient */
    Pfloat	specular_coef;	/* specular reflectance coefficient */
    Pgcolr	specular_colr;/* specular colour */
    Pfloat	specular_exp;	/* specular exponent */
} Prefl_props;

typedef struct {
    Pint_style	style;			/* interior style */
    Pint	style_ind;		/* interior style index */
    Pgcolr	colr;			/* interior colour */
    Pint	refl_eqn;		/* reflectance equation */
    Pint	shad_meth;		/* shading method */
    Prefl_props	refl_props;		/* reflectance properties */
    Pint_style	back_style;		/* interior style */
    Pint	back_style_ind;		/* interior style index */
    Pgcolr	back_colr;		/* interior colour */
    Pint	back_refl_eqn;		/* back reflectance equation */
    Pint	back_shad_meth;		/* back shading method */
    Prefl_props	back_refl_props;	/* back reflectance properties */
    Pint	approx_type;		/* approximation meth */
    Pfloat	approx_val[2];	/* approximation values, u and v */
} Pint_bundle_plus;

typedef struct {
    Pedge_flag	flag;		/* edge flag */
    Pint	type;		/* edgetype */
    Pfloat	width;		/* edgewidth scale factor */
    Pint	colr_ind;	/* edge colour index */
} Pedge_bundle;

typedef struct {
    Pedge_flag	flag;	/* edge flag */
    Pint	type;	/* edgetype */
    Pfloat	width;	/* edgewidth scale factor */
    Pgcolr	colr;	/* edge colour */
} Pedge_bundle_plus;

typedef struct {
    Pint_size	dims;	/* pattern's dimensions */
    Pint	*colr_array;	/* colour index array */
} Ppat_rep;

typedef struct {
    Pint_size	dims;	/* pattern's dimensions */
    Pint	type;	/* colour type */
    Pcoval	*colr_array;	/* array of colours */
} Ppat_rep_plus;

typedef struct {
    Pmatrix3	ori_matrix;	/* orientation matrix */
    Pmatrix3	map_matrix;	/* mapping matrix */
    Plimit3	clip_limit;	/* clipping limits */
    Pclip_ind	xy_clip;	/* X-Y clipping indicator */
    Pclip_ind	back_clip;	/* back clipping indicator */
    Pclip_ind	front_clip;	/* front clipping indicator */
} Pview_rep3;

typedef struct {
    Pmatrix	ori_matrix;	/* orientation matrix */
    Pmatrix	map_matrix;	/* mapping matrix */
    Plimit	clip_limit;	/* clipping limits */
    Pclip_ind	xy_clip;	/* X-Y clipping ind */
} Pview_rep;

typedef struct {
    Plimit	win;		/* window limits */
    Plimit3	proj_vp;	/* viewport limits */
    Pproj_type	proj_type;	/* projection type */
    Ppoint3	proj_ref_point;	/* projection reference point */
    Pfloat	view_plane;	/* view plane distance */
    Pfloat	back_plane;	/* back plane distance */
    Pfloat	front_plane;	/* front plane distance */
} Pview_map3;

typedef struct {
    Plimit	win;		/* window limits */
    Plimit	proj_vp;	/* viewport limits */
} Pview_map;

typedef struct {
    Pasf        	type_asf;	/* line type asf */
    Pasf        	width_asf;	/* line width asf */
    Pasf        	colr_ind_asf;	/* line colour index asf */
    Pint        	ind;	/* line index */
    Pline_bundle	bundle;	/* line bundle */
} Pline_attrs;

typedef struct {
    Pasf	style_asf;	/* interior asf */
    Pasf	style_ind_asf;	/* interior style asf */
    Pasf	colr_ind_asf;	/* interior colour index asf */
    Pint	ind;	/* interior index */
    Pint_bundle	bundle;	/* interior bundle */
} Pint_attrs;

typedef struct {
    Pasf	        flag_asf;	/* edge flag asf */
    Pasf	        type_asf;	/* edge type asf */
    Pasf	        width_asf;	/* edge width asf */
    Pasf	        colr_ind_asf;	/* edge colour index asf */
    Pint	        ind;		/* edge index */
    Pedge_bundle	bundle;		/* edge bundle */
} Pedge_attrs;

typedef struct {
    Pasf	        type_asf;	/* marker type asf */
    Pasf	        size_asf;	/* marker style asf */
    Pasf	        colr_ind_asf;	/* marker colour index asf */
    Pint	        ind;		/* marker index */
    Pmarker_bundle	bundle;		/* marker bundle */
} Pmarker_attrs;

typedef struct {
    Pgcolr	colr;	/* light source colour */
} Pamb_light_src_rec;

typedef struct {
    Pgcolr	colr;	/* light source colour */
    Pvec3	dir;	/* light source direction */
} Pdir_light_src_rec;

typedef struct {
    Pgcolr	colr;		/* light source colour */
    Ppoint3	pos;		/* light source position */
    Pfloat	coef[2];	/* attenuation coefficients */
} Ppos_light_src_rec;

typedef struct {
    Pgcolr	colr;		/* light source colour */
    Ppoint3	pos;		/* light source position */
    Pvec3	dir;		/* light source direction */
    Pfloat	exp;		/* concentration exponent */
    Pfloat	coef[2];	/* attenuation coefficients */
    Pfloat	angle;		/* spread angle */
} Pspot_light_src_rec;

typedef union {
    Pamb_light_src_rec	ambient;
    Pdir_light_src_rec	directional;
    Ppos_light_src_rec	positional;
    Pspot_light_src_rec	spot;
} Plight_src_rec;

typedef struct {
    Pint		type;	/* light source type */
    Plight_src_rec	rec;	/* light source data record */
} Plight_src_bundle;

typedef struct {
    Pint_list	types;		/* types of light source supported */  
    Pint	max;		/* max. no. of simultaneously active lights */
    Pint	num_pred_inds;	/* number of predefined bundles */
} Plight_src_facs;

typedef struct {
    Pint	struct_id;	/* structure identifier */
    Pint	pick_id;	/* hierarchical pick identifier */
    Pint	elem_pos;	/* element sequence number */
} Ppick_path_elem;

typedef struct {
    Pint		depth;		/* pick path_list depth */
    Ppick_path_elem	*path_list;	/* pick path */
} Ppick_path;

typedef struct {
    Pdc_units	dc_units;	/* device coordinate units */
    Pfloat_size	size_dc;	/* device size in coordinate units */
    Pint_size	size_raster;	/* device size in raster units */
} Pdisp_space_size;

typedef struct {
    Pdc_units		dc_units;	/* device coordinate units */
    Pfloat_size3	size_dc;	/* device volume in coordinate units */
    Pint_size3		size_raster;	/* device volume in raster units */
} Pdisp_space_size3;

typedef struct {
    Pdyn_mod	line_bundle;	/* polyline representation */
    Pdyn_mod	marker_bundle;	/* polymarker representation */
    Pdyn_mod	text_bundle;	/* text representation */
    Pdyn_mod	int_bundle;	/* interior representation */
    Pdyn_mod	edge_bundle;	/* edge representation */
    Pdyn_mod	pat_rep;	/* pattern representation */
    Pdyn_mod	colr_rep;	/* colour representation */
    Pdyn_mod	view_rep;	/* view representation */
    Pdyn_mod	ws_tran;	/* workstation transform */
    Pdyn_mod	highl_filter;	/* highlight filter */
    Pdyn_mod	invis_filter;	/* invisibility filter */
    Pdyn_mod	hlhsr_mode;	/* HLHSR mode */
} Pdyns_ws_attrs;

typedef struct {
    Pdyn_mod	light_src_rep;	/* light source representation */
    Pdyn_mod	dcue_rep;	/* depth cue representation */
    Pdyn_mod	colr_map_rep;	/* colour mapping representation */
} Pdyns_ws_attrs_plus;
    
typedef struct {
    Pint_list	types;		/* list of line types */
    Pint	num_widths;	/* number of available line widths */
    Pfloat	nom_width;	/* nominal line width */
    Pfloat	min_width;	/* min line width */
    Pfloat	max_width;	/* max line width */
    Pint	num_pred_inds;	/* number of predefined bundles */
} Pline_facs;

typedef struct {
    Pint_list	types;		/* list of line types */
    Pint	num_widths;		/* number of available line widths */
    Pfloat	nom_width;	/* nominal line width */
    Pfloat	min_width;	/* min line width */
    Pfloat	max_width;	/* max line width */
    Pint	num_pred_inds;	/* number of predefined bundles */
    Pint_list	shads;		/* list of shad_meth meths */
} Pline_facs_plus;

typedef struct {
    Pint_list	types;	/* list of marker types */
    Pint	num_sizes;	/* number of available marker sizes */
    Pfloat	nom_size;	/* nominal marker size */
    Pfloat	min_size;	/* min marker size */
    Pfloat	max_size;	/* max marker size */
    Pint	num_pred_inds;	/* number of predefined bundles */
} Pmarker_facs;

typedef struct {
    Pint	font;	/* text font */
    Ptext_prec	prec;	/* text precision */
} Ptext_font_prec;

typedef struct {
    Pint	num_font_precs;	/* number of fonts and precisions */
    Ptext_font_prec	*font_precs;	/* list of fonts and precisions */
    Pint	num_char_hts;	/* number of character heights */
    Pfloat	min_char_ht;	/* minimum height */
    Pfloat	max_char_ht;	/* maximum height */
    Pint	num_char_expans;	/* number of character expansion factors */
    Pfloat	min_char_expan;	/* minimum expansion factor */
    Pfloat	max_char_expan;	/* maximum expansion factor */
    Pint	num_pred_inds;	/* number of predefined bundles */
} Ptext_facs;

typedef struct {
    Pint	num_int_styles;	/* number of interior styles */
    Pint_style	int_styles[5];	/* list of available interior styles */
    Pint_list	hatch_styles;	/* list of available hatch styles */
    Pint	num_pred_inds;	/* number of predefined bundles */
} Pint_facs;

typedef struct {
    Pint	num_int_styles;	/* number of interior styles */
    Pint_style	*int_styles;	/* list of available interior styles */
    Pint_list	hatch_styles;	/* list of available hatch styles */
    Pint	num_pred_inds;	/* number of predefined interior indices */
    Pint_list	refl_eqns;		/* list of available reflectance equations */
    Pint_list	shad_meths;	/* list of available shading methods */
} Pint_facs_plus;

typedef struct {
    Pint	max_bsp_order;	/* maximum B-spline order */
    Pint	max_tc_order;	/* maximum trim curve order */
    Pint_list	cat_types;	/* list of curve approx types */
    Pint_list	sat_types;	/* list of surface approx types */
    Pint_list	tcat_types;	/* list of trim curve approx types */
    Pint_list	psc_types;	/* list of parametric surface
				    characteristics */
} Pcurvsurf_facs;

typedef enum {
    PCP_UNIFORM,
    PCP_NON_UNIFORM
} Pcurve_placement;

typedef union {
    struct {
	Pint	unused;
    } psc_1;
    struct {
	Pint	unused;
    } psc_2;
    struct {
	Pcurve_placement	placement;
	Pint			u_count;
	Pint			v_count;
    } psc_3;
    struct {
	Ppoint3		origin;
	Pvec3		direction;
	Pfloat_list	params;
    } psc_4;
    struct {
	Ppoint3		origin;
	Pvec3		direction;
	Pfloat_list	params;
    } psc_5;
} Ppara_surf_characs;

typedef struct {
    Pint_list	types;	/* list of edge types */
    Pint	num_widths;	/* number of available edge widths */
    Pfloat	nom_width;	/* nominal edge width */
    Pfloat	min_width;	/* min edge width */
    Pfloat	max_width;	/* max edge width */
    Pint	num_pred_inds;	/* number of predefined bundles */
} Pedge_facs;

typedef struct {
    Pint	num_colrs;	/* number of colours */
    Pcolr_avail	colr_avail;	/* colour availability */
    Pint	num_pred_inds;	/* number of predefined bundles */
    Pcieluv	prim_colrs[3];	/* primary colours */
} Pcolr_facs;

typedef enum {
    PSUPPRESSED,
    PALLOWED
} Pdcue_mode;

typedef struct {
    Pdcue_mode       mode;   /* depth cue mode */
    Pfloat  ref_planes[2];   /* depth cue ref planes */
    Pfloat  scaling[2];     /* depth cue scaling*/
    Pgcolr  colr;         /* depth cue colour */
} Pdcue_bundle;

typedef union {
    /* method 1 has no data associated with it */
    struct {
	Pint		colr_model;
	Pfloat_list	weights;
	Pcolr_rep_list	colrs;
    } meth_r2;
    struct {
	Pint			colr_model;
	Pfloat_list_list	colr_lists;
    } meth_r3;
} Pcolr_map_data;

typedef struct {
    Pint_list	meths;
    Pint	num_pred_inds;
} Pcolr_map_facs;

typedef struct {
	Pint	int_data;		/* for map methods 1 and 2 */
} Pcolr_map_st;

typedef struct {
    Pint        num_attrs;           /* number of attributes in list */
    Pattrs      *attrs;              /* list of attributes           */
} Pattrs_list;

typedef struct {
    Pint	line_bundles;	    /* polyline tables */
    Pint	mark_bundles;	    /* polymarker tables */
    Pint	text_bundles;	    /* text tables */
    Pint	int_bundles;            /* interior tables */
    Pint	edge_bundles;	    /* edge tables */
    Pint	pat_reps;	    /* pattern tables */
    Pint	colr_reps;	    /* colour tables */
    Pint	view_reps;	    /* view tables */
} Pws_st_tables;

typedef struct {
    Pint	line_bundles;	    /* polyline tables */
    Pint	mark_bundles;	    /* polymarker tables */
    Pint	text_bundles;	    /* text tables */
    Pint	int_bundles;            /* interior tables */
    Pint	edge_bundles;	    /* edge tables */
    Pint	pat_reps;	    /* pattern tables */
    Pint	colr_reps;	    /* colour tables */
    Pint	view_reps;	    /* view tables */
    Pint	dcue_rep;	    /* depth cue tables */
    Pint	light_src_rep;   /* light source tables */
    Pint	colr_map_rep;  /* colour mapping tables */
} Pws_tables_plus;

typedef struct {
    Pdyn_mod	content;	/* structure content */
    Pdyn_mod	post;	/* post structure  */
    Pdyn_mod	unpost;	/* unpost structure  */
    Pdyn_mod	del;	/* del structure  */
    Pdyn_mod	ref;	/* structure references  */
} Pdyns_structs;

typedef struct {
    Pint	loc;	/* locators */
    Pint	stroke;	/* strokes */
    Pint	val;	/* valuators */
    Pint	choice;	/* choices */
    Pint	pick;	/* picks */
    Pint	string;	/* strings */
} Pnum_in;

typedef struct {
    Pint	num_elem_types;	/* number of elements */
    Pelem_type	*elem_types;	/* list of elements */
} Pelem_type_list;

typedef union {
    Pdata  unsupp;           /* unsupp Metafile item data */
} Pitem_data;

/* Plocator_data -- locator data record */
typedef struct {
    union {
	struct {
	   Pint		unused;
	} pet_r1;
	struct {
	   Pint		unused;
	} pet_r2;
	struct {
	   Pint 	unused;
	} pet_r3;
	struct {
	   Pline_attrs	line_attrs;	/* polyline attributes */
	} pet_r4;
	struct {
	   Pline_fill_ctrl_flag	line_fill_ctrl_flag;
	   union {
		Pline_attrs	line_attrs;	/* polyline attributes */
		Pint_attrs	int_attrs;	/* interior attributes */
		struct {
			Pint_attrs	int_attrs; /* interior attributes */
			Pedge_attrs	edge_attrs; /* edge attributes */
		} fill_set;
	    } attrs;
	} pet_r5;
    } pets;
} Ploc_data;

typedef Ploc_data	Ploc_data3;

/* Pvaluator_data -- valuator data record */
typedef struct {
	Pfloat  low;            /* low range limit */
	Pfloat  high;           /* high range limit */
	union {
	    struct {
		Pint	unused;
	    } pet_r1;
	    struct {
		char	*label;
		char	*format;
		char	*low_label;
		char	*high_label;
	    } pet_u1;
	} pets;
} Pval_data;

typedef Pval_data	Pval_data3;

/* Pchoice_data -- choice data record */
typedef struct {
    union {
	struct {
	   Pint		unused;
	} pet_r1;
	struct {
	   Pint		num_prompts;	/* number of alternatives	*/
	   Ppr_switch	*prompts;	/* array of prompts		*/
	} pet_r2;
	struct {
	   Pint		num_strings;	/* number of choice strings	*/
	   char		**strings;	/* array of choice strings	*/
	} pet_r3;
	struct {
	   Pint		num_strings;	/* number of alternatives	*/
	   char		**strings;	/* array of strings		*/
	} pet_r4;
	struct {
	   Pint		struct_id;	/* struct identifier		*/
	   Pint		num_pick_ids;	/* number of alternatives	*/
	   Pint		*pick_ids;	/* array of pick identifiers	*/
	} pet_r5;
    } pets;
} Pchoice_data;

typedef Pchoice_data	Pchoice_data3;

/* Ppick_data -- pick data record */
typedef struct {
    union {
	struct {
	    Pint	unused;
	} pet_r1;
    } pets;
} Ppick_data;

typedef Ppick_data	Ppick_data3;

/* Pstroke_data -- stroke data record */
typedef struct {
	Pint	buffer_size;	/* input buffer size */
	Pint	init_pos;	/* initial editing position		*/
	Pfloat	x_interval;	/* x interval				*/
	Pfloat	y_interval;	/* y interval				*/
	Pfloat	time_interval;	/* time interval			*/
	union {
	   struct {
              Pint      unused;
           } pet_r1;
	   struct {
              Pint      unused;
           } pet_r2;
	   struct {
	      Pmarker_attrs	marker_attrs;	/* marker attributes */
	   } pet_r3;
	   struct {
	      Pline_attrs	line_attrs;	/* line attributes */
	   } pet_r4;
	} pets;
} Pstroke_data;

/* Pstroke_data3 -- stroke data record 3 */
typedef struct {
	Pint	buffer_size;	/* input buffer size */
	Pint	init_pos;	/* initial editing position	*/
	Pfloat	x_interval;	/* x interval			*/
	Pfloat	y_interval;	/* y interval			*/
	Pfloat	z_interval;	/* z interval			*/
	Pfloat	time_interval;	/* time interval		*/
	union {
	   struct {
              Pint      unused;
           } pet_r1;
	   struct {
              Pint      unused;
           } pet_r2;
	   struct {
	      Pmarker_attrs	marker_attrs;	/* marker attributes */
	   } pet_r3;
	   struct {
	      Pline_attrs	line_attrs;	/* marker attributes */
	   } pet_r4;
	} pets;
} Pstroke_data3;

/* Pstring_data -- string data record */
typedef struct {
	Pint	buffer_size;		/* input buffer size		*/
	Pint	init_pos;		/* initial editing position	*/
	union {
	    struct {
	       Pint	unused;
	    } pet_r1;
	} pets;
} Pstring_data;

typedef Pstring_data	Pstring_data3;


/* Pgdp_data -- gdp data record */
typedef union {
	struct {
	   Pint  unused;
	} gdp_r1;
	Pdata	unsupp;		/* unsupp GDP data record	*/
					/* implementation dependent     */
} Pgdp_data;

/* Pgdp_data3 -- gdp data record 3 */
typedef union {
	struct {
	   Pint		unused;
	} gdp3_r1;
	Pdata	unsupp;		/* nusupported GDP data record	*/
					/* implementation dependent     */
} Pgdp_data3;

/* Pgse_data -- gse data record */
typedef union {
	struct {
	   Pint 	unused;
	} gse_r1;
	Pdata		unsupp;	/* unsupp GSE data record	*/
                                        /* implementation dependent     */
} Pgse_data;

typedef enum {
	PERRSYNC_OFF = 0,
	PERRSYNC_ON = 1
} Perrsync;

/* Pescape_in_data -- escape in data record */
typedef union {
	struct {
	   Perrsync	sync_on;
	} escape_in_u1;
	struct {
	   Pint		ws_id;
	} escape_in_u2;
	struct {
	   Pint		ignore_DC_errors; /* ignore errors if non-zero */
	} escape_in_u3;
	struct {
	    Pint		ws_id;
	    Ppoint		point;	/* a drawable point -- not DC */
	    Pfloat		ap_size; /* aperture size, DC length */
	    Ppath_order		order;
	    Pint		depth;	/* depth of path to return */
	    Pint		pet;
	    Pecho_switch	echo_switch;
	    Plimit3		echo_volume;
	    Pfilter		filter;	/* detectibility filter */
	} escape_in_u4;	/* drawable point to pick */
	struct {
	    Pint		ws_id;
	    Ppoint_list3	points;	/* x,y are drawable coords, z is DC*/
	} escape_in_u5;	/* drawable points to WC */
	struct {
	    Pint		ws_id;
	    Pint		num_regions;
	    XRectangle		*regions;
	} escape_in_u6;	/* redraw regions */
	struct {
	    Pint		ws_id;
	} escape_in_u7;	/* ws synch */
} Pescape_in_data;

/* Pescape_out_data -- escape out data record */
typedef union {
	struct {
	    Pint	err_ind;
	    Display	*display;	/* may be returned as NULL */
	    XID		drawable_id;
	    XID         input_overlay_id; /* will be 0 if category not OUTIN */
	    char	*display_name;
	} escape_out_u2;
	struct {
	    Pin_status	status;
	    Ppick_path	pick;
	} escape_out_u4;
	struct {
	    Pint		view_index;
	    Ppoint_list3	points;
	} escape_out_u5;
} Pescape_out_data;

typedef union {
    Pint		int_data;		/* integer valued data */
    Pfloat		float_data;		/* float valued data */
    Ppoint_list3	point_list3;		/* list of 3d points */
    Ppoint_list		point_list;		/* list of 2d points */
    Ppoint_list_list3	point_list_list3;	/* list of 3d point lists */
    Ppoint_list_list	point_list_list;	/* list of 2d point lists */
    struct {
        Ppoint3		pos;		/* text pt */
        Pvec3   	dir[2];		/* direction vectors */
        char		*char_string;	/* text string */
    } text3;
    struct {
        Ppoint		pos;		/* text pt */
        char		*char_string;	/* text string */
    } text;
    struct {
        Ppoint3		ref_point;	/* reference pt */
        Pvec3		offset;		/* anno. pt/offset */
        char		*char_string;	/* text string */
    } anno_text_rel3;
    struct {
        Ppoint		ref_point;	/* reference pt */
        Pvec		offset;		/* anno. pt/offset */
        char		*char_string;	/* text string */
    } anno_text_rel;
    struct {
        Pparal		paral;  	/* parallelogram */
        Ppat_rep	colr_array;   	/* colour array */
    } cell_array3;
    struct {
        Prect		rect;		/* rectangle */
        Ppat_rep	colr_array;  	/* colour array */
    } cell_array;
    struct {
        Pint		id;		/* GDP3 id */
        Ppoint_list3	point_list;	/* pts */
        Pgdp_data3	data;		/* data record */
    } gdp3;
    struct {
        Pint		id;		/* GDP id */
        Ppoint_list	point_list;	/* pts */
        Pgdp_data	data;		/* data record */
    } gdp;
    Ptext_prec		text_prec;	/* text precision */
    Pvec		char_up_vec;	/* char up vector */
    Ptext_path		text_path;	/* text path */
    Ptext_align		text_align;	/* text alignment */
    Pint_style		int_style;	/* interior style */
    Pedge_flag		edge_flag;	/* edge flag */
    Ppoint		pat_ref_point;	/* pat ref pt */
    Pfloat_size		pat_size;	/* pattern size */
    struct {
        Ppoint3		ref_point;	/* pattern ref. pt */
        Pvec3   	ref_vec[2];	/* vectors */
    } pat_ref_point_vecs;
    Pint_list		names;  	/* name sets */
    struct {
        Paspect		id;		/* attribute id */
        Pasf		source;		/* asf */
    } asf;
    struct {
        Pcompose_type	compose_type;	/* composition type */
        Pmatrix3	matrix;		/* transformation matrix */
    } local_tran3;
    struct {
        Pcompose_type	compose_type;	/* composition type */
        Pmatrix		matrix;		/* transformation matrix */
    } local_tran;
    Pmatrix3		global_tran3;	/* global transform3 */
    Pmatrix		global_tran;	/* global transform */
    struct {
        Pint		  op;		/* operator */
        Phalf_space_list3 half_spaces;	/* half-space list */
    } model_clip3;
    struct {
        Pint		 op;		/* operator */
        Phalf_space_list half_spaces;	/* half-space list */
    } model_clip;
    Pclip_ind		clip_ind;	/* clipping indicator */
    Pdata		appl_data;	/* application data */
    struct {
        Pint		id;		/* GSE id */
        Pgse_data	data;		/* GSE data record */
    } gse;

    /* PHIGS PLUS structure elements */
    struct {
	Pint		order;
	Pfloat_list	knots;
	Prational	rationality;
	Ppoint_list34	cpts;
	Pfloat		min;
	Pfloat		max;
    } nurb_curve;
    struct {
	Pint		type;
	Pfloat		value;
    } curv_approx;
    struct {
	Pint		u_order;
	Pint		v_order;
	Prational	rationality;
	Pfloat_list	uknots;
	Pfloat_list	vknots;
	Ppoint_grid34	grid;
	Pint		num_trim_loops;
	Ptrimcurve_list	*trim_loops;
    } nurb_surf;
    struct {
	Pint		type;
	Pfloat		u_val;
	Pfloat		v_val;
    } surf_approx;
    struct {
        Pint            vflag;
        Pint            colr_model;
        Pint            npl;
        Pline_vdata_list3  *vdata;
    } plsd3;
    struct {
        Pint            fflag;
        Pint            eflag;
        Pint            vflag;
        Pint            colr_model;
        Pfacet_data3    fdata;
        Pint            nfa;
        Pedge_data_list	*edata;
        Pfacet_vdata_list3 *vdata;
    } fasd3;
    struct {
        Pint            fflag;
        Pint            vflag;
        Pint            colr_model;
        Pint            nv;		/* number of vertices */
        Pfacet_data_arr3  fdata;
        Pfacet_vdata_arr3 vdata;
    } tsd3;
    struct {
        Pint            fflag;
        Pint            vflag;
        Pint            colr_model;
        Pint_size	dim;
        Pfacet_data_arr3  fdata;
        Pfacet_vdata_arr3 vdata;
    } qmd3;
    struct {
        Pint            fflag;
        Pint            eflag;
        Pint            vflag;
        Pint            colr_model;
        Pint            num_sets;
        Pfacet_data_arr3  fdata;
        Pedge_data_list_list *edata;
        Pint_list_list      *vlist;
        Pfacet_vdata_list3 vdata;
    } sofas3;
    struct {
        Pparal		paral;  	/* parallelogram */
        Ppat_rep_plus	colr_array;   	/* colour array */
    } cell_array_plus;
    Pgcolr		colr;
    Prefl_props		props;
    struct {
        Pint_list         activation;
        Pint_list         deactivation;
    } lss;
    Pcull_mode		cull_mode;       /* culling mode */
    Pdisting_mode	disting_mode;      /* distinguishing mode */
    struct {
	Pint			type;
	Ppara_surf_characs	data;
    } para_surf_characs; /* parametric surface characteristics */
} Pelem_data;

/* Explicitly defined portions of unbounded ranges. */

/* Colour models */
#define PINDIRECT	(0)
#define PMODEL_RGB		(1)
#define PMODEL_CIELUV		(2)
#define PMODEL_HSV		(3)
#define PMODEL_HLS		(4)

/* Rendering colour models */
#define PRCM_WS_DEP	(0)
#define PRCM_RGB	(PMODEL_RGB)
#define PRCM_CIE	(PMODEL_CIELUV)
#define PRCM_HSV	(PMODEL_HSV)
#define PRCM_HLS	(PMODEL_HLS)

/* Vertex data flags */
#define PVERT_COORD			(0)
#define PVERT_COORD_COLOUR		(1)
#define PVERT_COORD_NORMAL		(2)
#define PVERT_COORD_COLOUR_NORMAL	(3)

/* Edge data flags */
#define PEDGE_NONE		(0)
#define PEDGE_VISIBILITY	(1)

/* Facet data flags */
#define PFACET_NONE		(0)
#define PFACET_COLOUR		(1)
#define PFACET_NORMAL		(2)
#define PFACET_COLOUR_NORMAL	(3)

/* Line types */
#define PLINE_SOLID		(1)
#define PLINE_DASH		(2)
#define PLINE_DOT		(3)
#define PLINE_DASH_DOT		(4)

/* Marker types */
#define PMARKER_DOT		(1)
#define PMARKER_PLUS		(2)
#define PMARKER_ASTERISK	(3)
#define PMARKER_CIRCLE		(4)
#define PMARKER_CROSS		(5)

/* Annotation styles */
#define PANNO_STYLE_UNCONNECTED	(1)
#define PANNO_STYLE_LEAD_LINE	(2)

/* Prompt and echo types */
#define PLOC_DEF		(1)
#define PLOC_CROSS_HAIR		(2)
#define PLOC_TRACK_CROSS	(3)
#define PLOC_RUB_BAND		(4)
#define PLOC_RECT		(5)
#define PLOC_DIGIT		(6)

#define PSTROKE_DEF		(1)
#define PSTROKE_DIGIT		(2)
#define PSTROKE_MARKER		(3)
#define PSTROKE_LINE		(4)

#define PVAL_DEF		(1)
#define PVAL_GRAPH		(2)
#define PVAL_DIGIT		(3)

#define PCHOICE_DEF		(1)
#define PCHOICE_PR_ECHO		(2)
#define PCHOICE_STRING_PR	(3)
#define PCHOICE_STRING_IN	(4)
#define PCHOICE_STRUCT		(5)

#define PPICK_DEF		(1)
#define PPICK_GROUP_HIGHL	(2)
#define PPICK_STRUCT_NETWORK	(3)

#define PSTRING_DEF		(1)

/* Modelling clip operator */
#define PMC_REPLACE		(1)
#define PMC_INTERSECT		(2)

/* curve approximation methods */
#define	PCURV_WS_DEP				(1)
#define	PCURV_CONSTANT_PARAMETRIC_BETWEEN_KNOTS	(2)
#define	PCURV_CHORDAL_SIZE_WC			(3)
#define	PCURV_CHORDAL_SIZE_NPC			(4)
#define	PCURV_CHORDAL_SIZE_DC			(5)
#define	PCURV_CHORDAL_DEVIATION_WC		(6)
#define	PCURV_CHORDAL_DEVIATION_NPC		(7)
#define	PCURV_CHORDAL_DEVIATION_DC		(8)

/* surface approximation methods */
#define	PSURF_WS_DEP				(1)
#define	PSURF_CONSTANT_PARAMETRIC_BETWEEN_KNOTS	(2)
#define	PSURF_CHORDAL_SIZE_WC			(3)
#define	PSURF_CHORDAL_SIZE_NPC			(4)
#define	PSURF_CHORDAL_SIZE_DC			(5)
#define	PSURF_PLANAR_DEVIATION_WC		(6)
#define	PSURF_PLANAR_DEVIATION_NPC		(7)
#define	PSURF_PLANAR_DEVIATION_DC		(8)

/* Polyline shading methods */
#define	PSD_NONE	(1)
#define	PSD_COLOUR	(2)

/* Additional shading method constants for interior shading method */
#define	PSD_DOT_PRODUCT	(3)
#define	PSD_NORMAL	(4)

/* Parametric surface characteristics types */
#define PSC_NONE			(1)
#define PSC_WS_DEP			(2)
#define PSC_ISOPARAMETRIC_CURVES	(3)
#define PSC_LEVEL_CURVES_MC		(4)
#define PSC_LEVEL_CURVES_WC		(5)

/* Reflectance Equation Constants */
#define PREFL_NONE		(1)	/* No reflectance calculation */
#define PREFL_AMBIENT		(2)	/* Use ambient term */
#define PREFL_AMB_DIFF		(3)	/* Use ambient and diffuse terms */
#define PREFL_AMB_DIFF_SPEC	(4)	/* Use ambient, diffuse & spec. terms */

/* Light Source Types */
#define PLIGHT_AMBIENT		(1)
#define	PLIGHT_DIRECTIONAL	(2)
#define PLIGHT_POSITIONAL	(3)
#define PLIGHT_SPOT		(4)

/* Colour Mapping Methods */
#define	PCOLR_MAP_TRUE		(1)
#define	PCOLR_MAP_PSEUDO	(2)
#define	PCOLR_MAP_PSEUDO_N	(3)


/* Global to hold specified X server */
extern char			*phg_x_server_name;

/* Preferred argument to OPEN PHIGS */
#define PDEF_MEM_SIZE	((size_t) (-1))
#define PDEF_ERR_FILE	((char *) (0))

#define PHIGS_MAX_NAME_LEN	 (255)

/* Character set numbers */
#define PCS_ASCII		(0)

/* Font numbers */
#define PFONT_MONO		(1)

/* Implementation dependent escape numbers */
#define PUESC_ERRSYNC			(-1)
#define PUESC_DPYINFO			(-2)
#define PUESC_IGNORE_DC_ERRORS		(-3)
#define PUESC_DRAWABLE_POINT_TO_PICK	(-4)
#define PUESC_DRAWABLE_POINTS_TO_WC	(-5)
#define PUESC_REDRAW_REGIONS		(-6)
#define PUESC_WS_SYNCH			(-7)

/* HLHSR constants */
#define	PHIGS_HLHSR_MODE_NONE 		(0)
#define	PHIGS_HLHSR_MODE_ZBUFF		(1)
#define	PHIGS_HLHSR_MODE_PAINTERS	(2)
#define	PHIGS_HLHSR_MODE_SCANLINE	(3)
#define	PHIGS_HLHSR_MODE_LINE_ONLY	(4)

#define	PHIGS_HLHSR_ID_OFF	(0)
#define	PHIGS_HLHSR_ID_ON 	(1)

/* Multi-buffering constants. */
#define PHIGS_BUF_SINGLE	(0)
#define PHIGS_BUF_DOUBLE	(1)

typedef enum {
    PHIGS_X_TOOL,
    PHIGS_X_DRAWABLE
} Phigs_base_name;

typedef enum {
    PHIGS_DC_LIMITS_FIXED,
    PHIGS_DC_LIMITS_ADJUST_TO_WINDOW
} Phigs_DC_model;

#define ATTR_PKG_PHIGS		(PHG_ATTR_PKG_UNUSED_FIRST + 1)
#define PHIGS_FIRST_ATTR	(1)

#define PHIGS_DEFAULT_DC_DEPTH		(1.0)
#define PHIGS_DEFAULT_TOOL_X		(50)
#define PHIGS_DEFAULT_TOOL_Y		(50)
#define PHIGS_DEFAULT_TOOL_WIDTH	(600)
#define PHIGS_DEFAULT_TOOL_HEIGHT	(600)
#define PHIGS_DEFAULT_TOOL_LABEL	("PHIGS Workstation")
#define PHIGS_DEFAULT_TOOL_ICON_LABEL	("")
#define PHIGS_DEFAULT_TOOL_BORDER_WIDTH	(0)

#define PHIGS_ATTR( type, ordinal) \
    PHG_ATTR( ATTR_PKG_PHIGS, type, ordinal)

#define PHIGS_ATTR_ORDINAL( attr ) \
    PHG_ATTR_ORDINAL((attr)) - PHIGS_FIRST_ATTR

typedef enum {
    /* generic attributes start at 1 */
    PHG_BASE_NAME 	= PHIGS_ATTR( PHG_ATTR_ENUM, PHIGS_FIRST_ATTR + 5),
#define    PHIGS_BASE_NAME  (char *)PHG_BASE_NAME

    /* generic X attributes start at 25 */
    /* PHIGS_X_DISPLAY can only be set.  Only the name is remembered. */
    PHG_X_DISPLAY = PHIGS_ATTR( PHG_ATTR_STRING, PHIGS_FIRST_ATTR + 25),
#define PHIGS_X_DISPLAY (char *)PHG_X_DISPLAY
    PHG_X_DISPLAY_NAME= PHIGS_ATTR( PHG_ATTR_STRING, PHIGS_FIRST_ATTR + 26),
#define PHIGS_X_DISPLAY_NAME (char *)PHG_X_DISPLAY_NAME
    PHG_X_DISPLAY_WINDOW= PHIGS_ATTR( PHG_ATTR_INT_PAIR, PHIGS_FIRST_ATTR + 27),
#define PHIGS_X_DISPLAY_WINDOW (char *)PHG_X_DISPLAY_WINDOW
    PHG_X_BUF_MODE	= PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 29),
#define PHIGS_X_BUF_MODE (char *)PHG_X_BUF_MODE
    PHG_X_HANDLE_EXPOSE	= PHIGS_ATTR( PHG_ATTR_BOOLEAN, PHIGS_FIRST_ATTR + 31),
#define PHIGS_X_HANDLE_EXPOSE (char *)PHG_X_HANDLE_EXPOSE
    PHG_X_HANDLE_DESTROY = PHIGS_ATTR( PHG_ATTR_BOOLEAN, PHIGS_FIRST_ATTR + 32),
#define PHIGS_X_HANDLE_DESTROY (char *)PHG_X_HANDLE_DESTROY
    PHG_DC_MODEL 	= PHIGS_ATTR( PHG_ATTR_ENUM, PHIGS_FIRST_ATTR + 33),
#define    PHIGS_DC_MODEL  (char *)PHG_DC_MODEL
    PHG_X_CMAP_PROP_ATOM = PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 34),
#define PHIGS_X_CMAP_PROP_ATOM (char *)PHG_X_CMAP_PROP_ATOM

    /* tool attributes start at 50 */
    PHG_TOOL_WIDTH	= PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 50),
#define    PHIGS_TOOL_WIDTH (char *)PHG_TOOL_WIDTH
    PHG_TOOL_HEIGHT	= PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 51),
#define PHIGS_TOOL_HEIGHT (char *)PHG_TOOL_HEIGHT
    PHG_TOOL_X	= PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 52),
#define PHIGS_TOOL_X (char *)PHG_TOOL_X
    PHG_TOOL_Y	= PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 53),
#define PHIGS_TOOL_Y (char *)PHG_TOOL_Y
    PHG_TOOL_LABEL	= PHIGS_ATTR( PHG_ATTR_STRING, PHIGS_FIRST_ATTR + 54),
#define PHIGS_TOOL_LABEL (char *)PHG_TOOL_LABEL
    PHG_TOOL_ICON_LABEL = PHIGS_ATTR( PHG_ATTR_STRING, PHIGS_FIRST_ATTR + 55),
#define PHIGS_TOOL_ICON_LABEL (char *)PHG_TOOL_ICON_LABEL
    PHG_TOOL_BORDER_WIDTH = PHIGS_ATTR( PHG_ATTR_INT, PHIGS_FIRST_ATTR + 56),
#define PHIGS_TOOL_BORDER_WIDTH (char *)PHG_TOOL_BORDER_WIDTH

    /* Phigs attributes start at 150 */
    PHG_WS_CATEGORY	     = PHIGS_ATTR( PHG_ATTR_ENUM, PHIGS_FIRST_ATTR + 150)
#define PHIGS_WS_CATEGORY (char *)PHG_WS_CATEGORY

} Phigs_ws_type_attr;

/* Pxphigs_info_mask values */
#define PXPHIGS_INFO_DISPLAY		(1L << 0)
#define PXPHIGS_INFO_RMDB		(1L << 1)
#define PXPHIGS_INFO_APPL_ID		(1L << 2)
#define PXPHIGS_INFO_ARGS		(1L << 3)
#define PXPHIGS_INFO_FLAGS_NO_MON	(1L << 4)
#define PXPHIGS_INFO_FLAGS_CLIENT_SS	(1L << 5)

typedef struct {
    Display	*display; /* valid display pointer.  */
    XrmDatabase	rmdb;	  /* a valid database */
    struct {
	char		*name;
	char		*class_name;
    }		appl_id;  /* for resolving database attributes */
    struct {
	int		*argc_p;
	char		**argv;
    } 		args;  /* for merging args into specified database */
    struct {
	unsigned no_monitor: 1; /* 1 ==> monitor will not be executed */
	unsigned force_client_SS; /* 1 ==> always use client-side CSS */
    }		flags;
} Pxphigs_info;

/* These are declared here because they return types defined here. */
extern  Pint		phigs_ws_type_create(
#if NeedVarargsPrototypes
    Pint, ...
#endif
);
#if NeedVarargsPrototypes
typedef struct _Wst *_PxphigsWst;		/* kludge! */
#endif
extern  caddr_t		phigs_ws_type_set(
#if NeedVarargsPrototypes
    _PxphigsWst, ...
#endif
);
extern  caddr_t		phigs_ws_type_get();

#endif
