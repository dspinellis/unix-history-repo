#ifndef _PEXLIB_H_
#define _PEXLIB_H_

/* $XConsortium: PEXlib.h,v 1.10 92/10/27 15:42:37 mor Exp $ */

/******************************************************************************/
/*  Copyright 1987,1991 by Digital Equipment Corporation, Maynard, Mass.      */
/*                                                                            */
/*  (c) Copyright Hewlett-Packard Company, 1992,  Fort Collins, Colorado      */
/*                                                                            */
/*                            All Rights Reserved                             */
/*                                                                            */
/*  Permission to use, copy, modify, and distribute this software and its     */
/*  documentation for any purpose and without fee is hereby granted,          */
/*  provided that the above copyright notices appear in all copies and that   */
/*  both the copyright notices and this permission notice appear in           */
/*  supporting documentation, and that the names of Digital or                */
/*  Hewlett-Packard not be used in advertising or publicity pertaining to     */
/*  distribution of the software without specific, written prior permission.  */
/*                                                                            */
/*  DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING  */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  */
/*  DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR   */
/*  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,       */
/*  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,    */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS       */
/*  SOFTWARE.                                                                 */
/*                                                                            */
/*  HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD TO THIS         */
/*  SOFTWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF        */
/*  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  Hewlett-Packard    */
/*  shall not be liable for errors contained herein or direct, indirect,      */
/*  special, incidental or consequential damages in connection with the       */
/*  furnishing, performance or use of this software.                          */
/*                                                                            */
/******************************************************************************/

#include <X11/Xlib.h>
#include <X11/PEX5/PEX.h>


#ifndef NeedFunctionPrototypes
#if defined(FUNCPROTO) || defined(__STDC__) || defined(__cplusplus) || defined(c_plusplus)
#define NeedFunctionPrototypes  1
#else
#define NeedFunctionPrototypes  0
#endif /* FUNCPROTO, __STDC__, __cplusplus, c_plusplus */
#endif /* NeedFunctionPrototypes */


#ifdef __cplusplus                      /* do not leave open across includes */
extern "C" {                                    /* for C++ V2.0 */
#endif


/*
 * floating point min and max values
 */
#define PEXMinFloatIeee_754_32  1.40129846432481707e-45
#define PEXMaxFloatIeee_754_32  3.40282346638528860e+38
#define PEXMinFloatIeee_754_64  4.94065645841246544e-324
#define PEXMaxFloatIeee_754_64  1.797693134862315708e+308

/*
 * constants for PEXInitiliaze - failure return values and error string length
 */
#define PEXBadExtension         1
#define PEXBadProtocolVersion   2
#define PEXBadFloatConversion   3
#define PEXBadLocalAlloc        4

#define PEXErrorStringLength    80


/*
 * type definitions
 */
typedef XID             PEXFont;
typedef XID             PEXLookupTable;
typedef XID             PEXNameSet;
typedef XID             PEXPickMeasure;
typedef XID             PEXPipelineContext;
typedef XID             PEXRenderer;
typedef XID             PEXSearchContext;
typedef XID             PEXStructure;
typedef XID             PEXWorkstation;

typedef unsigned long   PEXBitmask;
typedef unsigned short  PEXBitmaskShort;
typedef short           PEXColorType;
typedef unsigned char   PEXContourHint;
typedef unsigned short  PEXCoordType;
typedef short           PEXComposition;
typedef unsigned short  PEXCullMode;
typedef unsigned char   PEXDynamicType;
typedef short           PEXEnumTypeIndex;
typedef float           PEXMatrix[4][4];
typedef float           PEXMatrix3x3[3][3];
typedef unsigned long   PEXName;
typedef int             PEXOCRequestType;
typedef unsigned short  PEXShapeHint;
typedef unsigned char   PEXSwitch;
typedef unsigned short  PEXTableIndex;
typedef unsigned short  PEXTypeOrTableIndex;

#if NeedFunctionPrototypes
typedef void *PEXPointer;
#else
typedef char *PEXPointer;
#endif


/*
 * PEX extension information
 */
typedef struct {
    unsigned short      major_version;
    unsigned short      minor_version;
    unsigned long       release;
    unsigned long       subset_info;
    char                *vendor_name;
    int                 major_opcode;
    int                 first_event;
    int                 first_error;
} PEXExtensionInfo;

/*
 * enumerated type information
 */
typedef struct {
    PEXEnumTypeIndex    index;
    char                *descriptor;            /* null terminated string */
} PEXEnumTypeDesc;

/*
 * implementation dependent constants
 */
typedef union {
    unsigned long       integer;
    float               flt_point;
} PEXImpDepConstant;

/*
 * match rendering targets
 */
typedef struct {
    int                 depth;
    int                 type;
    Visual              *visual;
} PEXRenderingTarget;


/*
 * output primitive and attribute
 */

/* output command request types */
#define PEXOCRender             0
#define PEXOCStore              1
#define PEXOCRenderSingle       2
#define PEXOCStoreSingle        3

/* coordinates */
typedef struct {
    float               x;
    float               y;
    float               z;
} PEXCoord;

typedef struct {
    float               x;
    float               y;
} PEXCoord2D;

typedef struct {
    float               x;
    float               y;
    float               z;
    float               w;
} PEXCoord4D;

/* coordinate lists without data */
typedef struct {
    unsigned long       count;                  /* number of points */
    PEXCoord2D          *points;
} PEXListOfCoord2D;     /* Pointer to an array of 2D points */

typedef struct {
    unsigned long       count;                  /* number of points */
    PEXCoord            *points;
} PEXListOfCoord;       /* Pointer to an array of 3D points */

typedef struct {
    unsigned long       count;                  /* number of points */
    PEXCoord4D          *points;
} PEXListOfCoord4D;     /* Pointer to an array of 4D points */

typedef union {
    PEXCoord2D          *point_2d;
    PEXCoord            *point;
    PEXCoord4D          *point_4d;
} PEXArrayOfCoord;      /* Pointer to array of points */

/* colors */
typedef struct {
    float               red;
    float               green;
    float               blue;
} PEXColorRGB;

typedef struct {
    float               hue;
    float               saturation;
    float               value;
} PEXColorHSV;

typedef struct {
    float               hue;
    float               lightness;
    float               saturation;
} PEXColorHLS;

typedef struct {
    float               x;
    float               y;
    float               z;
} PEXColorCIE;

typedef struct {
    unsigned char       red;
    unsigned char       green;
    unsigned char       blue;
    unsigned char       reserved;
} PEXColorRGB8;

typedef struct {
    unsigned short      red;
    unsigned short      green;
    unsigned short      blue;
    unsigned short      reserved;
} PEXColorRGB16;

typedef struct {
    PEXTableIndex       index;
    unsigned short      reserved;
} PEXColorIndexed;

typedef union {
    PEXColorIndexed     indexed;
    PEXColorRGB         rgb;
    PEXColorHSV         hsv;
    PEXColorHLS         hls;
    PEXColorCIE         cie;
    PEXColorRGB8        rgb8;
    PEXColorRGB16       rgb16;
} PEXColor;

typedef union {
    PEXColorIndexed     *indexed;
    PEXColorRGB         *rgb;
    PEXColorHSV         *hsv;
    PEXColorHLS         *hls;
    PEXColorCIE         *cie;
    PEXColorRGB8        *rgb8;
    PEXColorRGB16       *rgb16;
} PEXArrayOfColor;

/* vectors */
typedef struct {
    float               x;
    float               y;
    float               z;
} PEXVector;

typedef struct {
    float               x;
    float               y;
} PEXVector2D;

/* facet data */
typedef struct {
    PEXColorIndexed     index;
    PEXVector           normal;
} PEXColorIndexedNormal;

typedef struct {
    PEXColorRGB         rgb;
    PEXVector           normal;
} PEXColorRGBNormal;

typedef struct {
    PEXColorCIE         cie;
    PEXVector           normal;
} PEXColorCIENormal;

typedef struct {
    PEXColorHSV         hsv;
    PEXVector           normal;
} PEXColorHSVNormal;

typedef struct {
    PEXColorHLS         hls;
    PEXVector           normal;
} PEXColorHLSNormal;

typedef struct {
    PEXColorRGB8        rgb8;
    PEXVector           normal;
} PEXColorRGB8Normal;

typedef struct {
    PEXColorRGB16       rgb16;
    PEXVector           normal;
} PEXColorRGB16Normal;

typedef union {
    PEXColorIndexed     index;
    PEXColorRGB         rgb;
    PEXColorHSV         hsv;
    PEXColorHLS         hls;
    PEXColorCIE         cie;
    PEXColorRGB8        rgb8;
    PEXColorRGB16       rgb16;
    PEXVector           normal;
    PEXColorIndexedNormal index_normal;
    PEXColorRGBNormal   rgb_normal;
    PEXColorHSVNormal   hsv_normal;
    PEXColorHLSNormal   hls_normal;
    PEXColorCIENormal   cie_normal;
    PEXColorRGB8Normal  rgb8_normal;
    PEXColorRGB16Normal rgb16_normal;
} PEXFacetData;

typedef union {
    PEXColorIndexed     *index;
    PEXColorRGB         *rgb;
    PEXColorHSV         *hsv;
    PEXColorHLS         *hls;
    PEXColorCIE         *cie;
    PEXColorRGB8        *rgb8;
    PEXColorRGB16       *rgb16;
    PEXVector           *normal;
    PEXColorIndexedNormal *index_normal;
    PEXColorRGBNormal   *rgb_normal;
    PEXColorCIENormal   *cie_normal;
    PEXColorHSVNormal   *hsv_normal;
    PEXColorHLSNormal   *hls_normal;
    PEXColorRGB8Normal  *rgb8_normal;
    PEXColorRGB16Normal *rgb16_normal;
} PEXArrayOfFacetData;

/* vertex data */
typedef struct {
    PEXCoord            point;
    PEXColorIndexed     index;
} PEXVertexIndexed;

typedef struct {
    PEXCoord            point;
    PEXColorRGB         rgb;
} PEXVertexRGB;

typedef struct {
    PEXCoord            point;
    PEXColorHSV         hsv;
} PEXVertexHSV;

typedef struct {
    PEXCoord            point;
    PEXColorHLS         hls;
} PEXVertexHLS;

typedef struct {
    PEXCoord            point;
    PEXColorCIE         cie;
} PEXVertexCIE;

typedef struct {
    PEXCoord            point;
    PEXColorRGB8        rgb8;
} PEXVertexRGB8;

typedef struct {
    PEXCoord            point;
    PEXColorRGB16       rgb16;
} PEXVertexRGB16;

typedef struct {
    PEXCoord            point;
    PEXVector           normal;
} PEXVertexNormal;

typedef struct {
    PEXCoord            point;
    unsigned int        edge;
} PEXVertexEdge;

typedef struct {
    PEXCoord            point;
    PEXColorIndexed     index;
    PEXVector           normal;
} PEXVertexIndexedNormal;

typedef struct {
    PEXCoord            point;
    PEXColorRGB         rgb;
    PEXVector           normal;
} PEXVertexRGBNormal;

typedef struct {
    PEXCoord            point;
    PEXColorHSV         hsv;
    PEXVector           normal;
} PEXVertexHSVNormal;

typedef struct {
    PEXCoord            point;
    PEXColorHLS         hls;
    PEXVector           normal;
} PEXVertexHLSNormal;

typedef struct {
    PEXCoord            point;
    PEXColorCIE         cie;
    PEXVector           normal;
} PEXVertexCIENormal;

typedef struct {
    PEXCoord            point;
    PEXColorRGB8        rgb8;
    PEXVector           normal;
} PEXVertexRGB8Normal;

typedef struct {
    PEXCoord            point;
    PEXColorRGB16       rgb16;
    PEXVector           normal;
} PEXVertexRGB16Normal;

typedef struct {
    PEXCoord            point;
    PEXColorIndexed     index;
    unsigned int        edge;
} PEXVertexIndexedEdge;

typedef struct {
    PEXCoord            point;
    PEXColorRGB         rgb;
    unsigned int        edge;
} PEXVertexRGBEdge;

typedef struct {
    PEXCoord            point;
    PEXColorHSV         hsv;
    unsigned int        edge;
} PEXVertexHSVEdge;

typedef struct {
    PEXCoord            point;
    PEXColorHLS         hls;
    unsigned int        edge;
} PEXVertexHLSEdge;

typedef struct {
    PEXCoord            point;
    PEXColorCIE         cie;
    unsigned int        edge;
} PEXVertexCIEEdge;

typedef struct {
    PEXCoord            point;
    PEXColorRGB8        rgb8;
    unsigned int        edge;
} PEXVertexRGB8Edge;

typedef struct {
    PEXCoord            point;
    PEXColorRGB16       rgb16;
    unsigned int        edge;
} PEXVertexRGB16Edge;

typedef struct {
    PEXCoord            point;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexNormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorIndexed     index;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexIndexedNormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorRGB         rgb;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexRGBNormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorHSV         hsv;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexHSVNormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorHLS         hls;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexHLSNormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorCIE         cie;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexCIENormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorRGB8        rgb8;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexRGB8NormalEdge;

typedef struct {
    PEXCoord            point;
    PEXColorRGB16       rgb16;
    PEXVector           normal;
    unsigned int        edge;
} PEXVertexRGB16NormalEdge;

typedef union {
    PEXCoord                    *no_data;
    PEXVertexIndexed            *index;
    PEXVertexRGB                *rgb;
    PEXVertexHSV                *hsv;
    PEXVertexHLS                *hls;
    PEXVertexCIE                *cie;
    PEXVertexRGB8               *rgb8;
    PEXVertexRGB16              *rgb16;
    PEXVertexNormal             *normal;
    PEXVertexEdge               *edge;
    PEXVertexIndexedNormal      *index_normal;
    PEXVertexRGBNormal          *rgb_normal;
    PEXVertexHSVNormal          *hsv_normal;
    PEXVertexHLSNormal          *hls_normal;
    PEXVertexCIENormal          *cie_normal;
    PEXVertexRGB8Normal         *rgb8_normal;
    PEXVertexRGB16Normal        *rgb16_normal;
    PEXVertexIndexedEdge        *index_edge;
    PEXVertexRGBEdge            *rgb_edge;
    PEXVertexHSVEdge            *hsv_edge;
    PEXVertexHLSEdge            *hls_edge;
    PEXVertexCIEEdge            *cie_edge;
    PEXVertexRGB8Edge           *rgb8_edge;
    PEXVertexRGB16Edge          *rgb16_edge;
    PEXVertexNormalEdge         *normal_edge;
    PEXVertexIndexedNormalEdge  *index_normal_edge;
    PEXVertexRGBNormalEdge      *rgb_normal_edge;
    PEXVertexHSVNormalEdge      *hsv_normal_edge;
    PEXVertexHLSNormalEdge      *hls_normal_edge;
    PEXVertexCIENormalEdge      *cie_normal_edge;
    PEXVertexRGB8NormalEdge     *rgb8_normal_edge;
    PEXVertexRGB16NormalEdge    *rgb16_normal_edge;
} PEXArrayOfVertex;

typedef struct {
    unsigned long       count;                  /* number of vertices */
    PEXArrayOfVertex    vertices;               /* pointer to vertices */
} PEXListOfVertex;

/* connectivity list for set of fill area sets */
typedef struct {
    unsigned short      count;                  /* number of shorts */
    unsigned short      *shorts;
} PEXListOfUShort;

typedef struct {
    unsigned short      count;                  /* number of lists */
    PEXListOfUShort     *lists;
} PEXConnectivityData;

/* encoded text */
typedef struct {
    unsigned short      character_set;
    unsigned char       character_set_width;
    unsigned char       encoding_state;
    unsigned short      reserved;
    unsigned short      length;
    char                *ch;
} PEXEncodedTextData;

typedef struct {
    unsigned short      count;                  /* number of encodings */
    PEXEncodedTextData  *encoded_text;
} PEXListOfEncodedText;

/* trimming curves */
typedef struct {
    unsigned short      count;                  /* number of floats */
    float               *floats;
} PEXListOfFloat;

typedef struct {
    PEXSwitch           visibility;
    unsigned char       reserved;
    unsigned short      order;
    PEXCoordType        rationality;
    PEXEnumTypeIndex    approx_method;
    float               tolerance;
    float               tmin, tmax;
    PEXListOfFloat      knots;
    unsigned short      count;                  /* number of control points */
    PEXArrayOfCoord     control_points;
} PEXTrimCurve;

typedef struct {
    unsigned short      count;                  /* number of curves */
    PEXTrimCurve        *curves;
} PEXListOfTrimCurve;

/* half spaces */
typedef struct {
    PEXCoord            point;
    PEXVector           vector;
} PEXHalfSpace;

typedef struct {
    PEXCoord2D          point;
    PEXVector2D         vector;
} PEXHalfSpace2D;

/* parametric surface characteristics */
typedef struct {
    unsigned short      placement_type;
    unsigned short      reserved;
    unsigned short      u_count;
    unsigned short      v_count;
} PEXPSCIsoparametricCurves;

typedef struct {
    PEXCoord            origin;
    PEXVector           direction;
    unsigned short      count;                  /* number of parameters */
    unsigned short      reserved;
    float               *parameters;
} PEXPSCLevelCurves;

typedef struct {
    unsigned short      length;
    char                *data;
} PEXPSCImpDepData;

typedef union {
    PEXPSCIsoparametricCurves iso_curves;
    PEXPSCLevelCurves       level_curves;
    PEXPSCImpDepData        imp_dep;
} PEXPSCData;


/*
 * pipeline context
 */
typedef struct {
    PEXColorType        type;
    unsigned short      reserved;
    PEXColor            value;
} PEXColorSpecifier;

typedef struct {
    unsigned short      vertical;
    unsigned short      horizontal;
} PEXTextAlignment;

typedef struct {
    PEXEnumTypeIndex    method;
    unsigned short      reserved;
    float               tolerance;
} PEXCurveApprox;

typedef struct {
    float               ambient;
    float               diffuse;
    float               specular;
    float               specular_conc;
    float               transmission;
    PEXColorSpecifier   specular_color;
} PEXReflectionAttributes;

typedef struct {
    PEXEnumTypeIndex    method;
    unsigned short      reserved;
    float               u_tolerance;
    float               v_tolerance;
} PEXSurfaceApprox;

typedef struct {
    unsigned short      count;                  /* number of half spaces */
    PEXHalfSpace        *half_spaces;
} PEXModelClipVolume;

typedef struct {
    unsigned short      count;                  /* number of lights */
    PEXTableIndex       *indices;
} PEXListOfLight;

typedef struct {
    short               type;
    PEXPSCData          psc;
} PEXPSCSpecifier;

typedef struct {
    PEXEnumTypeIndex    marker_type;
    float               marker_scale;
    PEXColorSpecifier   marker_color;
    PEXTableIndex       marker_bundle_index;
    PEXTableIndex       text_font;
    unsigned short      text_precision;
    float               char_expansion;
    float               char_spacing;
    PEXColorSpecifier   text_color;
    float               char_height;
    PEXVector2D         char_up_vector;
    unsigned short      text_path;
    PEXTextAlignment    text_alignment;
    float               atext_height;
    PEXVector2D         atext_up_vector;
    unsigned short      atext_path;
    PEXTextAlignment    atext_alignment;
    PEXEnumTypeIndex    atext_style;
    PEXTableIndex       text_bundle_index;
    PEXEnumTypeIndex    line_type;
    float               line_width;
    PEXColorSpecifier   line_color;
    PEXCurveApprox      curve_approx;
    PEXEnumTypeIndex    polyline_interp;
    PEXTableIndex       line_bundle_index;
    PEXEnumTypeIndex    interior_style;
    PEXTypeOrTableIndex interior_style_index;
    PEXColorSpecifier   surface_color;
    PEXReflectionAttributes   reflection_attr;
    PEXEnumTypeIndex    reflection_model;
    PEXEnumTypeIndex    surface_interp;
    PEXEnumTypeIndex    bf_interior_style;
    PEXTypeOrTableIndex bf_interior_style_index;
    PEXColorSpecifier   bf_surface_color;
    PEXReflectionAttributes   bf_reflection_attr;
    PEXEnumTypeIndex    bf_reflection_model;
    PEXEnumTypeIndex    bf_surface_interp;
    PEXSurfaceApprox    surface_approx;
    unsigned short      culling_mode;
    Bool                distinguish;
    PEXCoord2D          pattern_size;
    PEXCoord            pattern_ref_point;
    PEXVector           pattern_ref_vec1;
    PEXVector           pattern_ref_vec2;
    PEXTableIndex       interior_bundle_index;
    PEXSwitch           surface_edges;
    PEXEnumTypeIndex    surface_edge_type;
    float               surface_edge_width;
    PEXColorSpecifier   surface_edge_color;
    PEXTableIndex       edge_bundle_index;
    PEXMatrix           local_transform;
    PEXMatrix           global_transform;
    unsigned short      model_clip;
    PEXModelClipVolume  model_clip_volume;
    PEXTableIndex       view_index;
    PEXListOfLight      light_state;
    PEXTableIndex       depth_cue_index;
    PEXBitmask          asf_enables;
    PEXBitmask          asf_values;
    long                pick_id;
    unsigned long       hlhsr_id;
    PEXNameSet          name_set;
    PEXTableIndex       color_approx_index;
    PEXEnumTypeIndex    rendering_color_model;
    PEXPSCSpecifier     para_surf_char;
} PEXPCAttributes;

/* macros for setting bits in a PC attribute bitmask */

#define PEXSetPCAttributeMask(mask, attr) \
    mask[((attr)) >> 5] |= (unsigned long) 1 << ( ((attr)) & 0x1f)

#define PEXSetPCAttributeMaskAll(mask) \
    mask[0] = 0xffffffff;              \
    mask[1] = 0xffffffff;              \
    mask[2] = 0x0


/*
 * renderer
 */
typedef struct {
    short               xmin;
    short               ymin;
    short               xmax;
    short               ymax;
} PEXDeviceRect;

typedef struct {
    unsigned short      count;          /* number of device rectangles */
    PEXDeviceRect       *rectangles;
} PEXListOfClipRect;

typedef struct {
    PEXCoord            min;
    PEXCoord            max;
} PEXNPCSubVolume;

typedef struct {
    short               x;
    short               y;
    float               z;
} PEXDeviceCoord;

typedef struct {
    short               x;
    short               y;
} PEXDeviceCoord2D;

typedef struct {
    PEXDeviceCoord      min;
    PEXDeviceCoord      max;
    PEXSwitch           use_drawable;
    unsigned char       reserved[3];
} PEXViewport;

typedef struct {
    PEXStructure        structure;
    unsigned long       offset;
} PEXElementRef;

typedef struct {
    unsigned long       count;                  /* number of elements */
    PEXElementRef       *elements;
} PEXStructurePath;

typedef struct {
    PEXStructure        sid;
    unsigned long       offset;
    unsigned long       pick_id;
} PEXPickElementRef;

typedef struct {
    unsigned long       count;                  /* number of elements */
    PEXPickElementRef   *elements;
} PEXPickPath;

typedef struct {
    PEXPipelineContext  pipeline_context;
    PEXStructurePath    current_path;
    PEXLookupTable      marker_bundle;
    PEXLookupTable      text_bundle;
    PEXLookupTable      line_bundle;
    PEXLookupTable      interior_bundle;
    PEXLookupTable      edge_bundle;
    PEXLookupTable      view_table;
    PEXLookupTable      color_table;
    PEXLookupTable      depth_cue_table;
    PEXLookupTable      light_table;
    PEXLookupTable      color_approx_table;
    PEXLookupTable      pattern_table;
    PEXLookupTable      text_font_table;
    PEXNameSet          highlight_incl;
    PEXNameSet          highlight_excl;
    PEXNameSet          invisibility_incl;
    PEXNameSet          invisibility_excl;
    int                 renderer_state;
    PEXEnumTypeIndex    hlhsr_mode;
    PEXNPCSubVolume     npc_subvolume;
    PEXViewport         viewport;
    PEXListOfClipRect   clip_list;
    PEXNameSet          pick_incl;
    PEXNameSet          pick_excl;
    PEXStructurePath    pick_start_path;
    PEXColorSpecifier   background_color;
    Bool                clear_image;
    Bool                clear_z;
    int                 echo_mode;
} PEXRendererAttributes;

/* renderer picking */
typedef PEXNPCSubVolume PEXPDNPCHitVolume;

typedef struct {
    PEXDeviceCoord2D    position;
    float               distance;
} PEXPDDCHitBox;

typedef struct {
    unsigned short      length;                 /* number of bytes in record */
    char                *record;
} PEXPickDataRecord;

typedef union {
    PEXPDNPCHitVolume       volume;
    PEXPDDCHitBox           box;
    PEXPickDataRecord       data;
} PEXPickRecord;


/*
 * name set
 */
typedef struct {
    PEXNameSet          inclusion;
    PEXNameSet          exclusion;
} PEXNameSetPair;

typedef struct {
    unsigned short      count;                  /* number of pairs */
    PEXNameSetPair      *pairs;
} PEXListOfNameSetPair;


/*
 * font
 */
typedef struct {
    Atom                name;
    unsigned long       value;
} PEXFontProp;

typedef struct {
    unsigned long       first_glyph;
    unsigned long       last_glyph;
    unsigned long       default_glyph;
    Bool                all_exist;
    Bool                stroke;
    unsigned short      count;                  /* number of properties */
    PEXFontProp         *props;
} PEXFontInfo;
 
typedef struct {
    unsigned short      length;
    char                *ch;
} PEXStringData;

typedef struct {
    PEXCoord2D          lower_left;
    PEXCoord2D          upper_right;
    PEXCoord2D          concat_point;
} PEXTextExtent;


/*
 * look up table
 */
typedef struct {
    unsigned short      definable_entries;
    unsigned short      predefined_count;
    unsigned short      predefined_min;
    unsigned short      predefined_max;
} PEXTableInfo;

typedef struct {
    PEXEnumTypeIndex    type;
    PEXEnumTypeIndex    interp_method;
    PEXCurveApprox      curve_approx;
    float               width;
    PEXColorSpecifier   color;
} PEXLineBundleEntry;

typedef struct {
    PEXEnumTypeIndex    type;
    short               reserved;
    float               scale;
    PEXColorSpecifier   color;
} PEXMarkerBundleEntry;

typedef struct {
    PEXTableIndex       font_index;
    PEXEnumTypeIndex    precision;
    float               char_expansion;
    float               char_spacing;
    PEXColorSpecifier   color;
} PEXTextBundleEntry;

typedef struct {
    PEXEnumTypeIndex    style;
    PEXTypeOrTableIndex style_index;
    PEXEnumTypeIndex    reflection_model;
    PEXEnumTypeIndex    interp_method;
    PEXEnumTypeIndex    bf_style;
    PEXTypeOrTableIndex bf_style_index;
    PEXEnumTypeIndex    bf_reflection_model;
    PEXEnumTypeIndex    bf_interp_method;
    PEXSurfaceApprox    surface_approx;
    PEXColorSpecifier   color;
    PEXReflectionAttributes   reflection_attr;
    PEXColorSpecifier   bf_color;
    PEXReflectionAttributes   bf_reflection_attr;
} PEXInteriorBundleEntry;

typedef struct {
    PEXSwitch           edge_flag;
    unsigned char       reserved;
    PEXEnumTypeIndex    type;
    float               width;
    PEXColorSpecifier   color;
} PEXEdgeBundleEntry;

typedef struct {
    PEXColorType        color_type;
    unsigned short      row_count;
    unsigned short      col_count;
    PEXArrayOfColor     colors;  /* pointer to 2D array of colors */
} PEXPatternEntry;

typedef PEXColorSpecifier       PEXColorEntry;

typedef struct {
    unsigned short      count;          /* number of fonts */
    PEXFont             *fonts;
} PEXTextFontEntry;

typedef struct {
    PEXEnumTypeIndex    type;
    unsigned short      reserved;
    PEXVector           direction;
    PEXCoord            point;
    float               concentration;
    float               spread_angle;
    float               attenuation1;
    float               attenuation2;
    PEXColorSpecifier   color;
} PEXLightEntry;

typedef struct {
    PEXSwitch           mode;
    unsigned char       reserved[3];
    float               front_plane;
    float               back_plane;
    float               front_scaling;
    float               back_scaling;
    PEXColorSpecifier   color;
} PEXDepthCueEntry;

typedef struct {
    PEXEnumTypeIndex    type;
    PEXEnumTypeIndex    model;
    unsigned short      max1;
    unsigned short      max2;
    unsigned short      max3;
    PEXSwitch           dither;
    unsigned char       reserved;
    unsigned long       mult1;
    unsigned long       mult2;
    unsigned long       mult3;
    float               weight1;
    float               weight2;
    float               weight3;
    unsigned long       base_pixel;
} PEXColorApproxEntry;

typedef struct { 
    unsigned short      clip_flags;
    unsigned short      reserved;
    PEXNPCSubVolume     clip_limits;
    PEXMatrix           orientation;
    PEXMatrix           mapping;
} PEXViewEntry;


/*
 * structure
 */
typedef struct {
    unsigned long       element_count;
    unsigned long       size;
    Bool                has_refs;
    unsigned short      edit_mode;
    unsigned long       element_pointer;
} PEXStructureInfo;

typedef struct {
    unsigned short      type;
    unsigned short      length;
} PEXElementInfo;


/*
 * search context
 */
typedef struct {
    PEXCoord            position;
    float               distance;
    unsigned short      ceiling;
    Bool                model_clip_flag;
    PEXStructurePath    start_path;
    PEXListOfNameSetPair normal;
    PEXListOfNameSetPair inverted;
} PEXSCAttributes;


/*
 * PHIGS workstation
 */
typedef struct {
    PEXTableIndex       index;
    unsigned short      reserved;
    PEXViewEntry        view;
} PEXViewRep;

typedef struct {
    unsigned short      count;                  /* number of views */
    PEXTableIndex       *views;
} PEXListOfView;

typedef struct {
    PEXStructure        sid;
    float               priority;
} PEXPostedStructure;

typedef struct {
    unsigned long       count;                  /* number of posted structures*/
    PEXPostedStructure  *structures;
} PEXListOfPostedStructure;

typedef struct {
    short               drawable_update;
    int                 visual_state;
    int                 drawable_surface;
    int                 view_update;
    PEXListOfView       defined_views;
    int                 wks_update;
    PEXNPCSubVolume     req_npc_subvolume;
    PEXNPCSubVolume     cur_npc_subvolume;
    PEXViewport         req_workstation_viewport;
    PEXViewport         cur_workstation_viewport;
    int                 hlhsr_update;
    PEXEnumTypeIndex    req_hlhsr_mode;
    PEXEnumTypeIndex    cur_hlhsr_mode;
    Drawable            drawable;
    PEXLookupTable      marker_bundle;
    PEXLookupTable      text_bundle;
    PEXLookupTable      line_bundle;
    PEXLookupTable      interior_bundle;
    PEXLookupTable      edge_bundle;
    PEXLookupTable      color_table;
    PEXLookupTable      depth_cue_table;
    PEXLookupTable      light_table;
    PEXLookupTable      color_approx_table;
    PEXLookupTable      pattern_table;
    PEXLookupTable      text_font_table;
    PEXNameSet          highlight_incl;
    PEXNameSet          highlight_excl;
    PEXNameSet          invisibility_incl;
    PEXNameSet          invisibility_excl;
    PEXListOfPostedStructure posted_structures;
    unsigned long       count_priorities;
    int                 buffer_update;
    int                 req_buffer_mode;
    int                 cur_buffer_mode;
} PEXWorkstationAttributes;

/* macros for setting bits in a workstation attribute bitmask */

#define PEXSetPWAttributeMask(mask, attr) \
    mask[((attr)) >> 5] |= (unsigned long) 1 << ( ((attr)) & 0x1f)

#define PEXSetPWAttributeMaskAll(mask) \
    mask[0] = 0xffffffff;              \
    mask[1] = 0x00000003

typedef struct {
    unsigned char       view_rep;
    unsigned char       marker_bundle;
    unsigned char       text_bundle;
    unsigned char       line_bundle;
    unsigned char       interior_bundle;
    unsigned char       edge_bundle;
    unsigned char       color_table;
    unsigned char       pattern_table;
    unsigned char       wks_transform;
    unsigned char       highlight_filter;
    unsigned char       invisibility_filter;
    unsigned char       hlhsr_mode;
    unsigned char       structure_modify;
    unsigned char       post_structure;
    unsigned char       unpost_structure;
    unsigned char       delete_structure;
    unsigned char       reference_modify;
    unsigned char       buffer_modify;
    unsigned char       light_table;
    unsigned char       depth_cue_table;
    unsigned char       color_approx_table;
} PEXWorkstationDynamics;


/*
 * workstation picking
 */
typedef struct {
    unsigned short      status;
    PEXPickPath         pick_path;
} PEXPMAttributes;

typedef struct {
    unsigned short      status;
    PEXPickPath         path;
    int                 path_order;
    PEXNameSet          inclusion;
    PEXNameSet          exclusion;
    PEXPickRecord       pick_record;
    PEXEnumTypeIndex    prompt_echo_type;
    PEXViewport         echo_volume;
    int                 echo_switch;
} PEXPDAttributes;


/*
 * errors
 */
/* similar to XErrorEvent - use to access additional info in OC error */
typedef struct {
    int             type;
    Display         *display;      /* Display the event was read from */
    XID             resourceid;    /* resource id of renderer or structure */
    unsigned long   serial;        /* serial number of failed request */
    unsigned char   error_code;    /* error code of failed request */
    unsigned char   request_code;  /* Major op-code of failed request */
    unsigned char   minor_code;    /* Minor op-code of failed request */
    unsigned short  op_code;       /* op-code of failed output command */
    unsigned short  count;         /* number of output commands successfully */
                                   /* executed before error */
} PEXOCErrorEvent;


/*
 * events
 */
typedef struct {
    int             type;
    unsigned long   serial;     /* # of last request processed by server */
    Bool            send_event; /* true if this came from a SendEvent request */
    Display         *display;   /* Display the event was read from */
    PEXRenderer     renderer;
} PEXMaxHitsReachedEvent;


/*
 * encode and decode
 */
typedef struct {
    unsigned short oc_type;
    union {
        struct {
            unsigned long count;
            PEXName *names;
        } AddToNameSet;
        struct {
            int length;
            char *data;
        } ApplicationData;
        struct {
            PEXCoord point1;
            PEXCoord point2;
            PEXCoord point3;
            unsigned int col_count;
            unsigned int row_count;
            PEXTableIndex *color_indices;
        } CellArray;
        struct {
            PEXCoord2D point1;
            PEXCoord2D point2;
            unsigned int col_count;
            unsigned int row_count;
            PEXTableIndex *color_indices;
        } CellArray2D;
        struct {
            PEXCoord origin;
            PEXCoord offset;
            unsigned int count;
            PEXEncodedTextData *encoded_text;
        } EncodedAnnoText;
        struct {
            PEXCoord2D origin;
            PEXCoord2D offset;
            unsigned int count;
            PEXEncodedTextData *encoded_text;
        } EncodedAnnoText2D;
        struct {
            PEXCoord origin;
            PEXVector vector1;
            PEXVector vector2;
            unsigned int count;
            PEXEncodedTextData *encoded_text;
        } EncodedText;
        struct {
            PEXCoord2D origin;
            unsigned int count;
            PEXEncodedTextData *encoded_text;
        } EncodedText2D;
        struct {
            PEXStructure structure;
        } ExecuteStructure;
        struct {
            PEXCoord point1;
            PEXCoord point2;
            PEXCoord point3;
            unsigned int col_count;
            unsigned int row_count;
            int color_type;
            PEXArrayOfColor colors;
        } ExtendedCellArray;
        struct {
            int shape_hint;
            int ignore_edges;
            unsigned int count;
            PEXCoord *points;
        } FillArea;
        struct {
            int shape_hint;
            int ignore_edges;
            unsigned int count;
            PEXCoord2D *points;
        } FillArea2D;
        struct {
            int shape_hint;
            int ignore_edges;
            int contour_hint;
            unsigned int count;
            PEXListOfCoord *point_lists;
        } FillAreaSet;
        struct {
            int shape_hint;
            int ignore_edges;
            int contour_hint;
            unsigned int count;
            PEXListOfCoord2D *point_lists;
        } FillAreaSet2D;
        struct {
            int shape_hint;
            int ignore_edges;
            int contour_hint;
            unsigned int facet_attributes;
            unsigned int vertex_attributes;
            int color_type;
            unsigned int count;
            PEXFacetData facet_data;
            PEXListOfVertex *vertex_lists;
        } FillAreaSetWithData;
        struct {
            int shape_hint;
            int ignore_edges;
            unsigned int facet_attributes;
            unsigned int vertex_attributes;
            int color_type;
            PEXFacetData facet_data;
            unsigned int count;
            PEXArrayOfVertex vertices;
        } FillAreaWithData;
        struct {
            long gdp_id;
            unsigned int count;
            PEXCoord *points;
            unsigned long length;
            char *data;
        } GDP;
        struct {
            long gdp_id;
            unsigned int count;
            PEXCoord2D *points;
            unsigned long length;
            char *data;
        } GDP2D;
        struct {
            long id;
            int length;
            char *data;
        } GSE;
        struct {
            long label;
        } Label;
        struct {
            unsigned int count;
            PEXCoord *points;
        } Markers;
        struct {
            unsigned int count;
            PEXCoord2D *points;
        } Markers2D;
        struct {
            int rationality;
            int order;
            float *knots;
            unsigned int count;
            PEXArrayOfCoord points;
            double tmin;
            double tmax;
        } NURBCurve;
        struct {
            int rationality;
            int uorder;
            int vorder;
            float *uknots;
            float *vknots;
            unsigned int col_count;
            unsigned int row_count;
            PEXArrayOfCoord points;
            unsigned int curve_count;
            PEXListOfTrimCurve *trim_curves;
        } NURBSurface;
        struct {
            unsigned int count;
            PEXCoord *points;
        } Polyline;
        struct {
            unsigned int count;
            PEXCoord2D *points;
        } Polyline2D;
        struct {
            unsigned int vertex_attributes;
            int color_type;
            unsigned int count;
            PEXListOfVertex *vertex_lists;
        } PolylineSetWithData;
        struct {
            int shape_hint;
            unsigned int facet_attributes;
            unsigned int vertex_attributes;
            int color_type;
            PEXArrayOfFacetData facet_data;
            unsigned int col_count;
            unsigned int row_count;
            PEXArrayOfVertex vertices;
        } QuadrilateralMesh;
        struct {
            unsigned long count;
            PEXName *names;
        } RemoveFromNameSet;
        struct {
            int halignment;
            int valignment;
        } SetATextAlignment;
        struct {
            double height;
        } SetATextHeight;
        struct {
            int path;
        } SetATextPath;
        struct {
            int style;
        } SetATextStyle;
        struct {
            PEXVector2D vector;
        } SetATextUpVector;
        struct {
            int style;
        } SetBFInteriorStyle;
        struct {
            int index;
        } SetBFInteriorStyleIndex;
        struct {
            PEXReflectionAttributes attributes;
        } SetBFReflectionAttributes;
        struct {
            int model;
        } SetBFReflectionModel;
        struct {
            int color_type;
            PEXColor color;
        } SetBFSurfaceColor;
        struct {
            unsigned int index;
        } SetBFSurfaceColorIndex;
        struct {
            int method;
        } SetBFSurfaceInterpMethod;
        struct {
            double expansion;
        } SetCharExpansion;
        struct {
            double height;
        } SetCharHeight;
        struct {
            double spacing;
        } SetCharSpacing;
        struct {
            PEXVector2D vector;
        } SetCharUpVector;
        struct {
            unsigned int index;
        } SetColorApproxIndex;
        struct {
            int method;
            double tolerance;
        } SetCurveApprox;
        struct {
            unsigned int index;
        } SetDepthCueIndex;
        struct {
            unsigned int index;
        } SetEdgeBundleIndex;
        struct {
            int mode;
        } SetFacetCullingMode;
        struct {
            int flag;
        } SetFacetDistinguishFlag;
        struct {
            PEXMatrix transform;
        } SetGlobalTransform;
        struct {
            PEXMatrix3x3 transform;
        } SetGlobalTransform2D;
        struct {
            unsigned long hlhsr_id;
        } SetHLHSRID;
        struct {
            unsigned long attribute;
            int asf;
        } SetIndividualASF;
        struct {
            unsigned int index;
        } SetInteriorBundleIndex;
        struct {
            int style;
        } SetInteriorStyle;
        struct {
            int index;
        } SetInteriorStyleIndex;
        struct {
            unsigned int enable_count;
            PEXTableIndex *enable;
            unsigned int disable_count;
            PEXTableIndex *disable;
        } SetLightSourceState;
        struct {
            unsigned int index;
        } SetLineBundleIndex;
        struct {
            int color_type;
            PEXColor color;
        } SetLineColor;
        struct {
            unsigned int index;
        } SetLineColorIndex;
        struct {
            int line_type;
        } SetLineType;
        struct {
            double width;
        } SetLineWidth;
        struct {
            int composition;
            PEXMatrix transform;
        } SetLocalTransform;
        struct {
            int composition;
            PEXMatrix3x3 transform;
        } SetLocalTransform2D;
        struct {
            unsigned int index;
        } SetMarkerBundleIndex;
        struct {
            int color_type;
            PEXColor color;
        } SetMarkerColor;
        struct {
            unsigned int index;
        } SetMarkerColorIndex;
        struct {
            double scale;
        } SetMarkerScale;
        struct {
            int marker_type;
        } SetMarkerType;
        struct {
            int flag;
        } SetModelClipFlag;
        struct {
            int op;
            unsigned int count;
            PEXHalfSpace *half_spaces;
        } SetModelClipVolume;
        struct {
            int op;
            unsigned int count;
            PEXHalfSpace2D *half_spaces;
        } SetModelClipVolume2D;
        struct {
            int shape_hint;
            unsigned int facet_attributes;
            unsigned int vertex_attributes;
            unsigned int edge_attributes;
            int contour_hint;
            int contours_all_one;
            int color_type;
            unsigned int set_count;
            PEXArrayOfFacetData facet_data;
            unsigned int vertex_count;
            PEXArrayOfVertex vertices;
            unsigned int index_count;
            PEXSwitch *edge_flags;
            PEXConnectivityData *connectivity;
        } SetOfFillAreaSets;
        struct {
            int psc_type;
            PEXPSCData characteristics;
        } SetParaSurfCharacteristics;
        struct {
            PEXCoord ref_point;
            PEXVector vector1;
            PEXVector vector2;
        } SetPatternAttributes;
        struct {
            PEXCoord2D ref_point;
        } SetPatternAttributes2D;
        struct {
            double width;
            double height;
        } SetPatternSize;
        struct {
            unsigned long pick_id;
        } SetPickID;
        struct {
            int method;
        } SetPolylineInterpMethod;
        struct {
            PEXReflectionAttributes attributes;
        } SetReflectionAttributes;
        struct {
            int model;
        } SetReflectionModel;
        struct {
            int model;
        } SetRenderingColorModel;
        struct {
            int method;
            double utolerance;
            double vtolerance;
        } SetSurfaceApprox;
        struct {
            int color_type;
            PEXColor color;
        } SetSurfaceColor;
        struct {
            unsigned int index;
        } SetSurfaceColorIndex;
        struct {
            int color_type;
            PEXColor color;
        } SetSurfaceEdgeColor;
        struct {
            unsigned int index;
        } SetSurfaceEdgeColorIndex;
        struct {
            int flag;
        } SetSurfaceEdgeFlag;
        struct {
            int edge_type;
        } SetSurfaceEdgeType;
        struct {
            double width;
        } SetSurfaceEdgeWidth;
        struct {
            int method;
        } SetSurfaceInterpMethod;
        struct {
            int halignment;
            int valignment;
        } SetTextAlignment;
        struct {
            unsigned int index;
        } SetTextBundleIndex;
        struct {
            int color_type;
            PEXColor color;
        } SetTextColor;
        struct {
            unsigned int index;
        } SetTextColorIndex;
        struct {
            unsigned int index;
        } SetTextFontIndex;
        struct {
            int path;
        } SetTextPath;
        struct {
            int precision;
        } SetTextPrecision;
        struct {
            unsigned int index;
        } SetViewIndex;
        struct {
            unsigned int facet_attributes;
            unsigned int vertex_attributes;
            int color_type;
            PEXArrayOfFacetData facet_data;
            unsigned int count;
            PEXArrayOfVertex vertices;
        } TriangleStrip;
    } data;
} PEXOCData;


/*
 * encoded output commands
 */

/* macro for inquiring max length for PEXGetOCAddr */

#define PEXGetOCAddrMaxSize(_display) \
    ((_display)->bufmax - (_display)->buffer)   /* this macro returns the     */
                                                /* maximum allowable size (in */
                                                /* bytes) for PEXGetOCAddr    */
                                                /* individual implementations */
                                                /* can modify the value, but  */
                                                /* the minimum allowed is 1024*/


/*
 * constants for utilities
 */

/* constants for PEXRotate */
#define PEXXAxis                1
#define PEXYAxis                2
#define PEXZAxis                3

/* constants for utilities error return status */
#define PEXBadVector            1
#define PEXBadVectors           2
#define PEXBadLimits            3
#define PEXBadViewport          4
#define PEXBadPlanes            5
#define PEXBadPRP               6
#define PEXBadMatrix            7
#define PEXBadPrimitive         8
#define PEXBadDistance          9
#define PEXBadAxis              10
#define PEXBadHomoCoord         11
#define PEXBadSubVolume         12


/*
 * function declarations
 */

extern void PEXAccumulateState(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    unsigned long 		/* count */,
    PEXElementRef *		/* elements */
#endif
);

extern void PEXAddToNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned long 		/* count */,
    PEXName *		/* names */
#endif
);

extern void PEXAnnotationText(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* origin */,
    PEXCoord *		/* offset */,
    int 		/* length */,
    char *		/* string */
#endif
);

extern void PEXAnnotationText2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord2D *		/* origin */,
    PEXCoord2D *		/* offset */,
    int 		/* length */,
    char *		/* string */
#endif
);

extern void PEXApplicationData(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* length */,
    char *		/* data */
#endif
);

extern void PEXBeginPickAll(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXRenderer 		/* renderer */,
    long 		/* structure_id */,
    int 		/* method */,
    int 		/* send_event */,
    int 		/* max_hits */,
    int 		/* pick_device_type */,
    PEXPickRecord *		/* pick_record */
#endif
);

extern void PEXBeginPickOne(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXRenderer 		/* renderer */,
    long 		/* structure_id */,
    int 		/* method */,
    int 		/* pick_device_type */,
    PEXPickRecord *		/* pick_record */
#endif
);

extern void PEXBeginRendering(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXRenderer 		/* renderer */
#endif
);

extern void PEXBeginStructure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    long 		/* structure_id */
#endif
);

extern void PEXBuildTransform(
#if NeedFunctionPrototypes
    PEXCoord *		/* fixed_point */,
    PEXVector *		/* trans_vector */,
    double 		/* angle_x */,
    double 		/* angle_y */,
    double 		/* angle_z */,
    PEXVector *		/* scale_vector */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXBuildTransform2D(
#if NeedFunctionPrototypes
    PEXCoord2D *		/* fixed_point */,
    PEXVector2D *		/* trans_vector */,
    double 		/* angle_z */,
    PEXVector2D *		/* scale_vector */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern void PEXCellArray(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* point1 */,
    PEXCoord *		/* point2 */,
    PEXCoord *		/* point3 */,
    unsigned int 		/* col_count */,
    unsigned int 		/* row_count */,
    PEXTableIndex *		/* color_indices */
#endif
);

extern void PEXCellArray2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord2D *		/* point1 */,
    PEXCoord2D *		/* point2 */,
    unsigned int 		/* col_count */,
    unsigned int 		/* row_count */,
    PEXTableIndex *		/* color_indices */
#endif
);

extern void PEXChangeNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXNameSet 		/* nameset */,
    int 		/* action */,
    unsigned long 		/* count */,
    PEXName *		/* names */
#endif
);

extern void PEXChangePickDevice(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    int 		/* pick_device_type */,
    unsigned long 		/* value_mask */,
    PEXPDAttributes *		/* values */
#endif
);

extern void PEXChangePipelineContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXPipelineContext 		/* context */,
    unsigned long *		/* value_mask */,
    PEXPCAttributes *		/* values */
#endif
);

extern void PEXChangeRenderer(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    unsigned long 		/* value_mask */,
    PEXRendererAttributes *		/* values */
#endif
);

extern void PEXChangeSearchContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXSearchContext 		/* context */,
    unsigned long 		/* value_mask */,
    PEXSCAttributes *		/* values */
#endif
);

extern void PEXChangeStructureRefs(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* old_structure */,
    PEXStructure 		/* new_structure */
#endif
);

extern void PEXCopyBytesToOC(
#if NeedFunctionPrototypes
    Display *		/* display */,
    int 		/* length */,
    char *		/* data */
#endif
);

extern void PEXCopyElements(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* src_structure */,
    int 		/* src_whence1 */,
    long 		/* src_offset1 */,
    int 		/* src_whence2 */,
    long 		/* src_offset2 */,
    PEXStructure 		/* dst_structure */,
    int 		/* dst_whence */,
    long 		/* dst_offset */
#endif
);

extern void PEXCopyLookupTable(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* src_table */,
    PEXLookupTable 		/* dst_table */
#endif
);

extern void PEXCopyNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXNameSet 		/* src_nameset */,
    PEXNameSet 		/* dst_nameset */
#endif
);

extern void PEXCopyPipelineContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long *		/* value_mask */,
    PEXPipelineContext 		/* src_context */,
    PEXPipelineContext 		/* dst_context */
#endif
);

extern void PEXCopySearchContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long 		/* value_mask */,
    PEXSearchContext 		/* src_context */,
    PEXSearchContext 		/* dst_context */
#endif
);

extern void PEXCopyStructure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* src_structure */,
    PEXStructure 		/* dst_structure */
#endif
);

extern PEXLookupTable PEXCreateLookupTable(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    int 		/* table_type */
#endif
);

extern PEXNameSet PEXCreateNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */
#endif
);

extern PEXPickMeasure PEXCreatePickMeasure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    int 		/* pick_device_type */
#endif
);

extern PEXPipelineContext PEXCreatePipelineContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long *		/* value_mask */,
    PEXPCAttributes *		/* values */
#endif
);

extern PEXRenderer PEXCreateRenderer(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    unsigned long 		/* value_mask */,
    PEXRendererAttributes *		/* values */
#endif
);

extern PEXSearchContext PEXCreateSearchContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long 		/* value_mask */,
    PEXSCAttributes *		/* values */
#endif
);

extern PEXStructure PEXCreateStructure(
#if NeedFunctionPrototypes
    Display *		/* display */
#endif
);

extern PEXWorkstation PEXCreateWorkstation(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXLookupTable 		/* line_bundle */,
    PEXLookupTable 		/* marker_bundle */,
    PEXLookupTable 		/* text_bundle */,
    PEXLookupTable 		/* interior_bundle */,
    PEXLookupTable 		/* edge_bundle */,
    PEXLookupTable 		/* color_table */,
    PEXLookupTable 		/* pattern_table */,
    PEXLookupTable 		/* font_table */,
    PEXLookupTable 		/* depth_cue_table */,
    PEXLookupTable 		/* light_table */,
    PEXLookupTable 		/* color_approx_table */,
    PEXNameSet 		/* highlight_incl */,
    PEXNameSet 		/* highlight_excl */,
    PEXNameSet 		/* invisibility_incl */,
    PEXNameSet 		/* invisibility_excl */,
    int 		/* buffer_mode */
#endif
);

extern PEXOCData *PEXDecodeOCs(
#if NeedFunctionPrototypes
    int 		/* float_format */,
    unsigned long 		/* oc_count */,
    unsigned long 		/* length */,
    char *		/* encoded_ocs */
#endif
);

extern void PEXDeleteBetweenLabels(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    long 		/* label1 */,
    long 		/* label2 */
#endif
);

extern void PEXDeleteElements(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* whence1 */,
    long 		/* offset1 */,
    int 		/* whence2 */,
    long 		/* offset2 */
#endif
);

extern void PEXDeleteTableEntries(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* table */,
    unsigned int 		/* start */,
    unsigned int 		/* count */
#endif
);

extern void PEXDeleteToLabel(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* whence */,
    long 		/* offset */,
    long 		/* label */
#endif
);

extern void PEXDestroyStructures(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long 		/* count */,
    PEXStructure *		/* structures */
#endif
);

extern Status PEXElementSearch(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* whence */,
    long 		/* offset */,
    int 		/* direction */,
    unsigned long 		/* incl_count */,
    unsigned short *		/* incl_list */,
    unsigned long 		/* excl_count */,
    unsigned short *		/* excl_list */,
    unsigned long *		/* elem_offset_return */
#endif
);

extern char *PEXEncodeOCs(
#if NeedFunctionPrototypes
    int 		/* float_format */,
    unsigned long 		/* oc_count */,
    PEXOCData *		/* oc_data */,
    unsigned long *		/* length_return */
#endif
);

extern void PEXEncodedAnnoText(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* origin */,
    PEXCoord *		/* offset */,
    unsigned int 		/* count */,
    PEXEncodedTextData *		/* encoded_text */
#endif
);

extern void PEXEncodedAnnoText2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord2D *		/* origin */,
    PEXCoord2D *		/* offset */,
    unsigned int 		/* count */,
    PEXEncodedTextData *		/* encoded_text */
#endif
);

extern void PEXEncodedText(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* origin */,
    PEXVector *		/* vector1 */,
    PEXVector *		/* vector2 */,
    unsigned int 		/* count */,
    PEXEncodedTextData *		/* encoded_text */
#endif
);

extern void PEXEncodedText2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord2D *		/* origin */,
    unsigned int 		/* count */,
    PEXEncodedTextData *		/* encoded_text */
#endif
);

extern PEXPickPath *PEXEndPickAll(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    int *		/* status_return */,
    int *		/* more_return */,
    unsigned long *		/* count_return */
#endif
);

extern PEXPickPath *PEXEndPickOne(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    int *		/* status_return */,
    int *		/* undetectable_return */
#endif
);

extern void PEXEndRendering(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    int 		/* flush */
#endif
);

extern void PEXEndStructure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */
#endif
);

extern void PEXEscape(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long 		/* escape_id */,
    int 		/* length */,
    char *		/* escape_data */
#endif
);

extern char *PEXEscapeWithReply(
#if NeedFunctionPrototypes
    Display *		/* display */,
    unsigned long 		/* escape_id */,
    int 		/* length */,
    char *		/* escape_data */,
    unsigned long *		/* reply_length_return */
#endif
);

extern void PEXExecuteDeferredActions(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */
#endif
);

extern void PEXExecuteStructure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXStructure 		/* structure */
#endif
);

extern void PEXExtendedCellArray(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* point1 */,
    PEXCoord *		/* point2 */,
    PEXCoord *		/* point3 */,
    unsigned int 		/* col_count */,
    unsigned int 		/* row_count */,
    int 		/* color_type */,
    PEXArrayOfColor 		/* colors */
#endif
);

extern Status PEXFetchElements(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* whence1 */,
    long 		/* offset1 */,
    int 		/* whence2 */,
    long 		/* offset2 */,
    int 		/* float_format */,
    unsigned long *		/* count_return */,
    unsigned long *		/* length_return */,
    char **		/* ocs_return */
#endif
);

extern Status PEXFetchElementsAndSend(
#if NeedFunctionPrototypes
    Display *		/* src_display */,
    PEXStructure 		/* structure */,
    int 		/* whence1 */,
    long 		/* offset1 */,
    int 		/* whence2 */,
    long 		/* offset2 */,
    Display *		/* dst_display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */
#endif
);

extern void PEXFillArea(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    int 		/* ignore_edges */,
    unsigned int 		/* count */,
    PEXCoord *		/* points */
#endif
);

extern void PEXFillArea2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    int 		/* ignore_edges */,
    unsigned int 		/* count */,
    PEXCoord2D *		/* points */
#endif
);

extern void PEXFillAreaSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    int 		/* ignore_edges */,
    int 		/* contour_hint */,
    unsigned int 		/* count */,
    PEXListOfCoord *		/* point_lists */
#endif
);

extern void PEXFillAreaSet2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    int 		/* ignore_edges */,
    int 		/* contour_hint */,
    unsigned int 		/* count */,
    PEXListOfCoord2D *		/* point_lists */
#endif
);

extern void PEXFillAreaSetWithData(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    int 		/* ignore_edges */,
    int 		/* contour_hint */,
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    unsigned int 		/* count */,
    PEXFacetData *		/* facet_data */,
    PEXListOfVertex *		/* vertex_lists */
#endif
);

extern void PEXFillAreaWithData(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    int 		/* ignore_edges */,
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    PEXFacetData *		/* facet_data */,
    unsigned int 		/* count */,
    PEXArrayOfVertex 		/* vertices */
#endif
);

extern void PEXFinishOCs(
#if NeedFunctionPrototypes
    Display *		/* display */
#endif
);

extern void PEXFreeEnumInfo(
#if NeedFunctionPrototypes
    unsigned long 		/* count */,
    unsigned long *		/* info_count */,
    PEXEnumTypeDesc *		/* enum_info */
#endif
);

extern void PEXFreeFontInfo(
#if NeedFunctionPrototypes
    unsigned long 		/* count */,
    PEXFontInfo *		/* font_info */
#endif
);

extern void PEXFreeFontNames(
#if NeedFunctionPrototypes
    unsigned long 		/* count */,
    char **		/* font_names */
#endif
);

extern void PEXFreeLookupTable(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* table */
#endif
);

extern void PEXFreeNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXNameSet 		/* nameset */
#endif
);

extern void PEXFreeOCData(
#if NeedFunctionPrototypes
    unsigned long 		/* count */,
    PEXOCData *		/* oc_data */
#endif
);

extern void PEXFreePCAttributes(
#if NeedFunctionPrototypes
    PEXPCAttributes *		/* values */
#endif
);

extern void PEXFreePDAttributes(
#if NeedFunctionPrototypes
    PEXPDAttributes *		/* values */
#endif
);

extern void PEXFreePMAttributes(
#if NeedFunctionPrototypes
    PEXPMAttributes *		/* values */
#endif
);

extern void PEXFreePickMeasure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXPickMeasure 		/* pick_measure */
#endif
);

extern void PEXFreePickPaths(
#if NeedFunctionPrototypes
    unsigned long 		/* count */,
    PEXPickPath *		/* pick_paths */
#endif
);

extern void PEXFreePipelineContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXPipelineContext 		/* context */
#endif
);

extern void PEXFreeRenderer(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */
#endif
);

extern void PEXFreeRendererAttributes(
#if NeedFunctionPrototypes
    PEXRendererAttributes *		/* values */
#endif
);

extern void PEXFreeSCAttributes(
#if NeedFunctionPrototypes
    PEXSCAttributes *		/* values */
#endif
);

extern void PEXFreeSearchContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXSearchContext 		/* context */
#endif
);

extern void PEXFreeStructurePaths(
#if NeedFunctionPrototypes
    unsigned long 		/* count */,
    PEXStructurePath *		/* paths */
#endif
);

extern void PEXFreeTableEntries(
#if NeedFunctionPrototypes
    int 		/* table_type */,
    unsigned int 		/* count */,
    PEXPointer 		/* entries */
#endif
);

extern void PEXFreeWorkstation(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */
#endif
);

extern void PEXFreeWorkstationAttributes(
#if NeedFunctionPrototypes
    PEXWorkstationAttributes *		/* values */
#endif
);

extern void PEXGDP(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    long 		/* gdp_id */,
    unsigned int 		/* count */,
    PEXCoord *		/* points */,
    unsigned long 		/* length */,
    char *		/* data */
#endif
);

extern void PEXGDP2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    long 		/* gdp_id */,
    unsigned int 		/* count */,
    PEXCoord2D *		/* points */,
    unsigned long 		/* length */,
    char *		/* data */
#endif
);

extern void PEXGSE(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    long 		/* id */,
    int 		/* length */,
    char *		/* data */
#endif
);

extern int PEXGeoNormFillArea(
#if NeedFunctionPrototypes
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    PEXFacetData *		/* facet_data */,
    unsigned int 		/* count */,
    PEXArrayOfVertex 		/* vertices */
#endif
);

extern int PEXGeoNormFillAreaSet(
#if NeedFunctionPrototypes
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    unsigned int 		/* count */,
    PEXFacetData *		/* facet_data */,
    PEXListOfVertex *		/* vertex_lists */
#endif
);

extern int PEXGeoNormQuadrilateralMesh(
#if NeedFunctionPrototypes
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    PEXArrayOfFacetData 		/* facet_data */,
    unsigned int 		/* col_count */,
    unsigned int 		/* row_count */,
    PEXArrayOfVertex 		/* vertices */
#endif
);

extern int PEXGeoNormSetOfFillAreaSets(
#if NeedFunctionPrototypes
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    unsigned int 		/* set_count */,
    PEXArrayOfFacetData 		/* facet_data */,
    unsigned int 		/* vertex_count */,
    PEXArrayOfVertex 		/* vertices */,
    unsigned int 		/* index_count */,
    PEXConnectivityData *		/* connectivity */
#endif
);

extern int PEXGeoNormTriangleStrip(
#if NeedFunctionPrototypes
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    PEXArrayOfFacetData 		/* facet_data */,
    unsigned int 		/* count */,
    PEXArrayOfVertex 		/* vertices */
#endif
);

extern PEXStructurePath *PEXGetAncestors(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* path_part */,
    unsigned long 		/* path_depth */,
    unsigned long *		/* count_return */
#endif
);

extern Status PEXGetDefinedIndices(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* table */,
    unsigned long *		/* count_return */,
    PEXTableIndex **		/* indices_return */
#endif
);

extern PEXStructurePath *PEXGetDescendants(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* path_part */,
    unsigned long 		/* path_depth */,
    unsigned long *		/* count_return */
#endif
);

extern Status PEXGetElementInfo(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* whence1 */,
    long 		/* offset1 */,
    int 		/* whence2 */,
    long 		/* offset2 */,
    int 		/* float_format */,
    unsigned long *		/* count_return */,
    PEXElementInfo **		/* info_return */
#endif
);

extern Status PEXGetEnumTypeInfo(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    unsigned long 		/* count */,
    int *		/* enum_types */,
    unsigned long 		/* item_mask */,
    unsigned long **		/* info_count_return */,
    PEXEnumTypeDesc **		/* enum_info_return */
#endif
);

extern PEXExtensionInfo *PEXGetExtensionInfo(
#if NeedFunctionPrototypes
    Display *		/* display */
#endif
);

extern Status PEXGetImpDepConstants(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    unsigned long 		/* count */,
    unsigned short *		/* names */,
    PEXImpDepConstant **		/* constants_return */
#endif
);

extern Status PEXGetNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXNameSet 		/* nameset */,
    unsigned long *		/* count_return */,
    PEXName **		/* names_return */
#endif
);

extern char *PEXGetOCAddr(
#if NeedFunctionPrototypes
    Display *		/* display */,
    int 		/* length */
#endif
);

extern PEXPDAttributes *PEXGetPickDevice(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    int 		/* pick_device_type */,
    unsigned long 		/* value_mask */
#endif
);

extern PEXPMAttributes *PEXGetPickMeasure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXPickMeasure 		/* pick_measure */,
    unsigned long 		/* value_mask */
#endif
);

extern PEXPCAttributes *PEXGetPipelineContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXPipelineContext 		/* context */,
    unsigned long *		/* value_mask */
#endif
);

extern Status PEXGetPredefinedEntries(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    int 		/* table_type */,
    unsigned int 		/* start */,
    unsigned int 		/* count */,
    PEXPointer *		/* entries_return */
#endif
);

extern int PEXGetProtocolFloatFormat(
#if NeedFunctionPrototypes
    Display *		/* display */
#endif
);

extern PEXRendererAttributes *PEXGetRendererAttributes(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    unsigned long 		/* value_mask */
#endif
);

extern Status PEXGetRendererDynamics(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    unsigned long *		/* tables_return */,
    unsigned long *		/* name_sets_return */,
    unsigned long *		/* attributes_return */
#endif
);

extern PEXSCAttributes *PEXGetSearchContext(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXSearchContext 		/* context */,
    unsigned long 		/* value_mask */
#endif
);

extern int PEXGetSizeOCs(
#if NeedFunctionPrototypes
    int 		/* float_format */,
    int 		/* oc_count */,
    PEXOCData *		/* oc_data */
#endif
);

extern Status PEXGetStructureInfo(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* float_format */,
    unsigned long 		/* value_mask */,
    PEXStructureInfo *		/* info_return */
#endif
);

extern PEXStructure *PEXGetStructuresInNetwork(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* which */,
    unsigned long *		/* count_return */
#endif
);

extern Status PEXGetTableEntries(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* table */,
    unsigned int 		/* start */,
    unsigned int 		/* count */,
    int 		/* value_type */,
    int *		/* table_type_return */,
    PEXPointer *		/* entries_return */
#endif
);

extern PEXPointer PEXGetTableEntry(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* table */,
    unsigned int 		/* index */,
    int 		/* value_type */,
    int *		/* status_return */,
    int *		/* table_type_return */
#endif
);

extern Status PEXGetTableInfo(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    int 		/* table_type */,
    PEXTableInfo *		/* info_return */
#endif
);

extern PEXWorkstationAttributes *PEXGetWorkstationAttributes(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned long *		/* value_mask */
#endif
);

extern Status PEXGetWorkstationDynamics(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXWorkstationDynamics *		/* dynamics_return */
#endif
);

extern Status PEXGetWorkstationPostings(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    unsigned long *		/* count_return */,
    PEXWorkstation **		/* postings_return */
#endif
);

extern Status PEXGetWorkstationViewRep(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned int 		/* index */,
    int *		/* update_return */,
    PEXViewRep *		/* req_view_return */,
    PEXViewRep *		/* cur_view_return */
#endif
);

extern void PEXIdentityMatrix(
#if NeedFunctionPrototypes
    PEXMatrix 		/* transform_return */
#endif
);

extern void PEXIdentityMatrix2D(
#if NeedFunctionPrototypes
    PEXMatrix3x3 		/* transform_return */
#endif
);

extern int PEXInitialize(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXExtensionInfo **		/* info_return */,
    int 		/* length */,
    char *		/* error_string */
#endif
);

extern int PEXInvertMatrix(
#if NeedFunctionPrototypes
    PEXMatrix 		/* transform */,
    PEXMatrix 		/* transform_return */
#endif
);

extern int PEXInvertMatrix2D(
#if NeedFunctionPrototypes
    PEXMatrix3x3 		/* transform */,
    PEXMatrix3x3 		/* transform_return */
#endif
);

extern void PEXLabel(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    long 		/* label */
#endif
);

extern char **PEXListFonts(
#if NeedFunctionPrototypes
    Display *		/* display */,
    char *		/* pattern */,
    unsigned int 		/* max_names */,
    unsigned long *		/* count_return */
#endif
);

extern char **PEXListFontsWithInfo(
#if NeedFunctionPrototypes
    Display *		/* display */,
    char *		/* pattern */,
    unsigned int 		/* max_names */,
    unsigned long *		/* count_return */,
    PEXFontInfo **		/* info_return */
#endif
);

extern PEXFont PEXLoadFont(
#if NeedFunctionPrototypes
    Display *		/* display */,
    char *		/* font_name */
#endif
);

extern int PEXLookAtViewMatrix(
#if NeedFunctionPrototypes
    PEXCoord *		/* from */,
    PEXCoord *		/* to */,
    PEXVector *		/* up */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern Status PEXMapDCToWC(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned long 		/* dc_count */,
    PEXDeviceCoord *		/* dc_points */,
    unsigned int *		/* view_index_return */,
    unsigned long *		/* wc_count_return */,
    PEXCoord **		/* wc_points_return */
#endif
);

extern Status PEXMapWCToDC(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned long 		/* wc_count */,
    PEXCoord *		/* wc_points */,
    unsigned int 		/* view_index */,
    unsigned long *		/* dc_count_return */,
    PEXDeviceCoord **		/* dc_points_return */
#endif
);

extern int PEXMapXCToNPC(
#if NeedFunctionPrototypes
    int 		/* point_count */,
    PEXDeviceCoord2D *		/* points */,
    unsigned int 		/* window_height */,
    double 		/* z_dc */,
    PEXDeviceCoord *		/* viewport */,
    PEXNPCSubVolume *		/* npc_sub_volume */,
    int 		/* view_count */,
    PEXViewEntry *		/* views */,
    int *		/* view_return */,
    int *		/* count_return */,
    PEXCoord *		/* points_return */
#endif
);

extern int PEXMapXCToNPC2D(
#if NeedFunctionPrototypes
    int 		/* point_count */,
    PEXDeviceCoord2D *		/* points */,
    unsigned int 		/* window_height */,
    PEXDeviceCoord2D *		/* viewport */,
    PEXNPCSubVolume *		/* npc_sub_volume */,
    int 		/* view_count */,
    PEXViewEntry *		/* views */,
    int *		/* view_return */,
    int *		/* count_return */,
    PEXCoord2D *		/* points_return */
#endif
);

extern void PEXMarkers(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* count */,
    PEXCoord *		/* points */
#endif
);

extern void PEXMarkers2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* count */,
    PEXCoord2D *		/* points */
#endif
);

extern Status PEXMatchRenderingTargets(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    int 		/* depth */,
    int 		/* type */,
    Visual *		/* visual */,
    unsigned long 		/* max_targets */,
    unsigned long *		/* count_return */,
    PEXRenderingTarget **		/* targets_return */
#endif
);

extern void PEXMatrixMult(
#if NeedFunctionPrototypes
    PEXMatrix 		/* matrix1 */,
    PEXMatrix 		/* matrix2 */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXMatrixMult2D(
#if NeedFunctionPrototypes
    PEXMatrix3x3 		/* matrix1 */,
    PEXMatrix3x3 		/* matrix2 */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern int PEXNPCToXCTransform(
#if NeedFunctionPrototypes
    PEXNPCSubVolume *		/* npc_sub_volume */,
    PEXDeviceCoord *		/* viewport */,
    unsigned int 		/* window_height */,
    PEXMatrix 		/* transform_return */
#endif
);

extern int PEXNPCToXCTransform2D(
#if NeedFunctionPrototypes
    PEXNPCSubVolume *		/* npc_sub_volume */,
    PEXDeviceCoord2D *		/* viewport */,
    unsigned int 		/* window_height */,
    PEXMatrix3x3 		/* transform_return */
#endif
);

extern void PEXNURBCurve(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* rationality */,
    int 		/* order */,
    float *		/* knots */,
    unsigned int 		/* count */,
    PEXArrayOfCoord 		/* points */,
    double 		/* tmin */,
    double 		/* tmax */
#endif
);

extern void PEXNURBSurface(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* rationality */,
    int 		/* uorder */,
    int 		/* vorder */,
    float *		/* uknots */,
    float *		/* vknots */,
    unsigned int 		/* col_count */,
    unsigned int 		/* row_count */,
    PEXArrayOfCoord 		/* points */,
    unsigned int 		/* curve_count */,
    PEXListOfTrimCurve *		/* trim_curves */
#endif
);

extern void PEXNoop(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */
#endif
);

extern int PEXNormalizeVectors(
#if NeedFunctionPrototypes
    int 		/* count */,
    PEXVector *		/* vectors */,
    PEXVector *		/* vectors_return */
#endif
);

extern int PEXNormalizeVectors2D(
#if NeedFunctionPrototypes
    int 		/* count */,
    PEXVector2D *		/* vectors */,
    PEXVector2D *		/* vectors_return */
#endif
);

extern int PEXOrthoProjMatrix(
#if NeedFunctionPrototypes
    double 		/* height */,
    double 		/* aspect */,
    double 		/* near */,
    double 		/* far */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern int PEXPerspProjMatrix(
#if NeedFunctionPrototypes
    double 		/* fovy */,
    double 		/* distance */,
    double 		/* aspect */,
    double 		/* near */,
    double 		/* far */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern PEXPickPath *PEXPickAll(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXRenderer 		/* renderer */,
    int 		/* method */,
    int 		/* max_hits */,
    int 		/* pick_device_type */,
    PEXPickRecord *		/* pick_record */,
    int *		/* status_return */,
    int *		/* more_return */,
    unsigned long *		/* count_return */
#endif
);

extern PEXPickPath *PEXPickOne(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXRenderer 		/* renderer */,
    PEXStructure 		/* structure */,
    int 		/* method */,
    int 		/* pick_device_type */,
    PEXPickRecord *		/* pick_record */,
    int *		/* status_return */,
    int *		/* undetectable_return */
#endif
);

extern int PEXPolarViewMatrix(
#if NeedFunctionPrototypes
    PEXCoord *		/* from */,
    double 		/* distance */,
    double 		/* azimuth */,
    double 		/* altitude */,
    double 		/* twist */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXPolyline(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* count */,
    PEXCoord *		/* points */
#endif
);

extern void PEXPolyline2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* count */,
    PEXCoord2D *		/* points */
#endif
);

extern void PEXPolylineSetWithData(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    unsigned int 		/* count */,
    PEXListOfVertex *		/* vertex_lists */
#endif
);

extern void PEXPostStructure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    PEXStructure 		/* structure */,
    double 		/* priority */
#endif
);

extern void PEXQuadrilateralMesh(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    PEXArrayOfFacetData 		/* facet_data */,
    unsigned int 		/* col_count */,
    unsigned int 		/* row_count */,
    PEXArrayOfVertex 		/* vertices */
#endif
);

extern PEXTextExtent *PEXQueryEncodedTextExtents(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    unsigned int 		/* font_table_index */,
    int 		/* path */,
    double 		/* expansion */,
    double 		/* spacing */,
    double 		/* height */,
    int 		/* halign */,
    int 		/* valign */,
    unsigned long 		/* count */,
    PEXListOfEncodedText *		/* encoded_text */
#endif
);

extern PEXFontInfo *PEXQueryFont(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXFont 		/* font */
#endif
);

extern PEXTextExtent *PEXQueryTextExtents(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    unsigned int 		/* font_table_index */,
    int 		/* path */,
    double 		/* expansion */,
    double 		/* spacing */,
    double 		/* height */,
    int 		/* halign */,
    int 		/* valign */,
    unsigned long 		/* count */,
    PEXStringData *		/* text */
#endif
);

extern void PEXRedrawAllStructures(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */
#endif
);

extern void PEXRedrawClipRegion(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned long 		/* count */,
    PEXDeviceRect *		/* clip_rectangles */
#endif
);

extern void PEXRemoveFromNameSet(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned long 		/* count */,
    PEXName *		/* names */
#endif
);

extern void PEXRenderElements(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    PEXStructure 		/* structure */,
    int 		/* whence1 */,
    long 		/* offset1 */,
    int 		/* whence2 */,
    long 		/* offset2 */
#endif
);

extern void PEXRenderNetwork(
#if NeedFunctionPrototypes
    Display *		/* display */,
    Drawable 		/* drawable */,
    PEXRenderer 		/* renderer */,
    PEXStructure 		/* structure */
#endif
);

extern void PEXRestoreModelClipVolume(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */
#endif
);

extern int PEXRotate(
#if NeedFunctionPrototypes
    int 		/* axis */,
    double 		/* angle */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXRotate2D(
#if NeedFunctionPrototypes
    double 		/* angle */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern int PEXRotateGeneral(
#if NeedFunctionPrototypes
    PEXCoord *		/* point1 */,
    PEXCoord *		/* point2 */,
    double 		/* angle */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXScale(
#if NeedFunctionPrototypes
    PEXVector *		/* scale_vector */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXScale2D(
#if NeedFunctionPrototypes
    PEXVector2D *		/* scale_vector */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern Status PEXSearchNetwork(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXSearchContext 		/* context */,
    PEXStructurePath **		/* path_return */
#endif
);

extern void PEXSendOCs(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* float_format */,
    unsigned long 		/* oc_count */,
    unsigned int 		/* length */,
    char *		/* encoded_ocs */
#endif
);

extern void PEXSetATextAlignment(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* halignment */,
    int 		/* valignment */
#endif
);

extern void PEXSetATextHeight(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* height */
#endif
);

extern void PEXSetATextPath(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* path */
#endif
);

extern void PEXSetATextStyle(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* style */
#endif
);

extern void PEXSetATextUpVector(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXVector2D *		/* vector */
#endif
);

extern void PEXSetBFInteriorStyle(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* style */
#endif
);

extern void PEXSetBFInteriorStyleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* index */
#endif
);

extern void PEXSetBFReflectionAttributes(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXReflectionAttributes *		/* attributes */
#endif
);

extern void PEXSetBFReflectionModel(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* model */
#endif
);

extern void PEXSetBFSurfaceColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetBFSurfaceColorIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetBFSurfaceInterpMethod(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* method */
#endif
);

extern void PEXSetCharExpansion(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* expansion */
#endif
);

extern void PEXSetCharHeight(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* height */
#endif
);

extern void PEXSetCharSpacing(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* spacing */
#endif
);

extern void PEXSetCharUpVector(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXVector2D *		/* vector */
#endif
);

extern void PEXSetColorApproxIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetCurveApprox(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* method */,
    double 		/* tolerance */
#endif
);

extern void PEXSetDepthCueIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetEchoColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXRenderer 		/* renderer */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetEdgeBundleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetEditingMode(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* mode */
#endif
);

extern void PEXSetElementPtr(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    int 		/* whence */,
    long 		/* offset */
#endif
);

extern void PEXSetElementPtrAtLabel(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXStructure 		/* structure */,
    long 		/* label */,
    long 		/* offset */
#endif
);

extern void PEXSetFacetCullingMode(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* mode */
#endif
);

extern void PEXSetFacetDistinguishFlag(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* flag */
#endif
);

extern void PEXSetGlobalTransform(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXMatrix 		/* transform */
#endif
);

extern void PEXSetGlobalTransform2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXMatrix3x3 		/* transform */
#endif
);

extern void PEXSetHLHSRID(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned long 		/* hlhsr_id */
#endif
);

extern void PEXSetIndividualASF(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned long 		/* attribute */,
    int 		/* asf */
#endif
);

extern void PEXSetInteriorBundleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetInteriorStyle(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* style */
#endif
);

extern void PEXSetInteriorStyleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* index */
#endif
);

extern void PEXSetLightSourceState(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* enable_count */,
    PEXTableIndex *		/* enable */,
    unsigned int 		/* disable_count */,
    PEXTableIndex *		/* disable */
#endif
);

extern void PEXSetLineBundleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetLineColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetLineColorIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetLineType(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* line_type */
#endif
);

extern void PEXSetLineWidth(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* width */
#endif
);

extern void PEXSetLocalTransform(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* composition */,
    PEXMatrix 		/* transform */
#endif
);

extern void PEXSetLocalTransform2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* composition */,
    PEXMatrix3x3 		/* transform */
#endif
);

extern void PEXSetMarkerBundleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetMarkerColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetMarkerColorIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetMarkerScale(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* scale */
#endif
);

extern void PEXSetMarkerType(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* marker_type */
#endif
);

extern void PEXSetModelClipFlag(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* flag */
#endif
);

extern void PEXSetModelClipVolume(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* op */,
    unsigned int 		/* count */,
    PEXHalfSpace *		/* half_spaces */
#endif
);

extern void PEXSetModelClipVolume2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* op */,
    unsigned int 		/* count */,
    PEXHalfSpace2D *		/* half_spaces */
#endif
);

extern void PEXSetOfFillAreaSets(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* shape_hint */,
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    unsigned int 		/* edge_attributes */,
    int 		/* contour_hint */,
    int 		/* contours_all_one */,
    int 		/* color_type */,
    unsigned int 		/* set_count */,
    PEXArrayOfFacetData 		/* facet_data */,
    unsigned int 		/* vertex_count */,
    PEXArrayOfVertex 		/* vertices */,
    unsigned int 		/* index_count */,
    PEXSwitch *		/* edge_flags */,
    PEXConnectivityData *		/* connectivity */
#endif
);

extern void PEXSetParaSurfCharacteristics(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* psc_type */,
    PEXPSCData *		/* characteristics */
#endif
);

extern void PEXSetPatternAttributes(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* ref_point */,
    PEXVector *		/* vector1 */,
    PEXVector *		/* vector2 */
#endif
);

extern void PEXSetPatternAttributes2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord2D *		/* ref_point */
#endif
);

extern void PEXSetPatternSize(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* width */,
    double 		/* height */
#endif
);

extern void PEXSetPickID(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned long 		/* pick_id */
#endif
);

extern void PEXSetPolylineInterpMethod(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* method */
#endif
);

extern void PEXSetReflectionAttributes(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXReflectionAttributes *		/* attributes */
#endif
);

extern void PEXSetReflectionModel(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* model */
#endif
);

extern void PEXSetRenderingColorModel(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* model */
#endif
);

extern void PEXSetSurfaceApprox(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* method */,
    double 		/* utolerance */,
    double 		/* vtolerance */
#endif
);

extern void PEXSetSurfaceColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetSurfaceColorIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetSurfaceEdgeColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetSurfaceEdgeColorIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetSurfaceEdgeFlag(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* flag */
#endif
);

extern void PEXSetSurfaceEdgeType(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* edge_type */
#endif
);

extern void PEXSetSurfaceEdgeWidth(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    double 		/* width */
#endif
);

extern void PEXSetSurfaceInterpMethod(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* method */
#endif
);

extern void PEXSetTableEntries(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXLookupTable 		/* table */,
    unsigned int 		/* start */,
    unsigned int 		/* count */,
    int 		/* table_type */,
    PEXPointer 		/* entries */
#endif
);

extern void PEXSetTextAlignment(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* halignment */,
    int 		/* valignment */
#endif
);

extern void PEXSetTextBundleIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetTextColor(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* color_type */,
    PEXColor *		/* color */
#endif
);

extern void PEXSetTextColorIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetTextFontIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetTextPath(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* path */
#endif
);

extern void PEXSetTextPrecision(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* precision */
#endif
);

extern void PEXSetViewIndex(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* index */
#endif
);

extern void PEXSetWorkstationBufferMode(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    int 		/* buffer_mode */
#endif
);

extern void PEXSetWorkstationDisplayUpdateMode(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    int 		/* update_mode */
#endif
);

extern void PEXSetWorkstationHLHSRMode(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    int 		/* hlhsr_mode */
#endif
);

extern void PEXSetWorkstationViewPriority(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned int 		/* index1 */,
    unsigned int 		/* index2 */,
    int 		/* priority */
#endif
);

extern void PEXSetWorkstationViewRep(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    unsigned int 		/* view_index */,
    PEXViewEntry *		/* values */
#endif
);

extern void PEXSetWorkstationViewport(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    PEXViewport *		/* viewport */
#endif
);

extern void PEXSetWorkstationWindow(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    PEXNPCSubVolume *		/* workstation_window */
#endif
);

extern Status PEXStartOCs(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    int 		/* float_format */,
    int 		/* oc_count */,
    int 		/* word_count */
#endif
);

extern void PEXText(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord *		/* origin */,
    PEXVector *		/* vector1 */,
    PEXVector *		/* vector2 */,
    int 		/* length */,
    char *		/* string */
#endif
);

extern void PEXText2D(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    PEXCoord2D *		/* origin */,
    int 		/* length */,
    char *		/* string */
#endif
);

extern int PEXTransformPoints(
#if NeedFunctionPrototypes
    PEXMatrix 		/* transform */,
    int 		/* count */,
    PEXCoord *		/* points */,
    PEXCoord *		/* points_return */
#endif
);

extern int PEXTransformPoints2D(
#if NeedFunctionPrototypes
    PEXMatrix3x3 		/* transform */,
    int 		/* count */,
    PEXCoord2D *		/* points */,
    PEXCoord2D *		/* points_return */
#endif
);

extern void PEXTransformPoints2DH(
#if NeedFunctionPrototypes
    PEXMatrix3x3 		/* transform */,
    int 		/* count */,
    PEXCoord *		/* points */,
    PEXCoord *		/* points_return */
#endif
);

extern void PEXTransformPoints4D(
#if NeedFunctionPrototypes
    PEXMatrix 		/* transform */,
    int 		/* count */,
    PEXCoord4D *		/* points */,
    PEXCoord4D *		/* points_return */
#endif
);

extern void PEXTransformVectors(
#if NeedFunctionPrototypes
    PEXMatrix 		/* transform */,
    int 		/* count */,
    PEXVector *		/* vectors */,
    PEXVector *		/* vectors_return */
#endif
);

extern void PEXTransformVectors2D(
#if NeedFunctionPrototypes
    PEXMatrix3x3 		/* transform */,
    int 		/* count */,
    PEXVector2D *		/* vectors */,
    PEXVector2D *		/* vectors_return */
#endif
);

extern void PEXTranslate(
#if NeedFunctionPrototypes
    PEXVector *		/* trans_vector */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern void PEXTranslate2D(
#if NeedFunctionPrototypes
    PEXVector2D *		/* trans_vector */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern void PEXTriangleStrip(
#if NeedFunctionPrototypes
    Display *		/* display */,
    XID 		/* resource_id */,
    PEXOCRequestType 		/* req_type */,
    unsigned int 		/* facet_attributes */,
    unsigned int 		/* vertex_attributes */,
    int 		/* color_type */,
    PEXArrayOfFacetData 		/* facet_data */,
    unsigned int 		/* count */,
    PEXArrayOfVertex 		/* vertices */
#endif
);

extern void PEXUnloadFont(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXFont 		/* font */
#endif
);

extern void PEXUnpostAllStructures(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */
#endif
);

extern void PEXUnpostStructure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */,
    PEXStructure 		/* structure */
#endif
);

extern void PEXUpdatePickMeasure(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXPickMeasure 		/* pick_measure */,
    int 		/* pick_device_type */,
    PEXPickRecord *		/* pick_record */
#endif
);

extern void PEXUpdateWorkstation(
#if NeedFunctionPrototypes
    Display *		/* display */,
    PEXWorkstation 		/* workstation */
#endif
);

extern int PEXViewMappingMatrix(
#if NeedFunctionPrototypes
    PEXCoord2D *		/* frame */,
    PEXNPCSubVolume *		/* viewport */,
    int 		/* perspective */,
    PEXCoord *		/* prp */,
    double 		/* view_plane */,
    double 		/* back_plane */,
    double 		/* front_plane */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern int PEXViewMappingMatrix2D(
#if NeedFunctionPrototypes
    PEXCoord2D *		/* frame */,
    PEXCoord2D *		/* viewport */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern int PEXViewOrientationMatrix(
#if NeedFunctionPrototypes
    PEXCoord *		/* vrp */,
    PEXVector *		/* vpn */,
    PEXVector *		/* vup */,
    PEXMatrix 		/* matrix_return */
#endif
);

extern int PEXViewOrientationMatrix2D(
#if NeedFunctionPrototypes
    PEXCoord2D *		/* vrp */,
    PEXVector2D *		/* vup */,
    PEXMatrix3x3 		/* matrix_return */
#endif
);

extern int PEXXCToNPCTransform(
#if NeedFunctionPrototypes
    PEXNPCSubVolume *		/* npc_sub_volume */,
    PEXDeviceCoord *		/* viewport */,
    unsigned int 		/* window_height */,
    PEXMatrix 		/* transform_return */
#endif
);

extern int PEXXCToNPCTransform2D(
#if NeedFunctionPrototypes
    PEXNPCSubVolume *		/* npc_sub_volume */,
    PEXDeviceCoord2D *		/* viewport */,
    unsigned int 		/* window_height */,
    PEXMatrix3x3 		/* transform_return */
#endif
);

extern unsigned long PEXCountOCs(
#if NeedFunctionPrototypes
    int 		/* float_format */,
    unsigned long 		/* length */,
    char *		/* encoded_ocs */
#endif
);



#ifdef __cplusplus
}                                               /* for C++ V2.0 */
#endif

#endif /* _PEXLIB_H_ */
