/* $XConsortium: phigserr.h,v 5.2 91/07/12 20:22:21 hersh Exp $ */

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

/* Implementation Dependent Errors */
#define        PE_NOT_SUPP  -500	/* Ignoring function, this function
					   is not supported */
#define        PE_EL_TOO_LARGE  -501	/* Ignoring function, the element
					   is too large */

/* PEX/PHIGS errors */
#define        PE_PEX_NO_XSRVR  -200	/* Ignoring function, cannot connect
					   to the designated or default
					   server */
#define        PE_PEX_NO_PEX    -201	/* Ignoring function, the specified
					   or default X server does not
					   support a compatible PEX extension */
#define        PE_PEX_ALLOC    -202		/* Ignoring function, an X
					   allocation error has occurred */
#define        PE_PEX_NO_WIN_CREATE	-203	/* Ignoring function, can't create
					   an X window */
#define        PE_PEX_NO_SHELL_CREATE	-204	/* Ignoring function, can't create
					   an Xt shell */
/* TODO: These are only defined until the correct mapping can be determined. */
#define        PE_PEX_CTE    -250	/* PEX colour type error */
#define        PE_PEX_RSE    -251	/* PEX rendering state error */
#define        PE_PEX_FPFE   -252	/* PEX floating point format error */
#define        PE_PEX_LE     -253	/* PEX label error */
#define        PE_PEX_LTE    -254	/* PEX lookup table error */
#define        PE_PEX_NSE    -255	/* PEX name set error */
#define        PE_PEX_PE     -256	/* PEX path error */
#define        PE_PEX_FE     -257	/* PEX font error */
#define        PE_PEX_PWE    -258	/* PEX phigs workstation error */
#define        PE_PEX_PME    -259	/* PEX pick measure error */
#define        PE_PEX_PCE    -260	/* PEX pipeline context error */
#define        PE_PEX_RE     -261	/* PEX renderer error */
#define        PE_PEX_SCE    -262	/* PEX search context error */
#define        PE_PEX_SE     -263	/* PEX structure error */
#define        PE_PEX_OCE    -264	/* PEX output command error */
/* X Errors */
#define		PE_X_BAD_REQUEST	-301	/*X Bad Request Error */
#define		PE_X_BAD_VALUE	-302	/*X Bad Value Error */
#define		PE_X_BAD_WINDOW	-303	/*X Bad Window Error */
#define		PE_X_BAD_PIXMAP	-304	/*X Bad Pixmap Error */
#define		PE_X_BAD_ATOM	-305	/*X Bad Atom Error */
#define		PE_X_BAD_CURSOR	-306	/*X Bad Cursor Error */
#define		PE_X_BAD_FONT	-307	/*X Bad Font Error */
#define		PE_X_BAD_MATCH	-308	/*X Bad Match Error */
#define		PE_X_BAD_DRAWABLE	-309	/*X Bad Drawable Error */
#define		PE_X_BAD_ACCESS	-310	/*X Bad Access Error */
#define		PE_X_BAD_ALLOC	-311	/*X Bad Alloc Error */
#define		PE_X_BAD_COLOR	-312	/*X Bad Colour Error */
#define		PE_X_BAD_GC		-313	/*X Bad GC Error */
#define		PE_X_BAD_ID_CHOICE	-314	/*X Bad ID Choice Error */
#define		PE_X_BAD_NAME	-315	/*X Bad Name Error */
#define		PE_X_BAD_LENGTH	-316	/*X Bad Length Error */
#define		PE_X_BAD_IMPL	-317	/*X Bad Implementation Error */

    /* Miscellaneous*/

#define		PE_BAD_DC_VAL  -171 /*Ignoring function, the specified DC
				    limits are less than zero */
#define		PE_NO_DC_SET  -170 /*Ignoring function, DC limits cannot be
				    set for this workstation */
#define		PE_X_NO_CMAP  -169 /*Ignoring function, cannot create an X 
			          colormap resource */
#define		PE_X_BAD_VISUAL   -168 /*Ignoring function, cannot open workstation
				  on a read-only visual with no predefined
				  colormap properties */
#define		PE_BAD_NUM_VTX_LT_0  -164	/*Ignoring function, the specified number of
				  vertices or sets of vertices is less than 
				  zero */
#define		PE_BAD_EDGE_FLAG_INFO  -163	/*Ignoring function, the
				  specified edge data flag is invalid*/
#define		PE_BAD_VERT_FLAG  -162	/*Ignoring function, the specified vertex
				  flag is invalid*/
#define		PE_BAD_FACET_FLAG  -161	/*Ignoring function, the specified facet
				  flag is invalid*/
#define		PE_NO_FUNC   -160  /*Ignoring function, the specified 
                                  function is not available on the speci-
                                  fied workstation */
#define		PE_NO_INFO  -159  /*Ignoring function, the requested
				  information is not available */
#define		PE_NO_FONT_CS -156 /*Ignoring Function, specified font is not 
				  available for character set */
#define		PE_BAD_CHAR_SET -155 /*Specified character set is invalid */
#define		PE_LENGTH_LT_0 -153	/*List length is less than zero -- zero will
				  be used */
#define		PE_NOT_IMPL  -152	/*Ignoring function, not implemented */
#define		PE_BAD_NAME  -151	/*Ignoring function, nameset or filter
				  contains name outside supported range */
#define		PE_NUM_PTS_LT_0  -150	/*Ignoring function, the specified number of
				  points or sets of points is less than zero */
    /* Workstation Configuration */
#define	       PE_WST_BOUND -100	/*Ignoring function, workstation type is
				  a default type or bound to a workstation
				  and cannot be modified */
    /* Unusable Environment */
#define        PE_NO_SHMEM    -57	  /*Kernel not configured with shared-memory IPC
				    facility needed for PEX SI communication */
#define        PE_NO_FONT   -55	  /*Ignoring function, cannot open PHIGS,
				    cannot open font files */
#define        PE_NO_FILE -54	  /*Ignoring function, cannot locate SI
				    support file */
#define        PE_BAD_FILE_PATH -53	  /*Ignoring function, SI support file
				    path invalid */
#define        PE_PATH_TOO_LONG -52   /*Ignoring function, PEXAPIDIR path is too
				     long */
#define        PE_NO_SRVR_FILE -51	   /*Ignoring function, cannot open PHIGS,
				     cannot locate SI file "phigsmon" */
#define	       PE_COMM     -50	   /*Communication error */
    /* Expended or Failing System Resources */
#define	       PE_NO_TRAV_MEM -6	   /*Could not allocate additional dynamic
				     memory during structure traversal */
#define        PE_EXEC     -2	   /*Ignoring function, cannot open PHIGS,
				     cannot create server */
#define        PE_COMM_CREAT -1	   /*Ignoring function, cannot open PHIGS,
				    cannot create communication channel */

#define        PE_NO_ERROR   0       /*No Error */
#define        PE_NOT_PHCL     1       /*Ignoring function, function requires
                                     state (PHCL,WSCL,STCL,ARCL) */
#define        PE_NOT_PHOP   2       /*Ignoring function, function requires
                                     state (PHOP,*,*,*) */
#define        PE_NOT_WSOP   3       /*Ignoring function, function requires
                                     state (PHOP,WSOP,*,*) */
#define        PE_NOT_CL 4       /*Ignoring function, function requires
                                     state (PHOP,WSCL,STCL,ARCL) */
#define        PE_NOT_STOP   5       /*Ignoring function, function requires
                                     state (PHOP,*,STOP,*) */
#define        PE_NOT_STCL   6      /*Ignoring function, function requires
                                     state (PHOP,*,STCL,*) */
#define        PE_NOT_AROP   7      /*Ignoring function, function requires
                                     state (PHOP,*,*,AROP) */
#define        PE_BAD_CONN_ID   50      /*Ignoring function, connection identi-
                                     fier not recognized by the implementa-
                                     tion */
#define        PE_WS_TYPE  51      /*Ignoring function, this information is
                                     not yet available for this generic
				     workstation type; open a workstation of
				     this type and use the specific
				     workstation type */
#define        PE_BAD_WS_TYPE 52      /*Ignoring function, workstation type
                                     not recognized by the implementation
                                     */
#define        PE_DUP_WS_ID 53      /*Ignoring function, workstation iden-
                                     tifier already is in use */
#define        PE_WS_NOT_OPEN   54      /*Ignoring function, the specified
                                     workstation is not open */
#define        PE_NO_OPEN_WS    55      /*Ignoring function, workstation cannot
                                     be opened for an implementation depen-
                                     dent reason */
#define        PE_WS_NOT_MO   56      /*Ignoring function, specified
                                     workstation is not of category MO */
#define        PE_WS_MI   57      /*Ignoring function, specified worksta-
                                     tion is of category MI */
#define        PE_WS_NOT_MI   58      /*Ignoring function, specified
                                     workstation is not of category MI */
#define        PE_WS_NO_OUTPUT  59      /*Ignoring function, the specified
                                     workstation does not have output capa-
                                     bility (i.e., the workstation category
                                     is neither OUTPUT, OUTIN, nor MO) */
#define        PE_WS_NOT_OUTIN   60      /*Ignoring function, specified worksta-
                                     tion is not of category OUTIN */
#define        PE_WS_NO_INPUT   61      /*Ignoring function, specified worksta-
                                     tion is neither of category INPUT nor
                                     of category OUTIN */
#define        PE_WS_NOT_OUT   62      /*Ignoring function, this information is
                                     not available for this MO workstation
				     type */
#define        PE_MAX_WS  63      /*Ignoring function, opening this
                                     workstation would exceed the maximum
                                     number of simultaneously open
                                     workstations */
#define        PE_NO_GDP   64      /*Ignoring function, the specified
                                     workstation type is not able to gen-
                                     erate the specified generalized draw-
                                     ing primitive */
#define        PE_BUN_INDX_LT_1   100      /*Ignoring function, the bundle index
                                     value is less than one */
#define        PE_REP_UNDEF     101      /*The specified representation has not
                                     been defined */
#define        PE_REP_NOT_PREDEF  102      /*Ignoring function, the specified
                                     representation has not be predefined
                                     on this workstation */
#define        PE_MAX_BUN  103      /*Ignoring function, setting this bundle
                                     table entry would exceed the maximum
                                     number of entries allowed in the
                                     workstation bundle table */
#define        PE_BAD_LINETYPE   104      /*Ignoring function, the specified line-
                                     type is not available on the specified
                                     workstation */
#define        PE_BAD_MARKERTYPE   105      /*Ignoring function, the specified mark-
                                     er type is not available on the speci-
                                     fied workstation */
#define        PE_BAD_FONT   106      /*Ignoring function, the specified font
                                     is not available for the requested
                                     text precision on the specified
                                     workstation */
#define        PE_BAD_EDGETYPE   107      /*Ignoring function, the specified edge-
                                     type is not available on the specified
                                     workstation */
#define        PE_BAD_INT_STYLE   108      /*Ignoring function, the specified
                                     interior style is not available on the
                                     workstation */
#define        PE_NO_PAT     109      /*Ignoring function, interior style PAT-
                                     TERN  is not supported on the worksta-
                                     tion */
#define        PE_BAD_COLR_MODEL   110      /*Ignoring function, the specified
				     colour model is not available on the
				     workstation. */
#define        PE_BAD_HLHSR_MODE   111      /*Ignoring function, the specified HLHSR
                                     mode is not available on the specified
                                     workstation */
#define        PE_PAT_INDX_LT_1   112      /*Ignoring function, the pattern index
                                     value is less than one */
#define        PE_COLR_INDX_LT_0    113      /*Ignoring function, the colour index
                                     value is less than zero */
#define        PE_VIEW_INDX_LT_0    114     /*Ignoring function, the view index
                                     value is less than zero */
#define        PE_VIEW_INDX_LT_1    115     /*Ignoring function, the view index
                                     value is less than one */
#define        PE_BAD_PAT_DIM    116      /*Ignoring function, one of the dimen-
                                     sions of pattern colour array is less
                                     than one */
#define        PE_BAD_COLR_DIM     117     /*Ignoring function, one of the dimen-
                                     sions of the colour index array is
                                     less than zero */
#define        PE_BAD_COLR   118      /*Ignoring function, one of the com-
                                     ponents of the colour specification is
                                     out of range.  The valid range is
                                     dependent upon the current colour
                                     model */
#define        PE_DCUE_INDX_LT_0   119     /*Ignoring function, depth cue index
				     is less than zero */
#define        PE_DCUE_INDX_LT_1  120     /*Ignoring function, depth cue index
				     is less than one */
#define        PE_COLRMAP_INDX_LT_0    121     /*Ignoring function, the colour mapping
				     index is less than zero */
#define        PE_BAD_LINE_SHADE 122    /*Ignoring function, the specified poly-
                                     line shading method is not available
				     on the workstation */
#define        PE_BAD_INT_SHADE 123    /*Ignoring function, the specified interior
                                     shading method is not available
				     on the workstation */
#define        PE_BAD_REF_EQN 124      /*Ignoring function, the specified interior
                                     reflectance equation is not available
				     on the workstation */
#define        PE_BAD_COLRMAP_RANGE 125    /*Ignoring function, the total of the
				     colour range fields in all the table
				     entries is too large  */
#define        PE_BAD_COLRMAP_METHOD 126    /*Ignoring function, the specified colour
                                     mapping method is not available
				     on the specified workstation */
#define        PE_LTSRC_INDX_LT_1 129  /*Ignoring function, the light source index
				     is less than 1 */
#define        PE_BAD_REF_PLANES  130     /*Ignoring function, invalid reference
                                     planes; DQMIN > DQMAX */
#define        PE_BAD_LTSRCTYPE  131  /*Ignoring function, the specified light
				     source type is not available on the 
				     workstation */
#define        PE_BAD_LTANGLE     132  /*Ignoring function, the specified spot
				     light spread angle is out of range */
#define	       PE_ENTRY_LT_1  133    /*Ignoring function, one of the entries
				     in the activation list or the
				     deactivation list is less than 1 */
#define	       PE_NOT_INDIRECT  134    /*Ignoring function, the requested
				       entry contains a general colour
				       specification with colour type other
				       than INDIRECT */
#define	       PE_DUP_ENTRY   135    /*Ignoring function, the same entry
                                     exists in both the activation and the
				     deactivation list */
#define	       PE_BAD_COLR_RANGE   136    /*Ignoring function, one of the
				      components of the colour specification
				      is out of range. */
#define	       PE_BAD_DATA   138    /*Ignoring function, one or more fields
				      in the specified data record is
				      inconsistent */
#define        PE_MAX_VIEW   150     /*Ignoring function, setting this view
                                     table entry would exceed the maximum
                                     number of entries allowed in the
                                     workstation's view table */
#define        PE_INVALID_WINDOW    151     /*Ignoring function, invalid window;
                                     XMIN >= XMAX, YMIN >= YMAX, or
				     ZMIN > ZMAX */
#define        PE_INVALID_VIEWPORT     152     /*Ignoring function, invalid viewport;
                                     XMIN >= XMAX, YMIN >= YMAX, or
				     ZMIN > ZMAX */
#define        PE_INVALID_CLIP    153     /*Ignoring function, invalid view clipping
				     limits; XMIN >= XMAX, YMIN >= YMAX, or
				     ZMIN > ZMAX */
#define        PE_BAD_CLIP   154     /*Ignoring function, the view clipping
				     limits are not within NPC range */
#define        PE_BAD_PROJ_VIEWPORT  155     /*Ignoring function, the projection
                                     viewport limits are not within NPC range */
#define        PE_BAD_WS_WINDOW    156     /*Ignoring function, the workstation
                                     window limits are not within NPC range */
#define        PE_BAD_WS_VIEWPORT     157     /*Ignoring function, the workstation
                                     viewport is not within display space */
#define        PE_BAD_PLANES    158     /*Ignoring function, front plane and back
                                     plane distances are equal when z-extent
				     of the projection viewport is zero */


#define        PE_BAD_VPN    159      /*Ignoring function, the view plane nor-
                                     mal vector has length zero */
#define        PE_BAD_VUP    160      /*Ignoring function, the view up vector
                                     has length zero */
#define        PE_BAD_VUP_VPN   161      /*Ignoring function, the view up and
                                     view plane normal vectors are parallel
                                     thus the viewing coordinate system
                                     cannot be established */
#define        PE_BAD_PRP    162     /*Ignoring function, the projection
                                     reference point is between the front
                                     and back planes */
#define        PE_PRP_VIEW_PLANE     163     /*Ignoring function, the projection
                                     reference point cannot be positioned
                                     on the view plane */
#define        PE_FRONT_BACK   164     /*Ignoring function, the back
                                     plane is in front of the front plane */
#define        PE_IGNORE_STRUCTS 200     /*Warning, ignoring structures that do
                                     not exist */
#define        PE_BAD_STRUCT  201     /*Ignoring function, the specified
                                     structure does not exist */
#define        PE_BAD_ELEMENT    202     /*Ignoring function, the specified ele-
                                     ment does not exist */
#define        PE_BAD_PATH  203     /*Ignoring function, specified starting
                                     path not found in CSS */
#define        PE_BAD_CEILING_INDX   204     /*Ignoring function, specified search
                                     ceiling index out of range */
#define        PE_NO_LABEL   205     /*Ignoring function, the label does not
                                     exist in the open structure between
                                     the element pointer and the end of the
                                     structure */
#define        PE_NO_LABELS  206     /*Ignoring function, one or both of the
                                     labels does not exist in the open
                                     structure between the element pointer
                                     and the end of the structure */
#define	       PE_BAD_PATH_DEPTH 207    /*Ignoring function, the specified path
                                     depth is less than zero (0) */
#define        PE_BAD_DIPS_PRI 208     /*Ignoring function, the display priority
				     is out of range */
#define        PE_NO_DEVICE   250     /*Ignoring function, the specified
                                     device is not available on the specified
                                     workstation */
#define        PE_NOT_REQUEST 251    /*Ignoring function, the function re-
                                     quires the input device to be in RE-
                                     QUEST mode */
#define        PE_NOT_SAMPLE 252     /*Ignoring function, the function re-
                                     quires the input device to be in SAM-
                                     PLE Mode */
#define        PE_BAD_PET    253     /*Warning, the specified prompt/echo
                                     type is not available on the specified
                                     workstation.  Prompt/echo type one
                                     will be used in its place */
#define        PE_INVALID_ECHO   254     /*Ignoring function, invalid echo
				     area/volume; XMIN >= XMAX, YMIN >= YMAX,
				     or ZMIN > ZMAX */
#define        PE_BAD_ECHO   255     /*Ignoring function, one of the echo
				     area/volume boundary points is
				     outside the range of the device */
#define        PE_QUEUE_OFLOW   256     /*Warning, the input queue has over-
                                     flowed */
#define        PE_NO_QUEUE_OFLOW   257     /*Ignoring function, input queue has not
                                     overflowed */
#define        PE_OFLOW_NO_GO 258   /*Warning, input queue has over-
                                     flowed, but associated workstation has
				     been closed */
#define        PE_BAD_CLASS  259     /*Ignoring function, the input device
                                     class of the current input report does
                                     not match the class being requested */
#define        PE_BAD_DATA_REC   260     /*Ignoring function, one of the fields
                                     within the input device data record is
                                     in error */
#define	       PE_INVALID_VALUE   261     /*Ignoring function, initial value is
				     invalid */
#define	       PE_STROKE_BUF_SIZE  262     /*Ignoring function, number of points in
				     the initial stroke is greater than the
				     buffer size */
#define	       PE_STRING_BUF_SIZE  263     /*Ignoring function, length of the initial
				     string is greater than the buffer size */
#define        PE_ILLEGAL_ITEM_TYPE   300     /*Ignoring function, item type is not
                                     allowed for user items */
#define        PE_INVALID_ITEM_LEN   301     /*Ignoring function, item length is in-
                                     valid */
#define        PE_METAFILE_EMPTY    302     /*Ignoring function, no item is left in
                                     metafile input */
#define        PE_INVALID_ITEM    303     /*Ignoring function, metafile item is
                                     invalid */
#define        PE_BAD_ITEM_TYPE    304     /*Ignoring function, item type is
                                     unknown */
#define        PE_BAD_ITEM_REC   305     /*Ignoring function, content of item
                                     data record is invalid for the speci-
                                     fied item type */
#define        PE_MAX_ITEM_LEN    306     /*Ignoring function, maximum item data
                                     record length is invalid */
#define        PE_USER_ITEM   307     /*Ignoring function, user item cannot be
                                     interpreted */
#define        PE_ESC_NOT_AVAIL    350     /*Warning, the specified escape is not
                                     available on one or more workstations
                                     in this implementation.  The escape
                                     will be processed by those worksta-
                                     tions on which it is available */
#define        PE_BAD_ESC_DATA_REC    351     /*Ignoring function, one of the fields
                                     within the escape data record is in
                                     error */
#define        PE_AR_CANT_OPEN   400     /*Ignoring function, the archive file
                                     cannot be opened */
#define        PE_MAX_AR     401     /*Ignoring function, opening this ar-
                                     chive file would exceed the maximum
                                     number of simultaneously open archive
                                     files */
#define        PE_DUP_AR_ID 402     /*Ignoring function, archive file iden-
                                     tifier already in use */
#define        PE_BAD_AR 403     /*Ignoring function, the archive file is
                                     not a PHIGS archive file */
#define        PE_AR_NOT_OPEN  404     /*Ignoring function, the specified ar-
                                     chive file is not open */
#define        PE_NAME_CONFLICT  405     /*Ignoring function, name conflict oc-
                                     cured while conflict resolution flag
                                     has value ABANDON */
#define        PE_AR_FULL    406     /*Warning, the archive file is full.
                                     Any structures that were archived were
                                     archived in total */
#define        PE_AR_NO_STRUCT  407     /*Warning, some of the specified struc-
                                     tures do not exist on the archive file */
#define        PE_AR_NO_STRUCT_EMPTY  408     /*Warning, some of the specified struc-
                                     tures do not exist on the archive
                                     file.  PHIGS will create empty struc-
                                     tures in their places */
#define        PE_BAD_ERROR_FILE 450    /*Ignoring function, the specified
                                     error file is invalid */
/* PHIGS+ errors */
#define	       PE_ORDER_LT_1 500    /*Ignoring function, the specified order
                                     is less than 1 */
#define        PE_CTL_POINTS  501    /*Ignoring function, not enough control
                                     points for specified order */
#define        PE_BAD_ORDER   502    /*Ignoring function, the specified order
                                     is inconsistent with number of knots
                                     and control points */
#define        PE_BAD_KNOTS   503    /*Ignoring function, the knot sequence is
                                     not non-decreasing */
#define        PE_BAD_VERT_INDX  504    /*Ignoring function, one or more of the
                                     vertex indices is out of range */
#define        PE_DEGEN_FAS   505    /*Warning, the fill area is degenerate */
#define        PE_BAD_PARAM_RANGE 506    /*Ignoring function, parameter range is
                                     inconsistent with the knots */
#define        PE_BAD_EDGE_FLAG   513    /*Ignoring function, inconsistent edge
                                     flag specification */
#define        PE_OFLOOW_PHIGS     900	   /*Storage overflow has occurred in PHIGS */
#define        PE_OFLOW_CSS    901	   /*Storage overflow has occurred in CSS */
#define        PE_IO_ERROR_READ     902	   /*Input/Output error has occurred while
				     reading */
#define        PE_IO_ERROR_WRITE    903	   /*Input/Output error has occurred while
				     writing */
#define        PE_IO_ERROR_TO_WS     904	   /*Input/Output error has occurred while
				     sending data to a workstation */
#define        PE_IO_ERROR_FROM_WS     905	   /*Input/Output error has occurred while
				     receiving data from a workstation */
#define        PE_IO_ERROR_LIB     906	   /*Input/Output error has occurred during
				     program library management */
#define        PE_IO_ERROR_WDT     907	   /*Input/Output error has occurred while
				     reading workstation description table */
#define        PE_ARITHMETIC_ERROR      908	   /*Arithmetic error has occurred */

/* C Binding specific errors */
#define        PE_START_IND_INVAL   2200    /*Buffer overflow in input or inquiry
                                     function */
#define        PE_LIST_LEN_LT_ZERO   2201    /*Start index out of range */
#define        PE_ENUM_TYPE_INVAL      2202    /* Enumeration type out of range */

/* FORTRAN Binding specific errors - these are defined here only to allow
   translation of fortran binding error numbers to messages */
#define        PE_FTN_2000   2000    /*Ignoring function, enumeration type out of
				     range */
#define        PE_FTN_2001   2001    /*Ignoring function, output parameter size
				     insufficient */
#define        PE_FTN_2002   2002    /*Ignoring function, list or set element
				     not available */
#define        PE_FTN_2003   2003    /*Ignoring function, invalid data record */
#define        PE_FTN_2004   2004    /*Ignoring function, input parameter size
				     out of range */
#define        PE_FTN_2005   2005    /*Ignoring function, invalid list of point
				     lists */
#define        PE_FTN_2006   2006    /*Ignoring function, invalid list of 
				     filters */
