/* $XConsortium: attr.h,v 5.5 91/07/18 12:57:51 hersh Exp $ */

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

#ifndef PHG_ATTR_H_INCLUDED
#define	PHG_ATTR_H_INCLUDED

typedef caddr_t *	Phg_attr_avlist;
typedef unsigned	Phg_attr_attribute;		/* 32 bit quantity */
/* Phg_attr_avlist is not an array of Phg_attr_attributes, because it is an array
 * of intermixed attributes and values.
 */

/* This macro is machine dependent in that it assumes
 * the cardinality will be in the lower 5 bits of the type-cardinality
 * pair.
 */
#define	PHG_ATTR_TYPE(base_type, cardinality) \
    (((((unsigned)(base_type)) & 0x07f) << 5) |	\
     (((unsigned)(cardinality)) & 0x0f))

/* Base types in the range PHG_ATTR_BASE_UNUSED_FIRST
 * to PHG_ATTR_BASE_UNUSED_LAST are available for
 * client use.
 */
#define	PHG_ATTR_BASE_UNUSED_FIRST		0
#define	PHG_ATTR_BASE_UNUSED_LAST		31
/* Base types 32 through 63 are
 * reserved for future use.
 */
#define	PHG_ATTR_BASE_FIRST			64

typedef enum {
    PHG_ATTR_BASE_INT		= PHG_ATTR_BASE_FIRST,
    PHG_ATTR_BASE_INT_PAIR	= PHG_ATTR_BASE_FIRST + 1,
    PHG_ATTR_BASE_X		= PHG_ATTR_BASE_FIRST + 2,
    PHG_ATTR_BASE_INDEX_X	= PHG_ATTR_BASE_FIRST + 3,
    PHG_ATTR_BASE_Y		= PHG_ATTR_BASE_FIRST + 4,
    PHG_ATTR_BASE_INDEX_Y	= PHG_ATTR_BASE_FIRST + 5,
    PHG_ATTR_BASE_XY		= PHG_ATTR_BASE_FIRST + 6,
    PHG_ATTR_BASE_INDEX_XY	= PHG_ATTR_BASE_FIRST + 7,
    PHG_ATTR_BASE_BOOLEAN	= PHG_ATTR_BASE_FIRST + 8,
    PHG_ATTR_BASE_ENUM		= PHG_ATTR_BASE_FIRST + 9,
    PHG_ATTR_BASE_CHAR		= PHG_ATTR_BASE_FIRST + 10,
    PHG_ATTR_BASE_STRING	= PHG_ATTR_BASE_FIRST + 11,
    PHG_ATTR_BASE_RECT_PTR	= PHG_ATTR_BASE_FIRST + 15,
    PHG_ATTR_BASE_OPAQUE	= PHG_ATTR_BASE_FIRST + 16,
    PHG_ATTR_BASE_NO_VALUE	= PHG_ATTR_BASE_FIRST + 17,
    PHG_ATTR_BASE_AV		= PHG_ATTR_BASE_FIRST + 18,
    PHG_ATTR_BASE_FUNCTION_PTR	= PHG_ATTR_BASE_FIRST + 19,
    PHG_ATTR_BASE_ICON_PTR	= PHG_ATTR_BASE_FIRST + 20,
    PHG_ATTR_BASE_INT_TRIPLE	= PHG_ATTR_BASE_FIRST + 23,
    PHG_ATTR_BASE_LONG		= PHG_ATTR_BASE_FIRST + 24,
    PHG_ATTR_BASE_SHORT		= PHG_ATTR_BASE_FIRST + 25
} Phg_attr_base_type;

/* Clients of the attribute value package should use
 * Phg_attr_base_cardinality elements to define the base type
 * and cardinality of their attributes.
 */  
typedef enum {
    PHG_ATTR_INT		= PHG_ATTR_TYPE(PHG_ATTR_BASE_INT, 1),
    PHG_ATTR_INT_PAIR		= PHG_ATTR_TYPE(PHG_ATTR_BASE_INT_PAIR, 2),
    PHG_ATTR_INT_TRIPLE		= PHG_ATTR_TYPE(PHG_ATTR_BASE_INT_TRIPLE, 3),
    PHG_ATTR_X			= PHG_ATTR_TYPE(PHG_ATTR_BASE_X, 1),
    PHG_ATTR_INDEX_X		= PHG_ATTR_TYPE(PHG_ATTR_BASE_INDEX_X, 2),
    PHG_ATTR_Y			= PHG_ATTR_TYPE(PHG_ATTR_BASE_Y, 1),
    PHG_ATTR_INDEX_Y		= PHG_ATTR_TYPE(PHG_ATTR_BASE_INDEX_Y, 2),
    PHG_ATTR_XY			= PHG_ATTR_TYPE(PHG_ATTR_BASE_XY, 2),
    PHG_ATTR_INDEX_XY		= PHG_ATTR_TYPE(PHG_ATTR_BASE_INDEX_XY, 3),
    PHG_ATTR_BOOLEAN		= PHG_ATTR_TYPE(PHG_ATTR_BASE_BOOLEAN, 1),
    PHG_ATTR_ENUM		= PHG_ATTR_TYPE(PHG_ATTR_BASE_ENUM, 1),
    PHG_ATTR_CHAR		= PHG_ATTR_TYPE(PHG_ATTR_BASE_CHAR, 1),
    PHG_ATTR_STRING		= PHG_ATTR_TYPE(PHG_ATTR_BASE_STRING, 1),
    PHG_ATTR_RECT_PTR		= PHG_ATTR_TYPE(PHG_ATTR_BASE_RECT_PTR, 1),
    PHG_ATTR_OPAQUE		= PHG_ATTR_TYPE(PHG_ATTR_BASE_OPAQUE, 1),
    PHG_ATTR_NO_VALUE		= PHG_ATTR_TYPE(PHG_ATTR_BASE_NO_VALUE, 0),
    PHG_ATTR_AV			= PHG_ATTR_TYPE(PHG_ATTR_BASE_AV, 1),
    PHG_ATTR_FUNCTION_PTR	= PHG_ATTR_TYPE(PHG_ATTR_BASE_FUNCTION_PTR, 1),
    PHG_ATTR_ICON_PTR		= PHG_ATTR_TYPE(PHG_ATTR_BASE_ICON_PTR, 1),
    PHG_ATTR_LONG		= PHG_ATTR_TYPE(PHG_ATTR_BASE_LONG, 1),
    PHG_ATTR_SHORT		= PHG_ATTR_TYPE(PHG_ATTR_BASE_SHORT, 1)
} Phg_attr_base_cardinality;

/* Note that this macro is machine dependent in that it assumes the
 * base_type-cardinality pair will be in the lower 13 bits of the 
 * list_type-base_cardinality pair.
 */
#define	PHG_ATTR_LIST_OF(list_type, list_ptr_type, base_cardinality) \
    (((((unsigned)(list_type)) & 0x3) << 14) | \
     (((unsigned)(list_ptr_type) & 0x1) << 13) | \
     (((unsigned)(base_cardinality)) & 0x3fff))

typedef enum {
    PHG_ATTR_LIST_IS_INLINE	= 0,
    PHG_ATTR_LIST_IS_PTR	= 1
} Phg_attr_list_ptr_type;

#define	PHG_ATTR_LIST_INLINE(list_type, base_cardinality)	\
    PHG_ATTR_LIST_OF(list_type, PHG_ATTR_LIST_IS_INLINE, base_cardinality)
    
#define	PHG_ATTR_LIST_PTR(list_type, base_cardinality)	\
    PHG_ATTR_LIST_OF(list_type, PHG_ATTR_LIST_IS_PTR, base_cardinality)
    
typedef enum {
    /* Note that PHG_ATTR_NONE must have a value of zero,
     * since a no-list type is assumed for each of the
     * types in Phg_attr_base_cardinality.
     */
    PHG_ATTR_NONE	= 0,
    PHG_ATTR_RECURSIVE	= 1,
    PHG_ATTR_NULL	= 2,
    PHG_ATTR_COUNTED	= 3
} Phg_attr_list_type;

/*
 * Note that 0 is NEVER a valid package id.
 *
 * The range from PHG_ATTR_PKG_UNUSED_FIRST to
 * PHG_ATTR_PKG_UNUSED_LAST is reserved for non-Sun packages.
 */
#define	PHG_ATTR_PKG_UNUSED_FIRST	1
#define PHG_ATTR_PKG_UNUSED_LAST	31
/* 32 through 63 are reserved for 
 * future use.
 */
#define	PHG_ATTR_PKG_FIRST		64
/* #define	PHG_ATTR_PKG_LAST		127 */
/* 128 through 255 are spare */
    
typedef enum {
    PHG_ATTR_PKG_ZERO	= 0,
    PHG_ATTR_PKG_GENERIC	= PHG_ATTR_PKG_FIRST,
    PHG_ATTR_PKG_LAST	= PHG_ATTR_PKG_FIRST + 18
} Phg_attr_pkg;
    /* Change PHG_ATTR_PKG_LAST to be EQUAL to the last legal pkg id. */
    /* The procedure counter(), called by phg_attr_make, aborts if */
    /* PKG_ID > PHG_ATTR_PKG_LAST */
    /* PKG name should also be added to phg_attr_names[] in phg_attr.c */

/* An attribute is composed of 
 * pkg     ( 8 bits): id of package that uses the attribute
 * ordinal ( 8 bits): ordinal number of the attribute
 * type    (16 bits): list type, list pointer type, base type, and cardinality
 */
#define	PHG_ATTR(pkg, type, ordinal)	\
    ( ((((unsigned)(pkg))	& 0x7f) << 24) | \
      ((((unsigned)(ordinal))	& 0xff) << 16) | \
       (((unsigned)(type))	& 0xefef)	)

typedef union {
    struct {
	unsigned		pkg		: 8;
	unsigned		ordinal		: 8;
	unsigned		list_type	: 2;
	unsigned		list_ptr_type	: 1;
	unsigned		spare1		: 1;	/* unused */
	unsigned		base_type	: 7;
	unsigned		spare2		: 1;	/* unused */
	unsigned		cardinality	: 4;
    } 			info;
    Phg_attr_attribute	code;
} Phg_attr_union;

/* Generic attributes
 */
typedef enum {
    PHG_ATTR_LIST = PHG_ATTR(PHG_ATTR_PKG_GENERIC, PHG_ATTR_LIST_PTR(PHG_ATTR_RECURSIVE, PHG_ATTR_NO_VALUE), 0)
} Phg_attr_generic;

/* PHG_ATTR_STANDARD_SIZE is large enough to allow for 
 * most attribute-value lists.
 */
#define	PHG_ATTR_STANDARD_SIZE	250

#ifdef notdef
/* Note that in these macros, attr must not be 
 * in a register or be a constant.
 * Since this is deemed too restrictive, we use
 * shifting & masking instead.  
 */
#define PHG_ATTR_UNION(attr)	((Phg_attr_union *) &((Phg_attr_attribute) (attr)))
#define	PHG_ATTR_INFO(attr)		(PHG_ATTR_UNION(attr)->info)
#define	PHG_ATTR_CODE(attr)		(PHG_ATTR_UNION(attr)->code)
#define	PHG_ATTR_LIST_TYPE(attr)	(PHG_ATTR_INFO(attr).list_type)
#define	PHG_ATTR_LIST_PTR_TYPE(attr)	(PHG_ATTR_INFO(attr).list_ptr_type)
#define	PHG_ATTR_BASE_TYPE(attr)	(PHG_ATTR_INFO(attr).base_type)
#define	PHG_ATTR_CARDINALITY(attr)	(PHG_ATTR_INFO(attr).cardinality)
#define	PHG_ATTR_PKG(attr)		(PHG_ATTR_INFO(attr).pkg)
#define	PHG_ATTR_ORDINAL(attr)	(PHG_ATTR_INFO(attr).ordinal)
#endif /* notdef */

#define	PHG_ATTR_CODE(attr)		((unsigned)(attr))

#define	PHG_ATTR_PKG(attr)	\
	((Phg_attr_pkg) ((PHG_ATTR_CODE(attr) >> 24) & 0xFF))

#define PHG_ATTR_VALID_PKG_ID(attr)		\
	(((int)PHG_ATTR_PKG(attr)) > ((int) PHG_ATTR_PKG_ZERO) && \
	 ((int)PHG_ATTR_PKG(attr)) <= ((int)PHG_ATTR_PKG_LAST))

#define	PHG_ATTR_ORDINAL(attr)	\
	((unsigned) ((PHG_ATTR_CODE(attr) >> 16) & 0xFF))

#define	PHG_ATTR_LIST_TYPE(attr)	\
	((Phg_attr_list_type) ((PHG_ATTR_CODE(attr) >> 14) & 0x3))

#define	PHG_ATTR_LIST_PTR_TYPE(attr)	\
	((Phg_attr_list_ptr_type) ((PHG_ATTR_CODE(attr) >> 13) & 0x1))

#define	PHG_ATTR_BASE_TYPE(attr)	\
	((Phg_attr_base_type) ((PHG_ATTR_CODE(attr) >> 5) & 0x7F))

#define	PHG_ATTR_CARDINALITY(attr)	\
	((unsigned) (PHG_ATTR_CODE(attr) & 0xF))


/* Character unit support */

#ifdef	lint
/* The following #ifndef fixes kernel lint warnings, but is pretty strange */
#ifndef lint
extern	void	phg_attr_replace_cu();
extern	int	phg_attr_cu_to_x();
extern	int	phg_attr_cu_to_y();
#endif
#else	/* lint */
#define phg_attr_replace_cu(avlist, font, lmargin, tmargin, rgap) \
    phg_attr_rc_units_to_pixels(avlist, font->pf_defaultsize.x, \
	font->pf_defaultsize.y, lmargin, tmargin, 0, rgap)
    
#define phg_attr_cu_to_x(encoded_value, font, left_margin) \
    phg_attr_rc_unit_to_x(encoded_value, font->pf_defaultsize.x, left_margin, 0)

#define phg_attr_cu_to_y(encoded_value, font, top_margin, row_gap) \
    phg_attr_rc_unit_to_y(encoded_value, font->pf_defaultsize.y, top_margin, row_gap)
#endif	/* lint */

typedef enum {
    PHG_ATTR_CU_POSITION	= 0x0,		/* bit 29 is off */
    PHG_ATTR_CU_LENGTH	= 0x20000000	/* bit 29 is on */

} Phg_attr_cu_type;

#define	PHG_ATTR_CU_TAG		0x80000000
#define PHG_ATTR_PIXEL_OFFSET	0x00008000

#define	PHG_ATTR_CU(unit, n)	\
   (((unsigned)(unit)) | (((unsigned)(n) & 0x1FFF) << 16) |	\
    PHG_ATTR_CU_TAG | PHG_ATTR_PIXEL_OFFSET)
    
#define	PHG_ATTR_CU_MASK		0xC0000000
#define	PHG_ATTR_CU_TYPE(n)		\
    ((Phg_attr_cu_type) ((n) & (unsigned) (PHG_ATTR_CU_LENGTH)))

/* phg_attr_is_cu(n) returns non-zero if n has 
 * been encoded using PHG_ATTR_CU()
 */
#define	phg_attr_is_cu(n)		(((n) & PHG_ATTR_CU_MASK) == PHG_ATTR_CU_TAG)

/* Macros for position including
 * margins.
 */
#define	PHG_ATTR_COL(n)		PHG_ATTR_CU(PHG_ATTR_CU_POSITION, n)
#define	PHG_ATTR_ROW(n)		PHG_ATTR_CU(PHG_ATTR_CU_POSITION, n)

/* Macros for length excluding
 * margins.
 */
#define	PHG_ATTR_COLS(n)	PHG_ATTR_CU(PHG_ATTR_CU_LENGTH, n)
#define	PHG_ATTR_ROWS(n)	PHG_ATTR_CU(PHG_ATTR_CU_LENGTH, n)
#define	PHG_ATTR_CHARS(n)	PHG_ATTR_CU(PHG_ATTR_CU_LENGTH, n)
#define	PHG_ATTR_LINES(n)	PHG_ATTR_CU(PHG_ATTR_CU_LENGTH, n)
   

/* phg_attr_make() is not interested in the
 * count from phg_attr_make_count().
 */
#define	phg_attr_make(array, max_size, argv)	\
    phg_attr_make_count(array, max_size, argv, (int *)0)

/* Following are useful for multi-pass avlist processing. */
#define	PHG_ATTR_NOP(attr)					\
 	( (((unsigned)PHG_ATTR_PKG_NOP) << 24) | (PHG_ATTR_CODE(attr) & 0xffffff) )
#define	PHG_ATTR_CONSUME(attr)	(attr) = ((caddr_t)PHG_ATTR_NOP(attr))

#define	phg_attr_skip(attr, argv)	\
    ((PHG_ATTR_LIST_TYPE((attr)) == PHG_ATTR_NONE) \
	? (argv) + PHG_ATTR_CARDINALITY((attr)) \
	: ((Phg_attr_avlist)phg_attr_skip_value((Phg_attr_attribute)(attr), (argv))))

#define	phg_attr_next(attrs)	phg_attr_skip((*(attrs)), ((attrs)+1))

#ifndef lint
/* Available functions
 */
extern Phg_attr_avlist	phg_attr_create_list();
extern Phg_attr_avlist	phg_attr_create();
extern Phg_attr_avlist	phg_attr_make_count();
extern int		phg_attr_copy();
extern int		phg_attr_count();
extern char 	       *phg_attr_sprint();
extern void		phg_attr_free();
extern Phg_attr_avlist	phg_attr_skip_value();
#endif /* lint */
#endif

