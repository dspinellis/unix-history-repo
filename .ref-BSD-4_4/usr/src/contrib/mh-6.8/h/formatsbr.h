/* formatsbr.h - definitions for fmtscan () */
/* $Id: formatsbr.h,v 1.1 1992/01/23 23:14:54 jromine Exp $ */

/*
 * This structure describes an "interesting" component.  It holds
 * the name & text from the component (if found) and one piece of
 * auxilary info.  The structure for a particular component is located
 * by hashing the name and using it as an index into the ptr array
 * "wantcomp".  All format entries that reference a particular component
 * point to its comp struct (so we only have to do component specific
 * processing once.  e.g., parse an address.).
 */
struct comp {
	char		*c_name;	/* component name (in lower case) */
	struct	comp	*c_next;	/* hash chain linkage */
	char		*c_text;	/* component text (if found) */
	short		c_flags;	/* misc. flags (from formatsbr) */
	short		c_type;		/* type info (from fmtcompile) */
	union {
		struct tws	*c_u_tws;
		struct mailname *c_u_mn;
	} c_un;
#define c_tws c_un.c_u_tws
#define c_mn c_un.c_u_mn
};

/* c_type bits */
#define	CT_ADDR		1	/* referenced as address */
#define	CT_DATE		2	/* referenced as date */
#define	CT_MYMBOX	4	/* "mymbox" test being done */
#define	CT_ADDRPARSE	8	/* address parse being done */

extern int fmt_norm;

struct	comp	*wantcomp[128];	/* hash table for deciding if a
				 * component is "interesting" */

/* 
 * Hash function for component name.  The function should be
 * case independent and probably shouldn't involve a routine
 * call.  This function is pretty good but will not work on
 * single character component names.  
 */
#define	CHASH(nm)	(((((nm)[0]) - ((nm)[1])) & 0x1f) + (((nm)[2]) & 0x5f))

#ifdef	GOULD_PN
/* bug in the Gould PowerNode compiler: need a local pointer to name... */
#define FINDCOMP(comp,name1) \
	{ \
	char *name = (name1); \
	for (comp = wantcomp[CHASH(name)]; \
		comp && strcmp(comp->c_name,name); \
		comp = comp->c_next) ; \
	}
#else
#define FINDCOMP(comp,name) \
		for (comp = wantcomp[CHASH(name)]; \
		     comp && strcmp(comp->c_name,name); \
		     comp = comp->c_next) ;
#endif

/*
 * This structure defines one formatting instruction.
 */
struct format {
	unsigned char	f_type;
	char		f_fill;
	short		f_width;	/* output field width */
#define f_skip f_width			/* instr to skip (false "if") */
	union {
		struct comp	*f_u_comp;	/* associated component */
		char		*f_u_text;	/* literal text */
		char		f_u_char;	/* literal character */
		int		f_u_value;	/* literal value */
	} f_un;
#define f_comp f_un.f_u_comp
#define f_text f_un.f_u_text
#define f_char f_un.f_u_char
#define f_value f_un.f_u_value
};

struct format *fmtscan ();
char   *new_fs ();
