/* $Header$ */

/*
 * Mkmf definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Buffer sizes
 */
#define ANSWERBUFSIZE		256	/* interactive answer buffer size */
#define INCLUDETABLESIZE	127	/* include file table size */
#define MACRODEFSIZE		1024	/* macro definition body size */
#define MACRONAMSIZE		32	/* macro definition name size */
#define MDEFTABLESIZE		127	/* macro definition table size */
#define SFXTABSIZE		256	/* suffix table size */
#define SUFFIXSIZE		16	/* suffix size */
/*
 * Predefined macro names
 */
#define MCFLAGS		"CFLAGS"
#define MDESTDIR	"DEST"
#define MEXTERNALS	"EXTHDRS"
#define MFFLAGS		"FFLAGS"
#define MHEADERS	"HDRS"
#define MLIBLIST	"LIBS"
#define MLIBRARY	"LIBRARY"
#define MMAKEFILE	"MAKEFILE"
#define MOBJECTS	"OBJS"
#define MPFLAGS		"PFLAGS"
#define MPROGRAM	"PROGRAM"
#define MSOURCE		"SRCS"
#define MSUFFIX		"SUFFIX"
/*
 * Predefined macro values
 */
#define VUNKNOWN		0
#define VREADONLY		1
#define VREADWRITE		2
#define VLIBRARY		3
#define VPROGRAM		4
#define VDESTDIR		5
/*
 * Include statement styles
 */
#define INCLUDE_C		'C'	/* #include "file" */
#define INCLUDE_FORTRAN		'F'	/* include "file" or #include "file" */
#define INCLUDE_PASCAL		'P'	/* #include "file" */
#define INCLUDE_NONE		0	/* no include file */
