/* rst.h	1.2	(Berkeley)	83/10/09
 *
 *	Definitions for imagen RST files.
 */

#define FMARK 	8			/* number of bytes in the filemark */
#define RES	240			/* number of dots/inch on imagen */
#define	FIX	(1.0 / (1 << 20))	/* points per fix */
#define POINT	(1.0 / 72.27)		/* inches per point */
#define	FIXIN	(POINT * FIX)		/* inches per fix */
#define FIXPIX	(FIXIN * RES)		/* pixels per fix */
#define DIRSIZ	128			/* max size of a glyph directory */


char	filemark[FMARK];	/* string to test beginning of file */

typedef struct {	/* glyph directory entry */
	short g_height;		/* height (pixels) of glyph */
	short g_width;		/* width of glyph */
	short g_up;		/* distance from top to reference point */
	short g_left;		/* distance from left to reference point */
	int g_pwidth;		/* printing width of character (troff width) */
	int g_bitp;		/* pointer to raster data (index to file) */
} glyph_dir;

typedef struct {	/* preamble information entries */
	short p_size;		/* size of preamble (not including this) */
	char p_version;		/* rst format version number (hopefully 0) */
	int p_glyph;		/* pointer to glyph directory */
	short p_first;		/* first glyph in directory */
	short p_last;		/* last glyph in directory */
	int p_mag;		/* magnification (in 1/1000ths) */
	int p_desiz;		/* design size (in FIX units) */
	int p_linesp;		/* spacing 'tween lines (FIX) */
	int p_wordsp;		/* word spacing (FIX) = troff's spacewidth */
	short p_rot;		/* rotation (degrees) */
	char p_cadv;		/* character advance direction (0,1,2,3) = 0 */
	char p_ladv;		/* line advance direction (0,1,2,3) = 1 */
	int p_id;		/* identifying mark */
	short p_res;		/* resolution of font (pixels/inch) = 240 */
} preamble;
