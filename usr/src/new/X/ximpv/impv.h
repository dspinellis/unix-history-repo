/* $Header* */

struct glyph {
	int	advance;	/* advance size */
	int	width;		/* width */
	int	left;		/* left offset */
	int	height;		/* height of glyph */
	int	top;		/* top offset */
	unsigned char	*bits;	/* pointer to actual bitmap */
} 
font0[128];
struct glyph *family[128];
short fam, member;
char fam_rot[128];

/* macro structure and variables*/
struct macro_struct{
	int length;
	unsigned char *pointer;
} 
macro[256];
int macro_length, macro_on, macro_count;
unsigned char *mp;

/* current state of the imagen */
int push_mask;
int advance_dir, orient;       
int horigin, vorigin;
int HPos, VPos, xpos, ypos;
int SpaceSize, BeginOfLine, InterLine, CurFamily;
int diameter;
int operation, texture;
short	finish, page, start;
short	pages;	/* set how many pages of document to save*/
short	magnification;
short	originlh, originlv, hvangle;
short	big, little;

struct state{
	int diameter, texture, SpaceSize, InterLine, BeginOfLine, fam;
	int HPos, VPos, advance_dir, horigin, vorigin, orient, push_mask;
} 
pstack;
struct state *push_stack[128], *stap;
int pushed;

short  fam_in, map_name, ntuples, vertex_count;
struct path {
	short hor;
	short vert;
} 
*path_point;
extern char map8_4[];

short backcolor;
#ifdef XWIND
unsigned char pscreen[SCREENSIZE];
#else XWIND
struct pixrect *pscreen;
short slide;
#ifdef COLOR
struct colors {
	float red,green,blue;
};
struct colors bc,cc;
#endif COLOR
#endif XWIND

int scr_x;
int wide;			/* width of line in pixrect for page */
int scr_y;
int scr_d;
int scr_size;
