/*
 *  solFb.h -- 
 *
 *	remade by A.Fujita, DEC-16-1992
 */


#include <luna68k/dev/fbio.h>


typedef struct _SolFbProc {
	int		(*CreateProc)();
	int		(*InitProc)();
	void		(*GiveUpProc)();
} SolFbProc;        

typedef struct _SolFbInfo {
	int		fb_type;
	int		fb_width;
	int		fb_height;
	int		fb_depth;
	int		scr_width;
	int		scr_height;
	char		*plane;				/* BitMap Plane 1   [0xB10C0008] */
	int		fbfd;				/* /dev/fb */
	int		fbmapsize;			/* frame buffer mapping size   */
	char		*fbmap;				/* frame buffer mapping addres */
	SolFbProc	*func;
} SolFbInfo, *SolFbInfoPtr;


/*	fb_type		*/
#define FB_BM 		0


/* scr_width and scr_height */
#define SCREEN_WIDTH				1280
#define SCREEN_HEIGHT				1024

/* fb_width and fb_height */
#define FB_WIDTH				2048
#define FB_HEIGHT				1024

#define COLOR_TV_RESOLUTION  			110 
#define MONO_TV_RESOLUTION			125   


/*
**	frame buffer memory memory map
*/
struct bm_one_data {	/* mono frame buffer */
	int	sd[1][1024][64];	/* 2048 x 1024 */
};

struct bm_eight_data {	/* 8 depth color frame buffer */
	int	sd[8][1024][64];	/* 2048 x 1024 x 8 */
};

/*
** luna raster operateon hardware memory map
*/
union bm_function_set {
	long	op_cont[16];	/* function set register */
	char	pad[0x40000];
};

/*
**	luna graphic display control hardware mememory map 
*/
typedef struct sol_bm_fbmap {
	union {	
		long	reg;
		char	pad2[0x40000];
	} refresh;				/* display area control register (write only) */
	union {
		long	reg;
		char	pad3[0x40000];
	} pselect;				/* plane select register (write only) */
	struct bm_one_data	cbmplane;	/* common frame buffer (write only) */
	struct bm_eight_data	bmplane;	/* frame buffer (read/write) */
	union bm_function_set	cplane;		/* luna raster operateon hardware (common frame buffer)  */
	union bm_function_set	planes[8];	/* luna raster operateon hardware */
} *SolBmMapPtr;


extern Bool solBmCreate();
extern Bool solBmInit();
extern void solBmGiveUp();
