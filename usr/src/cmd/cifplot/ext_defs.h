#define ALL		-1
#define GATE		0
#define METAL		1
#define POLY		2
#define DIFFUSION	3
#define CUT		4
#define BURIED		5
#define IMPLANT		6

#define ACTIVE_LAYERS	4

#define EXT_MAGIC_WORD	0xdf010000

#define NEW_SWATH	1
#define EXT_EDGE	2
#define EXT_POINT	3

struct ExtEdgeRecord {
    int type;
    real x,deltax;
    int layer,start;
    };

struct ExtNewSwathRecord {
    int type;
    int top,bottom;
    };

struct ExtPointRecord {
    int type;
    real x;
    int y;
    int	name;
    int layer;
    };
