
typedef struct temp100 {
	struct temp100 *Link;
	int type;
	int min;
	transform *trans;
	Command   *item;
	int depth;
	} instance;

typedef struct {
	int refs,count,level;
	real dy,iy;
	} PolyDesc;

typedef struct temp101 {
	struct temp101 *Link;
	int type;
	int min;
	int x1,y1,x2,y2;
	int dir;
	PolyDesc *poly;
	} iedge;

typedef struct temp102 {
	struct temp102 *Link;
	int type;
	int ix,ex;
	real iy,dy;
	int dir;
	PolyDesc *poly;
	} edge;

typedef struct temp103 {
	struct temp103 *Link;
	real start,dstart,end,dend;
	int level;
	} nedge;

int Top,Bottom;

Queue *EdgeQueue,*NecEdgeQueue,*NEdgeQueue;
Queue FreeHolders;
List *ActiveEdges;
List TempList;

int *EdgeStart,NextEdge,*EdgeEnd,LastEdge;
int *EdgeIntersection,NextIntersection,*Valid,*Drawn,*change;
int *NextChange;
int *xprev;
int xLast;	/* last line sent to plotter */
int finishBuf;	/* flag set when outputting end of output buffer */

nedge **EdgeHolder;

extern int UsedLayers;
int ModCount;

PolyDesc DummyPoly;

int NextUnAct;
