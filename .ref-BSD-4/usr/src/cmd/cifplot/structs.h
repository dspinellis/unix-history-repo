
struct BBox {
	real xmin,ymin,xmax,ymax;
	} GWindow,Window;

typedef struct {
	int refs;
	char *TransString;
	real t[3][3];
	} transform;

struct TCell {
	struct TCell *TLink;
	transform *TPtr;
	};

struct LCell {
	struct LCell *Link;
	struct LCell *BackLink;
	char *LName;
	int *pat;
	int LNum;
	short int visible;
	};

typedef struct {
	real x,y;
	} point;

typedef struct temp010 {
	struct temp010 *PLink;
	point pt;
	} PointList;

struct PathHeader {
	PointList *PHead,*PTail;
	int PNo;
	struct BBox PBBox;
	};

typedef struct temp011 {
	struct temp011 *CLink;
	int type;
	int min;
	int level;
	struct BBox CBBox;
	union {
		char *s;
		PointList *Path;
		char *Layer;
		struct {
		    real WWidth;
		    PointList *WPath;
		    struct temp011 *WIns;
		    } Wire;
		struct {
		    int SymNo;
		    struct temp011 *CStart,*CFinnish;
		    struct CCell *backTrace;
		    int status,a,b,Sid;
		    char *SName;
		    } Symbl;
		struct {
		    int CallNo;
		    transform *trans;
		    struct temp011 *CSymb;
		    } Call;
		struct {
		    real blength,bwidth;
		    point bcenter,bdirect;
		    } Box;
		struct {
		    real fdia;
		    point fcenter;
		    } Flash;
		struct {
		    struct temp011 *ACom;
		    int As,Am,An;
		    real Adx,Ady;
		    struct temp011 *AIns;
		    } Array;
		struct {
		    char *TString;
		    transform *TTrans;
		    char TLoc;
		    } Text;
		struct {
		    char *Name;
		    point loc;
		    char *Label;
		    } PointName;
		} Ctype;
	} Command;

struct CmdHeader {
	Command *CHhead,*CHtail;
	};

struct CCell {
	struct CCell *CCLink;
	int CCNo;
	Command *CCCom;
	};

typedef struct temp900 {
	struct temp900 *Link;
	} Element;

typedef struct {
	Element *Link;
	char *str;
	int xpos,ypos;
	} TextStruct;

typedef struct {
	Element *Link;
	} List;

typedef struct {
	Element *QStart;
	Element *QEnd;
	} Queue;

Command *prog;

int MaxLayers;
int **Pats;
List TextList;
double ConvertFactor;		/* Used to convert from chip to screen co-ords */
transform *ident;
transform *GlobalTransform;
