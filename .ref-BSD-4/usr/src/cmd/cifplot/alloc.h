#define	GetDesc()	(PolyDesc *) Get(&FreeLDesc,sizeof(PolyDesc),0x1234de5c)
#define FreeDesc(x)	FreeGet(x,&FreeLDesc,0x1234de5c)

#define GetIEdge()	(iedge *) Get(&FreeLIEdges,sizeof(iedge),0x1234ed9e)
#define FreeIEdge(x)	FreeGet(x,&FreeLIEdges,0x1234ed9e)
#define GetEdge()	(edge *) Get(&FreeLEdge,sizeof(edge),0x1235ed9e)
#define FreeEdge(x)	FreeGet(x,&FreeLEdge,0x1235ed9e)

#define GetItem()	(instance *) Get(&FreeLItems,sizeof(instance),0x123417e3)
#define FreeItem(x)	FreeGet(x,&FreeLItems,0x123417e3)
#define GetTextStruct()	(TextStruct *) Get(&FreeLText,sizeof(TextStruct),0x12347ec7)
#define FreeTextStruct(x)	FreeGet(x,&FreeLText,0x12347ec7);
#define GetCommand()	(Command *) Get(&FreeLComm,sizeof(Command),0x1234c033)
#define FreeCommand(x)	FreeGet(x,&FreeLComm,0x1234c033);
#define GetTransform()	(transform *) Get(&FreeLTrans,sizeof(transform),0x123477a8)
#define FreeTransform(x)	FreeGet(x,&FreeLTrans,0x123477a8);
#define GetPointList()	(PointList *) Get(&FreeLPList,sizeof(PointList),0x12349fda)
#define FreePointList(x)	FreeGet(x,&FreeLPList,0x12349fda);


Queue FreeLItems,FreeLTrans,FreeLIEdges,FreeLDesc,FreeLComm,FreeLEdge;
Queue FreeLText,FreeHolders,FreeLPList;

