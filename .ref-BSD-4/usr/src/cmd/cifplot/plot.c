/*******************************************************************
*                                                                  *
*    File: CIFPLOT/plot.c                                          *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "out_structs.h"

#define CONST 4

IMPORT GetList();
IMPORT ICompare();
IMPORT instance *GetUnAct();

Plot()
{
     int i,j,k;

      vopen();
      Instanciate(prog,GlobalTransform,0);
      NextEdge = -INFINITY;
      for(i=CONVERT((real) prog->CBBox.xmin - 1); i<Bottom;i=Sift(i));
      for(j = Bottom; j<=Top+40; j += OUT_BUF_SIZE) {
	for(i = j; i < j+OUT_BUF_SIZE; i++) {
	    Sift(i);
	    if(i>=LastEdge) RemoveEdges(i);
	    /*
	    if(i>=NextEdge) 
	    */
		AddEdges(i);
	    Sort(i);
	    ScanActives(i);
	    }
	i = j + OUT_BUF_SIZE;
	/* We're at the end of output buffer, send remaining traps */
	finishBuf = 1;
	for(k=0;k<UsedLayers;k++) change[k] = 1;
	Sift(i);
	if(i>=LastEdge) RemoveEdges(i);
	/*
	if(i>=NextEdge) 
	*/
	    AddEdges(i);
	Sort(i);
	ScanActives(i);
	DumpBuf(i);
	finishBuf = 0;
	for(k=0;k<UsedLayers;k++) AddLine(k,i);
	}
    vclose();
    }

Sift(i)
register int i;
{
    register instance *p;

    while((p=GetUnAct(i)) != NIL) {
	Activate(p);
	}
    return(FindNext(i));
    }

InitPlotter()
{
    int i;
    Top = CONVERT((real) Window.xmax) + 1;
    xLast = Bottom = CONVERT((real) Window.xmin - 1) - 1;
    EdgeQueue = (Queue *) alloc(UsedLayers*sizeof(Queue));
    NEdgeQueue = (Queue *) alloc(UsedLayers*sizeof(Queue));
    ActiveEdges = (List *) alloc(UsedLayers*sizeof(List));
    NecEdgeQueue = (Queue *) alloc(UsedLayers*sizeof(Queue));
    EdgeEnd = (int *) alloc(UsedLayers*sizeof(int));
    EdgeStart = (int *) alloc(UsedLayers*sizeof(int));
    EdgeIntersection = (int *) alloc(UsedLayers*sizeof(int));
    Valid = (int *) alloc(UsedLayers*sizeof(int));
    Drawn = (int *) alloc(UsedLayers*sizeof(int));
    NextChange = (int *) alloc(UsedLayers*sizeof(int));
    EdgeHolder = (nedge **) alloc(UsedLayers*sizeof(nedge));
    change = (int *) alloc(UsedLayers*sizeof(int));
    xprev = (int *) alloc(UsedLayers*sizeof(int));
    NextEdge = LastEdge = INFINITY;
    finishBuf = 0;
    DummyPoly.level = 0;
    DummyPoly.count = 0;
    DummyPoly.refs = 0;
    for(i=0;i<UsedLayers;i++) {
	EdgeEnd[i] = INFINITY;
	EdgeStart[i] = Bottom;
	EdgeIntersection[i] = INFINITY;
	Valid[i] = INFINITY;
	Drawn[i] = -INFINITY;
	NextChange[i] = -INFINITY;
	EdgeHolder[i] = NIL;
	change[i] = 0;
	xprev[i] = -INFINITY;
	InitQueue(&(EdgeQueue[i]));
	InitQueue(&(NEdgeQueue[i]));
	InitQueue(&(NecEdgeQueue[i]));
	InitList(&(ActiveEdges[i]));
	}
    InitList(&TempList);
    InitUnAct();
    InitQueue(&FreeHolders);
    InitList(&TextList);
    InitScan();
    }

ReStartPlotter()
{
    int i;
    InitPlotter();
    NextEdge = LastEdge = INFINITY;
    DummyPoly.level = 0;
    DummyPoly.count = 0;
    DummyPoly.refs = 0;
    for(i=0;i<UsedLayers;i++) {
	EdgeEnd[i] = INFINITY;
	EdgeStart[i] = Bottom;
	EdgeIntersection[i] = INFINITY;
	Valid[i] = INFINITY;
	Drawn[i] = -INFINITY;
	NextChange[i] = -INFINITY;
	EdgeHolder[i] = NIL;
	change[i] = 0;
	InitQueue(&(EdgeQueue[i]));
	InitQueue(&(NEdgeQueue[i]));
	InitQueue(&(NecEdgeQueue[i]));
	InitList(&(ActiveEdges[i]));
	}
    }
