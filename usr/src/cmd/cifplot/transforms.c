/*******************************************************************
*                                                                  *
*    File: CIFPLOT/transforms.c                                    *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include <math.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "alloc.h"

IMPORT alloc();
IMPORT point *MakePoint();
IMPORT char *Concat();

transform *CurTransform;
struct TCell *TransStack;

transform *
MatrixMult(t1,t2)
register transform *t1,*t2;
/* Return a pointer to a transform that is the product of 't1' & 't2' */
{
    register transform *out;
    /*
    int i,j,k;
    */
    out = GetTransform();
    out->refs = 0;
    if(output == NOPLOT)
    	out->TransString = Concat(t1->TransString,t2->TransString,0);
    /*
    for(i=0; i<3; i++)
	for(j=0; j<3; j++)
	    out->t[i][j] = 0;
     for(i=0; i<3; i++)
	for(j=0; j<3; j++)
	    for(k=0; k<3; k++)
		out->t[i][j] = out->t[i][j] + t1->t[i][k]*t2->t[k][j];
		*/
     out->t[0][0] = t1->t[0][0]*t2->t[0][0] + t1->t[0][1]*t2->t[1][0];
     out->t[0][1] = t1->t[0][0]*t2->t[0][1] + t1->t[0][1]*t2->t[1][1];
     out->t[1][0] = t1->t[1][0]*t2->t[0][0] + t1->t[1][1]*t2->t[1][0];
     out->t[1][1] = t1->t[1][0]*t2->t[0][1] + t1->t[1][1]*t2->t[1][1];
     out->t[2][0] = t1->t[2][0]*t2->t[0][0] + t1->t[2][1]*t2->t[1][0] + t2->t[2][0];
     out->t[2][1] = t1->t[2][0]*t2->t[0][1] + t1->t[2][1]*t2->t[1][1] + t2->t[2][1];
     return(out);
     }

transform *
MakeTransform()
/* Return a pointer to an identity transform */
{
    transform *trans;
    int i,j;
    trans = GetTransform();
    if(output == NOPLOT)
    	trans->TransString = Concat("",0);
    trans->refs = 0;
    for(i=0; i<3; i++)
	for(j=0; j<3; j++)
	    if (i==j) trans->t[i][i]=1;
		  else trans->t[i][j]=0;
    return(trans);
    }

transform *
CopyTransform(t)
transform *t;
{
    transform *temp;
    int i,j;
    temp = MakeTransform();
    for(i=0; i<3; i++)
	for(j=0; j<3; j++)
		temp->t[i][j] = t->t[i][j];
    return(temp);
    }

transform *
Mirror(ch,trans)
char ch;
transform *trans;
/* Return a pointer to a transform that does a mirroring about the
 * axis specified in 'ch' combined with the effect of 'trans' */
{
    transform *temp,*temp2;
    temp = MakeTransform();
    if(output == NOPLOT)
    	Free(temp->TransString);
    if(ch == 'y' || ch == 'Y') {
	temp->t[1][1] = -1;
        if(output == NOPLOT)
		temp->TransString = Concat(" MY",0);
	}
    else if(ch == 'x' || ch == 'X') {
	temp->t[0][0] = -1;
        if(output == NOPLOT)
		temp->TransString = Concat(" MX",0);
	}
      else {
	Error("Mirror call about unknown axis",INTERNAL);
        if(output == NOPLOT)
		temp->TransString = Concat(" XX",0);
	}
    temp2 = MatrixMult(temp,trans);
    if(output == NOPLOT)
    	Free(temp->TransString);
    FreeTransform(temp);
    return(temp2);
    }

transform *
Rotate(pt,trans)
point *pt;
transform *trans;
/* Return a pointer to a transform that does a rotation to a vector
 * specified by 'pt' combined with the effect of trans. */
{
    transform *temp,*temp2;
    real c;
    char s[128];

    temp = MakeTransform();
    if(output == NOPLOT) {
	Free(temp->TransString);
    	sprintf(s," R %d %d",TRUNC(pt->x), TRUNC(pt->y));
    	temp->TransString = Concat(s,0);
	}
    c = (real) sqrt((double) ((pt->x)*(pt->x) + (pt->y)*(pt->y)));
    /* Watch out for divide by 0 */
    if( -0.00001 <= c && c <= 0.00001 ) Error("Attempted to divide by zero",INTERNAL);
    temp->t[0][0] = (pt->x)/c;	temp->t[0][1] = (pt->y)/c;
    temp->t[1][0] = -(pt->y)/c;	temp->t[1][1] = (pt->x)/c;
    temp2 = MatrixMult(temp,trans);
    if(output == NOPLOT)
       Free(temp->TransString);
    FreeTransform(temp);
    return(temp2);
    }

transform *
Translate(pt,trans)
point *pt;
transform *trans;
/* Return a pointer to a transform that translates points by the vector
 * 'pt' combined with the effects of 'trans'. */
{
    transform *temp,*temp2;
    char s[128];

    temp = MakeTransform();
    if(output == NOPLOT) {
    	Free(temp->TransString);
    	sprintf(s," T %d %d",TRUNC(pt->x), TRUNC(pt->y));
    	temp->TransString = Concat(s,0);
	}
    temp->t[2][0] = pt->x;
    temp->t[2][1] = pt->y;
    temp2 = MatrixMult(temp,trans);
    if(output == NOPLOT) 
	Free(temp->TransString);
    FreeTransform(temp);
    return(temp2);
    }

transform *
Scale(a,b,trans)
int a,b;
transform *trans;
/* Return a pointer to a transform that scales points by a/b
 * combined with the effects of 'trans'. */
{
    transform *temp,*temp2;
    char s[128];

    temp = MakeTransform();
    if(output == NOPLOT) {
    	Free(temp->TransString);
    	sprintf(s," S %d %d",a,b);
    	temp->TransString = Concat(s,0);
	}
    temp->t[0][0] = ((real) a)/((real) b);
    temp->t[1][1] = ((real) a)/((real) b);
    temp2 = MatrixMult(temp,trans);
    if(output == NOPLOT) 
	Free(temp->TransString);
    FreeTransform(temp);
    return(temp2);
    }

/*
real
MakeFloat(a,b)
int a,b;
{
    real f,d;
    int rem;
    f = 0.0;
    d = 1.0;

    while(b>0) {
	rem = b%10;
	d = d*0.1;
	f = f + ((real) rem)*d;
	b = TRUNC(b/10);
	}
    return(a>=0 ? ((real) a) + f: ((real) a) - f);
    }
    */

/*
transform *
ReadTransform(a,b,c,d,e,f,g,h,i,j,k,l,t)
int a,b,c,d,e,f;
int g,h,i,j,k,l;
transform *t;
{
    transform *temp,*temp2;
    temp = MakeTransform();
    temp->t[0][0] = MakeFloat(a,b);
    temp->t[0][1] = MakeFloat(c,d);
    temp->t[1][0] = MakeFloat(e,f);
    temp->t[1][1] = MakeFloat(g,h);
    temp->t[2][0] = MakeFloat(i,j);
    temp->t[2][1] = MakeFloat(k,l);
    temp2 = MatrixMult(temp,t);
    FreeTransform(temp);
    return(temp2);
    }
    */

/*
float MakeFlt(i,d)
int i;
float d;
{
    float f;

    f = (i < 0) ? i - d : i + d;
    return(f);
    }

float Float(ch,f)
char ch;
float f;
{
    float d;

    d = 0.1*((float) ((ch -'0') + f));
    return(d);
    }
    */

point *
TransPt(pt,trans)
point *pt;
transform *trans;
/* Transform point */
{
    real x,y;

    x = pt->x*trans->t[0][0] + pt->y*trans->t[1][0] + trans->t[2][0];
    y = pt->x*trans->t[0][1] + pt->y*trans->t[1][1] + trans->t[2][1];
    return(MakePoint(x,y));
    }

Trans(x,y,trans)
register real *x,*y;
register transform *trans;
{
    register real z;

    z = *x;
    *x = z*trans->t[0][0] + (*y)*trans->t[1][0] + trans->t[2][0];
    *y = z*trans->t[0][1] + (*y)*trans->t[1][1] + trans->t[2][1];
    }

/*
InitTransform()
{
    TransStack = NIL;
    CurTransform = MakeTransform();
    }

PushTransform(trans)
transform *trans;
/* Push the Current Transform on the transform stack. Multiply
 * 'trans' by old Current Transform and make the product the
 * Current Traansform.	*/
 /*
{
    struct TCell *p;

    p = (struct TCell *) alloc(sizeof(struct TCell));
    p->TPtr = CurTransform;
    p->TLink = TransStack;
    TransStack = p;
    CurTransform = MatrixMult(CurTransform,trans);
    return;
    }

transform *
PopTransform()
/* Replace the Current Transform by the top of the transform stack
 * and pop the transform stack */
 /*
{
    struct TCell *t;

    if(TransStack == NIL)
	Error("Attempted to Pop empty Transform Stack",INTERNAL);
    CurTransform = TransStack->TPtr;
    t = TransStack;
    TransStack = TransStack->TLink;
    FreeTransform(t);
    return(CurTransform);
    }
    */
