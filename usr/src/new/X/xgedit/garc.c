#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

#include "gedit.h"

#ifndef lint
static char *rcsid_garc_c = "$Header: garc.c,v 10.5 86/02/01 16:18:41 tony Rel $";
#endif	lint

/* arc drawing routines */

double sqrt(),atan2(),sin(),cos();
char *malloc();

/* construct a line object */
gptr mline(x1,y1,x2,y2,next)
  gptr next;
  {	register gptr p;

	if ((p = (gptr)malloc(sizeof(struct segment))) == NULL) {
	  msg("out of room!");
	  return(next);
	}
	p->s.type = SEGMENT;
	p->s.next = next;
	p->s.parent = NULL;
	p->s.x1 = x1;
	p->s.y1 = y1;
	p->s.x2 = x2;
	p->s.y2 = y2;
	p->s.angle = 0;
	p->s.cache = NULL;
	return(p);
}

/* rounds the double x off to an integer */
int round(x)
 double x;
  {	register int i;
	if (x >= 0.0) i = x+0.5; else i = x-0.5;
	return(i);
  }

/* compute center from arc end point and radius */
circcent(ang,fx,fy,tx,ty,xcp,ycp,rp)
 int ang,fx,fy,tx,ty,*xcp,*ycp;
 double *rp;
  {	double xx,yy,ss,dd,x1,y1;

	xx = tx - fx; yy = ty - fy;
	dd = sqrt(xx*xx + yy*yy);	/* length of chord */
	if (dd == 0) { *xcp = fx; *ycp = fy; *rp = 0; return; }

	/* radius of arc */
	*rp = dd / (2.0 * sin(3.141592653 * ang / 2048.0));

	/* length of perpendicular to chord */
	ss = sqrt((*rp)*(*rp) - dd*dd/4.0);

	if (ang > 1024) ss = -ss;
	x1 = (tx + fx) / 2.0;	/* compute center point of chord */
	y1 = (ty + fy) / 2.0;
	x1 -= yy*ss/dd;		/* now compute center of circle */
	y1 += xx*ss/dd;

	*xcp = round(x1); *ycp = round(y1);
}

/* calculate new arc list */
newalist(p,xa,ya,xb,yb)
  register struct segment *p;
  {	double xx,yy,aa,bb;
	double ainc,r;
 	int ix,iy,xc,yc;
	int wherex,wherey;

	if (p->cache != NULL) {
	  rmalist(p->cache);
	  p->cache = NULL;
	}

	if (p->angle == 0) return;
	circcent(p->angle,xa,ya,xb,yb,&xc,&yc,&r);

	if (r < 2.0) {
	  p->cache = mline(xa-1,ya-1,xa+1,ya-1,p->cache);
	  p->cache = mline(xa-1,ya,xa+1,ya,p->cache);
	  p->cache = mline(xa-1,ya+1,xa+1,ya+1,p->cache);
	  return;
	}

	xx = xa-xc; yy = ya-yc;
	aa = atan2(yy,xx);
	xx = xb-xc; yy = yb-yc;
	bb = atan2(yy,xx);

	/* put aa and bb into proper relationship */
/*	if (p->angle <= 1024) {
*/	  if (aa > bb) bb += 2*3.141592653;
/*	} else {
	  if (bb > aa) aa += 2*3.141592653;
	}
*/
	/* usually 32 lines/arc, but use enough to ensure that there would
	 * be at least 100 segments for a full circle.
	 */
	ainc = (bb - aa)/32.0;
	if (ainc > 0.628) ainc = 0.628;
	else if (ainc < -0.628) ainc = -0.628;

	wherex = xa; wherey = ya;
	while (1) {
	  ix = xc + r*cos(aa);
	  iy = yc + r*sin(aa);
	  p->cache = mline(wherex,wherey,ix,iy,p->cache);
	  wherex = ix; wherey = iy;
	  aa += ainc;
	  if ((ainc<0 && aa<=bb) || (ainc>=0 && aa>=bb)) break;
	};
	p->cache = mline(wherex,wherey,xb,yb,p->cache);
}
