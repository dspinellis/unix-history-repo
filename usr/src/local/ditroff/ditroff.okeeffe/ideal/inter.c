#ifndef lint
static char *sccsid ="inter.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

boolean llinter (x1,y1, x2,y2, z1,w1, z2,w2, alpha, beta, collinear)
	/* if this function returns TRUE,
	/* then alpha[(x1,y1),(x2,y2)] = beta[(z1,w1),(z2,w2)] */
float x1,y1, x2,y2, z1,w1, z2,w2;
float *alpha;
float *beta;
boolean *collinear;
{
	float A1, B1, C1, A2, B2, C2, D, x, y;
	dprintf "(%f,%f) -- (%f,%f)\n", x1,y1, x2,y2);
	dprintf "(%f,%f) -- (%f,%f)\n", z1,w1, z2,w2);
	A1 = y1 - y2;
	B1 = x2 - x1;
	C1 = -B1*y1 - A1*x1;
	A2 = w1 - w2;
	B2 = z2 - z1;
	C2 = -B2*w1 - A2*z1;
	D = A1*B2 - A2*B1;
	if (fabs(D) < EPSILON) {
		*collinear = arecollinear(x1,y1,x2,y2,z1,w1);
		dprintf "%s\n", (*collinear)?"coincident":"disjoint");
		return (FALSE);
	}
	*collinear = FALSE;
	x = (B1*C2 - B2*C1)/D;
	y = (A2*C1 - A1*C2)/D;
	if (fabs(x2 - x1) > EPSILON) {
		*alpha = (x - x1)/(x2 - x1);
	} else if (fabs(y2 - y1) > EPSILON) {
		*alpha = (y - y1)/(y2 - y1);
	} else fprintf (stderr, "ideal: llinter: can't happen\n");
	if (fabs(z2 - z1) > EPSILON) {
		*beta = (x - z1)/(z2 - z1);
	} else if (fabs(w2 - w1) > EPSILON) {
		*beta = (y - w1)/(w2 - w1);
	} else fprintf (stderr, "ideal: llinter: can't happen\n");
	dprintf "intersection alpha = %f; beta = %f\n", *alpha, *beta);
	return (TRUE);
} /* llinter */

boolean lcinter (x1,y1, x2,y2, x0,y0, r, alpha1,theta1, alpha2,theta2)
	/* if this function returns TRUE,
	/* then alpha1[(x1,y1),(x2,y2)] = (x0,y0) + r*cis(theta1)
	/* and alpha2[(x1,y1),(x2,y2)] = (x0,y0) + r*cis(theta2) */
float x1,y1, x2,y2, x0,y0, r;
float *alpha1;
float *theta1;
float *alpha2;
float *theta2;
{
	float dx1, dx2, dy1, dy2;
	float A, B, C, D;

	dprintf "intersection parameters:\n");
	dprintf "%f, %f -- %f, %f\n", x1, y1, x2, y2);
	dprintf "%f, %f (%f)\n", x0, y0, r);
	r = fabs(r);
	dx1 = x1 - x0;
	dx2 = x2 - x1;
	dy1 = y1 - y0;
	dy2 = y2 - y1;
	A = dx2*dx2 + dy2*dy2;
	dprintf "A=%f\n", A);
	if (A < EPSILON) {
		if (fabs (hypot (dx1, dy1) - r) < EPSILON) {
			*alpha1 = *alpha2 = 0.0;
			*theta1 = atan2 (dy1, dx1);
			*theta2 = *theta1 = rprin (*theta1);
			dprintf "alpha1 = alpha2 = %f theta1 = theta2 = %f\n",
				*alpha1, *theta1);
			return (TRUE);
		}
		else
			return (FALSE);
	}
	B = 2*(dx1*dx2 + dy1*dy2);
	C = dx1*dx1 + dy1*dy1 - r*r;
	D = B*B - 4*A*C;
	dprintf "B=%f C=%f D=%f\n", B, C, D);
	if (D < -EPSILON)
		return (FALSE);
	if (fabs(D) < EPSILON)
		D = 0.0;
	D = sqrt(D);
	*alpha1 = (-B + D)/(2.0*A);
	*theta1 = rprin (atan2 (dy1 + *alpha1*dy2, dx1 + *alpha1*dx2));
	*alpha2 = (-B - D)/(2.0*A);
	*theta2 = rprin (atan2 (dy1 + *alpha2*dy2, dx1 + *alpha2*dx2));
	dprintf "intersection alpha1 = %f, theta1 = %f\n", *alpha1, *theta1);
	dprintf "intersection alpha2 = %f, theta2 = %f\n", *alpha2, *theta2);
	return (TRUE);
}

boolean ccinter (x0,y0,r0, x1,y1,r1, theta1,phi1, theta2,phi2)
	/* if this function returns TRUE,
	/* then (x0,y0) + r0*cis(theta1) = (x1,y1) + r1*cis(phi1)
	/* and (x0,y0) + r0*cis(theta2) = (x1,y1) + r1*cis(phi2) */
float x0,y0,r0;
float x1,y1,r1;
float *theta1;
float *phi1;
float *theta2;
float *phi2;
{
	float xcoeff, ycoeff, const;
	float u1, v1, u2, v2;
	boolean lncrc;

	dprintf "intersection parameters\n");
	dprintf "%f %f (%f)\n", x0, y0, r0);
	dprintf "%f %f (%f)\n", x1, y1, r1);

	r0 = fabs(r0);
	r1 = fabs(r1);
	xcoeff = 2*(x1 - x0);
	ycoeff = 2*(y1 - y0);
	const = r0*r0 - x0*x0 - y0*y0 - r1*r1 + x1*x1 + y1*y1;
	if (fabs(xcoeff) < EPSILON && fabs(ycoeff) < EPSILON)
		return (FALSE);
	if (fabs(xcoeff) < EPSILON) {
		u1 = 0.0;
		u2 = 1.0;
		v1 = v2 = const/ycoeff;
	} else if (fabs(ycoeff) < EPSILON) {
		v1 = 0.0;
		v2 = 1.0;
		u1 = u2 = const/xcoeff;
	} else if (fabs(const) < EPSILON) {
		u1 = 0.0;
		v1 = 0.0;
		u2 = 1.0;
		v2 = (const - 1.0/xcoeff)/ycoeff;
	} else {
		u1 = 0.0;
		v1 = const/ycoeff;
		u2 = const/xcoeff;
		v2 = 0.0;
	}
	lncrc = lcinter (u1,v1, u2,v2, x1,y1,r1, theta1,phi1, theta2,phi2);
	if (lncrc) {
		*phi1 = rprin (*phi1);
		*phi2 = rprin (*phi2);
		*theta1 = atan2 (y1 + r1*sin(*phi1) - y0, x1 + r1*cos(*phi1) - x0);
		*theta2 = atan2 (y1 + r1*sin(*phi2) - y0, x1 + r1*cos(*phi2) - x0);
		*theta1 = rprin (*theta1);
		*theta2 = rprin (*theta2);
		dprintf "intersection theta1 = %f phi1 = %f\n", *theta1, *phi1);
		dprintf "intersection theta2 = %f phi2 = %f\n", *theta2, *phi2);
		return (TRUE);
	} else
		return (FALSE);
}
