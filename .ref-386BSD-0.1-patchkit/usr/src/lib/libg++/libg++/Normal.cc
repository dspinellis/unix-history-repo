/* 
Copyright (C) 1988 Free Software Foundation
    written by Dirk Grunwald (grunwald@cs.uiuc.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/
#ifdef __GNUG__
#pragma implementation
#endif
#include <builtin.h>
#include <Random.h>

#include <Normal.h>
//
//	See Simulation, Modelling & Analysis by Law & Kelton, pp259
//
//	This is the ``polar'' method.
// 

double Normal::operator()()
{
    
    if (haveCachedNormal == 1) {
	haveCachedNormal = 0;
	return(cachedNormal * pStdDev + pMean );
    } else {
	
	for(;;) {
	    double u1 = pGenerator -> asDouble();
	    double u2 = pGenerator -> asDouble();
	    double v1 = 2 * u1 - 1;
	    double v2 = 2 * u2 - 1;
	    double w = (v1 * v1) + (v2 * v2);
	    
//
//	We actually generate two IID normal distribution variables.
//	We cache the one & return the other.
// 
	    if (w <= 1) {
		double y = sqrt( (-2 * log(w)) / w);
		double x1 = v1 * y;
		double x2 = v2 * y;
		
		haveCachedNormal = 1;
		cachedNormal = x2;
		return(x1 * pStdDev + pMean);
	    }
	}
    }
}

