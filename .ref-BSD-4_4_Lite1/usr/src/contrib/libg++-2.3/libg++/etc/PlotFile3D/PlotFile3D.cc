// Source file for PlotFile3D class -*- C++ -*-
/* 
Copyright (C) 1990 Free Software Foundation
    written by J. Thomas Ngo, Harvard University

This file is part of the GNU C++ Library.

The GNU C++ Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor accepts
responsibility to anyone for the consequences of using it or for
whether it serves any particular purpose or works at all, unless he
says so in writing.  Refer to the GNU General Public License for full
details.

Everyone is granted permission to copy, modify and redistribute The
GNU C++ Library, but only under the conditions described in the GNU
General Public License.  A copy of this license is supposed to have
been given to you along with The GNU C++ Library so you can know your
rights and responsibilities.  It should be in a file named COPYING.
Among other things, the copyright notice and this notice must be
preserved on all copies.  
*/

// An analog of the PlotFile class, but written for 3D graphics.  In
// the initializer you specify a camera location, and decide whether
// you want stereo viewing.  Most other methods are analogous to the
// unix plot(3) commands, except that they take 3D positions.

#ifdef __GNUG__
#pragma implementation
#endif
#include "PlotFile3D.h"

// Error handling
void PlotFile3D::error(const char* s) const 
{
  (*lib_error_handler)("PlotFile3D",s);
}

//===========================================================================
// Constructor
//===========================================================================

static void _find_basis(Vec3D& u, Vec3D& v, Vec3D& w,
                        const double th, const double ph)
{
    double sth = sin(th), cth = cos(th);
    double sph = sin(ph), cph = cos(ph);
    u = Vec3D( sph, cph, 0 );
    v = Vec3D( -sth * cph, sth * sph, cth );
    w = u ^ v;
}

void PlotFile3D::initialize(bool st, double th, double ph)
{
    stereo = st;
    ppq = 0;
    valid3D = FALSE;
    if( stereo ) {
        const double stereo_separation = 2.5 * M_PI / 180.0;
        _find_basis( ul,vl,wl, th, ph-stereo_separation );
        _find_basis( ur,vr,wr, th, ph+stereo_separation );
    } else {
        _find_basis( ul,vl,wl, th, ph                   );
        ur = ul; vr = vl; wr = wl;
    }
    undefine3D();
}

// destructor

PlotFile3D::~PlotFile3D() 
{
  if (ppq != 0) { delete sintab; delete costab; }
}

//===========================================================================
// Set plot limits via space()
//===========================================================================

PlotFile3D& PlotFile3D::space(const Vec3D& pt0, const Vec3D& pt1)
{
    // Set up 2D plot package
    PlotFile::space(umini,vmini,umaxi,vmaxi);
    
    // Find 2D rectangle bounding the box specified by user

    double umin  = (pt0.x() * ul.x()) <? (pt0.x() * ur.x())
                <? (pt1.x() * ul.x()) <? (pt1.x() * ur.x());
    double umax  = (pt0.x() * ul.x()) >? (pt0.x() * ur.x())
                >? (pt1.x() * ul.x()) >? (pt1.x() * ur.x());
    double vmin  = (pt0.x() * vl.x()) <? (pt0.x() * vr.x())
                <? (pt1.x() * vl.x()) <? (pt1.x() * vr.x());
    double vmax  = (pt0.x() * vl.x()) >? (pt0.x() * vr.x())
                >? (pt1.x() * vl.x()) >? (pt1.x() * vr.x());

    umin        += (pt0.y() * ul.y()) <? (pt0.y() * ur.y())
                <? (pt1.y() * ul.y()) <? (pt1.y() * ur.y());
    umax        += (pt0.y() * ul.y()) >? (pt0.y() * ur.y())
                >? (pt1.y() * ul.y()) >? (pt1.y() * ur.y());
    vmin        += (pt0.y() * vl.y()) <? (pt0.y() * vr.y())
                <? (pt1.y() * vl.y()) <? (pt1.y() * vr.y());
    vmax        += (pt0.y() * vl.y()) >? (pt0.y() * vr.y())
                >? (pt1.y() * vl.y()) >? (pt1.y() * vr.y());

    umin        += (pt0.z() * ul.z()) <? (pt0.z() * ur.z())
                <? (pt1.z() * ul.z()) <? (pt1.z() * ur.z());
    umax        += (pt0.z() * ul.z()) >? (pt0.z() * ur.z())
                >? (pt1.z() * ul.z()) >? (pt1.z() * ur.z());
    vmin        += (pt0.z() * vl.z()) <? (pt0.z() * vr.z())
                <? (pt1.z() * vl.z()) <? (pt1.z() * vr.z());
    vmax        += (pt0.z() * vl.z()) >? (pt0.z() * vr.z())
                >? (pt1.z() * vl.z()) >? (pt1.z() * vr.z());
   
    // Map this rectangle on to umini et al. using as much space as
    // possible and centering.  Also, if stereo was requested, split
    // the window right down the middle and try to fit the picture in.
    int pixwidth  = umaxi - umini;
    int pixheight = vmaxi - vmini;
    if (stereo) pixwidth /= 2;
    double uscale = pixwidth  / (umax-umin);
    double vscale = pixheight / (vmax-vmin);
    if (uscale <= vscale) scale = uscale;
    else { scale = vscale; pixwidth = int(pixwidth * vscale/uscale); }
    scale *= margin;
    uorig = 0.5 * (pixwidth  - scale*(umin+umax));
    vorig = 0.5 * (pixheight - scale*(vmin+vmax));
    if (stereo) stereo_offset = pixwidth;
    
    // Set state appropriately
    return home();
}


//===========================================================================
// move() and cont() are used as primitives by the other routines
//===========================================================================

PlotFile3D& PlotFile3D::move(const Vec3D& p)
{
    intp = p;
    define3D();
    return *this;
}


void PlotFile3D::project(int& ui,int& vi, const Vec3D& u, const Vec3D& v,
                          const Vec3D& rel) const
{
  ui = int(rel * u * scale + uorig);
  vi = int(rel * v * scale + vorig);
}

static inline bool _clip(int& a, const int amin, const int amax,
                         int& b, const int afar, const int bfar)
{
    double frac = 0;
    if( a < amin ) {
        if( afar <= amin ) return FALSE;
        frac = ((double)amin-a)/(afar-a);
        a = amin;
    } else if( a > amax ) {
        if( afar >= amax ) return FALSE;
        frac = ((double)amax-a)/(afar-a);
        a = amax;
    }
    if( frac != 0 ) b += int(frac*(bfar-b));
    return TRUE;
}

void PlotFile3D::line2D(int u0, int v0, int u1, int v1)
{
    // Clip against each edge in turn
    if( !_clip(u0,umini,umaxi,v0,u1,v1) ) return;
    if( !_clip(v0,vmini,vmaxi,u0,v1,u1) ) return;
    if( !_clip(u1,umini,umaxi,v1,u0,v0) ) return;
    if( !_clip(v1,vmini,vmaxi,u1,v0,u0) ) return;

    // Draw clipped segment
    PlotFile::move(u0,v0); PlotFile::cont(u1,v1);
}


PlotFile3D& PlotFile3D::cont(const Vec3D& p)
{
    must_be_valid3D();
    int u0, v0, u1, v1;
    if (stereo) {
        project(u0,v0, ul,vl, intp);
        project(u1,v1, ul,vl, p);
        line2D(u0,v0,u1,v1);
        project(u0,v0, ur,vr, intp); u0 += stereo_offset;
        project(u1,v1, ur,vr, p);    u1 += stereo_offset;
        line2D(u0,v0,u1,v1);
    } else {
        project(u0,v0, ul,vl, intp);
        project(u1,v1, ul,vl, p);
        line2D(u0,v0,u1,v1);
    }
    intp = p;
    return *this;
}


//===========================================================================
// These last commands are generated by sequences of move() and cont()
//===========================================================================


PlotFile3D& PlotFile3D::box(const Vec3D& p0, const Vec3D& p1)
{
  // The corners:
  Vec3D x0y0z1 (p0.x(),p0.y(),p1.z()); 
  Vec3D x0y1z0 (p0.x(),p1.y(),p0.z()); 
  Vec3D x0y1z1 (p0.x(),p1.y(),p1.z()); 
  Vec3D x1y0z0 (p1.x(),p0.y(),p0.z()); 
  Vec3D x1y0z1 (p1.x(),p0.y(),p1.z());
  Vec3D x1y1z0 (p1.x(),p1.y(),p0.z()); 

  // Bottom rectangle
  move(p0);
  cont(x1y0z0);
  cont(x1y1z0);
  cont(x0y1z0);
  cont(p0);

  // One leg
  cont(x0y0z1);
  
  // Top rectangle
  cont(x1y0z1);
  cont(p1);
  cont(x0y1z1);
  cont(x0y0z1);

  // Three other legs
  line(x1y0z0, x1y0z1);
  line(x1y1z0, p1);
  line(x0y1z0, x0y1z1);

  // Set state
  undefine3D(); 
  return *this;
}


PlotFile3D& PlotFile3D::circle(const Vec3D& center, const Vec3D& rad,
                               const int points_per_quadrant )
{
  int i;
  
  // Build trig lookup table if necessary

  if( ppq != points_per_quadrant ) 
  {
    if( ppq != 0 ) { delete sintab; delete costab; }
    ppq = points_per_quadrant;
    sintab = new double[ppq]; costab = new double[ppq];
    for( i=0; i<ppq; i++ ) sintab[i] = sin(double(i)*M_PI_2/double(ppq));
    for( i=0; i<ppq; i++ ) costab[i] = cos(double(i)*M_PI_2/double(ppq));
  }

  // Find radius vector normal to circle, and two vectors
  // orthogonal to it

  Vec3D nz = rad;

  double radius = mod(nz); 
  nz /= radius;

  double tx, ty, tz;

  if( nz.x() <= nz.y() && nz.x() <= nz.z() ) 
  {
    tx = 0; ty = nz.z(); tz = -nz.y();
  }
  else if ( nz.y() <= nz.z() )
  {
    tx = nz.z(); ty = 0; tz = -nz.x();
  }
  else
  {
    tx = nz.y(); ty = -nz.x(); tz = 0;
  }
  
  Vec3D nx (tx, ty, tz);

  nx /= mod(nx);      // Unit vector orthogonal to nz

  Vec3D ny = nz ^ nx;       // Unit vector orthogonal to ny and nz

  nx *= radius; ny *= radius; // Now they're basis vectors for circle


  // Iterate...
  Vec3D cnx(center+nx);
  move(cnx);

  for( i=0; i<ppq; i++ ) cont(center + nx * costab[i] + ny * sintab[i]);
  for( i=0; i<ppq; i++ ) cont(center + ny * costab[i] - nx * sintab[i]);
  for( i=0; i<ppq; i++ ) cont(center - nx * costab[i] - ny * sintab[i]); 
  for( i=0; i<ppq; i++ ) cont(center - ny * costab[i] + nx * sintab[i]); 

  cont(cnx);

  // Set state
  undefine3D(); 
  return *this;
}


//===========================================================================
// Methods not found in plot(3)--particular to this package
//===========================================================================

PlotFile3D& PlotFile3D::home()  // Cursor to upper left corner
{
    int homex = umini;
    int homey = int(vmaxi + 0.05 * (vmini-vmaxi));
    PlotFile::move(homex,homey);
    undefine3D();
    return *this;
}


PlotFile3D& PlotFile3D::sphere(const Vec3D& center, const Vec3D& rad,
                               const int points_per_quadrant)
{
  Vec3D eyeline = wl * mod(rad);
  circle( center, eyeline, points_per_quadrant );
  return circle(center, rad, points_per_quadrant );
}


//===========================================================================
// Raw coordinate versions of things
//===========================================================================

PlotFile3D& PlotFile3D::space(const double x0, const double y0, 
                              const double z0,
                              const double x1, const double y1, 
                              const double z1)
{
  Vec3D p0(x0, y0, z0);
  Vec3D p1(x1, y1, z1);
  return space(p0, p1);
};

PlotFile3D& PlotFile3D::move(const double xi, const double yi, const double zi)
{
    intp = Vec3D(xi, yi, zi);
    define3D();
    return *this;
}

PlotFile3D& PlotFile3D::cont(const double xi, const double yi, const double zi)
{
  Vec3D p(xi, yi, zi);
  return cont(p);
}

PlotFile3D&  PlotFile3D::box(const double x0, const double y0, const double z0,
                             const double x1, const double y1, const double z1)
{
  Vec3D x0y0z0(x0, y0, z0);
  Vec3D x1y1z1(x1, y1, z1);
  return box(x0y0z0, x1y1z1);
}

PlotFile3D& PlotFile3D::circle(const double cx,const double cy,const double cz,
                   const double rx,const double ry,const double rz,
                   const int points_per_quadrant )
    // cx,cy,cz:  position of circle's center
    // rx,ry,rz:  length is circle's radius; direction is normal to circle
{
  Vec3D center(cx, cy, cz);
  Vec3D rad(rx, ry, rz);
  return circle(center, rad, points_per_quadrant);
}

PlotFile3D& PlotFile3D::sphere(const double cx,const double cy,const double cz,
                   const double rx,const double ry,const double rz,
                   const int points_per_quadrant)
{
  Vec3D center(cx, cy, cz);
  Vec3D rad(rx, ry, rz);
  return sphere(center, rad, points_per_quadrant);
}

//===========================================================================
// Test program
//===========================================================================

#ifdef TEST
#include <stream.h>
main()
{
  PlotFile3D foo(fopen("test.pl", "w"));
  foo.space(-1000,-1000,-1000,1000,1000,1000).erase();
      
  // Draw and label
  foo.linemod("longdashed").box(-1000,-1000,-1500,1000,1000,0);
  foo.linemod("solid").sphere(0,0,500,0,0,500,30);
  foo.linemod("dotted").circle(0,0,500,0,0,800,50);
  char title[] = "Test of PlotFile3D package";
  foo.home().label(title);
  
}
#endif
