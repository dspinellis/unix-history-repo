// Header file for PlotFile3D class -*- C++ -*-
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


#ifndef _PlotFile3D_h
#ifdef __GNUG__
#pragma interface
#endif
#define _PlotFile3D_h 1

//===========================================================================
// Top-level declarations
//===========================================================================

#include <builtin.h>
#include <bool.h>
#include <assert.h>
#include <PlotFile.h>
#include "Vec3D.h"

class PlotFile3D : private PlotFile 
{
protected:

  // constant implementation parameters

  const int    umini = 0;        // Plot limits to send to 2D package
  const int    vmini = 0;
  const int    umaxi = 3120;
  const int    vmaxi = 3120;

  const double margin = 0.9;     // How much leeway to give on edge of pic

  // internal state

  Vec3D        intp;             // Internal idea of where cursor is in 3D
  bool         valid3D;          // true if 3D cursor valid

  bool         stereo;           // true for stereo images
  int          stereo_offset;    // Stereo separation in pixels (computed)
  
  Vec3D        ul,vl,wl;         // Basis vectors for the left or main image
  Vec3D        ur,vr,wr;         // Basis for alternate image, if stereo
  
  double       scale;            // Amount by which to stretch u,v to fit
  double       uorig, vorig;     // Offset for scaled u, v
  
  double*      sintab;           // tables, needed by circle, sphere
  double*      costab;
  int          ppq;              // points per quadrant in current table
  
  // helper functions

  // project 3D into 2D

  void         project(int&, int&, const Vec3D&, const Vec3D&,
                       const Vec3D& rel) const;
  
  // All clipping handled here
  
  void         line2D(const int u0, const int v0, const int u1, const int v1);
  
  // Methods to handle state of 3D cursor:  defined or undefined
  
  void         define3D();
  void         undefine3D();
  void         must_be_valid3D();

  void initialize(bool stereo, double th, double ph);
  
public:
  
  PlotFile3D(bool st=FALSE, double th=M_PI/12, double ph=M_PI/3)
      : PlotFile() { initialize(st, th, ph); }
  PlotFile3D(int fd, bool st=FALSE, double th=M_PI/12,double ph=M_PI/3)
      : PlotFile(fd) { initialize(st, th, ph); }
  PlotFile3D(const char *name,
	     bool st=FALSE, double th=M_PI/12,double ph=M_PI/3)
      : PlotFile(name) { initialize(st, th, ph); }
#ifdef _OLD_STREAMS
  PlotFile3D(FILE* fp, bool st=FALSE, double th=M_PI/12,double ph=M_PI/3)
      : PlotFile(fp) { initialize(st, th, ph); }
#endif
               ~PlotFile3D();

   // plot commands taking Vec3D args

   PlotFile3D&  space (const Vec3D& p0, const Vec3D& p1);

   PlotFile3D&  move  (const Vec3D& p);
   PlotFile3D&  cont  (const Vec3D& p);
   PlotFile3D&  line  (const Vec3D& p0, const Vec3D& p1);
   PlotFile3D&  point (const Vec3D& p);

   PlotFile3D&  box   (const Vec3D& p0, const Vec3D& p1);
   PlotFile3D&  circle(const Vec3D& center, const Vec3D& radius,
                       const int points_per_quadrant =10 );

   // This one has no PlotFile analog
   PlotFile3D&  sphere(const Vec3D& center, const Vec3D& radius,
                       const int points_per_quadrant =10 );

   // versions taking raw coordinates

   PlotFile3D&  space (const double x0, const double y0, const double z0,
                       const double x1, const double y1, const double z1);
   PlotFile3D&  move  (const double xi, const double yi, const double zi);
   PlotFile3D&  cont  (const double xi, const double yi, const double zi);
   PlotFile3D&  line  (const double x0, const double y0, const double z0,
                       const double x1, const double y1, const double z1);
   PlotFile3D&  point (const double xi, const double yi, const double zi);
   PlotFile3D&  box   (const double x0, const double y0, const double z0,
                       const double x1, const double y1, const double z1);
   PlotFile3D&  circle(const double cx, const double cy, const double cz,
                       const double rx, const double ry, const double rz,
                       const int points_per_quadrant =10 );
   PlotFile3D&  sphere(const double cx, const double cy, const double cz,
                       const double rx, const double ry, const double rz,
                       const int points_per_quadrant =10 );

  // For convenience

  PlotFile3D&  home(); // Move cursor to upper left, out of the way
  
  // These plot commands get passed right to PlotFile
  
  PlotFile3D&  erase();
  PlotFile3D&  label(const char* s);
  PlotFile3D&  linemod(const char* s);
  
  // Error handling
  void error(const char* s) const;
};

// Handling of valid3D

inline void PlotFile3D::define3D() 
{ 
  valid3D = TRUE;
}

inline void PlotFile3D::undefine3D() 
{ 
  valid3D = FALSE;
}

inline void PlotFile3D::must_be_valid3D() 
{
  assert(valid3D);
}

// Versions of routines that take coordinates as Vec3D

inline PlotFile3D& PlotFile3D::line(const Vec3D& p0, const Vec3D& p1)
{ 
  move(p0); return cont(p1);
}

inline PlotFile3D& PlotFile3D::point(const Vec3D& p)
{ 
  return line(p, p);
}

// Versions of routines that take coordinates as doubles

inline PlotFile3D& PlotFile3D::line(const double x0, const double y0, 
                                    const double z0,
                                    const double x1, const double y1, 
                                    const double z1)
{ 
  Vec3D p0(x0, y0, z0);
  Vec3D p1(x1, y1, z1);
  move(p0);
  return cont(p1);
}

inline PlotFile3D& PlotFile3D::point(const double xi, const double yi, 
                                     const double zi)
{ 
  Vec3D p(xi, yi, zi);
  return line(p, p);
}

// Versions of routines that take no coordinates

inline PlotFile3D& PlotFile3D::erase() 
{ 
  PlotFile::erase(); return home(); 
}

inline PlotFile3D& PlotFile3D::label(const char* s) 
{ 
  PlotFile::label(s); return home(); 

}

inline PlotFile3D& PlotFile3D::linemod(const char* s) 
{ 
  PlotFile::linemod(s); return *this; 
}


#endif              // _PlotFile3D_h
