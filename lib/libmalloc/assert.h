/* $Id: assert.h,v 1.7 1993/05/23 03:38:27 moraes Exp $ */
#ifndef __ASSERT_H__
#define __ASSERT_H__
#ifdef DEBUG
#define ASSERT(p, s) ((void) (!(p) ? __m_botch(s, __FILE__, __LINE__) : 0))
extern int __m_botch proto((const char *, const char *, int));
#else
#define ASSERT(p, s)
#endif
#endif /* __ASSERT_H__ */ /* Do not add anything after this line */
