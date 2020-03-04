/* AUTOGENERATED FILE. DO NOT EDIT. */

//=======Test Runner Used To Run Each Test Below=====
#define RUN_TEST(TestFunc, TestLineNum) \
{ \
  Unity.CurrentTestName = #TestFunc; \
  Unity.CurrentTestLineNumber = TestLineNum; \
  Unity.NumberOfTests++; \
  if (TEST_PROTECT()) \
  { \
      setUp(); \
      TestFunc(); \
  } \
  if (TEST_PROTECT() && !TEST_IS_IGNORED) \
  { \
    tearDown(); \
  } \
  UnityConcludeTest(); \
}

//=======Automagically Detected Files To Include=====
#include "unity.h"
#include <setjmp.h>
#include <stdio.h>

//=======External Functions This Runner Calls=====
extern void setUp(void);
extern void tearDown(void);
extern void test_main(void );
extern void test_XPASS(void);
extern void test_XFAIL(void);
extern void test_XFAIL_WITH_MESSAGE(void);
extern void test_main_incorrect(void);
extern void test_ignored(void);


//=======Suite Setup=====
static void suite_setup(void)
{
extern int change_iobufs(int);
extern int change_logfile(const char*, int);
change_iobufs(1);
change_logfile("stderr", 0);
}

//=======Test Reset Option=====
void resetTest(void);
void resetTest(void)
{
  tearDown();
  setUp();
}

char const *progname;


//=======MAIN=====
int main(int argc, char *argv[])
{
  progname = argv[0];
  suite_setup();
  UnityBegin("ut-2803.c");
  RUN_TEST(test_main, 30);
  RUN_TEST(test_XPASS, 37);
  RUN_TEST(test_XFAIL, 49);
  RUN_TEST(test_XFAIL_WITH_MESSAGE, 61);
  RUN_TEST(test_main_incorrect, 73);
  RUN_TEST(test_ignored, 77);

  return (UnityEnd());
}
