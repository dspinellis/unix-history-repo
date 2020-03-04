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
#include "config.h"
#include "ntp_stdlib.h"

//=======External Functions This Runner Calls=====
extern void setUp(void);
extern void tearDown(void);
extern void test_SingleDigit(void);
extern void test_MultipleDigits(void);
extern void test_Zero(void);
extern void test_MaximumUnsigned32bit(void);
extern void test_Overflow(void);
extern void test_IllegalCharacter(void);
extern void test_IllegalDigit(void);


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
  UnityBegin("octtoint.c");
  RUN_TEST(test_SingleDigit, 8);
  RUN_TEST(test_MultipleDigits, 9);
  RUN_TEST(test_Zero, 10);
  RUN_TEST(test_MaximumUnsigned32bit, 11);
  RUN_TEST(test_Overflow, 12);
  RUN_TEST(test_IllegalCharacter, 13);
  RUN_TEST(test_IllegalDigit, 14);

  return (UnityEnd());
}
