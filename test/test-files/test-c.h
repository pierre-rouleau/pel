/* C FILE: test-c.h
**
** Purpose   : Test PEL's ability to detect C .h files.
** Created   : Thursday, April  9 2026.
*/
/* -------------------------------------------------------------------------- */
/* Module Description
** ------------------
**
** PEL detects a C header from the source of the C file and ignores comments and
** strings.  It needs to distinguish C from C++ and Objective-C as all these
** languages support files with the same file extensions.
**
** PEL code that performs the detection is `pel-cc-mode' located inside
** pel-cc.el.  It first checks if the file is an Objective-C file, then if it is
** a C++ file and if both tests fails declares it a C file.
**
**
*/

/*
**  Test: attempt to fool it: check that detection ignores comments.
**
**  @class is a comment, not code: it won't trigger Objective-C detection.
**  public:  also a comment.
*/

/* -------------------------------------------------------------------------- */
/* Dependencies
** ------------
**
**
*/

#include <stdio.h>

#define MY_ATTEMPT_TO_FOOL_IT "PEL @class"
#define SECOND_ATTEMPT        "class private"

/* -------------------------------------------------------------------------- */
/* Code
** ----
**
**
*/


/* -------------------------------------------------------------------------- */
