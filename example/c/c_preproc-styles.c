/* C MODULE: c_preproc-styles.c
**
** Purpose   : Shows the various C proprocessor styles and its uses.
** Created   : Thursday, October 29 2020.
** Author    : Pierre Rouleau <prouleau001@gmail.com>
** Time-stamp: <2020-11-05 14:35:13, updated by Pierre Rouleau>
**
** Copyright (C) 2020  Pierre Rouleau
**
** This program is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program.  If not, see <http://www.gnu.org/licenses/>.
**
*/
/* -------------------------------------------------------------------------- */
/* Module Description
** ------------------
**
** This is not real code.  It just holds examples of the various C preprocessor
** directive layout styles.
**
** This file is written with the CC bsd style with indent of 3 columns and a
** fill-column of 80.
*/
/* -------------------------------------------------------------------------- */
/* Style: no-indent */
/* ================ */
/* This is Emacs default style.
** None of the preprocessor directive is indented.  They all start on the first
** column regardless of their position inside the source code.
*/

void function_01(int some_arg)
{
#ifdef USING_WINDOWS
#ifdef USING_CYGWIND
   do_some_unix_call();
#else
   do_some_windows_call();
#endif
#elif USING_MAC_OS
   some_macos_call();
#else
   do_some_unix_call();
#endif
}


/* -------------------------------------------------------------------------- */
/* Style: full_indent */
/* ================== */


void function_01(int some_arg)
{
   #ifdef USING_WINDOWS
      #ifdef USING_CYGWIND
         do_some_unix_call();
      #else
         do_some_windows_call();
      #endif
   #elif USING_MAC_OS
      some_macos_call();

   #else
      do_some_unix_call();
   #endif
}

/* -------------------------------------------------------------------------- */
/* Style: indent_anchored_on_first_column */
/* ====================================== */

void function_01(int some_arg)
{
#   ifdef USING_WINDOWS
#      ifdef USING_CYGWIND
         do_some_unix_call();
#      else
         do_some_windows_call();
#      endif
#   elif USING_MAC_OS
      some_macos_call();

#   else
      do_some_unix_call();
#   endif
}


/* -------------------------------------------------------------------------- */
/* Style: indent_to_scope */
/* ====================== */

voidfunction_01(int some_arg)
{
   #ifdef USING_WINDOWS
   #ifdef USING_CYGWIND
   do_some_unix_call();
   #else
   do_some_windows_call();
   #endif
   USING_MAC_OS
   some_macos_call();
   #else
   do_some_unix_call();
   #endif
}



/* -------------------------------------------------------------------------- */
/* Avoid using pre_processor statements inside a function argument list. */
/* Because what looks like a function could be implemented as a preprocessor macro! */


/* Avoid preprocessor directive that wrap only a portion of a scope block transition. */
/* Do not do this: */

#ifdef __WIN32__
   DWORD size;
   char *absrest;
   size = GetFullPathName(relpath, PMAX, abspath, &absrest);
   if ((size == 0) || (size > PMAX))
   {
#else
   if (!realpath(relpath, abspath))
   {
#endif /* __WIN32__ */
      /* Cannot determine absolute path to escript. Try the origin.  */
      return strsave(origpath);
   }
   else
   {
      return strsave(abspath);
   }
}



/* The following is better: all blocks are complete*/
{
  #ifdef __WIN32__
    DWORD size;
    char *absrest;
    size = GetFullPathName(relpath, PMAX, abspath, &absrest);
    if ((size == 0) || (size > PMAX))
  #else
    if (!realpath(relpath, abspath))
  #endif /* __WIN32__ */
  {
      /* Cannot determine absolute path to escript. Try the origin.  */
      return strsave(origpath);
  }
  else
  {
      return strsave(abspath);
  }
}
