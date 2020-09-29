/*
 * A.Ohki 2019/04/14 ================================================
 *
 * included patches by http://ikumi.que.jp/cmp/emacs.html
 *
 * cleanup obsolete & dynamic-xxx() related lines
 * change  #ifndef WITHOUT_KKCP --> #ifdef WITH_KKCP
 * adapting emacs-module API (very first env_25)
 *
 * from emacs-27
 *	XINT(ival) -> XFIXNUM(ival)
 *	make_number(num) -> make_fixnum(num)
 *
 * compile options
 *	-DAS_EMACS_MODULES	make canna.so for emacs-module API
 *
 *	-DWITH_KKCP	include KKCP interface
 */

/* CANNA interface for GNU Emacs 25 or later(?)

   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1996,1997,1999 MORIOKA Tomohiko

   This file is not part of GNU Emacs.
   
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU Emacs; see the file COPYING.  If not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

  Authors: Akira Kon (kon@d1.bs2.mt.nec.co.jp)
           Ichiro Hirakura (hirakura@uxp.bs2.mt.nec.co.jp)
	   modified by MORIOKA Tomohiko <morioka@jaist.ac.jp>

  Functions defined in this file are

  (canna-key-proc key)
 		key: single STRING
 		RETURNS:
			 Length of converted string if no error occurs.
			 Error string if error occurs.
 		DESCRIPTION:
			 Convert a key input to a set of strings.  The
			 strings contain both well-formed string and a
			 intermediate result to show the translation
			 information to a user.  converted strings are
			 stored in specific variables.

  (canna-initialize)
  		RETURNS:
			List of the following things:
			- list of keys to toggle Japanese-mode
			- error message
			- list of warning messages
		DESCRIPTION:
			Initialize ``canna'', which is a kana-to-kanji
			converter for GNU Emacs.  The first arg
			specifies if inserting space character between
			BUNSETSU when candidates are displayed.  The
			second arg specifies server.  The third arg
			specifies a file which will be used as a
			customization description.  If nil is
			specified for each arg, the default value will
			be used.

  (canna-finalize)
		RETURNS:
			list of warning messages
		DESCRIPTION:
			finalize ``canna'', which is a kana-to-kanji
			converter for GNU Emacs.  This cause to write
			miscellaneous informations to kana-to-kanji
			dictionary.

  (canna-touroku-string string)
		string:
			String to register to a dictionary.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Register Kanji words into kana-to-kanji
			conversion dictionary.

  (canna-set-width width)
		width:
			Column width of the place where the candidates
			of kana-to-kanji conversion will be shown.
		RETURNS:
			nil
		DESCRIPTION:
			Set status-line width information, which is
			used to display kanji candidates.

  (canna-change-mode num)
		num:
			The mode number of Canna.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Change Japanese pre-edit mode.

  (canna-store-yomi yomi roma)
		yomi:
			``Yomi'' to be stored.
		roma:
			``Romaji'' which corresponds to the ``Yomi''.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Store yomi characters as a YOMI of
			kana-to-kanji conversion.

  (canna-do-function num ch)
		num:
			A function number to be called.
		ch:
			A character will be specified in order to feed
			the character to the function if the function
			needs an input character.
		RETURNS:
			The same thing returns as canna-key-proc does.
		DESCRIPTION:
			Do specified function at current mode.

  (canna-parse string)
		string:
			To be parsed.
		RETURNS:
			List of warning messages.
		DESCRIPTION:
			Parse customize string.

  (canna-query-mode)
		RETURNS:
			A string which indicate the current mode.
		DESCRIPTION:
			Get current mode string.

#ifdef	WITH_KKCP
  Functions below are used for KKCP compatible library.  These
  functions provides a base kana-to-kanji conversion system for EGG.
  These functions may be used when users want to change the engine
  from Wnn to Canna without changing user interface of Japanese input.

  (canna-henkan-begin)
  (canna-henkan-next)
  (canna-bunsetu-henkou)
  (canna-henkan-kakutei)
  (canna-henkan-end)
  (canna-henkan-quit)
#endif	WITH_KKCP

 */

#ifndef lint
static char rcs_id[] = "$Id: canna.c,v 1.36 1999/03/02 18:52:41 morioka Exp $";
#endif

#ifdef AS_EMACS_MODULES /* ================================================== */
/*
 * stuff for compiled as canna.so,
 * which will be loaded via Lisp function `module-load()'.
 */

/*
 * emacs 25 to 26:
 *	emacs_value is an encoded raw lisp pointer.
 *	it is valid to check equality with `a == b'.
 *
 * emacs 27:
 *	emacs_value is changed as
 *		struct emacs_value_tag { Lisp_Object v; } *emacs_value ;
 *	use `module_eq(a, b)' for equality check, instead of `a->v == b->v'.
 *	the latter form would be slightly efficient, but loses
 *	compatibility between different versions.
 *
 *	since emacs_value is valid only for an each call from emacs-module side,
 *	emacs_value of interned symbols must be passed to
 *	`module_make_global_ref()' to get a globally valid value.
 *
 *	mule_extract_string()/mule_make_string() perform base64 de/encoding
 *	to exchange euc-coded string transparently with canna.el.
 */
#include "emacs-module.h"

#define module_nil	((emacs_value)0)

static emacs_env *crnt_env;	/* module environmnet */

/* API defined by emacs-module env_25 ===== */
inline static void
module_non_local_exit_signal(emacs_value non_local_exit_symbol,
			     emacs_value non_local_exit_data)
{
 (*(crnt_env->non_local_exit_signal))(crnt_env,
				      non_local_exit_symbol,
				      non_local_exit_data);
}

inline static emacs_value
module_make_function(ptrdiff_t min_arity,
                     ptrdiff_t max_arity,
                     emacs_value (*function) (emacs_env *env,
                                              ptrdiff_t nargs,
                                              emacs_value args[],
                                              void *),
                     const char *documentation,
                     void *data)
{
  return (*(crnt_env->make_function))(crnt_env,
				     min_arity, max_arity,
				     function, documentation, data);
}

inline static emacs_value
module_funcall(emacs_value function, ptrdiff_t nargs, emacs_value args[])
{
  return (*(crnt_env->funcall))(crnt_env, function, nargs, args);
}

inline static emacs_value
module_intern(const char *symbol_name)
{
  /*
   * make it global reference,
   * since interned symbols are used thoughout the life time of canna.so.
   */
  return (*(crnt_env->make_global_ref))(crnt_env,
					(*(crnt_env->intern))(crnt_env,
							      symbol_name));
}

inline static emacs_value
module_type_of(emacs_value value)
{
  return (*(crnt_env->type_of))(crnt_env, value);
}

inline static bool
module_is_not_nil(emacs_value value)
{
  return (value != module_nil) && (*(crnt_env->is_not_nil))(crnt_env, value);
}

inline static bool
module_eq(emacs_value a, emacs_value b)
{
  return (*(crnt_env->eq))(crnt_env, a, b);
}

inline static intmax_t
module_extract_integer(emacs_value value)
{
  return (*(crnt_env->extract_integer))(crnt_env, value);
}

inline static emacs_value
module_make_integer(intmax_t value)
{
  return (*(crnt_env->make_integer))(crnt_env, value);
}

inline static double
module_extract_float(emacs_value value)
{
  return (*(crnt_env->extract_float))(crnt_env, value);
}

inline static emacs_value
module_make_float(double value)
{
  return (*(crnt_env->make_float))(crnt_env, value);
}

inline static bool
module_copy_string_contents(emacs_value value,
                            char *buffer,
                            ptrdiff_t *size_inout)
{
  return (*(crnt_env->copy_string_contents))(crnt_env,
					     value, buffer, size_inout);
}
    
inline static emacs_value
module_make_string(const char *contents, ptrdiff_t length)
{
  return (*(crnt_env->make_string))(crnt_env, contents, length);
}

inline static emacs_value
module_make_user_ptr(void (*fin) (void *), void *ptr)
{
  return (*(crnt_env->make_user_ptr))(crnt_env, fin, ptr);
}

inline static void*
module_get_user_ptr(emacs_value uptr)
{
  return (*(crnt_env->get_user_ptr))(crnt_env, uptr);
}

inline static void
module_set_user_ptr(emacs_value uptr, void *ptr)
{
  (*(crnt_env->set_user_ptr))(crnt_env, uptr, ptr);
}

inline static void*
module_get_user_finalizer(emacs_value uptr)
{
  return (*(crnt_env->get_user_finalizer))(crnt_env, uptr);
}

inline static void
module_set_user_finalizer(emacs_value uptr, void (*fin) (void *))
{
  (*(crnt_env->set_user_finalizer))(crnt_env, uptr, fin);
}

inline static emacs_value
module_vec_get(emacs_value vec, ptrdiff_t i)
{
  return (*(crnt_env->vec_get))(crnt_env, vec, i);
}

inline static void
module_vec_set(emacs_value vec, ptrdiff_t i, emacs_value val)
{
  (*(crnt_env->vec_set))(crnt_env, vec, i, val);
}

inline static ptrdiff_t
module_vec_size(emacs_value vec)
{
  return (*(crnt_env->vec_size))(crnt_env, vec);
}
/* ===== API defined by emacs-module env_25 */

#define Lisp_Object emacs_value

/* copied from lisp.h for DEFUN() ========= */
struct Lisp_Subr
  {
    union {
      Lisp_Object (*a0) (void);
      Lisp_Object (*a1) (Lisp_Object);
      Lisp_Object (*a2) (Lisp_Object, Lisp_Object);
      Lisp_Object (*a3) (Lisp_Object, Lisp_Object, Lisp_Object);
    } function;
    short min_args, max_args;
    const char *symbol_name;
    const char *intspec;
    const char **doc; /* XXX was char *doc */
  };

#define GCALIGNMENT 8
#define DEFUN(lname, fnname, sname, minargs, maxargs, intspec, doc)     \
   static Lisp_Object fnname DEFUN_ARGS_ ## maxargs ;                   \
   static struct Lisp_Subr alignas (GCALIGNMENT) sname =                \
     { { .a ## maxargs = fnname },                                      \
       minargs, maxargs, lname, intspec, & doc_ ## fnname};             \
   static Lisp_Object fnname

#define DEFUN_ARGS_0    (void)
#define DEFUN_ARGS_1    (Lisp_Object)
#define DEFUN_ARGS_2    (Lisp_Object, Lisp_Object)
#define DEFUN_ARGS_3    (Lisp_Object, Lisp_Object, Lisp_Object)
/* ========= copied from lisp.h for DEFUN() */

#define CHECK_NUMBER(x)                         \
   if (((x) == module_nil) || !module_eq(module_type_of(x), Qinteger)) { \
      type_error(Qintegerp, x);                 \
      return 0;                                 \
   }

#define CHECK_STRING(x)                         \
   if (((x) == module_nil) || !module_eq(module_type_of(x), Qstring)) { \
      type_error(Qstringp, x);                  \
      return 0;                                 \
   }

#define SET_OBJ(var,obj)	(void) set_obj(var, obj)
#define SET_INT(var,num)	(void) set_obj(var, module_make_integer(num))

#define DEFVAR_LISP(str, name, doc)	defvar_doc(name=module_intern(str),doc)
#define DEFVAR_INT(str, name, doc)	defvar_doc(name=module_intern(str),doc)
#define DEFVAR_BOOL(str, name, doc)	defvar_doc(name=module_intern(str),doc)
#define intern(str)		module_intern(str)
#define XFIXNUM(ival)		module_extract_integer(ival)
#define make_fixnum(num)	module_make_integer(num)
#define NILP(v)		(!module_is_not_nil(v))

int plugin_is_GPL_compatible;   /* required symbol to indicate GPL software */

/* constant symbols */
static emacs_value Qt;
static emacs_value Qnil;
static emacs_value Qinteger;
static emacs_value Qintegerp;
static emacs_value Qstring;
static emacs_value Qstringp;

static emacs_value _Fset;
static emacs_value _Fsymval;
static emacs_value _Fcons;

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdalign.h>

#include "canna-module.h"

static emacs_value
func_call_a0(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *vp)
{
  emacs_env *old_env = crnt_env;
  emacs_value (*a0) (void) = vp;
  emacs_value val;

  crnt_env = env;
  val = (*a0)();
  crnt_env = old_env;
  return val;
}

static emacs_value
func_call_a1(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *vp)
{
  emacs_env *old_env = crnt_env;
  emacs_value (*a1) (Lisp_Object) = vp;
  emacs_value val;

  crnt_env = env;
  val = (*a1)(nargs>=1?args[0]:module_nil);
  crnt_env = old_env;
  return val;
}

static emacs_value
func_call_a2(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *vp)
{
  emacs_env *old_env = crnt_env;
  emacs_value (*a2) (Lisp_Object, Lisp_Object) = vp;
  emacs_value val;

  crnt_env = env;
  val = (*a2)(nargs>=1?args[0]:module_nil,
	      nargs>=2?args[1]:module_nil);
  crnt_env = old_env;
  return val;
}

static emacs_value
func_call_a3(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *vp)
{
  emacs_env *old_env = crnt_env;
  emacs_value (*a3) (Lisp_Object, Lisp_Object, Lisp_Object) = vp;
  emacs_value val;

  crnt_env = env;
  val = (*a3)(nargs>=1?args[0]:module_nil,
	      nargs>=2?args[1]:module_nil,
	      nargs>=3?args[2]:module_nil);
  crnt_env = old_env;
  return val;
}

static emacs_value
func_call_nop(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *vp)
{
  return Qnil;
}

static void
defsubr(struct Lisp_Subr *def)
{
  static emacs_value _Ffset = NULL;
  emacs_value args[2];

  if (_Ffset == NULL) {
    _Ffset =  module_intern("fset");
  }
  args[0] = module_intern(def->symbol_name);
  args[1] = module_make_function(def->min_args, def->max_args,
				 (def->max_args == 0)?func_call_a0:
				 (def->max_args == 1)?func_call_a1:
				 (def->max_args == 2)?func_call_a2:
				 (def->max_args == 3)?func_call_a3:
				 func_call_nop, /* XXX */
				 *(def->doc),
				 def->function.a0);
  (void) module_funcall(_Ffset, 2, args);
}

inline static emacs_value
set_obj(emacs_value var, emacs_value obj)
{
  emacs_value args[2];

  args[0] = var;
  args[1] = obj;
  return module_funcall(_Fset, 2, args);
}

/* (symbol-value var) */
inline static emacs_value
value_of(emacs_value var)
{
  emacs_value args[1];

  args[0] = var;
  return module_funcall(_Fsymval, 1, args);
}

inline static emacs_value
Fcons(emacs_value a, emacs_value b)
{
  emacs_value args[2];

  args[0] = a;
  args[1] = b;
  return module_funcall(_Fcons, 2, args);
}

static void
type_error(emacs_value pred, emacs_value data)
{
  static emacs_value Qwrong_type_argument = NULL;

  if (Qwrong_type_argument == NULL) {
    Qwrong_type_argument = module_intern("wrong-type-argument");
  }
  (void) module_non_local_exit_signal(Qwrong_type_argument,
			       Fcons(pred,
				      Fcons (data, Qnil)));
}

static void
defvar_doc(emacs_value var, char *str)
{
  static emacs_value _Fput = NULL, Qvar_doc, empty_str;
  emacs_value args[3];

  if (_Fput == NULL) {
    _Fput = module_intern("put");
    Qvar_doc = module_intern("variable-documentation");
    empty_str = module_make_string("", 0);
  }
  args[0] = var;
  args[1] = Qvar_doc;
  if (str && str[0])
    args[2] = module_make_string(str, strlen(str));
  else
    args[2] = empty_str;
  (void) module_funcall(_Fput, 3, args);
}

static emacs_value
Fding(emacs_value a)
{
  static emacs_value _Fding = NULL;
  emacs_value args[1];
  if (_Fding == NULL) {
    _Fding =  module_intern("ding");
  }
  args[0] = a;
  return module_funcall(_Fding, 1, args);
}

#ifdef WITH_KKCP
static emacs_value
Fsetcdr(emacs_value a, emacs_value b)
{
  static emacs_value _Fsetcdr = NULL;
  emacs_value args[2];

  if (_Fsetcdr == NULL) {
    _Fsetcdr =  module_intern("setcdr");
  }
  args[0] = a;
  args[1] = b;
  return module_funcall(_Fsetcdr, 2, args);
}
#endif

/*
 * called just after module loading
 */
static void syms_of_canna ();

int
emacs_module_init(struct emacs_runtime *ert)
{
  crnt_env = (*(ert->get_environment))(ert);

  /* setup basic symbols */
  Qt = module_intern("t");
  if (Qt == module_nil) {
    (void) write(2, "*** emacs-module not working ***\n", 33);
    exit(0);
  }
  Qnil = module_intern("nil");
  Qinteger  = module_intern("integer");
  Qintegerp = module_intern("integerp");
  Qstring  = module_intern("string");
  Qstringp = module_intern("stringp");

  _Fset  =  module_intern("set");
  _Fsymval =  module_intern("symbol-value");
  _Fcons =  module_intern("cons");

  /* setup canna module */
  syms_of_canna ();

  crnt_env = NULL;

  return 0;
}
#else /* !AS_EMACS_MODULES ================================================== */
/*
 * stuff for statically linked to emacs itself
 *
 * lisp.h and emacs.c must be patched to declare/call syms_of_canna()
 */

#include "config.h"
#include "lisp.h"
#include "coding.h"

#define SET_OBJ(var,obj)	var = obj
#define SET_INT(var,ival)	var = ival

#endif /* AS_EMACS_MODULES! ================================================= */

#define ISO_CODE_SS2 0x8E	/* defined in  coding.c */
#define ISO_CODE_SS3 0x8F	/* defined in  coding.c */

#define CANNA3_7
#ifdef CANNA3_7
# define CANNA_NEW_WCHAR_AWARE
#endif

#define IROHA_BC
#include "canna/jrkanji.h"
#include "canna/RK.h"

#ifndef CANNA3_7
extern char *jrKanjiError;
#endif

#define KEYTOSTRSIZE 2048
static unsigned char buf[KEYTOSTRSIZE];
static char **warning;

#ifdef WITH_KKCP
static int IRCP_context;
#endif /* WITH_KKCP */

static Lisp_Object storeResults(unsigned char *, int, jrKanjiStatus *);
#ifdef WITH_KKCP
static Lisp_Object kanjiYomiList(int, int);
#endif /* WITH_KKCP */
static Lisp_Object mule_make_string(unsigned char *, int);
static void mule_extract_string(Lisp_Object, char *, int, int);

static int mule_strlen(unsigned char *, int);

/* Lisp functions definition */

DEFUN ("canna-key-proc", Fcanna_key_proc, Scanna_key_proc,
       1, 1, 0,
       doc: /* Translate a key input CH to a set of strings.
The strings contain both well-formed string and intermediate result
to show the translation information to a user.  Converted strings are
stored in specific variables.

Return the length of converted string if no error occurs, while
the error string if error occurs.  */)
  (Lisp_Object ch)
{
  jrKanjiStatus ks;
  int len;

  CHECK_NUMBER (ch);
  len = jrKanjiString(0, XFIXNUM (ch), buf, KEYTOSTRSIZE, &ks);
  return storeResults(buf, len, &ks);
}

static Lisp_Object
storeResults(unsigned char *buf, int len, jrKanjiStatus *ks)
{
  Lisp_Object val = Qnil;

  if (len < 0) { /* Error detected */
    val = mule_make_string((unsigned char*)jrKanjiError, strlen(jrKanjiError));
  }
  else {
    /* 確定した文字列 */
    SET_OBJ(Vcanna_kakutei_string, mule_make_string(buf, len));
    val = make_fixnum(len);
    /* 確定した文字列の読みの情報... */
    SET_OBJ(Vcanna_kakutei_yomi,  Qnil);
    SET_OBJ(Vcanna_kakutei_romaji, Qnil);
    if (ks->info & KanjiYomiInfo) {
      unsigned char *p = buf + len + 1;
      int yomilen = strlen(p);

      if (len + yomilen + 1 < KEYTOSTRSIZE) {
	int yomilen2;

	SET_OBJ(Vcanna_kakutei_yomi, mule_make_string(p, yomilen)); /* 読み */
	p += yomilen + 1;
	yomilen2 = strlen(p);
	if (len + yomilen + yomilen2 + 2 < KEYTOSTRSIZE) {
	  SET_OBJ(Vcanna_kakutei_romaji, mule_make_string(p, yomilen2)); /* ローマ字 */
	}
      }
    }

    /* 候補表示の文字列です。*/
    SET_OBJ(Vcanna_henkan_string, Qnil);
    if (ks->length >= 0) {
      SET_OBJ(Vcanna_henkan_string, mule_make_string(ks->echoStr, ks->length));
      /* canna_underline の真偽によらず、実質的に同じ処理をしている。
	 しかも、偽の場合の count_char の方は EUC の３バイト文字に対応して
         いない。 */
      SET_INT(canna_henkan_length, mule_strlen(ks->echoStr,ks->length));
      SET_INT(canna_henkan_revPos, mule_strlen(ks->echoStr,ks->revPos));
      SET_INT(canna_henkan_revLen, mule_strlen(ks->echoStr+ks->revPos,ks->revLen));
    }

    /* 一覧の情報 */
    SET_OBJ(Vcanna_ichiran_string, Qnil);
    if (ks->info & KanjiGLineInfo && ks->gline.length >= 0) {
      SET_OBJ(Vcanna_ichiran_string, mule_make_string(ks->gline.line, ks->gline.length));
      SET_INT(canna_ichiran_length, mule_strlen(ks->gline.line,ks->gline.length));
      SET_INT(canna_ichiran_revPos, mule_strlen(ks->gline.line,ks->gline.revPos));
      SET_INT(canna_ichiran_revLen, mule_strlen(ks->gline.line+ks->gline.revPos,ks->gline.revLen));
    }

    /* モードの情報 */
    SET_OBJ(Vcanna_mode_string, Qnil);
    if (ks->info & KanjiModeInfo) {
      SET_OBJ(Vcanna_mode_string, mule_make_string(ks->mode, strlen(ks->mode)));
    }

    /* その他の情報 */
#ifdef AS_EMACS_MODULES
    SET_OBJ(canna_empty_info, (ks->info & KanjiEmptyInfo) ? Qt : Qnil);
    SET_OBJ(canna_through_info, (ks->info & KanjiThroughInfo) ? Qt : Qnil);
#else   
    canna_empty_info = (ks->info & KanjiEmptyInfo) ? 1 : 0;
    canna_through_info = (ks->info & KanjiThroughInfo) ? 1 : 0;
#endif /* AS_EMACS_MODULES */
  }

  return val;
}

DEFUN ("canna-set-bunsetsu-kugiri",
       Fcanna_set_bunsetsu_kugiri, Scanna_set_bunsetsu_kugiri,
       0, 1, 0,
       doc: /* Set the clause separator.

Optional arg NUM non-nil means the white space separator will be used.
No separator will be used otherwise.  */)
  (Lisp_Object num)
{
  int kugiri; /* 文節区切りをするか？ */

  kugiri = NILP(num) ? 0 : 1;

  jrKanjiControl(0, KC_SETBUNSETSUKUGIRI, (char *)((long)kugiri));

  return Qnil;
}

/*
 * Mule-canna.c of XEmacs-21.4.14 reads
 * "For whatever reason, calling Fding directly from libCanna loses".
 * In any case following glue is needed for new jrBeepFunc prototype.
 */
static int
call_Fding (void)
{
  Fding (Qnil);
  return 0;
}

static Lisp_Object
CANNA_mode_keys()
{
#define CANNAWORKBUFSIZE 32
  char xxx[CANNAWORKBUFSIZE];
  Lisp_Object val;
  int i, n;

  n = jrKanjiControl(0, KC_MODEKEYS, xxx);
  val = Qnil;
  for (i = n ; i > 0 ;) {
    --i;
    val = Fcons(make_fixnum((int)(0xFF & (unsigned char)xxx[i])), val);
  }
  return val;
}

DEFUN ("canna-initialize", Fcanna_initialize, Scanna_initialize, 0, 3, 0,
       doc: /* Initialize ``canna'', which is a kana-to-kanji converter for GNU Emacs.

Optional first arg NUM specifies whether to insert space character
between clauses when candidates are displayed.  If NUM is 1, which is
the default value, a space is inserted.  If NUM is a number other than
1, no space is inserted.

Optional second arg SERVER specifies canna server.

Optional third arg RCFILE specifies a file of personal
customization of canna.

If nil is specified for each arg, the default value will be used.

Return a list of the following elements.
- list of keys to turn on Japanese-mode
- error message
- list of warning messages
*/)
  (Lisp_Object num, Lisp_Object server, Lisp_Object rcfile)
{
  Lisp_Object val;
  int res;
  unsigned char **p, **q;

  int kugiri; /* 文節区切りをするか？ */

#ifdef WITH_KKCP
  IRCP_context = -1;
#endif /* WITH_KKCP */

  if (NILP(num)) {
    kugiri = 1;
  }
  else {
    CHECK_NUMBER(num);
    kugiri = XFIXNUM(num);
    kugiri = (kugiri == 1) ? 1 : 0;
  }

  if (NILP(server)) {
    jrKanjiControl(0, KC_SETSERVERNAME, (char *)0);
  }
  else {
    char servername[256];

    CHECK_STRING(server);
    mule_extract_string(server, servername, sizeof(servername), 0);
    jrKanjiControl(0, KC_SETSERVERNAME, servername);
  }

  if (NILP(rcfile)) {
    jrKanjiControl(0, KC_SETINITFILENAME, (char *)0);
  }
  else {
    char rcname[256];

    CHECK_STRING(rcfile);
    mule_extract_string(rcfile, rcname, sizeof(rcname), 0);
    jrKanjiControl(0, KC_SETINITFILENAME, rcname);
  }

  warning = (char **)0;
#ifdef nec_ews_svr4				/* hir, 1994.2.24 */
  stop_polling ();
#endif /* nec_ews_svr4 */
  res = jrKanjiControl(0, KC_INITIALIZE, (char *)&warning);
#ifdef nec_ews_svr4				/* hir, 1994.2.24 */
  start_polling ();
#endif /* nec_ews_svr4 */
  val = Qnil;
  if (warning) {
    for (p = q = (unsigned char **)warning ; *q ; q++)
      ;
    while (p < q) {
      q--;
      val = Fcons(mule_make_string(*q, strlen(*q)), val);
    }
  }
  val = Fcons(val, Qnil);

  if (res == -1) {
    val = Fcons(mule_make_string((unsigned char*)jrKanjiError, 
			    strlen(jrKanjiError)), val);
    /* イニシャライズで失敗した場合。 */
    return Fcons(Qnil, val);
  }
  else {
#ifndef CANNA_JR_BEEP_FUNC_DECLARED
    extern int (*jrBeepFunc) (void);
#endif
    jrBeepFunc = call_Fding;

#ifdef KC_SETAPPNAME
#ifdef AS_EMACS_MODULES
    { char vers[256];
      emacs_value val = value_of(intern("emacs-version"));
      if ((val != module_nil) && module_eq(module_type_of(val), Qstring)) {
	vers[0] = 'E';
	mule_extract_string(val, vers+1, sizeof(vers)-1, 0);
      } else
	strcpy(vers, "Emacs");
      wcKanjiControl(0, KC_SETAPPNAME, vers);
    }
#else /* !AS_EMACS_MODULES */
#ifdef PACKAGE_VERSION
    wcKanjiControl(0, KC_SETAPPNAME, "E" PACKAGE_VERSION);
#else
    wcKanjiControl(0, KC_SETAPPNAME, "Emacs");
#endif
#endif /* !AS_EMACS_MODULES */
#endif /* KC_SETAPPNAME */

    jrKanjiControl(0, KC_SETBUNSETSUKUGIRI, (char *)((long)kugiri));
    jrKanjiControl(0, KC_SETWIDTH, (char *)78);
    /* mule だったら半角カタカナも使える */
#ifdef AS_EMACS_MODULES
    if (!NILP(value_of(canna_inhibit_hankakukana)))
      jrKanjiControl(0, KC_INHIBITHANKAKUKANA, (char *)1);
#else
    if (canna_inhibit_hankakukana)
      jrKanjiControl(0, KC_INHIBITHANKAKUKANA, (char *)1);
#endif /* AS_EMACS_MODULES */
    jrKanjiControl(0, KC_YOMIINFO, (char *)2); /* ※２: ローマ字まで返す */
    val = Fcons(Qnil, val);
    return Fcons(CANNA_mode_keys(), val);
  }
}

DEFUN ("canna-finalize", Fcanna_finalize, Scanna_finalize, 0, 0, 0,
       doc: /* Finalize ``canna'', which is a kana-to-kanji converter for GNU Emacs.
Write miscellaneous informations to kana-to-kanji dictionary.

Return a list of warning messages.  */)
  (void)
{
  Lisp_Object val;
  unsigned char **p;

  jrKanjiControl(0, KC_FINALIZE, (char *)&warning);

  val = Qnil;
  if (warning) {
    for (p = (unsigned char**)warning ; *p ; p++) {
      val = Fcons(mule_make_string(*p, strlen(*p)), val);
    }
  }
#ifdef WITH_KKCP
  IRCP_context = -1;
#endif /* WITH_KKCP */
  return val;
}

DEFUN ("canna-touroku-string", Fcanna_touroku_string, 
       Scanna_touroku_string, 1, 1, 0,
       doc: /* Register Kanji words STR into kana-to-kanji conversion dictionary.

Return the same thing as `canna-key-proc'.  */)
  (Lisp_Object str)
{
  jrKanjiStatusWithValue ksv;
  jrKanjiStatus ks;
  int len;
  Lisp_Object val;
  unsigned char cbuf[4096];

  CHECK_STRING(str);
  ksv.buffer = (unsigned char *)buf;
  ksv.bytes_buffer = KEYTOSTRSIZE;
  mule_extract_string(str, cbuf, sizeof(cbuf), 1);
  ks.echoStr = cbuf;
  ks.length = strlen(cbuf);
  ksv.ks = &ks;
  len = jrKanjiControl(0, KC_DEFINEKANJI, (char *)&ksv);
  val = storeResults(buf, ksv.val, ksv.ks);
  return val;
}

DEFUN ("canna-set-width", Fcanna_set_width,
       Scanna_set_width, 1, 1, 0,
       doc: /* Set status-line width to NUM to display kanji candidates.  */)
  (Lisp_Object num)
{
  CHECK_NUMBER(num);

  jrKanjiControl(0, KC_SETWIDTH,  (char *)XFIXNUM (num));
  return Qnil;
}

DEFUN ("canna-change-mode", Fcanna_change_mode,
       Scanna_change_mode, 1, 1, 0,
       doc: /* Change Japanese pre-edit mode.  NUM is the mode number of Canna.

Return the same thing as `canna-key-proc'.  */)
  (Lisp_Object num)
{
  jrKanjiStatusWithValue ksv;
  jrKanjiStatus ks;
  Lisp_Object val;

  CHECK_NUMBER(num);

  ksv.buffer = (unsigned char *)buf;
  ksv.bytes_buffer = KEYTOSTRSIZE;
  ksv.ks = &ks;
  ksv.val = XFIXNUM (num);
  jrKanjiControl(0, KC_CHANGEMODE,  (char *)&ksv);
  val = storeResults(buf, ksv.val, ksv.ks);
  return val;
}

DEFUN ("canna-store-yomi", Fcanna_store_yomi, Scanna_store_yomi, 
       1, 2, 0,
       doc: /* Store the yomi characters YOMI on canna.
Optional arg ROMA is the roman spell of YOMI.

Return the same thing as `canna-key-proc'.  */)
  (Lisp_Object yomi, Lisp_Object roma)
{
  jrKanjiStatusWithValue ksv;
  jrKanjiStatus ks;

  CHECK_STRING(yomi);
  mule_extract_string(yomi, buf, sizeof(buf), 1);
  ks.length = strlen(buf);

  if (NILP(roma)) {
    ks.mode = 0;
  }
  else {
    CHECK_STRING(roma);

    ks.mode = (unsigned char *)(buf + ks.length + 1);
    mule_extract_string(roma, ks.mode, sizeof(buf)-ks.length-1, 1);
  }

  ks.echoStr = (unsigned char *)buf;
  ksv.buffer = (unsigned char *)buf; /* 返値用 */
  ksv.bytes_buffer = KEYTOSTRSIZE;
  ksv.ks = &ks;

  jrKanjiControl(0, KC_STOREYOMI, (char *)&ksv);

  return storeResults(buf, ksv.val, ksv.ks);
}

DEFUN ("canna-do-function", Fcanna_do_function, Scanna_do_function, 
       1, 2, 0,
       doc: /* Do the NUM-th canna function at the current mode.
Key input is supplied as optional arg CH, for the function which
needs an input character.

Return the same thing as `canna-key-proc'.  */)
  (Lisp_Object num, Lisp_Object ch)
{
  jrKanjiStatusWithValue ksv;
  jrKanjiStatus ks;
  Lisp_Object val;

  CHECK_NUMBER(num);

  if (NILP(ch)) {
    *buf = '@';
  }
  else {
    CHECK_NUMBER(ch);
    *buf = XFIXNUM (ch);
  }

  ksv.buffer = (unsigned char *)buf;
  ksv.bytes_buffer = KEYTOSTRSIZE;
  ksv.ks = &ks;
  ksv.val = XFIXNUM (num);
  jrKanjiControl(0, KC_DO,  (char *)&ksv);
  val = storeResults(buf, ksv.val, ksv.ks);
  return val;
}

DEFUN ("canna-parse", Fcanna_parse, Scanna_parse, 
       1, 1, 0,
       doc: /* Parse string STR as canlisp to customize canna setting.

Return nil or list of warning messages if exists.  */)
  (Lisp_Object str)
{
  jrKanjiStatusWithValue ksv;
  jrKanjiStatus ks;
  Lisp_Object val;
  unsigned char **p;
  int n;

  CHECK_STRING(str);

  mule_extract_string(str, buf, sizeof(buf), 1);
  p = (unsigned char**)buf;
  n = jrKanjiControl(0, KC_PARSE,  (char *)&p);
  val = Qnil;
  while (n > 0) {
    n--;
    val = Fcons(mule_make_string(p[n], strlen(p[n])), val);
  }
  return val;
}

DEFUN ("canna-query-mode", Fcanna_query_mode, Scanna_query_mode, 
       0, 0, 0,
       doc: /* Get the current mode string.  */)
  (void)
{
  unsigned char buf[256];

  jrKanjiControl(0, KC_QUERYMODE, buf);
  return mule_make_string(buf, strlen(buf));
}

#ifdef WITH_KKCP
/*
 * Functions following this line are for KKCP interface compatible
 * library.  These functions may be used by MILK system.
 */

#define RKBUFSIZE 1024

static unsigned char yomibuf[RKBUFSIZE];
static short kugiri[RKBUFSIZE / 2];

static int
confirmContext()
{
  if (IRCP_context < 0) {
    int context;

    if ((context = jrKanjiControl(0, KC_GETCONTEXT, (char *)0)) == -1) {
      return 0;
    }
    IRCP_context = context;
  }
  return 1;
}

static int
byteLen(int bun, int len)
{
  int i = 0, offset = 0, ch;

  if (0 <= bun && bun < RKBUFSIZE) {
    offset = kugiri[bun];
  }

  while (len-- > 0 && (ch = (int)yomibuf[offset + i])) {
    i++;
    if (ch & 0x80) {
      i++;
    }
  }
  return i;
}

DEFUN ("canna-henkan-begin", Fcanna_henkan_begin, Scanna_henkan_begin,
       1, 1, 0,
       doc: /**/)
/* "かな漢字変換した結果を返還する。文節切りがしてある。" */
  (Lisp_Object yomi)
{
  int nbun;
  Lisp_Object res;

  CHECK_STRING(yomi);
  if (confirmContext() == 0) {
    return Qnil;
  }
  mule_extract_string(yomi, yomibuf, sizeof(yomibuf), 1);
  nbun = RkBgnBun(IRCP_context, (char *)yomibuf, strlen(yomibuf),
		  (RK_XFER << RK_XFERBITS) | RK_KFER);

  return kanjiYomiList(IRCP_context, nbun);
}

static Lisp_Object
kanjiYomiList(int context, int nbun)
{
  Lisp_Object val, res = Qnil;
  unsigned char RkBuf[RKBUFSIZE];
  int len, i, total;

  for (i = nbun ; i > 0 ; ) {
    i--;
    RkGoTo(context, i);
    len = RkGetKanji(context, RkBuf, RKBUFSIZE);
    val = mule_make_string(RkBuf, len);
    len = RkGetYomi(context, RkBuf, RKBUFSIZE);
    res = Fcons(Fcons(val, mule_make_string(RkBuf, len)), res);
    if (i < RKBUFSIZE / 2) {
      kugiri[i] = len;
    }
  }
  for (i = 0, total = 0 ; i < nbun ; i++) {
    int temp = kugiri[i];
    kugiri[i] = total;
    total += temp;
  }
  return res;
}

DEFUN ("canna-henkan-next", Fcanna_henkan_next, Scanna_henkan_next,
       1, 1, 0,
       doc: /**/)
/* "候補一覧を求める。" */
  (Lisp_Object bunsetsu)
{
  int i, nbun, slen, len;
  unsigned char *p, RkBuf[RKBUFSIZE];
  Lisp_Object res = Qnil, endp;

  CHECK_NUMBER(bunsetsu);
  if (confirmContext() == 0) {
    return Qnil;
  }
  RkGoTo(IRCP_context, XFIXNUM (bunsetsu));
  len = RkGetKanjiList(IRCP_context, RkBuf, RKBUFSIZE);
  p = RkBuf;
  for (i = 0 ; i < len ; i++) {
    slen = strlen(p);
    if (res == Qnil) {
      endp = res = Fcons(mule_make_string(p, slen), Qnil);
    }
    else {
#ifdef AS_EMACS_MODULES
      endp = Fsetcdr(endp, Fcons(mule_make_string(p, slen), Qnil));
#else
      endp = XCONS (endp)->u.cdr = Fcons(mule_make_string(p, slen), Qnil);
#endif /* AS_EMACS_MODULES */
    }
    p += slen + 1;
  }
  return res;
}

DEFUN ("canna-bunsetu-henkou", Fcanna_bunsetu_henkou, Scanna_bunsetu_henkou,
       2, 2, 0,
       doc: /**/)
/* "文節の長さを指定する。" */
  (Lisp_Object bunsetsu, Lisp_Object bunlen)
{
  int nbun, len;

  CHECK_NUMBER(bunsetsu);
  CHECK_NUMBER(bunlen);
  
  nbun = XFIXNUM (bunsetsu);
  if (confirmContext() == 0) {
    return Qnil;
  }
  RkGoTo(IRCP_context, nbun);
  len = byteLen(nbun, XFIXNUM(bunlen));
  return kanjiYomiList(IRCP_context, RkResize(IRCP_context, len));
}

DEFUN ("canna-henkan-kakutei", Fcanna_henkan_kakutei, Scanna_henkan_kakutei,
       2, 2, 0,
       doc: /**/)
/* "候補選択。" */
  (register Lisp_Object bun, register Lisp_Object kouho)
{
  int nbun, nkouho;

  if (confirmContext() == 0) {
    return Qnil;
  }
  CHECK_NUMBER(bun);
  CHECK_NUMBER(kouho);
  nbun = XFIXNUM (bun);
  nkouho = XFIXNUM (kouho);
  RkGoTo(IRCP_context, nbun);
  RkXfer(IRCP_context, nkouho);
  return Qt;
}

DEFUN ("canna-henkan-end", Fcanna_henkan_end, Scanna_henkan_end,
       0, 0, 0,
       doc: /**/)
/* "変換終了。" */
  (void)
{
  if (confirmContext() == 0) {
    return Qnil;
  }
  RkEndBun(IRCP_context, 1); /* 学習はいつでも行って良いものなのか？ */
  return Qt;
}

DEFUN ("canna-henkan-quit", Fcanna_henkan_quit, Scanna_henkan_quit,
       0, 0, 0,
       doc: /**/)
/* "変換終了。" */
  (void)
{
  if (confirmContext() == 0) {
    return Qnil;
  }
  RkEndBun(IRCP_context, 0);
  return Qt;
}
#endif /* WITH_KKCP */

#ifdef AS_EMACS_MODULES
static
#endif /* AS_EMACS_MODULES */
void
syms_of_canna ()
{
  DEFVAR_LISP ("CANNA", VCANNA, "");		/* hir@nec, 1992.5.21 */
  SET_OBJ(VCANNA,  Qt);				/* hir@nec, 1992.5.21 */

  defsubr (&Scanna_key_proc);
  defsubr (&Scanna_initialize);
  defsubr (&Scanna_finalize);
  defsubr (&Scanna_touroku_string);
  defsubr (&Scanna_set_width);
  defsubr (&Scanna_change_mode);
  defsubr (&Scanna_store_yomi);
  defsubr (&Scanna_do_function);
  defsubr (&Scanna_parse);
  defsubr (&Scanna_query_mode);
  defsubr (&Scanna_set_bunsetsu_kugiri);

  DEFVAR_LISP ("canna-kakutei-string", Vcanna_kakutei_string, "");
  DEFVAR_LISP ("canna-kakutei-yomi",   Vcanna_kakutei_yomi,   "");
  DEFVAR_LISP ("canna-kakutei-romaji", Vcanna_kakutei_romaji, "");
  DEFVAR_LISP ("canna-henkan-string",  Vcanna_henkan_string,  "");
  DEFVAR_INT ("canna-henkan-length",  canna_henkan_length,  "");
  DEFVAR_INT ("canna-henkan-revpos",  canna_henkan_revPos,  "");
  DEFVAR_INT ("canna-henkan-revlen",  canna_henkan_revLen,  "");
  DEFVAR_LISP ("canna-ichiran-string", Vcanna_ichiran_string, "");
  DEFVAR_INT ("canna-ichiran-length", canna_ichiran_length, "");
  DEFVAR_INT ("canna-ichiran-revpos", canna_ichiran_revPos, "");
  DEFVAR_INT ("canna-ichiran-revlen", canna_ichiran_revLen, "");
  DEFVAR_LISP ("canna-mode-string",    Vcanna_mode_string,    "");
  DEFVAR_BOOL ("canna-empty-info", canna_empty_info, "");
  DEFVAR_BOOL ("canna-through-info", canna_through_info, "");
  DEFVAR_BOOL ("canna-inhibit-hankakukana", canna_inhibit_hankakukana, "");
						/* hir, 1994.12.5 */

#ifdef WITH_KKCP
  defsubr (&Scanna_henkan_begin);
  defsubr (&Scanna_henkan_next);
  defsubr (&Scanna_bunsetu_henkou);
  defsubr (&Scanna_henkan_kakutei);
  defsubr (&Scanna_henkan_end);
  defsubr (&Scanna_henkan_quit);
#endif

  DEFVAR_LISP ("canna-coding-system",  Vcanna_coding_system,  "");
  SET_OBJ(Vcanna_coding_system, intern("euc-jp"));

  /* variables below this line is constants of Canna */
  DEFVAR_INT ("canna-mode-alpha-mode", canna_mode_AlphaMode, "");
  SET_INT(canna_mode_AlphaMode, IROHA_MODE_AlphaMode);

  DEFVAR_INT ("canna-mode-empty-mode", canna_mode_EmptyMode, "");
  SET_INT(canna_mode_EmptyMode, IROHA_MODE_EmptyMode);

  DEFVAR_INT ("canna-mode-kigo-mode",  canna_mode_KigoMode,  "");
  SET_INT(canna_mode_KigoMode, IROHA_MODE_KigoMode);

  DEFVAR_INT ("canna-mode-yomi-mode",  canna_mode_YomiMode,  "");
  SET_INT(canna_mode_YomiMode, IROHA_MODE_YomiMode);

  DEFVAR_INT ("canna-mode-jishu-mode", canna_mode_JishuMode, "");
  SET_INT(canna_mode_JishuMode, IROHA_MODE_JishuMode);

  DEFVAR_INT ("canna-mode-tankouho-mode", canna_mode_TankouhoMode, "");
  SET_INT(canna_mode_TankouhoMode, IROHA_MODE_TankouhoMode);

  DEFVAR_INT ("canna-mode-ichiran-mode",  canna_mode_IchiranMode,  "");
  SET_INT(canna_mode_IchiranMode, IROHA_MODE_IchiranMode);

  DEFVAR_INT ("canna-mode-yes-no-mode", canna_mode_YesNoMode, "");
  SET_INT(canna_mode_YesNoMode, IROHA_MODE_YesNoMode);

  DEFVAR_INT ("canna-mode-on-off-mode", canna_mode_OnOffMode, "");
  SET_INT(canna_mode_OnOffMode, IROHA_MODE_OnOffMode);

#ifdef CANNA_MODE_AdjustBunsetsuMode
  DEFVAR_INT ("canna-mode-adjust-bunsetsu-mode",
	      canna_mode_AdjustBunsetsuMode, "");
  SET_INT(canna_mode_AdjustBunsetsuMode, CANNA_MODE_AdjustBunsetsuMode);
#endif
#ifdef CANNA_MODE_ChikujiYomiMode
  DEFVAR_INT ("canna-mode-chikuji-yomi-mode", canna_mode_ChikujiYomiMode,"");
  SET_INT(canna_mode_ChikujiYomiMode, CANNA_MODE_ChikujiYomiMode);

  DEFVAR_INT ("canna-mode-chikuji-bunsetsu-mode",
	      canna_mode_ChikujiTanMode, "");
  SET_INT(canna_mode_ChikujiTanMode, CANNA_MODE_ChikujiTanMode);
#endif

  DEFVAR_INT ("canna-mode-henkan-mode", canna_mode_HenkanMode, "");
  SET_INT(canna_mode_HenkanMode, IROHA_MODE_HenkanMode);
#ifdef CANNA_MODE_HenkanNyuryokuMode
  DEFVAR_INT ("canna-mode-henkan-nyuuryoku-mode",
	      canna_mode_HenkanNyuryokuMode, "");
  SET_INT(canna_mode_HenkanNyuryokuMode, CANNA_MODE_HenkanNyuryokuMode);
#endif
#ifdef CANNA_MODE_ZenHiraHenkanMode
  DEFVAR_INT ("canna-mode-zen-hira-henkan-mode",
	      canna_mode_ZenHiraHenkanMode, "");
  SET_INT(canna_mode_ZenHiraHenkanMode, CANNA_MODE_ZenHiraHenkanMode);

#ifdef CANNA_MODE_HanHiraHenkanMode
  DEFVAR_INT ("canna-mode-han-hira-henkan-mode",
	      canna_mode_HanHiraHenkanMode, "");
  SET_INT(canna_mode_HanHiraHenkanMode, CANNA_MODE_HanHiraHenkanMode);
#endif
  DEFVAR_INT ("canna-mode-zen-kata-henkan-mode",
	      canna_mode_ZenKataHenkanMode, "");
  SET_INT(canna_mode_ZenKataHenkanMode, CANNA_MODE_ZenKataHenkanMode);

  DEFVAR_INT ("canna-mode-han-kata-henkan-mode",
	      canna_mode_HanKataHenkanMode, "");
  SET_INT(canna_mode_HanKataHenkanMode, CANNA_MODE_HanKataHenkanMode);

  DEFVAR_INT ("canna-mode-zen-alpha-henkan-mode",
	      canna_mode_ZenAlphaHenkanMode, "");
  SET_INT(canna_mode_ZenAlphaHenkanMode, CANNA_MODE_ZenAlphaHenkanMode);

  DEFVAR_INT ("canna-mode-han-alpha-henkan-mode",
	      canna_mode_HanAlphaHenkanMode, "");
  SET_INT(canna_mode_HanAlphaHenkanMode, CANNA_MODE_HanAlphaHenkanMode);
#endif
  DEFVAR_INT ("canna-mode-zen-hira-kakutei-mode",
	      canna_mode_ZenHiraKakuteiMode, "");
  SET_INT(canna_mode_ZenHiraKakuteiMode, IROHA_MODE_ZenHiraKakuteiMode);

#ifdef CANNA_MODE_HanHiraKakuteiMode
  DEFVAR_INT ("canna-mode-han-hira-kakutei-mode",
	      canna_mode_HanHiraKakuteiMode, "");
  SET_INT(canna_mode_HanHiraKakuteiMode, CANNA_MODE_HanHiraKakuteiMode);
#endif
  DEFVAR_INT ("canna-mode-zen-kata-kakutei-mode",
	      canna_mode_ZenKataKakuteiMode, "");
  SET_INT(canna_mode_ZenKataKakuteiMode, IROHA_MODE_ZenKataKakuteiMode);

  DEFVAR_INT ("canna-mode-han-kata-kakutei-mode",
	      canna_mode_HanKataKakuteiMode, "");
  SET_INT(canna_mode_HanKataKakuteiMode, IROHA_MODE_HanKataKakuteiMode);

  DEFVAR_INT ("canna-mode-zen-alpha-kakutei-mode",
	      canna_mode_ZenAlphaKakuteiMode, "");
  SET_INT(canna_mode_ZenAlphaKakuteiMode, IROHA_MODE_ZenAlphaKakuteiMode);

  DEFVAR_INT ("canna-mode-han-alpha-kakutei-mode",
	      canna_mode_HanAlphaKakuteiMode, "");
  SET_INT(canna_mode_HanAlphaKakuteiMode, IROHA_MODE_HanAlphaKakuteiMode);

  DEFVAR_INT ("canna-mode-hex-mode", canna_mode_HexMode, "");
  SET_INT(canna_mode_HexMode, IROHA_MODE_HexMode);

  DEFVAR_INT ("canna-mode-bushu-mode", canna_mode_BushuMode, "");
  SET_INT(canna_mode_BushuMode, IROHA_MODE_BushuMode);

  DEFVAR_INT ("canna-mode-extend-mode", canna_mode_ExtendMode, "");
  SET_INT(canna_mode_ExtendMode, IROHA_MODE_ExtendMode);

  DEFVAR_INT ("canna-mode-russian-mode", canna_mode_RussianMode, "");
  SET_INT(canna_mode_RussianMode, IROHA_MODE_RussianMode);

  DEFVAR_INT ("canna-mode-greek-mode", canna_mode_GreekMode, "");
  SET_INT(canna_mode_GreekMode, IROHA_MODE_GreekMode);

  DEFVAR_INT ("canna-mode-line-mode", canna_mode_LineMode, "");
  SET_INT(canna_mode_LineMode, IROHA_MODE_LineMode);

  DEFVAR_INT ("canna-mode-changing-server-mode",
	      canna_mode_ChangingServerMode, "");
  SET_INT(canna_mode_ChangingServerMode, IROHA_MODE_ChangingServerMode);

  DEFVAR_INT ("canna-mode-henkan-method-mode",
	      canna_mode_HenkanMethodMode, "");
  SET_INT(canna_mode_HenkanMethodMode, IROHA_MODE_HenkanMethodMode);

  DEFVAR_INT ("canna-mode-delete-dic-mode", canna_mode_DeleteDicMode, "");
  SET_INT(canna_mode_DeleteDicMode, IROHA_MODE_DeleteDicMode);

  DEFVAR_INT ("canna-mode-touroku-mode", canna_mode_TourokuMode, "");
  SET_INT(canna_mode_TourokuMode, IROHA_MODE_TourokuMode);

  DEFVAR_INT ("canna-mode-touroku-empty-mode",
	      canna_mode_TourokuEmptyMode, "");
  SET_INT(canna_mode_TourokuEmptyMode, IROHA_MODE_TourokuEmptyMode);

  DEFVAR_INT ("canna-mode-touroku-hinshi-mode",
	      canna_mode_TourokuHinshiMode, "");
  SET_INT(canna_mode_TourokuHinshiMode, IROHA_MODE_TourokuHinshiMode);

  DEFVAR_INT ("canna-mode-touroku-dic-mode", canna_mode_TourokuDicMode, "");
  SET_INT(canna_mode_TourokuDicMode, IROHA_MODE_TourokuDicMode);

  DEFVAR_INT ("canna-mode-quoted-insert-mode",
	      canna_mode_QuotedInsertMode, "");
  SET_INT(canna_mode_QuotedInsertMode, IROHA_MODE_QuotedInsertMode);

  DEFVAR_INT ("canna-mode-bubun-muhenkan-mode",
	      canna_mode_BubunMuhenkanMode, "");
  SET_INT(canna_mode_BubunMuhenkanMode, IROHA_MODE_BubunMuhenkanMode);

  DEFVAR_INT ("canna-mode-mount-dic-mode", canna_mode_MountDicMode, "");
  SET_INT(canna_mode_MountDicMode, IROHA_MODE_MountDicMode);

  DEFVAR_INT ("canna-func-self-insert", canna_fn_SelfInsert ,"");
  SET_INT(canna_fn_SelfInsert, IROHA_FN_SelfInsert);

  DEFVAR_INT ("canna-func-functional-insert", canna_fn_FunctionalInsert ,"");
  SET_INT(canna_fn_FunctionalInsert, IROHA_FN_FunctionalInsert);

  DEFVAR_INT ("canna-func-quoted-insert", canna_fn_QuotedInsert ,"");
  SET_INT(canna_fn_QuotedInsert, IROHA_FN_QuotedInsert);

  DEFVAR_INT ("canna-func-japanese-mode", canna_fn_JapaneseMode ,"");
  SET_INT(canna_fn_JapaneseMode, IROHA_FN_JapaneseMode);

  DEFVAR_INT ("canna-func-alpha-mode", canna_fn_AlphaMode ,"");
  SET_INT(canna_fn_AlphaMode, IROHA_FN_AlphaMode);

  DEFVAR_INT ("canna-func-henkan-nyuryoku-mode",
	      canna_fn_HenkanNyuryokuMode ,"");
  SET_INT(canna_fn_HenkanNyuryokuMode, IROHA_FN_HenkanNyuryokuMode);

  DEFVAR_INT ("canna-func-forward", canna_fn_Forward ,"");
  SET_INT(canna_fn_Forward, IROHA_FN_Forward);

  DEFVAR_INT ("canna-func-backward", canna_fn_Backward ,"");
  SET_INT(canna_fn_Backward, IROHA_FN_Backward);

  DEFVAR_INT ("canna-func-next", canna_fn_Next ,"");
  SET_INT(canna_fn_Next, IROHA_FN_Next);

  DEFVAR_INT ("canna-func-previous", canna_fn_Prev ,"");
  SET_INT(canna_fn_Prev, IROHA_FN_Prev);

  DEFVAR_INT ("canna-func-beginning-of-line", canna_fn_BeginningOfLine ,"");
  SET_INT(canna_fn_BeginningOfLine, IROHA_FN_BeginningOfLine);
 
  DEFVAR_INT ("canna-func-end-of-line", canna_fn_EndOfLine ,"");
  SET_INT(canna_fn_EndOfLine, IROHA_FN_EndOfLine);

  DEFVAR_INT ("canna-func-delete-next", canna_fn_DeleteNext ,"");
  SET_INT(canna_fn_DeleteNext, IROHA_FN_DeleteNext);

  DEFVAR_INT ("canna-func-delete-previous", canna_fn_DeletePrevious ,"");
  SET_INT(canna_fn_DeletePrevious, IROHA_FN_DeletePrevious);

  DEFVAR_INT ("canna-func-kill-to-end-of-line", canna_fn_KillToEndOfLine,"");
  SET_INT(canna_fn_KillToEndOfLine, IROHA_FN_KillToEndOfLine);

  DEFVAR_INT ("canna-func-henkan", canna_fn_Henkan ,"");
  SET_INT(canna_fn_Henkan, IROHA_FN_Henkan);

  DEFVAR_INT ("canna-func-kakutei", canna_fn_Kakutei ,"");
  SET_INT(canna_fn_Kakutei, IROHA_FN_Kakutei);

  DEFVAR_INT ("canna-func-extend", canna_fn_Extend ,"");
  SET_INT(canna_fn_Extend, IROHA_FN_Extend);

  DEFVAR_INT ("canna-func-shrink", canna_fn_Shrink ,"");
  SET_INT(canna_fn_Shrink, IROHA_FN_Shrink);

#ifdef CANNA_FN_AdjustBunsetsu
  DEFVAR_INT ("canna-func-adjust-bunsetsu", canna_fn_AdjustBunsetsu ,"");
  SET_INT(canna_fn_AdjustBunsetsu, CANNA_FN_AdjustBunsetsu);
#endif
  DEFVAR_INT ("canna-func-quit", canna_fn_Quit ,"");
  SET_INT(canna_fn_Quit, IROHA_FN_Quit);

  DEFVAR_INT ("canna-func-convert-as-hex", canna_fn_ConvertAsHex ,"");
  SET_INT(canna_fn_ConvertAsHex, IROHA_FN_ConvertAsHex);

  DEFVAR_INT ("canna-func-convert-as-bushu", canna_fn_ConvertAsBushu ,"");
  SET_INT(canna_fn_ConvertAsBushu, IROHA_FN_ConvertAsBushu);

  DEFVAR_INT ("canna-func-kouho-ichiran", canna_fn_KouhoIchiran ,"");
  SET_INT(canna_fn_KouhoIchiran, IROHA_FN_KouhoIchiran);

  DEFVAR_INT ("canna-func-bubun-muhenkan", canna_fn_BubunMuhenkan ,"");
  SET_INT(canna_fn_BubunMuhenkan, IROHA_FN_BubunMuhenkan);

  DEFVAR_INT ("canna-func-zenkaku", canna_fn_Zenkaku ,"");
  SET_INT(canna_fn_Zenkaku, IROHA_FN_Zenkaku);

  DEFVAR_INT ("canna-func-hankaku", canna_fn_Hankaku ,"");
  SET_INT(canna_fn_Hankaku, IROHA_FN_Hankaku);

  DEFVAR_INT ("canna-func-to-upper", canna_fn_ToUpper ,"");
  SET_INT(canna_fn_ToUpper, IROHA_FN_ToUpper);

  DEFVAR_INT ("canna-func-capitalize", canna_fn_Capitalize ,"");
  SET_INT(canna_fn_Capitalize, IROHA_FN_Capitalize);

  DEFVAR_INT ("canna-func-to-lower", canna_fn_ToLower ,"");
  SET_INT(canna_fn_ToLower, IROHA_FN_ToLower);

  DEFVAR_INT ("canna-func-hiragana", canna_fn_Hiragana ,"");
  SET_INT(canna_fn_Hiragana, IROHA_FN_Hiragana);

  DEFVAR_INT ("canna-func-katakana", canna_fn_Katakana ,"");
  SET_INT(canna_fn_Katakana, IROHA_FN_Katakana);

  DEFVAR_INT ("canna-func-romaji", canna_fn_Romaji ,"");
  SET_INT(canna_fn_Romaji, IROHA_FN_Romaji);

#ifdef CANNA_FN_BaseHiragana
  DEFVAR_INT ("canna-func-base-hiragana", canna_fn_BaseHiragana ,"");
  SET_INT(canna_fn_BaseHiragana, CANNA_FN_BaseHiragana);

  DEFVAR_INT ("canna-func-base-katakana", canna_fn_BaseKatakana ,"");
  SET_INT(canna_fn_BaseKatakana, CANNA_FN_BaseKatakana);

  DEFVAR_INT ("canna-func-base-eisu", canna_fn_BaseEisu ,"");
  SET_INT(canna_fn_BaseEisu, CANNA_FN_BaseEisu);

  DEFVAR_INT ("canna-func-base-zenkaku", canna_fn_BaseZenkaku ,"");
  SET_INT(canna_fn_BaseZenkaku, CANNA_FN_BaseZenkaku);

  DEFVAR_INT ("canna-func-base-hankaku", canna_fn_BaseHankaku ,"");
  SET_INT(canna_fn_BaseHankaku, CANNA_FN_BaseHankaku);

  DEFVAR_INT ("canna-func-base-kana", canna_fn_BaseKana ,"");
  SET_INT(canna_fn_BaseKana, CANNA_FN_BaseKana);

  DEFVAR_INT ("canna-func-base-kakutei", canna_fn_BaseKakutei ,"");
  SET_INT(canna_fn_BaseKakutei, CANNA_FN_BaseKakutei);

  DEFVAR_INT ("canna-func-base-henkan", canna_fn_BaseHenkan ,"");
  SET_INT(canna_fn_BaseHenkan, CANNA_FN_BaseHenkan);

  DEFVAR_INT ("canna-func-base-hiragana-katakana-toggle",
	      canna_fn_BaseHiraKataToggle ,"");
  SET_INT(canna_fn_BaseHiraKataToggle, CANNA_FN_BaseHiraKataToggle);

  DEFVAR_INT ("canna-func-base-zenkaku-hankaku-toggle",
	      canna_fn_BaseZenHanToggle ,"");
  SET_INT(canna_fn_BaseZenHanToggle, CANNA_FN_BaseZenHanToggle);

  DEFVAR_INT ("canna-func-base-kana-eisu-toggle",
	      canna_fn_BaseKanaEisuToggle ,"");
  SET_INT(canna_fn_BaseKanaEisuToggle, CANNA_FN_BaseKanaEisuToggle);

  DEFVAR_INT ("canna-func-base-kakutei-henkan-toggle",
	      canna_fn_BaseKakuteiHenkanToggle ,"");
  SET_INT(canna_fn_BaseKakuteiHenkanToggle, CANNA_FN_BaseKakuteiHenkanToggle);

  DEFVAR_INT ("canna-func-base-rotate-forward",
	      canna_fn_BaseRotateForward ,"");
  SET_INT(canna_fn_BaseRotateForward, CANNA_FN_BaseRotateForward);

  DEFVAR_INT ("canna-func-base-rotate-backward",
	      canna_fn_BaseRotateBackward ,"");
  SET_INT(canna_fn_BaseRotateBackward, CANNA_FN_BaseRotateBackward);
#endif
  DEFVAR_INT ("canna-func-extend-mode", canna_fn_ExtendMode ,"");
  SET_INT(canna_fn_ExtendMode, IROHA_FN_ExtendMode);

  DEFVAR_INT ("canna-func-touroku", canna_fn_Touroku ,"");
  SET_INT(canna_fn_Touroku, IROHA_FN_Touroku);

  DEFVAR_INT ("canna-func-hex-mode", canna_fn_HexMode ,"");
  SET_INT(canna_fn_HexMode, IROHA_FN_HexMode);

  DEFVAR_INT ("canna-func-bushu-mode", canna_fn_BushuMode ,"");
  SET_INT(canna_fn_BushuMode, IROHA_FN_BushuMode);

  DEFVAR_INT ("canna-func-kigo-mode", canna_fn_KigouMode ,"");
  SET_INT(canna_fn_KigouMode, IROHA_FN_KigouMode);

#ifdef CANNA_FN_Mark
  DEFVAR_INT ("canna-func-mark", canna_fn_Mark ,"");
  SET_INT(canna_fn_Mark, CANNA_FN_Mark);
#endif
#ifdef CANNA_FN_TemporalMode
  DEFVAR_INT ("canna-func-temporal-mode", canna_fn_TemporalMode ,"");
  SET_INT(canna_fn_TemporalMode, CANNA_FN_TemporalMode);
#endif

  DEFVAR_INT ("canna-key-nfer", canna_key_Nfer, "");
  SET_INT(canna_key_Nfer, IROHA_KEY_Nfer);

  DEFVAR_INT ("canna-key-xfer", canna_key_Xfer, "");
  SET_INT(canna_key_Xfer, IROHA_KEY_Xfer);

  DEFVAR_INT ("canna-key-up", canna_key_Up, "");
  SET_INT(canna_key_Up, IROHA_KEY_Up);

  DEFVAR_INT ("canna-key-left", canna_key_Left, "");
  SET_INT(canna_key_Left, IROHA_KEY_Left);

  DEFVAR_INT ("canna-key-right", canna_key_Right, "");
  SET_INT(canna_key_Right, IROHA_KEY_Right);

  DEFVAR_INT ("canna-key-down", canna_key_Down, "");
  SET_INT(canna_key_Down, IROHA_KEY_Down);

  DEFVAR_INT ("canna-key-insert", canna_key_Insert, "");
  SET_INT(canna_key_Insert, IROHA_KEY_Insert);

  DEFVAR_INT ("canna-key-rollup", canna_key_Rollup, "");
  SET_INT(canna_key_Rollup, IROHA_KEY_Rollup);

  DEFVAR_INT ("canna-key-rolldown", canna_key_Rolldown, "");
  SET_INT(canna_key_Rolldown, IROHA_KEY_Rolldown);

  DEFVAR_INT ("canna-key-home", canna_key_Home, "");
  SET_INT(canna_key_Home, IROHA_KEY_Home);

  DEFVAR_INT ("canna-key-help", canna_key_Help, "");
  SET_INT(canna_key_Help, IROHA_KEY_Help);

#ifdef CANNA_KEY_End
  DEFVAR_INT ("canna-key-end", canna_key_End, "");
  SET_INT(canna_key_End, CANNA_KEY_End);
#endif
  DEFVAR_INT ("canna-key-kp-key", canna_key_KP_Key, "");
  SET_INT(canna_key_KP_Key, IROHA_KEY_KP_Key);

  DEFVAR_INT ("canna-key-shift-nfer", canna_key_Shift_Nfer, "");
  SET_INT(canna_key_Shift_Nfer, IROHA_KEY_Shift_Nfer);

  DEFVAR_INT ("canna-key-shift-xfer", canna_key_Shift_Xfer, "");
  SET_INT(canna_key_Shift_Xfer, IROHA_KEY_Shift_Xfer);

  DEFVAR_INT ("canna-key-shift-up", canna_key_Shift_Up, "");
  SET_INT(canna_key_Shift_Up, IROHA_KEY_Shift_Up);

  DEFVAR_INT ("canna-key-shift-left", canna_key_Shift_Left, "");
  SET_INT(canna_key_Shift_Left, IROHA_KEY_Shift_Left);

  DEFVAR_INT ("canna-key-shift-right", canna_key_Shift_Right, "");
  SET_INT(canna_key_Shift_Right, IROHA_KEY_Shift_Right);

  DEFVAR_INT ("canna-key-shift-down", canna_key_Shift_Down, "");
  SET_INT(canna_key_Shift_Down, IROHA_KEY_Shift_Down);

  DEFVAR_INT ("canna-key-control-nfer", canna_key_Cntrl_Nfer, "");
  SET_INT(canna_key_Cntrl_Nfer, IROHA_KEY_Cntrl_Nfer);

  DEFVAR_INT ("canna-key-control-xfer", canna_key_Cntrl_Xfer, "");
  SET_INT(canna_key_Cntrl_Xfer, IROHA_KEY_Cntrl_Xfer);

  DEFVAR_INT ("canna-key-control-up", canna_key_Cntrl_Up, "");
  SET_INT(canna_key_Cntrl_Up, IROHA_KEY_Cntrl_Up);

  DEFVAR_INT ("canna-key-control-left", canna_key_Cntrl_Left, "");
  SET_INT(canna_key_Cntrl_Left, IROHA_KEY_Cntrl_Left);

  DEFVAR_INT ("canna-key-control-right", canna_key_Cntrl_Right, "");
  SET_INT(canna_key_Cntrl_Right, IROHA_KEY_Cntrl_Right);

  DEFVAR_INT ("canna-key-control-down", canna_key_Cntrl_Down, "");
  SET_INT(canna_key_Cntrl_Down, IROHA_KEY_Cntrl_Down);

#ifdef CANNA_KEY_F1
  DEFVAR_INT ("canna-key-f1", canna_key_F1, "");
  SET_INT(canna_key_F1, CANNA_KEY_F1);

  DEFVAR_INT ("canna-key-f2", canna_key_F2, "");
  SET_INT(canna_key_F2, CANNA_KEY_F2);

  DEFVAR_INT ("canna-key-f3", canna_key_F3, "");
  SET_INT(canna_key_F3, CANNA_KEY_F3);

  DEFVAR_INT ("canna-key-f4", canna_key_F4, "");
  SET_INT(canna_key_F4, CANNA_KEY_F4);

  DEFVAR_INT ("canna-key-f5", canna_key_F5, "");
  SET_INT(canna_key_F5, CANNA_KEY_F5);

  DEFVAR_INT ("canna-key-f6", canna_key_F6, "");
  SET_INT(canna_key_F6, CANNA_KEY_F6);

  DEFVAR_INT ("canna-key-f7", canna_key_F7, "");
  SET_INT(canna_key_F7, CANNA_KEY_F7);

  DEFVAR_INT ("canna-key-f8", canna_key_F8, "");
  SET_INT(canna_key_F8, CANNA_KEY_F8);

  DEFVAR_INT ("canna-key-f9", canna_key_F9, "");
  SET_INT(canna_key_F9, CANNA_KEY_F9);

  DEFVAR_INT ("canna-key-f10", canna_key_F10, "");
  SET_INT(canna_key_F10, CANNA_KEY_F10);
#endif
#ifdef CANNA_KEY_HIRAGANA
  DEFVAR_INT ("canna-key-hiragana", canna_key_Hiragana, "");
  SET_INT(canna_key_Hiragana, CANNA_KEY_HIRAGANA);

  DEFVAR_INT ("canna-key-katakana", canna_key_Katakana, "");
  SET_INT(canna_key_Katakana, CANNA_KEY_KATAKANA);

  DEFVAR_INT ("canna-key-hankakuzenkaku", canna_key_Hankakuzenkaku, "");
  SET_INT(canna_key_Hankakuzenkaku, CANNA_KEY_HANKAKUZENKAKU);

  DEFVAR_INT ("canna-key-eisu", canna_key_Eisu, "");
  SET_INT(canna_key_Eisu, CANNA_KEY_EISU);
#endif
}

/* To handle MULE internal code and EUC.
   I assume CANNA can handle only Japanese EUC. */

#ifdef AS_EMACS_MODULES
static char *b64chr =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
#endif

static Lisp_Object
mule_make_string(unsigned char *p, int l)
{
#ifdef AS_EMACS_MODULES
  /* euc-encoded string `p' is returned as base64-encoded string */
  char buf[8192]; /* big enough? */
  char *bp = buf, *bpe = buf + sizeof(buf);

  while ((l > 0) && (bp + 4 + 1 < bpe)) {
    int v24, bc;
    v24 = *p++; l--; bc = 1;
    v24 = (v24 << 8) | (l>0?(l--, bc++, *p++):0);
    v24 = (v24 << 8) | (l>0?(l--, bc++, *p++):0);
    bp[2] = bp[3] = '=';
    if (bc == 3) bp[3] = b64chr[v24&0x00003f];
    v24 = v24 >> 6;
    if (bc >= 2) bp[2] = b64chr[v24&0x00003f];
    v24 = v24 >> 6;
    bp[1] = b64chr[v24&0x00003f];
    v24 = v24 >> 6;
    bp[0] = b64chr[v24&0x00003f];
    bp += 4;
  }
  *bp = '\0';

  return module_make_string(buf, bp-buf);
#else
  /* make_string after converting EUC string to UTF-8 string */
  return (code_convert_string(make_unibyte_string(p,l), Vcanna_coding_system, Qnil, 0, 1, 1));
#endif /* AS_EMACS_MODULES */
}	

static void
mule_extract_string(Lisp_Object lispstr, char *strbuf, int len, int decode)
{
#ifdef AS_EMACS_MODULES
  /* euc-encoded string `lispstr' is passed as base64-encoded string */
  static char b64cd_init = 0, b64cd[256];

  char buf[8192]; /* big enough? */
  ptrdiff_t blen = sizeof(buf);

  if (!decode) {
    blen = len;
    module_copy_string_contents(lispstr, strbuf, &blen);
    return;
  }

  if (!b64cd_init) {
    int i;
    for (i=0; i<256; i++) b64cd[i] = -1;
    for (i=0; b64chr[i]; i++) b64cd[b64chr[i]] = i;
    b64cd_init++;
  }

  if (module_copy_string_contents(lispstr, buf, &blen)) {
    unsigned char *p = (unsigned char*)buf;
    char *strbpe = strbuf + len;

    while (blen >= 4) {
      int v24, bc;
      bc = 3; /* 3 bytes at max */
      v24 = b64cd[p[0]];
      v24 = (v24 << 6) | b64cd[p[1]];
      v24 = (v24 << 6) | ((p[2] == '=')?(bc--, 0):b64cd[p[2]]);
      v24 = (v24 << 6) | ((p[3] == '=')?(bc--, 0):(p[2] == '=')?-1:b64cd[p[3]]);
      if (v24 < 0) break; /* invalid char is seen */
      blen -= 4;
      p += 4;
      if (strbuf + bc + 1 >= strbpe) break;
      if (bc == 3) strbuf[2] = v24 & 0x0000ff;
      v24 = v24 >> 8;
      if (bc >= 2) strbuf[1] = v24 & 0x0000ff;
      v24 = v24 >> 8;
      strbuf[0] = v24 & 0x0000ff;
      strbuf += bc;
      if (bc != 3) break;
    }
  }
  *strbuf = '\0';
#else
  if (decode)
    lispstr = code_convert_string(lispstr, Vcanna_coding_system, Qnil, 1, 1, 1);
  strncpy(strbuf, XSTRING(lispstr)->u.s.data, XSTRING(lispstr)->u.s.size);
  strbuf[XSTRING(lispstr)->u.s.size] = '\0';
#endif /* AS_EMACS_MODULES */
}

#if 1 /* was checking STRING_BYTES defined in lisp.h */
#define MULE_STR_SS2 1
#define MULE_STR_SS3 1
#define MULE_STR_KNJ 1
#else
#define MULE_STR_SS2 2
#define MULE_STR_SS3 3
#define MULE_STR_KNJ 3
#endif

/* return the MULE internal string length of EUC string */
static int
mule_strlen(unsigned char *p, int l)
{
  unsigned char ch, *cp = p;
  int len = 0;
  
  while((cp < p + l) && (ch = *cp)) {
    if ((unsigned char)ch == ISO_CODE_SS2) {
      len += MULE_STR_SS2;
      cp += 2;
    }
    else if ((unsigned char)ch == ISO_CODE_SS3) {
      len += MULE_STR_SS3;
      cp += 3;
    }
    else if(ch & 0x80) {
      len += MULE_STR_KNJ;
      cp += 2;
    }
    else {
      len++;
      cp++;	
    }
  }
  return(len);
}
