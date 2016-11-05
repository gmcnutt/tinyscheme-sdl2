#ifndef scm_h
#define scm_h

/**
 * Utilities for interfacing with tinyscheme.
 */

#include <scheme-private.h>

#define scm_mk_ptr(sc, val) \
	(sc)->vptr->mk_foreign_func((sc), (foreign_func)(val))
#define scm_mk_integer(sc, val) (sc)->vptr->mk_integer((sc), (val))
#define scm_mk_real(sc, val) (sc)->vptr->mk_real((sc), (val))
#define scm_mk_symbol(sc, val) (sc)->vptr->mk_symbol((sc), (val))
#define scm_mk_string(sc, val) (sc)->vptr->mk_string((sc), (val))

#define scm_define(sc, sym, val)	 \
        (sc)->vptr->scheme_define((sc),  \
                                  (sc)->global_env,  \
                                  (sc)->vptr->mk_symbol(sc, (sym)), \
                                  (val))
#define scm_define_int(sc, sym, val) \
        scm_define(sc, sym, (sc)->vptr->mk_integer((sc), (val)))
#define scm_define_bool(sc, sym, val) \
        scm_define(sc, sym, (val) ? (sc)->T : (sc)->F)
#define scm_define_ptr(sc, sym, val) \
        scm_define(sc, sym, scm_mk_ptr(sc, val))
#define scm_define_api_call(sc, sym, val) \
        scm_define(sc, sym, scm_mk_ptr(sc, val))

#define scm_is_pair(sc,arg) ((sc)->vptr->is_pair(arg))
#define scm_is_num(sc, arg) ((sc)->vptr->is_number(arg))
#define scm_is_int(sc, arg) ((sc)->vptr->is_integer(arg))
#define scm_is_real(sc, arg) ((sc)->vptr->is_real(arg))
#define scm_is_str(sc, arg) ((sc)->vptr->is_string(arg))
#define scm_is_sym(sc, arg) ((sc)->vptr->is_symbol(arg))
#define scm_is_ptr(sc, arg) ((sc)->vptr->is_foreign(arg))
#define scm_is_closure(sc, arg) ((sc)->vptr->is_closure(arg))

#define scm_car(sc, arg) ((sc)->vptr->pair_car(arg))
#define scm_cdr(sc, arg) ((sc)->vptr->pair_cdr(arg))

#define scm_str_val(sc, arg) ((sc)->vptr->string_value(arg))
#define scm_ptr_val(sc, arg) ((void*)(arg)->_object._ff)
#define scm_int_val(sc, arg) ((sc)->vptr->ivalue(arg))
#define scm_real_val(sc, arg) ((sc)->vptr->rvalue(arg))
#define scm_sym_val(sc, arg) ((sc)->vptr->symname(arg))
#define scm_closure_code(sc, arg) ((sc)->vptr->closure_code(arg))
#define scm_closure_env(sc, arg) ((sc)->vptr->closure_env(arg))

#endif
