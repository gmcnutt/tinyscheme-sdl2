#include "scm.h"
#include <stdarg.h>
#include <string.h>


#define MAX_ERROR_LEN 256

static char _error_buf[MAX_ERROR_LEN + 1];


const char *scm_get_error(void)
{
        return _error_buf;
}


void scm_clear_error(void)
{
        memset(_error_buf, 0, sizeof(_error_buf));
}

static void scm_set_error(const char *fmt, ...)
{
        va_list args;
        va_start(args, fmt);
        vsnprintf(_error_buf, sizeof(_error_buf), fmt, args);
        va_end(args);
}


int scm_unpack(scheme * sc, pointer * cell, const char *fmt, ...)
{
        va_list args;
        int count = 0, errs = 0;
        pointer car;
        char **strval;
        int *ival;
        float *rval;
        void **ptrval;
        pointer *cval;

        va_start(args, fmt);

        while (*fmt && scm_is_pair(sc, *cell)) {

                count++;
                car = scm_car(sc, *cell);
                *cell = scm_cdr(sc, *cell);

                switch (*fmt++) {
                case 'b':      /* bool */
                        ival = va_arg(args, int *);
                        if (car == sc->T) {
                                *ival = 1;
                        } else if (car == sc->F) {
                                *ival = 0;
                        } else {
                                errs++;
                                scm_set_error("arg %d not a bool", count);
                        }
                        break;
                case 'c':
                        /* closure (actually, a symbol, possibly for a closure;
                         * this is misleading) */
                        cval = va_arg(args, pointer *);
                        if (car == sc->NIL) {
                                *cval = sc->NIL;
                        } else if (!scm_is_sym(sc, car)) {
                                errs++;
                                scm_set_error("arg %d not a symbol", count);
                        } else {
                                *cval = car;
                        }
                        break;
                case 'd':      /* integer */
                        ival = va_arg(args, int *);
                        if (!scm_is_num(sc, car)) {
                                errs++;
                                scm_set_error("arg %d not a number", count);
                        } else if (!scm_is_int(sc, car)) {
                                /*errs++;
                                 * scm_set_error("arg %d not an int", count); */
                                /* coerce it */
                                *ival = (int) scm_real_val(sc, car);
                        } else {
                                *ival = scm_int_val(sc, car);
                        }
                        break;
                case 'f':      /* float */
                        rval = va_arg(args, float *);
                        if (!scm_is_num(sc, car)) {
                                errs++;
                                scm_set_error("arg %d not a number", count);
                        } else if (!scm_is_real(sc, car)) {
                                /* coerce it */
                                *rval = scm_int_val(sc, car);
                        } else {
                                *rval = scm_real_val(sc, car);
                        }
                        break;
                case 'o':      /* procedure */
                        cval = va_arg(args, pointer *);
                        if (car == sc->NIL) {
                                *cval = sc->NIL;
                        } else if (!scm_is_closure(sc, car)) {
                                errs++;
                                scm_set_error("arg %d not a closure", count);
                        } else {
                                *cval = car;
                        }
                        break;
                case 'p':      /* C pointer */
                        ptrval = va_arg(args, void **);
                        if (car == sc->NIL) {
                                *ptrval = 0;
                        } else if (scm_is_ptr(sc, car)) {
                                *ptrval = scm_ptr_val(sc, car);
                        } else {
                                errs++;
                                scm_set_error("arg %d not a C ptr", count);
                        }
                        break;
                case 'r':      /* real number */
                        rval = va_arg(args, float *);
                        if (!scm_is_num(sc, car)) {
                                errs++;
                                scm_set_error("arg %d not a number", count);
                        } else if (!scm_is_real(sc, car)) {
                                errs++;
                                scm_set_error("arg %d not a real", count);
                        } else {
                                *rval = scm_real_val(sc, car);
                        }
                        break;
                case 's':      /* string */
                        strval = va_arg(args, char **);
                        if (car == sc->NIL) {
                                *strval = 0;
                        } else if (scm_is_str(sc, car)) {
                                *strval = scm_str_val(sc, car);
                        } else {
                                errs++;
                                scm_set_error("arg %d not a string", count);
                        }
                        break;
                case 'y':      /* symbol */
                        strval = va_arg(args, char **);
                        if (car == sc->NIL) {
                                *strval = 0;
                        } else if (scm_is_sym(sc, car)) {
                                *strval = scm_sym_val(sc, car);
                        } else {
                                errs++;
                                scm_set_error("arg %d not a symbol", count);
                        }
                        break;
                case 'l':      /* plain old cell, (eg a gob) */
                        cval = va_arg(args, pointer *);
                        *cval = car;
                        break;
                default:
                        scm_set_error("unknown format char: %c\n", *(fmt - 1));
                        break;
                }
        }

        if (*fmt) {
                scm_set_error("received only %d of %d arguments",
                         count, count + strlen(fmt));
        }

        va_end(args);

        return (!*fmt && !errs) ? 0 : -1;
}

static int scm_vpack(scheme * sc, pointer *head, const char *fmt, va_list ap)
{
        pointer tail = sc->NIL;
        pointer cell = sc->NIL;
        pointer arg = sc->NIL;
        void *ptr;
        int ival;
        char *strval;

        *head = sc->NIL;

        while (*fmt) {

                switch (*fmt++) {
                case 'p':
                        ptr = va_arg(ap, void *);
                        arg = scm_mk_ptr(sc, ptr);
                        break;
                case 'd':
                        ival = va_arg(ap, int);
                        arg = scm_mk_int(sc, ival);
                        break;
                case 'y':
                        strval = va_arg(ap, char *);
                        arg = scm_mk_symbol(sc, strval);
                        break;
                case 'l':
                        arg = va_arg(ap, pointer);
                        if (!arg) {
                                arg = sc->NIL;
                        }
                        break;
                default:
                        scm_set_error("unknown format char: %c\n", *(fmt - 1));
                        return -1;
                }

                cell = scm_cons(sc, arg, sc->NIL);

                if (*head == sc->NIL) {
                        *head = cell;
                        tail = cell;
                } else {
                        tail->_object._cons._cdr = cell;
                        tail = cell;
                }
        }

        return 0;
}

int scm_pack(scheme * sc, pointer *head, const char *fmt, ...)
{
        va_list ap;
        int result;

        va_start(ap, fmt);
        result = scm_vpack(sc, head, fmt, ap);
        va_end(ap);

        return result;
}
