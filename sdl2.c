#include "scheme-private.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_log.h>


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


static void log_critical(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	SDL_LogMessageV(SDL_LOG_CATEGORY_APPLICATION,
			SDL_LOG_PRIORITY_CRITICAL, fmt, args);
	va_end(args);
}

static void log_error(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	SDL_LogMessageV(SDL_LOG_CATEGORY_APPLICATION,
			SDL_LOG_PRIORITY_ERROR, fmt, args);
	va_end(args);
}

static void log_debug(const char *fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	SDL_LogMessageV(SDL_LOG_CATEGORY_APPLICATION,
			SDL_LOG_PRIORITY_DEBUG, fmt, args);
	va_end(args);
}


/**
 * Initialize the SDL2 subsystem.
 */
static pointer sdl2_init(scheme *sc, pointer args)
{
	/* Initialize the video, event, threading and file i/o subsystems. */
	if (SDL_Init(SDL_INIT_VIDEO)) {
		log_critical("SDL_Init: %s\n", SDL_GetError());
		return sc->NIL;
	}

	/* Cleanup SDL on exit. */
	atexit(SDL_Quit);

	/* Set logging level. */
	SDL_LogSetPriority(SDL_LOG_CATEGORY_APPLICATION,
			   SDL_LOG_PRIORITY_DEBUG);

	return sc->T;
}

/**
 * Wrapper for SDL_CreateWindow.
 */
static pointer sdl2_create_window(scheme *sc, pointer args)
{
	SDL_Window *window;
	log_debug("sdl2_create_window:enter\n");
	/* Create the main window */
	if (! (window = SDL_CreateWindow(
		       "Demo", SDL_WINDOWPOS_UNDEFINED,
		       SDL_WINDOWPOS_UNDEFINED, 640, 480,
		       SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN))) {
		log_error("SDL_CreateWindow: %s\n", SDL_GetError());
		return sc->NIL;
	}

	/* Return the window as a foreign pointer. */
	return scm_mk_ptr(sc, window);
}

/**
 * Wrapper for SDL_DestroyWindow.
 */
static pointer sdl2_destroy_window(scheme *sc, pointer args)
{
	SDL_Window *window=NULL;
	pointer car=NULL;

	log_debug("sdl2_destroy_window:enter\n");
	if (! scm_is_pair(sc, args)) {
		log_error("%s: expected list of args\n", __FUNCTION__);
		return sc->NIL;
	}

	car = scm_car(sc, args);

	if (! scm_is_ptr(sc, car)) {
		log_error("%s: not a pointer\n", __FUNCTION__);
		return sc->NIL;
	}

	window = (SDL_Window*)scm_ptr_val(sc, car);
	SDL_DestroyWindow(window);
	return sc->T;
}

/**
 * Wrapper for SDL_CreateRenderer.
 */
static pointer sdl2_create_renderer(scheme *sc, pointer args)
{
	SDL_Window *window=NULL;
	SDL_Renderer *renderer=NULL;
	pointer car=NULL;

	log_debug("sdl2_create_renderer:enter\n");
	if (! scm_is_pair(sc, args)) {
		log_error("%s: expected list of args\n", __FUNCTION__);
		return sc->NIL;
	}

	car = scm_car(sc, args);

	if (! scm_is_ptr(sc, car)) {
		log_error("%s: not a pointer\n", __FUNCTION__);
		return sc->NIL;
	}

	window = (SDL_Window*)scm_ptr_val(sc, car);

	if (! (renderer = SDL_CreateRenderer(window, -1, 0))) {
		log_error("SDL_CreateRenderer: %s\n", SDL_GetError());
		return sc->NIL;
	}

	return scm_mk_ptr(sc, renderer);
}

/**
 * Wrapper for SDL_DestroyRenderer.
 */
static pointer sdl2_destroy_renderer(scheme *sc, pointer args)
{
	SDL_Renderer *renderer=NULL;
	pointer car=NULL;

	log_debug("sdl2_destroy_renderer:enter\n");
	if (! scm_is_pair(sc, args)) {
		log_error("%s: expected list of args\n", __FUNCTION__);
		return sc->NIL;
	}

	car = scm_car(sc, args);

	if (! scm_is_ptr(sc, car)) {
		log_error("%s: not a pointer\n", __FUNCTION__);
		return sc->NIL;
	}

	renderer = (SDL_Renderer*)scm_ptr_val(sc, car);
	SDL_DestroyRenderer(renderer);
	return sc->T;

}

/**
 * Initialize this dynamic extension.
 *
 * This is called by tinyscheme in response to
 *
 *     (load-extension "sdl2")
 *
 * It adds all of our functions into the scheme environment.
 */
void init_sdl2(scheme *sc)
{
	scm_define_api_call(sc, "sdl2-init", sdl2_init);
	scm_define_api_call(sc, "sdl2-create-window", sdl2_create_window);
	scm_define_api_call(sc, "sdl2-destroy-window", sdl2_destroy_window);
	scm_define_api_call(sc, "sdl2-create-renderer",
			    sdl2_create_renderer);
	scm_define_api_call(sc, "sdl2-destroy-renderer",
			    sdl2_destroy_renderer);
}
