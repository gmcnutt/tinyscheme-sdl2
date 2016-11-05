#include <SDL2/SDL.h>
#include "scm.h"
#include "log.h"


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
	log_init();

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

	log_debug("%s:enter\n", __FUNCTION__);
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

	log_debug("%s:enter\n", __FUNCTION__);
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
 * Wrapper for SDL_SetRenderDrawColor.
 */
static pointer sdl2_set_render_draw_color(scheme *sc, pointer args)
{
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
	scm_define_api_call(sc, "sdl2-set-render-draw-color",
			    sdl2_set_render_draw_color);
}
