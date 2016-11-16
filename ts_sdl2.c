#include "log.h"
#include "scm.h"
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>


/**
 * Unpack an SDL_Rect.
 */
static inline int _scm_unpack_rect(scheme *sc, pointer *args, SDL_Rect *rect)
{
        return scm_unpack(
                sc, args, "dddd", &rect->x, &rect->y, &rect->w, &rect->h);
}


/**
 * Initialize the SDL2 subsystem.
 */
static pointer ts_sdl2_init(scheme *sc, pointer args)
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
static pointer ts_sdl2_create_window(scheme *sc, pointer args)
{
        SDL_Window *window;

        /* Create the main window */
        if (! (window = SDL_CreateWindow(
                       "Demo", SDL_WINDOWPOS_UNDEFINED,
                       SDL_WINDOWPOS_UNDEFINED, 640, 480,
                       SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN))) {
                log_error("SDL_CreateWindow: %s\n", SDL_GetError());
                return sc->NIL;
        }

        /* Return the window as a foreign pointer. */
        log_debug("%s:%p\n", __FUNCTION__, window);
        return scm_mk_ptr(sc, window);
}


/**
 * Wrapper for SDL_DestroyWindow.
 *
 * (sdl2-destroy-window window)
 */
static pointer ts_sdl2_destroy_window(scheme *sc, pointer args)
{
        SDL_Window *window=NULL;

        if (scm_unpack(sc, &args, "p", &window)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        log_debug("%s:%p\n", __FUNCTION__, window);
        SDL_DestroyWindow(window);
        return sc->T;
}


/**
 * Wrapper for SDL_CreateRenderer.
 *
 * (let ((renderer (sdl2-create-renderer window))) ...)
 */
static pointer ts_sdl2_create_renderer(scheme *sc, pointer args)
{
        SDL_Window *window=NULL;
        SDL_Renderer *renderer=NULL;

        if (scm_unpack(sc, &args, "p", &window)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! (renderer = SDL_CreateRenderer(window, -1, 0))) {
                log_error("SDL_CreateRenderer: %s\n", SDL_GetError());
                return sc->NIL;
        }

        log_debug("%s:%p\n", __FUNCTION__, renderer);
        return scm_mk_ptr(sc, renderer);
}


/**
 * Wrapper for SDL_DestroyRenderer.
 *
 * (sdl2-destroy-renderer renderer)
 */
static pointer ts_sdl2_destroy_renderer(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;

        if (scm_unpack(sc, &args, "p", &renderer)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        log_debug("%s:%p\n", __FUNCTION__, renderer);

        SDL_DestroyRenderer(renderer);
        return sc->T;

}


/**
 * Wrapper for SDL_SetRenderDrawColor.
 *
 * (sdl2-set-render-draw-color renderer red green blue alpha)
 *
 * Where red, green, blue and alpha are all 0-255.
 */
static pointer ts_sdl2_set_render_draw_color(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        int red, green, blue, alpha;

        if (scm_unpack(sc, &args, "pdddd", &renderer, &red, &green, &blue,
                       &alpha)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }
        SDL_SetRenderDrawColor(renderer, red, green, blue, alpha);

        return sc->T;
}


/**
 * Wrapper for SDL_RenderClear.
 *
 * (sdl2-render-clear renderer)
 */
static pointer ts_sdl2_render_clear(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;

        if (scm_unpack(sc, &args, "p", &renderer)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        SDL_RenderClear(renderer);
        return sc->T;
}


/**
 * Wrapper for SDL_RenderDrawLine.
 *
 * (sdl2-render-draw-line renderer x0 y0 x1 y1)
 *
 * Draws a line on the current rendering target.
 */
static pointer ts_sdl2_render_draw_line(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        int x0, y0, x1, y1;

        if (scm_unpack(sc, &args, "pdddd", &renderer, &x0, &y0, &x1, &y1)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }
        SDL_RenderDrawLine(renderer, x0, y0, x1, y1);
        return sc->T;
}


/**
 * Wrapper for SDL_RenderPresent.
 *
 * (sdl2-render-present renderer)
 *
 * Update the screen with any rendering performed since the previous call.
 */
static pointer ts_sdl2_render_present(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;

        if (scm_unpack(sc, &args, "p", &renderer)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        SDL_RenderPresent(renderer);
        return sc->T;
}


/**
 * Wrapper for SDL_PollEvent. Returns a list where the first element is the
 * event type and the remaining elements depend on the type.
 *
 * (let ((event (sdl2-poll-event))) ...)
 *
 * Return details:
 *   default: '(<type>)
 *   Mouse button down: '(<type> <x> <y>)
 */
static pointer ts_sdl2_poll_event(scheme *sc, pointer args)
{
        SDL_Event event;
        pointer head = sc->NIL;

        if (! SDL_PollEvent(&event)) {
                return head;
        }

        switch (event.type) {
        case SDL_MOUSEBUTTONDOWN:
                if (scm_pack(sc, &head, "ddd", event.type, event.button.x,
                             event.button.y)) {
                        log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                        head = sc->NIL;
                }
                break;
        default:
                head = scm_cons(sc, scm_mk_int(sc, event.type), sc->NIL);
                break;
        }

        return head;
}


/**
 * Wrapper for SDL_Delay.
 *
 * (sdl2-delay ms)
 */
static pointer ts_sdl2_delay(scheme *sc, pointer args)
{
        int ms;

        if (scm_unpack(sc, &args, "d", &ms)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->F;
        }
        SDL_Delay(ms);
        return sc->T;
}


/**
 * Wrapper for SDL_GetTicks.
 */
static pointer ts_sdl2_get_ticks(scheme *sc, pointer args)
{
        return scm_mk_int(sc, SDL_GetTicks());
}


/**
 * Wrapper that combines IMG_Load with SDL_CreateTextureFromSurface.
 *
 * (sdl2-load-texture renderer "picture.png")
 */
static pointer ts_sdl2_load_texture(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        SDL_Surface *surface=NULL;
        SDL_Texture *texture=NULL;
        char *filename = NULL;
        pointer result = sc->NIL;

        if (scm_unpack(sc, &args, "ps", &renderer, &filename)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }

        if (! filename) {
                log_error("%s: NULL filename\n", __FUNCTION__);
                return sc->NIL;
        }

        if (! (surface = IMG_Load(filename))) {
                log_error("%s:%s:IMG_Load:%s\n", __FUNCTION__, filename,
			  SDL_GetError());
                return sc->NIL;
        }

        if (! (texture = SDL_CreateTextureFromSurface(renderer, surface))) {
                log_error("%s:SDL_CreateTextureFromSurface:%s\n",
                          __FUNCTION__, SDL_GetError());
                goto free_surface;
        }

        log_debug("%s:%p\n", __FUNCTION__, texture);
        result = scm_mk_ptr(sc, texture);

free_surface:
        SDL_FreeSurface(surface);
        return result;
}


/**
 * Wrapper for SDL_DestroyTexture.
 */
static pointer ts_sdl2_destroy_texture(scheme *sc, pointer args)
{
        SDL_Texture *texture=NULL;

        if (scm_unpack(sc, &args, "p", &texture)) {
                log_error("%s: %s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        log_debug("%s:%p\n", __FUNCTION__, texture);
        SDL_DestroyTexture(texture);
        return sc->T;
}


/**
 * Wrapper for SDL_RenderCopy.
 *
 * (sdl2-render-copy renderer texture srcrect dstrect)
 *
 * Where `srcrect` and `dstrect` are nil or lists of 4 non-negative
 * integers.
 */
static pointer ts_sdl2_render_copy(scheme *sc, pointer args)
{
        SDL_Renderer *renderer=NULL;
        SDL_Texture *texture=NULL;
        pointer slist, dlist;
        SDL_Rect src, dst;
        SDL_Rect *psrc=NULL, *pdst=NULL;

        if (scm_unpack(sc, &args, "ppll", &renderer, &texture, &slist,
                       &dlist)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (! renderer) {
                log_error("%s: NULL renderer\n", __FUNCTION__);
                return sc->NIL;
        }

        if (! texture) {
                log_error("%s: NULL texture\n", __FUNCTION__);
                return sc->NIL;
        }

        if (slist != sc->NIL) {
                if (_scm_unpack_rect(sc, &slist, &src)) {
                        log_error("%s:source:%s\n", __FUNCTION__,
                                  scm_get_error());
                        return sc->NIL;
                }
                psrc = &src;
        }

        if (dlist != sc->NIL) {
                if (_scm_unpack_rect(sc, &dlist, &dst)) {
                        log_error("%s:source:%s\n", __FUNCTION__,
                                  scm_get_error());
                        return sc->NIL;
                }
                pdst = &dst;
        }

        if (SDL_RenderCopy(renderer, texture, psrc, pdst)) {
                log_error("%s:SDL_RenderCopy:%s\n", __FUNCTION__,
                          SDL_GetError());
                return sc->NIL;
        }
        return sc->T;
}


/**
 * Initialize this dynamic extension.
 *
 * This is called by tinyscheme in response to
 *
 *     (load-extension "sdl2")
 *
 * Adds all of our functions into the scheme environment.
 */
void init_ts_sdl2(scheme *sc)
{
        scm_define_api_call(sc, "sdl2-create-renderer", ts_sdl2_create_renderer);
        scm_define_api_call(sc, "sdl2-create-window", ts_sdl2_create_window);
        scm_define_api_call(sc, "sdl2-delay", ts_sdl2_delay);
        scm_define_api_call(sc, "sdl2-destroy-renderer", ts_sdl2_destroy_renderer);
        scm_define_api_call(sc, "sdl2-destroy-texture", ts_sdl2_destroy_texture);
        scm_define_api_call(sc, "sdl2-destroy-window", ts_sdl2_destroy_window);
        scm_define_api_call(sc, "sdl2-get-ticks", ts_sdl2_get_ticks);
        scm_define_api_call(sc, "sdl2-init", ts_sdl2_init);
        scm_define_api_call(sc, "sdl2-load-texture", ts_sdl2_load_texture);
        scm_define_api_call(sc, "sdl2-poll-event", ts_sdl2_poll_event);
        scm_define_api_call(sc, "sdl2-render-clear", ts_sdl2_render_clear);
        scm_define_api_call(sc, "sdl2-render-draw-line", ts_sdl2_render_draw_line);
        scm_define_api_call(sc, "sdl2-render-present", ts_sdl2_render_present);
        scm_define_api_call(sc, "sdl2-set-render-draw-color", ts_sdl2_set_render_draw_color);
        scm_define_api_call(sc, "sdl2-render-copy", ts_sdl2_render_copy);
        scm_define_int(sc, "sdl2-alpha-opaque", SDL_ALPHA_OPAQUE);
        scm_define_int(sc, "sdl2-mouse-button-down", SDL_MOUSEBUTTONDOWN);
        scm_define_int(sc, "sdl2-quit", SDL_QUIT);
}
