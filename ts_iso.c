/**
 * Tinyscheme extension to wrap iso.c.
 */

#include "iso.h"
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
 * Wrapper for iso_blit.
 *
 * (iso-blit renderer texture textsrc loclist)
 */
static pointer ts_iso_blit(scheme *sc, pointer args)
{
        SDL_Rect src, *psrc=NULL;
        SDL_Renderer *renderer=NULL;
        SDL_Texture *texture=NULL;
        pointer plist, ploclist, ploc;
        int map_x, map_y;

        if (scm_unpack(sc, &args, "ppll", &renderer, &texture, &plist,
		       &ploclist)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (plist != sc->NIL) {
                if (scm_unpack(sc, &plist, "dddd", &src.x, &src.y, &src.w,
                               &src.h)) {
                        log_error("%s:textsrc:%s\n", __FUNCTION__,
                                  scm_get_error());
                        return sc->NIL;
                }
                psrc = &src;
        }

	while (ploclist != sc->NIL) {
		if (scm_unpack(sc, &ploclist, "l", &ploc)) {
			log_error("%s:loclist:%s\n", __FUNCTION__,
				  scm_get_error());
			return sc->NIL;
		}
		if (scm_unpack(sc, &ploc, "dd", &map_x, &map_y)) {
			log_error("%s:loc:%s\n", __FUNCTION__,
				  scm_get_error());
			return sc->NIL;
		}
		iso_blit(renderer, texture, psrc, map_x, map_y);
	}

        return sc->NIL;
}

/**
 * Fill an iso grid with a texture.
 *
 * (iso-fill renderer texture textsrc mapdst)
 */
static pointer ts_iso_fill(scheme *sc, pointer args)
{
        SDL_Rect src, map, *psrc=NULL;
        SDL_Renderer *renderer=NULL;
        SDL_Texture *texture=NULL;
        pointer plist, mlist;


        if (scm_unpack(sc, &args, "ppll", &renderer, &texture, &plist,
                       &mlist)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (plist != sc->NIL) {
                if (_scm_unpack_rect(sc, &plist, &src)) {
                        log_error("%s:textsrc:%s\n", __FUNCTION__,
                                  scm_get_error());
                        return sc->NIL;
                }
                psrc = &src;
        }

        /* XXX: only need two ints, not a full SDL_Rect */
        if (_scm_unpack_rect(sc, &mlist, &map)) {
                log_error("%s:mapdst:%s\n", __FUNCTION__,
                          scm_get_error());
                return sc->NIL;
        }

        iso_fill(renderer, texture, psrc, map.w, map.h);

        return sc->NIL;
}

/**
 * Render isometrict grid lines.
 *
 * (iso-grid renderer width height)
 */
static pointer ts_iso_grid(scheme *sc, pointer args)
{
        SDL_Renderer *renderer;
        int map_w, map_h;

        if (scm_unpack(sc, &args, "pdd", &renderer, &map_w, &map_h)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        iso_grid(renderer, map_w, map_h);

        return sc->NIL;
}

/**
 * Convert screen coordinates back to map coordinates.
 *
 * (screen-to-map x y)
 *
 * Returns a (x . y) pair, or nil if the coords are off-map.
 */
static pointer ts_iso_screen_to_map(scheme *sc, pointer args)
{
        int screen_x, screen_y, map_x, map_y;

        if (scm_unpack(sc, &args, "dd", &screen_x, &screen_y)) {
                log_error("%s:%s\n", __FUNCTION__, scm_get_error());
                return sc->NIL;
        }

        if (iso_screen_to_map(screen_x, screen_y, &map_x, &map_y)) {
                return sc->NIL;
        }

        return scm_cons(sc, scm_mk_int(sc, map_x),
			scm_cons(sc, scm_mk_int(sc, map_y), sc->NIL));
}

void init_ts_iso(scheme *sc)
{
        scm_define_api_call(sc, "iso-blit", ts_iso_blit);
        scm_define_api_call(sc, "iso-fill", ts_iso_fill);
        scm_define_api_call(sc, "iso-grid", ts_iso_grid);
        scm_define_api_call(sc, "iso-screen-to-map", ts_iso_screen_to_map);
}
