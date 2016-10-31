#include <SDL2/SDL.h>
#include <SDL/SDL_image.h>
#include <getopt.h>
#include <stdio.h>

struct args {

};


/**
 * Handle key presses.
 */
void on_keydown(SDL_KeyboardEvent *event)
{
        switch (event->keysym.sym) {
        case SDLK_LEFT:
                break;
        case SDLK_RIGHT:
                break;
        case SDLK_UP:
                break;
        case SDLK_DOWN:
                break;
        case SDLK_q:
                exit(0);
        default:
                break;
        }
}


/**
 * Print a command-line usage message.
 */
static void print_usage(void)
{
        printf("Usage:  demo [options]\n"
               "Options: \n"
               "    -h:	help\n"
                );
}


/**
 * Parse command-line args.
 */
static void parse_args(int argc, char **argv, struct args *args)
{
        int c = 0;

        memset(args, 0, sizeof(*args));

        while ((c = getopt(argc, argv, "t:h")) != -1) {
                switch (c) {
                case 'h':
                        print_usage();
                        exit(0);
                case '?':
                default:
                        print_usage();
                        exit(-1);
                        break;
                }
        }
}


int main(int argc, char **argv)
{
        SDL_Event event;
        SDL_Window *window;
        int done=0;
        Uint32 start_ticks, end_ticks, frames=0;
        struct args args;

        parse_args(argc, argv, &args);

        /* Init SDL */
        if (SDL_Init(SDL_INIT_VIDEO)) {
                printf("SDL_Init: %s\n", SDL_GetError());
                return -1;
        }

        /* Cleanup SDL on exit. */
        atexit(SDL_Quit);

        /* Create the main window */
        if (! (window = SDL_CreateWindow(
                       "Demo", SDL_WINDOWPOS_UNDEFINED,
                       SDL_WINDOWPOS_UNDEFINED, 640, 480,
                       SDL_WINDOW_OPENGL | SDL_WINDOW_SHOWN))) {
                printf("SDL_Init: %s\n", SDL_GetError());
                return -1;
        }

        start_ticks = SDL_GetTicks();

        while (!done) {
                frames++;
                while (SDL_PollEvent(&event)) {
                        switch (event.type) {
                        case SDL_QUIT:
                                done = 1;
                                break;
                        case SDL_KEYDOWN:
                                on_keydown(&event.key);
                                break;
                        default:
                                break;
                        }
                }

        }

        end_ticks = SDL_GetTicks();
        if (end_ticks > start_ticks) {
                printf("%2.2f FPS\n",
                       ((double)frames * 1000) / (end_ticks - start_ticks)
                        );
        }

        SDL_DestroyWindow(window);

        return 0;
}
