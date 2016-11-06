#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <getopt.h>
#include <stdio.h>


struct args {
        char *filename;
        char *cmd;
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
        printf("Usage:  demo [options] [command]\n"
               "Options: \n"
               "    -h:	help\n"
               "    -i: image filename\n"
               "Commands:\n"
               "    info    Show graphics info\n"
                );
}


static void draw_grid(SDL_Renderer *renderer)
{
	SDL_SetRenderDrawColor(renderer, 0xff, 0xff, 0xff, SDL_ALPHA_OPAQUE);
	SDL_RenderClear(renderer);
	SDL_SetRenderDrawColor(renderer, 0x10, 0xf0, 0xf0, SDL_ALPHA_OPAQUE);
	SDL_RenderDrawLine(renderer, 0, 0, 200, 200);
	SDL_RenderPresent(renderer);
}


/**
 * Parse command-line args.
 */
static void parse_args(int argc, char **argv, struct args *args)
{
        int c = 0;

        memset(args, 0, sizeof(*args));

        while ((c = getopt(argc, argv, "i:h")) != -1) {
                switch (c) {
                case 'i':
                        args->filename = optarg;
                        break;
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

        /* Any remaining option is assumed to be the save-file to load the game
         * from. If there is none then abort. */
        if (optind < argc) {
                args->cmd = argv[optind];
        }
}

/**
 * Print renderer info to stdout.
 */
static void show_renderer_info(SDL_RendererInfo *info)
{
        Uint32 tfi;

        printf("name: %s\n", info->name);
        printf("\tflags: 0x%x - ", info->flags);
        if (info->flags & SDL_RENDERER_SOFTWARE) {
                printf("software ");
        }
        if (info->flags & SDL_RENDERER_ACCELERATED) {
                printf("accelerated ");
        }
        if (info->flags & SDL_RENDERER_PRESENTVSYNC) {
                printf("presentvsync ");
        }
        if (info->flags & SDL_RENDERER_TARGETTEXTURE) {
                printf("targettexture ");
        }
        printf("\n");
        printf("\tnum_texture_formats: %d\n", info->num_texture_formats);
        for (tfi = 0; tfi < info->num_texture_formats; tfi++) {
                Uint32 tf = info->texture_formats[tfi];
                printf("\tTexture format %d:\n", tfi);
                printf("\t\tname: %s\n", SDL_GetPixelFormatName(tf));
                printf("\t\ttype: %d\n", SDL_PIXELTYPE(tf));
                printf("\t\torder: %d\n", SDL_PIXELORDER(tf));
                printf("\t\tlayout: %d\n", SDL_PIXELLAYOUT(tf));
                printf("\t\tbitsperpixel: %d\n", SDL_BITSPERPIXEL(tf));
                printf("\t\tbytesperpixel: %d\n",
                       SDL_BYTESPERPIXEL(tf));
                printf("\t\tindexed: %c\n",
                       SDL_ISPIXELFORMAT_INDEXED(tf) ? 'y': 'n');
                printf("\t\talpha: %c\n",
                       SDL_ISPIXELFORMAT_ALPHA(tf) ? 'y': 'n');
                printf("\t\tfourcc: %c\n",
                       SDL_ISPIXELFORMAT_FOURCC(tf) ? 'y': 'n');
        }
        printf("\tmax_texture_width: %d\n", info->max_texture_width);
        printf("\tmax_texture_height: %d\n", info->max_texture_height);

}

/**
 * Print available driver info to stdout.
 */
static void show_driver_info(void)
{
        int i, n;

        n = SDL_GetNumRenderDrivers();
        printf("%d renderer drivers\n", n);
        for (i = 0; i < n; i++) {
                SDL_RendererInfo info;

                if (SDL_GetRenderDriverInfo(i, &info)) {
                        printf("SDL_GetRenderDriverInfo(%d): %s\n",
                               i, SDL_GetError());
                        exit(-1);
                }

                show_renderer_info(&info);
        }
}

int main(int argc, char **argv)
{
        SDL_Event event;
        SDL_Window *window=NULL;
        SDL_Renderer *renderer=NULL;
        SDL_RendererInfo info;
        int done=0;
        Uint32 start_ticks, end_ticks, frames=0;
        struct args args;
        //SDL_Surface *image=NULL, *screen=NULL;


        parse_args(argc, argv, &args);

        if (args.cmd) {
                if (! strcmp(args.cmd, "info")) {
                        show_driver_info();
                }
        }

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
                printf("SDL_CreateWindow: %s\n", SDL_GetError());
                return -1;
        }

        /* Create the renderer. */
        if (! (renderer = SDL_CreateRenderer(window, -1, 0))) {
                printf("SDL_CreateRenderer: %s\n", SDL_GetError());
                goto destroy_window;
        }

        SDL_GetRendererInfo(renderer, &info);
        printf("Created renderer info:\n");
        //show_renderer_info(&info);


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

                draw_grid(renderer);
        }

        end_ticks = SDL_GetTicks();
        if (end_ticks > start_ticks) {
                printf("%2.2f FPS\n",
                       ((double)frames * 1000) / (end_ticks - start_ticks)
                        );
        }

        SDL_DestroyRenderer(renderer);
destroy_window:
        SDL_DestroyWindow(window);

        return 0;
}
