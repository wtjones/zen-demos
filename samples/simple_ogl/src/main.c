#ifdef __APPLE__
#    include <OpenGL/glu.h>
#else
#    include <GL/glu.h>
#endif
#include <SDL2/SDL.h>
#include <SDL2/SDL_opengl.h>
#include <stdbool.h>

#define SCREEN_WIDTH 640
#define SCREEN_HEIGHT 480

SDL_GLContext gl_context;

char* repr_mat_4x4(char* buffer, size_t count, float s[16])
{
    char matrix_buffer[255];
    buffer[0] = '\0';
    strcat(buffer, "[");

    for (int i = 0; i < 16; i++) {
        // strcat(buffer, "\n[");

        snprintf(
            matrix_buffer,
            sizeof matrix_buffer,
            "%12.5f",
            s[i]);

        strcat(buffer, matrix_buffer);
        strcat(buffer, ", ");

        if ((i + 1) % 4 == 0) {

            strcat(buffer, "]");
            strcat(buffer, "\n[");
        }
    }

    strcat(buffer, "]");
    return buffer;
}

bool init_gl()
{
    GLenum error = GL_NO_ERROR;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-100.0, 10.0, -10.0, 10.0, -10.5, 10.0);

    glClearColor(0.f, 0.f, 0.f, 1.f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    GLfloat proj[16];
    glGetFloatv(GL_PROJECTION_MATRIX, proj);

    char buffer[500];
    printf("%s\n", repr_mat_4x4(buffer, sizeof(buffer), proj));

    error = glGetError();
    if (error != GL_NO_ERROR) {
        printf("Error initializing OpenGL! %s\n", gluErrorString(error));
        return false;
    }
    return true;
}

void render()
{
    glClear(GL_COLOR_BUFFER_BIT);

    glBegin(GL_QUADS);
    glVertex2f(-0.5f, -0.5f);
    glVertex2f(0.5f, -0.5f);
    glVertex2f(0.5f, 0.5f);
    glVertex2f(-0.5f, 0.5f);
    glEnd();
}

int main(int argc, char* argv[])
{

    SDL_bool should_quit = SDL_FALSE;
    char str[80];

    if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
        printf("SDL error \n");
        return 1;
    }

    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);

    SDL_Window* win = SDL_CreateWindow("Simple Demo", 100, 100, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL);

    gl_context = SDL_GL_CreateContext(win);

    if (gl_context == NULL) {
        printf("OpenGL context could not be created. SDL Error: %s\n", SDL_GetError());
        return 1;
    }

    if (SDL_GL_SetSwapInterval(1) < 0) {
        printf("Unable to set swap interval. SDL Error: %s\n", SDL_GetError());
        return 1;
    }

    if (!init_gl()) {
        printf("OpenGL init error\n");
        return 1;
    }

    int last_frame = SDL_GetTicks();
    while (!should_quit) {
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            switch (event.type) {
            case SDL_QUIT:
                should_quit = SDL_TRUE;
                break;
            case SDL_KEYUP:
                should_quit = event.key.keysym.scancode == SDL_SCANCODE_ESCAPE;
            }
        }
        int num_keys;
        const Uint8* keys = SDL_GetKeyboardState(&num_keys);

        while (SDL_GetTicks() - last_frame < 1000 / 60) {
        }
        last_frame = SDL_GetTicks();

        render();
        SDL_GL_SwapWindow(win);
    }

    SDL_DestroyWindow(win);

    SDL_Quit();
    return 0;
}
