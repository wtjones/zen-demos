#include <stdio.h>
#include <stdlib.h>
#include <GLFW/glfw3.h>
#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

static void draw_filled_quad(float x, float y, float w, float h, float r, float g, float b)
{
    glColor3f(r, g, b);
    glBegin(GL_QUADS);
    glVertex2f(x, y);
    glVertex2f(x + w, y);
    glVertex2f(x + w, y + h);
    glVertex2f(x, y + h);
    glEnd();
}

int main(void)
{
    if (!glfwInit())
    {
        fprintf(stderr, "Failed to init GLFW\n");
        return 1;
    }

    GLFWwindow *window = glfwCreateWindow(640, 480, "Ortho 320x240 (pixel-doubled)", NULL, NULL);
    if (!window)
    {
        glfwTerminate();
        return 1;
    }

    glfwMakeContextCurrent(window);

    /* Camera (world-space pan) state */
    double last_time = glfwGetTime();
    float cam_x = 0.0f;
    float cam_y = 0.0f;

    while (!glfwWindowShouldClose(window))
    {
        int fb_width, fb_height;
        glfwGetFramebufferSize(window, &fb_width, &fb_height);
        glViewport(0, 0, fb_width, fb_height);

        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        /* Logical coordinates: 0..320 x 0..240 (bottom-left origin).
            Use conventional OpenGL (+Y up). Window 640x480 will pixel-double. */
        glOrtho(0.0, 320.0, 0.0, 240.0, -1.0, 1.0);

        /* update delta time and poll arrow keys to pan camera */
        double current_time = glfwGetTime();
        double delta_time = current_time - last_time;
        last_time = current_time;

        const float pan_speed = 120.0f; /* logical units per second */
        if (glfwGetKey(window, GLFW_KEY_LEFT) == GLFW_PRESS)
            cam_x -= pan_speed * (float)delta_time;
        if (glfwGetKey(window, GLFW_KEY_RIGHT) == GLFW_PRESS)
            cam_x += pan_speed * (float)delta_time;
        if (glfwGetKey(window, GLFW_KEY_UP) == GLFW_PRESS)
            cam_y += pan_speed * (float)delta_time;
        if (glfwGetKey(window, GLFW_KEY_DOWN) == GLFW_PRESS)
            cam_y -= pan_speed * (float)delta_time;

        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        /* apply camera transform (move world opposite to camera) */
        glTranslatef(-cam_x, -cam_y, 0.0f);

        glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        /* Draw a mixture of 8x8 and 16x16 filled quads.
            The original coordinates used a top-left origin (y=0 at top).
            Convert those top-left coordinates to bottom-left coordinates
            so the visual placement remains the same. */
        float top_to_bottom_8 = 240.0f - 8.0f - 8.0f;       /* for h=8 */
        float top_to_bottom_16_at8 = 240.0f - 8.0f - 16.0f; /* for h=16, y=8 */
        float top_to_bottom_8_at20 = 240.0f - 20.0f - 8.0f;
        draw_filled_quad(300, 215, 8, 8, 1.0f, 0.0f, 0.0f);
        draw_filled_quad(32, top_to_bottom_16_at8, 16, 16, 0.0f, 1.0f, 0.0f);
        draw_filled_quad(60, top_to_bottom_8_at20, 8, 8, 0.0f, 0.0f, 1.0f);

        /* pattern row of 8x8 (convert top-left y=180 to bottom-left) */
        for (int i = 0; i < 10; ++i)
        {
            float r = (i & 1) ? 1.0f : 0.2f;
            float g = (i & 2) ? 1.0f : 0.2f;
            float b = (i & 4) ? 1.0f : 0.2f;
            float x = 10 + i * 24;
            float y_top = 180.0f;
            float y = 240.0f - y_top - 8.0f; /* convert top-left to bottom-left for h=8 */
            draw_filled_quad(x, y, 8, 8, r, g, b);
        }

        glfwSwapBuffers(window);
        glfwPollEvents();

        if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
            glfwSetWindowShouldClose(window, GLFW_TRUE);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
