#include <stdio.h>
#include <libguile.h>
#include <curses.h>

void* register_functions(void* data) {
    return NULL;
}

int main(int argc, char **argv) {
    printf("Hello, World!\n");
    scm_with_guile(register_functions, NULL);

    WINDOW* window = initscr();
    cbreak();
    noecho();
    intrflush(window, FALSE);
    keypad(window, true);

    while(true) {
        int ch = getch();
        if (ch == 'q') {
            break;
        }
    }

    endwin();
    return 0;
}
