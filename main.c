#include <stdio.h>
#include <libguile.h>
#include <curses.h>
#include <assert.h>

void *register_functions(void *data) {
    SCM result = scm_c_eval_string("(+ 2 2)");
    assert(scm_is_integer(result));
    printf("%d\n", scm_to_int(result));
    return NULL;
}

void init_curses() {
    WINDOW *window = initscr();
    cbreak();
    noecho();
    intrflush(window, FALSE);
    keypad(window, true);
}

int main(int argc, char **argv) {
    printf("Hello, World!\n");
    scm_with_guile(register_functions, NULL);

    while (true) {
        int ch = getch();
        if (ch == 'q') {
            break;
        }
    }

    endwin();
    return 0;
}
