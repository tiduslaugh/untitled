#include <stdio.h>
#include <libguile.h>
#include <curses.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

typedef int error_t;

error_t find_root_dir(char** out_buf) {
    char* buf = malloc(1200); // some extra to add to the path
    struct stat stats;
    error_t return_value;
    if (buf == NULL) {
        return ENOMEM;
    }
    while(1) {
        char* cwd = getcwd(buf, 1024);
        if (cwd == NULL) {
            return_value = errno;
            free(buf);
            return return_value;
        }
        printf("Checking path %s...\n", cwd);
        if (strcmp(cwd, "/") == 0) {
            // we've reached the root, better stop searching
            return_value = errno;
            free(buf);
            return ENOENT;
        }
        char* filepath = strdup(cwd);
        strcat(filepath, "/config.scm");
        int status = stat(filepath, &stats);
        free(filepath);
        if(status == 0) {
            // file exists, off we go
            printf("Directory found: %s\n", cwd);
            *out_buf = cwd;
            return 0;
        }
        chdir("..");
    }
}

void *load_config(void *data) {
    char *root_dir;
    error_t err = find_root_dir(&root_dir);
    if (err) {
        return NULL;
    }
    char *file_path = strdup(root_dir);
    strcat(file_path, "/config.scm");
    SCM s = scm_c_primitive_load(file_path);

    char *rep = scm_to_locale_string(scm_object_to_string(s, SCM_UNDEFINED));
    printf("Scheme object is %s\n", rep);
    free(rep);
}

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
    scm_with_guile(load_config, NULL);
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
