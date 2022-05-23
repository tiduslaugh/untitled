#include <stdio.h>
#include <libguile.h>
#include <curses.h>
#include <assert.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>

//typedef int error_t;
typedef struct {
  int code;
  const char *file;
  int line;
} error_t;

error_t make_error_(int code, const char *file, int line) {
    error_t ret = {code, file, line};
    return ret;
}
#define make_error(code) make_error_(code, __FILE__, __LINE__)

const error_t SUCCESS = {0, NULL, -1};

inline bool is_success(error_t err) {
    return err.code == 0;
}
// Error trap macro.
#define ERRT(x) { error_t err = (x); if (!is_success(err)) { return err; } }

error_t find_root_dir(char **out_buf) {
    char buf[1024];
    char filebuf[1200]; // some extra to add to the path
    struct stat stats;
    error_t return_value;

    while (1) {
        char *cwd = getcwd(buf, 1024);
        if (cwd == NULL) {
            return make_error(errno);
        }
        printf("Checking path %s...\n", cwd);
        if (strcmp(cwd, "/") == 0) {
            // we've reached the root, better stop searching
            return make_error(ENOENT);
        }
        strncpy(filebuf, cwd, sizeof(filebuf));
        strncat(filebuf, "/config.scm", sizeof(filebuf) - strlen(filebuf) - 1);
        int status = stat(filebuf, &stats);
        if (status == 0) {
            // file exists, off we go
            printf("Directory found: %s\n", cwd);
            *out_buf = strdup(cwd);
            return SUCCESS;
        }
        chdir("..");
    }
}

error_t load_config() {
    char *root_dir;
    ERRT(find_root_dir(&root_dir));
    char *file_path = calloc(strlen(root_dir) + sizeof("/config.scm"), sizeof(char)); // sizeof string includes \0
    strcpy(file_path, root_dir);
    strcat(file_path, "/config.scm");

    SCM s = scm_c_primitive_load(file_path);
    char *representation = scm_to_locale_string(scm_object_to_string(s, SCM_UNDEFINED));
    printf("scheme object is %s\n", representation);
    free(representation);
    free(file_path);
    free(root_dir);
    return SUCCESS;
}

void register_functions() {
    SCM result = scm_c_eval_string("(+ 2 2)");
    assert(scm_is_integer(result));
    printf("%d\n", scm_to_int(result));
}

void init_curses() {
    initscr();
    cbreak();
    echo();
    intrflush(stdscr, false);
    keypad(stdscr, true);
}

void guile_main(void *unused, int argc, char **argv) {
    load_config();
    register_functions();
    init_curses();

    while (true) {
        int ch = getch();
        if (ch == 'q') {
            break;
        }
    }

    endwin();
}

int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, guile_main, NULL);
    return 0;
}
