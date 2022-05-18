#include <stdio.h>
#include <libguile.h>

void* register_functions(void* data) {
    return NULL;
}

int main(int argc, char **argv) {
    printf("Hello, World!\n");
    scm_with_guile(register_functions, NULL);
    scm_shell(argc, argv);
    return 0;
}
