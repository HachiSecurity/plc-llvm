#include "pair0.h"

int main() {
    closure *fst = plc_new_text("Hello");
    closure *snd = plc_new_integer("42");
    closure *arg = plc_new_pair(fst, snd);
    closure *ptr = plc_entry();
    closure *res = plc_apply(ptr, arg);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
