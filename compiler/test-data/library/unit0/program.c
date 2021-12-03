#include "unit0.h"

int main() {
    closure *arg = plc_new_unit();
    closure *ptr = plc_entry();
    closure *res = plc_apply(ptr, arg);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
