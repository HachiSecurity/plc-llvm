#include "data0.h"

int main() {
    closure *tag = plc_new_integer("108");
    closure *arg = plc_new_data_constr(tag, plc_empty_list());
    closure *ptr = plc_entry();
    closure *res = plc_apply(ptr, arg);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
