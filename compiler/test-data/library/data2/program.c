#include "data2.h"

int main() {
    closure *hd = plc_new_integer("108");
    closure *list = plc_new_list(hd, plc_empty_list());
    closure *arg = plc_new_data_list(list);
    closure *ptr = plc_entry();
    closure *res = plc_apply(ptr, arg);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
