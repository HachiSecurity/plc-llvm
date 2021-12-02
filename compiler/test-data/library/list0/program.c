#include "list0.h"

int main() {
    closure *head = plc_new_integer("4");
    closure *eight = plc_new_integer("8");
    closure *tail = plc_new_list(eight, plc_empty_list());
    closure *ptr = plc_entry();
    closure *res = plc_apply(plc_apply(ptr, head), tail);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
