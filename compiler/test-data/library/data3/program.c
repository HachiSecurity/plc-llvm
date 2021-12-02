#include "data3.h"

int main() {
    closure *val = plc_new_integer("108");
    closure *arg = plc_new_data_integer(val);
    closure *ptr = plc_entry();
    closure *res = plc_apply(ptr, arg);
    plc_print_closure(res);
    putchar('\n');
    return 0;
}
