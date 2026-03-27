#include <stdio.h>

extern int scheme_entry();

#define bool_f     0x2F
#define bool_t     0x6F
#define fix_mask   0x03
#define fix_tag    0x00
#define fix_shift  2
#define null       0x3f
#define char_tag   0x0f
#define char_mask  0xff
#define char_shift 8

typedef unsigned int ptr;

static void print_ptr(ptr x)
{
	if ((x & fix_mask) == fix_tag)
		printf("%d", ((int)x) >> fix_shift);
	else if (x == bool_f)
		printf("#f");
	else if (x == bool_t)
		printf("#t");
	else if (x == null)
		printf("()");
	else if ((x & char_mask) == char_tag) {
		int ch = (int)x >> char_shift;
		switch (ch) {
		case '\t': printf("#\\tab");     break;
		case '\n': printf("#\\newline"); break;
		case '\r': printf("#\\return");  break;
		case ' ':  printf("#\\space");   break;
		default:
			   printf("#\\%c", (int)x >> char_shift);
		}
	} else
		printf("#<unknown 0x%08x>", x);
	printf("\n");
}

int main(int argc, char *argv[])
{
	print_ptr(scheme_entry());
	return 0;
}
