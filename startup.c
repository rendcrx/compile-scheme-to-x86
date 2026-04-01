#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/mman.h>

extern int scheme_entry(void *, char *, char *);

#define bool_f     0x2F
#define bool_t     0x6F
#define fix_mask   0x03
#define fix_tag    0x00
#define fix_shift  2
#define null       0x3f
#define char_tag   0x0f
#define char_mask  0xff
#define char_shift 8
#define pair_mask  0x07
#define pair_tag   0x01
#define pair_shift 0x03

typedef unsigned int ptr;

typedef struct {
	void *eax;
	void *ebx;
	void *ecx;
	void *edx;
	void *esi;
	void *edi;
	void *ebp;
	void *esp;
} context;

static void print_pair(ptr x, int ok);

static void _print_ptr(ptr x, int ok)
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
	} else if ((x & pair_mask) == pair_tag) {
		print_pair(x, ok);
	} else
		printf("#<unknown 0x%08x>", x);
}

static void print_pair(ptr x, int ok)
{
	int a, b;
	int *p = (int *)(((int)x) & ~pair_mask);
	a = *p;
	b = *(p+1);
	if (ok)
		printf("(");
	_print_ptr(a, 1);
	if (b != null) {
		if ((b & pair_mask) == pair_tag) {
			printf(" ");
			_print_ptr(b, 0);
		} else {
			printf(" . ");
			_print_ptr(b, 0);
		}
	}
	if (ok)
		printf(")");
}

static void print_ptr(ptr x)
{
	_print_ptr(x, 1);
	printf("\n");
}

static char *allocate_protected_space(int size)
{
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;
	char *p = mmap(0, aligned_size + 2 * page,
		       PROT_READ | PROT_WRITE,
		       MAP_ANONYMOUS | MAP_PRIVATE,
		       0, 0);
	assert(p != MAP_FAILED);
	status = mprotect(p, page, PROT_NONE);
	assert(status == 0);
	status = mprotect(p + page + aligned_size, page, PROT_NONE);
	assert(status == 0);
	return p + page;
}

static void deallocate_protected_space(char *p, int size)
{
	int page = getpagesize();
	int status;
	int aligned_size = ((size + page - 1) / page) * page;
	status = munmap(p - page, aligned_size + 2 * page);
	assert(status == 0);
}

int main(int argc, char *argv[])
{
	int stack_size = 16 * 4096;
	char *stack_top = allocate_protected_space(stack_size);
	char *stack_base = stack_top + stack_size;
	context ctx;
	char *heap_ptr = malloc(1024);
	char *heap = heap_ptr;
	if ((int)heap & 7)
		heap = (char *)(((int)heap + 7) & ~7);
	print_ptr(scheme_entry(&ctx, stack_base, heap));
	free(heap_ptr);
	deallocate_protected_space(stack_top, stack_size);
	return 0;
}
