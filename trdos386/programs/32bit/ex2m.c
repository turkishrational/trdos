extern int printf(const char *format, ...);

#define N 20

// Tüm sistem durumunu yığın üzerinden taşımak için ortak bir veri yapısı (Context)
struct Context {
    int nb_num;
    int tab[N];
    int stack_ptr;
    int stack_op[N];
    int stack_res[60];
    int result;
};

int atoi(const char *strg)
{
    int res = 0;
    int i = 0;
    while (strg[i] != '\0')
    {
        res = res * 10 + (strg[i] - '0');
        i++;
    }
    return res;
}

int find(struct Context *ctx, int n, int i1, int a, int b, int op)
{
    int i, j, c;

    if (ctx->stack_ptr >= 0) {
        ctx->stack_res[3 * ctx->stack_ptr] = a;
        ctx->stack_op[ctx->stack_ptr] = op;
        ctx->stack_res[3 * ctx->stack_ptr + 1] = b;
        ctx->stack_res[3 * ctx->stack_ptr + 2] = n;
        if (n == ctx->result)
            return 1;
        ctx->tab[i1] = n;
    }
    
    for(i = 0; i < ctx->nb_num; i++) {
        for(j = i + 1; j < ctx->nb_num; j++) {
            a = ctx->tab[i];
            b = ctx->tab[j];
            if (a != 0 && b != 0) {
                ctx->tab[j] = 0;
                ctx->stack_ptr++;

                if (find(ctx, a + b, i, a, b, '+')) return 1;
                if (find(ctx, a - b, i, a, b, '-')) return 1;
                if (find(ctx, b - a, i, b, a, '-')) return 1;
                if (find(ctx, a * b, i, a, b, '*')) return 1;
                
                if (b != 0) {
                    c = a / b;
                    if (find(ctx, c, i, a, b, '/')) return 1;
                }
                if (a != 0) {
                    c = b / a;
                    if (find(ctx, c, i, b, a, '/')) return 1;
                }
                
                ctx->stack_ptr--;
                ctx->tab[i] = a;
                ctx->tab[j] = b;
            }
        }
    }
    return 0;
}

int main(int argc, char **argv)
{
    int i, res, p;
    struct Context ctx; // Tüm küresel değişkenler artık Ring 3 Stack alanında!
    
    if (argc < 3) {
        printf("usage: %s: result numbers...\n"
               "Try to find result from numbers with the 4 basic operations.\n", argv[0]);
        return 1;
    }

    p = 1;
    ctx.result = atoi(argv[p]);
    printf("result=%d\n", ctx.result);
    
    ctx.nb_num = 0;
    for(i = p + 1; i < argc; i++) {
        ctx.tab[ctx.nb_num++] = atoi(argv[i]);
    }

    ctx.stack_ptr = -1;
    res = find(&ctx, 0, 0, 0, 0, ' ');
    
    if (res) {
        for(i = 0; i <= ctx.stack_ptr; i++) {
            printf("%d %c %d = %d\n", 
                   ctx.stack_res[3 * i], ctx.stack_op[i],
                   ctx.stack_res[3 * i + 1], ctx.stack_res[3 * i + 2]);
        }
        return 0;
    } else {
        printf("Impossible\n");
        return 1;
    }
}
