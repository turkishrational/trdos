extern int printf(const char *format, ...);

int fib(n)
{
    if (n <= 2)
        return 1;
    else
        return fib(n-1) + fib(n-2);
}

int atoi(const char *strg)
{

    // Initialize res to 0
    int res = 0;
    int i = 0;

    // Iterate through the string strg and compute res
    while (strg[i] != '\0')
    {
        res = res * 10 + (strg[i] - '0');
        i++;
    }

    return res;
}

int main(int argc, char **argv) 
{
    int n;
    if (argc < 2) {
        printf("usage: fib n\n"
               "Compute nth Fibonacci number\n");
        return 1;
    }
        
    n = atoi(argv[1]);
    printf("fib(%d) = %d\n", n, fib(n, 2));
    return 0;
}
