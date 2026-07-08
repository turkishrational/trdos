/* TEST.C - Saf TRDOS Yerel I/O ve Printf Testi */
extern int printf(const char *format, ...);

int main(void) {
    printf("Merhaba TRDOS 386!\n");
    printf("TCC Port projesi basariyla ekrana yazdi: %d\n", 2026);
    return 30;
}