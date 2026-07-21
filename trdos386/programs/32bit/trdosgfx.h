/* 22/07/2026 */
/* 21/07/2026 */
/* 20/07/2026 - Google AI - ref: aclock5.s - Erdogan Tan */
/* =========================================================================
   trdos_gfx.h - TRDOS 386 Grafik ve Sistem Başlık Dosyası
   ========================================================================= */
#ifndef TRDOS_GFX_H
#define TRDOS_GFX_H

typedef struct {
    unsigned char hour;
    unsigned char minute;
    unsigned char second;
} TRDOSTime;

/* Donanım ve Çizim Değişkenleri */
extern unsigned int _x0, _y0, radius;
extern unsigned char color;
extern unsigned int angle;
extern unsigned int circlebuffer[40000];
extern unsigned int *pixelpos;

/* Sistem Fonksiyonları */
void trdos_print_msg(const char *msg);
int trdos_set_video_mode(unsigned int mode_func, unsigned int mode_val);
void trdos_get_time(TRDOSTime *t);
unsigned int trdos_get_ticks(void);
int trdos_getchar_nowait(void);
void trdos_beep(unsigned int duration, unsigned int freq);
void trdos_write_buffer(unsigned int count, unsigned char color_val);
/* 22/07/2026 */
void trdos_sysexit(void);

/* Grafik ve Matematik */
unsigned int getsinus(unsigned int deg);
unsigned int getcosinus(unsigned int deg);
unsigned int getxy(unsigned int trig_value);
void draw_line(unsigned int target_angle);
void draw_centermark(void);
void draw_background(void);
void draw_circle(void);
unsigned int get_squareroot(unsigned int square_target);

#endif
