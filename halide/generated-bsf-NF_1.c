#include <iostream>
#include <math.h>
#include <float.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>

extern "C" void *halide_malloc(void *ctx, size_t);
extern "C" void halide_free(void *ctx, void *ptr);
extern "C" int halide_debug_to_file(void *ctx, const char *filename, void *data, int, int, int, int, int, int);
extern "C" int halide_start_clock(void *ctx);
extern "C" int64_t halide_current_time_ns(void *ctx);
extern "C" uint64_t halide_profiling_timer(void *ctx);
extern "C" int halide_printf(void *ctx, const char *fmt, ...);

#ifdef _WIN32
extern "C" float roundf(float);
extern "C" double round(double);
#else
inline float asinh_f32(float x) {return asinhf(x);}
inline float acosh_f32(float x) {return acoshf(x);}
inline float atanh_f32(float x) {return atanhf(x);}
inline double asinh_f64(double x) {return asinh(x);}
inline double acosh_f64(double x) {return acosh(x);}
inline double atanh_f64(double x) {return atanh(x);}
#endif
inline float sqrt_f32(float x) {return sqrtf(x);}
inline float sin_f32(float x) {return sinf(x);}
inline float asin_f32(float x) {return asinf(x);}
inline float cos_f32(float x) {return cosf(x);}
inline float acos_f32(float x) {return acosf(x);}
inline float tan_f32(float x) {return tanf(x);}
inline float atan_f32(float x) {return atanf(x);}
inline float sinh_f32(float x) {return sinhf(x);}
inline float cosh_f32(float x) {return coshf(x);}
inline float tanh_f32(float x) {return tanhf(x);}
inline float hypot_f32(float x, float y) {return hypotf(x, y);}
inline float exp_f32(float x) {return expf(x);}
inline float log_f32(float x) {return logf(x);}
inline float pow_f32(float x, float y) {return powf(x, y);}
inline float floor_f32(float x) {return floorf(x);}
inline float ceil_f32(float x) {return ceilf(x);}
inline float round_f32(float x) {return roundf(x);}

inline double sqrt_f64(double x) {return sqrt(x);}
inline double sin_f64(double x) {return sin(x);}
inline double asin_f64(double x) {return asin(x);}
inline double cos_f64(double x) {return cos(x);}
inline double acos_f64(double x) {return acos(x);}
inline double tan_f64(double x) {return tan(x);}
inline double atan_f64(double x) {return atan(x);}
inline double sinh_f64(double x) {return sinh(x);}
inline double cosh_f64(double x) {return cosh(x);}
inline double tanh_f64(double x) {return tanh(x);}
inline double hypot_f64(double x, double y) {return hypot(x, y);}
inline double exp_f64(double x) {return exp(x);}
inline double log_f64(double x) {return log(x);}
inline double pow_f64(double x, double y) {return pow(x, y);}
inline double floor_f64(double x) {return floor(x);}
inline double ceil_f64(double x) {return ceil(x);}
inline double round_f64(double x) {return round(x);}

inline float maxval_f32() {return FLT_MAX;}
inline float minval_f32() {return -FLT_MAX;}
inline double maxval_f64() {return DBL_MAX;}
inline double minval_f64() {return -DBL_MAX;}
inline uint8_t maxval_u8() {return 0xff;}
inline uint8_t minval_u8() {return 0;}
inline uint16_t maxval_u16() {return 0xffff;}
inline uint16_t minval_u16() {return 0;}
inline uint32_t maxval_u32() {return 0xffffffff;}
inline uint32_t minval_u32() {return 0;}
inline uint64_t maxval_u64() {return 0xffffffffffffffff;}
inline uint64_t minval_u64() {return 0;}
inline int8_t maxval_s8() {return 0x7f;}
inline int8_t minval_s8() {return 0x80;}
inline int16_t maxval_s16() {return 0x7fff;}
inline int16_t minval_s16() {return 0x8000;}
inline int32_t maxval_s32() {return 0x7fffffff;}
inline int32_t minval_s32() {return 0x80000000;}
inline int64_t maxval_s64() {return 0x7fffffffffffffff;}
inline int64_t minval_s64() {return 0x8000000000000000;}

inline int8_t abs_i8(int8_t a) {return a >= 0 ? a : -a;}
inline int16_t abs_i16(int16_t a) {return a >= 0 ? a : -a;}
inline int32_t abs_i32(int32_t a) {return a >= 0 ? a : -a;}
inline int64_t abs_i64(int64_t a) {return a >= 0 ? a : -a;}
inline float abs_f32(float a) {return fabsf(a);}
inline double abs_f64(double a) {return fabs(a);}

inline float nan_f32() {return NAN;}
inline float neg_inf_f32() {return -INFINITY;}
inline float inf_f32() {return INFINITY;}
inline float float_from_bits(uint32_t bits) {
 union {
  uint32_t as_uint;
  float as_float;
 } u;
 u.as_uint = bits;
 return u.as_float;
}

template<typename T> T max(T a, T b) {if (a > b) return a; return b;}
template<typename T> T min(T a, T b) {if (a < b) return a; return b;}
template<typename T> T mod(T a, T b) {T result = a % b; if (result < 0) result += b; return result;}
template<typename T> T sdiv(T a, T b) {return (a - mod(a, b))/b;}
template<typename A, typename B> A reinterpret(B b) {A a; memcpy(&a, &b, sizeof(a)); return a;}

#ifndef BUFFER_T_DEFINED
#define BUFFER_T_DEFINED
#include <stdint.h>
typedef struct buffer_t {
    uint64_t dev;
    uint8_t* host;
    int32_t extent[4];
    int32_t stride[4];
    int32_t min[4];
    int32_t elem_size;
    bool host_dirty;
    bool dev_dirty;
} buffer_t;
#endif
static bool halide_rewrite_buffer(buffer_t *b, int32_t elem_size,
                           int32_t min0, int32_t extent0, int32_t stride0,
                           int32_t min1, int32_t extent1, int32_t stride1,
                           int32_t min2, int32_t extent2, int32_t stride2,
                           int32_t min3, int32_t extent3, int32_t stride3) {
 b->min[0] = min0;
 b->min[1] = min1;
 b->min[2] = min2;
 b->min[3] = min3;
 b->extent[0] = extent0;
 b->extent[1] = extent1;
 b->extent[2] = extent2;
 b->extent[3] = extent3;
 b->stride[0] = stride0;
 b->stride[1] = stride1;
 b->stride[2] = stride2;
 b->stride[3] = stride3;
 return true;
}


extern "C" int main_compute(buffer_t *_inPar_buffer, buffer_t *_f1_buffer) {
int32_t *_inPar = (int32_t *)(_inPar_buffer->host);
const bool _inPar_host_and_dev_are_null = (_inPar_buffer->host == NULL) && (_inPar_buffer->dev == 0);
(void)_inPar_host_and_dev_are_null;
const int32_t _inPar_min_0 = _inPar_buffer->min[0];
(void)_inPar_min_0;
const int32_t _inPar_min_1 = _inPar_buffer->min[1];
(void)_inPar_min_1;
const int32_t _inPar_min_2 = _inPar_buffer->min[2];
(void)_inPar_min_2;
const int32_t _inPar_min_3 = _inPar_buffer->min[3];
(void)_inPar_min_3;
const int32_t _inPar_extent_0 = _inPar_buffer->extent[0];
(void)_inPar_extent_0;
const int32_t _inPar_extent_1 = _inPar_buffer->extent[1];
(void)_inPar_extent_1;
const int32_t _inPar_extent_2 = _inPar_buffer->extent[2];
(void)_inPar_extent_2;
const int32_t _inPar_extent_3 = _inPar_buffer->extent[3];
(void)_inPar_extent_3;
const int32_t _inPar_stride_0 = _inPar_buffer->stride[0];
(void)_inPar_stride_0;
const int32_t _inPar_stride_1 = _inPar_buffer->stride[1];
(void)_inPar_stride_1;
const int32_t _inPar_stride_2 = _inPar_buffer->stride[2];
(void)_inPar_stride_2;
const int32_t _inPar_stride_3 = _inPar_buffer->stride[3];
(void)_inPar_stride_3;
const int32_t _inPar_elem_size = _inPar_buffer->elem_size;
float *_f1 = (float *)(_f1_buffer->host);
const bool _f1_host_and_dev_are_null = (_f1_buffer->host == NULL) && (_f1_buffer->dev == 0);
(void)_f1_host_and_dev_are_null;
const int32_t _f1_min_0 = _f1_buffer->min[0];
(void)_f1_min_0;
const int32_t _f1_min_1 = _f1_buffer->min[1];
(void)_f1_min_1;
const int32_t _f1_min_2 = _f1_buffer->min[2];
(void)_f1_min_2;
const int32_t _f1_min_3 = _f1_buffer->min[3];
(void)_f1_min_3;
const int32_t _f1_extent_0 = _f1_buffer->extent[0];
(void)_f1_extent_0;
const int32_t _f1_extent_1 = _f1_buffer->extent[1];
(void)_f1_extent_1;
const int32_t _f1_extent_2 = _f1_buffer->extent[2];
(void)_f1_extent_2;
const int32_t _f1_extent_3 = _f1_buffer->extent[3];
(void)_f1_extent_3;
const int32_t _f1_stride_0 = _f1_buffer->stride[0];
(void)_f1_stride_0;
const int32_t _f1_stride_1 = _f1_buffer->stride[1];
(void)_f1_stride_1;
const int32_t _f1_stride_2 = _f1_buffer->stride[2];
(void)_f1_stride_2;
const int32_t _f1_stride_3 = _f1_buffer->stride[3];
(void)_f1_stride_3;
const int32_t _f1_elem_size = _f1_buffer->elem_size;
int32_t _0 = _f1_min_1 + _f1_extent_1;
int32_t _1 = _0 + -1;
int32_t _2 = min(_0, 8191);
int32_t _3 = max(_2, 0);
int32_t _4 = max(_1, _3);
int32_t _5 = _0 + -2;
int32_t _6 = min(_5, 8191);
int32_t _7 = max(_6, 0);
int32_t _8 = max(_4, _7);
int32_t _9 = _f1_min_1 + 1;
int32_t _10 = min(_9, 8191);
int32_t _11 = max(_10, 0);
int32_t _12 = min(_f1_min_1, _11);
int32_t _13 = _f1_min_1 + -1;
int32_t _14 = min(_13, 8191);
int32_t _15 = max(_14, 0);
int32_t _16 = min(_12, _15);
int32_t _17 = _f1_extent_0 + -1;
int32_t _18 = _17 >> 9;
int32_t _19 = _18 * 512;
int32_t _20 = _19 + _f1_min_0;
int32_t _21 = _20 + 512;
int32_t _22 = _f1_min_0 + _f1_extent_0;
int32_t _23 = min(_21, _22);
int32_t _24 = _22 + -512;
int32_t _25 = min(_f1_min_0, _24);
int32_t _26 = _23 - _25;
int32_t _27 = _f1_extent_0 + 511;
int32_t _28 = _27 >> 9;
int32_t _29 = _f1_extent_1 + 511;
int32_t _30 = _29 >> 9;
int32_t _31 = _28 * _30;
int32_t _32 = _31 + -1;
int32_t _33 = sdiv(_32, _28);
int32_t _34 = max(_33, 0);
int32_t _35 = _34 * 512;
int32_t _36 = _35 + _f1_min_1;
int32_t _37 = _36 + 512;
int32_t _38 = min(_37, _0);
int32_t _39 = min(_33, 0);
int32_t _40 = _39 * 512;
int32_t _41 = _40 + _f1_min_1;
int32_t _42 = _0 + -512;
int32_t _43 = min(_41, _42);
int32_t _44 = _38 - _43;
int32_t _45 = _17 >> 2;
int32_t _46 = _45 * 4;
int32_t _47 = _46 + _f1_min_0;
int32_t _48 = _22 + -4;
int32_t _49 = min(_47, _48);
int32_t _50 = _49 + 3;
int32_t _51 = _47 + 4;
int32_t _52 = min(_51, _22);
int32_t _53 = min(_52, 8191);
int32_t _54 = max(_53, 0);
int32_t _55 = max(_50, _54);
int32_t _56 = _49 + 2;
int32_t _57 = min(_56, 8191);
int32_t _58 = max(_57, 0);
int32_t _59 = max(_55, _58);
int32_t _60 = min(_f1_min_0, _48);
int32_t _61 = _60 + 1;
int32_t _62 = min(_61, 8191);
int32_t _63 = max(_62, 0);
int32_t _64 = min(_60, _63);
int32_t _65 = _60 + -1;
int32_t _66 = min(_65, 8191);
int32_t _67 = max(_66, 0);
int32_t _68 = min(_64, _67);
int32_t _69 = _59 - _68;
if (_f1_host_and_dev_are_null)
{
 bool _70 = halide_rewrite_buffer(_f1_buffer, 4, _25, _26, 1, _43, _44, _26, 0, 0, 0, 0, 0, 0);
 (void)_70;
} // if _f1_host_and_dev_are_null
if (_inPar_host_and_dev_are_null)
{
 int32_t _71 = _69 + 1;
 int32_t _72 = _8 - _16;
 int32_t _73 = _72 + 1;
 bool _74 = halide_rewrite_buffer(_inPar_buffer, 4, _68, _71, 1, _16, _73, _71, 0, 0, 0, 0, 0, 0);
 (void)_74;
} // if _inPar_host_and_dev_are_null
bool _75 = _f1_host_and_dev_are_null || _inPar_host_and_dev_are_null;
bool _76 = !(_75);
if (_76)
{
 bool _77 = _f1_elem_size == 4;
 if (!_77) {
  halide_printf(NULL, "Output buffer f1 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _f1_elem_size);
  return -1;
 }
 bool _78 = _inPar_elem_size == 4;
 if (!_78) {
  halide_printf(NULL, "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _inPar_elem_size);
  return -1;
 }
 bool _79 = _f1_min_0 <= _25;
 if (!_79) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is before the min (%d) in dimension 0\n", _25, _f1_min_0);
  return -1;
 }
 int32_t _80 = _25 + _26;
 int32_t _81 = _80 - _f1_extent_0;
 bool _82 = _81 <= _f1_min_0;
 int32_t _83 = _80 + -1;
 int32_t _84 = _f1_min_0 + _f1_extent_0;
 int32_t _85 = _84 + -1;
 if (!_82) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is beyond the max (%d) in dimension 0\n", _83, _85);
  return -1;
 }
 bool _86 = _f1_min_1 <= _43;
 if (!_86) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is before the min (%d) in dimension 1\n", _43, _f1_min_1);
  return -1;
 }
 int32_t _87 = _43 + _44;
 int32_t _88 = _87 - _f1_extent_1;
 bool _89 = _88 <= _f1_min_1;
 int32_t _90 = _87 + -1;
 int32_t _91 = _f1_min_1 + _f1_extent_1;
 int32_t _92 = _91 + -1;
 if (!_89) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is beyond the max (%d) in dimension 1\n", _90, _92);
  return -1;
 }
 bool _93 = _inPar_min_0 <= _68;
 if (!_93) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _68, _inPar_min_0);
  return -1;
 }
 int32_t _94 = _68 + _69;
 int32_t _95 = _94 - _inPar_extent_0;
 int32_t _96 = _95 + 1;
 bool _97 = _96 <= _inPar_min_0;
 int32_t _98 = _inPar_min_0 + _inPar_extent_0;
 int32_t _99 = _98 + -1;
 if (!_97) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n", _94, _99);
  return -1;
 }
 bool _100 = _inPar_min_1 <= _16;
 if (!_100) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _16, _inPar_min_1);
  return -1;
 }
 int32_t _101 = _8 - _inPar_extent_1;
 int32_t _102 = _101 + 1;
 bool _103 = _102 <= _inPar_min_1;
 int32_t _104 = _inPar_min_1 + _inPar_extent_1;
 int32_t _105 = _104 + -1;
 if (!_103) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n", _8, _105);
  return -1;
 }
 bool _106 = _f1_stride_0 == 1;
 if (!_106) {
  halide_printf(NULL, "Static constraint violated: f1.stride.0 == 1\n");
  return -1;
 }
 bool _107 = _inPar_stride_0 == 1;
 if (!_107) {
  halide_printf(NULL, "Static constraint violated: inPar.stride.0 == 1\n");
  return -1;
 }
 int64_t _108 = (int64_t)(_f1_extent_0);
 int64_t _109 = (int64_t)(_f1_extent_1);
 int64_t _110 = (int64_t)(_inPar_extent_0);
 int64_t _111 = (int64_t)(_inPar_extent_1);
 int64_t _112 = (int64_t)(2147483647);
 bool _113 = _108 <= _112;
 if (!_113) {
  halide_printf(NULL, "Total allocation for buffer f1 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _114 = (int64_t)(_f1_stride_1);
 int64_t _115 = _109 * _114;
 bool _116 = _115 <= _112;
 if (!_116) {
  halide_printf(NULL, "Total allocation for buffer f1 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _117 = _109 * _108;
 bool _118 = _117 <= _112;
 if (!_118) {
  halide_printf(NULL, "Product of extents for buffer f1 exceeds 2^31 - 1\n");
  return -1;
 }
 bool _119 = _110 <= _112;
 if (!_119) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _120 = (int64_t)(_inPar_stride_1);
 int64_t _121 = _111 * _120;
 bool _122 = _121 <= _112;
 if (!_122) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _123 = _111 * _110;
 bool _124 = _123 <= _112;
 if (!_124) {
  halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 // produce f1
 int32_t _125 = _f1_extent_0 + 511;
 int32_t _126 = _125 >> 9;
 int32_t _127 = _f1_extent_1 + 511;
 int32_t _128 = _127 >> 9;
 int32_t _129 = _126 * _128;
 #pragma omp parallel for
 for (int _f1_s0_x_xo_nid = 0; _f1_s0_x_xo_nid < 0 + _129; _f1_s0_x_xo_nid++)
 {
  int32_t _130 = _f1_extent_0 + 511;
  int32_t _131 = _130 >> 9;
  int32_t _132 = mod(_f1_s0_x_xo_nid, _131);
  int32_t _133 = _132 * 512;
  int32_t _134 = _133 + _f1_min_0;
  int32_t _135 = _f1_min_0 + _f1_extent_0;
  int32_t _136 = _135 + -512;
  int32_t _137 = min(_134, _136);
  int32_t _138 = sdiv(_f1_s0_x_xo_nid, _131);
  int32_t _139 = _138 * 512;
  int32_t _140 = _139 + _f1_min_1;
  int32_t _141 = _f1_min_1 + _f1_extent_1;
  int32_t _142 = _141 + -512;
  int32_t _143 = min(_140, _142);
  for (int _f1_s0_y_yi_yi = 0; _f1_s0_y_yi_yi < 0 + 16; _f1_s0_y_yi_yi++)
  {
   int32_t _144 = _f1_s0_y_yi_yi * 32;
   int32_t _145 = _143 + _144;
   for (int _f1_s0_x_xi_xi = 0; _f1_s0_x_xi_xi < 0 + 16; _f1_s0_x_xi_xi++)
   {
    int32_t _146 = _f1_s0_x_xi_xi * 32;
    int32_t _147 = _137 + _146;
    int32_t _148 = _145 + 1;
    int32_t _149 = min(_148, 8191);
    int32_t _150 = max(_149, 0);
    int32_t _151 = _145 + -1;
    int32_t _152 = min(_151, 8191);
    int32_t _153 = max(_152, 0);
    int32_t _154 = min(_145, _150);
    int32_t _155 = min(_154, _153);
    int32_t _156 = min(_145, 8191);
    int32_t _157 = max(_156, 0);
    int32_t _158 = max(_151, _157);
    int32_t _159 = _145 + -2;
    int32_t _160 = min(_159, 8191);
    int32_t _161 = max(_160, 0);
    int32_t _162 = max(_158, _161);
    int32_t _163 = _162 + 1;
    int32_t _164 = min(_155, _163);
    int32_t _165 = min(_164, _145);
    int32_t _166 = _145 + 32;
    int32_t _167 = min(_166, 8191);
    int32_t _168 = max(_167, 0);
    int32_t _169 = _145 + 30;
    int32_t _170 = min(_169, 8191);
    int32_t _171 = max(_170, 0);
    int32_t _172 = _145 + 31;
    int32_t _173 = max(_172, _168);
    int32_t _174 = max(_173, _171);
    int32_t _175 = max(_174, _172);
    int32_t _176 = _175 - _165;
    {
     int64_t _177 = 32;
     int32_t _178 = _176 + 1;
     int64_t _179 = _177 * _178;
     if ((_179 > ((int64_t(1) << 31) - 1)) || ((_179 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
     {
      halide_printf(NULL, "32-bit signed overflow computing size of allocation f0\n");
     } // overflow test f0
     int64_t _180 = _179;
     float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_180);
     for (int _f1_s0_y_yi_yii = 0; _f1_s0_y_yi_yii < 0 + 32; _f1_s0_y_yi_yii++)
     {
      int32_t _181 = _f1_s0_y_yi_yi * 32;
      int32_t _182 = _181 + _f1_s0_y_yi_yii;
      int32_t _183 = _143 + _182;
      for (int _f1_s0_x_xi_xii_xii = 0; _f1_s0_x_xi_xii_xii < 0 + 8; _f1_s0_x_xi_xii_xii++)
      {
       int32_t _184 = _f1_s0_x_xi_xi * 32;
       int32_t _185 = _f1_s0_x_xi_xii_xii * 4;
       int32_t _186 = _184 + _185;
       int32_t _187 = _137 + _186;
       int32_t _188 = _183 + 1;
       int32_t _189 = min(_188, 8191);
       int32_t _190 = max(_189, 0);
       int32_t _191 = max(_183, _190);
       int32_t _192 = _183 + -1;
       int32_t _193 = min(_192, 8191);
       int32_t _194 = max(_193, 0);
       int32_t _195 = max(_191, _194);
       int32_t _196 = min(_183, _190);
       int32_t _197 = min(_196, _194);
       int32_t _198 = min(_183, 8191);
       int32_t _199 = max(_198, 0);
       int32_t _200 = max(_192, _199);
       int32_t _201 = _183 + -2;
       int32_t _202 = min(_201, 8191);
       int32_t _203 = max(_202, 0);
       int32_t _204 = max(_200, _203);
       int32_t _205 = _204 + 1;
       bool _206 = _f1_s0_y_yi_yii == 0;
       int32_t _207 = (int32_t)(_206 ? _197 : _205);
       // produce f0
       int32_t _208 = _195 - _207;
       int32_t _209 = _208 + 1;
       for (int _f0_s0_y = _207; _f0_s0_y < _207 + _209; _f0_s0_y++)
       {
        for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + 1; _f0_s0_x_x++)
        {
         int32_t _210 = _f0_s0_x_x * 4;
         int32_t _211 = _210 + _187;
         int32_t _212 = min(_211, _187);
         int32_t _213 = _212 - _147;
         int32_t _214 = _f0_s0_y - _165;
         int32_t _215 = _214 * 32;
         int32_t _216 = _213 + _215;
         int32_t _217 = _f0_s0_y * _inPar_stride_1;
         int32_t _218 = _212 + _217;
         int32_t _219 = _inPar_min_1 * _inPar_stride_1;
         int32_t _220 = _inPar_min_0 + _219;
         int32_t _221 = _218 - _220;
         float _222 = ((float *)_inPar)[_221];
         float _223 = _222 * float_from_bits(1056964608 /* 0.5 */);
         int32_t _224 = _212 + 1;
         int32_t _225 = min(_224, 8191);
         int32_t _226 = max(_225, 0);
         int32_t _227 = _226 + _217;
         int32_t _228 = _227 - _220;
         float _229 = ((float *)_inPar)[_228];
         float _230 = _229 * float_from_bits(1048576000 /* 0.25 */);
         float _231 = _223 + _230;
         int32_t _232 = _212 + -1;
         int32_t _233 = min(_232, 8191);
         int32_t _234 = max(_233, 0);
         int32_t _235 = _234 + _217;
         int32_t _236 = _235 - _220;
         float _237 = ((float *)_inPar)[_236];
         float _238 = _237 * float_from_bits(1048576000 /* 0.25 */);
         float _239 = _231 + _238;
         _f0[_216] = _239;
         int32_t _240 = _212 - _147;
         int32_t _241 = _f0_s0_y - _165;
         int32_t _242 = _241 * 32;
         int32_t _243 = _240 + _242;
         int32_t _244 = _243 + 1;
         int32_t _245 = _f0_s0_y * _inPar_stride_1;
         int32_t _246 = _212 + _245;
         int32_t _247 = _inPar_min_1 * _inPar_stride_1;
         int32_t _248 = _inPar_min_0 + _247;
         int32_t _249 = _246 - _248;
         int32_t _250 = _249 + 1;
         float _251 = ((float *)_inPar)[_250];
         float _252 = _251 * float_from_bits(1056964608 /* 0.5 */);
         int32_t _253 = _212 + 2;
         int32_t _254 = min(_253, 8191);
         int32_t _255 = max(_254, 0);
         int32_t _256 = _255 + _245;
         int32_t _257 = _256 - _248;
         float _258 = ((float *)_inPar)[_257];
         float _259 = _258 * float_from_bits(1048576000 /* 0.25 */);
         float _260 = _252 + _259;
         int32_t _261 = min(_212, 8191);
         int32_t _262 = max(_261, 0);
         int32_t _263 = _262 + _245;
         int32_t _264 = _263 - _248;
         float _265 = ((float *)_inPar)[_264];
         float _266 = _265 * float_from_bits(1048576000 /* 0.25 */);
         float _267 = _260 + _266;
         _f0[_244] = _267;
         int32_t _268 = _212 - _147;
         int32_t _269 = _f0_s0_y - _165;
         int32_t _270 = _269 * 32;
         int32_t _271 = _268 + _270;
         int32_t _272 = _271 + 2;
         int32_t _273 = _f0_s0_y * _inPar_stride_1;
         int32_t _274 = _212 + _273;
         int32_t _275 = _inPar_min_1 * _inPar_stride_1;
         int32_t _276 = _inPar_min_0 + _275;
         int32_t _277 = _274 - _276;
         int32_t _278 = _277 + 2;
         float _279 = ((float *)_inPar)[_278];
         float _280 = _279 * float_from_bits(1056964608 /* 0.5 */);
         int32_t _281 = _212 + 3;
         int32_t _282 = min(_281, 8191);
         int32_t _283 = max(_282, 0);
         int32_t _284 = _283 + _273;
         int32_t _285 = _284 - _276;
         float _286 = ((float *)_inPar)[_285];
         float _287 = _286 * float_from_bits(1048576000 /* 0.25 */);
         float _288 = _280 + _287;
         int32_t _289 = _212 + 1;
         int32_t _290 = min(_289, 8191);
         int32_t _291 = max(_290, 0);
         int32_t _292 = _291 + _273;
         int32_t _293 = _292 - _276;
         float _294 = ((float *)_inPar)[_293];
         float _295 = _294 * float_from_bits(1048576000 /* 0.25 */);
         float _296 = _288 + _295;
         _f0[_272] = _296;
         int32_t _297 = _212 - _147;
         int32_t _298 = _f0_s0_y - _165;
         int32_t _299 = _298 * 32;
         int32_t _300 = _297 + _299;
         int32_t _301 = _300 + 3;
         int32_t _302 = _f0_s0_y * _inPar_stride_1;
         int32_t _303 = _212 + _302;
         int32_t _304 = _inPar_min_1 * _inPar_stride_1;
         int32_t _305 = _inPar_min_0 + _304;
         int32_t _306 = _303 - _305;
         int32_t _307 = _306 + 3;
         float _308 = ((float *)_inPar)[_307];
         float _309 = _308 * float_from_bits(1056964608 /* 0.5 */);
         int32_t _310 = _212 + 4;
         int32_t _311 = min(_310, 8191);
         int32_t _312 = max(_311, 0);
         int32_t _313 = _312 + _302;
         int32_t _314 = _313 - _305;
         float _315 = ((float *)_inPar)[_314];
         float _316 = _315 * float_from_bits(1048576000 /* 0.25 */);
         float _317 = _309 + _316;
         int32_t _318 = _212 + 2;
         int32_t _319 = min(_318, 8191);
         int32_t _320 = max(_319, 0);
         int32_t _321 = _320 + _302;
         int32_t _322 = _321 - _305;
         float _323 = ((float *)_inPar)[_322];
         float _324 = _323 * float_from_bits(1048576000 /* 0.25 */);
         float _325 = _317 + _324;
         _f0[_301] = _325;
        } // for _f0_s0_x_x
       } // for _f0_s0_y
       // consume f0
       int32_t _326 = _f1_s0_x_xi_xi * 32;
       int32_t _327 = _f1_s0_x_xi_xii_xii * 4;
       int32_t _328 = _326 + _327;
       int32_t _329 = _137 + _328;
       int32_t _330 = _329 - _f1_min_0;
       int32_t _331 = _183 - _f1_min_1;
       int32_t _332 = _331 * _f1_stride_1;
       int32_t _333 = _330 + _332;
       int32_t _334 = _329 - _147;
       int32_t _335 = _183 - _165;
       int32_t _336 = _335 * 32;
       int32_t _337 = _334 + _336;
       float _338 = _f0[_337];
       float _339 = _338 * float_from_bits(1056964608 /* 0.5 */);
       int32_t _340 = _183 + 1;
       int32_t _341 = min(_340, 8191);
       int32_t _342 = max(_341, 0);
       int32_t _343 = _342 - _165;
       int32_t _344 = _343 * 32;
       int32_t _345 = _334 + _344;
       float _346 = _f0[_345];
       float _347 = _346 * float_from_bits(1048576000 /* 0.25 */);
       float _348 = _339 + _347;
       int32_t _349 = _183 + -1;
       int32_t _350 = min(_349, 8191);
       int32_t _351 = max(_350, 0);
       int32_t _352 = _351 - _165;
       int32_t _353 = _352 * 32;
       int32_t _354 = _334 + _353;
       float _355 = _f0[_354];
       float _356 = _355 * float_from_bits(1048576000 /* 0.25 */);
       float _357 = _348 + _356;
       _f1[_333] = _357;
       int32_t _358 = _f1_s0_x_xi_xi * 32;
       int32_t _359 = _f1_s0_x_xi_xii_xii * 4;
       int32_t _360 = _358 + _359;
       int32_t _361 = _137 + _360;
       int32_t _362 = _361 - _f1_min_0;
       int32_t _363 = _183 - _f1_min_1;
       int32_t _364 = _363 * _f1_stride_1;
       int32_t _365 = _362 + _364;
       int32_t _366 = _365 + 1;
       int32_t _367 = _361 - _147;
       int32_t _368 = _183 - _165;
       int32_t _369 = _368 * 32;
       int32_t _370 = _367 + _369;
       int32_t _371 = _370 + 1;
       float _372 = _f0[_371];
       float _373 = _372 * float_from_bits(1056964608 /* 0.5 */);
       int32_t _374 = _183 + 1;
       int32_t _375 = min(_374, 8191);
       int32_t _376 = max(_375, 0);
       int32_t _377 = _376 - _165;
       int32_t _378 = _377 * 32;
       int32_t _379 = _367 + _378;
       int32_t _380 = _379 + 1;
       float _381 = _f0[_380];
       float _382 = _381 * float_from_bits(1048576000 /* 0.25 */);
       float _383 = _373 + _382;
       int32_t _384 = _183 + -1;
       int32_t _385 = min(_384, 8191);
       int32_t _386 = max(_385, 0);
       int32_t _387 = _386 - _165;
       int32_t _388 = _387 * 32;
       int32_t _389 = _367 + _388;
       int32_t _390 = _389 + 1;
       float _391 = _f0[_390];
       float _392 = _391 * float_from_bits(1048576000 /* 0.25 */);
       float _393 = _383 + _392;
       _f1[_366] = _393;
       int32_t _394 = _f1_s0_x_xi_xi * 32;
       int32_t _395 = _f1_s0_x_xi_xii_xii * 4;
       int32_t _396 = _394 + _395;
       int32_t _397 = _137 + _396;
       int32_t _398 = _397 - _f1_min_0;
       int32_t _399 = _183 - _f1_min_1;
       int32_t _400 = _399 * _f1_stride_1;
       int32_t _401 = _398 + _400;
       int32_t _402 = _401 + 2;
       int32_t _403 = _397 - _147;
       int32_t _404 = _183 - _165;
       int32_t _405 = _404 * 32;
       int32_t _406 = _403 + _405;
       int32_t _407 = _406 + 2;
       float _408 = _f0[_407];
       float _409 = _408 * float_from_bits(1056964608 /* 0.5 */);
       int32_t _410 = _183 + 1;
       int32_t _411 = min(_410, 8191);
       int32_t _412 = max(_411, 0);
       int32_t _413 = _412 - _165;
       int32_t _414 = _413 * 32;
       int32_t _415 = _403 + _414;
       int32_t _416 = _415 + 2;
       float _417 = _f0[_416];
       float _418 = _417 * float_from_bits(1048576000 /* 0.25 */);
       float _419 = _409 + _418;
       int32_t _420 = _183 + -1;
       int32_t _421 = min(_420, 8191);
       int32_t _422 = max(_421, 0);
       int32_t _423 = _422 - _165;
       int32_t _424 = _423 * 32;
       int32_t _425 = _403 + _424;
       int32_t _426 = _425 + 2;
       float _427 = _f0[_426];
       float _428 = _427 * float_from_bits(1048576000 /* 0.25 */);
       float _429 = _419 + _428;
       _f1[_402] = _429;
       int32_t _430 = _f1_s0_x_xi_xi * 32;
       int32_t _431 = _f1_s0_x_xi_xii_xii * 4;
       int32_t _432 = _430 + _431;
       int32_t _433 = _137 + _432;
       int32_t _434 = _433 - _f1_min_0;
       int32_t _435 = _183 - _f1_min_1;
       int32_t _436 = _435 * _f1_stride_1;
       int32_t _437 = _434 + _436;
       int32_t _438 = _437 + 3;
       int32_t _439 = _433 - _147;
       int32_t _440 = _183 - _165;
       int32_t _441 = _440 * 32;
       int32_t _442 = _439 + _441;
       int32_t _443 = _442 + 3;
       float _444 = _f0[_443];
       float _445 = _444 * float_from_bits(1056964608 /* 0.5 */);
       int32_t _446 = _183 + 1;
       int32_t _447 = min(_446, 8191);
       int32_t _448 = max(_447, 0);
       int32_t _449 = _448 - _165;
       int32_t _450 = _449 * 32;
       int32_t _451 = _439 + _450;
       int32_t _452 = _451 + 3;
       float _453 = _f0[_452];
       float _454 = _453 * float_from_bits(1048576000 /* 0.25 */);
       float _455 = _445 + _454;
       int32_t _456 = _183 + -1;
       int32_t _457 = min(_456, 8191);
       int32_t _458 = max(_457, 0);
       int32_t _459 = _458 - _165;
       int32_t _460 = _459 * 32;
       int32_t _461 = _439 + _460;
       int32_t _462 = _461 + 3;
       float _463 = _f0[_462];
       float _464 = _463 * float_from_bits(1048576000 /* 0.25 */);
       float _465 = _455 + _464;
       _f1[_438] = _465;
      } // for _f1_s0_x_xi_xii_xii
     } // for _f1_s0_y_yi_yii
     halide_free(NULL, _f0);
    } // alloc _f0
   } // for _f1_s0_x_xi_xi
  } // for _f1_s0_y_yi_yi
 } // for _f1_s0_x_xo_nid
 // consume f1
} // if _76
return 0;
}
