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
int32_t _0 = _f1_extent_0 + -1;
int32_t _1 = _0 >> 9;
int32_t _2 = _1 * 512;
int32_t _3 = _2 + _f1_min_0;
int32_t _4 = _3 + 512;
int32_t _5 = _f1_min_0 + _f1_extent_0;
int32_t _6 = min(_4, _5);
int32_t _7 = _5 + -512;
int32_t _8 = min(_f1_min_0, _7);
int32_t _9 = _6 - _8;
int32_t _10 = _f1_extent_0 + 511;
int32_t _11 = _10 >> 9;
int32_t _12 = _f1_extent_1 + 511;
int32_t _13 = _12 >> 9;
int32_t _14 = _11 * _13;
int32_t _15 = _14 + -1;
int32_t _16 = sdiv(_15, _11);
int32_t _17 = max(_16, 0);
int32_t _18 = _17 * 512;
int32_t _19 = _18 + _f1_min_1;
int32_t _20 = _19 + 512;
int32_t _21 = _f1_min_1 + _f1_extent_1;
int32_t _22 = min(_20, _21);
int32_t _23 = min(_16, 0);
int32_t _24 = _23 * 512;
int32_t _25 = _24 + _f1_min_1;
int32_t _26 = _21 + -512;
int32_t _27 = min(_25, _26);
int32_t _28 = _22 - _27;
int32_t _29 = min(_3, _7);
int32_t _30 = _29 + 511;
int32_t _31 = min(_6, 8191);
int32_t _32 = max(_31, 0);
int32_t _33 = max(_30, _32);
int32_t _34 = _29 + 510;
int32_t _35 = min(_34, 8191);
int32_t _36 = max(_35, 0);
int32_t _37 = max(_33, _36);
int32_t _38 = max(_37, _30);
int32_t _39 = _8 + 1;
int32_t _40 = min(_39, 8191);
int32_t _41 = max(_40, 0);
int32_t _42 = min(_8, _41);
int32_t _43 = _8 + -1;
int32_t _44 = min(_43, 8191);
int32_t _45 = max(_44, 0);
int32_t _46 = min(_42, _45);
int32_t _47 = min(_46, _8);
int32_t _48 = _38 - _47;
int32_t _49 = min(_19, _26);
int32_t _50 = _49 + 511;
int32_t _51 = min(_22, 8191);
int32_t _52 = max(_51, 0);
int32_t _53 = max(_50, _52);
int32_t _54 = _49 + 510;
int32_t _55 = min(_54, 8191);
int32_t _56 = max(_55, 0);
int32_t _57 = max(_53, _56);
int32_t _58 = _27 + 1;
int32_t _59 = min(_58, 8191);
int32_t _60 = max(_59, 0);
int32_t _61 = min(_27, _60);
int32_t _62 = _27 + -1;
int32_t _63 = min(_62, 8191);
int32_t _64 = max(_63, 0);
int32_t _65 = min(_61, _64);
int32_t _66 = _57 - _65;
if (_f1_host_and_dev_are_null)
{
 bool _67 = halide_rewrite_buffer(_f1_buffer, 4, _8, _9, 1, _27, _28, _9, 0, 0, 0, 0, 0, 0);
 (void)_67;
} // if _f1_host_and_dev_are_null
if (_inPar_host_and_dev_are_null)
{
 int32_t _68 = _48 + 1;
 int32_t _69 = _66 + 1;
 bool _70 = halide_rewrite_buffer(_inPar_buffer, 4, _47, _68, 1, _65, _69, _68, 0, 0, 0, 0, 0, 0);
 (void)_70;
} // if _inPar_host_and_dev_are_null
bool _71 = _f1_host_and_dev_are_null || _inPar_host_and_dev_are_null;
bool _72 = !(_71);
if (_72)
{
 bool _73 = _f1_elem_size == 4;
 if (!_73) {
  halide_printf(NULL, "Output buffer f1 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _f1_elem_size);
  return -1;
 }
 bool _74 = _inPar_elem_size == 4;
 if (!_74) {
  halide_printf(NULL, "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _inPar_elem_size);
  return -1;
 }
 bool _75 = _f1_min_0 <= _8;
 if (!_75) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is before the min (%d) in dimension 0\n", _8, _f1_min_0);
  return -1;
 }
 int32_t _76 = _8 + _9;
 int32_t _77 = _76 - _f1_extent_0;
 bool _78 = _77 <= _f1_min_0;
 int32_t _79 = _76 + -1;
 int32_t _80 = _f1_min_0 + _f1_extent_0;
 int32_t _81 = _80 + -1;
 if (!_78) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is beyond the max (%d) in dimension 0\n", _79, _81);
  return -1;
 }
 bool _82 = _f1_min_1 <= _27;
 if (!_82) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is before the min (%d) in dimension 1\n", _27, _f1_min_1);
  return -1;
 }
 int32_t _83 = _27 + _28;
 int32_t _84 = _83 - _f1_extent_1;
 bool _85 = _84 <= _f1_min_1;
 int32_t _86 = _83 + -1;
 int32_t _87 = _f1_min_1 + _f1_extent_1;
 int32_t _88 = _87 + -1;
 if (!_85) {
  halide_printf(NULL, "Output buffer f1 is accessed at %d, which is beyond the max (%d) in dimension 1\n", _86, _88);
  return -1;
 }
 bool _89 = _inPar_min_0 <= _47;
 if (!_89) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _47, _inPar_min_0);
  return -1;
 }
 int32_t _90 = _47 + _48;
 int32_t _91 = _90 - _inPar_extent_0;
 int32_t _92 = _91 + 1;
 bool _93 = _92 <= _inPar_min_0;
 int32_t _94 = _inPar_min_0 + _inPar_extent_0;
 int32_t _95 = _94 + -1;
 if (!_93) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n", _90, _95);
  return -1;
 }
 bool _96 = _inPar_min_1 <= _65;
 if (!_96) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _65, _inPar_min_1);
  return -1;
 }
 int32_t _97 = _65 + _66;
 int32_t _98 = _97 - _inPar_extent_1;
 int32_t _99 = _98 + 1;
 bool _100 = _99 <= _inPar_min_1;
 int32_t _101 = _inPar_min_1 + _inPar_extent_1;
 int32_t _102 = _101 + -1;
 if (!_100) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n", _97, _102);
  return -1;
 }
 bool _103 = _f1_stride_0 == 1;
 if (!_103) {
  halide_printf(NULL, "Static constraint violated: f1.stride.0 == 1\n");
  return -1;
 }
 bool _104 = _inPar_stride_0 == 1;
 if (!_104) {
  halide_printf(NULL, "Static constraint violated: inPar.stride.0 == 1\n");
  return -1;
 }
 int64_t _105 = (int64_t)(_f1_extent_0);
 int64_t _106 = (int64_t)(_f1_extent_1);
 int64_t _107 = (int64_t)(_inPar_extent_0);
 int64_t _108 = (int64_t)(_inPar_extent_1);
 int64_t _109 = (int64_t)(2147483647);
 bool _110 = _105 <= _109;
 if (!_110) {
  halide_printf(NULL, "Total allocation for buffer f1 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _111 = (int64_t)(_f1_stride_1);
 int64_t _112 = _106 * _111;
 bool _113 = _112 <= _109;
 if (!_113) {
  halide_printf(NULL, "Total allocation for buffer f1 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _114 = _106 * _105;
 bool _115 = _114 <= _109;
 if (!_115) {
  halide_printf(NULL, "Product of extents for buffer f1 exceeds 2^31 - 1\n");
  return -1;
 }
 bool _116 = _107 <= _109;
 if (!_116) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _117 = (int64_t)(_inPar_stride_1);
 int64_t _118 = _108 * _117;
 bool _119 = _118 <= _109;
 if (!_119) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _120 = _108 * _107;
 bool _121 = _120 <= _109;
 if (!_121) {
  halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 // produce f1
 int32_t _122 = _f1_extent_0 + 511;
 int32_t _123 = _122 >> 9;
 int32_t _124 = _f1_extent_1 + 511;
 int32_t _125 = _124 >> 9;
 int32_t _126 = _123 * _125;
 #pragma omp parallel for
 for (int _f1_s0_x_xo_nid = 0; _f1_s0_x_xo_nid < 0 + _126; _f1_s0_x_xo_nid++)
 {
  int32_t _127 = _f1_extent_0 + 511;
  int32_t _128 = _127 >> 9;
  int32_t _129 = mod(_f1_s0_x_xo_nid, _128);
  int32_t _130 = _129 * 512;
  int32_t _131 = _130 + _f1_min_0;
  int32_t _132 = _f1_min_0 + _f1_extent_0;
  int32_t _133 = _132 + -512;
  int32_t _134 = min(_131, _133);
  int32_t _135 = sdiv(_f1_s0_x_xo_nid, _128);
  int32_t _136 = _135 * 512;
  int32_t _137 = _136 + _f1_min_1;
  int32_t _138 = _f1_min_1 + _f1_extent_1;
  int32_t _139 = _138 + -512;
  int32_t _140 = min(_137, _139);
  for (int _f1_s0_y_yi_yi = 0; _f1_s0_y_yi_yi < 0 + 16; _f1_s0_y_yi_yi++)
  {
   for (int _f1_s0_x_xi_xi = 0; _f1_s0_x_xi_xi < 0 + 16; _f1_s0_x_xi_xi++)
   {
    for (int _f1_s0_y_yi_yii = 0; _f1_s0_y_yi_yii < 0 + 32; _f1_s0_y_yi_yii++)
    {
     for (int _f1_s0_x_xi_xii = 0; _f1_s0_x_xi_xii < 0 + 32; _f1_s0_x_xi_xii++)
     {
      int32_t _141 = _f1_s0_x_xi_xi * 32;
      int32_t _142 = _141 + _f1_s0_x_xi_xii;
      int32_t _143 = _134 + _142;
      int32_t _144 = _143 - _f1_min_0;
      int32_t _145 = _f1_s0_y_yi_yi * 32;
      int32_t _146 = _145 + _f1_s0_y_yi_yii;
      int32_t _147 = _140 + _146;
      int32_t _148 = _147 - _f1_min_1;
      int32_t _149 = _148 * _f1_stride_1;
      int32_t _150 = _144 + _149;
      int32_t _151 = _inPar_min_1 * _inPar_stride_1;
      int32_t _152 = _inPar_min_0 + _151;
      int32_t _153 = _143 + 1;
      int32_t _154 = min(_153, 8191);
      int32_t _155 = max(_154, 0);
      int32_t _156 = _147 + 1;
      int32_t _157 = min(_156, 8191);
      int32_t _158 = max(_157, 0);
      int32_t _159 = _143 + -1;
      int32_t _160 = min(_159, 8191);
      int32_t _161 = max(_160, 0);
      int32_t _162 = _147 + -1;
      int32_t _163 = min(_162, 8191);
      int32_t _164 = max(_163, 0);
      int32_t _165 = _147 * _inPar_stride_1;
      int32_t _166 = _143 + _165;
      int32_t _167 = _166 - _152;
      float _168 = ((float *)_inPar)[_167];
      float _169 = _168 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _170 = _155 + _165;
      int32_t _171 = _170 - _152;
      float _172 = ((float *)_inPar)[_171];
      float _173 = _172 * float_from_bits(1048576000 /* 0.25 */);
      float _174 = _169 + _173;
      int32_t _175 = _161 + _165;
      int32_t _176 = _175 - _152;
      float _177 = ((float *)_inPar)[_176];
      float _178 = _177 * float_from_bits(1048576000 /* 0.25 */);
      float _179 = _174 + _178;
      float _180 = _179 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _181 = _158 * _inPar_stride_1;
      int32_t _182 = _143 + _181;
      int32_t _183 = _182 - _152;
      float _184 = ((float *)_inPar)[_183];
      float _185 = _184 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _186 = _155 + _181;
      int32_t _187 = _186 - _152;
      float _188 = ((float *)_inPar)[_187];
      float _189 = _188 * float_from_bits(1048576000 /* 0.25 */);
      float _190 = _185 + _189;
      int32_t _191 = _161 + _181;
      int32_t _192 = _191 - _152;
      float _193 = ((float *)_inPar)[_192];
      float _194 = _193 * float_from_bits(1048576000 /* 0.25 */);
      float _195 = _190 + _194;
      float _196 = _195 * float_from_bits(1048576000 /* 0.25 */);
      float _197 = _180 + _196;
      int32_t _198 = _164 * _inPar_stride_1;
      int32_t _199 = _143 + _198;
      int32_t _200 = _199 - _152;
      float _201 = ((float *)_inPar)[_200];
      float _202 = _201 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _203 = _155 + _198;
      int32_t _204 = _203 - _152;
      float _205 = ((float *)_inPar)[_204];
      float _206 = _205 * float_from_bits(1048576000 /* 0.25 */);
      float _207 = _202 + _206;
      int32_t _208 = _161 + _198;
      int32_t _209 = _208 - _152;
      float _210 = ((float *)_inPar)[_209];
      float _211 = _210 * float_from_bits(1048576000 /* 0.25 */);
      float _212 = _207 + _211;
      float _213 = _212 * float_from_bits(1048576000 /* 0.25 */);
      float _214 = _197 + _213;
      _f1[_150] = _214;
     } // for _f1_s0_x_xi_xii
    } // for _f1_s0_y_yi_yii
   } // for _f1_s0_x_xi_xi
  } // for _f1_s0_y_yi_yi
 } // for _f1_s0_x_xo_nid
 // consume f1
} // if _72
return 0;
}
