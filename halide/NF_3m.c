#include <Halide.h>
#include <iostream>
#include <math.h>
#include <float.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

float tmp[100000000]={3.14};

void *halide_malloc(void *ctx, size_t s){printf("msz = %d\n", s) ; return (void*)tmp;}
void halide_free(void *ctx, void *ptr){return ;}
// extern "C" int halide_debug_to_file(void *ctx, const char *filename, void *data, int, int, int, int, int, int);
extern "C" int halide_start_clock(void *ctx);
extern "C" int64_t halide_current_time_ns(void *ctx);
extern "C" uint64_t halide_profiling_timer(void *ctx);
int halide_printf(void *ctx, const char *fmt, ...){return 0;}

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


extern "C" int main_compute(buffer_t *_inPar_buffer, buffer_t *_f5_buffer) {
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
float *_f5 = (float *)(_f5_buffer->host);
const bool _f5_host_and_dev_are_null = (_f5_buffer->host == NULL) && (_f5_buffer->dev == 0);
(void)_f5_host_and_dev_are_null;
const int32_t _f5_min_0 = _f5_buffer->min[0];
(void)_f5_min_0;
const int32_t _f5_min_1 = _f5_buffer->min[1];
(void)_f5_min_1;
const int32_t _f5_min_2 = _f5_buffer->min[2];
(void)_f5_min_2;
const int32_t _f5_min_3 = _f5_buffer->min[3];
(void)_f5_min_3;
const int32_t _f5_extent_0 = _f5_buffer->extent[0];
(void)_f5_extent_0;
const int32_t _f5_extent_1 = _f5_buffer->extent[1];
(void)_f5_extent_1;
const int32_t _f5_extent_2 = _f5_buffer->extent[2];
(void)_f5_extent_2;
const int32_t _f5_extent_3 = _f5_buffer->extent[3];
(void)_f5_extent_3;
const int32_t _f5_stride_0 = _f5_buffer->stride[0];
(void)_f5_stride_0;
const int32_t _f5_stride_1 = _f5_buffer->stride[1];
(void)_f5_stride_1;
const int32_t _f5_stride_2 = _f5_buffer->stride[2];
(void)_f5_stride_2;
const int32_t _f5_stride_3 = _f5_buffer->stride[3];
(void)_f5_stride_3;
const int32_t _f5_elem_size = _f5_buffer->elem_size;
int32_t _0 = _f5_min_1 + _f5_extent_1;
int32_t _1 = _0 + -1;
int32_t _2 = min(_0, 8191);
int32_t _3 = max(_2, 0);
int32_t _4 = max(_1, _3);
int32_t _5 = _0 + -2;
int32_t _6 = min(_5, 8191);
int32_t _7 = max(_6, 0);
int32_t _8 = max(_4, _7);
int32_t _9 = _f5_min_1 + 1;
int32_t _10 = min(_9, 8191);
int32_t _11 = max(_10, 0);
int32_t _12 = min(_f5_min_1, _11);
int32_t _13 = _f5_min_1 + -1;
int32_t _14 = min(_13, 8191);
int32_t _15 = max(_14, 0);
int32_t _16 = min(_12, _15);
int32_t _17 = _f5_min_0 + _f5_extent_0;
int32_t _18 = _17 + -1;
int32_t _19 = min(_17, 8191);
int32_t _20 = max(_19, 0);
int32_t _21 = max(_18, _20);
int32_t _22 = _17 + -2;
int32_t _23 = min(_22, 8191);
int32_t _24 = max(_23, 0);
int32_t _25 = max(_21, _24);
int32_t _26 = _f5_min_0 + 1;
int32_t _27 = min(_26, 8191);
int32_t _28 = max(_27, 0);
int32_t _29 = min(_f5_min_0, _28);
int32_t _30 = _f5_min_0 + -1;
int32_t _31 = min(_30, 8191);
int32_t _32 = max(_31, 0);
int32_t _33 = min(_29, _32);
int32_t _34 = _8 + 1;
int32_t _35 = min(_34, 8191);
int32_t _36 = max(_35, 0);
int32_t _37 = max(_8, _36);
int32_t _38 = _8 + -1;
int32_t _39 = min(_38, 8191);
int32_t _40 = max(_39, 0);
int32_t _41 = max(_37, _40);
int32_t _42 = _16 + 1;
int32_t _43 = min(_42, 8191);
int32_t _44 = max(_43, 0);
int32_t _45 = min(_16, _44);
int32_t _46 = _16 + -1;
int32_t _47 = min(_46, 8191);
int32_t _48 = max(_47, 0);
int32_t _49 = min(_45, _48);
int32_t _50 = _25 + 1;
int32_t _51 = min(_50, 8191);
int32_t _52 = max(_51, 0);
int32_t _53 = max(_25, _52);
int32_t _54 = _25 + -1;
int32_t _55 = min(_54, 8191);
int32_t _56 = max(_55, 0);
int32_t _57 = max(_53, _56);
int32_t _58 = _33 + 1;
int32_t _59 = min(_58, 8191);
int32_t _60 = max(_59, 0);
int32_t _61 = min(_33, _60);
int32_t _62 = _33 + -1;
int32_t _63 = min(_62, 8191);
int32_t _64 = max(_63, 0);
int32_t _65 = min(_61, _64);
int32_t _66 = _41 + 1;
int32_t _67 = min(_66, 8191);
int32_t _68 = max(_67, 0);
int32_t _69 = max(_41, _68);
int32_t _70 = _41 + -1;
int32_t _71 = min(_70, 8191);
int32_t _72 = max(_71, 0);
int32_t _73 = max(_69, _72);
int32_t _74 = _49 + 1;
int32_t _75 = min(_74, 8191);
int32_t _76 = max(_75, 0);
int32_t _77 = min(_49, _76);
int32_t _78 = _49 + -1;
int32_t _79 = min(_78, 8191);
int32_t _80 = max(_79, 0);
int32_t _81 = min(_77, _80);
int32_t _82 = _f5_extent_0 + -1;
int32_t _83 = _82 >> 9;
int32_t _84 = _83 * 512;
int32_t _85 = _84 + _f5_min_0;
int32_t _86 = _85 + 512;
int32_t _87 = min(_86, _17);
int32_t _88 = _17 + -512;
int32_t _89 = min(_f5_min_0, _88);
int32_t _90 = _87 - _89;
int32_t _91 = _f5_extent_0 + 511;
int32_t _92 = _91 >> 9;
int32_t _93 = _f5_extent_1 + 511;
int32_t _94 = _93 >> 9;
int32_t _95 = _92 * _94;
int32_t _96 = _95 + -1;
int32_t _97 = sdiv(_96, _92);
int32_t _98 = max(_97, 0);
int32_t _99 = _98 * 512;
int32_t _100 = _99 + _f5_min_1;
int32_t _101 = _100 + 512;
int32_t _102 = min(_101, _0);
int32_t _103 = min(_97, 0);
int32_t _104 = _103 * 512;
int32_t _105 = _104 + _f5_min_1;
int32_t _106 = _0 + -512;
int32_t _107 = min(_105, _106);
int32_t _108 = _102 - _107;
int32_t _109 = _57 - _65;
int32_t _110 = _109 >> 2;
int32_t _111 = _110 * 4;
int32_t _112 = _111 + _65;
int32_t _113 = _112 + 3;
int32_t _114 = min(_113, _57);
int32_t _115 = _114 + 1;
int32_t _116 = min(_115, 8191);
int32_t _117 = max(_116, 0);
int32_t _118 = max(_114, _117);
int32_t _119 = _114 + -1;
int32_t _120 = min(_119, 8191);
int32_t _121 = max(_120, 0);
int32_t _122 = max(_118, _121);
int32_t _123 = _57 + -3;
int32_t _124 = min(_65, _123);
int32_t _125 = _124 + 1;
int32_t _126 = min(_125, 8191);
int32_t _127 = max(_126, 0);
int32_t _128 = min(_124, _127);
int32_t _129 = _124 + -1;
int32_t _130 = min(_129, 8191);
int32_t _131 = max(_130, 0);
int32_t _132 = min(_128, _131);
int32_t _133 = _122 - _132;
if (_f5_host_and_dev_are_null)
{
 bool _134 = halide_rewrite_buffer(_f5_buffer, 4, _89, _90, 1, _107, _108, _90, 0, 0, 0, 0, 0, 0);
 (void)_134;
} // if _f5_host_and_dev_are_null
if (_inPar_host_and_dev_are_null)
{
 int32_t _135 = _133 + 1;
 int32_t _136 = _73 - _81;
 int32_t _137 = _136 + 1;
 bool _138 = halide_rewrite_buffer(_inPar_buffer, 4, _132, _135, 1, _81, _137, _135, 0, 0, 0, 0, 0, 0);
 (void)_138;
} // if _inPar_host_and_dev_are_null
bool _139 = _f5_host_and_dev_are_null || _inPar_host_and_dev_are_null;
bool _140 = !(_139);
if (_140)
{
 bool _141 = _f5_elem_size == 4;
 if (!_141) {
  halide_printf(NULL, "Output buffer f5 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _f5_elem_size);
  return -1;
 }
 bool _142 = _inPar_elem_size == 4;
 if (!_142) {
  halide_printf(NULL, "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _inPar_elem_size);
  return -1;
 }
 bool _143 = _f5_min_0 <= _89;
 if (!_143) {
  halide_printf(NULL, "Output buffer f5 is accessed at %d, which is before the min (%d) in dimension 0\n", _89, _f5_min_0);
  return -1;
 }
 int32_t _144 = _89 + _90;
 int32_t _145 = _144 - _f5_extent_0;
 bool _146 = _145 <= _f5_min_0;
 int32_t _147 = _144 + -1;
 int32_t _148 = _f5_min_0 + _f5_extent_0;
 int32_t _149 = _148 + -1;
 if (!_146) {
  halide_printf(NULL, "Output buffer f5 is accessed at %d, which is beyond the max (%d) in dimension 0\n", _147, _149);
  return -1;
 }
 bool _150 = _f5_min_1 <= _107;
 if (!_150) {
  halide_printf(NULL, "Output buffer f5 is accessed at %d, which is before the min (%d) in dimension 1\n", _107, _f5_min_1);
  return -1;
 }
 int32_t _151 = _107 + _108;
 int32_t _152 = _151 - _f5_extent_1;
 bool _153 = _152 <= _f5_min_1;
 int32_t _154 = _151 + -1;
 int32_t _155 = _f5_min_1 + _f5_extent_1;
 int32_t _156 = _155 + -1;
 if (!_153) {
  halide_printf(NULL, "Output buffer f5 is accessed at %d, which is beyond the max (%d) in dimension 1\n", _154, _156);
  return -1;
 }
 bool _157 = _inPar_min_0 <= _132;
 if (!_157) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _132, _inPar_min_0);
  return -1;
 }
 int32_t _158 = _132 + _133;
 int32_t _159 = _158 - _inPar_extent_0;
 int32_t _160 = _159 + 1;
 bool _161 = _160 <= _inPar_min_0;
 int32_t _162 = _inPar_min_0 + _inPar_extent_0;
 int32_t _163 = _162 + -1;
 if (!_161) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n", _158, _163);
  return -1;
 }
 bool _164 = _inPar_min_1 <= _81;
 if (!_164) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _81, _inPar_min_1);
  return -1;
 }
 int32_t _165 = _73 - _inPar_extent_1;
 int32_t _166 = _165 + 1;
 bool _167 = _166 <= _inPar_min_1;
 int32_t _168 = _inPar_min_1 + _inPar_extent_1;
 int32_t _169 = _168 + -1;
 if (!_167) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n", _73, _169);
  return -1;
 }
 bool _170 = _f5_stride_0 == 1;
 if (!_170) {
  halide_printf(NULL, "Static constraint violated: f5.stride.0 == 1\n");
  return -1;
 }
 bool _171 = _inPar_stride_0 == 1;
 if (!_171) {
  halide_printf(NULL, "Static constraint violated: inPar.stride.0 == 1\n");
  return -1;
 }
 int64_t _172 = (int64_t)(_f5_extent_0);
 int64_t _173 = (int64_t)(_f5_extent_1);
 int64_t _174 = (int64_t)(_inPar_extent_0);
 int64_t _175 = (int64_t)(_inPar_extent_1);
 int64_t _176 = (int64_t)(2147483647);
 bool _177 = _172 <= _176;
 if (!_177) {
  halide_printf(NULL, "Total allocation for buffer f5 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _178 = (int64_t)(_f5_stride_1);
 int64_t _179 = _173 * _178;
 bool _180 = _179 <= _176;
 if (!_180) {
  halide_printf(NULL, "Total allocation for buffer f5 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _181 = _173 * _172;
 bool _182 = _181 <= _176;
 if (!_182) {
  halide_printf(NULL, "Product of extents for buffer f5 exceeds 2^31 - 1\n");
  return -1;
 }
 bool _183 = _174 <= _176;
 if (!_183) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _184 = (int64_t)(_inPar_stride_1);
 int64_t _185 = _175 * _184;
 bool _186 = _185 <= _176;
 if (!_186) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _187 = _175 * _174;
 bool _188 = _187 <= _176;
 if (!_188) {
  halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 // produce f5
 int32_t _189 = _f5_extent_0 + 511;
 int32_t _190 = _189 >> 9;
 int32_t _191 = _f5_extent_1 + 511;
 int32_t _192 = _191 >> 9;
 int32_t _193 = _190 * _192;
 #pragma omp parallel for
 for (int _f5_s0_x_xo_nid = 0; _f5_s0_x_xo_nid < 0 + _193; _f5_s0_x_xo_nid++)
 {
  int32_t _194 = _f5_extent_0 + 511;
  int32_t _195 = _194 >> 9;
  int32_t _196 = mod(_f5_s0_x_xo_nid, _195);
  int32_t _197 = _196 * 512;
  int32_t _198 = _197 + _f5_min_0;
  int32_t _199 = _f5_min_0 + _f5_extent_0;
  int32_t _200 = _199 + -512;
  int32_t _201 = min(_198, _200);
  int32_t _202 = sdiv(_f5_s0_x_xo_nid, _195);
  int32_t _203 = _202 * 512;
  int32_t _204 = _203 + _f5_min_1;
  int32_t _205 = _f5_min_1 + _f5_extent_1;
  int32_t _206 = _205 + -512;
  int32_t _207 = min(_204, _206);
  for (int _f5_s0_y_yi_yi = 0; _f5_s0_y_yi_yi < 0 + 16; _f5_s0_y_yi_yi++)
  {
   int32_t _208 = _f5_s0_y_yi_yi * 32;
   int32_t _209 = _207 + _208;
   for (int _f5_s0_x_xi_xi = 0; _f5_s0_x_xi_xi < 0 + 16; _f5_s0_x_xi_xi++)
   {
    int32_t _210 = _f5_s0_x_xi_xi * 32;
    int32_t _211 = _201 + _210;
    int32_t _212 = _209 + 1;
    int32_t _213 = min(_212, 8191);
    int32_t _214 = max(_213, 0);
    int32_t _215 = min(_209, _214);
    int32_t _216 = _209 + -1;
    int32_t _217 = min(_216, 8191);
    int32_t _218 = max(_217, 0);
    int32_t _219 = min(_215, _218);
    int32_t _220 = _219 + 1;
    int32_t _221 = min(_220, 8191);
    int32_t _222 = max(_221, 0);
    int32_t _223 = min(_219, _222);
    int32_t _224 = _219 + -1;
    int32_t _225 = min(_224, 8191);
    int32_t _226 = max(_225, 0);
    int32_t _227 = min(_223, _226);
    int32_t _228 = min(_209, 8191);
    int32_t _229 = max(_228, 0);
    int32_t _230 = max(_216, _229);
    int32_t _231 = _209 + -2;
    int32_t _232 = min(_231, 8191);
    int32_t _233 = max(_232, 0);
    int32_t _234 = max(_230, _233);
    int32_t _235 = _234 + 1;
    int32_t _236 = min(_235, 8191);
    int32_t _237 = max(_236, 0);
    int32_t _238 = max(_234, _237);
    int32_t _239 = _234 + -1;
    int32_t _240 = min(_239, 8191);
    int32_t _241 = max(_240, 0);
    int32_t _242 = max(_238, _241);
    int32_t _243 = _242 + 1;
    int32_t _244 = min(_227, _243);
    int32_t _245 = _227 + 1;
    int32_t _246 = min(_245, 8191);
    int32_t _247 = max(_246, 0);
    int32_t _248 = min(_227, _247);
    int32_t _249 = _227 + -1;
    int32_t _250 = min(_249, 8191);
    int32_t _251 = max(_250, 0);
    int32_t _252 = min(_248, _251);
    int32_t _253 = min(_243, 8191);
    int32_t _254 = max(_253, 0);
    int32_t _255 = max(_242, _254);
    int32_t _256 = _242 + -1;
    int32_t _257 = min(_256, 8191);
    int32_t _258 = max(_257, 0);
    int32_t _259 = max(_255, _258);
    int32_t _260 = _259 + 1;
    int32_t _261 = min(_252, _260);
    int32_t _262 = min(_261, _244);
    int32_t _263 = _244 + 1;
    int32_t _264 = min(_263, 8191);
    int32_t _265 = max(_264, 0);
    int32_t _266 = min(_262, _265);
    int32_t _267 = _244 + -1;
    int32_t _268 = min(_267, 8191);
    int32_t _269 = max(_268, 0);
    int32_t _270 = min(_266, _269);
    int32_t _271 = _209 + 31;
    int32_t _272 = _209 + 32;
    int32_t _273 = min(_272, 8191);
    int32_t _274 = max(_273, 0);
    int32_t _275 = max(_271, _274);
    int32_t _276 = _209 + 30;
    int32_t _277 = min(_276, 8191);
    int32_t _278 = max(_277, 0);
    int32_t _279 = max(_275, _278);
    int32_t _280 = _279 + 1;
    int32_t _281 = min(_280, 8191);
    int32_t _282 = max(_281, 0);
    int32_t _283 = max(_279, _282);
    int32_t _284 = _279 + -1;
    int32_t _285 = min(_284, 8191);
    int32_t _286 = max(_285, 0);
    int32_t _287 = max(_283, _286);
    int32_t _288 = _287 + 1;
    int32_t _289 = min(_288, 8191);
    int32_t _290 = max(_289, 0);
    int32_t _291 = max(_287, _290);
    int32_t _292 = _287 + -1;
    int32_t _293 = min(_292, 8191);
    int32_t _294 = max(_293, 0);
    int32_t _295 = max(_291, _294);
    int32_t _296 = max(_295, _287);
    int32_t _297 = _296 - _270;
    int32_t _298 = _211 + 1;
    int32_t _299 = min(_298, 8191);
    int32_t _300 = max(_299, 0);
    int32_t _301 = min(_211, _300);
    int32_t _302 = _211 + -1;
    int32_t _303 = min(_302, 8191);
    int32_t _304 = max(_303, 0);
    int32_t _305 = min(_301, _304);
    int32_t _306 = _305 + 1;
    int32_t _307 = min(_306, 8191);
    int32_t _308 = max(_307, 0);
    int32_t _309 = min(_305, _308);
    int32_t _310 = _305 + -1;
    int32_t _311 = min(_310, 8191);
    int32_t _312 = max(_311, 0);
    int32_t _313 = min(_309, _312);
    int32_t _314 = min(_211, 8191);
    int32_t _315 = max(_314, 0);
    int32_t _316 = max(_302, _315);
    int32_t _317 = _211 + -2;
    int32_t _318 = min(_317, 8191);
    int32_t _319 = max(_318, 0);
    int32_t _320 = max(_316, _319);
    int32_t _321 = _320 + 1;
    int32_t _322 = min(_321, 8191);
    int32_t _323 = max(_322, 0);
    int32_t _324 = max(_320, _323);
    int32_t _325 = _320 + -1;
    int32_t _326 = min(_325, 8191);
    int32_t _327 = max(_326, 0);
    int32_t _328 = max(_324, _327);
    int32_t _329 = _328 + 1;
    int32_t _330 = min(_313, _329);
    int32_t _331 = _211 + 3;
    int32_t _332 = _211 + 4;
    int32_t _333 = min(_332, 8191);
    int32_t _334 = max(_333, 0);
    int32_t _335 = max(_331, _334);
    int32_t _336 = _211 + 2;
    int32_t _337 = min(_336, 8191);
    int32_t _338 = max(_337, 0);
    int32_t _339 = max(_335, _338);
    int32_t _340 = _339 + 1;
    int32_t _341 = min(_340, 8191);
    int32_t _342 = max(_341, 0);
    int32_t _343 = max(_339, _342);
    int32_t _344 = _339 + -1;
    int32_t _345 = min(_344, 8191);
    int32_t _346 = max(_345, 0);
    int32_t _347 = max(_343, _346);
    int32_t _348 = _347 + -3;
    int32_t _349 = min(_330, _348);
    int32_t _350 = _211 + 31;
    int32_t _351 = _211 + 32;
    int32_t _352 = min(_351, 8191);
    int32_t _353 = max(_352, 0);
    int32_t _354 = max(_350, _353);
    int32_t _355 = _211 + 30;
    int32_t _356 = min(_355, 8191);
    int32_t _357 = max(_356, 0);
    int32_t _358 = max(_354, _357);
    int32_t _359 = _211 + 28;
    int32_t _360 = _211 + 29;
    int32_t _361 = min(_360, 8191);
    int32_t _362 = max(_361, 0);
    int32_t _363 = min(_359, _362);
    int32_t _364 = _211 + 27;
    int32_t _365 = min(_364, 8191);
    int32_t _366 = max(_365, 0);
    int32_t _367 = min(_363, _366);
    int32_t _368 = _367 + 1;
    int32_t _369 = min(_368, 8191);
    int32_t _370 = max(_369, 0);
    int32_t _371 = min(_367, _370);
    int32_t _372 = _367 + -1;
    int32_t _373 = min(_372, 8191);
    int32_t _374 = max(_373, 0);
    int32_t _375 = min(_371, _374);
    int32_t _376 = min(_359, 8191);
    int32_t _377 = max(_376, 0);
    int32_t _378 = max(_364, _377);
    int32_t _379 = _211 + 26;
    int32_t _380 = min(_379, 8191);
    int32_t _381 = max(_380, 0);
    int32_t _382 = max(_378, _381);
    int32_t _383 = _382 + 1;
    int32_t _384 = min(_383, 8191);
    int32_t _385 = max(_384, 0);
    int32_t _386 = max(_382, _385);
    int32_t _387 = _382 + -1;
    int32_t _388 = min(_387, 8191);
    int32_t _389 = max(_388, 0);
    int32_t _390 = max(_386, _389);
    int32_t _391 = _390 + 1;
    int32_t _392 = max(_375, _391);
    int32_t _393 = _358 + 1;
    int32_t _394 = min(_393, 8191);
    int32_t _395 = max(_394, 0);
    int32_t _396 = max(_358, _395);
    int32_t _397 = _358 + -1;
    int32_t _398 = min(_397, 8191);
    int32_t _399 = max(_398, 0);
    int32_t _400 = max(_396, _399);
    int32_t _401 = _400 - _330;
    int32_t _402 = _401 >> 2;
    int32_t _403 = _402 * 4;
    int32_t _404 = _392 + _403;
    int32_t _405 = _404 + 3;
    int32_t _406 = min(_405, _400);
    int32_t _407 = _406 - _349;
    {
     int32_t _408 = _407 + 1;
     int64_t _409 = _408;
     int32_t _410 = _297 + 1;
     int64_t _411 = _409 * _410;
     if ((_411 > ((int64_t(1) << 31) - 1)) || ((_411 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
     {
      halide_printf(NULL, "32-bit signed overflow computing size of allocation f0\n");
     } // overflow test f0
     int64_t _412 = _411;
     float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_412);
     int32_t _413 = _209 + 1;
     int32_t _414 = min(_413, 8191);
     int32_t _415 = max(_414, 0);
     int32_t _416 = min(_209, _415);
     int32_t _417 = _209 + -1;
     int32_t _418 = min(_417, 8191);
     int32_t _419 = max(_418, 0);
     int32_t _420 = min(_416, _419);
     int32_t _421 = _420 + 1;
     int32_t _422 = min(_421, 8191);
     int32_t _423 = max(_422, 0);
     int32_t _424 = min(_420, _423);
     int32_t _425 = _420 + -1;
     int32_t _426 = min(_425, 8191);
     int32_t _427 = max(_426, 0);
     int32_t _428 = min(_424, _427);
     int32_t _429 = min(_209, 8191);
     int32_t _430 = max(_429, 0);
     int32_t _431 = max(_417, _430);
     int32_t _432 = _209 + -2;
     int32_t _433 = min(_432, 8191);
     int32_t _434 = max(_433, 0);
     int32_t _435 = max(_431, _434);
     int32_t _436 = _435 + 1;
     int32_t _437 = min(_436, 8191);
     int32_t _438 = max(_437, 0);
     int32_t _439 = max(_435, _438);
     int32_t _440 = _435 + -1;
     int32_t _441 = min(_440, 8191);
     int32_t _442 = max(_441, 0);
     int32_t _443 = max(_439, _442);
     int32_t _444 = _443 + 1;
     int32_t _445 = min(_428, _444);
     int32_t _446 = _209 + 31;
     int32_t _447 = _209 + 32;
     int32_t _448 = min(_447, 8191);
     int32_t _449 = max(_448, 0);
     int32_t _450 = max(_446, _449);
     int32_t _451 = _209 + 30;
     int32_t _452 = min(_451, 8191);
     int32_t _453 = max(_452, 0);
     int32_t _454 = max(_450, _453);
     int32_t _455 = _454 + 1;
     int32_t _456 = min(_455, 8191);
     int32_t _457 = max(_456, 0);
     int32_t _458 = max(_454, _457);
     int32_t _459 = _454 + -1;
     int32_t _460 = min(_459, 8191);
     int32_t _461 = max(_460, 0);
     int32_t _462 = max(_458, _461);
     int32_t _463 = _462 - _445;
     int32_t _464 = _211 + 3;
     int32_t _465 = _211 + 4;
     int32_t _466 = min(_465, 8191);
     int32_t _467 = max(_466, 0);
     int32_t _468 = max(_464, _467);
     int32_t _469 = _211 + 2;
     int32_t _470 = min(_469, 8191);
     int32_t _471 = max(_470, 0);
     int32_t _472 = max(_468, _471);
     int32_t _473 = _211 + 1;
     int32_t _474 = min(_473, 8191);
     int32_t _475 = max(_474, 0);
     int32_t _476 = min(_211, _475);
     int32_t _477 = _211 + -1;
     int32_t _478 = min(_477, 8191);
     int32_t _479 = max(_478, 0);
     int32_t _480 = min(_476, _479);
     int32_t _481 = min(_211, 8191);
     int32_t _482 = max(_481, 0);
     int32_t _483 = max(_477, _482);
     int32_t _484 = _211 + -2;
     int32_t _485 = min(_484, 8191);
     int32_t _486 = max(_485, 0);
     int32_t _487 = max(_483, _486);
     int32_t _488 = _487 + 1;
     int32_t _489 = min(_480, _488);
     int32_t _490 = _472 + -3;
     int32_t _491 = min(_489, _490);
     int32_t _492 = _480 + 1;
     int32_t _493 = min(_492, 8191);
     int32_t _494 = max(_493, 0);
     int32_t _495 = min(_480, _494);
     int32_t _496 = _480 + -1;
     int32_t _497 = min(_496, 8191);
     int32_t _498 = max(_497, 0);
     int32_t _499 = min(_495, _498);
     int32_t _500 = min(_488, 8191);
     int32_t _501 = max(_500, 0);
     int32_t _502 = max(_487, _501);
     int32_t _503 = _487 + -1;
     int32_t _504 = min(_503, 8191);
     int32_t _505 = max(_504, 0);
     int32_t _506 = max(_502, _505);
     int32_t _507 = _506 + 1;
     int32_t _508 = min(_499, _507);
     int32_t _509 = _472 + 1;
     int32_t _510 = min(_509, 8191);
     int32_t _511 = max(_510, 0);
     int32_t _512 = max(_472, _511);
     int32_t _513 = _472 + -1;
     int32_t _514 = min(_513, 8191);
     int32_t _515 = max(_514, 0);
     int32_t _516 = max(_512, _515);
     int32_t _517 = _516 + -3;
     int32_t _518 = min(_508, _517);
     int32_t _519 = min(_518, _491);
     int32_t _520 = _491 + 1;
     int32_t _521 = min(_520, 8191);
     int32_t _522 = max(_521, 0);
     int32_t _523 = min(_519, _522);
     int32_t _524 = _491 + -1;
     int32_t _525 = min(_524, 8191);
     int32_t _526 = max(_525, 0);
     int32_t _527 = min(_523, _526);
     int32_t _528 = _211 + 31;
     int32_t _529 = _211 + 32;
     int32_t _530 = min(_529, 8191);
     int32_t _531 = max(_530, 0);
     int32_t _532 = max(_528, _531);
     int32_t _533 = _211 + 30;
     int32_t _534 = min(_533, 8191);
     int32_t _535 = max(_534, 0);
     int32_t _536 = max(_532, _535);
     int32_t _537 = _536 + 1;
     int32_t _538 = min(_537, 8191);
     int32_t _539 = max(_538, 0);
     int32_t _540 = max(_536, _539);
     int32_t _541 = _536 + -1;
     int32_t _542 = min(_541, 8191);
     int32_t _543 = max(_542, 0);
     int32_t _544 = max(_540, _543);
     int32_t _545 = _211 + 28;
     int32_t _546 = _211 + 29;
     int32_t _547 = min(_546, 8191);
     int32_t _548 = max(_547, 0);
     int32_t _549 = min(_545, _548);
     int32_t _550 = _211 + 27;
     int32_t _551 = min(_550, 8191);
     int32_t _552 = max(_551, 0);
     int32_t _553 = min(_549, _552);
     int32_t _554 = min(_545, 8191);
     int32_t _555 = max(_554, 0);
     int32_t _556 = max(_550, _555);
     int32_t _557 = _211 + 26;
     int32_t _558 = min(_557, 8191);
     int32_t _559 = max(_558, 0);
     int32_t _560 = max(_556, _559);
     int32_t _561 = _560 + 1;
     int32_t _562 = max(_553, _561);
     int32_t _563 = _536 - _489;
     int32_t _564 = _563 >> 2;
     int32_t _565 = _564 * 4;
     int32_t _566 = _562 + _565;
     int32_t _567 = _566 + 3;
     int32_t _568 = min(_567, _536);
     int32_t _569 = _553 + 1;
     int32_t _570 = min(_569, 8191);
     int32_t _571 = max(_570, 0);
     int32_t _572 = min(_553, _571);
     int32_t _573 = _553 + -1;
     int32_t _574 = min(_573, 8191);
     int32_t _575 = max(_574, 0);
     int32_t _576 = min(_572, _575);
     int32_t _577 = min(_561, 8191);
     int32_t _578 = max(_577, 0);
     int32_t _579 = max(_560, _578);
     int32_t _580 = _560 + -1;
     int32_t _581 = min(_580, 8191);
     int32_t _582 = max(_581, 0);
     int32_t _583 = max(_579, _582);
     int32_t _584 = _583 + 1;
     int32_t _585 = max(_576, _584);
     int32_t _586 = _544 - _508;
     int32_t _587 = _586 >> 2;
     int32_t _588 = _587 * 4;
     int32_t _589 = _585 + _588;
     int32_t _590 = _589 + 3;
     int32_t _591 = min(_590, _544);
     int32_t _592 = max(_591, _568);
     int32_t _593 = _568 + 1;
     int32_t _594 = min(_593, 8191);
     int32_t _595 = max(_594, 0);
     int32_t _596 = max(_592, _595);
     int32_t _597 = _568 + -1;
     int32_t _598 = min(_597, 8191);
     int32_t _599 = max(_598, 0);
     int32_t _600 = max(_596, _599);
     int32_t _601 = _600 - _527;
     {
      int32_t _602 = _601 + 1;
      int64_t _603 = _602;
      int32_t _604 = _463 + 1;
      int64_t _605 = _603 * _604;
      if ((_605 > ((int64_t(1) << 31) - 1)) || ((_605 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
      {
       halide_printf(NULL, "32-bit signed overflow computing size of allocation f3\n");
      } // overflow test f3
      int64_t _606 = _605;
      float *_f3 = (float *)halide_malloc(NULL, sizeof(float)*_606);
      int32_t _607 = _209 + 1;
      int32_t _608 = min(_607, 8191);
      int32_t _609 = max(_608, 0);
      int32_t _610 = min(_209, _609);
      int32_t _611 = _209 + -1;
      int32_t _612 = min(_611, 8191);
      int32_t _613 = max(_612, 0);
      int32_t _614 = min(_610, _613);
      int32_t _615 = min(_209, 8191);
      int32_t _616 = max(_615, 0);
      int32_t _617 = max(_611, _616);
      int32_t _618 = _209 + -2;
      int32_t _619 = min(_618, 8191);
      int32_t _620 = max(_619, 0);
      int32_t _621 = max(_617, _620);
      int32_t _622 = _621 + 1;
      int32_t _623 = min(_614, _622);
      int32_t _624 = _614 + 1;
      int32_t _625 = min(_624, 8191);
      int32_t _626 = max(_625, 0);
      int32_t _627 = min(_614, _626);
      int32_t _628 = _614 + -1;
      int32_t _629 = min(_628, 8191);
      int32_t _630 = max(_629, 0);
      int32_t _631 = min(_627, _630);
      int32_t _632 = min(_622, 8191);
      int32_t _633 = max(_632, 0);
      int32_t _634 = max(_621, _633);
      int32_t _635 = _621 + -1;
      int32_t _636 = min(_635, 8191);
      int32_t _637 = max(_636, 0);
      int32_t _638 = max(_634, _637);
      int32_t _639 = _638 + 1;
      int32_t _640 = min(_631, _639);
      int32_t _641 = min(_640, _623);
      int32_t _642 = _623 + 1;
      int32_t _643 = min(_642, 8191);
      int32_t _644 = max(_643, 0);
      int32_t _645 = min(_641, _644);
      int32_t _646 = _623 + -1;
      int32_t _647 = min(_646, 8191);
      int32_t _648 = max(_647, 0);
      int32_t _649 = min(_645, _648);
      int32_t _650 = _209 + 31;
      int32_t _651 = _209 + 32;
      int32_t _652 = min(_651, 8191);
      int32_t _653 = max(_652, 0);
      int32_t _654 = max(_650, _653);
      int32_t _655 = _209 + 30;
      int32_t _656 = min(_655, 8191);
      int32_t _657 = max(_656, 0);
      int32_t _658 = max(_654, _657);
      int32_t _659 = _658 + 1;
      int32_t _660 = min(_659, 8191);
      int32_t _661 = max(_660, 0);
      int32_t _662 = max(_658, _661);
      int32_t _663 = _658 + -1;
      int32_t _664 = min(_663, 8191);
      int32_t _665 = max(_664, 0);
      int32_t _666 = max(_662, _665);
      int32_t _667 = max(_666, _658);
      int32_t _668 = _667 - _649;
      int32_t _669 = _211 + 1;
      int32_t _670 = min(_669, 8191);
      int32_t _671 = max(_670, 0);
      int32_t _672 = min(_211, _671);
      int32_t _673 = _211 + -1;
      int32_t _674 = min(_673, 8191);
      int32_t _675 = max(_674, 0);
      int32_t _676 = min(_672, _675);
      int32_t _677 = min(_211, 8191);
      int32_t _678 = max(_677, 0);
      int32_t _679 = max(_673, _678);
      int32_t _680 = _211 + -2;
      int32_t _681 = min(_680, 8191);
      int32_t _682 = max(_681, 0);
      int32_t _683 = max(_679, _682);
      int32_t _684 = _683 + 1;
      int32_t _685 = min(_676, _684);
      int32_t _686 = _211 + 3;
      int32_t _687 = _211 + 4;
      int32_t _688 = min(_687, 8191);
      int32_t _689 = max(_688, 0);
      int32_t _690 = max(_686, _689);
      int32_t _691 = _211 + 2;
      int32_t _692 = min(_691, 8191);
      int32_t _693 = max(_692, 0);
      int32_t _694 = max(_690, _693);
      int32_t _695 = _694 + -3;
      int32_t _696 = min(_685, _695);
      int32_t _697 = _211 + 28;
      int32_t _698 = _211 + 29;
      int32_t _699 = min(_698, 8191);
      int32_t _700 = max(_699, 0);
      int32_t _701 = min(_697, _700);
      int32_t _702 = _211 + 27;
      int32_t _703 = min(_702, 8191);
      int32_t _704 = max(_703, 0);
      int32_t _705 = min(_701, _704);
      int32_t _706 = min(_697, 8191);
      int32_t _707 = max(_706, 0);
      int32_t _708 = max(_702, _707);
      int32_t _709 = _211 + 26;
      int32_t _710 = min(_709, 8191);
      int32_t _711 = max(_710, 0);
      int32_t _712 = max(_708, _711);
      int32_t _713 = _712 + 1;
      int32_t _714 = max(_705, _713);
      int32_t _715 = _211 + 31;
      int32_t _716 = _211 + 32;
      int32_t _717 = min(_716, 8191);
      int32_t _718 = max(_717, 0);
      int32_t _719 = max(_715, _718);
      int32_t _720 = _211 + 30;
      int32_t _721 = min(_720, 8191);
      int32_t _722 = max(_721, 0);
      int32_t _723 = max(_719, _722);
      int32_t _724 = _723 - _685;
      int32_t _725 = _724 >> 2;
      int32_t _726 = _725 * 4;
      int32_t _727 = _714 + _726;
      int32_t _728 = _727 + 3;
      int32_t _729 = min(_728, _723);
      int32_t _730 = _729 - _696;
      {
       int32_t _731 = _730 + 1;
       int64_t _732 = _731;
       int32_t _733 = _668 + 1;
       int64_t _734 = _732 * _733;
       if ((_734 > ((int64_t(1) << 31) - 1)) || ((_734 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
       {
        halide_printf(NULL, "32-bit signed overflow computing size of allocation f1\n");
       } // overflow test f1
       int64_t _735 = _734;
       float *_f1 = (float *)halide_malloc(NULL, sizeof(float)*_735);
       int32_t _736 = _209 + 1;
       int32_t _737 = min(_736, 8191);
       int32_t _738 = max(_737, 0);
       int32_t _739 = min(_209, _738);
       int32_t _740 = _209 + -1;
       int32_t _741 = min(_740, 8191);
       int32_t _742 = max(_741, 0);
       int32_t _743 = min(_739, _742);
       int32_t _744 = min(_209, 8191);
       int32_t _745 = max(_744, 0);
       int32_t _746 = max(_740, _745);
       int32_t _747 = _209 + -2;
       int32_t _748 = min(_747, 8191);
       int32_t _749 = max(_748, 0);
       int32_t _750 = max(_746, _749);
       int32_t _751 = _750 + 1;
       int32_t _752 = min(_743, _751);
       int32_t _753 = _209 + 31;
       int32_t _754 = _209 + 32;
       int32_t _755 = min(_754, 8191);
       int32_t _756 = max(_755, 0);
       int32_t _757 = max(_753, _756);
       int32_t _758 = _209 + 30;
       int32_t _759 = min(_758, 8191);
       int32_t _760 = max(_759, 0);
       int32_t _761 = max(_757, _760);
       int32_t _762 = _761 - _752;
       int32_t _763 = _211 + 1;
       int32_t _764 = min(_763, 8191);
       int32_t _765 = max(_764, 0);
       int32_t _766 = _211 + -1;
       int32_t _767 = min(_766, 8191);
       int32_t _768 = max(_767, 0);
       int32_t _769 = min(_211, _765);
       int32_t _770 = min(_769, _768);
       int32_t _771 = min(_211, 8191);
       int32_t _772 = max(_771, 0);
       int32_t _773 = max(_766, _772);
       int32_t _774 = _211 + -2;
       int32_t _775 = min(_774, 8191);
       int32_t _776 = max(_775, 0);
       int32_t _777 = max(_773, _776);
       int32_t _778 = _777 + 1;
       int32_t _779 = min(_770, _778);
       int32_t _780 = _211 + 3;
       int32_t _781 = _211 + 4;
       int32_t _782 = min(_781, 8191);
       int32_t _783 = max(_782, 0);
       int32_t _784 = max(_780, _783);
       int32_t _785 = _211 + 2;
       int32_t _786 = min(_785, 8191);
       int32_t _787 = max(_786, 0);
       int32_t _788 = max(_784, _787);
       int32_t _789 = _788 + -3;
       int32_t _790 = min(_779, _789);
       int32_t _791 = min(_790, _211);
       int32_t _792 = min(_791, _765);
       int32_t _793 = min(_792, _768);
       int32_t _794 = _211 + 31;
       int32_t _795 = _211 + 32;
       int32_t _796 = min(_795, 8191);
       int32_t _797 = max(_796, 0);
       int32_t _798 = max(_794, _797);
       int32_t _799 = _211 + 30;
       int32_t _800 = min(_799, 8191);
       int32_t _801 = max(_800, 0);
       int32_t _802 = max(_798, _801);
       int32_t _803 = _211 + 28;
       int32_t _804 = _211 + 29;
       int32_t _805 = min(_804, 8191);
       int32_t _806 = max(_805, 0);
       int32_t _807 = min(_803, _806);
       int32_t _808 = _211 + 27;
       int32_t _809 = min(_808, 8191);
       int32_t _810 = max(_809, 0);
       int32_t _811 = min(_807, _810);
       int32_t _812 = min(_803, 8191);
       int32_t _813 = max(_812, 0);
       int32_t _814 = max(_808, _813);
       int32_t _815 = _211 + 26;
       int32_t _816 = min(_815, 8191);
       int32_t _817 = max(_816, 0);
       int32_t _818 = max(_814, _817);
       int32_t _819 = _818 + 1;
       int32_t _820 = max(_811, _819);
       int32_t _821 = _802 - _779;
       int32_t _822 = _821 >> 2;
       int32_t _823 = _822 * 4;
       int32_t _824 = _820 + _823;
       int32_t _825 = _824 + 3;
       int32_t _826 = min(_825, _802);
       int32_t _827 = max(_826, _794);
       int32_t _828 = max(_827, _797);
       int32_t _829 = max(_828, _801);
       int32_t _830 = _829 - _793;
       {
        int32_t _831 = _830 + 1;
        int64_t _832 = _831;
        int32_t _833 = _762 + 1;
        int64_t _834 = _832 * _833;
        if ((_834 > ((int64_t(1) << 31) - 1)) || ((_834 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
        {
         halide_printf(NULL, "32-bit signed overflow computing size of allocation f4\n");
        } // overflow test f4
        int64_t _835 = _834;
        float *_f4 = (float *)halide_malloc(NULL, sizeof(float)*_835);
        int32_t _836 = _209 + 1;
        int32_t _837 = min(_836, 8191);
        int32_t _838 = max(_837, 0);
        int32_t _839 = _209 + -1;
        int32_t _840 = min(_839, 8191);
        int32_t _841 = max(_840, 0);
        int32_t _842 = min(_209, _838);
        int32_t _843 = min(_842, _841);
        int32_t _844 = min(_209, 8191);
        int32_t _845 = max(_844, 0);
        int32_t _846 = max(_839, _845);
        int32_t _847 = _209 + -2;
        int32_t _848 = min(_847, 8191);
        int32_t _849 = max(_848, 0);
        int32_t _850 = max(_846, _849);
        int32_t _851 = _850 + 1;
        int32_t _852 = min(_843, _851);
        int32_t _853 = min(_852, _209);
        int32_t _854 = _209 + 32;
        int32_t _855 = min(_854, 8191);
        int32_t _856 = max(_855, 0);
        int32_t _857 = _209 + 30;
        int32_t _858 = min(_857, 8191);
        int32_t _859 = max(_858, 0);
        int32_t _860 = _209 + 31;
        int32_t _861 = max(_860, _856);
        int32_t _862 = max(_861, _859);
        int32_t _863 = max(_862, _860);
        int32_t _864 = _863 - _853;
        {
         int64_t _865 = 32;
         int32_t _866 = _864 + 1;
         int64_t _867 = _865 * _866;
         if ((_867 > ((int64_t(1) << 31) - 1)) || ((_867 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
         {
          halide_printf(NULL, "32-bit signed overflow computing size of allocation f2\n");
         } // overflow test f2
         int64_t _868 = _867;
         float *_f2 = (float *)halide_malloc(NULL, sizeof(float)*_868);
         for (int _f5_s0_y_yi_yii = 0; _f5_s0_y_yi_yii < 0 + 32; _f5_s0_y_yi_yii++)
         {
          int32_t _869 = _f5_s0_y_yi_yi * 32;
          int32_t _870 = _869 + _f5_s0_y_yi_yii;
          int32_t _871 = _207 + _870;
          for (int _f5_s0_x_xi_xii_xii = 0; _f5_s0_x_xi_xii_xii < 0 + 8; _f5_s0_x_xi_xii_xii++)
          {
           int32_t _872 = _f5_s0_x_xi_xi * 32;
           int32_t _873 = _f5_s0_x_xi_xii_xii * 4;
           int32_t _874 = _872 + _873;
           int32_t _875 = _201 + _874;
           int32_t _876 = _871 + 1;
           int32_t _877 = min(_876, 8191);
           int32_t _878 = max(_877, 0);
           int32_t _879 = max(_871, _878);
           int32_t _880 = _871 + -1;
           int32_t _881 = min(_880, 8191);
           int32_t _882 = max(_881, 0);
           int32_t _883 = max(_879, _882);
           int32_t _884 = min(_871, _878);
           int32_t _885 = min(_884, _882);
           int32_t _886 = min(_871, 8191);
           int32_t _887 = max(_886, 0);
           int32_t _888 = max(_880, _887);
           int32_t _889 = _871 + -2;
           int32_t _890 = min(_889, 8191);
           int32_t _891 = max(_890, 0);
           int32_t _892 = max(_888, _891);
           int32_t _893 = _892 + 1;
           bool _894 = _f5_s0_y_yi_yii == 0;
           int32_t _895 = (int32_t)(_894 ? _885 : _893);
           int32_t _896 = _875 + 3;
           int32_t _897 = _875 + 4;
           int32_t _898 = min(_897, 8191);
           int32_t _899 = max(_898, 0);
           int32_t _900 = max(_896, _899);
           int32_t _901 = _875 + 2;
           int32_t _902 = min(_901, 8191);
           int32_t _903 = max(_902, 0);
           int32_t _904 = max(_900, _903);
           int32_t _905 = _875 + 1;
           int32_t _906 = min(_905, 8191);
           int32_t _907 = max(_906, 0);
           int32_t _908 = min(_875, _907);
           int32_t _909 = _875 + -1;
           int32_t _910 = min(_909, 8191);
           int32_t _911 = max(_910, 0);
           int32_t _912 = min(_908, _911);
           int32_t _913 = min(_875, 8191);
           int32_t _914 = max(_913, 0);
           int32_t _915 = max(_909, _914);
           int32_t _916 = _875 + -2;
           int32_t _917 = min(_916, 8191);
           int32_t _918 = max(_917, 0);
           int32_t _919 = max(_915, _918);
           int32_t _920 = _919 + 1;
           bool _921 = _f5_s0_x_xi_xii_xii == 0;
           int32_t _922 = (int32_t)(_921 ? _912 : _920);
           int32_t _923 = _883 + 1;
           int32_t _924 = min(_923, 8191);
           int32_t _925 = max(_924, 0);
           int32_t _926 = max(_883, _925);
           int32_t _927 = _883 + -1;
           int32_t _928 = min(_927, 8191);
           int32_t _929 = max(_928, 0);
           int32_t _930 = max(_926, _929);
           int32_t _931 = _885 + 1;
           int32_t _932 = min(_931, 8191);
           int32_t _933 = max(_932, 0);
           int32_t _934 = min(_885, _933);
           int32_t _935 = _885 + -1;
           int32_t _936 = min(_935, 8191);
           int32_t _937 = max(_936, 0);
           int32_t _938 = min(_934, _937);
           int32_t _939 = min(_893, 8191);
           int32_t _940 = max(_939, 0);
           int32_t _941 = max(_892, _940);
           int32_t _942 = _892 + -1;
           int32_t _943 = min(_942, 8191);
           int32_t _944 = max(_943, 0);
           int32_t _945 = max(_941, _944);
           int32_t _946 = _945 + 1;
           int32_t _947 = (int32_t)(_894 ? _938 : _946);
           int32_t _948 = _904 + 1;
           int32_t _949 = min(_948, 8191);
           int32_t _950 = max(_949, 0);
           int32_t _951 = max(_904, _950);
           int32_t _952 = _904 + -1;
           int32_t _953 = min(_952, 8191);
           int32_t _954 = max(_953, 0);
           int32_t _955 = max(_951, _954);
           int32_t _956 = _912 + 1;
           int32_t _957 = min(_956, 8191);
           int32_t _958 = max(_957, 0);
           int32_t _959 = min(_912, _958);
           int32_t _960 = _912 + -1;
           int32_t _961 = min(_960, 8191);
           int32_t _962 = max(_961, 0);
           int32_t _963 = min(_959, _962);
           int32_t _964 = min(_920, 8191);
           int32_t _965 = max(_964, 0);
           int32_t _966 = max(_919, _965);
           int32_t _967 = _919 + -1;
           int32_t _968 = min(_967, 8191);
           int32_t _969 = max(_968, 0);
           int32_t _970 = max(_966, _969);
           int32_t _971 = _970 + 1;
           int32_t _972 = (int32_t)(_921 ? _963 : _971);
           int32_t _973 = _930 + 1;
           int32_t _974 = min(_973, 8191);
           int32_t _975 = max(_974, 0);
           int32_t _976 = max(_930, _975);
           int32_t _977 = _930 + -1;
           int32_t _978 = min(_977, 8191);
           int32_t _979 = max(_978, 0);
           int32_t _980 = max(_976, _979);
           int32_t _981 = _938 + 1;
           int32_t _982 = min(_981, 8191);
           int32_t _983 = max(_982, 0);
           int32_t _984 = min(_938, _983);
           int32_t _985 = _938 + -1;
           int32_t _986 = min(_985, 8191);
           int32_t _987 = max(_986, 0);
           int32_t _988 = min(_984, _987);
           int32_t _989 = min(_946, 8191);
           int32_t _990 = max(_989, 0);
           int32_t _991 = max(_945, _990);
           int32_t _992 = _945 + -1;
           int32_t _993 = min(_992, 8191);
           int32_t _994 = max(_993, 0);
           int32_t _995 = max(_991, _994);
           int32_t _996 = _995 + 1;
           int32_t _997 = (int32_t)(_894 ? _988 : _996);
           // produce f0
           int32_t _998 = _980 - _997;
           int32_t _999 = _998 + 1;
           for (int _f0_s0_y = _997; _f0_s0_y < _997 + _999; _f0_s0_y++)
           {
            int32_t _1000 = _955 - _972;
            int32_t _1001 = _1000 + 4;
            int32_t _1002 = _1001 >> 2;
            for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + _1002; _f0_s0_x_x++)
            {
             int32_t _1003 = _f0_s0_x_x * 4;
             int32_t _1004 = _1003 + _972;
             int32_t _1005 = _955 + -3;
             int32_t _1006 = min(_1004, _1005);
             int32_t _1007 = _1006 - _349;
             int32_t _1008 = _f0_s0_y - _270;
             int32_t _1009 = _407 + 1;
             int32_t _1010 = _1008 * _1009;
             int32_t _1011 = _1007 + _1010;
             int32_t _1012 = _f0_s0_y * _inPar_stride_1;
             int32_t _1013 = _1006 + _1012;
             int32_t _1014 = _inPar_min_1 * _inPar_stride_1;
             int32_t _1015 = _inPar_min_0 + _1014;
             int32_t _1016 = _1013 - _1015;
             float _1017 = ((float *)_inPar)[_1016];
             float _1018 = _1017 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1019 = _1006 + 1;
             int32_t _1020 = min(_1019, 8191);
             int32_t _1021 = max(_1020, 0);
             int32_t _1022 = _1021 + _1012;
             int32_t _1023 = _1022 - _1015;
             float _1024 = ((float *)_inPar)[_1023];
             float _1025 = _1024 * float_from_bits(1048576000 /* 0.25 */);
             float _1026 = _1018 + _1025;
             int32_t _1027 = _1006 + -1;
             int32_t _1028 = min(_1027, 8191);
             int32_t _1029 = max(_1028, 0);
             int32_t _1030 = _1029 + _1012;
             int32_t _1031 = _1030 - _1015;
             float _1032 = ((float *)_inPar)[_1031];
             float _1033 = _1032 * float_from_bits(1048576000 /* 0.25 */);
             float _1034 = _1026 + _1033;
             _f0[_1011] = _1034;
             int32_t _1035 = _1006 - _349;
             int32_t _1036 = _f0_s0_y - _270;
             int32_t _1037 = _407 + 1;
             int32_t _1038 = _1036 * _1037;
             int32_t _1039 = _1035 + _1038;
             int32_t _1040 = _1039 + 1;
             int32_t _1041 = _f0_s0_y * _inPar_stride_1;
             int32_t _1042 = _1006 + _1041;
             int32_t _1043 = _inPar_min_1 * _inPar_stride_1;
             int32_t _1044 = _inPar_min_0 + _1043;
             int32_t _1045 = _1042 - _1044;
             int32_t _1046 = _1045 + 1;
             float _1047 = ((float *)_inPar)[_1046];
             float _1048 = _1047 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1049 = _1006 + 2;
             int32_t _1050 = min(_1049, 8191);
             int32_t _1051 = max(_1050, 0);
             int32_t _1052 = _1051 + _1041;
             int32_t _1053 = _1052 - _1044;
             float _1054 = ((float *)_inPar)[_1053];
             float _1055 = _1054 * float_from_bits(1048576000 /* 0.25 */);
             float _1056 = _1048 + _1055;
             int32_t _1057 = min(_1006, 8191);
             int32_t _1058 = max(_1057, 0);
             int32_t _1059 = _1058 + _1041;
             int32_t _1060 = _1059 - _1044;
             float _1061 = ((float *)_inPar)[_1060];
             float _1062 = _1061 * float_from_bits(1048576000 /* 0.25 */);
             float _1063 = _1056 + _1062;
             _f0[_1040] = _1063;
             int32_t _1064 = _1006 - _349;
             int32_t _1065 = _f0_s0_y - _270;
             int32_t _1066 = _407 + 1;
             int32_t _1067 = _1065 * _1066;
             int32_t _1068 = _1064 + _1067;
             int32_t _1069 = _1068 + 2;
             int32_t _1070 = _f0_s0_y * _inPar_stride_1;
             int32_t _1071 = _1006 + _1070;
             int32_t _1072 = _inPar_min_1 * _inPar_stride_1;
             int32_t _1073 = _inPar_min_0 + _1072;
             int32_t _1074 = _1071 - _1073;
             int32_t _1075 = _1074 + 2;
             float _1076 = ((float *)_inPar)[_1075];
             float _1077 = _1076 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1078 = _1006 + 3;
             int32_t _1079 = min(_1078, 8191);
             int32_t _1080 = max(_1079, 0);
             int32_t _1081 = _1080 + _1070;
             int32_t _1082 = _1081 - _1073;
             float _1083 = ((float *)_inPar)[_1082];
             float _1084 = _1083 * float_from_bits(1048576000 /* 0.25 */);
             float _1085 = _1077 + _1084;
             int32_t _1086 = _1006 + 1;
             int32_t _1087 = min(_1086, 8191);
             int32_t _1088 = max(_1087, 0);
             int32_t _1089 = _1088 + _1070;
             int32_t _1090 = _1089 - _1073;
             float _1091 = ((float *)_inPar)[_1090];
             float _1092 = _1091 * float_from_bits(1048576000 /* 0.25 */);
             float _1093 = _1085 + _1092;
             _f0[_1069] = _1093;
             int32_t _1094 = _1006 - _349;
             int32_t _1095 = _f0_s0_y - _270;
             int32_t _1096 = _407 + 1;
             int32_t _1097 = _1095 * _1096;
             int32_t _1098 = _1094 + _1097;
             int32_t _1099 = _1098 + 3;
             int32_t _1100 = _f0_s0_y * _inPar_stride_1;
             int32_t _1101 = _1006 + _1100;
             int32_t _1102 = _inPar_min_1 * _inPar_stride_1;
             int32_t _1103 = _inPar_min_0 + _1102;
             int32_t _1104 = _1101 - _1103;
             int32_t _1105 = _1104 + 3;
             float _1106 = ((float *)_inPar)[_1105];
             float _1107 = _1106 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1108 = _1006 + 4;
             int32_t _1109 = min(_1108, 8191);
             int32_t _1110 = max(_1109, 0);
             int32_t _1111 = _1110 + _1100;
             int32_t _1112 = _1111 - _1103;
             float _1113 = ((float *)_inPar)[_1112];
             float _1114 = _1113 * float_from_bits(1048576000 /* 0.25 */);
             float _1115 = _1107 + _1114;
             int32_t _1116 = _1006 + 2;
             int32_t _1117 = min(_1116, 8191);
             int32_t _1118 = max(_1117, 0);
             int32_t _1119 = _1118 + _1100;
             int32_t _1120 = _1119 - _1103;
             float _1121 = ((float *)_inPar)[_1120];
             float _1122 = _1121 * float_from_bits(1048576000 /* 0.25 */);
             float _1123 = _1115 + _1122;
             _f0[_1099] = _1123;
            } // for _f0_s0_x_x
           } // for _f0_s0_y
           // consume f0
           // produce f3
           int32_t _1124 = _930 - _947;
           int32_t _1125 = _1124 + 1;
           for (int _f3_s0_y = _947; _f3_s0_y < _947 + _1125; _f3_s0_y++)
           {
            int32_t _1126 = _955 - _972;
            int32_t _1127 = _1126 + 4;
            int32_t _1128 = _1127 >> 2;
            for (int _f3_s0_x_x = 0; _f3_s0_x_x < 0 + _1128; _f3_s0_x_x++)
            {
             int32_t _1129 = _f3_s0_x_x * 4;
             int32_t _1130 = _1129 + _972;
             int32_t _1131 = _955 + -3;
             int32_t _1132 = min(_1130, _1131);
             int32_t _1133 = _1132 - _527;
             int32_t _1134 = _f3_s0_y - _445;
             int32_t _1135 = _601 + 1;
             int32_t _1136 = _1134 * _1135;
             int32_t _1137 = _1133 + _1136;
             int32_t _1138 = _1132 - _349;
             int32_t _1139 = _f3_s0_y - _270;
             int32_t _1140 = _407 + 1;
             int32_t _1141 = _1139 * _1140;
             int32_t _1142 = _1138 + _1141;
             float _1143 = _f0[_1142];
             float _1144 = _1143 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1145 = _f3_s0_y + 1;
             int32_t _1146 = min(_1145, 8191);
             int32_t _1147 = max(_1146, 0);
             int32_t _1148 = _1147 - _270;
             int32_t _1149 = _1148 * _1140;
             int32_t _1150 = _1138 + _1149;
             float _1151 = _f0[_1150];
             float _1152 = _1151 * float_from_bits(1048576000 /* 0.25 */);
             float _1153 = _1144 + _1152;
             int32_t _1154 = _f3_s0_y + -1;
             int32_t _1155 = min(_1154, 8191);
             int32_t _1156 = max(_1155, 0);
             int32_t _1157 = _1156 - _270;
             int32_t _1158 = _1157 * _1140;
             int32_t _1159 = _1138 + _1158;
             float _1160 = _f0[_1159];
             float _1161 = _1160 * float_from_bits(1048576000 /* 0.25 */);
             float _1162 = _1153 + _1161;
             _f3[_1137] = _1162;
             int32_t _1163 = _1132 - _527;
             int32_t _1164 = _f3_s0_y - _445;
             int32_t _1165 = _601 + 1;
             int32_t _1166 = _1164 * _1165;
             int32_t _1167 = _1163 + _1166;
             int32_t _1168 = _1167 + 1;
             int32_t _1169 = _1132 - _349;
             int32_t _1170 = _f3_s0_y - _270;
             int32_t _1171 = _407 + 1;
             int32_t _1172 = _1170 * _1171;
             int32_t _1173 = _1169 + _1172;
             int32_t _1174 = _1173 + 1;
             float _1175 = _f0[_1174];
             float _1176 = _1175 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1177 = _f3_s0_y + 1;
             int32_t _1178 = min(_1177, 8191);
             int32_t _1179 = max(_1178, 0);
             int32_t _1180 = _1179 - _270;
             int32_t _1181 = _1180 * _1171;
             int32_t _1182 = _1169 + _1181;
             int32_t _1183 = _1182 + 1;
             float _1184 = _f0[_1183];
             float _1185 = _1184 * float_from_bits(1048576000 /* 0.25 */);
             float _1186 = _1176 + _1185;
             int32_t _1187 = _f3_s0_y + -1;
             int32_t _1188 = min(_1187, 8191);
             int32_t _1189 = max(_1188, 0);
             int32_t _1190 = _1189 - _270;
             int32_t _1191 = _1190 * _1171;
             int32_t _1192 = _1169 + _1191;
             int32_t _1193 = _1192 + 1;
             float _1194 = _f0[_1193];
             float _1195 = _1194 * float_from_bits(1048576000 /* 0.25 */);
             float _1196 = _1186 + _1195;
             _f3[_1168] = _1196;
             int32_t _1197 = _1132 - _527;
             int32_t _1198 = _f3_s0_y - _445;
             int32_t _1199 = _601 + 1;
             int32_t _1200 = _1198 * _1199;
             int32_t _1201 = _1197 + _1200;
             int32_t _1202 = _1201 + 2;
             int32_t _1203 = _1132 - _349;
             int32_t _1204 = _f3_s0_y - _270;
             int32_t _1205 = _407 + 1;
             int32_t _1206 = _1204 * _1205;
             int32_t _1207 = _1203 + _1206;
             int32_t _1208 = _1207 + 2;
             float _1209 = _f0[_1208];
             float _1210 = _1209 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1211 = _f3_s0_y + 1;
             int32_t _1212 = min(_1211, 8191);
             int32_t _1213 = max(_1212, 0);
             int32_t _1214 = _1213 - _270;
             int32_t _1215 = _1214 * _1205;
             int32_t _1216 = _1203 + _1215;
             int32_t _1217 = _1216 + 2;
             float _1218 = _f0[_1217];
             float _1219 = _1218 * float_from_bits(1048576000 /* 0.25 */);
             float _1220 = _1210 + _1219;
             int32_t _1221 = _f3_s0_y + -1;
             int32_t _1222 = min(_1221, 8191);
             int32_t _1223 = max(_1222, 0);
             int32_t _1224 = _1223 - _270;
             int32_t _1225 = _1224 * _1205;
             int32_t _1226 = _1203 + _1225;
             int32_t _1227 = _1226 + 2;
             float _1228 = _f0[_1227];
             float _1229 = _1228 * float_from_bits(1048576000 /* 0.25 */);
             float _1230 = _1220 + _1229;
             _f3[_1202] = _1230;
             int32_t _1231 = _1132 - _527;
             int32_t _1232 = _f3_s0_y - _445;
             int32_t _1233 = _601 + 1;
             int32_t _1234 = _1232 * _1233;
             int32_t _1235 = _1231 + _1234;
             int32_t _1236 = _1235 + 3;
             int32_t _1237 = _1132 - _349;
             int32_t _1238 = _f3_s0_y - _270;
             int32_t _1239 = _407 + 1;
             int32_t _1240 = _1238 * _1239;
             int32_t _1241 = _1237 + _1240;
             int32_t _1242 = _1241 + 3;
             float _1243 = _f0[_1242];
             float _1244 = _1243 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1245 = _f3_s0_y + 1;
             int32_t _1246 = min(_1245, 8191);
             int32_t _1247 = max(_1246, 0);
             int32_t _1248 = _1247 - _270;
             int32_t _1249 = _1248 * _1239;
             int32_t _1250 = _1237 + _1249;
             int32_t _1251 = _1250 + 3;
             float _1252 = _f0[_1251];
             float _1253 = _1252 * float_from_bits(1048576000 /* 0.25 */);
             float _1254 = _1244 + _1253;
             int32_t _1255 = _f3_s0_y + -1;
             int32_t _1256 = min(_1255, 8191);
             int32_t _1257 = max(_1256, 0);
             int32_t _1258 = _1257 - _270;
             int32_t _1259 = _1258 * _1239;
             int32_t _1260 = _1237 + _1259;
             int32_t _1261 = _1260 + 3;
             float _1262 = _f0[_1261];
             float _1263 = _1262 * float_from_bits(1048576000 /* 0.25 */);
             float _1264 = _1254 + _1263;
             _f3[_1236] = _1264;
            } // for _f3_s0_x_x
           } // for _f3_s0_y
           // consume f3
           // produce f1
           int32_t _1265 = _930 - _947;
           int32_t _1266 = _1265 + 1;
           for (int _f1_s0_y = _947; _f1_s0_y < _947 + _1266; _f1_s0_y++)
           {
            int32_t _1267 = _904 - _922;
            int32_t _1268 = _1267 + 4;
            int32_t _1269 = _1268 >> 2;
            for (int _f1_s0_x_x = 0; _f1_s0_x_x < 0 + _1269; _f1_s0_x_x++)
            {
             int32_t _1270 = _f1_s0_x_x * 4;
             int32_t _1271 = _1270 + _922;
             int32_t _1272 = _904 + -3;
             int32_t _1273 = min(_1271, _1272);
             int32_t _1274 = _1273 - _696;
             int32_t _1275 = _f1_s0_y - _649;
             int32_t _1276 = _730 + 1;
             int32_t _1277 = _1275 * _1276;
             int32_t _1278 = _1274 + _1277;
             int32_t _1279 = _1273 - _527;
             int32_t _1280 = _f1_s0_y - _445;
             int32_t _1281 = _601 + 1;
             int32_t _1282 = _1280 * _1281;
             int32_t _1283 = _1279 + _1282;
             float _1284 = _f3[_1283];
             float _1285 = _1284 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1286 = _1273 + 1;
             int32_t _1287 = min(_1286, 8191);
             int32_t _1288 = max(_1287, 0);
             int32_t _1289 = _1288 - _527;
             int32_t _1290 = _1289 + _1282;
             float _1291 = _f3[_1290];
             float _1292 = _1291 * float_from_bits(1048576000 /* 0.25 */);
             float _1293 = _1285 + _1292;
             int32_t _1294 = _1273 + -1;
             int32_t _1295 = min(_1294, 8191);
             int32_t _1296 = max(_1295, 0);
             int32_t _1297 = _1296 - _527;
             int32_t _1298 = _1297 + _1282;
             float _1299 = _f3[_1298];
             float _1300 = _1299 * float_from_bits(1048576000 /* 0.25 */);
             float _1301 = _1293 + _1300;
             _f1[_1278] = _1301;
             int32_t _1302 = _1273 - _696;
             int32_t _1303 = _f1_s0_y - _649;
             int32_t _1304 = _730 + 1;
             int32_t _1305 = _1303 * _1304;
             int32_t _1306 = _1302 + _1305;
             int32_t _1307 = _1306 + 1;
             int32_t _1308 = _1273 - _527;
             int32_t _1309 = _f1_s0_y - _445;
             int32_t _1310 = _601 + 1;
             int32_t _1311 = _1309 * _1310;
             int32_t _1312 = _1308 + _1311;
             int32_t _1313 = _1312 + 1;
             float _1314 = _f3[_1313];
             float _1315 = _1314 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1316 = _1273 + 2;
             int32_t _1317 = min(_1316, 8191);
             int32_t _1318 = max(_1317, 0);
             int32_t _1319 = _1318 - _527;
             int32_t _1320 = _1319 + _1311;
             float _1321 = _f3[_1320];
             float _1322 = _1321 * float_from_bits(1048576000 /* 0.25 */);
             float _1323 = _1315 + _1322;
             int32_t _1324 = min(_1273, 8191);
             int32_t _1325 = max(_1324, 0);
             int32_t _1326 = _1325 - _527;
             int32_t _1327 = _1326 + _1311;
             float _1328 = _f3[_1327];
             float _1329 = _1328 * float_from_bits(1048576000 /* 0.25 */);
             float _1330 = _1323 + _1329;
             _f1[_1307] = _1330;
             int32_t _1331 = _1273 - _696;
             int32_t _1332 = _f1_s0_y - _649;
             int32_t _1333 = _730 + 1;
             int32_t _1334 = _1332 * _1333;
             int32_t _1335 = _1331 + _1334;
             int32_t _1336 = _1335 + 2;
             int32_t _1337 = _1273 - _527;
             int32_t _1338 = _f1_s0_y - _445;
             int32_t _1339 = _601 + 1;
             int32_t _1340 = _1338 * _1339;
             int32_t _1341 = _1337 + _1340;
             int32_t _1342 = _1341 + 2;
             float _1343 = _f3[_1342];
             float _1344 = _1343 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1345 = _1273 + 3;
             int32_t _1346 = min(_1345, 8191);
             int32_t _1347 = max(_1346, 0);
             int32_t _1348 = _1347 - _527;
             int32_t _1349 = _1348 + _1340;
             float _1350 = _f3[_1349];
             float _1351 = _1350 * float_from_bits(1048576000 /* 0.25 */);
             float _1352 = _1344 + _1351;
             int32_t _1353 = _1273 + 1;
             int32_t _1354 = min(_1353, 8191);
             int32_t _1355 = max(_1354, 0);
             int32_t _1356 = _1355 - _527;
             int32_t _1357 = _1356 + _1340;
             float _1358 = _f3[_1357];
             float _1359 = _1358 * float_from_bits(1048576000 /* 0.25 */);
             float _1360 = _1352 + _1359;
             _f1[_1336] = _1360;
             int32_t _1361 = _1273 - _696;
             int32_t _1362 = _f1_s0_y - _649;
             int32_t _1363 = _730 + 1;
             int32_t _1364 = _1362 * _1363;
             int32_t _1365 = _1361 + _1364;
             int32_t _1366 = _1365 + 3;
             int32_t _1367 = _1273 - _527;
             int32_t _1368 = _f1_s0_y - _445;
             int32_t _1369 = _601 + 1;
             int32_t _1370 = _1368 * _1369;
             int32_t _1371 = _1367 + _1370;
             int32_t _1372 = _1371 + 3;
             float _1373 = _f3[_1372];
             float _1374 = _1373 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1375 = _1273 + 4;
             int32_t _1376 = min(_1375, 8191);
             int32_t _1377 = max(_1376, 0);
             int32_t _1378 = _1377 - _527;
             int32_t _1379 = _1378 + _1370;
             float _1380 = _f3[_1379];
             float _1381 = _1380 * float_from_bits(1048576000 /* 0.25 */);
             float _1382 = _1374 + _1381;
             int32_t _1383 = _1273 + 2;
             int32_t _1384 = min(_1383, 8191);
             int32_t _1385 = max(_1384, 0);
             int32_t _1386 = _1385 - _527;
             int32_t _1387 = _1386 + _1370;
             float _1388 = _f3[_1387];
             float _1389 = _1388 * float_from_bits(1048576000 /* 0.25 */);
             float _1390 = _1382 + _1389;
             _f1[_1366] = _1390;
            } // for _f1_s0_x_x
           } // for _f1_s0_y
           // consume f1
           // produce f4
           int32_t _1391 = _883 - _895;
           int32_t _1392 = _1391 + 1;
           for (int _f4_s0_y = _895; _f4_s0_y < _895 + _1392; _f4_s0_y++)
           {
            int32_t _1393 = _904 - _922;
            int32_t _1394 = _1393 + 4;
            int32_t _1395 = _1394 >> 2;
            for (int _f4_s0_x_x = 0; _f4_s0_x_x < 0 + _1395; _f4_s0_x_x++)
            {
             int32_t _1396 = _f4_s0_x_x * 4;
             int32_t _1397 = _1396 + _922;
             int32_t _1398 = _904 + -3;
             int32_t _1399 = min(_1397, _1398);
             int32_t _1400 = _1399 - _793;
             int32_t _1401 = _f4_s0_y - _752;
             int32_t _1402 = _830 + 1;
             int32_t _1403 = _1401 * _1402;
             int32_t _1404 = _1400 + _1403;
             int32_t _1405 = _1399 - _696;
             int32_t _1406 = _f4_s0_y - _649;
             int32_t _1407 = _730 + 1;
             int32_t _1408 = _1406 * _1407;
             int32_t _1409 = _1405 + _1408;
             float _1410 = _f1[_1409];
             float _1411 = _1410 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1412 = _f4_s0_y + 1;
             int32_t _1413 = min(_1412, 8191);
             int32_t _1414 = max(_1413, 0);
             int32_t _1415 = _1414 - _649;
             int32_t _1416 = _1415 * _1407;
             int32_t _1417 = _1405 + _1416;
             float _1418 = _f1[_1417];
             float _1419 = _1418 * float_from_bits(1048576000 /* 0.25 */);
             float _1420 = _1411 + _1419;
             int32_t _1421 = _f4_s0_y + -1;
             int32_t _1422 = min(_1421, 8191);
             int32_t _1423 = max(_1422, 0);
             int32_t _1424 = _1423 - _649;
             int32_t _1425 = _1424 * _1407;
             int32_t _1426 = _1405 + _1425;
             float _1427 = _f1[_1426];
             float _1428 = _1427 * float_from_bits(1048576000 /* 0.25 */);
             float _1429 = _1420 + _1428;
             _f4[_1404] = _1429;
             int32_t _1430 = _1399 - _793;
             int32_t _1431 = _f4_s0_y - _752;
             int32_t _1432 = _830 + 1;
             int32_t _1433 = _1431 * _1432;
             int32_t _1434 = _1430 + _1433;
             int32_t _1435 = _1434 + 1;
             int32_t _1436 = _1399 - _696;
             int32_t _1437 = _f4_s0_y - _649;
             int32_t _1438 = _730 + 1;
             int32_t _1439 = _1437 * _1438;
             int32_t _1440 = _1436 + _1439;
             int32_t _1441 = _1440 + 1;
             float _1442 = _f1[_1441];
             float _1443 = _1442 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1444 = _f4_s0_y + 1;
             int32_t _1445 = min(_1444, 8191);
             int32_t _1446 = max(_1445, 0);
             int32_t _1447 = _1446 - _649;
             int32_t _1448 = _1447 * _1438;
             int32_t _1449 = _1436 + _1448;
             int32_t _1450 = _1449 + 1;
             float _1451 = _f1[_1450];
             float _1452 = _1451 * float_from_bits(1048576000 /* 0.25 */);
             float _1453 = _1443 + _1452;
             int32_t _1454 = _f4_s0_y + -1;
             int32_t _1455 = min(_1454, 8191);
             int32_t _1456 = max(_1455, 0);
             int32_t _1457 = _1456 - _649;
             int32_t _1458 = _1457 * _1438;
             int32_t _1459 = _1436 + _1458;
             int32_t _1460 = _1459 + 1;
             float _1461 = _f1[_1460];
             float _1462 = _1461 * float_from_bits(1048576000 /* 0.25 */);
             float _1463 = _1453 + _1462;
             _f4[_1435] = _1463;
             int32_t _1464 = _1399 - _793;
             int32_t _1465 = _f4_s0_y - _752;
             int32_t _1466 = _830 + 1;
             int32_t _1467 = _1465 * _1466;
             int32_t _1468 = _1464 + _1467;
             int32_t _1469 = _1468 + 2;
             int32_t _1470 = _1399 - _696;
             int32_t _1471 = _f4_s0_y - _649;
             int32_t _1472 = _730 + 1;
             int32_t _1473 = _1471 * _1472;
             int32_t _1474 = _1470 + _1473;
             int32_t _1475 = _1474 + 2;
             float _1476 = _f1[_1475];
             float _1477 = _1476 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1478 = _f4_s0_y + 1;
             int32_t _1479 = min(_1478, 8191);
             int32_t _1480 = max(_1479, 0);
             int32_t _1481 = _1480 - _649;
             int32_t _1482 = _1481 * _1472;
             int32_t _1483 = _1470 + _1482;
             int32_t _1484 = _1483 + 2;
             float _1485 = _f1[_1484];
             float _1486 = _1485 * float_from_bits(1048576000 /* 0.25 */);
             float _1487 = _1477 + _1486;
             int32_t _1488 = _f4_s0_y + -1;
             int32_t _1489 = min(_1488, 8191);
             int32_t _1490 = max(_1489, 0);
             int32_t _1491 = _1490 - _649;
             int32_t _1492 = _1491 * _1472;
             int32_t _1493 = _1470 + _1492;
             int32_t _1494 = _1493 + 2;
             float _1495 = _f1[_1494];
             float _1496 = _1495 * float_from_bits(1048576000 /* 0.25 */);
             float _1497 = _1487 + _1496;
             _f4[_1469] = _1497;
             int32_t _1498 = _1399 - _793;
             int32_t _1499 = _f4_s0_y - _752;
             int32_t _1500 = _830 + 1;
             int32_t _1501 = _1499 * _1500;
             int32_t _1502 = _1498 + _1501;
             int32_t _1503 = _1502 + 3;
             int32_t _1504 = _1399 - _696;
             int32_t _1505 = _f4_s0_y - _649;
             int32_t _1506 = _730 + 1;
             int32_t _1507 = _1505 * _1506;
             int32_t _1508 = _1504 + _1507;
             int32_t _1509 = _1508 + 3;
             float _1510 = _f1[_1509];
             float _1511 = _1510 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1512 = _f4_s0_y + 1;
             int32_t _1513 = min(_1512, 8191);
             int32_t _1514 = max(_1513, 0);
             int32_t _1515 = _1514 - _649;
             int32_t _1516 = _1515 * _1506;
             int32_t _1517 = _1504 + _1516;
             int32_t _1518 = _1517 + 3;
             float _1519 = _f1[_1518];
             float _1520 = _1519 * float_from_bits(1048576000 /* 0.25 */);
             float _1521 = _1511 + _1520;
             int32_t _1522 = _f4_s0_y + -1;
             int32_t _1523 = min(_1522, 8191);
             int32_t _1524 = max(_1523, 0);
             int32_t _1525 = _1524 - _649;
             int32_t _1526 = _1525 * _1506;
             int32_t _1527 = _1504 + _1526;
             int32_t _1528 = _1527 + 3;
             float _1529 = _f1[_1528];
             float _1530 = _1529 * float_from_bits(1048576000 /* 0.25 */);
             float _1531 = _1521 + _1530;
             _f4[_1503] = _1531;
            } // for _f4_s0_x_x
           } // for _f4_s0_y
           // consume f4
           // produce f2
           int32_t _1532 = _883 - _895;
           int32_t _1533 = _1532 + 1;
           for (int _f2_s0_y = _895; _f2_s0_y < _895 + _1533; _f2_s0_y++)
           {
            for (int _f2_s0_x_x = 0; _f2_s0_x_x < 0 + 1; _f2_s0_x_x++)
            {
             int32_t _1534 = _f2_s0_x_x * 4;
             int32_t _1535 = _1534 + _875;
             int32_t _1536 = min(_1535, _875);
             int32_t _1537 = _1536 - _211;
             int32_t _1538 = _f2_s0_y - _853;
             int32_t _1539 = _1538 * 32;
             int32_t _1540 = _1537 + _1539;
             int32_t _1541 = _1536 - _793;
             int32_t _1542 = _f2_s0_y - _752;
             int32_t _1543 = _830 + 1;
             int32_t _1544 = _1542 * _1543;
             int32_t _1545 = _1541 + _1544;
             float _1546 = _f4[_1545];
             float _1547 = _1546 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1548 = _1536 + 1;
             int32_t _1549 = min(_1548, 8191);
             int32_t _1550 = max(_1549, 0);
             int32_t _1551 = _1550 - _793;
             int32_t _1552 = _1551 + _1544;
             float _1553 = _f4[_1552];
             float _1554 = _1553 * float_from_bits(1048576000 /* 0.25 */);
             float _1555 = _1547 + _1554;
             int32_t _1556 = _1536 + -1;
             int32_t _1557 = min(_1556, 8191);
             int32_t _1558 = max(_1557, 0);
             int32_t _1559 = _1558 - _793;
             int32_t _1560 = _1559 + _1544;
             float _1561 = _f4[_1560];
             float _1562 = _1561 * float_from_bits(1048576000 /* 0.25 */);
             float _1563 = _1555 + _1562;
             _f2[_1540] = _1563;
             int32_t _1564 = _1536 - _211;
             int32_t _1565 = _f2_s0_y - _853;
             int32_t _1566 = _1565 * 32;
             int32_t _1567 = _1564 + _1566;
             int32_t _1568 = _1567 + 1;
             int32_t _1569 = _1536 - _793;
             int32_t _1570 = _f2_s0_y - _752;
             int32_t _1571 = _830 + 1;
             int32_t _1572 = _1570 * _1571;
             int32_t _1573 = _1569 + _1572;
             int32_t _1574 = _1573 + 1;
             float _1575 = _f4[_1574];
             float _1576 = _1575 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1577 = _1536 + 2;
             int32_t _1578 = min(_1577, 8191);
             int32_t _1579 = max(_1578, 0);
             int32_t _1580 = _1579 - _793;
             int32_t _1581 = _1580 + _1572;
             float _1582 = _f4[_1581];
             float _1583 = _1582 * float_from_bits(1048576000 /* 0.25 */);
             float _1584 = _1576 + _1583;
             int32_t _1585 = min(_1536, 8191);
             int32_t _1586 = max(_1585, 0);
             int32_t _1587 = _1586 - _793;
             int32_t _1588 = _1587 + _1572;
             float _1589 = _f4[_1588];
             float _1590 = _1589 * float_from_bits(1048576000 /* 0.25 */);
             float _1591 = _1584 + _1590;
             _f2[_1568] = _1591;
             int32_t _1592 = _1536 - _211;
             int32_t _1593 = _f2_s0_y - _853;
             int32_t _1594 = _1593 * 32;
             int32_t _1595 = _1592 + _1594;
             int32_t _1596 = _1595 + 2;
             int32_t _1597 = _1536 - _793;
             int32_t _1598 = _f2_s0_y - _752;
             int32_t _1599 = _830 + 1;
             int32_t _1600 = _1598 * _1599;
             int32_t _1601 = _1597 + _1600;
             int32_t _1602 = _1601 + 2;
             float _1603 = _f4[_1602];
             float _1604 = _1603 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1605 = _1536 + 3;
             int32_t _1606 = min(_1605, 8191);
             int32_t _1607 = max(_1606, 0);
             int32_t _1608 = _1607 - _793;
             int32_t _1609 = _1608 + _1600;
             float _1610 = _f4[_1609];
             float _1611 = _1610 * float_from_bits(1048576000 /* 0.25 */);
             float _1612 = _1604 + _1611;
             int32_t _1613 = _1536 + 1;
             int32_t _1614 = min(_1613, 8191);
             int32_t _1615 = max(_1614, 0);
             int32_t _1616 = _1615 - _793;
             int32_t _1617 = _1616 + _1600;
             float _1618 = _f4[_1617];
             float _1619 = _1618 * float_from_bits(1048576000 /* 0.25 */);
             float _1620 = _1612 + _1619;
             _f2[_1596] = _1620;
             int32_t _1621 = _1536 - _211;
             int32_t _1622 = _f2_s0_y - _853;
             int32_t _1623 = _1622 * 32;
             int32_t _1624 = _1621 + _1623;
             int32_t _1625 = _1624 + 3;
             int32_t _1626 = _1536 - _793;
             int32_t _1627 = _f2_s0_y - _752;
             int32_t _1628 = _830 + 1;
             int32_t _1629 = _1627 * _1628;
             int32_t _1630 = _1626 + _1629;
             int32_t _1631 = _1630 + 3;
             float _1632 = _f4[_1631];
             float _1633 = _1632 * float_from_bits(1056964608 /* 0.5 */);
             int32_t _1634 = _1536 + 4;
             int32_t _1635 = min(_1634, 8191);
             int32_t _1636 = max(_1635, 0);
             int32_t _1637 = _1636 - _793;
             int32_t _1638 = _1637 + _1629;
             float _1639 = _f4[_1638];
             float _1640 = _1639 * float_from_bits(1048576000 /* 0.25 */);
             float _1641 = _1633 + _1640;
             int32_t _1642 = _1536 + 2;
             int32_t _1643 = min(_1642, 8191);
             int32_t _1644 = max(_1643, 0);
             int32_t _1645 = _1644 - _793;
             int32_t _1646 = _1645 + _1629;
             float _1647 = _f4[_1646];
             float _1648 = _1647 * float_from_bits(1048576000 /* 0.25 */);
             float _1649 = _1641 + _1648;
             _f2[_1625] = _1649;
            } // for _f2_s0_x_x
           } // for _f2_s0_y
           // consume f2
           int32_t _1650 = _f5_s0_x_xi_xi * 32;
           int32_t _1651 = _f5_s0_x_xi_xii_xii * 4;
           int32_t _1652 = _1650 + _1651;
           int32_t _1653 = _201 + _1652;
           int32_t _1654 = _1653 - _f5_min_0;
           int32_t _1655 = _871 - _f5_min_1;
           int32_t _1656 = _1655 * _f5_stride_1;
           int32_t _1657 = _1654 + _1656;
           int32_t _1658 = _1653 - _211;
           int32_t _1659 = _871 - _853;
           int32_t _1660 = _1659 * 32;
           int32_t _1661 = _1658 + _1660;
           float _1662 = _f2[_1661];
           float _1663 = _1662 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1664 = _871 + 1;
           int32_t _1665 = min(_1664, 8191);
           int32_t _1666 = max(_1665, 0);
           int32_t _1667 = _1666 - _853;
           int32_t _1668 = _1667 * 32;
           int32_t _1669 = _1658 + _1668;
           float _1670 = _f2[_1669];
           float _1671 = _1670 * float_from_bits(1048576000 /* 0.25 */);
           float _1672 = _1663 + _1671;
           int32_t _1673 = _871 + -1;
           int32_t _1674 = min(_1673, 8191);
           int32_t _1675 = max(_1674, 0);
           int32_t _1676 = _1675 - _853;
           int32_t _1677 = _1676 * 32;
           int32_t _1678 = _1658 + _1677;
           float _1679 = _f2[_1678];
           float _1680 = _1679 * float_from_bits(1048576000 /* 0.25 */);
           float _1681 = _1672 + _1680;
           _f5[_1657] = _1681;
           int32_t _1682 = _f5_s0_x_xi_xi * 32;
           int32_t _1683 = _f5_s0_x_xi_xii_xii * 4;
           int32_t _1684 = _1682 + _1683;
           int32_t _1685 = _201 + _1684;
           int32_t _1686 = _1685 - _f5_min_0;
           int32_t _1687 = _871 - _f5_min_1;
           int32_t _1688 = _1687 * _f5_stride_1;
           int32_t _1689 = _1686 + _1688;
           int32_t _1690 = _1689 + 1;
           int32_t _1691 = _1685 - _211;
           int32_t _1692 = _871 - _853;
           int32_t _1693 = _1692 * 32;
           int32_t _1694 = _1691 + _1693;
           int32_t _1695 = _1694 + 1;
           float _1696 = _f2[_1695];
           float _1697 = _1696 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1698 = _871 + 1;
           int32_t _1699 = min(_1698, 8191);
           int32_t _1700 = max(_1699, 0);
           int32_t _1701 = _1700 - _853;
           int32_t _1702 = _1701 * 32;
           int32_t _1703 = _1691 + _1702;
           int32_t _1704 = _1703 + 1;
           float _1705 = _f2[_1704];
           float _1706 = _1705 * float_from_bits(1048576000 /* 0.25 */);
           float _1707 = _1697 + _1706;
           int32_t _1708 = _871 + -1;
           int32_t _1709 = min(_1708, 8191);
           int32_t _1710 = max(_1709, 0);
           int32_t _1711 = _1710 - _853;
           int32_t _1712 = _1711 * 32;
           int32_t _1713 = _1691 + _1712;
           int32_t _1714 = _1713 + 1;
           float _1715 = _f2[_1714];
           float _1716 = _1715 * float_from_bits(1048576000 /* 0.25 */);
           float _1717 = _1707 + _1716;
           _f5[_1690] = _1717;
           int32_t _1718 = _f5_s0_x_xi_xi * 32;
           int32_t _1719 = _f5_s0_x_xi_xii_xii * 4;
           int32_t _1720 = _1718 + _1719;
           int32_t _1721 = _201 + _1720;
           int32_t _1722 = _1721 - _f5_min_0;
           int32_t _1723 = _871 - _f5_min_1;
           int32_t _1724 = _1723 * _f5_stride_1;
           int32_t _1725 = _1722 + _1724;
           int32_t _1726 = _1725 + 2;
           int32_t _1727 = _1721 - _211;
           int32_t _1728 = _871 - _853;
           int32_t _1729 = _1728 * 32;
           int32_t _1730 = _1727 + _1729;
           int32_t _1731 = _1730 + 2;
           float _1732 = _f2[_1731];
           float _1733 = _1732 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1734 = _871 + 1;
           int32_t _1735 = min(_1734, 8191);
           int32_t _1736 = max(_1735, 0);
           int32_t _1737 = _1736 - _853;
           int32_t _1738 = _1737 * 32;
           int32_t _1739 = _1727 + _1738;
           int32_t _1740 = _1739 + 2;
           float _1741 = _f2[_1740];
           float _1742 = _1741 * float_from_bits(1048576000 /* 0.25 */);
           float _1743 = _1733 + _1742;
           int32_t _1744 = _871 + -1;
           int32_t _1745 = min(_1744, 8191);
           int32_t _1746 = max(_1745, 0);
           int32_t _1747 = _1746 - _853;
           int32_t _1748 = _1747 * 32;
           int32_t _1749 = _1727 + _1748;
           int32_t _1750 = _1749 + 2;
           float _1751 = _f2[_1750];
           float _1752 = _1751 * float_from_bits(1048576000 /* 0.25 */);
           float _1753 = _1743 + _1752;
           _f5[_1726] = _1753;
           int32_t _1754 = _f5_s0_x_xi_xi * 32;
           int32_t _1755 = _f5_s0_x_xi_xii_xii * 4;
           int32_t _1756 = _1754 + _1755;
           int32_t _1757 = _201 + _1756;
           int32_t _1758 = _1757 - _f5_min_0;
           int32_t _1759 = _871 - _f5_min_1;
           int32_t _1760 = _1759 * _f5_stride_1;
           int32_t _1761 = _1758 + _1760;
           int32_t _1762 = _1761 + 3;
           int32_t _1763 = _1757 - _211;
           int32_t _1764 = _871 - _853;
           int32_t _1765 = _1764 * 32;
           int32_t _1766 = _1763 + _1765;
           int32_t _1767 = _1766 + 3;
           float _1768 = _f2[_1767];
           float _1769 = _1768 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1770 = _871 + 1;
           int32_t _1771 = min(_1770, 8191);
           int32_t _1772 = max(_1771, 0);
           int32_t _1773 = _1772 - _853;
           int32_t _1774 = _1773 * 32;
           int32_t _1775 = _1763 + _1774;
           int32_t _1776 = _1775 + 3;
           float _1777 = _f2[_1776];
           float _1778 = _1777 * float_from_bits(1048576000 /* 0.25 */);
           float _1779 = _1769 + _1778;
           int32_t _1780 = _871 + -1;
           int32_t _1781 = min(_1780, 8191);
           int32_t _1782 = max(_1781, 0);
           int32_t _1783 = _1782 - _853;
           int32_t _1784 = _1783 * 32;
           int32_t _1785 = _1763 + _1784;
           int32_t _1786 = _1785 + 3;
           float _1787 = _f2[_1786];
           float _1788 = _1787 * float_from_bits(1048576000 /* 0.25 */);
           float _1789 = _1779 + _1788;
           _f5[_1762] = _1789;
          } // for _f5_s0_x_xi_xii_xii
         } // for _f5_s0_y_yi_yii
         halide_free(NULL, _f0);
         halide_free(NULL, _f3);
         halide_free(NULL, _f1);
         halide_free(NULL, _f4);
         halide_free(NULL, _f2);
        } // alloc _f2
       } // alloc _f4
      } // alloc _f1
     } // alloc _f3
    } // alloc _f0
   } // for _f5_s0_x_xi_xi
  } // for _f5_s0_y_yi_yi
 } // for _f5_s0_x_xo_nid
 // consume f5
} // if _140
return 0;
}

int main(){
  main_compute((buffer_t *)tmp, (buffer_t *)tmp) ;
}
 
