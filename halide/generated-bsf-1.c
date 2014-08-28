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


extern "C" int main_compute(buffer_t *_inPar_buffer, buffer_t *_f7_buffer) {
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
float *_f7 = (float *)(_f7_buffer->host);
const bool _f7_host_and_dev_are_null = (_f7_buffer->host == NULL) && (_f7_buffer->dev == 0);
(void)_f7_host_and_dev_are_null;
const int32_t _f7_min_0 = _f7_buffer->min[0];
(void)_f7_min_0;
const int32_t _f7_min_1 = _f7_buffer->min[1];
(void)_f7_min_1;
const int32_t _f7_min_2 = _f7_buffer->min[2];
(void)_f7_min_2;
const int32_t _f7_min_3 = _f7_buffer->min[3];
(void)_f7_min_3;
const int32_t _f7_extent_0 = _f7_buffer->extent[0];
(void)_f7_extent_0;
const int32_t _f7_extent_1 = _f7_buffer->extent[1];
(void)_f7_extent_1;
const int32_t _f7_extent_2 = _f7_buffer->extent[2];
(void)_f7_extent_2;
const int32_t _f7_extent_3 = _f7_buffer->extent[3];
(void)_f7_extent_3;
const int32_t _f7_stride_0 = _f7_buffer->stride[0];
(void)_f7_stride_0;
const int32_t _f7_stride_1 = _f7_buffer->stride[1];
(void)_f7_stride_1;
const int32_t _f7_stride_2 = _f7_buffer->stride[2];
(void)_f7_stride_2;
const int32_t _f7_stride_3 = _f7_buffer->stride[3];
(void)_f7_stride_3;
const int32_t _f7_elem_size = _f7_buffer->elem_size;
int32_t _0 = _f7_min_1 + _f7_extent_1;
int32_t _1 = _0 + -1;
int32_t _2 = min(_0, 2047);
int32_t _3 = max(_2, 0);
int32_t _4 = max(_1, _3);
int32_t _5 = _0 + -2;
int32_t _6 = min(_5, 2047);
int32_t _7 = max(_6, 0);
int32_t _8 = max(_4, _7);
int32_t _9 = _f7_min_1 + 1;
int32_t _10 = min(_9, 2047);
int32_t _11 = max(_10, 0);
int32_t _12 = min(_f7_min_1, _11);
int32_t _13 = _f7_min_1 + -1;
int32_t _14 = min(_13, 2047);
int32_t _15 = max(_14, 0);
int32_t _16 = min(_12, _15);
int32_t _17 = _f7_min_0 + _f7_extent_0;
int32_t _18 = _17 + -1;
int32_t _19 = min(_17, 2047);
int32_t _20 = max(_19, 0);
int32_t _21 = max(_18, _20);
int32_t _22 = _17 + -2;
int32_t _23 = min(_22, 2047);
int32_t _24 = max(_23, 0);
int32_t _25 = max(_21, _24);
int32_t _26 = _f7_min_0 + 1;
int32_t _27 = min(_26, 2047);
int32_t _28 = max(_27, 0);
int32_t _29 = min(_f7_min_0, _28);
int32_t _30 = _f7_min_0 + -1;
int32_t _31 = min(_30, 2047);
int32_t _32 = max(_31, 0);
int32_t _33 = min(_29, _32);
int32_t _34 = _8 + 1;
int32_t _35 = min(_34, 2047);
int32_t _36 = max(_35, 0);
int32_t _37 = max(_8, _36);
int32_t _38 = _8 + -1;
int32_t _39 = min(_38, 2047);
int32_t _40 = max(_39, 0);
int32_t _41 = max(_37, _40);
int32_t _42 = _16 + 1;
int32_t _43 = min(_42, 2047);
int32_t _44 = max(_43, 0);
int32_t _45 = min(_16, _44);
int32_t _46 = _16 + -1;
int32_t _47 = min(_46, 2047);
int32_t _48 = max(_47, 0);
int32_t _49 = min(_45, _48);
int32_t _50 = _25 + 1;
int32_t _51 = min(_50, 2047);
int32_t _52 = max(_51, 0);
int32_t _53 = max(_25, _52);
int32_t _54 = _25 + -1;
int32_t _55 = min(_54, 2047);
int32_t _56 = max(_55, 0);
int32_t _57 = max(_53, _56);
int32_t _58 = _33 + 1;
int32_t _59 = min(_58, 2047);
int32_t _60 = max(_59, 0);
int32_t _61 = min(_33, _60);
int32_t _62 = _33 + -1;
int32_t _63 = min(_62, 2047);
int32_t _64 = max(_63, 0);
int32_t _65 = min(_61, _64);
int32_t _66 = _41 + 1;
int32_t _67 = min(_66, 2047);
int32_t _68 = max(_67, 0);
int32_t _69 = max(_41, _68);
int32_t _70 = _41 + -1;
int32_t _71 = min(_70, 2047);
int32_t _72 = max(_71, 0);
int32_t _73 = max(_69, _72);
int32_t _74 = _49 + 1;
int32_t _75 = min(_74, 2047);
int32_t _76 = max(_75, 0);
int32_t _77 = min(_49, _76);
int32_t _78 = _49 + -1;
int32_t _79 = min(_78, 2047);
int32_t _80 = max(_79, 0);
int32_t _81 = min(_77, _80);
int32_t _82 = _57 + 1;
int32_t _83 = min(_82, 2047);
int32_t _84 = max(_83, 0);
int32_t _85 = max(_57, _84);
int32_t _86 = _57 + -1;
int32_t _87 = min(_86, 2047);
int32_t _88 = max(_87, 0);
int32_t _89 = max(_85, _88);
int32_t _90 = _65 + 1;
int32_t _91 = min(_90, 2047);
int32_t _92 = max(_91, 0);
int32_t _93 = min(_65, _92);
int32_t _94 = _65 + -1;
int32_t _95 = min(_94, 2047);
int32_t _96 = max(_95, 0);
int32_t _97 = min(_93, _96);
int32_t _98 = _73 + 1;
int32_t _99 = min(_98, 2047);
int32_t _100 = max(_99, 0);
int32_t _101 = max(_73, _100);
int32_t _102 = _73 + -1;
int32_t _103 = min(_102, 2047);
int32_t _104 = max(_103, 0);
int32_t _105 = max(_101, _104);
int32_t _106 = _81 + 1;
int32_t _107 = min(_106, 2047);
int32_t _108 = max(_107, 0);
int32_t _109 = min(_81, _108);
int32_t _110 = _81 + -1;
int32_t _111 = min(_110, 2047);
int32_t _112 = max(_111, 0);
int32_t _113 = min(_109, _112);
int32_t _114 = _f7_extent_0 + -1;
int32_t _115 = _114 >> 6;
int32_t _116 = _115 * 64;
int32_t _117 = _116 + _f7_min_0;
int32_t _118 = _117 + 64;
int32_t _119 = min(_118, _17);
int32_t _120 = _17 + -64;
int32_t _121 = min(_f7_min_0, _120);
int32_t _122 = _119 - _121;
int32_t _123 = _f7_extent_0 + 63;
int32_t _124 = _123 >> 6;
int32_t _125 = _f7_extent_1 + 63;
int32_t _126 = _125 >> 6;
int32_t _127 = _124 * _126;
int32_t _128 = _127 + -1;
int32_t _129 = sdiv(_128, _124);
int32_t _130 = max(_129, 0);
int32_t _131 = _130 * 64;
int32_t _132 = _131 + _f7_min_1;
int32_t _133 = _132 + 64;
int32_t _134 = min(_133, _0);
int32_t _135 = min(_129, 0);
int32_t _136 = _135 * 64;
int32_t _137 = _136 + _f7_min_1;
int32_t _138 = _0 + -64;
int32_t _139 = min(_137, _138);
int32_t _140 = _134 - _139;
int32_t _141 = _89 - _97;
int32_t _142 = _141 >> 2;
int32_t _143 = _142 * 4;
int32_t _144 = _143 + _97;
int32_t _145 = _144 + 3;
int32_t _146 = min(_145, _89);
int32_t _147 = _146 + 1;
int32_t _148 = min(_147, 2047);
int32_t _149 = max(_148, 0);
int32_t _150 = max(_146, _149);
int32_t _151 = _146 + -1;
int32_t _152 = min(_151, 2047);
int32_t _153 = max(_152, 0);
int32_t _154 = max(_150, _153);
int32_t _155 = _89 + -3;
int32_t _156 = min(_97, _155);
int32_t _157 = _156 + 1;
int32_t _158 = min(_157, 2047);
int32_t _159 = max(_158, 0);
int32_t _160 = min(_156, _159);
int32_t _161 = _156 + -1;
int32_t _162 = min(_161, 2047);
int32_t _163 = max(_162, 0);
int32_t _164 = min(_160, _163);
int32_t _165 = _154 - _164;
if (_f7_host_and_dev_are_null)
{
 bool _166 = halide_rewrite_buffer(_f7_buffer, 4, _121, _122, 1, _139, _140, _122, 0, 0, 0, 0, 0, 0);
 (void)_166;
} // if _f7_host_and_dev_are_null
if (_inPar_host_and_dev_are_null)
{
 int32_t _167 = _165 + 1;
 int32_t _168 = _105 - _113;
 int32_t _169 = _168 + 1;
 bool _170 = halide_rewrite_buffer(_inPar_buffer, 4, _164, _167, 1, _113, _169, _167, 0, 0, 0, 0, 0, 0);
 (void)_170;
} // if _inPar_host_and_dev_are_null
bool _171 = _f7_host_and_dev_are_null || _inPar_host_and_dev_are_null;
bool _172 = !(_171);
if (_172)
{
 bool _173 = _f7_elem_size == 4;
 if (!_173) {
  halide_printf(NULL, "Output buffer f7 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _f7_elem_size);
  return -1;
 }
 bool _174 = _inPar_elem_size == 4;
 if (!_174) {
  halide_printf(NULL, "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _inPar_elem_size);
  return -1;
 }
 bool _175 = _f7_min_0 <= _121;
 if (!_175) {
  halide_printf(NULL, "Output buffer f7 is accessed at %d, which is before the min (%d) in dimension 0\n", _121, _f7_min_0);
  return -1;
 }
 int32_t _176 = _121 + _122;
 int32_t _177 = _176 - _f7_extent_0;
 bool _178 = _177 <= _f7_min_0;
 int32_t _179 = _176 + -1;
 int32_t _180 = _f7_min_0 + _f7_extent_0;
 int32_t _181 = _180 + -1;
 if (!_178) {
  halide_printf(NULL, "Output buffer f7 is accessed at %d, which is beyond the max (%d) in dimension 0\n", _179, _181);
  return -1;
 }
 bool _182 = _f7_min_1 <= _139;
 if (!_182) {
  halide_printf(NULL, "Output buffer f7 is accessed at %d, which is before the min (%d) in dimension 1\n", _139, _f7_min_1);
  return -1;
 }
 int32_t _183 = _139 + _140;
 int32_t _184 = _183 - _f7_extent_1;
 bool _185 = _184 <= _f7_min_1;
 int32_t _186 = _183 + -1;
 int32_t _187 = _f7_min_1 + _f7_extent_1;
 int32_t _188 = _187 + -1;
 if (!_185) {
  halide_printf(NULL, "Output buffer f7 is accessed at %d, which is beyond the max (%d) in dimension 1\n", _186, _188);
  return -1;
 }
 bool _189 = _inPar_min_0 <= _164;
 if (!_189) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _164, _inPar_min_0);
  return -1;
 }
 int32_t _190 = _164 + _165;
 int32_t _191 = _190 - _inPar_extent_0;
 int32_t _192 = _191 + 1;
 bool _193 = _192 <= _inPar_min_0;
 int32_t _194 = _inPar_min_0 + _inPar_extent_0;
 int32_t _195 = _194 + -1;
 if (!_193) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n", _190, _195);
  return -1;
 }
 bool _196 = _inPar_min_1 <= _113;
 if (!_196) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _113, _inPar_min_1);
  return -1;
 }
 int32_t _197 = _105 - _inPar_extent_1;
 int32_t _198 = _197 + 1;
 bool _199 = _198 <= _inPar_min_1;
 int32_t _200 = _inPar_min_1 + _inPar_extent_1;
 int32_t _201 = _200 + -1;
 if (!_199) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n", _105, _201);
  return -1;
 }
 bool _202 = _f7_stride_0 == 1;
 if (!_202) {
  halide_printf(NULL, "Static constraint violated: f7.stride.0 == 1\n");
  return -1;
 }
 bool _203 = _inPar_stride_0 == 1;
 if (!_203) {
  halide_printf(NULL, "Static constraint violated: inPar.stride.0 == 1\n");
  return -1;
 }
 int64_t _204 = (int64_t)(_f7_extent_0);
 int64_t _205 = (int64_t)(_f7_extent_1);
 int64_t _206 = (int64_t)(_inPar_extent_0);
 int64_t _207 = (int64_t)(_inPar_extent_1);
 int64_t _208 = (int64_t)(2147483647);
 bool _209 = _204 <= _208;
 if (!_209) {
  halide_printf(NULL, "Total allocation for buffer f7 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _210 = (int64_t)(_f7_stride_1);
 int64_t _211 = _205 * _210;
 bool _212 = _211 <= _208;
 if (!_212) {
  halide_printf(NULL, "Total allocation for buffer f7 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _213 = _205 * _204;
 bool _214 = _213 <= _208;
 if (!_214) {
  halide_printf(NULL, "Product of extents for buffer f7 exceeds 2^31 - 1\n");
  return -1;
 }
 bool _215 = _206 <= _208;
 if (!_215) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _216 = (int64_t)(_inPar_stride_1);
 int64_t _217 = _207 * _216;
 bool _218 = _217 <= _208;
 if (!_218) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _219 = _207 * _206;
 bool _220 = _219 <= _208;
 if (!_220) {
  halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 // produce f7
 int32_t _221 = _f7_extent_0 + 63;
 int32_t _222 = _221 >> 6;
 int32_t _223 = _f7_extent_1 + 63;
 int32_t _224 = _223 >> 6;
 int32_t _225 = _222 * _224;
 #pragma omp parallel for
 for (int _f7_s0_x_xo_nid = 0; _f7_s0_x_xo_nid < 0 + _225; _f7_s0_x_xo_nid++)
 {
  int32_t _226 = _f7_extent_0 + 63;
  int32_t _227 = _226 >> 6;
  int32_t _228 = mod(_f7_s0_x_xo_nid, _227);
  int32_t _229 = _228 * 64;
  int32_t _230 = _229 + _f7_min_0;
  int32_t _231 = _f7_min_0 + _f7_extent_0;
  int32_t _232 = _231 + -64;
  int32_t _233 = min(_230, _232);
  int32_t _234 = sdiv(_f7_s0_x_xo_nid, _227);
  int32_t _235 = _234 * 64;
  int32_t _236 = _235 + _f7_min_1;
  int32_t _237 = _f7_min_1 + _f7_extent_1;
  int32_t _238 = _237 + -64;
  int32_t _239 = min(_236, _238);
  int32_t _240 = _239 + 63;
  int32_t _241 = _239 + 64;
  int32_t _242 = min(_241, 2047);
  int32_t _243 = max(_242, 0);
  int32_t _244 = max(_240, _243);
  int32_t _245 = _239 + 62;
  int32_t _246 = min(_245, 2047);
  int32_t _247 = max(_246, 0);
  int32_t _248 = max(_244, _247);
  int32_t _249 = _239 + 1;
  int32_t _250 = min(_249, 2047);
  int32_t _251 = max(_250, 0);
  int32_t _252 = min(_239, _251);
  int32_t _253 = _239 + -1;
  int32_t _254 = min(_253, 2047);
  int32_t _255 = max(_254, 0);
  int32_t _256 = min(_252, _255);
  int32_t _257 = _233 + 63;
  int32_t _258 = _233 + 64;
  int32_t _259 = min(_258, 2047);
  int32_t _260 = max(_259, 0);
  int32_t _261 = max(_257, _260);
  int32_t _262 = _233 + 62;
  int32_t _263 = min(_262, 2047);
  int32_t _264 = max(_263, 0);
  int32_t _265 = max(_261, _264);
  int32_t _266 = _233 + 1;
  int32_t _267 = min(_266, 2047);
  int32_t _268 = max(_267, 0);
  int32_t _269 = min(_233, _268);
  int32_t _270 = _233 + -1;
  int32_t _271 = min(_270, 2047);
  int32_t _272 = max(_271, 0);
  int32_t _273 = min(_269, _272);
  int32_t _274 = _256 + 1;
  int32_t _275 = min(_274, 2047);
  int32_t _276 = max(_275, 0);
  int32_t _277 = min(_256, _276);
  int32_t _278 = _256 + -1;
  int32_t _279 = min(_278, 2047);
  int32_t _280 = max(_279, 0);
  int32_t _281 = min(_277, _280);
  int32_t _282 = _281 + 1;
  int32_t _283 = min(_282, 2047);
  int32_t _284 = max(_283, 0);
  int32_t _285 = min(_281, _284);
  int32_t _286 = _281 + -1;
  int32_t _287 = min(_286, 2047);
  int32_t _288 = max(_287, 0);
  int32_t _289 = min(_285, _288);
  int32_t _290 = min(_239, 2047);
  int32_t _291 = max(_290, 0);
  int32_t _292 = max(_253, _291);
  int32_t _293 = _239 + -2;
  int32_t _294 = min(_293, 2047);
  int32_t _295 = max(_294, 0);
  int32_t _296 = max(_292, _295);
  int32_t _297 = _296 + 1;
  int32_t _298 = min(_297, 2047);
  int32_t _299 = max(_298, 0);
  int32_t _300 = max(_296, _299);
  int32_t _301 = _296 + -1;
  int32_t _302 = min(_301, 2047);
  int32_t _303 = max(_302, 0);
  int32_t _304 = max(_300, _303);
  int32_t _305 = _304 + 1;
  int32_t _306 = min(_305, 2047);
  int32_t _307 = max(_306, 0);
  int32_t _308 = max(_304, _307);
  int32_t _309 = _304 + -1;
  int32_t _310 = min(_309, 2047);
  int32_t _311 = max(_310, 0);
  int32_t _312 = max(_308, _311);
  int32_t _313 = _312 + 1;
  int32_t _314 = min(_289, _313);
  int32_t _315 = _289 + 1;
  int32_t _316 = min(_315, 2047);
  int32_t _317 = max(_316, 0);
  int32_t _318 = min(_289, _317);
  int32_t _319 = _289 + -1;
  int32_t _320 = min(_319, 2047);
  int32_t _321 = max(_320, 0);
  int32_t _322 = min(_318, _321);
  int32_t _323 = min(_313, 2047);
  int32_t _324 = max(_323, 0);
  int32_t _325 = max(_312, _324);
  int32_t _326 = _312 + -1;
  int32_t _327 = min(_326, 2047);
  int32_t _328 = max(_327, 0);
  int32_t _329 = max(_325, _328);
  int32_t _330 = _329 + 1;
  int32_t _331 = min(_322, _330);
  int32_t _332 = min(_331, _314);
  int32_t _333 = _314 + 1;
  int32_t _334 = min(_333, 2047);
  int32_t _335 = max(_334, 0);
  int32_t _336 = min(_332, _335);
  int32_t _337 = _314 + -1;
  int32_t _338 = min(_337, 2047);
  int32_t _339 = max(_338, 0);
  int32_t _340 = min(_336, _339);
  int32_t _341 = _248 + 1;
  int32_t _342 = min(_341, 2047);
  int32_t _343 = max(_342, 0);
  int32_t _344 = max(_248, _343);
  int32_t _345 = _248 + -1;
  int32_t _346 = min(_345, 2047);
  int32_t _347 = max(_346, 0);
  int32_t _348 = max(_344, _347);
  int32_t _349 = _348 + 1;
  int32_t _350 = min(_349, 2047);
  int32_t _351 = max(_350, 0);
  int32_t _352 = max(_348, _351);
  int32_t _353 = _348 + -1;
  int32_t _354 = min(_353, 2047);
  int32_t _355 = max(_354, 0);
  int32_t _356 = max(_352, _355);
  int32_t _357 = _356 + 1;
  int32_t _358 = min(_357, 2047);
  int32_t _359 = max(_358, 0);
  int32_t _360 = max(_356, _359);
  int32_t _361 = _356 + -1;
  int32_t _362 = min(_361, 2047);
  int32_t _363 = max(_362, 0);
  int32_t _364 = max(_360, _363);
  int32_t _365 = max(_364, _356);
  int32_t _366 = _365 - _340;
  int32_t _367 = _273 + 1;
  int32_t _368 = min(_367, 2047);
  int32_t _369 = max(_368, 0);
  int32_t _370 = min(_273, _369);
  int32_t _371 = _273 + -1;
  int32_t _372 = min(_371, 2047);
  int32_t _373 = max(_372, 0);
  int32_t _374 = min(_370, _373);
  int32_t _375 = _374 + 1;
  int32_t _376 = min(_375, 2047);
  int32_t _377 = max(_376, 0);
  int32_t _378 = min(_374, _377);
  int32_t _379 = _374 + -1;
  int32_t _380 = min(_379, 2047);
  int32_t _381 = max(_380, 0);
  int32_t _382 = min(_378, _381);
  int32_t _383 = _265 + 1;
  int32_t _384 = min(_383, 2047);
  int32_t _385 = max(_384, 0);
  int32_t _386 = max(_265, _385);
  int32_t _387 = _265 + -1;
  int32_t _388 = min(_387, 2047);
  int32_t _389 = max(_388, 0);
  int32_t _390 = max(_386, _389);
  int32_t _391 = _390 + 1;
  int32_t _392 = min(_391, 2047);
  int32_t _393 = max(_392, 0);
  int32_t _394 = max(_390, _393);
  int32_t _395 = _390 + -1;
  int32_t _396 = min(_395, 2047);
  int32_t _397 = max(_396, 0);
  int32_t _398 = max(_394, _397);
  int32_t _399 = _398 + -3;
  int32_t _400 = min(_382, _399);
  int32_t _401 = _398 - _382;
  int32_t _402 = _401 >> 2;
  int32_t _403 = _402 * 4;
  int32_t _404 = _382 + _403;
  int32_t _405 = _404 + 3;
  int32_t _406 = min(_405, _398);
  int32_t _407 = _406 - _400;
  {
   int32_t _408 = _407 + 1;
   int64_t _409 = _408;
   int32_t _410 = _366 + 1;
   int64_t _411 = _409 * _410;
   if ((_411 > ((int64_t(1) << 31) - 1)) || ((_411 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
   {
    halide_printf(NULL, "32-bit signed overflow computing size of allocation f0\n");
   } // overflow test f0
   int64_t _412 = _411;
   float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_412);
   int32_t _413 = _256 + 1;
   int32_t _414 = min(_413, 2047);
   int32_t _415 = max(_414, 0);
   int32_t _416 = min(_256, _415);
   int32_t _417 = _256 + -1;
   int32_t _418 = min(_417, 2047);
   int32_t _419 = max(_418, 0);
   int32_t _420 = min(_416, _419);
   int32_t _421 = _420 + 1;
   int32_t _422 = min(_421, 2047);
   int32_t _423 = max(_422, 0);
   int32_t _424 = min(_420, _423);
   int32_t _425 = _420 + -1;
   int32_t _426 = min(_425, 2047);
   int32_t _427 = max(_426, 0);
   int32_t _428 = min(_424, _427);
   int32_t _429 = _239 + -1;
   int32_t _430 = min(_239, 2047);
   int32_t _431 = max(_430, 0);
   int32_t _432 = max(_429, _431);
   int32_t _433 = _239 + -2;
   int32_t _434 = min(_433, 2047);
   int32_t _435 = max(_434, 0);
   int32_t _436 = max(_432, _435);
   int32_t _437 = _436 + 1;
   int32_t _438 = min(_437, 2047);
   int32_t _439 = max(_438, 0);
   int32_t _440 = max(_436, _439);
   int32_t _441 = _436 + -1;
   int32_t _442 = min(_441, 2047);
   int32_t _443 = max(_442, 0);
   int32_t _444 = max(_440, _443);
   int32_t _445 = _444 + 1;
   int32_t _446 = min(_445, 2047);
   int32_t _447 = max(_446, 0);
   int32_t _448 = max(_444, _447);
   int32_t _449 = _444 + -1;
   int32_t _450 = min(_449, 2047);
   int32_t _451 = max(_450, 0);
   int32_t _452 = max(_448, _451);
   int32_t _453 = _452 + 1;
   int32_t _454 = min(_428, _453);
   int32_t _455 = _248 + 1;
   int32_t _456 = min(_455, 2047);
   int32_t _457 = max(_456, 0);
   int32_t _458 = max(_248, _457);
   int32_t _459 = _248 + -1;
   int32_t _460 = min(_459, 2047);
   int32_t _461 = max(_460, 0);
   int32_t _462 = max(_458, _461);
   int32_t _463 = _462 + 1;
   int32_t _464 = min(_463, 2047);
   int32_t _465 = max(_464, 0);
   int32_t _466 = max(_462, _465);
   int32_t _467 = _462 + -1;
   int32_t _468 = min(_467, 2047);
   int32_t _469 = max(_468, 0);
   int32_t _470 = max(_466, _469);
   int32_t _471 = _470 - _454;
   int32_t _472 = _273 + 1;
   int32_t _473 = min(_472, 2047);
   int32_t _474 = max(_473, 0);
   int32_t _475 = min(_273, _474);
   int32_t _476 = _273 + -1;
   int32_t _477 = min(_476, 2047);
   int32_t _478 = max(_477, 0);
   int32_t _479 = min(_475, _478);
   int32_t _480 = _265 + 1;
   int32_t _481 = min(_480, 2047);
   int32_t _482 = max(_481, 0);
   int32_t _483 = max(_265, _482);
   int32_t _484 = _265 + -1;
   int32_t _485 = min(_484, 2047);
   int32_t _486 = max(_485, 0);
   int32_t _487 = max(_483, _486);
   int32_t _488 = _487 + -3;
   int32_t _489 = min(_479, _488);
   int32_t _490 = _479 + 1;
   int32_t _491 = min(_490, 2047);
   int32_t _492 = max(_491, 0);
   int32_t _493 = min(_479, _492);
   int32_t _494 = _479 + -1;
   int32_t _495 = min(_494, 2047);
   int32_t _496 = max(_495, 0);
   int32_t _497 = min(_493, _496);
   int32_t _498 = _487 + 1;
   int32_t _499 = min(_498, 2047);
   int32_t _500 = max(_499, 0);
   int32_t _501 = max(_487, _500);
   int32_t _502 = _487 + -1;
   int32_t _503 = min(_502, 2047);
   int32_t _504 = max(_503, 0);
   int32_t _505 = max(_501, _504);
   int32_t _506 = _505 + -3;
   int32_t _507 = min(_497, _506);
   int32_t _508 = min(_507, _489);
   int32_t _509 = _489 + 1;
   int32_t _510 = min(_509, 2047);
   int32_t _511 = max(_510, 0);
   int32_t _512 = min(_508, _511);
   int32_t _513 = _489 + -1;
   int32_t _514 = min(_513, 2047);
   int32_t _515 = max(_514, 0);
   int32_t _516 = min(_512, _515);
   int32_t _517 = _487 - _479;
   int32_t _518 = _517 >> 2;
   int32_t _519 = _518 * 4;
   int32_t _520 = _479 + _519;
   int32_t _521 = min(_520, _488);
   int32_t _522 = _505 - _497;
   int32_t _523 = _522 >> 2;
   int32_t _524 = _523 * 4;
   int32_t _525 = _497 + _524;
   int32_t _526 = _525 + 3;
   int32_t _527 = min(_526, _505);
   int32_t _528 = _521 + 3;
   int32_t _529 = max(_527, _528);
   int32_t _530 = _521 + 4;
   int32_t _531 = min(_530, 2047);
   int32_t _532 = max(_531, 0);
   int32_t _533 = max(_529, _532);
   int32_t _534 = _521 + 2;
   int32_t _535 = min(_534, 2047);
   int32_t _536 = max(_535, 0);
   int32_t _537 = max(_533, _536);
   int32_t _538 = _537 - _516;
   {
    int32_t _539 = _538 + 1;
    int64_t _540 = _539;
    int32_t _541 = _471 + 1;
    int64_t _542 = _540 * _541;
    if ((_542 > ((int64_t(1) << 31) - 1)) || ((_542 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
    {
     halide_printf(NULL, "32-bit signed overflow computing size of allocation f4\n");
    } // overflow test f4
    int64_t _543 = _542;
    float *_f4 = (float *)halide_malloc(NULL, sizeof(float)*_543);
    int32_t _544 = _256 + 1;
    int32_t _545 = min(_544, 2047);
    int32_t _546 = max(_545, 0);
    int32_t _547 = min(_256, _546);
    int32_t _548 = _256 + -1;
    int32_t _549 = min(_548, 2047);
    int32_t _550 = max(_549, 0);
    int32_t _551 = min(_547, _550);
    int32_t _552 = _239 + -1;
    int32_t _553 = min(_239, 2047);
    int32_t _554 = max(_553, 0);
    int32_t _555 = max(_552, _554);
    int32_t _556 = _239 + -2;
    int32_t _557 = min(_556, 2047);
    int32_t _558 = max(_557, 0);
    int32_t _559 = max(_555, _558);
    int32_t _560 = _559 + 1;
    int32_t _561 = min(_560, 2047);
    int32_t _562 = max(_561, 0);
    int32_t _563 = max(_559, _562);
    int32_t _564 = _559 + -1;
    int32_t _565 = min(_564, 2047);
    int32_t _566 = max(_565, 0);
    int32_t _567 = max(_563, _566);
    int32_t _568 = _567 + 1;
    int32_t _569 = min(_551, _568);
    int32_t _570 = _551 + 1;
    int32_t _571 = min(_570, 2047);
    int32_t _572 = max(_571, 0);
    int32_t _573 = min(_551, _572);
    int32_t _574 = _551 + -1;
    int32_t _575 = min(_574, 2047);
    int32_t _576 = max(_575, 0);
    int32_t _577 = min(_573, _576);
    int32_t _578 = min(_568, 2047);
    int32_t _579 = max(_578, 0);
    int32_t _580 = max(_567, _579);
    int32_t _581 = _567 + -1;
    int32_t _582 = min(_581, 2047);
    int32_t _583 = max(_582, 0);
    int32_t _584 = max(_580, _583);
    int32_t _585 = _584 + 1;
    int32_t _586 = min(_577, _585);
    int32_t _587 = min(_586, _569);
    int32_t _588 = _569 + 1;
    int32_t _589 = min(_588, 2047);
    int32_t _590 = max(_589, 0);
    int32_t _591 = min(_587, _590);
    int32_t _592 = _569 + -1;
    int32_t _593 = min(_592, 2047);
    int32_t _594 = max(_593, 0);
    int32_t _595 = min(_591, _594);
    int32_t _596 = _248 + 1;
    int32_t _597 = min(_596, 2047);
    int32_t _598 = max(_597, 0);
    int32_t _599 = max(_248, _598);
    int32_t _600 = _248 + -1;
    int32_t _601 = min(_600, 2047);
    int32_t _602 = max(_601, 0);
    int32_t _603 = max(_599, _602);
    int32_t _604 = _603 + 1;
    int32_t _605 = min(_604, 2047);
    int32_t _606 = max(_605, 0);
    int32_t _607 = max(_603, _606);
    int32_t _608 = _603 + -1;
    int32_t _609 = min(_608, 2047);
    int32_t _610 = max(_609, 0);
    int32_t _611 = max(_607, _610);
    int32_t _612 = max(_611, _603);
    int32_t _613 = _612 - _595;
    int32_t _614 = _273 + 1;
    int32_t _615 = min(_614, 2047);
    int32_t _616 = max(_615, 0);
    int32_t _617 = min(_273, _616);
    int32_t _618 = _273 + -1;
    int32_t _619 = min(_618, 2047);
    int32_t _620 = max(_619, 0);
    int32_t _621 = min(_617, _620);
    int32_t _622 = _265 + 1;
    int32_t _623 = min(_622, 2047);
    int32_t _624 = max(_623, 0);
    int32_t _625 = max(_265, _624);
    int32_t _626 = _265 + -1;
    int32_t _627 = min(_626, 2047);
    int32_t _628 = max(_627, 0);
    int32_t _629 = max(_625, _628);
    int32_t _630 = _629 + -3;
    int32_t _631 = min(_621, _630);
    int32_t _632 = _629 - _621;
    int32_t _633 = _632 >> 2;
    int32_t _634 = _633 * 4;
    int32_t _635 = _621 + _634;
    int32_t _636 = _635 + 3;
    int32_t _637 = min(_636, _629);
    int32_t _638 = _637 - _631;
    {
     int32_t _639 = _638 + 1;
     int64_t _640 = _639;
     int32_t _641 = _613 + 1;
     int64_t _642 = _640 * _641;
     if ((_642 > ((int64_t(1) << 31) - 1)) || ((_642 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
     {
      halide_printf(NULL, "32-bit signed overflow computing size of allocation f1\n");
     } // overflow test f1
     int64_t _643 = _642;
     float *_f1 = (float *)halide_malloc(NULL, sizeof(float)*_643);
     int32_t _644 = _256 + 1;
     int32_t _645 = min(_644, 2047);
     int32_t _646 = max(_645, 0);
     int32_t _647 = min(_256, _646);
     int32_t _648 = _256 + -1;
     int32_t _649 = min(_648, 2047);
     int32_t _650 = max(_649, 0);
     int32_t _651 = min(_647, _650);
     int32_t _652 = _239 + -1;
     int32_t _653 = min(_239, 2047);
     int32_t _654 = max(_653, 0);
     int32_t _655 = max(_652, _654);
     int32_t _656 = _239 + -2;
     int32_t _657 = min(_656, 2047);
     int32_t _658 = max(_657, 0);
     int32_t _659 = max(_655, _658);
     int32_t _660 = _659 + 1;
     int32_t _661 = min(_660, 2047);
     int32_t _662 = max(_661, 0);
     int32_t _663 = max(_659, _662);
     int32_t _664 = _659 + -1;
     int32_t _665 = min(_664, 2047);
     int32_t _666 = max(_665, 0);
     int32_t _667 = max(_663, _666);
     int32_t _668 = _667 + 1;
     int32_t _669 = min(_651, _668);
     int32_t _670 = _248 + 1;
     int32_t _671 = min(_670, 2047);
     int32_t _672 = max(_671, 0);
     int32_t _673 = max(_248, _672);
     int32_t _674 = _248 + -1;
     int32_t _675 = min(_674, 2047);
     int32_t _676 = max(_675, 0);
     int32_t _677 = max(_673, _676);
     int32_t _678 = _677 - _669;
     int32_t _679 = _273 + 1;
     int32_t _680 = min(_679, 2047);
     int32_t _681 = max(_680, 0);
     int32_t _682 = min(_273, _681);
     int32_t _683 = _273 + -1;
     int32_t _684 = min(_683, 2047);
     int32_t _685 = max(_684, 0);
     int32_t _686 = min(_682, _685);
     int32_t _687 = _265 + 1;
     int32_t _688 = min(_687, 2047);
     int32_t _689 = max(_688, 0);
     int32_t _690 = max(_265, _689);
     int32_t _691 = _265 + -1;
     int32_t _692 = min(_691, 2047);
     int32_t _693 = max(_692, 0);
     int32_t _694 = max(_690, _693);
     int32_t _695 = _694 + -3;
     int32_t _696 = min(_686, _695);
     int32_t _697 = _265 + -3;
     int32_t _698 = min(_273, _697);
     int32_t _699 = min(_696, _698);
     int32_t _700 = _698 + 1;
     int32_t _701 = min(_700, 2047);
     int32_t _702 = max(_701, 0);
     int32_t _703 = min(_699, _702);
     int32_t _704 = _698 + -1;
     int32_t _705 = min(_704, 2047);
     int32_t _706 = max(_705, 0);
     int32_t _707 = min(_703, _706);
     int32_t _708 = _694 - _686;
     int32_t _709 = _708 >> 2;
     int32_t _710 = _709 * 4;
     int32_t _711 = _686 + _710;
     int32_t _712 = _711 + 3;
     int32_t _713 = min(_712, _694);
     int32_t _714 = _265 - _273;
     int32_t _715 = _714 >> 2;
     int32_t _716 = _715 * 4;
     int32_t _717 = _716 + _273;
     int32_t _718 = _717 + 3;
     int32_t _719 = min(_718, _265);
     int32_t _720 = max(_713, _719);
     int32_t _721 = _719 + 1;
     int32_t _722 = min(_721, 2047);
     int32_t _723 = max(_722, 0);
     int32_t _724 = max(_720, _723);
     int32_t _725 = _719 + -1;
     int32_t _726 = min(_725, 2047);
     int32_t _727 = max(_726, 0);
     int32_t _728 = max(_724, _727);
     int32_t _729 = _728 - _707;
     {
      int32_t _730 = _729 + 1;
      int64_t _731 = _730;
      int32_t _732 = _678 + 1;
      int64_t _733 = _731 * _732;
      if ((_733 > ((int64_t(1) << 31) - 1)) || ((_733 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
      {
       halide_printf(NULL, "32-bit signed overflow computing size of allocation f5\n");
      } // overflow test f5
      int64_t _734 = _733;
      float *_f5 = (float *)halide_malloc(NULL, sizeof(float)*_734);
      int32_t _735 = _239 + -1;
      int32_t _736 = min(_239, 2047);
      int32_t _737 = max(_736, 0);
      int32_t _738 = max(_735, _737);
      int32_t _739 = _239 + -2;
      int32_t _740 = min(_739, 2047);
      int32_t _741 = max(_740, 0);
      int32_t _742 = max(_738, _741);
      int32_t _743 = _742 + 1;
      int32_t _744 = min(_256, _743);
      int32_t _745 = _256 + 1;
      int32_t _746 = min(_745, 2047);
      int32_t _747 = max(_746, 0);
      int32_t _748 = min(_256, _747);
      int32_t _749 = _256 + -1;
      int32_t _750 = min(_749, 2047);
      int32_t _751 = max(_750, 0);
      int32_t _752 = min(_748, _751);
      int32_t _753 = min(_743, 2047);
      int32_t _754 = max(_753, 0);
      int32_t _755 = max(_742, _754);
      int32_t _756 = _742 + -1;
      int32_t _757 = min(_756, 2047);
      int32_t _758 = max(_757, 0);
      int32_t _759 = max(_755, _758);
      int32_t _760 = _759 + 1;
      int32_t _761 = min(_752, _760);
      int32_t _762 = min(_761, _744);
      int32_t _763 = _744 + 1;
      int32_t _764 = min(_763, 2047);
      int32_t _765 = max(_764, 0);
      int32_t _766 = min(_762, _765);
      int32_t _767 = _744 + -1;
      int32_t _768 = min(_767, 2047);
      int32_t _769 = max(_768, 0);
      int32_t _770 = min(_766, _769);
      int32_t _771 = _248 + 1;
      int32_t _772 = min(_771, 2047);
      int32_t _773 = max(_772, 0);
      int32_t _774 = _248 + -1;
      int32_t _775 = min(_774, 2047);
      int32_t _776 = max(_775, 0);
      int32_t _777 = max(_248, _773);
      int32_t _778 = max(_777, _776);
      int32_t _779 = max(_778, _248);
      int32_t _780 = _779 - _770;
      int32_t _781 = _265 + -3;
      int32_t _782 = min(_273, _781);
      int32_t _783 = _265 - _273;
      int32_t _784 = _783 >> 2;
      int32_t _785 = _784 * 4;
      int32_t _786 = _785 + _273;
      int32_t _787 = _786 + 3;
      int32_t _788 = min(_787, _265);
      int32_t _789 = _788 - _782;
      {
       int32_t _790 = _789 + 1;
       int64_t _791 = _790;
       int32_t _792 = _780 + 1;
       int64_t _793 = _791 * _792;
       if ((_793 > ((int64_t(1) << 31) - 1)) || ((_793 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
       {
        halide_printf(NULL, "32-bit signed overflow computing size of allocation f2\n");
       } // overflow test f2
       int64_t _794 = _793;
       float *_f2 = (float *)halide_malloc(NULL, sizeof(float)*_794);
       int32_t _795 = _239 + -1;
       int32_t _796 = min(_239, 2047);
       int32_t _797 = max(_796, 0);
       int32_t _798 = max(_795, _797);
       int32_t _799 = _239 + -2;
       int32_t _800 = min(_799, 2047);
       int32_t _801 = max(_800, 0);
       int32_t _802 = max(_798, _801);
       int32_t _803 = _802 + 1;
       int32_t _804 = min(_256, _803);
       int32_t _805 = _248 - _804;
       int32_t _806 = _265 + -3;
       int32_t _807 = min(_273, _806);
       int32_t _808 = min(_807, _233);
       int32_t _809 = _233 + 1;
       int32_t _810 = min(_809, 2047);
       int32_t _811 = max(_810, 0);
       int32_t _812 = min(_808, _811);
       int32_t _813 = _233 + -1;
       int32_t _814 = min(_813, 2047);
       int32_t _815 = max(_814, 0);
       int32_t _816 = min(_812, _815);
       int32_t _817 = _265 - _273;
       int32_t _818 = _817 >> 2;
       int32_t _819 = _818 * 4;
       int32_t _820 = _819 + _273;
       int32_t _821 = _820 + 3;
       int32_t _822 = min(_821, _265);
       int32_t _823 = _233 + 63;
       int32_t _824 = max(_822, _823);
       int32_t _825 = _233 + 64;
       int32_t _826 = min(_825, 2047);
       int32_t _827 = max(_826, 0);
       int32_t _828 = max(_824, _827);
       int32_t _829 = _233 + 62;
       int32_t _830 = min(_829, 2047);
       int32_t _831 = max(_830, 0);
       int32_t _832 = max(_828, _831);
       int32_t _833 = _832 - _816;
       {
        int32_t _834 = _833 + 1;
        int64_t _835 = _834;
        int32_t _836 = _805 + 1;
        int64_t _837 = _835 * _836;
        if ((_837 > ((int64_t(1) << 31) - 1)) || ((_837 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
        {
         halide_printf(NULL, "32-bit signed overflow computing size of allocation f6\n");
        } // overflow test f6
        int64_t _838 = _837;
        float *_f6 = (float *)halide_malloc(NULL, sizeof(float)*_838);
        int32_t _839 = _239 + -1;
        int32_t _840 = min(_239, 2047);
        int32_t _841 = max(_840, 0);
        int32_t _842 = max(_839, _841);
        int32_t _843 = _239 + -2;
        int32_t _844 = min(_843, 2047);
        int32_t _845 = max(_844, 0);
        int32_t _846 = max(_842, _845);
        int32_t _847 = _846 + 1;
        int32_t _848 = min(_256, _847);
        int32_t _849 = min(_848, _239);
        int32_t _850 = _239 + 1;
        int32_t _851 = min(_850, 2047);
        int32_t _852 = max(_851, 0);
        int32_t _853 = min(_849, _852);
        int32_t _854 = min(_839, 2047);
        int32_t _855 = max(_854, 0);
        int32_t _856 = min(_853, _855);
        int32_t _857 = _239 + 63;
        int32_t _858 = max(_248, _857);
        int32_t _859 = _858 - _856;
        {
         int64_t _860 = 64;
         int32_t _861 = _859 + 1;
         int64_t _862 = _860 * _861;
         if ((_862 > ((int64_t(1) << 31) - 1)) || ((_862 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
         {
          halide_printf(NULL, "32-bit signed overflow computing size of allocation f3\n");
         } // overflow test f3
         int64_t _863 = _862;
         float *_f3 = (float *)halide_malloc(NULL, sizeof(float)*_863);
         for (int _f7_s0_y_yi = 0; _f7_s0_y_yi < 0 + 64; _f7_s0_y_yi++)
         {
          int32_t _864 = _239 + _f7_s0_y_yi;
          int32_t _865 = _864 + 1;
          int32_t _866 = min(_865, 2047);
          int32_t _867 = max(_866, 0);
          int32_t _868 = max(_864, _867);
          int32_t _869 = _864 + -1;
          int32_t _870 = min(_869, 2047);
          int32_t _871 = max(_870, 0);
          int32_t _872 = max(_868, _871);
          int32_t _873 = min(_864, _867);
          int32_t _874 = min(_873, _871);
          int32_t _875 = min(_864, 2047);
          int32_t _876 = max(_875, 0);
          int32_t _877 = max(_869, _876);
          int32_t _878 = _864 + -2;
          int32_t _879 = min(_878, 2047);
          int32_t _880 = max(_879, 0);
          int32_t _881 = max(_877, _880);
          int32_t _882 = _881 + 1;
          bool _883 = _f7_s0_y_yi == 0;
          int32_t _884 = (int32_t)(_883 ? _874 : _882);
          int32_t _885 = _872 + 1;
          int32_t _886 = min(_885, 2047);
          int32_t _887 = max(_886, 0);
          int32_t _888 = max(_872, _887);
          int32_t _889 = _872 + -1;
          int32_t _890 = min(_889, 2047);
          int32_t _891 = max(_890, 0);
          int32_t _892 = max(_888, _891);
          int32_t _893 = _874 + 1;
          int32_t _894 = min(_893, 2047);
          int32_t _895 = max(_894, 0);
          int32_t _896 = min(_874, _895);
          int32_t _897 = _874 + -1;
          int32_t _898 = min(_897, 2047);
          int32_t _899 = max(_898, 0);
          int32_t _900 = min(_896, _899);
          int32_t _901 = min(_882, 2047);
          int32_t _902 = max(_901, 0);
          int32_t _903 = max(_881, _902);
          int32_t _904 = _881 + -1;
          int32_t _905 = min(_904, 2047);
          int32_t _906 = max(_905, 0);
          int32_t _907 = max(_903, _906);
          int32_t _908 = _907 + 1;
          int32_t _909 = (int32_t)(_883 ? _900 : _908);
          int32_t _910 = _265 + 1;
          int32_t _911 = min(_910, 2047);
          int32_t _912 = max(_911, 0);
          int32_t _913 = max(_265, _912);
          int32_t _914 = _265 + -1;
          int32_t _915 = min(_914, 2047);
          int32_t _916 = max(_915, 0);
          int32_t _917 = max(_913, _916);
          int32_t _918 = _273 + 1;
          int32_t _919 = min(_918, 2047);
          int32_t _920 = max(_919, 0);
          int32_t _921 = min(_273, _920);
          int32_t _922 = _273 + -1;
          int32_t _923 = min(_922, 2047);
          int32_t _924 = max(_923, 0);
          int32_t _925 = min(_921, _924);
          int32_t _926 = _892 + 1;
          int32_t _927 = min(_926, 2047);
          int32_t _928 = max(_927, 0);
          int32_t _929 = max(_892, _928);
          int32_t _930 = _892 + -1;
          int32_t _931 = min(_930, 2047);
          int32_t _932 = max(_931, 0);
          int32_t _933 = max(_929, _932);
          int32_t _934 = _900 + 1;
          int32_t _935 = min(_934, 2047);
          int32_t _936 = max(_935, 0);
          int32_t _937 = min(_900, _936);
          int32_t _938 = _900 + -1;
          int32_t _939 = min(_938, 2047);
          int32_t _940 = max(_939, 0);
          int32_t _941 = min(_937, _940);
          int32_t _942 = min(_908, 2047);
          int32_t _943 = max(_942, 0);
          int32_t _944 = max(_907, _943);
          int32_t _945 = _907 + -1;
          int32_t _946 = min(_945, 2047);
          int32_t _947 = max(_946, 0);
          int32_t _948 = max(_944, _947);
          int32_t _949 = _948 + 1;
          int32_t _950 = (int32_t)(_883 ? _941 : _949);
          int32_t _951 = _917 + 1;
          int32_t _952 = min(_951, 2047);
          int32_t _953 = max(_952, 0);
          int32_t _954 = max(_917, _953);
          int32_t _955 = _917 + -1;
          int32_t _956 = min(_955, 2047);
          int32_t _957 = max(_956, 0);
          int32_t _958 = max(_954, _957);
          int32_t _959 = _925 + 1;
          int32_t _960 = min(_959, 2047);
          int32_t _961 = max(_960, 0);
          int32_t _962 = min(_925, _961);
          int32_t _963 = _925 + -1;
          int32_t _964 = min(_963, 2047);
          int32_t _965 = max(_964, 0);
          int32_t _966 = min(_962, _965);
          int32_t _967 = _933 + 1;
          int32_t _968 = min(_967, 2047);
          int32_t _969 = max(_968, 0);
          int32_t _970 = max(_933, _969);
          int32_t _971 = _933 + -1;
          int32_t _972 = min(_971, 2047);
          int32_t _973 = max(_972, 0);
          int32_t _974 = max(_970, _973);
          int32_t _975 = _941 + 1;
          int32_t _976 = min(_975, 2047);
          int32_t _977 = max(_976, 0);
          int32_t _978 = min(_941, _977);
          int32_t _979 = _941 + -1;
          int32_t _980 = min(_979, 2047);
          int32_t _981 = max(_980, 0);
          int32_t _982 = min(_978, _981);
          int32_t _983 = min(_949, 2047);
          int32_t _984 = max(_983, 0);
          int32_t _985 = max(_948, _984);
          int32_t _986 = _948 + -1;
          int32_t _987 = min(_986, 2047);
          int32_t _988 = max(_987, 0);
          int32_t _989 = max(_985, _988);
          int32_t _990 = _989 + 1;
          int32_t _991 = (int32_t)(_883 ? _982 : _990);
          // produce f0
          int32_t _992 = _974 - _991;
          int32_t _993 = _992 + 1;
          for (int _f0_s0_y = _991; _f0_s0_y < _991 + _993; _f0_s0_y++)
          {
           int32_t _994 = _958 - _966;
           int32_t _995 = _994 + 4;
           int32_t _996 = _995 >> 2;
           for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + _996; _f0_s0_x_x++)
           {
            int32_t _997 = _f0_s0_x_x * 4;
            int32_t _998 = _997 + _966;
            int32_t _999 = _958 + -3;
            int32_t _1000 = min(_998, _999);
            int32_t _1001 = _1000 - _400;
            int32_t _1002 = _f0_s0_y - _340;
            int32_t _1003 = _407 + 1;
            int32_t _1004 = _1002 * _1003;
            int32_t _1005 = _1001 + _1004;
            int32_t _1006 = _f0_s0_y * _inPar_stride_1;
            int32_t _1007 = _1000 + _1006;
            int32_t _1008 = _inPar_min_1 * _inPar_stride_1;
            int32_t _1009 = _inPar_min_0 + _1008;
            int32_t _1010 = _1007 - _1009;
            float _1011 = ((float *)_inPar)[_1010];
            float _1012 = _1011 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1013 = _1000 + 1;
            int32_t _1014 = min(_1013, 2047);
            int32_t _1015 = max(_1014, 0);
            int32_t _1016 = _1015 + _1006;
            int32_t _1017 = _1016 - _1009;
            float _1018 = ((float *)_inPar)[_1017];
            float _1019 = _1018 * float_from_bits(1048576000 /* 0.25 */);
            float _1020 = _1012 + _1019;
            int32_t _1021 = _1000 + -1;
            int32_t _1022 = min(_1021, 2047);
            int32_t _1023 = max(_1022, 0);
            int32_t _1024 = _1023 + _1006;
            int32_t _1025 = _1024 - _1009;
            float _1026 = ((float *)_inPar)[_1025];
            float _1027 = _1026 * float_from_bits(1048576000 /* 0.25 */);
            float _1028 = _1020 + _1027;
            _f0[_1005] = _1028;
            int32_t _1029 = _1000 - _400;
            int32_t _1030 = _f0_s0_y - _340;
            int32_t _1031 = _407 + 1;
            int32_t _1032 = _1030 * _1031;
            int32_t _1033 = _1029 + _1032;
            int32_t _1034 = _1033 + 1;
            int32_t _1035 = _f0_s0_y * _inPar_stride_1;
            int32_t _1036 = _1000 + _1035;
            int32_t _1037 = _inPar_min_1 * _inPar_stride_1;
            int32_t _1038 = _inPar_min_0 + _1037;
            int32_t _1039 = _1036 - _1038;
            int32_t _1040 = _1039 + 1;
            float _1041 = ((float *)_inPar)[_1040];
            float _1042 = _1041 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1043 = _1000 + 2;
            int32_t _1044 = min(_1043, 2047);
            int32_t _1045 = max(_1044, 0);
            int32_t _1046 = _1045 + _1035;
            int32_t _1047 = _1046 - _1038;
            float _1048 = ((float *)_inPar)[_1047];
            float _1049 = _1048 * float_from_bits(1048576000 /* 0.25 */);
            float _1050 = _1042 + _1049;
            int32_t _1051 = min(_1000, 2047);
            int32_t _1052 = max(_1051, 0);
            int32_t _1053 = _1052 + _1035;
            int32_t _1054 = _1053 - _1038;
            float _1055 = ((float *)_inPar)[_1054];
            float _1056 = _1055 * float_from_bits(1048576000 /* 0.25 */);
            float _1057 = _1050 + _1056;
            _f0[_1034] = _1057;
            int32_t _1058 = _1000 - _400;
            int32_t _1059 = _f0_s0_y - _340;
            int32_t _1060 = _407 + 1;
            int32_t _1061 = _1059 * _1060;
            int32_t _1062 = _1058 + _1061;
            int32_t _1063 = _1062 + 2;
            int32_t _1064 = _f0_s0_y * _inPar_stride_1;
            int32_t _1065 = _1000 + _1064;
            int32_t _1066 = _inPar_min_1 * _inPar_stride_1;
            int32_t _1067 = _inPar_min_0 + _1066;
            int32_t _1068 = _1065 - _1067;
            int32_t _1069 = _1068 + 2;
            float _1070 = ((float *)_inPar)[_1069];
            float _1071 = _1070 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1072 = _1000 + 3;
            int32_t _1073 = min(_1072, 2047);
            int32_t _1074 = max(_1073, 0);
            int32_t _1075 = _1074 + _1064;
            int32_t _1076 = _1075 - _1067;
            float _1077 = ((float *)_inPar)[_1076];
            float _1078 = _1077 * float_from_bits(1048576000 /* 0.25 */);
            float _1079 = _1071 + _1078;
            int32_t _1080 = _1000 + 1;
            int32_t _1081 = min(_1080, 2047);
            int32_t _1082 = max(_1081, 0);
            int32_t _1083 = _1082 + _1064;
            int32_t _1084 = _1083 - _1067;
            float _1085 = ((float *)_inPar)[_1084];
            float _1086 = _1085 * float_from_bits(1048576000 /* 0.25 */);
            float _1087 = _1079 + _1086;
            _f0[_1063] = _1087;
            int32_t _1088 = _1000 - _400;
            int32_t _1089 = _f0_s0_y - _340;
            int32_t _1090 = _407 + 1;
            int32_t _1091 = _1089 * _1090;
            int32_t _1092 = _1088 + _1091;
            int32_t _1093 = _1092 + 3;
            int32_t _1094 = _f0_s0_y * _inPar_stride_1;
            int32_t _1095 = _1000 + _1094;
            int32_t _1096 = _inPar_min_1 * _inPar_stride_1;
            int32_t _1097 = _inPar_min_0 + _1096;
            int32_t _1098 = _1095 - _1097;
            int32_t _1099 = _1098 + 3;
            float _1100 = ((float *)_inPar)[_1099];
            float _1101 = _1100 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1102 = _1000 + 4;
            int32_t _1103 = min(_1102, 2047);
            int32_t _1104 = max(_1103, 0);
            int32_t _1105 = _1104 + _1094;
            int32_t _1106 = _1105 - _1097;
            float _1107 = ((float *)_inPar)[_1106];
            float _1108 = _1107 * float_from_bits(1048576000 /* 0.25 */);
            float _1109 = _1101 + _1108;
            int32_t _1110 = _1000 + 2;
            int32_t _1111 = min(_1110, 2047);
            int32_t _1112 = max(_1111, 0);
            int32_t _1113 = _1112 + _1094;
            int32_t _1114 = _1113 - _1097;
            float _1115 = ((float *)_inPar)[_1114];
            float _1116 = _1115 * float_from_bits(1048576000 /* 0.25 */);
            float _1117 = _1109 + _1116;
            _f0[_1093] = _1117;
           } // for _f0_s0_x_x
          } // for _f0_s0_y
          // consume f0
          // produce f4
          int32_t _1118 = _933 - _950;
          int32_t _1119 = _1118 + 1;
          for (int _f4_s0_y = _950; _f4_s0_y < _950 + _1119; _f4_s0_y++)
          {
           int32_t _1120 = _958 - _966;
           int32_t _1121 = _1120 + 4;
           int32_t _1122 = _1121 >> 2;
           for (int _f4_s0_x_x = 0; _f4_s0_x_x < 0 + _1122; _f4_s0_x_x++)
           {
            int32_t _1123 = _f4_s0_x_x * 4;
            int32_t _1124 = _1123 + _966;
            int32_t _1125 = _958 + -3;
            int32_t _1126 = min(_1124, _1125);
            int32_t _1127 = _1126 - _516;
            int32_t _1128 = _f4_s0_y - _454;
            int32_t _1129 = _538 + 1;
            int32_t _1130 = _1128 * _1129;
            int32_t _1131 = _1127 + _1130;
            int32_t _1132 = _1126 - _400;
            int32_t _1133 = _f4_s0_y - _340;
            int32_t _1134 = _407 + 1;
            int32_t _1135 = _1133 * _1134;
            int32_t _1136 = _1132 + _1135;
            float _1137 = _f0[_1136];
            float _1138 = _1137 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1139 = _f4_s0_y + 1;
            int32_t _1140 = min(_1139, 2047);
            int32_t _1141 = max(_1140, 0);
            int32_t _1142 = _1141 - _340;
            int32_t _1143 = _1142 * _1134;
            int32_t _1144 = _1132 + _1143;
            float _1145 = _f0[_1144];
            float _1146 = _1145 * float_from_bits(1048576000 /* 0.25 */);
            float _1147 = _1138 + _1146;
            int32_t _1148 = _f4_s0_y + -1;
            int32_t _1149 = min(_1148, 2047);
            int32_t _1150 = max(_1149, 0);
            int32_t _1151 = _1150 - _340;
            int32_t _1152 = _1151 * _1134;
            int32_t _1153 = _1132 + _1152;
            float _1154 = _f0[_1153];
            float _1155 = _1154 * float_from_bits(1048576000 /* 0.25 */);
            float _1156 = _1147 + _1155;
            _f4[_1131] = _1156;
            int32_t _1157 = _1126 - _516;
            int32_t _1158 = _f4_s0_y - _454;
            int32_t _1159 = _538 + 1;
            int32_t _1160 = _1158 * _1159;
            int32_t _1161 = _1157 + _1160;
            int32_t _1162 = _1161 + 1;
            int32_t _1163 = _1126 - _400;
            int32_t _1164 = _f4_s0_y - _340;
            int32_t _1165 = _407 + 1;
            int32_t _1166 = _1164 * _1165;
            int32_t _1167 = _1163 + _1166;
            int32_t _1168 = _1167 + 1;
            float _1169 = _f0[_1168];
            float _1170 = _1169 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1171 = _f4_s0_y + 1;
            int32_t _1172 = min(_1171, 2047);
            int32_t _1173 = max(_1172, 0);
            int32_t _1174 = _1173 - _340;
            int32_t _1175 = _1174 * _1165;
            int32_t _1176 = _1163 + _1175;
            int32_t _1177 = _1176 + 1;
            float _1178 = _f0[_1177];
            float _1179 = _1178 * float_from_bits(1048576000 /* 0.25 */);
            float _1180 = _1170 + _1179;
            int32_t _1181 = _f4_s0_y + -1;
            int32_t _1182 = min(_1181, 2047);
            int32_t _1183 = max(_1182, 0);
            int32_t _1184 = _1183 - _340;
            int32_t _1185 = _1184 * _1165;
            int32_t _1186 = _1163 + _1185;
            int32_t _1187 = _1186 + 1;
            float _1188 = _f0[_1187];
            float _1189 = _1188 * float_from_bits(1048576000 /* 0.25 */);
            float _1190 = _1180 + _1189;
            _f4[_1162] = _1190;
            int32_t _1191 = _1126 - _516;
            int32_t _1192 = _f4_s0_y - _454;
            int32_t _1193 = _538 + 1;
            int32_t _1194 = _1192 * _1193;
            int32_t _1195 = _1191 + _1194;
            int32_t _1196 = _1195 + 2;
            int32_t _1197 = _1126 - _400;
            int32_t _1198 = _f4_s0_y - _340;
            int32_t _1199 = _407 + 1;
            int32_t _1200 = _1198 * _1199;
            int32_t _1201 = _1197 + _1200;
            int32_t _1202 = _1201 + 2;
            float _1203 = _f0[_1202];
            float _1204 = _1203 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1205 = _f4_s0_y + 1;
            int32_t _1206 = min(_1205, 2047);
            int32_t _1207 = max(_1206, 0);
            int32_t _1208 = _1207 - _340;
            int32_t _1209 = _1208 * _1199;
            int32_t _1210 = _1197 + _1209;
            int32_t _1211 = _1210 + 2;
            float _1212 = _f0[_1211];
            float _1213 = _1212 * float_from_bits(1048576000 /* 0.25 */);
            float _1214 = _1204 + _1213;
            int32_t _1215 = _f4_s0_y + -1;
            int32_t _1216 = min(_1215, 2047);
            int32_t _1217 = max(_1216, 0);
            int32_t _1218 = _1217 - _340;
            int32_t _1219 = _1218 * _1199;
            int32_t _1220 = _1197 + _1219;
            int32_t _1221 = _1220 + 2;
            float _1222 = _f0[_1221];
            float _1223 = _1222 * float_from_bits(1048576000 /* 0.25 */);
            float _1224 = _1214 + _1223;
            _f4[_1196] = _1224;
            int32_t _1225 = _1126 - _516;
            int32_t _1226 = _f4_s0_y - _454;
            int32_t _1227 = _538 + 1;
            int32_t _1228 = _1226 * _1227;
            int32_t _1229 = _1225 + _1228;
            int32_t _1230 = _1229 + 3;
            int32_t _1231 = _1126 - _400;
            int32_t _1232 = _f4_s0_y - _340;
            int32_t _1233 = _407 + 1;
            int32_t _1234 = _1232 * _1233;
            int32_t _1235 = _1231 + _1234;
            int32_t _1236 = _1235 + 3;
            float _1237 = _f0[_1236];
            float _1238 = _1237 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1239 = _f4_s0_y + 1;
            int32_t _1240 = min(_1239, 2047);
            int32_t _1241 = max(_1240, 0);
            int32_t _1242 = _1241 - _340;
            int32_t _1243 = _1242 * _1233;
            int32_t _1244 = _1231 + _1243;
            int32_t _1245 = _1244 + 3;
            float _1246 = _f0[_1245];
            float _1247 = _1246 * float_from_bits(1048576000 /* 0.25 */);
            float _1248 = _1238 + _1247;
            int32_t _1249 = _f4_s0_y + -1;
            int32_t _1250 = min(_1249, 2047);
            int32_t _1251 = max(_1250, 0);
            int32_t _1252 = _1251 - _340;
            int32_t _1253 = _1252 * _1233;
            int32_t _1254 = _1231 + _1253;
            int32_t _1255 = _1254 + 3;
            float _1256 = _f0[_1255];
            float _1257 = _1256 * float_from_bits(1048576000 /* 0.25 */);
            float _1258 = _1248 + _1257;
            _f4[_1230] = _1258;
           } // for _f4_s0_x_x
          } // for _f4_s0_y
          // consume f4
          // produce f1
          int32_t _1259 = _933 - _950;
          int32_t _1260 = _1259 + 1;
          for (int _f1_s0_y = _950; _f1_s0_y < _950 + _1260; _f1_s0_y++)
          {
           int32_t _1261 = _917 - _925;
           int32_t _1262 = _1261 + 4;
           int32_t _1263 = _1262 >> 2;
           for (int _f1_s0_x_x = 0; _f1_s0_x_x < 0 + _1263; _f1_s0_x_x++)
           {
            int32_t _1264 = _f1_s0_x_x * 4;
            int32_t _1265 = _1264 + _925;
            int32_t _1266 = _917 + -3;
            int32_t _1267 = min(_1265, _1266);
            int32_t _1268 = _1267 - _631;
            int32_t _1269 = _f1_s0_y - _595;
            int32_t _1270 = _638 + 1;
            int32_t _1271 = _1269 * _1270;
            int32_t _1272 = _1268 + _1271;
            int32_t _1273 = _1267 - _516;
            int32_t _1274 = _f1_s0_y - _454;
            int32_t _1275 = _538 + 1;
            int32_t _1276 = _1274 * _1275;
            int32_t _1277 = _1273 + _1276;
            float _1278 = _f4[_1277];
            float _1279 = _1278 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1280 = _1267 + 1;
            int32_t _1281 = min(_1280, 2047);
            int32_t _1282 = max(_1281, 0);
            int32_t _1283 = _1282 - _516;
            int32_t _1284 = _1283 + _1276;
            float _1285 = _f4[_1284];
            float _1286 = _1285 * float_from_bits(1048576000 /* 0.25 */);
            float _1287 = _1279 + _1286;
            int32_t _1288 = _1267 + -1;
            int32_t _1289 = min(_1288, 2047);
            int32_t _1290 = max(_1289, 0);
            int32_t _1291 = _1290 - _516;
            int32_t _1292 = _1291 + _1276;
            float _1293 = _f4[_1292];
            float _1294 = _1293 * float_from_bits(1048576000 /* 0.25 */);
            float _1295 = _1287 + _1294;
            _f1[_1272] = _1295;
            int32_t _1296 = _1267 - _631;
            int32_t _1297 = _f1_s0_y - _595;
            int32_t _1298 = _638 + 1;
            int32_t _1299 = _1297 * _1298;
            int32_t _1300 = _1296 + _1299;
            int32_t _1301 = _1300 + 1;
            int32_t _1302 = _1267 - _516;
            int32_t _1303 = _f1_s0_y - _454;
            int32_t _1304 = _538 + 1;
            int32_t _1305 = _1303 * _1304;
            int32_t _1306 = _1302 + _1305;
            int32_t _1307 = _1306 + 1;
            float _1308 = _f4[_1307];
            float _1309 = _1308 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1310 = _1267 + 2;
            int32_t _1311 = min(_1310, 2047);
            int32_t _1312 = max(_1311, 0);
            int32_t _1313 = _1312 - _516;
            int32_t _1314 = _1313 + _1305;
            float _1315 = _f4[_1314];
            float _1316 = _1315 * float_from_bits(1048576000 /* 0.25 */);
            float _1317 = _1309 + _1316;
            int32_t _1318 = min(_1267, 2047);
            int32_t _1319 = max(_1318, 0);
            int32_t _1320 = _1319 - _516;
            int32_t _1321 = _1320 + _1305;
            float _1322 = _f4[_1321];
            float _1323 = _1322 * float_from_bits(1048576000 /* 0.25 */);
            float _1324 = _1317 + _1323;
            _f1[_1301] = _1324;
            int32_t _1325 = _1267 - _631;
            int32_t _1326 = _f1_s0_y - _595;
            int32_t _1327 = _638 + 1;
            int32_t _1328 = _1326 * _1327;
            int32_t _1329 = _1325 + _1328;
            int32_t _1330 = _1329 + 2;
            int32_t _1331 = _1267 - _516;
            int32_t _1332 = _f1_s0_y - _454;
            int32_t _1333 = _538 + 1;
            int32_t _1334 = _1332 * _1333;
            int32_t _1335 = _1331 + _1334;
            int32_t _1336 = _1335 + 2;
            float _1337 = _f4[_1336];
            float _1338 = _1337 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1339 = _1267 + 3;
            int32_t _1340 = min(_1339, 2047);
            int32_t _1341 = max(_1340, 0);
            int32_t _1342 = _1341 - _516;
            int32_t _1343 = _1342 + _1334;
            float _1344 = _f4[_1343];
            float _1345 = _1344 * float_from_bits(1048576000 /* 0.25 */);
            float _1346 = _1338 + _1345;
            int32_t _1347 = _1267 + 1;
            int32_t _1348 = min(_1347, 2047);
            int32_t _1349 = max(_1348, 0);
            int32_t _1350 = _1349 - _516;
            int32_t _1351 = _1350 + _1334;
            float _1352 = _f4[_1351];
            float _1353 = _1352 * float_from_bits(1048576000 /* 0.25 */);
            float _1354 = _1346 + _1353;
            _f1[_1330] = _1354;
            int32_t _1355 = _1267 - _631;
            int32_t _1356 = _f1_s0_y - _595;
            int32_t _1357 = _638 + 1;
            int32_t _1358 = _1356 * _1357;
            int32_t _1359 = _1355 + _1358;
            int32_t _1360 = _1359 + 3;
            int32_t _1361 = _1267 - _516;
            int32_t _1362 = _f1_s0_y - _454;
            int32_t _1363 = _538 + 1;
            int32_t _1364 = _1362 * _1363;
            int32_t _1365 = _1361 + _1364;
            int32_t _1366 = _1365 + 3;
            float _1367 = _f4[_1366];
            float _1368 = _1367 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1369 = _1267 + 4;
            int32_t _1370 = min(_1369, 2047);
            int32_t _1371 = max(_1370, 0);
            int32_t _1372 = _1371 - _516;
            int32_t _1373 = _1372 + _1364;
            float _1374 = _f4[_1373];
            float _1375 = _1374 * float_from_bits(1048576000 /* 0.25 */);
            float _1376 = _1368 + _1375;
            int32_t _1377 = _1267 + 2;
            int32_t _1378 = min(_1377, 2047);
            int32_t _1379 = max(_1378, 0);
            int32_t _1380 = _1379 - _516;
            int32_t _1381 = _1380 + _1364;
            float _1382 = _f4[_1381];
            float _1383 = _1382 * float_from_bits(1048576000 /* 0.25 */);
            float _1384 = _1376 + _1383;
            _f1[_1360] = _1384;
           } // for _f1_s0_x_x
          } // for _f1_s0_y
          // consume f1
          // produce f5
          int32_t _1385 = _892 - _909;
          int32_t _1386 = _1385 + 1;
          for (int _f5_s0_y = _909; _f5_s0_y < _909 + _1386; _f5_s0_y++)
          {
           int32_t _1387 = _917 - _925;
           int32_t _1388 = _1387 + 4;
           int32_t _1389 = _1388 >> 2;
           for (int _f5_s0_x_x = 0; _f5_s0_x_x < 0 + _1389; _f5_s0_x_x++)
           {
            int32_t _1390 = _f5_s0_x_x * 4;
            int32_t _1391 = _1390 + _925;
            int32_t _1392 = _917 + -3;
            int32_t _1393 = min(_1391, _1392);
            int32_t _1394 = _1393 - _707;
            int32_t _1395 = _f5_s0_y - _669;
            int32_t _1396 = _729 + 1;
            int32_t _1397 = _1395 * _1396;
            int32_t _1398 = _1394 + _1397;
            int32_t _1399 = _1393 - _631;
            int32_t _1400 = _f5_s0_y - _595;
            int32_t _1401 = _638 + 1;
            int32_t _1402 = _1400 * _1401;
            int32_t _1403 = _1399 + _1402;
            float _1404 = _f1[_1403];
            float _1405 = _1404 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1406 = _f5_s0_y + 1;
            int32_t _1407 = min(_1406, 2047);
            int32_t _1408 = max(_1407, 0);
            int32_t _1409 = _1408 - _595;
            int32_t _1410 = _1409 * _1401;
            int32_t _1411 = _1399 + _1410;
            float _1412 = _f1[_1411];
            float _1413 = _1412 * float_from_bits(1048576000 /* 0.25 */);
            float _1414 = _1405 + _1413;
            int32_t _1415 = _f5_s0_y + -1;
            int32_t _1416 = min(_1415, 2047);
            int32_t _1417 = max(_1416, 0);
            int32_t _1418 = _1417 - _595;
            int32_t _1419 = _1418 * _1401;
            int32_t _1420 = _1399 + _1419;
            float _1421 = _f1[_1420];
            float _1422 = _1421 * float_from_bits(1048576000 /* 0.25 */);
            float _1423 = _1414 + _1422;
            _f5[_1398] = _1423;
            int32_t _1424 = _1393 - _707;
            int32_t _1425 = _f5_s0_y - _669;
            int32_t _1426 = _729 + 1;
            int32_t _1427 = _1425 * _1426;
            int32_t _1428 = _1424 + _1427;
            int32_t _1429 = _1428 + 1;
            int32_t _1430 = _1393 - _631;
            int32_t _1431 = _f5_s0_y - _595;
            int32_t _1432 = _638 + 1;
            int32_t _1433 = _1431 * _1432;
            int32_t _1434 = _1430 + _1433;
            int32_t _1435 = _1434 + 1;
            float _1436 = _f1[_1435];
            float _1437 = _1436 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1438 = _f5_s0_y + 1;
            int32_t _1439 = min(_1438, 2047);
            int32_t _1440 = max(_1439, 0);
            int32_t _1441 = _1440 - _595;
            int32_t _1442 = _1441 * _1432;
            int32_t _1443 = _1430 + _1442;
            int32_t _1444 = _1443 + 1;
            float _1445 = _f1[_1444];
            float _1446 = _1445 * float_from_bits(1048576000 /* 0.25 */);
            float _1447 = _1437 + _1446;
            int32_t _1448 = _f5_s0_y + -1;
            int32_t _1449 = min(_1448, 2047);
            int32_t _1450 = max(_1449, 0);
            int32_t _1451 = _1450 - _595;
            int32_t _1452 = _1451 * _1432;
            int32_t _1453 = _1430 + _1452;
            int32_t _1454 = _1453 + 1;
            float _1455 = _f1[_1454];
            float _1456 = _1455 * float_from_bits(1048576000 /* 0.25 */);
            float _1457 = _1447 + _1456;
            _f5[_1429] = _1457;
            int32_t _1458 = _1393 - _707;
            int32_t _1459 = _f5_s0_y - _669;
            int32_t _1460 = _729 + 1;
            int32_t _1461 = _1459 * _1460;
            int32_t _1462 = _1458 + _1461;
            int32_t _1463 = _1462 + 2;
            int32_t _1464 = _1393 - _631;
            int32_t _1465 = _f5_s0_y - _595;
            int32_t _1466 = _638 + 1;
            int32_t _1467 = _1465 * _1466;
            int32_t _1468 = _1464 + _1467;
            int32_t _1469 = _1468 + 2;
            float _1470 = _f1[_1469];
            float _1471 = _1470 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1472 = _f5_s0_y + 1;
            int32_t _1473 = min(_1472, 2047);
            int32_t _1474 = max(_1473, 0);
            int32_t _1475 = _1474 - _595;
            int32_t _1476 = _1475 * _1466;
            int32_t _1477 = _1464 + _1476;
            int32_t _1478 = _1477 + 2;
            float _1479 = _f1[_1478];
            float _1480 = _1479 * float_from_bits(1048576000 /* 0.25 */);
            float _1481 = _1471 + _1480;
            int32_t _1482 = _f5_s0_y + -1;
            int32_t _1483 = min(_1482, 2047);
            int32_t _1484 = max(_1483, 0);
            int32_t _1485 = _1484 - _595;
            int32_t _1486 = _1485 * _1466;
            int32_t _1487 = _1464 + _1486;
            int32_t _1488 = _1487 + 2;
            float _1489 = _f1[_1488];
            float _1490 = _1489 * float_from_bits(1048576000 /* 0.25 */);
            float _1491 = _1481 + _1490;
            _f5[_1463] = _1491;
            int32_t _1492 = _1393 - _707;
            int32_t _1493 = _f5_s0_y - _669;
            int32_t _1494 = _729 + 1;
            int32_t _1495 = _1493 * _1494;
            int32_t _1496 = _1492 + _1495;
            int32_t _1497 = _1496 + 3;
            int32_t _1498 = _1393 - _631;
            int32_t _1499 = _f5_s0_y - _595;
            int32_t _1500 = _638 + 1;
            int32_t _1501 = _1499 * _1500;
            int32_t _1502 = _1498 + _1501;
            int32_t _1503 = _1502 + 3;
            float _1504 = _f1[_1503];
            float _1505 = _1504 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1506 = _f5_s0_y + 1;
            int32_t _1507 = min(_1506, 2047);
            int32_t _1508 = max(_1507, 0);
            int32_t _1509 = _1508 - _595;
            int32_t _1510 = _1509 * _1500;
            int32_t _1511 = _1498 + _1510;
            int32_t _1512 = _1511 + 3;
            float _1513 = _f1[_1512];
            float _1514 = _1513 * float_from_bits(1048576000 /* 0.25 */);
            float _1515 = _1505 + _1514;
            int32_t _1516 = _f5_s0_y + -1;
            int32_t _1517 = min(_1516, 2047);
            int32_t _1518 = max(_1517, 0);
            int32_t _1519 = _1518 - _595;
            int32_t _1520 = _1519 * _1500;
            int32_t _1521 = _1498 + _1520;
            int32_t _1522 = _1521 + 3;
            float _1523 = _f1[_1522];
            float _1524 = _1523 * float_from_bits(1048576000 /* 0.25 */);
            float _1525 = _1515 + _1524;
            _f5[_1497] = _1525;
           } // for _f5_s0_x_x
          } // for _f5_s0_y
          // consume f5
          // produce f2
          int32_t _1526 = _892 - _909;
          int32_t _1527 = _1526 + 1;
          for (int _f2_s0_y = _909; _f2_s0_y < _909 + _1527; _f2_s0_y++)
          {
           int32_t _1528 = _265 - _273;
           int32_t _1529 = _1528 + 4;
           int32_t _1530 = _1529 >> 2;
           for (int _f2_s0_x_x = 0; _f2_s0_x_x < 0 + _1530; _f2_s0_x_x++)
           {
            int32_t _1531 = _f2_s0_x_x * 4;
            int32_t _1532 = _1531 + _273;
            int32_t _1533 = _265 + -3;
            int32_t _1534 = min(_1532, _1533);
            int32_t _1535 = _1534 - _782;
            int32_t _1536 = _f2_s0_y - _770;
            int32_t _1537 = _789 + 1;
            int32_t _1538 = _1536 * _1537;
            int32_t _1539 = _1535 + _1538;
            int32_t _1540 = _1534 - _707;
            int32_t _1541 = _f2_s0_y - _669;
            int32_t _1542 = _729 + 1;
            int32_t _1543 = _1541 * _1542;
            int32_t _1544 = _1540 + _1543;
            float _1545 = _f5[_1544];
            float _1546 = _1545 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1547 = _1534 + 1;
            int32_t _1548 = min(_1547, 2047);
            int32_t _1549 = max(_1548, 0);
            int32_t _1550 = _1549 - _707;
            int32_t _1551 = _1550 + _1543;
            float _1552 = _f5[_1551];
            float _1553 = _1552 * float_from_bits(1048576000 /* 0.25 */);
            float _1554 = _1546 + _1553;
            int32_t _1555 = _1534 + -1;
            int32_t _1556 = min(_1555, 2047);
            int32_t _1557 = max(_1556, 0);
            int32_t _1558 = _1557 - _707;
            int32_t _1559 = _1558 + _1543;
            float _1560 = _f5[_1559];
            float _1561 = _1560 * float_from_bits(1048576000 /* 0.25 */);
            float _1562 = _1554 + _1561;
            _f2[_1539] = _1562;
            int32_t _1563 = _1534 - _782;
            int32_t _1564 = _f2_s0_y - _770;
            int32_t _1565 = _789 + 1;
            int32_t _1566 = _1564 * _1565;
            int32_t _1567 = _1563 + _1566;
            int32_t _1568 = _1567 + 1;
            int32_t _1569 = _1534 - _707;
            int32_t _1570 = _f2_s0_y - _669;
            int32_t _1571 = _729 + 1;
            int32_t _1572 = _1570 * _1571;
            int32_t _1573 = _1569 + _1572;
            int32_t _1574 = _1573 + 1;
            float _1575 = _f5[_1574];
            float _1576 = _1575 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1577 = _1534 + 2;
            int32_t _1578 = min(_1577, 2047);
            int32_t _1579 = max(_1578, 0);
            int32_t _1580 = _1579 - _707;
            int32_t _1581 = _1580 + _1572;
            float _1582 = _f5[_1581];
            float _1583 = _1582 * float_from_bits(1048576000 /* 0.25 */);
            float _1584 = _1576 + _1583;
            int32_t _1585 = min(_1534, 2047);
            int32_t _1586 = max(_1585, 0);
            int32_t _1587 = _1586 - _707;
            int32_t _1588 = _1587 + _1572;
            float _1589 = _f5[_1588];
            float _1590 = _1589 * float_from_bits(1048576000 /* 0.25 */);
            float _1591 = _1584 + _1590;
            _f2[_1568] = _1591;
            int32_t _1592 = _1534 - _782;
            int32_t _1593 = _f2_s0_y - _770;
            int32_t _1594 = _789 + 1;
            int32_t _1595 = _1593 * _1594;
            int32_t _1596 = _1592 + _1595;
            int32_t _1597 = _1596 + 2;
            int32_t _1598 = _1534 - _707;
            int32_t _1599 = _f2_s0_y - _669;
            int32_t _1600 = _729 + 1;
            int32_t _1601 = _1599 * _1600;
            int32_t _1602 = _1598 + _1601;
            int32_t _1603 = _1602 + 2;
            float _1604 = _f5[_1603];
            float _1605 = _1604 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1606 = _1534 + 3;
            int32_t _1607 = min(_1606, 2047);
            int32_t _1608 = max(_1607, 0);
            int32_t _1609 = _1608 - _707;
            int32_t _1610 = _1609 + _1601;
            float _1611 = _f5[_1610];
            float _1612 = _1611 * float_from_bits(1048576000 /* 0.25 */);
            float _1613 = _1605 + _1612;
            int32_t _1614 = _1534 + 1;
            int32_t _1615 = min(_1614, 2047);
            int32_t _1616 = max(_1615, 0);
            int32_t _1617 = _1616 - _707;
            int32_t _1618 = _1617 + _1601;
            float _1619 = _f5[_1618];
            float _1620 = _1619 * float_from_bits(1048576000 /* 0.25 */);
            float _1621 = _1613 + _1620;
            _f2[_1597] = _1621;
            int32_t _1622 = _1534 - _782;
            int32_t _1623 = _f2_s0_y - _770;
            int32_t _1624 = _789 + 1;
            int32_t _1625 = _1623 * _1624;
            int32_t _1626 = _1622 + _1625;
            int32_t _1627 = _1626 + 3;
            int32_t _1628 = _1534 - _707;
            int32_t _1629 = _f2_s0_y - _669;
            int32_t _1630 = _729 + 1;
            int32_t _1631 = _1629 * _1630;
            int32_t _1632 = _1628 + _1631;
            int32_t _1633 = _1632 + 3;
            float _1634 = _f5[_1633];
            float _1635 = _1634 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1636 = _1534 + 4;
            int32_t _1637 = min(_1636, 2047);
            int32_t _1638 = max(_1637, 0);
            int32_t _1639 = _1638 - _707;
            int32_t _1640 = _1639 + _1631;
            float _1641 = _f5[_1640];
            float _1642 = _1641 * float_from_bits(1048576000 /* 0.25 */);
            float _1643 = _1635 + _1642;
            int32_t _1644 = _1534 + 2;
            int32_t _1645 = min(_1644, 2047);
            int32_t _1646 = max(_1645, 0);
            int32_t _1647 = _1646 - _707;
            int32_t _1648 = _1647 + _1631;
            float _1649 = _f5[_1648];
            float _1650 = _1649 * float_from_bits(1048576000 /* 0.25 */);
            float _1651 = _1643 + _1650;
            _f2[_1627] = _1651;
           } // for _f2_s0_x_x
          } // for _f2_s0_y
          // consume f2
          // produce f6
          int32_t _1652 = _872 - _884;
          int32_t _1653 = _1652 + 1;
          for (int _f6_s0_y = _884; _f6_s0_y < _884 + _1653; _f6_s0_y++)
          {
           int32_t _1654 = _265 - _273;
           int32_t _1655 = _1654 + 4;
           int32_t _1656 = _1655 >> 2;
           for (int _f6_s0_x_x = 0; _f6_s0_x_x < 0 + _1656; _f6_s0_x_x++)
           {
            int32_t _1657 = _f6_s0_x_x * 4;
            int32_t _1658 = _1657 + _273;
            int32_t _1659 = _265 + -3;
            int32_t _1660 = min(_1658, _1659);
            int32_t _1661 = _1660 - _816;
            int32_t _1662 = _f6_s0_y - _804;
            int32_t _1663 = _833 + 1;
            int32_t _1664 = _1662 * _1663;
            int32_t _1665 = _1661 + _1664;
            int32_t _1666 = _1660 - _782;
            int32_t _1667 = _f6_s0_y - _770;
            int32_t _1668 = _789 + 1;
            int32_t _1669 = _1667 * _1668;
            int32_t _1670 = _1666 + _1669;
            float _1671 = _f2[_1670];
            float _1672 = _1671 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1673 = _f6_s0_y + 1;
            int32_t _1674 = min(_1673, 2047);
            int32_t _1675 = max(_1674, 0);
            int32_t _1676 = _1675 - _770;
            int32_t _1677 = _1676 * _1668;
            int32_t _1678 = _1666 + _1677;
            float _1679 = _f2[_1678];
            float _1680 = _1679 * float_from_bits(1048576000 /* 0.25 */);
            float _1681 = _1672 + _1680;
            int32_t _1682 = _f6_s0_y + -1;
            int32_t _1683 = min(_1682, 2047);
            int32_t _1684 = max(_1683, 0);
            int32_t _1685 = _1684 - _770;
            int32_t _1686 = _1685 * _1668;
            int32_t _1687 = _1666 + _1686;
            float _1688 = _f2[_1687];
            float _1689 = _1688 * float_from_bits(1048576000 /* 0.25 */);
            float _1690 = _1681 + _1689;
            _f6[_1665] = _1690;
            int32_t _1691 = _1660 - _816;
            int32_t _1692 = _f6_s0_y - _804;
            int32_t _1693 = _833 + 1;
            int32_t _1694 = _1692 * _1693;
            int32_t _1695 = _1691 + _1694;
            int32_t _1696 = _1695 + 1;
            int32_t _1697 = _1660 - _782;
            int32_t _1698 = _f6_s0_y - _770;
            int32_t _1699 = _789 + 1;
            int32_t _1700 = _1698 * _1699;
            int32_t _1701 = _1697 + _1700;
            int32_t _1702 = _1701 + 1;
            float _1703 = _f2[_1702];
            float _1704 = _1703 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1705 = _f6_s0_y + 1;
            int32_t _1706 = min(_1705, 2047);
            int32_t _1707 = max(_1706, 0);
            int32_t _1708 = _1707 - _770;
            int32_t _1709 = _1708 * _1699;
            int32_t _1710 = _1697 + _1709;
            int32_t _1711 = _1710 + 1;
            float _1712 = _f2[_1711];
            float _1713 = _1712 * float_from_bits(1048576000 /* 0.25 */);
            float _1714 = _1704 + _1713;
            int32_t _1715 = _f6_s0_y + -1;
            int32_t _1716 = min(_1715, 2047);
            int32_t _1717 = max(_1716, 0);
            int32_t _1718 = _1717 - _770;
            int32_t _1719 = _1718 * _1699;
            int32_t _1720 = _1697 + _1719;
            int32_t _1721 = _1720 + 1;
            float _1722 = _f2[_1721];
            float _1723 = _1722 * float_from_bits(1048576000 /* 0.25 */);
            float _1724 = _1714 + _1723;
            _f6[_1696] = _1724;
            int32_t _1725 = _1660 - _816;
            int32_t _1726 = _f6_s0_y - _804;
            int32_t _1727 = _833 + 1;
            int32_t _1728 = _1726 * _1727;
            int32_t _1729 = _1725 + _1728;
            int32_t _1730 = _1729 + 2;
            int32_t _1731 = _1660 - _782;
            int32_t _1732 = _f6_s0_y - _770;
            int32_t _1733 = _789 + 1;
            int32_t _1734 = _1732 * _1733;
            int32_t _1735 = _1731 + _1734;
            int32_t _1736 = _1735 + 2;
            float _1737 = _f2[_1736];
            float _1738 = _1737 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1739 = _f6_s0_y + 1;
            int32_t _1740 = min(_1739, 2047);
            int32_t _1741 = max(_1740, 0);
            int32_t _1742 = _1741 - _770;
            int32_t _1743 = _1742 * _1733;
            int32_t _1744 = _1731 + _1743;
            int32_t _1745 = _1744 + 2;
            float _1746 = _f2[_1745];
            float _1747 = _1746 * float_from_bits(1048576000 /* 0.25 */);
            float _1748 = _1738 + _1747;
            int32_t _1749 = _f6_s0_y + -1;
            int32_t _1750 = min(_1749, 2047);
            int32_t _1751 = max(_1750, 0);
            int32_t _1752 = _1751 - _770;
            int32_t _1753 = _1752 * _1733;
            int32_t _1754 = _1731 + _1753;
            int32_t _1755 = _1754 + 2;
            float _1756 = _f2[_1755];
            float _1757 = _1756 * float_from_bits(1048576000 /* 0.25 */);
            float _1758 = _1748 + _1757;
            _f6[_1730] = _1758;
            int32_t _1759 = _1660 - _816;
            int32_t _1760 = _f6_s0_y - _804;
            int32_t _1761 = _833 + 1;
            int32_t _1762 = _1760 * _1761;
            int32_t _1763 = _1759 + _1762;
            int32_t _1764 = _1763 + 3;
            int32_t _1765 = _1660 - _782;
            int32_t _1766 = _f6_s0_y - _770;
            int32_t _1767 = _789 + 1;
            int32_t _1768 = _1766 * _1767;
            int32_t _1769 = _1765 + _1768;
            int32_t _1770 = _1769 + 3;
            float _1771 = _f2[_1770];
            float _1772 = _1771 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1773 = _f6_s0_y + 1;
            int32_t _1774 = min(_1773, 2047);
            int32_t _1775 = max(_1774, 0);
            int32_t _1776 = _1775 - _770;
            int32_t _1777 = _1776 * _1767;
            int32_t _1778 = _1765 + _1777;
            int32_t _1779 = _1778 + 3;
            float _1780 = _f2[_1779];
            float _1781 = _1780 * float_from_bits(1048576000 /* 0.25 */);
            float _1782 = _1772 + _1781;
            int32_t _1783 = _f6_s0_y + -1;
            int32_t _1784 = min(_1783, 2047);
            int32_t _1785 = max(_1784, 0);
            int32_t _1786 = _1785 - _770;
            int32_t _1787 = _1786 * _1767;
            int32_t _1788 = _1765 + _1787;
            int32_t _1789 = _1788 + 3;
            float _1790 = _f2[_1789];
            float _1791 = _1790 * float_from_bits(1048576000 /* 0.25 */);
            float _1792 = _1782 + _1791;
            _f6[_1764] = _1792;
           } // for _f6_s0_x_x
          } // for _f6_s0_y
          // consume f6
          // produce f3
          int32_t _1793 = _872 - _884;
          int32_t _1794 = _1793 + 1;
          for (int _f3_s0_y = _884; _f3_s0_y < _884 + _1794; _f3_s0_y++)
          {
           for (int _f3_s0_x_x = 0; _f3_s0_x_x < 0 + 16; _f3_s0_x_x++)
           {
            int32_t _1795 = _f3_s0_x_x * 4;
            int32_t _1796 = min(_1795, 60);
            int32_t _1797 = _f3_s0_y - _856;
            int32_t _1798 = _1797 * 64;
            int32_t _1799 = _1796 + _1798;
            int32_t _1800 = _1796 + _233;
            int32_t _1801 = _1800 - _816;
            int32_t _1802 = _f3_s0_y - _804;
            int32_t _1803 = _833 + 1;
            int32_t _1804 = _1802 * _1803;
            int32_t _1805 = _1801 + _1804;
            float _1806 = _f6[_1805];
            float _1807 = _1806 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1808 = _1800 + 1;
            int32_t _1809 = min(_1808, 2047);
            int32_t _1810 = max(_1809, 0);
            int32_t _1811 = _1810 - _816;
            int32_t _1812 = _1811 + _1804;
            float _1813 = _f6[_1812];
            float _1814 = _1813 * float_from_bits(1048576000 /* 0.25 */);
            float _1815 = _1807 + _1814;
            int32_t _1816 = _1800 + -1;
            int32_t _1817 = min(_1816, 2047);
            int32_t _1818 = max(_1817, 0);
            int32_t _1819 = _1818 - _816;
            int32_t _1820 = _1819 + _1804;
            float _1821 = _f6[_1820];
            float _1822 = _1821 * float_from_bits(1048576000 /* 0.25 */);
            float _1823 = _1815 + _1822;
            _f3[_1799] = _1823;
            int32_t _1824 = _f3_s0_y - _856;
            int32_t _1825 = _1824 * 64;
            int32_t _1826 = _1796 + _1825;
            int32_t _1827 = _1826 + 1;
            int32_t _1828 = _1796 + _233;
            int32_t _1829 = _1828 - _816;
            int32_t _1830 = _f3_s0_y - _804;
            int32_t _1831 = _833 + 1;
            int32_t _1832 = _1830 * _1831;
            int32_t _1833 = _1829 + _1832;
            int32_t _1834 = _1833 + 1;
            float _1835 = _f6[_1834];
            float _1836 = _1835 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1837 = _1828 + 2;
            int32_t _1838 = min(_1837, 2047);
            int32_t _1839 = max(_1838, 0);
            int32_t _1840 = _1839 - _816;
            int32_t _1841 = _1840 + _1832;
            float _1842 = _f6[_1841];
            float _1843 = _1842 * float_from_bits(1048576000 /* 0.25 */);
            float _1844 = _1836 + _1843;
            int32_t _1845 = min(_1828, 2047);
            int32_t _1846 = max(_1845, 0);
            int32_t _1847 = _1846 - _816;
            int32_t _1848 = _1847 + _1832;
            float _1849 = _f6[_1848];
            float _1850 = _1849 * float_from_bits(1048576000 /* 0.25 */);
            float _1851 = _1844 + _1850;
            _f3[_1827] = _1851;
            int32_t _1852 = _f3_s0_y - _856;
            int32_t _1853 = _1852 * 64;
            int32_t _1854 = _1796 + _1853;
            int32_t _1855 = _1854 + 2;
            int32_t _1856 = _1796 + _233;
            int32_t _1857 = _1856 - _816;
            int32_t _1858 = _f3_s0_y - _804;
            int32_t _1859 = _833 + 1;
            int32_t _1860 = _1858 * _1859;
            int32_t _1861 = _1857 + _1860;
            int32_t _1862 = _1861 + 2;
            float _1863 = _f6[_1862];
            float _1864 = _1863 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1865 = _1856 + 3;
            int32_t _1866 = min(_1865, 2047);
            int32_t _1867 = max(_1866, 0);
            int32_t _1868 = _1867 - _816;
            int32_t _1869 = _1868 + _1860;
            float _1870 = _f6[_1869];
            float _1871 = _1870 * float_from_bits(1048576000 /* 0.25 */);
            float _1872 = _1864 + _1871;
            int32_t _1873 = _1856 + 1;
            int32_t _1874 = min(_1873, 2047);
            int32_t _1875 = max(_1874, 0);
            int32_t _1876 = _1875 - _816;
            int32_t _1877 = _1876 + _1860;
            float _1878 = _f6[_1877];
            float _1879 = _1878 * float_from_bits(1048576000 /* 0.25 */);
            float _1880 = _1872 + _1879;
            _f3[_1855] = _1880;
            int32_t _1881 = _f3_s0_y - _856;
            int32_t _1882 = _1881 * 64;
            int32_t _1883 = _1796 + _1882;
            int32_t _1884 = _1883 + 3;
            int32_t _1885 = _1796 + _233;
            int32_t _1886 = _1885 - _816;
            int32_t _1887 = _f3_s0_y - _804;
            int32_t _1888 = _833 + 1;
            int32_t _1889 = _1887 * _1888;
            int32_t _1890 = _1886 + _1889;
            int32_t _1891 = _1890 + 3;
            float _1892 = _f6[_1891];
            float _1893 = _1892 * float_from_bits(1056964608 /* 0.5 */);
            int32_t _1894 = _1885 + 4;
            int32_t _1895 = min(_1894, 2047);
            int32_t _1896 = max(_1895, 0);
            int32_t _1897 = _1896 - _816;
            int32_t _1898 = _1897 + _1889;
            float _1899 = _f6[_1898];
            float _1900 = _1899 * float_from_bits(1048576000 /* 0.25 */);
            float _1901 = _1893 + _1900;
            int32_t _1902 = _1885 + 2;
            int32_t _1903 = min(_1902, 2047);
            int32_t _1904 = max(_1903, 0);
            int32_t _1905 = _1904 - _816;
            int32_t _1906 = _1905 + _1889;
            float _1907 = _f6[_1906];
            float _1908 = _1907 * float_from_bits(1048576000 /* 0.25 */);
            float _1909 = _1901 + _1908;
            _f3[_1884] = _1909;
           } // for _f3_s0_x_x
          } // for _f3_s0_y
          // consume f3
          for (int _f7_s0_x_xi_xi = 0; _f7_s0_x_xi_xi < 0 + 16; _f7_s0_x_xi_xi++)
          {
           int32_t _1910 = _f7_s0_x_xi_xi * 4;
           int32_t _1911 = _233 + _1910;
           int32_t _1912 = _1911 - _f7_min_0;
           int32_t _1913 = _239 + _f7_s0_y_yi;
           int32_t _1914 = _1913 - _f7_min_1;
           int32_t _1915 = _1914 * _f7_stride_1;
           int32_t _1916 = _1912 + _1915;
           int32_t _1917 = _1913 - _856;
           int32_t _1918 = _1917 * 64;
           int32_t _1919 = _1910 + _1918;
           float _1920 = _f3[_1919];
           float _1921 = _1920 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1922 = _1913 + 1;
           int32_t _1923 = min(_1922, 2047);
           int32_t _1924 = max(_1923, 0);
           int32_t _1925 = _1924 - _856;
           int32_t _1926 = _1925 * 64;
           int32_t _1927 = _1910 + _1926;
           float _1928 = _f3[_1927];
           float _1929 = _1928 * float_from_bits(1048576000 /* 0.25 */);
           float _1930 = _1921 + _1929;
           int32_t _1931 = _1913 + -1;
           int32_t _1932 = min(_1931, 2047);
           int32_t _1933 = max(_1932, 0);
           int32_t _1934 = _1933 - _856;
           int32_t _1935 = _1934 * 64;
           int32_t _1936 = _1910 + _1935;
           float _1937 = _f3[_1936];
           float _1938 = _1937 * float_from_bits(1048576000 /* 0.25 */);
           float _1939 = _1930 + _1938;
           _f7[_1916] = _1939;
           int32_t _1940 = _f7_s0_x_xi_xi * 4;
           int32_t _1941 = _233 + _1940;
           int32_t _1942 = _1941 - _f7_min_0;
           int32_t _1943 = _239 + _f7_s0_y_yi;
           int32_t _1944 = _1943 - _f7_min_1;
           int32_t _1945 = _1944 * _f7_stride_1;
           int32_t _1946 = _1942 + _1945;
           int32_t _1947 = _1946 + 1;
           int32_t _1948 = _1943 - _856;
           int32_t _1949 = _1948 * 64;
           int32_t _1950 = _1940 + _1949;
           int32_t _1951 = _1950 + 1;
           float _1952 = _f3[_1951];
           float _1953 = _1952 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1954 = _1943 + 1;
           int32_t _1955 = min(_1954, 2047);
           int32_t _1956 = max(_1955, 0);
           int32_t _1957 = _1956 - _856;
           int32_t _1958 = _1957 * 64;
           int32_t _1959 = _1940 + _1958;
           int32_t _1960 = _1959 + 1;
           float _1961 = _f3[_1960];
           float _1962 = _1961 * float_from_bits(1048576000 /* 0.25 */);
           float _1963 = _1953 + _1962;
           int32_t _1964 = _1943 + -1;
           int32_t _1965 = min(_1964, 2047);
           int32_t _1966 = max(_1965, 0);
           int32_t _1967 = _1966 - _856;
           int32_t _1968 = _1967 * 64;
           int32_t _1969 = _1940 + _1968;
           int32_t _1970 = _1969 + 1;
           float _1971 = _f3[_1970];
           float _1972 = _1971 * float_from_bits(1048576000 /* 0.25 */);
           float _1973 = _1963 + _1972;
           _f7[_1947] = _1973;
           int32_t _1974 = _f7_s0_x_xi_xi * 4;
           int32_t _1975 = _233 + _1974;
           int32_t _1976 = _1975 - _f7_min_0;
           int32_t _1977 = _239 + _f7_s0_y_yi;
           int32_t _1978 = _1977 - _f7_min_1;
           int32_t _1979 = _1978 * _f7_stride_1;
           int32_t _1980 = _1976 + _1979;
           int32_t _1981 = _1980 + 2;
           int32_t _1982 = _1977 - _856;
           int32_t _1983 = _1982 * 64;
           int32_t _1984 = _1974 + _1983;
           int32_t _1985 = _1984 + 2;
           float _1986 = _f3[_1985];
           float _1987 = _1986 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _1988 = _1977 + 1;
           int32_t _1989 = min(_1988, 2047);
           int32_t _1990 = max(_1989, 0);
           int32_t _1991 = _1990 - _856;
           int32_t _1992 = _1991 * 64;
           int32_t _1993 = _1974 + _1992;
           int32_t _1994 = _1993 + 2;
           float _1995 = _f3[_1994];
           float _1996 = _1995 * float_from_bits(1048576000 /* 0.25 */);
           float _1997 = _1987 + _1996;
           int32_t _1998 = _1977 + -1;
           int32_t _1999 = min(_1998, 2047);
           int32_t _2000 = max(_1999, 0);
           int32_t _2001 = _2000 - _856;
           int32_t _2002 = _2001 * 64;
           int32_t _2003 = _1974 + _2002;
           int32_t _2004 = _2003 + 2;
           float _2005 = _f3[_2004];
           float _2006 = _2005 * float_from_bits(1048576000 /* 0.25 */);
           float _2007 = _1997 + _2006;
           _f7[_1981] = _2007;
           int32_t _2008 = _f7_s0_x_xi_xi * 4;
           int32_t _2009 = _233 + _2008;
           int32_t _2010 = _2009 - _f7_min_0;
           int32_t _2011 = _239 + _f7_s0_y_yi;
           int32_t _2012 = _2011 - _f7_min_1;
           int32_t _2013 = _2012 * _f7_stride_1;
           int32_t _2014 = _2010 + _2013;
           int32_t _2015 = _2014 + 3;
           int32_t _2016 = _2011 - _856;
           int32_t _2017 = _2016 * 64;
           int32_t _2018 = _2008 + _2017;
           int32_t _2019 = _2018 + 3;
           float _2020 = _f3[_2019];
           float _2021 = _2020 * float_from_bits(1056964608 /* 0.5 */);
           int32_t _2022 = _2011 + 1;
           int32_t _2023 = min(_2022, 2047);
           int32_t _2024 = max(_2023, 0);
           int32_t _2025 = _2024 - _856;
           int32_t _2026 = _2025 * 64;
           int32_t _2027 = _2008 + _2026;
           int32_t _2028 = _2027 + 3;
           float _2029 = _f3[_2028];
           float _2030 = _2029 * float_from_bits(1048576000 /* 0.25 */);
           float _2031 = _2021 + _2030;
           int32_t _2032 = _2011 + -1;
           int32_t _2033 = min(_2032, 2047);
           int32_t _2034 = max(_2033, 0);
           int32_t _2035 = _2034 - _856;
           int32_t _2036 = _2035 * 64;
           int32_t _2037 = _2008 + _2036;
           int32_t _2038 = _2037 + 3;
           float _2039 = _f3[_2038];
           float _2040 = _2039 * float_from_bits(1048576000 /* 0.25 */);
           float _2041 = _2031 + _2040;
           _f7[_2015] = _2041;
          } // for _f7_s0_x_xi_xi
         } // for _f7_s0_y_yi
         halide_free(NULL, _f0);
         halide_free(NULL, _f4);
         halide_free(NULL, _f1);
         halide_free(NULL, _f5);
         halide_free(NULL, _f2);
         halide_free(NULL, _f6);
         halide_free(NULL, _f3);
        } // alloc _f3
       } // alloc _f6
      } // alloc _f2
     } // alloc _f5
    } // alloc _f1
   } // alloc _f4
  } // alloc _f0
 } // for _f7_s0_x_xo_nid
 // consume f7
} // if _172
return 0;
}
