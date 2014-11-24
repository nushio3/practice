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


extern "C" int main_compute(buffer_t *_inPar_buffer, buffer_t *_f3_buffer) {
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
float *_f3 = (float *)(_f3_buffer->host);
const bool _f3_host_and_dev_are_null = (_f3_buffer->host == NULL) && (_f3_buffer->dev == 0);
(void)_f3_host_and_dev_are_null;
const int32_t _f3_min_0 = _f3_buffer->min[0];
(void)_f3_min_0;
const int32_t _f3_min_1 = _f3_buffer->min[1];
(void)_f3_min_1;
const int32_t _f3_min_2 = _f3_buffer->min[2];
(void)_f3_min_2;
const int32_t _f3_min_3 = _f3_buffer->min[3];
(void)_f3_min_3;
const int32_t _f3_extent_0 = _f3_buffer->extent[0];
(void)_f3_extent_0;
const int32_t _f3_extent_1 = _f3_buffer->extent[1];
(void)_f3_extent_1;
const int32_t _f3_extent_2 = _f3_buffer->extent[2];
(void)_f3_extent_2;
const int32_t _f3_extent_3 = _f3_buffer->extent[3];
(void)_f3_extent_3;
const int32_t _f3_stride_0 = _f3_buffer->stride[0];
(void)_f3_stride_0;
const int32_t _f3_stride_1 = _f3_buffer->stride[1];
(void)_f3_stride_1;
const int32_t _f3_stride_2 = _f3_buffer->stride[2];
(void)_f3_stride_2;
const int32_t _f3_stride_3 = _f3_buffer->stride[3];
(void)_f3_stride_3;
const int32_t _f3_elem_size = _f3_buffer->elem_size;
int32_t _0 = _f3_extent_0 + -1;
int32_t _1 = _0 >> 9;
int32_t _2 = _1 * 512;
int32_t _3 = _2 + _f3_min_0;
int32_t _4 = _3 + 512;
int32_t _5 = _f3_min_0 + _f3_extent_0;
int32_t _6 = min(_4, _5);
int32_t _7 = _5 + -512;
int32_t _8 = min(_f3_min_0, _7);
int32_t _9 = _6 - _8;
int32_t _10 = _f3_extent_0 + 511;
int32_t _11 = _10 >> 9;
int32_t _12 = _f3_extent_1 + 511;
int32_t _13 = _12 >> 9;
int32_t _14 = _11 * _13;
int32_t _15 = _14 + -1;
int32_t _16 = sdiv(_15, _11);
int32_t _17 = max(_16, 0);
int32_t _18 = _17 * 512;
int32_t _19 = _18 + _f3_min_1;
int32_t _20 = _19 + 512;
int32_t _21 = _f3_min_1 + _f3_extent_1;
int32_t _22 = min(_20, _21);
int32_t _23 = min(_16, 0);
int32_t _24 = _23 * 512;
int32_t _25 = _24 + _f3_min_1;
int32_t _26 = _21 + -512;
int32_t _27 = min(_25, _26);
int32_t _28 = _22 - _27;
int32_t _29 = min(_3, _7);
int32_t _30 = _29 + 510;
int32_t _31 = max(_6, _30);
int32_t _32 = min(_31, 8191);
int32_t _33 = max(_32, 0);
int32_t _34 = _29 + 511;
int32_t _35 = max(_33, _34);
int32_t _36 = min(_6, 8191);
int32_t _37 = max(_36, 0);
int32_t _38 = _37 + 1;
int32_t _39 = min(_38, 8191);
int32_t _40 = max(_39, 0);
int32_t _41 = max(_35, _40);
int32_t _42 = _37 + -1;
int32_t _43 = min(_42, 8191);
int32_t _44 = max(_43, 0);
int32_t _45 = max(_41, _44);
int32_t _46 = max(_45, _37);
int32_t _47 = min(_30, 8191);
int32_t _48 = max(_47, 0);
int32_t _49 = _48 + 1;
int32_t _50 = min(_49, 8191);
int32_t _51 = max(_50, 0);
int32_t _52 = max(_46, _51);
int32_t _53 = _48 + -1;
int32_t _54 = min(_53, 8191);
int32_t _55 = max(_54, 0);
int32_t _56 = max(_52, _55);
int32_t _57 = max(_56, _48);
int32_t _58 = max(_57, _34);
int32_t _59 = max(_58, _40);
int32_t _60 = max(_59, _44);
int32_t _61 = max(_60, _37);
int32_t _62 = max(_61, _51);
int32_t _63 = max(_62, _55);
int32_t _64 = max(_63, _48);
int32_t _65 = max(_64, _34);
int32_t _66 = max(_65, _40);
int32_t _67 = max(_66, _44);
int32_t _68 = max(_67, _51);
int32_t _69 = max(_68, _55);
int32_t _70 = max(_69, _34);
int32_t _71 = max(_70, _40);
int32_t _72 = max(_71, _44);
int32_t _73 = max(_72, _51);
int32_t _74 = max(_73, _55);
int32_t _75 = max(_74, _34);
int32_t _76 = max(_75, _40);
int32_t _77 = max(_76, _44);
int32_t _78 = max(_77, _51);
int32_t _79 = max(_78, _55);
int32_t _80 = _8 + -1;
int32_t _81 = min(_80, 8191);
int32_t _82 = max(_81, 0);
int32_t _83 = min(_82, _8);
int32_t _84 = _8 + 1;
int32_t _85 = min(_84, 8191);
int32_t _86 = max(_85, 0);
int32_t _87 = _86 + 1;
int32_t _88 = min(_87, 8191);
int32_t _89 = max(_88, 0);
int32_t _90 = min(_83, _89);
int32_t _91 = _86 + -1;
int32_t _92 = min(_91, 8191);
int32_t _93 = max(_92, 0);
int32_t _94 = min(_90, _93);
int32_t _95 = min(_94, _86);
int32_t _96 = _82 + 1;
int32_t _97 = min(_96, 8191);
int32_t _98 = max(_97, 0);
int32_t _99 = min(_95, _98);
int32_t _100 = _82 + -1;
int32_t _101 = min(_100, 8191);
int32_t _102 = max(_101, 0);
int32_t _103 = min(_99, _102);
int32_t _104 = min(_103, _82);
int32_t _105 = min(_104, _8);
int32_t _106 = min(_105, _89);
int32_t _107 = min(_106, _93);
int32_t _108 = min(_107, _86);
int32_t _109 = min(_108, _98);
int32_t _110 = min(_109, _102);
int32_t _111 = min(_110, _82);
int32_t _112 = min(_111, _8);
int32_t _113 = min(_112, _89);
int32_t _114 = min(_113, _93);
int32_t _115 = min(_114, _98);
int32_t _116 = min(_115, _102);
int32_t _117 = min(_116, _8);
int32_t _118 = min(_117, _89);
int32_t _119 = min(_118, _93);
int32_t _120 = min(_119, _98);
int32_t _121 = min(_120, _102);
int32_t _122 = min(_121, _8);
int32_t _123 = min(_122, _89);
int32_t _124 = min(_123, _93);
int32_t _125 = min(_124, _98);
int32_t _126 = min(_125, _102);
int32_t _127 = _79 - _126;
int32_t _128 = min(_19, _26);
int32_t _129 = _128 + 511;
int32_t _130 = min(_22, 8191);
int32_t _131 = max(_130, 0);
int32_t _132 = max(_129, _131);
int32_t _133 = _128 + 510;
int32_t _134 = min(_133, 8191);
int32_t _135 = max(_134, 0);
int32_t _136 = max(_132, _135);
int32_t _137 = max(_136, _129);
int32_t _138 = _131 + 1;
int32_t _139 = min(_138, 8191);
int32_t _140 = max(_139, 0);
int32_t _141 = max(_137, _140);
int32_t _142 = _131 + -1;
int32_t _143 = min(_142, 8191);
int32_t _144 = max(_143, 0);
int32_t _145 = max(_141, _144);
int32_t _146 = max(_145, _131);
int32_t _147 = max(_146, _135);
int32_t _148 = _135 + 1;
int32_t _149 = min(_148, 8191);
int32_t _150 = max(_149, 0);
int32_t _151 = max(_147, _150);
int32_t _152 = _135 + -1;
int32_t _153 = min(_152, 8191);
int32_t _154 = max(_153, 0);
int32_t _155 = max(_151, _154);
int32_t _156 = max(_155, _129);
int32_t _157 = max(_156, _140);
int32_t _158 = max(_157, _144);
int32_t _159 = max(_158, _150);
int32_t _160 = max(_159, _154);
int32_t _161 = _27 + 1;
int32_t _162 = min(_161, 8191);
int32_t _163 = max(_162, 0);
int32_t _164 = min(_27, _163);
int32_t _165 = _27 + -1;
int32_t _166 = min(_165, 8191);
int32_t _167 = max(_166, 0);
int32_t _168 = min(_164, _167);
int32_t _169 = min(_168, _27);
int32_t _170 = _163 + 1;
int32_t _171 = min(_170, 8191);
int32_t _172 = max(_171, 0);
int32_t _173 = min(_169, _172);
int32_t _174 = _163 + -1;
int32_t _175 = min(_174, 8191);
int32_t _176 = max(_175, 0);
int32_t _177 = min(_173, _176);
int32_t _178 = min(_177, _163);
int32_t _179 = min(_178, _167);
int32_t _180 = _167 + 1;
int32_t _181 = min(_180, 8191);
int32_t _182 = max(_181, 0);
int32_t _183 = min(_179, _182);
int32_t _184 = _167 + -1;
int32_t _185 = min(_184, 8191);
int32_t _186 = max(_185, 0);
int32_t _187 = min(_183, _186);
int32_t _188 = min(_187, _27);
int32_t _189 = min(_188, _172);
int32_t _190 = min(_189, _176);
int32_t _191 = min(_190, _182);
int32_t _192 = min(_191, _186);
int32_t _193 = _160 - _192;
if (_f3_host_and_dev_are_null)
{
 bool _194 = halide_rewrite_buffer(_f3_buffer, 4, _8, _9, 1, _27, _28, _9, 0, 0, 0, 0, 0, 0);
 (void)_194;
} // if _f3_host_and_dev_are_null
if (_inPar_host_and_dev_are_null)
{
 int32_t _195 = _127 + 1;
 int32_t _196 = _193 + 1;
 bool _197 = halide_rewrite_buffer(_inPar_buffer, 4, _126, _195, 1, _192, _196, _195, 0, 0, 0, 0, 0, 0);
 (void)_197;
} // if _inPar_host_and_dev_are_null
bool _198 = _f3_host_and_dev_are_null || _inPar_host_and_dev_are_null;
bool _199 = !(_198);
if (_199)
{
 bool _200 = _f3_elem_size == 4;
 if (!_200) {
  halide_printf(NULL, "Output buffer f3 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _f3_elem_size);
  return -1;
 }
 bool _201 = _inPar_elem_size == 4;
 if (!_201) {
  halide_printf(NULL, "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _inPar_elem_size);
  return -1;
 }
 bool _202 = _f3_min_0 <= _8;
 if (!_202) {
  halide_printf(NULL, "Output buffer f3 is accessed at %d, which is before the min (%d) in dimension 0\n", _8, _f3_min_0);
  return -1;
 }
 int32_t _203 = _8 + _9;
 int32_t _204 = _203 - _f3_extent_0;
 bool _205 = _204 <= _f3_min_0;
 int32_t _206 = _203 + -1;
 int32_t _207 = _f3_min_0 + _f3_extent_0;
 int32_t _208 = _207 + -1;
 if (!_205) {
  halide_printf(NULL, "Output buffer f3 is accessed at %d, which is beyond the max (%d) in dimension 0\n", _206, _208);
  return -1;
 }
 bool _209 = _f3_min_1 <= _27;
 if (!_209) {
  halide_printf(NULL, "Output buffer f3 is accessed at %d, which is before the min (%d) in dimension 1\n", _27, _f3_min_1);
  return -1;
 }
 int32_t _210 = _27 + _28;
 int32_t _211 = _210 - _f3_extent_1;
 bool _212 = _211 <= _f3_min_1;
 int32_t _213 = _210 + -1;
 int32_t _214 = _f3_min_1 + _f3_extent_1;
 int32_t _215 = _214 + -1;
 if (!_212) {
  halide_printf(NULL, "Output buffer f3 is accessed at %d, which is beyond the max (%d) in dimension 1\n", _213, _215);
  return -1;
 }
 bool _216 = _inPar_min_0 <= _126;
 if (!_216) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _126, _inPar_min_0);
  return -1;
 }
 int32_t _217 = _126 + _127;
 int32_t _218 = _217 - _inPar_extent_0;
 int32_t _219 = _218 + 1;
 bool _220 = _219 <= _inPar_min_0;
 int32_t _221 = _inPar_min_0 + _inPar_extent_0;
 int32_t _222 = _221 + -1;
 if (!_220) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n", _217, _222);
  return -1;
 }
 bool _223 = _inPar_min_1 <= _192;
 if (!_223) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _192, _inPar_min_1);
  return -1;
 }
 int32_t _224 = _192 + _193;
 int32_t _225 = _224 - _inPar_extent_1;
 int32_t _226 = _225 + 1;
 bool _227 = _226 <= _inPar_min_1;
 int32_t _228 = _inPar_min_1 + _inPar_extent_1;
 int32_t _229 = _228 + -1;
 if (!_227) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n", _224, _229);
  return -1;
 }
 bool _230 = _f3_stride_0 == 1;
 if (!_230) {
  halide_printf(NULL, "Static constraint violated: f3.stride.0 == 1\n");
  return -1;
 }
 bool _231 = _inPar_stride_0 == 1;
 if (!_231) {
  halide_printf(NULL, "Static constraint violated: inPar.stride.0 == 1\n");
  return -1;
 }
 int64_t _232 = (int64_t)(_f3_extent_0);
 int64_t _233 = (int64_t)(_f3_extent_1);
 int64_t _234 = (int64_t)(_inPar_extent_0);
 int64_t _235 = (int64_t)(_inPar_extent_1);
 int64_t _236 = (int64_t)(2147483647);
 bool _237 = _232 <= _236;
 if (!_237) {
  halide_printf(NULL, "Total allocation for buffer f3 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _238 = (int64_t)(_f3_stride_1);
 int64_t _239 = _233 * _238;
 bool _240 = _239 <= _236;
 if (!_240) {
  halide_printf(NULL, "Total allocation for buffer f3 exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _241 = _233 * _232;
 bool _242 = _241 <= _236;
 if (!_242) {
  halide_printf(NULL, "Product of extents for buffer f3 exceeds 2^31 - 1\n");
  return -1;
 }
 bool _243 = _234 <= _236;
 if (!_243) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _244 = (int64_t)(_inPar_stride_1);
 int64_t _245 = _235 * _244;
 bool _246 = _245 <= _236;
 if (!_246) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 int64_t _247 = _235 * _234;
 bool _248 = _247 <= _236;
 if (!_248) {
  halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
  return -1;
 }
 // produce f3
 int32_t _249 = _f3_extent_0 + 511;
 int32_t _250 = _249 >> 9;
 int32_t _251 = _f3_extent_1 + 511;
 int32_t _252 = _251 >> 9;
 int32_t _253 = _250 * _252;
 #pragma omp parallel for
 for (int _f3_s0_x_xo_nid = 0; _f3_s0_x_xo_nid < 0 + _253; _f3_s0_x_xo_nid++)
 {
  int32_t _254 = _f3_extent_0 + 511;
  int32_t _255 = _254 >> 9;
  int32_t _256 = mod(_f3_s0_x_xo_nid, _255);
  int32_t _257 = _256 * 512;
  int32_t _258 = _257 + _f3_min_0;
  int32_t _259 = _f3_min_0 + _f3_extent_0;
  int32_t _260 = _259 + -512;
  int32_t _261 = min(_258, _260);
  int32_t _262 = sdiv(_f3_s0_x_xo_nid, _255);
  int32_t _263 = _262 * 512;
  int32_t _264 = _263 + _f3_min_1;
  int32_t _265 = _f3_min_1 + _f3_extent_1;
  int32_t _266 = _265 + -512;
  int32_t _267 = min(_264, _266);
  for (int _f3_s0_y_yi_yi = 0; _f3_s0_y_yi_yi < 0 + 16; _f3_s0_y_yi_yi++)
  {
   for (int _f3_s0_x_xi_xi = 0; _f3_s0_x_xi_xi < 0 + 16; _f3_s0_x_xi_xi++)
   {
    for (int _f3_s0_y_yi_yii = 0; _f3_s0_y_yi_yii < 0 + 32; _f3_s0_y_yi_yii++)
    {
     for (int _f3_s0_x_xi_xii = 0; _f3_s0_x_xi_xii < 0 + 32; _f3_s0_x_xi_xii++)
     {
      int32_t _268 = _f3_s0_x_xi_xi * 32;
      int32_t _269 = _268 + _f3_s0_x_xi_xii;
      int32_t _270 = _261 + _269;
      int32_t _271 = _270 - _f3_min_0;
      int32_t _272 = _f3_s0_y_yi_yi * 32;
      int32_t _273 = _272 + _f3_s0_y_yi_yii;
      int32_t _274 = _267 + _273;
      int32_t _275 = _274 - _f3_min_1;
      int32_t _276 = _275 * _f3_stride_1;
      int32_t _277 = _271 + _276;
      int32_t _278 = _inPar_min_1 * _inPar_stride_1;
      int32_t _279 = _inPar_min_0 + _278;
      int32_t _280 = _270 + 1;
      int32_t _281 = min(_280, 8191);
      int32_t _282 = max(_281, 0);
      int32_t _283 = _274 + 1;
      int32_t _284 = min(_283, 8191);
      int32_t _285 = max(_284, 0);
      int32_t _286 = _270 + -1;
      int32_t _287 = min(_286, 8191);
      int32_t _288 = max(_287, 0);
      int32_t _289 = _274 + -1;
      int32_t _290 = min(_289, 8191);
      int32_t _291 = max(_290, 0);
      int32_t _292 = _274 * _inPar_stride_1;
      int32_t _293 = _282 + _292;
      int32_t _294 = _293 - _279;
      float _295 = ((float *)_inPar)[_294];
      int32_t _296 = _285 * _inPar_stride_1;
      int32_t _297 = _282 + _296;
      int32_t _298 = _297 - _279;
      float _299 = ((float *)_inPar)[_298];
      int32_t _300 = _282 + 1;
      int32_t _301 = min(_300, 8191);
      int32_t _302 = max(_301, 0);
      int32_t _303 = _282 + -1;
      int32_t _304 = min(_303, 8191);
      int32_t _305 = max(_304, 0);
      int32_t _306 = _291 * _inPar_stride_1;
      int32_t _307 = _282 + _306;
      int32_t _308 = _307 - _279;
      float _309 = ((float *)_inPar)[_308];
      int32_t _310 = _288 + _292;
      int32_t _311 = _310 - _279;
      float _312 = ((float *)_inPar)[_311];
      int32_t _313 = _288 + _296;
      int32_t _314 = _313 - _279;
      float _315 = ((float *)_inPar)[_314];
      int32_t _316 = _288 + 1;
      int32_t _317 = min(_316, 8191);
      int32_t _318 = max(_317, 0);
      int32_t _319 = _288 + -1;
      int32_t _320 = min(_319, 8191);
      int32_t _321 = max(_320, 0);
      int32_t _322 = _288 + _306;
      int32_t _323 = _322 - _279;
      float _324 = ((float *)_inPar)[_323];
      int32_t _325 = _270 + _296;
      int32_t _326 = _325 - _279;
      float _327 = ((float *)_inPar)[_326];
      float _328 = _327 * float_from_bits(1056964608 /* 0.5 */);
      float _329 = _299 * float_from_bits(1048576000 /* 0.25 */);
      float _330 = _328 + _329;
      float _331 = _315 * float_from_bits(1048576000 /* 0.25 */);
      float _332 = _330 + _331;
      int32_t _333 = _285 + 1;
      int32_t _334 = min(_333, 8191);
      int32_t _335 = max(_334, 0);
      int32_t _336 = _285 + -1;
      int32_t _337 = min(_336, 8191);
      int32_t _338 = max(_337, 0);
      float _339 = _299 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _340 = _302 + _296;
      int32_t _341 = _340 - _279;
      float _342 = ((float *)_inPar)[_341];
      float _343 = _342 * float_from_bits(1048576000 /* 0.25 */);
      float _344 = _339 + _343;
      int32_t _345 = _305 + _296;
      int32_t _346 = _345 - _279;
      float _347 = ((float *)_inPar)[_346];
      float _348 = _347 * float_from_bits(1048576000 /* 0.25 */);
      float _349 = _344 + _348;
      int32_t _350 = _335 * _inPar_stride_1;
      int32_t _351 = _282 + _350;
      int32_t _352 = _351 - _279;
      float _353 = ((float *)_inPar)[_352];
      int32_t _354 = _338 * _inPar_stride_1;
      int32_t _355 = _282 + _354;
      int32_t _356 = _355 - _279;
      float _357 = ((float *)_inPar)[_356];
      float _358 = _315 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _359 = _318 + _296;
      int32_t _360 = _359 - _279;
      float _361 = ((float *)_inPar)[_360];
      float _362 = _361 * float_from_bits(1048576000 /* 0.25 */);
      float _363 = _358 + _362;
      int32_t _364 = _321 + _296;
      int32_t _365 = _364 - _279;
      float _366 = ((float *)_inPar)[_365];
      float _367 = _366 * float_from_bits(1048576000 /* 0.25 */);
      float _368 = _363 + _367;
      int32_t _369 = _288 + _350;
      int32_t _370 = _369 - _279;
      float _371 = ((float *)_inPar)[_370];
      int32_t _372 = _288 + _354;
      int32_t _373 = _372 - _279;
      float _374 = ((float *)_inPar)[_373];
      int32_t _375 = _270 + _306;
      int32_t _376 = _375 - _279;
      float _377 = ((float *)_inPar)[_376];
      float _378 = _377 * float_from_bits(1056964608 /* 0.5 */);
      float _379 = _309 * float_from_bits(1048576000 /* 0.25 */);
      float _380 = _378 + _379;
      float _381 = _324 * float_from_bits(1048576000 /* 0.25 */);
      float _382 = _380 + _381;
      int32_t _383 = _291 + 1;
      int32_t _384 = min(_383, 8191);
      int32_t _385 = max(_384, 0);
      int32_t _386 = _291 + -1;
      int32_t _387 = min(_386, 8191);
      int32_t _388 = max(_387, 0);
      float _389 = _309 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _390 = _302 + _306;
      int32_t _391 = _390 - _279;
      float _392 = ((float *)_inPar)[_391];
      float _393 = _392 * float_from_bits(1048576000 /* 0.25 */);
      float _394 = _389 + _393;
      int32_t _395 = _305 + _306;
      int32_t _396 = _395 - _279;
      float _397 = ((float *)_inPar)[_396];
      float _398 = _397 * float_from_bits(1048576000 /* 0.25 */);
      float _399 = _394 + _398;
      int32_t _400 = _385 * _inPar_stride_1;
      int32_t _401 = _282 + _400;
      int32_t _402 = _401 - _279;
      float _403 = ((float *)_inPar)[_402];
      int32_t _404 = _388 * _inPar_stride_1;
      int32_t _405 = _282 + _404;
      int32_t _406 = _405 - _279;
      float _407 = ((float *)_inPar)[_406];
      float _408 = _324 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _409 = _318 + _306;
      int32_t _410 = _409 - _279;
      float _411 = ((float *)_inPar)[_410];
      float _412 = _411 * float_from_bits(1048576000 /* 0.25 */);
      float _413 = _408 + _412;
      int32_t _414 = _321 + _306;
      int32_t _415 = _414 - _279;
      float _416 = ((float *)_inPar)[_415];
      float _417 = _416 * float_from_bits(1048576000 /* 0.25 */);
      float _418 = _413 + _417;
      int32_t _419 = _288 + _400;
      int32_t _420 = _419 - _279;
      float _421 = ((float *)_inPar)[_420];
      int32_t _422 = _288 + _404;
      int32_t _423 = _422 - _279;
      float _424 = ((float *)_inPar)[_423];
      int32_t _425 = _270 + _292;
      int32_t _426 = _425 - _279;
      float _427 = ((float *)_inPar)[_426];
      float _428 = _427 * float_from_bits(1056964608 /* 0.5 */);
      float _429 = _295 * float_from_bits(1048576000 /* 0.25 */);
      float _430 = _428 + _429;
      float _431 = _312 * float_from_bits(1048576000 /* 0.25 */);
      float _432 = _430 + _431;
      float _433 = _432 * float_from_bits(1056964608 /* 0.5 */);
      float _434 = _332 * float_from_bits(1048576000 /* 0.25 */);
      float _435 = _433 + _434;
      float _436 = _382 * float_from_bits(1048576000 /* 0.25 */);
      float _437 = _435 + _436;
      float _438 = _437 * float_from_bits(1056964608 /* 0.5 */);
      float _439 = _295 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _440 = _302 + _292;
      int32_t _441 = _440 - _279;
      float _442 = ((float *)_inPar)[_441];
      float _443 = _442 * float_from_bits(1048576000 /* 0.25 */);
      float _444 = _439 + _443;
      int32_t _445 = _305 + _292;
      int32_t _446 = _445 - _279;
      float _447 = ((float *)_inPar)[_446];
      float _448 = _447 * float_from_bits(1048576000 /* 0.25 */);
      float _449 = _444 + _448;
      float _450 = _449 * float_from_bits(1056964608 /* 0.5 */);
      float _451 = _349 * float_from_bits(1048576000 /* 0.25 */);
      float _452 = _450 + _451;
      float _453 = _399 * float_from_bits(1048576000 /* 0.25 */);
      float _454 = _452 + _453;
      float _455 = _454 * float_from_bits(1048576000 /* 0.25 */);
      float _456 = _438 + _455;
      float _457 = _312 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _458 = _318 + _292;
      int32_t _459 = _458 - _279;
      float _460 = ((float *)_inPar)[_459];
      float _461 = _460 * float_from_bits(1048576000 /* 0.25 */);
      float _462 = _457 + _461;
      int32_t _463 = _321 + _292;
      int32_t _464 = _463 - _279;
      float _465 = ((float *)_inPar)[_464];
      float _466 = _465 * float_from_bits(1048576000 /* 0.25 */);
      float _467 = _462 + _466;
      float _468 = _467 * float_from_bits(1056964608 /* 0.5 */);
      float _469 = _368 * float_from_bits(1048576000 /* 0.25 */);
      float _470 = _468 + _469;
      float _471 = _418 * float_from_bits(1048576000 /* 0.25 */);
      float _472 = _470 + _471;
      float _473 = _472 * float_from_bits(1048576000 /* 0.25 */);
      float _474 = _456 + _473;
      float _475 = _474 * float_from_bits(1056964608 /* 0.5 */);
      float _476 = _332 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _477 = _270 + _350;
      int32_t _478 = _477 - _279;
      float _479 = ((float *)_inPar)[_478];
      float _480 = _479 * float_from_bits(1056964608 /* 0.5 */);
      float _481 = _353 * float_from_bits(1048576000 /* 0.25 */);
      float _482 = _480 + _481;
      float _483 = _371 * float_from_bits(1048576000 /* 0.25 */);
      float _484 = _482 + _483;
      float _485 = _484 * float_from_bits(1048576000 /* 0.25 */);
      float _486 = _476 + _485;
      int32_t _487 = _270 + _354;
      int32_t _488 = _487 - _279;
      float _489 = ((float *)_inPar)[_488];
      float _490 = _489 * float_from_bits(1056964608 /* 0.5 */);
      float _491 = _357 * float_from_bits(1048576000 /* 0.25 */);
      float _492 = _490 + _491;
      float _493 = _374 * float_from_bits(1048576000 /* 0.25 */);
      float _494 = _492 + _493;
      float _495 = _494 * float_from_bits(1048576000 /* 0.25 */);
      float _496 = _486 + _495;
      float _497 = _496 * float_from_bits(1056964608 /* 0.5 */);
      float _498 = _349 * float_from_bits(1056964608 /* 0.5 */);
      float _499 = _353 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _500 = _302 + _350;
      int32_t _501 = _500 - _279;
      float _502 = ((float *)_inPar)[_501];
      float _503 = _502 * float_from_bits(1048576000 /* 0.25 */);
      float _504 = _499 + _503;
      int32_t _505 = _305 + _350;
      int32_t _506 = _505 - _279;
      float _507 = ((float *)_inPar)[_506];
      float _508 = _507 * float_from_bits(1048576000 /* 0.25 */);
      float _509 = _504 + _508;
      float _510 = _509 * float_from_bits(1048576000 /* 0.25 */);
      float _511 = _498 + _510;
      float _512 = _357 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _513 = _302 + _354;
      int32_t _514 = _513 - _279;
      float _515 = ((float *)_inPar)[_514];
      float _516 = _515 * float_from_bits(1048576000 /* 0.25 */);
      float _517 = _512 + _516;
      int32_t _518 = _305 + _354;
      int32_t _519 = _518 - _279;
      float _520 = ((float *)_inPar)[_519];
      float _521 = _520 * float_from_bits(1048576000 /* 0.25 */);
      float _522 = _517 + _521;
      float _523 = _522 * float_from_bits(1048576000 /* 0.25 */);
      float _524 = _511 + _523;
      float _525 = _524 * float_from_bits(1048576000 /* 0.25 */);
      float _526 = _497 + _525;
      float _527 = _368 * float_from_bits(1056964608 /* 0.5 */);
      float _528 = _371 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _529 = _318 + _350;
      int32_t _530 = _529 - _279;
      float _531 = ((float *)_inPar)[_530];
      float _532 = _531 * float_from_bits(1048576000 /* 0.25 */);
      float _533 = _528 + _532;
      int32_t _534 = _321 + _350;
      int32_t _535 = _534 - _279;
      float _536 = ((float *)_inPar)[_535];
      float _537 = _536 * float_from_bits(1048576000 /* 0.25 */);
      float _538 = _533 + _537;
      float _539 = _538 * float_from_bits(1048576000 /* 0.25 */);
      float _540 = _527 + _539;
      float _541 = _374 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _542 = _318 + _354;
      int32_t _543 = _542 - _279;
      float _544 = ((float *)_inPar)[_543];
      float _545 = _544 * float_from_bits(1048576000 /* 0.25 */);
      float _546 = _541 + _545;
      int32_t _547 = _321 + _354;
      int32_t _548 = _547 - _279;
      float _549 = ((float *)_inPar)[_548];
      float _550 = _549 * float_from_bits(1048576000 /* 0.25 */);
      float _551 = _546 + _550;
      float _552 = _551 * float_from_bits(1048576000 /* 0.25 */);
      float _553 = _540 + _552;
      float _554 = _553 * float_from_bits(1048576000 /* 0.25 */);
      float _555 = _526 + _554;
      float _556 = _555 * float_from_bits(1048576000 /* 0.25 */);
      float _557 = _475 + _556;
      float _558 = _382 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _559 = _270 + _400;
      int32_t _560 = _559 - _279;
      float _561 = ((float *)_inPar)[_560];
      float _562 = _561 * float_from_bits(1056964608 /* 0.5 */);
      float _563 = _403 * float_from_bits(1048576000 /* 0.25 */);
      float _564 = _562 + _563;
      float _565 = _421 * float_from_bits(1048576000 /* 0.25 */);
      float _566 = _564 + _565;
      float _567 = _566 * float_from_bits(1048576000 /* 0.25 */);
      float _568 = _558 + _567;
      int32_t _569 = _270 + _404;
      int32_t _570 = _569 - _279;
      float _571 = ((float *)_inPar)[_570];
      float _572 = _571 * float_from_bits(1056964608 /* 0.5 */);
      float _573 = _407 * float_from_bits(1048576000 /* 0.25 */);
      float _574 = _572 + _573;
      float _575 = _424 * float_from_bits(1048576000 /* 0.25 */);
      float _576 = _574 + _575;
      float _577 = _576 * float_from_bits(1048576000 /* 0.25 */);
      float _578 = _568 + _577;
      float _579 = _578 * float_from_bits(1056964608 /* 0.5 */);
      float _580 = _399 * float_from_bits(1056964608 /* 0.5 */);
      float _581 = _403 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _582 = _302 + _400;
      int32_t _583 = _582 - _279;
      float _584 = ((float *)_inPar)[_583];
      float _585 = _584 * float_from_bits(1048576000 /* 0.25 */);
      float _586 = _581 + _585;
      int32_t _587 = _305 + _400;
      int32_t _588 = _587 - _279;
      float _589 = ((float *)_inPar)[_588];
      float _590 = _589 * float_from_bits(1048576000 /* 0.25 */);
      float _591 = _586 + _590;
      float _592 = _591 * float_from_bits(1048576000 /* 0.25 */);
      float _593 = _580 + _592;
      float _594 = _407 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _595 = _302 + _404;
      int32_t _596 = _595 - _279;
      float _597 = ((float *)_inPar)[_596];
      float _598 = _597 * float_from_bits(1048576000 /* 0.25 */);
      float _599 = _594 + _598;
      int32_t _600 = _305 + _404;
      int32_t _601 = _600 - _279;
      float _602 = ((float *)_inPar)[_601];
      float _603 = _602 * float_from_bits(1048576000 /* 0.25 */);
      float _604 = _599 + _603;
      float _605 = _604 * float_from_bits(1048576000 /* 0.25 */);
      float _606 = _593 + _605;
      float _607 = _606 * float_from_bits(1048576000 /* 0.25 */);
      float _608 = _579 + _607;
      float _609 = _418 * float_from_bits(1056964608 /* 0.5 */);
      float _610 = _421 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _611 = _318 + _400;
      int32_t _612 = _611 - _279;
      float _613 = ((float *)_inPar)[_612];
      float _614 = _613 * float_from_bits(1048576000 /* 0.25 */);
      float _615 = _610 + _614;
      int32_t _616 = _321 + _400;
      int32_t _617 = _616 - _279;
      float _618 = ((float *)_inPar)[_617];
      float _619 = _618 * float_from_bits(1048576000 /* 0.25 */);
      float _620 = _615 + _619;
      float _621 = _620 * float_from_bits(1048576000 /* 0.25 */);
      float _622 = _609 + _621;
      float _623 = _424 * float_from_bits(1056964608 /* 0.5 */);
      int32_t _624 = _318 + _404;
      int32_t _625 = _624 - _279;
      float _626 = ((float *)_inPar)[_625];
      float _627 = _626 * float_from_bits(1048576000 /* 0.25 */);
      float _628 = _623 + _627;
      int32_t _629 = _321 + _404;
      int32_t _630 = _629 - _279;
      float _631 = ((float *)_inPar)[_630];
      float _632 = _631 * float_from_bits(1048576000 /* 0.25 */);
      float _633 = _628 + _632;
      float _634 = _633 * float_from_bits(1048576000 /* 0.25 */);
      float _635 = _622 + _634;
      float _636 = _635 * float_from_bits(1048576000 /* 0.25 */);
      float _637 = _608 + _636;
      float _638 = _637 * float_from_bits(1048576000 /* 0.25 */);
      float _639 = _557 + _638;
      _f3[_277] = _639;
     } // for _f3_s0_x_xi_xii
    } // for _f3_s0_y_yi_yii
   } // for _f3_s0_x_xi_xi
  } // for _f3_s0_y_yi_yi
 } // for _f3_s0_x_xo_nid
 // consume f3
} // if _199
return 0;
}
