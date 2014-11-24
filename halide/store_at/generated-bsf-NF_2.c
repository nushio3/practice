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
  int32_t _0 = _f3_min_1 + _f3_extent_1;
  int32_t _1 = _0 + -1;
  int32_t _2 = min(_0, 8191);
  int32_t _3 = max(_2, 0);
  int32_t _4 = max(_1, _3);
  int32_t _5 = _0 + -2;
  int32_t _6 = min(_5, 8191);
  int32_t _7 = max(_6, 0);
  int32_t _8 = max(_4, _7);
  int32_t _9 = _f3_min_1 + 1;
  int32_t _10 = min(_9, 8191);
  int32_t _11 = max(_10, 0);
  int32_t _12 = min(_f3_min_1, _11);
  int32_t _13 = _f3_min_1 + -1;
  int32_t _14 = min(_13, 8191);
  int32_t _15 = max(_14, 0);
  int32_t _16 = min(_12, _15);
  int32_t _17 = _f3_min_0 + _f3_extent_0;
  int32_t _18 = _17 + -1;
  int32_t _19 = min(_17, 8191);
  int32_t _20 = max(_19, 0);
  int32_t _21 = max(_18, _20);
  int32_t _22 = _17 + -2;
  int32_t _23 = min(_22, 8191);
  int32_t _24 = max(_23, 0);
  int32_t _25 = max(_21, _24);
  int32_t _26 = _f3_min_0 + 1;
  int32_t _27 = min(_26, 8191);
  int32_t _28 = max(_27, 0);
  int32_t _29 = min(_f3_min_0, _28);
  int32_t _30 = _f3_min_0 + -1;
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
  int32_t _50 = _f3_extent_0 + -1;
  int32_t _51 = _50 >> 9;
  int32_t _52 = _51 * 512;
  int32_t _53 = _52 + _f3_min_0;
  int32_t _54 = _53 + 512;
  int32_t _55 = min(_54, _17);
  int32_t _56 = _17 + -512;
  int32_t _57 = min(_f3_min_0, _56);
  int32_t _58 = _55 - _57;
  int32_t _59 = _f3_extent_0 + 511;
  int32_t _60 = _59 >> 9;
  int32_t _61 = _f3_extent_1 + 511;
  int32_t _62 = _61 >> 9;
  int32_t _63 = _60 * _62;
  int32_t _64 = _63 + -1;
  int32_t _65 = sdiv(_64, _60);
  int32_t _66 = max(_65, 0);
  int32_t _67 = _66 * 512;
  int32_t _68 = _67 + _f3_min_1;
  int32_t _69 = _68 + 512;
  int32_t _70 = min(_69, _0);
  int32_t _71 = min(_65, 0);
  int32_t _72 = _71 * 512;
  int32_t _73 = _72 + _f3_min_1;
  int32_t _74 = _0 + -512;
  int32_t _75 = min(_73, _74);
  int32_t _76 = _70 - _75;
  int32_t _77 = _25 - _33;
  int32_t _78 = _77 >> 2;
  int32_t _79 = _78 * 4;
  int32_t _80 = _79 + _33;
  int32_t _81 = _80 + 3;
  int32_t _82 = min(_81, _25);
  int32_t _83 = _82 + 1;
  int32_t _84 = min(_83, 8191);
  int32_t _85 = max(_84, 0);
  int32_t _86 = max(_82, _85);
  int32_t _87 = _82 + -1;
  int32_t _88 = min(_87, 8191);
  int32_t _89 = max(_88, 0);
  int32_t _90 = max(_86, _89);
  int32_t _91 = _25 + -3;
  int32_t _92 = min(_33, _91);
  int32_t _93 = _92 + 1;
  int32_t _94 = min(_93, 8191);
  int32_t _95 = max(_94, 0);
  int32_t _96 = min(_92, _95);
  int32_t _97 = _92 + -1;
  int32_t _98 = min(_97, 8191);
  int32_t _99 = max(_98, 0);
  int32_t _100 = min(_96, _99);
  int32_t _101 = _90 - _100;
  if (_f3_host_and_dev_are_null)
    {
      bool _102 = halide_rewrite_buffer(_f3_buffer, 4, _57, _58, 1, _75, _76, _58, 0, 0, 0, 0, 0, 0);
      (void)_102;
    } // if _f3_host_and_dev_are_null
  if (_inPar_host_and_dev_are_null)
    {
      int32_t _103 = _101 + 1;
      int32_t _104 = _41 - _49;
      int32_t _105 = _104 + 1;
      bool _106 = halide_rewrite_buffer(_inPar_buffer, 4, _100, _103, 1, _49, _105, _103, 0, 0, 0, 0, 0, 0);
      (void)_106;
    } // if _inPar_host_and_dev_are_null
  bool _107 = _f3_host_and_dev_are_null || _inPar_host_and_dev_are_null;
  bool _108 = !(_107);
  if (_108)
    {
      bool _109 = _f3_elem_size == 4;
      if (!_109) {
	halide_printf(NULL, "Output buffer f3 has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _f3_elem_size);
	return -1;
      }
      bool _110 = _inPar_elem_size == 4;
      if (!_110) {
	halide_printf(NULL, "Input buffer inPar has type float32, but elem_size of the buffer_t passed in is %d instead of 4\n", _inPar_elem_size);
	return -1;
      }
      bool _111 = _f3_min_0 <= _57;
      if (!_111) {
	halide_printf(NULL, "Output buffer f3 is accessed at %d, which is before the min (%d) in dimension 0\n", _57, _f3_min_0);
	return -1;
      }
      int32_t _112 = _57 + _58;
      int32_t _113 = _112 - _f3_extent_0;
      bool _114 = _113 <= _f3_min_0;
      int32_t _115 = _112 + -1;
      int32_t _116 = _f3_min_0 + _f3_extent_0;
      int32_t _117 = _116 + -1;
      if (!_114) {
	halide_printf(NULL, "Output buffer f3 is accessed at %d, which is beyond the max (%d) in dimension 0\n", _115, _117);
	return -1;
      }
      bool _118 = _f3_min_1 <= _75;
      if (!_118) {
	halide_printf(NULL, "Output buffer f3 is accessed at %d, which is before the min (%d) in dimension 1\n", _75, _f3_min_1);
	return -1;
      }
      int32_t _119 = _75 + _76;
      int32_t _120 = _119 - _f3_extent_1;
      bool _121 = _120 <= _f3_min_1;
      int32_t _122 = _119 + -1;
      int32_t _123 = _f3_min_1 + _f3_extent_1;
      int32_t _124 = _123 + -1;
      if (!_121) {
	halide_printf(NULL, "Output buffer f3 is accessed at %d, which is beyond the max (%d) in dimension 1\n", _122, _124);
	return -1;
      }
      bool _125 = _inPar_min_0 <= _100;
      if (!_125) {
	halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _100, _inPar_min_0);
	return -1;
      }
      int32_t _126 = _100 + _101;
      int32_t _127 = _126 - _inPar_extent_0;
      int32_t _128 = _127 + 1;
      bool _129 = _128 <= _inPar_min_0;
      int32_t _130 = _inPar_min_0 + _inPar_extent_0;
      int32_t _131 = _130 + -1;
      if (!_129) {
	halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 0\n", _126, _131);
	return -1;
      }
      bool _132 = _inPar_min_1 <= _49;
      if (!_132) {
	halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _49, _inPar_min_1);
	return -1;
      }
      int32_t _133 = _41 - _inPar_extent_1;
      int32_t _134 = _133 + 1;
      bool _135 = _134 <= _inPar_min_1;
      int32_t _136 = _inPar_min_1 + _inPar_extent_1;
      int32_t _137 = _136 + -1;
      if (!_135) {
	halide_printf(NULL, "Input buffer inPar is accessed at %d, which is beyond the max (%d) in dimension 1\n", _41, _137);
	return -1;
      }
      bool _138 = _f3_stride_0 == 1;
      if (!_138) {
	halide_printf(NULL, "Static constraint violated: f3.stride.0 == 1\n");
	return -1;
      }
      bool _139 = _inPar_stride_0 == 1;
      if (!_139) {
	halide_printf(NULL, "Static constraint violated: inPar.stride.0 == 1\n");
	return -1;
      }
      int64_t _140 = (int64_t)(_f3_extent_0);
      int64_t _141 = (int64_t)(_f3_extent_1);
      int64_t _142 = (int64_t)(_inPar_extent_0);
      int64_t _143 = (int64_t)(_inPar_extent_1);
      int64_t _144 = (int64_t)(2147483647);
      bool _145 = _140 <= _144;
      if (!_145) {
	halide_printf(NULL, "Total allocation for buffer f3 exceeds 2^31 - 1\n");
	return -1;
      }
      int64_t _146 = (int64_t)(_f3_stride_1);
      int64_t _147 = _141 * _146;
      bool _148 = _147 <= _144;
      if (!_148) {
	halide_printf(NULL, "Total allocation for buffer f3 exceeds 2^31 - 1\n");
	return -1;
      }
      int64_t _149 = _141 * _140;
      bool _150 = _149 <= _144;
      if (!_150) {
	halide_printf(NULL, "Product of extents for buffer f3 exceeds 2^31 - 1\n");
	return -1;
      }
      bool _151 = _142 <= _144;
      if (!_151) {
	halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
	return -1;
      }
      int64_t _152 = (int64_t)(_inPar_stride_1);
      int64_t _153 = _143 * _152;
      bool _154 = _153 <= _144;
      if (!_154) {
	halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
	return -1;
      }
      int64_t _155 = _143 * _142;
      bool _156 = _155 <= _144;
      if (!_156) {
	halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
	return -1;
      }
      // produce f3
      int32_t _157 = _f3_extent_0 + 511;
      int32_t _158 = _157 >> 9;
      int32_t _159 = _f3_extent_1 + 511;
      int32_t _160 = _159 >> 9;
      int32_t _161 = _158 * _160;
#pragma omp parallel for
      for (int _f3_s0_x_xo_nid = 0; _f3_s0_x_xo_nid < 0 + _161; _f3_s0_x_xo_nid++)
	{
	  int32_t _162 = _f3_extent_0 + 511;
	  int32_t _163 = _162 >> 9;
	  int32_t _164 = mod(_f3_s0_x_xo_nid, _163);
	  int32_t _165 = _164 * 512;
	  int32_t _166 = _165 + _f3_min_0;
	  int32_t _167 = _f3_min_0 + _f3_extent_0;
	  int32_t _168 = _167 + -512;
	  int32_t _169 = min(_166, _168);
	  int32_t _170 = sdiv(_f3_s0_x_xo_nid, _163);
	  int32_t _171 = _170 * 512;
	  int32_t _172 = _171 + _f3_min_1;
	  int32_t _173 = _f3_min_1 + _f3_extent_1;
	  int32_t _174 = _173 + -512;
	  int32_t _175 = min(_172, _174);
	  for (int _f3_s0_y_yi_yi = 0; _f3_s0_y_yi_yi < 0 + 16; _f3_s0_y_yi_yi++)
	    {
	      int32_t _176 = _f3_s0_y_yi_yi * 32;
	      int32_t _177 = _175 + _176;
	      for (int _f3_s0_x_xi_xi = 0; _f3_s0_x_xi_xi < 0 + 16; _f3_s0_x_xi_xi++)
		{
		  int32_t _178 = _f3_s0_x_xi_xi * 32;
		  int32_t _179 = _169 + _178;
		  int32_t _180 = _177 + 1;
		  int32_t _181 = min(_180, 8191);
		  int32_t _182 = max(_181, 0);
		  int32_t _183 = min(_177, _182);
		  int32_t _184 = _177 + -1;
		  int32_t _185 = min(_184, 8191);
		  int32_t _186 = max(_185, 0);
		  int32_t _187 = min(_183, _186);
		  int32_t _188 = min(_177, 8191);
		  int32_t _189 = max(_188, 0);
		  int32_t _190 = max(_184, _189);
		  int32_t _191 = _177 + -2;
		  int32_t _192 = min(_191, 8191);
		  int32_t _193 = max(_192, 0);
		  int32_t _194 = max(_190, _193);
		  int32_t _195 = _194 + 1;
		  int32_t _196 = min(_187, _195);
		  int32_t _197 = _187 + 1;
		  int32_t _198 = min(_197, 8191);
		  int32_t _199 = max(_198, 0);
		  int32_t _200 = min(_187, _199);
		  int32_t _201 = _187 + -1;
		  int32_t _202 = min(_201, 8191);
		  int32_t _203 = max(_202, 0);
		  int32_t _204 = min(_200, _203);
		  int32_t _205 = min(_195, 8191);
		  int32_t _206 = max(_205, 0);
		  int32_t _207 = max(_194, _206);
		  int32_t _208 = _194 + -1;
		  int32_t _209 = min(_208, 8191);
		  int32_t _210 = max(_209, 0);
		  int32_t _211 = max(_207, _210);
		  int32_t _212 = _211 + 1;
		  int32_t _213 = min(_204, _212);
		  int32_t _214 = min(_213, _196);
		  int32_t _215 = _196 + 1;
		  int32_t _216 = min(_215, 8191);
		  int32_t _217 = max(_216, 0);
		  int32_t _218 = min(_214, _217);
		  int32_t _219 = _196 + -1;
		  int32_t _220 = min(_219, 8191);
		  int32_t _221 = max(_220, 0);
		  int32_t _222 = min(_218, _221);
		  int32_t _223 = _177 + 31;
		  int32_t _224 = _177 + 32;
		  int32_t _225 = min(_224, 8191);
		  int32_t _226 = max(_225, 0);
		  int32_t _227 = max(_223, _226);
		  int32_t _228 = _177 + 30;
		  int32_t _229 = min(_228, 8191);
		  int32_t _230 = max(_229, 0);
		  int32_t _231 = max(_227, _230);
		  int32_t _232 = _231 + 1;
		  int32_t _233 = min(_232, 8191);
		  int32_t _234 = max(_233, 0);
		  int32_t _235 = max(_231, _234);
		  int32_t _236 = _231 + -1;
		  int32_t _237 = min(_236, 8191);
		  int32_t _238 = max(_237, 0);
		  int32_t _239 = max(_235, _238);
		  int32_t _240 = max(_239, _231);
		  int32_t _241 = _240 - _222;
		  int32_t _242 = _179 + 1;
		  int32_t _243 = min(_242, 8191);
		  int32_t _244 = max(_243, 0);
		  int32_t _245 = min(_179, _244);
		  int32_t _246 = _179 + -1;
		  int32_t _247 = min(_246, 8191);
		  int32_t _248 = max(_247, 0);
		  int32_t _249 = min(_245, _248);
		  int32_t _250 = min(_179, 8191);
		  int32_t _251 = max(_250, 0);
		  int32_t _252 = max(_246, _251);
		  int32_t _253 = _179 + -2;
		  int32_t _254 = min(_253, 8191);
		  int32_t _255 = max(_254, 0);
		  int32_t _256 = max(_252, _255);
		  int32_t _257 = _256 + 1;
		  int32_t _258 = min(_249, _257);
		  int32_t _259 = _179 + 3;
		  int32_t _260 = _179 + 4;
		  int32_t _261 = min(_260, 8191);
		  int32_t _262 = max(_261, 0);
		  int32_t _263 = max(_259, _262);
		  int32_t _264 = _179 + 2;
		  int32_t _265 = min(_264, 8191);
		  int32_t _266 = max(_265, 0);
		  int32_t _267 = max(_263, _266);
		  int32_t _268 = _267 + -3;
		  int32_t _269 = min(_258, _268);
		  int32_t _270 = _179 + 28;
		  int32_t _271 = _179 + 29;
		  int32_t _272 = min(_271, 8191);
		  int32_t _273 = max(_272, 0);
		  int32_t _274 = min(_270, _273);
		  int32_t _275 = _179 + 27;
		  int32_t _276 = min(_275, 8191);
		  int32_t _277 = max(_276, 0);
		  int32_t _278 = min(_274, _277);
		  int32_t _279 = min(_270, 8191);
		  int32_t _280 = max(_279, 0);
		  int32_t _281 = max(_275, _280);
		  int32_t _282 = _179 + 26;
		  int32_t _283 = min(_282, 8191);
		  int32_t _284 = max(_283, 0);
		  int32_t _285 = max(_281, _284);
		  int32_t _286 = _285 + 1;
		  int32_t _287 = max(_278, _286);
		  int32_t _288 = _179 + 31;
		  int32_t _289 = _179 + 32;
		  int32_t _290 = min(_289, 8191);
		  int32_t _291 = max(_290, 0);
		  int32_t _292 = max(_288, _291);
		  int32_t _293 = _179 + 30;
		  int32_t _294 = min(_293, 8191);
		  int32_t _295 = max(_294, 0);
		  int32_t _296 = max(_292, _295);
		  int32_t _297 = _296 - _258;
		  int32_t _298 = _297 >> 2;
		  int32_t _299 = _298 * 4;
		  int32_t _300 = _287 + _299;
		  int32_t _301 = _300 + 3;
		  int32_t _302 = min(_301, _296);
		  int32_t _303 = _302 - _269;
		  {
		    int32_t _304 = _303 + 1;
		    int64_t _305 = _304;
		    int32_t _306 = _241 + 1;
		    int64_t _307 = _305 * _306;
		    if ((_307 > ((int64_t(1) << 31) - 1)) || ((_307 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
		      {
			halide_printf(NULL, "32-bit signed overflow computing size of allocation f0\n");
		      } // overflow test f0
		    int64_t _308 = _307;
		    float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_308);
		    int32_t _309 = _177 + 1;
		    int32_t _310 = min(_309, 8191);
		    int32_t _311 = max(_310, 0);
		    int32_t _312 = min(_177, _311);
		    int32_t _313 = _177 + -1;
		    int32_t _314 = min(_313, 8191);
		    int32_t _315 = max(_314, 0);
		    int32_t _316 = min(_312, _315);
		    int32_t _317 = min(_177, 8191);
		    int32_t _318 = max(_317, 0);
		    int32_t _319 = max(_313, _318);
		    int32_t _320 = _177 + -2;
		    int32_t _321 = min(_320, 8191);
		    int32_t _322 = max(_321, 0);
		    int32_t _323 = max(_319, _322);
		    int32_t _324 = _323 + 1;
		    int32_t _325 = min(_316, _324);
		    int32_t _326 = _177 + 31;
		    int32_t _327 = _177 + 32;
		    int32_t _328 = min(_327, 8191);
		    int32_t _329 = max(_328, 0);
		    int32_t _330 = max(_326, _329);
		    int32_t _331 = _177 + 30;
		    int32_t _332 = min(_331, 8191);
		    int32_t _333 = max(_332, 0);
		    int32_t _334 = max(_330, _333);
		    int32_t _335 = _334 - _325;
		    int32_t _336 = _179 + 1;
		    int32_t _337 = min(_336, 8191);
		    int32_t _338 = max(_337, 0);
		    int32_t _339 = _179 + -1;
		    int32_t _340 = min(_339, 8191);
		    int32_t _341 = max(_340, 0);
		    int32_t _342 = min(_179, _338);
		    int32_t _343 = min(_342, _341);
		    int32_t _344 = min(_179, 8191);
		    int32_t _345 = max(_344, 0);
		    int32_t _346 = max(_339, _345);
		    int32_t _347 = _179 + -2;
		    int32_t _348 = min(_347, 8191);
		    int32_t _349 = max(_348, 0);
		    int32_t _350 = max(_346, _349);
		    int32_t _351 = _350 + 1;
		    int32_t _352 = min(_343, _351);
		    int32_t _353 = _179 + 3;
		    int32_t _354 = _179 + 4;
		    int32_t _355 = min(_354, 8191);
		    int32_t _356 = max(_355, 0);
		    int32_t _357 = max(_353, _356);
		    int32_t _358 = _179 + 2;
		    int32_t _359 = min(_358, 8191);
		    int32_t _360 = max(_359, 0);
		    int32_t _361 = max(_357, _360);
		    int32_t _362 = _361 + -3;
		    int32_t _363 = min(_352, _362);
		    int32_t _364 = min(_363, _179);
		    int32_t _365 = min(_364, _338);
		    int32_t _366 = min(_365, _341);
		    int32_t _367 = _179 + 31;
		    int32_t _368 = _179 + 32;
		    int32_t _369 = min(_368, 8191);
		    int32_t _370 = max(_369, 0);
		    int32_t _371 = max(_367, _370);
		    int32_t _372 = _179 + 30;
		    int32_t _373 = min(_372, 8191);
		    int32_t _374 = max(_373, 0);
		    int32_t _375 = max(_371, _374);
		    int32_t _376 = _179 + 28;
		    int32_t _377 = _179 + 29;
		    int32_t _378 = min(_377, 8191);
		    int32_t _379 = max(_378, 0);
		    int32_t _380 = min(_376, _379);
		    int32_t _381 = _179 + 27;
		    int32_t _382 = min(_381, 8191);
		    int32_t _383 = max(_382, 0);
		    int32_t _384 = min(_380, _383);
		    int32_t _385 = min(_376, 8191);
		    int32_t _386 = max(_385, 0);
		    int32_t _387 = max(_381, _386);
		    int32_t _388 = _179 + 26;
		    int32_t _389 = min(_388, 8191);
		    int32_t _390 = max(_389, 0);
		    int32_t _391 = max(_387, _390);
		    int32_t _392 = _391 + 1;
		    int32_t _393 = max(_384, _392);
		    int32_t _394 = _375 - _352;
		    int32_t _395 = _394 >> 2;
		    int32_t _396 = _395 * 4;
		    int32_t _397 = _393 + _396;
		    int32_t _398 = _397 + 3;
		    int32_t _399 = min(_398, _375);
		    int32_t _400 = max(_399, _367);
		    int32_t _401 = max(_400, _370);
		    int32_t _402 = max(_401, _374);
		    int32_t _403 = _402 - _366;
		    {
		      int32_t _404 = _403 + 1;
		      int64_t _405 = _404;
		      int32_t _406 = _335 + 1;
		      int64_t _407 = _405 * _406;
		      if ((_407 > ((int64_t(1) << 31) - 1)) || ((_407 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
			{
			  halide_printf(NULL, "32-bit signed overflow computing size of allocation f2\n");
			} // overflow test f2
		      int64_t _408 = _407;
		      float *_f2 = (float *)halide_malloc(NULL, sizeof(float)*_408);
		      int32_t _409 = _177 + 1;
		      int32_t _410 = min(_409, 8191);
		      int32_t _411 = max(_410, 0);
		      int32_t _412 = _177 + -1;
		      int32_t _413 = min(_412, 8191);
		      int32_t _414 = max(_413, 0);
		      int32_t _415 = min(_177, _411);
		      int32_t _416 = min(_415, _414);
		      int32_t _417 = min(_177, 8191);
		      int32_t _418 = max(_417, 0);
		      int32_t _419 = max(_412, _418);
		      int32_t _420 = _177 + -2;
		      int32_t _421 = min(_420, 8191);
		      int32_t _422 = max(_421, 0);
		      int32_t _423 = max(_419, _422);
		      int32_t _424 = _423 + 1;
		      int32_t _425 = min(_416, _424);
		      int32_t _426 = min(_425, _177);
		      int32_t _427 = _177 + 32;
		      int32_t _428 = min(_427, 8191);
		      int32_t _429 = max(_428, 0);
		      int32_t _430 = _177 + 30;
		      int32_t _431 = min(_430, 8191);
		      int32_t _432 = max(_431, 0);
		      int32_t _433 = _177 + 31;
		      int32_t _434 = max(_433, _429);
		      int32_t _435 = max(_434, _432);
		      int32_t _436 = max(_435, _433);
		      int32_t _437 = _436 - _426;
		      {
			int64_t _438 = 32;
			int32_t _439 = _437 + 1;
			int64_t _440 = _438 * _439;
			if ((_440 > ((int64_t(1) << 31) - 1)) || ((_440 * sizeof(float)) > ((int64_t(1) << 31) - 1)))
			  {
			    halide_printf(NULL, "32-bit signed overflow computing size of allocation f1\n");
			  } // overflow test f1
			int64_t _441 = _440;
			float *_f1 = (float *)halide_malloc(NULL, sizeof(float)*_441);
			for (int _f3_s0_y_yi_yii = 0; _f3_s0_y_yi_yii < 0 + 32; _f3_s0_y_yi_yii++)
			  {
			    int32_t _442 = _f3_s0_y_yi_yi * 32;
			    int32_t _443 = _442 + _f3_s0_y_yi_yii;
			    int32_t _444 = _175 + _443;
			    for (int _f3_s0_x_xi_xii_xii = 0; _f3_s0_x_xi_xii_xii < 0 + 8; _f3_s0_x_xi_xii_xii++)
			      {
				int32_t _445 = _f3_s0_x_xi_xi * 32;
				int32_t _446 = _f3_s0_x_xi_xii_xii * 4;
				int32_t _447 = _445 + _446;
				int32_t _448 = _169 + _447;
				int32_t _449 = _444 + 1;
				int32_t _450 = min(_449, 8191);
				int32_t _451 = max(_450, 0);
				int32_t _452 = max(_444, _451);
				int32_t _453 = _444 + -1;
				int32_t _454 = min(_453, 8191);
				int32_t _455 = max(_454, 0);
				int32_t _456 = max(_452, _455);
				int32_t _457 = min(_444, _451);
				int32_t _458 = min(_457, _455);
				int32_t _459 = min(_444, 8191);
				int32_t _460 = max(_459, 0);
				int32_t _461 = max(_453, _460);
				int32_t _462 = _444 + -2;
				int32_t _463 = min(_462, 8191);
				int32_t _464 = max(_463, 0);
				int32_t _465 = max(_461, _464);
				int32_t _466 = _465 + 1;
				bool _467 = _f3_s0_y_yi_yii == 0;
				int32_t _468 = (int32_t)(_467 ? _458 : _466);
				int32_t _469 = _448 + 3;
				int32_t _470 = _448 + 4;
				int32_t _471 = min(_470, 8191);
				int32_t _472 = max(_471, 0);
				int32_t _473 = max(_469, _472);
				int32_t _474 = _448 + 2;
				int32_t _475 = min(_474, 8191);
				int32_t _476 = max(_475, 0);
				int32_t _477 = max(_473, _476);
				int32_t _478 = _448 + 1;
				int32_t _479 = min(_478, 8191);
				int32_t _480 = max(_479, 0);
				int32_t _481 = min(_448, _480);
				int32_t _482 = _448 + -1;
				int32_t _483 = min(_482, 8191);
				int32_t _484 = max(_483, 0);
				int32_t _485 = min(_481, _484);
				int32_t _486 = min(_448, 8191);
				int32_t _487 = max(_486, 0);
				int32_t _488 = max(_482, _487);
				int32_t _489 = _448 + -2;
				int32_t _490 = min(_489, 8191);
				int32_t _491 = max(_490, 0);
				int32_t _492 = max(_488, _491);
				int32_t _493 = _492 + 1;
				bool _494 = _f3_s0_x_xi_xii_xii == 0;
				int32_t _495 = (int32_t)(_494 ? _485 : _493);
				int32_t _496 = _456 + 1;
				int32_t _497 = min(_496, 8191);
				int32_t _498 = max(_497, 0);
				int32_t _499 = max(_456, _498);
				int32_t _500 = _456 + -1;
				int32_t _501 = min(_500, 8191);
				int32_t _502 = max(_501, 0);
				int32_t _503 = max(_499, _502);
				int32_t _504 = _458 + 1;
				int32_t _505 = min(_504, 8191);
				int32_t _506 = max(_505, 0);
				int32_t _507 = min(_458, _506);
				int32_t _508 = _458 + -1;
				int32_t _509 = min(_508, 8191);
				int32_t _510 = max(_509, 0);
				int32_t _511 = min(_507, _510);
				int32_t _512 = min(_466, 8191);
				int32_t _513 = max(_512, 0);
				int32_t _514 = max(_465, _513);
				int32_t _515 = _465 + -1;
				int32_t _516 = min(_515, 8191);
				int32_t _517 = max(_516, 0);
				int32_t _518 = max(_514, _517);
				int32_t _519 = _518 + 1;
				int32_t _520 = (int32_t)(_467 ? _511 : _519);
				// produce f0
				int32_t _521 = _503 - _520;
				int32_t _522 = _521 + 1;
				for (int _f0_s0_y = _520; _f0_s0_y < _520 + _522; _f0_s0_y++)
				  {
				    int32_t _523 = _477 - _495;
				    int32_t _524 = _523 + 4;
				    int32_t _525 = _524 >> 2;
				    for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + _525; _f0_s0_x_x++)
				      {
					int32_t _526 = _f0_s0_x_x * 4;
					int32_t _527 = _526 + _495;
					int32_t _528 = _477 + -3;
					int32_t _529 = min(_527, _528);
					int32_t _530 = _529 - _269;
					int32_t _531 = _f0_s0_y - _222;
					int32_t _532 = _303 + 1;
					int32_t _533 = _531 * _532;
					int32_t _534 = _530 + _533;
					int32_t _535 = _f0_s0_y * _inPar_stride_1;
					int32_t _536 = _529 + _535;
					int32_t _537 = _inPar_min_1 * _inPar_stride_1;
					int32_t _538 = _inPar_min_0 + _537;
					int32_t _539 = _536 - _538;
					float _540 = ((float *)_inPar)[_539];
					float _541 = _540 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _542 = _529 + 1;
					int32_t _543 = min(_542, 8191);
					int32_t _544 = max(_543, 0);
					int32_t _545 = _544 + _535;
					int32_t _546 = _545 - _538;
					float _547 = ((float *)_inPar)[_546];
					float _548 = _547 * float_from_bits(1048576000 /* 0.25 */);
					float _549 = _541 + _548;
					int32_t _550 = _529 + -1;
					int32_t _551 = min(_550, 8191);
					int32_t _552 = max(_551, 0);
					int32_t _553 = _552 + _535;
					int32_t _554 = _553 - _538;
					float _555 = ((float *)_inPar)[_554];
					float _556 = _555 * float_from_bits(1048576000 /* 0.25 */);
					float _557 = _549 + _556;
					_f0[_534] = _557;
					int32_t _558 = _529 - _269;
					int32_t _559 = _f0_s0_y - _222;
					int32_t _560 = _303 + 1;
					int32_t _561 = _559 * _560;
					int32_t _562 = _558 + _561;
					int32_t _563 = _562 + 1;
					int32_t _564 = _f0_s0_y * _inPar_stride_1;
					int32_t _565 = _529 + _564;
					int32_t _566 = _inPar_min_1 * _inPar_stride_1;
					int32_t _567 = _inPar_min_0 + _566;
					int32_t _568 = _565 - _567;
					int32_t _569 = _568 + 1;
					float _570 = ((float *)_inPar)[_569];
					float _571 = _570 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _572 = _529 + 2;
					int32_t _573 = min(_572, 8191);
					int32_t _574 = max(_573, 0);
					int32_t _575 = _574 + _564;
					int32_t _576 = _575 - _567;
					float _577 = ((float *)_inPar)[_576];
					float _578 = _577 * float_from_bits(1048576000 /* 0.25 */);
					float _579 = _571 + _578;
					int32_t _580 = min(_529, 8191);
					int32_t _581 = max(_580, 0);
					int32_t _582 = _581 + _564;
					int32_t _583 = _582 - _567;
					float _584 = ((float *)_inPar)[_583];
					float _585 = _584 * float_from_bits(1048576000 /* 0.25 */);
					float _586 = _579 + _585;
					_f0[_563] = _586;
					int32_t _587 = _529 - _269;
					int32_t _588 = _f0_s0_y - _222;
					int32_t _589 = _303 + 1;
					int32_t _590 = _588 * _589;
					int32_t _591 = _587 + _590;
					int32_t _592 = _591 + 2;
					int32_t _593 = _f0_s0_y * _inPar_stride_1;
					int32_t _594 = _529 + _593;
					int32_t _595 = _inPar_min_1 * _inPar_stride_1;
					int32_t _596 = _inPar_min_0 + _595;
					int32_t _597 = _594 - _596;
					int32_t _598 = _597 + 2;
					float _599 = ((float *)_inPar)[_598];
					float _600 = _599 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _601 = _529 + 3;
					int32_t _602 = min(_601, 8191);
					int32_t _603 = max(_602, 0);
					int32_t _604 = _603 + _593;
					int32_t _605 = _604 - _596;
					float _606 = ((float *)_inPar)[_605];
					float _607 = _606 * float_from_bits(1048576000 /* 0.25 */);
					float _608 = _600 + _607;
					int32_t _609 = _529 + 1;
					int32_t _610 = min(_609, 8191);
					int32_t _611 = max(_610, 0);
					int32_t _612 = _611 + _593;
					int32_t _613 = _612 - _596;
					float _614 = ((float *)_inPar)[_613];
					float _615 = _614 * float_from_bits(1048576000 /* 0.25 */);
					float _616 = _608 + _615;
					_f0[_592] = _616;
					int32_t _617 = _529 - _269;
					int32_t _618 = _f0_s0_y - _222;
					int32_t _619 = _303 + 1;
					int32_t _620 = _618 * _619;
					int32_t _621 = _617 + _620;
					int32_t _622 = _621 + 3;
					int32_t _623 = _f0_s0_y * _inPar_stride_1;
					int32_t _624 = _529 + _623;
					int32_t _625 = _inPar_min_1 * _inPar_stride_1;
					int32_t _626 = _inPar_min_0 + _625;
					int32_t _627 = _624 - _626;
					int32_t _628 = _627 + 3;
					float _629 = ((float *)_inPar)[_628];
					float _630 = _629 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _631 = _529 + 4;
					int32_t _632 = min(_631, 8191);
					int32_t _633 = max(_632, 0);
					int32_t _634 = _633 + _623;
					int32_t _635 = _634 - _626;
					float _636 = ((float *)_inPar)[_635];
					float _637 = _636 * float_from_bits(1048576000 /* 0.25 */);
					float _638 = _630 + _637;
					int32_t _639 = _529 + 2;
					int32_t _640 = min(_639, 8191);
					int32_t _641 = max(_640, 0);
					int32_t _642 = _641 + _623;
					int32_t _643 = _642 - _626;
					float _644 = ((float *)_inPar)[_643];
					float _645 = _644 * float_from_bits(1048576000 /* 0.25 */);
					float _646 = _638 + _645;
					_f0[_622] = _646;
				      } // for _f0_s0_x_x
				  } // for _f0_s0_y
				// consume f0
				// produce f2
				int32_t _647 = _456 - _468;
				int32_t _648 = _647 + 1;
				for (int _f2_s0_y = _468; _f2_s0_y < _468 + _648; _f2_s0_y++)
				  {
				    int32_t _649 = _477 - _495;
				    int32_t _650 = _649 + 4;
				    int32_t _651 = _650 >> 2;
				    for (int _f2_s0_x_x = 0; _f2_s0_x_x < 0 + _651; _f2_s0_x_x++)
				      {
					int32_t _652 = _f2_s0_x_x * 4;
					int32_t _653 = _652 + _495;
					int32_t _654 = _477 + -3;
					int32_t _655 = min(_653, _654);
					int32_t _656 = _655 - _366;
					int32_t _657 = _f2_s0_y - _325;
					int32_t _658 = _403 + 1;
					int32_t _659 = _657 * _658;
					int32_t _660 = _656 + _659;
					int32_t _661 = _655 - _269;
					int32_t _662 = _f2_s0_y - _222;
					int32_t _663 = _303 + 1;
					int32_t _664 = _662 * _663;
					int32_t _665 = _661 + _664;
					float _666 = _f0[_665];
					float _667 = _666 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _668 = _f2_s0_y + 1;
					int32_t _669 = min(_668, 8191);
					int32_t _670 = max(_669, 0);
					int32_t _671 = _670 - _222;
					int32_t _672 = _671 * _663;
					int32_t _673 = _661 + _672;
					float _674 = _f0[_673];
					float _675 = _674 * float_from_bits(1048576000 /* 0.25 */);
					float _676 = _667 + _675;
					int32_t _677 = _f2_s0_y + -1;
					int32_t _678 = min(_677, 8191);
					int32_t _679 = max(_678, 0);
					int32_t _680 = _679 - _222;
					int32_t _681 = _680 * _663;
					int32_t _682 = _661 + _681;
					float _683 = _f0[_682];
					float _684 = _683 * float_from_bits(1048576000 /* 0.25 */);
					float _685 = _676 + _684;
					_f2[_660] = _685;
					int32_t _686 = _655 - _366;
					int32_t _687 = _f2_s0_y - _325;
					int32_t _688 = _403 + 1;
					int32_t _689 = _687 * _688;
					int32_t _690 = _686 + _689;
					int32_t _691 = _690 + 1;
					int32_t _692 = _655 - _269;
					int32_t _693 = _f2_s0_y - _222;
					int32_t _694 = _303 + 1;
					int32_t _695 = _693 * _694;
					int32_t _696 = _692 + _695;
					int32_t _697 = _696 + 1;
					float _698 = _f0[_697];
					float _699 = _698 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _700 = _f2_s0_y + 1;
					int32_t _701 = min(_700, 8191);
					int32_t _702 = max(_701, 0);
					int32_t _703 = _702 - _222;
					int32_t _704 = _703 * _694;
					int32_t _705 = _692 + _704;
					int32_t _706 = _705 + 1;
					float _707 = _f0[_706];
					float _708 = _707 * float_from_bits(1048576000 /* 0.25 */);
					float _709 = _699 + _708;
					int32_t _710 = _f2_s0_y + -1;
					int32_t _711 = min(_710, 8191);
					int32_t _712 = max(_711, 0);
					int32_t _713 = _712 - _222;
					int32_t _714 = _713 * _694;
					int32_t _715 = _692 + _714;
					int32_t _716 = _715 + 1;
					float _717 = _f0[_716];
					float _718 = _717 * float_from_bits(1048576000 /* 0.25 */);
					float _719 = _709 + _718;
					_f2[_691] = _719;
					int32_t _720 = _655 - _366;
					int32_t _721 = _f2_s0_y - _325;
					int32_t _722 = _403 + 1;
					int32_t _723 = _721 * _722;
					int32_t _724 = _720 + _723;
					int32_t _725 = _724 + 2;
					int32_t _726 = _655 - _269;
					int32_t _727 = _f2_s0_y - _222;
					int32_t _728 = _303 + 1;
					int32_t _729 = _727 * _728;
					int32_t _730 = _726 + _729;
					int32_t _731 = _730 + 2;
					float _732 = _f0[_731];
					float _733 = _732 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _734 = _f2_s0_y + 1;
					int32_t _735 = min(_734, 8191);
					int32_t _736 = max(_735, 0);
					int32_t _737 = _736 - _222;
					int32_t _738 = _737 * _728;
					int32_t _739 = _726 + _738;
					int32_t _740 = _739 + 2;
					float _741 = _f0[_740];
					float _742 = _741 * float_from_bits(1048576000 /* 0.25 */);
					float _743 = _733 + _742;
					int32_t _744 = _f2_s0_y + -1;
					int32_t _745 = min(_744, 8191);
					int32_t _746 = max(_745, 0);
					int32_t _747 = _746 - _222;
					int32_t _748 = _747 * _728;
					int32_t _749 = _726 + _748;
					int32_t _750 = _749 + 2;
					float _751 = _f0[_750];
					float _752 = _751 * float_from_bits(1048576000 /* 0.25 */);
					float _753 = _743 + _752;
					_f2[_725] = _753;
					int32_t _754 = _655 - _366;
					int32_t _755 = _f2_s0_y - _325;
					int32_t _756 = _403 + 1;
					int32_t _757 = _755 * _756;
					int32_t _758 = _754 + _757;
					int32_t _759 = _758 + 3;
					int32_t _760 = _655 - _269;
					int32_t _761 = _f2_s0_y - _222;
					int32_t _762 = _303 + 1;
					int32_t _763 = _761 * _762;
					int32_t _764 = _760 + _763;
					int32_t _765 = _764 + 3;
					float _766 = _f0[_765];
					float _767 = _766 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _768 = _f2_s0_y + 1;
					int32_t _769 = min(_768, 8191);
					int32_t _770 = max(_769, 0);
					int32_t _771 = _770 - _222;
					int32_t _772 = _771 * _762;
					int32_t _773 = _760 + _772;
					int32_t _774 = _773 + 3;
					float _775 = _f0[_774];
					float _776 = _775 * float_from_bits(1048576000 /* 0.25 */);
					float _777 = _767 + _776;
					int32_t _778 = _f2_s0_y + -1;
					int32_t _779 = min(_778, 8191);
					int32_t _780 = max(_779, 0);
					int32_t _781 = _780 - _222;
					int32_t _782 = _781 * _762;
					int32_t _783 = _760 + _782;
					int32_t _784 = _783 + 3;
					float _785 = _f0[_784];
					float _786 = _785 * float_from_bits(1048576000 /* 0.25 */);
					float _787 = _777 + _786;
					_f2[_759] = _787;
				      } // for _f2_s0_x_x
				  } // for _f2_s0_y
				// consume f2
				// produce f1
				int32_t _788 = _456 - _468;
				int32_t _789 = _788 + 1;
				for (int _f1_s0_y = _468; _f1_s0_y < _468 + _789; _f1_s0_y++)
				  {
				    for (int _f1_s0_x_x = 0; _f1_s0_x_x < 0 + 1; _f1_s0_x_x++)
				      {
					int32_t _790 = _f1_s0_x_x * 4;
					int32_t _791 = _790 + _448;
					int32_t _792 = min(_791, _448);
					int32_t _793 = _792 - _179;
					int32_t _794 = _f1_s0_y - _426;
					int32_t _795 = _794 * 32;
					int32_t _796 = _793 + _795;
					int32_t _797 = _792 - _366;
					int32_t _798 = _f1_s0_y - _325;
					int32_t _799 = _403 + 1;
					int32_t _800 = _798 * _799;
					int32_t _801 = _797 + _800;
					float _802 = _f2[_801];
					float _803 = _802 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _804 = _792 + 1;
					int32_t _805 = min(_804, 8191);
					int32_t _806 = max(_805, 0);
					int32_t _807 = _806 - _366;
					int32_t _808 = _807 + _800;
					float _809 = _f2[_808];
					float _810 = _809 * float_from_bits(1048576000 /* 0.25 */);
					float _811 = _803 + _810;
					int32_t _812 = _792 + -1;
					int32_t _813 = min(_812, 8191);
					int32_t _814 = max(_813, 0);
					int32_t _815 = _814 - _366;
					int32_t _816 = _815 + _800;
					float _817 = _f2[_816];
					float _818 = _817 * float_from_bits(1048576000 /* 0.25 */);
					float _819 = _811 + _818;
					_f1[_796] = _819;
					int32_t _820 = _792 - _179;
					int32_t _821 = _f1_s0_y - _426;
					int32_t _822 = _821 * 32;
					int32_t _823 = _820 + _822;
					int32_t _824 = _823 + 1;
					int32_t _825 = _792 - _366;
					int32_t _826 = _f1_s0_y - _325;
					int32_t _827 = _403 + 1;
					int32_t _828 = _826 * _827;
					int32_t _829 = _825 + _828;
					int32_t _830 = _829 + 1;
					float _831 = _f2[_830];
					float _832 = _831 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _833 = _792 + 2;
					int32_t _834 = min(_833, 8191);
					int32_t _835 = max(_834, 0);
					int32_t _836 = _835 - _366;
					int32_t _837 = _836 + _828;
					float _838 = _f2[_837];
					float _839 = _838 * float_from_bits(1048576000 /* 0.25 */);
					float _840 = _832 + _839;
					int32_t _841 = min(_792, 8191);
					int32_t _842 = max(_841, 0);
					int32_t _843 = _842 - _366;
					int32_t _844 = _843 + _828;
					float _845 = _f2[_844];
					float _846 = _845 * float_from_bits(1048576000 /* 0.25 */);
					float _847 = _840 + _846;
					_f1[_824] = _847;
					int32_t _848 = _792 - _179;
					int32_t _849 = _f1_s0_y - _426;
					int32_t _850 = _849 * 32;
					int32_t _851 = _848 + _850;
					int32_t _852 = _851 + 2;
					int32_t _853 = _792 - _366;
					int32_t _854 = _f1_s0_y - _325;
					int32_t _855 = _403 + 1;
					int32_t _856 = _854 * _855;
					int32_t _857 = _853 + _856;
					int32_t _858 = _857 + 2;
					float _859 = _f2[_858];
					float _860 = _859 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _861 = _792 + 3;
					int32_t _862 = min(_861, 8191);
					int32_t _863 = max(_862, 0);
					int32_t _864 = _863 - _366;
					int32_t _865 = _864 + _856;
					float _866 = _f2[_865];
					float _867 = _866 * float_from_bits(1048576000 /* 0.25 */);
					float _868 = _860 + _867;
					int32_t _869 = _792 + 1;
					int32_t _870 = min(_869, 8191);
					int32_t _871 = max(_870, 0);
					int32_t _872 = _871 - _366;
					int32_t _873 = _872 + _856;
					float _874 = _f2[_873];
					float _875 = _874 * float_from_bits(1048576000 /* 0.25 */);
					float _876 = _868 + _875;
					_f1[_852] = _876;
					int32_t _877 = _792 - _179;
					int32_t _878 = _f1_s0_y - _426;
					int32_t _879 = _878 * 32;
					int32_t _880 = _877 + _879;
					int32_t _881 = _880 + 3;
					int32_t _882 = _792 - _366;
					int32_t _883 = _f1_s0_y - _325;
					int32_t _884 = _403 + 1;
					int32_t _885 = _883 * _884;
					int32_t _886 = _882 + _885;
					int32_t _887 = _886 + 3;
					float _888 = _f2[_887];
					float _889 = _888 * float_from_bits(1056964608 /* 0.5 */);
					int32_t _890 = _792 + 4;
					int32_t _891 = min(_890, 8191);
					int32_t _892 = max(_891, 0);
					int32_t _893 = _892 - _366;
					int32_t _894 = _893 + _885;
					float _895 = _f2[_894];
					float _896 = _895 * float_from_bits(1048576000 /* 0.25 */);
					float _897 = _889 + _896;
					int32_t _898 = _792 + 2;
					int32_t _899 = min(_898, 8191);
					int32_t _900 = max(_899, 0);
					int32_t _901 = _900 - _366;
					int32_t _902 = _901 + _885;
					float _903 = _f2[_902];
					float _904 = _903 * float_from_bits(1048576000 /* 0.25 */);
					float _905 = _897 + _904;
					_f1[_881] = _905;
				      } // for _f1_s0_x_x
				  } // for _f1_s0_y
				// consume f1
				int32_t _906 = _f3_s0_x_xi_xi * 32;
				int32_t _907 = _f3_s0_x_xi_xii_xii * 4;
				int32_t _908 = _906 + _907;
				int32_t _909 = _169 + _908;
				int32_t _910 = _909 - _f3_min_0;
				int32_t _911 = _444 - _f3_min_1;
				int32_t _912 = _911 * _f3_stride_1;
				int32_t _913 = _910 + _912;
				int32_t _914 = _909 - _179;
				int32_t _915 = _444 - _426;
				int32_t _916 = _915 * 32;
				int32_t _917 = _914 + _916;
				float _918 = _f1[_917];
				float _919 = _918 * float_from_bits(1056964608 /* 0.5 */);
				int32_t _920 = _444 + 1;
				int32_t _921 = min(_920, 8191);
				int32_t _922 = max(_921, 0);
				int32_t _923 = _922 - _426;
				int32_t _924 = _923 * 32;
				int32_t _925 = _914 + _924;
				float _926 = _f1[_925];
				float _927 = _926 * float_from_bits(1048576000 /* 0.25 */);
				float _928 = _919 + _927;
				int32_t _929 = _444 + -1;
				int32_t _930 = min(_929, 8191);
				int32_t _931 = max(_930, 0);
				int32_t _932 = _931 - _426;
				int32_t _933 = _932 * 32;
				int32_t _934 = _914 + _933;
				float _935 = _f1[_934];
				float _936 = _935 * float_from_bits(1048576000 /* 0.25 */);
				float _937 = _928 + _936;
				_f3[_913] = _937;
				int32_t _938 = _f3_s0_x_xi_xi * 32;
				int32_t _939 = _f3_s0_x_xi_xii_xii * 4;
				int32_t _940 = _938 + _939;
				int32_t _941 = _169 + _940;
				int32_t _942 = _941 - _f3_min_0;
				int32_t _943 = _444 - _f3_min_1;
				int32_t _944 = _943 * _f3_stride_1;
				int32_t _945 = _942 + _944;
				int32_t _946 = _945 + 1;
				int32_t _947 = _941 - _179;
				int32_t _948 = _444 - _426;
				int32_t _949 = _948 * 32;
				int32_t _950 = _947 + _949;
				int32_t _951 = _950 + 1;
				float _952 = _f1[_951];
				float _953 = _952 * float_from_bits(1056964608 /* 0.5 */);
				int32_t _954 = _444 + 1;
				int32_t _955 = min(_954, 8191);
				int32_t _956 = max(_955, 0);
				int32_t _957 = _956 - _426;
				int32_t _958 = _957 * 32;
				int32_t _959 = _947 + _958;
				int32_t _960 = _959 + 1;
				float _961 = _f1[_960];
				float _962 = _961 * float_from_bits(1048576000 /* 0.25 */);
				float _963 = _953 + _962;
				int32_t _964 = _444 + -1;
				int32_t _965 = min(_964, 8191);
				int32_t _966 = max(_965, 0);
				int32_t _967 = _966 - _426;
				int32_t _968 = _967 * 32;
				int32_t _969 = _947 + _968;
				int32_t _970 = _969 + 1;
				float _971 = _f1[_970];
				float _972 = _971 * float_from_bits(1048576000 /* 0.25 */);
				float _973 = _963 + _972;
				_f3[_946] = _973;
				int32_t _974 = _f3_s0_x_xi_xi * 32;
				int32_t _975 = _f3_s0_x_xi_xii_xii * 4;
				int32_t _976 = _974 + _975;
				int32_t _977 = _169 + _976;
				int32_t _978 = _977 - _f3_min_0;
				int32_t _979 = _444 - _f3_min_1;
				int32_t _980 = _979 * _f3_stride_1;
				int32_t _981 = _978 + _980;
				int32_t _982 = _981 + 2;
				int32_t _983 = _977 - _179;
				int32_t _984 = _444 - _426;
				int32_t _985 = _984 * 32;
				int32_t _986 = _983 + _985;
				int32_t _987 = _986 + 2;
				float _988 = _f1[_987];
				float _989 = _988 * float_from_bits(1056964608 /* 0.5 */);
				int32_t _990 = _444 + 1;
				int32_t _991 = min(_990, 8191);
				int32_t _992 = max(_991, 0);
				int32_t _993 = _992 - _426;
				int32_t _994 = _993 * 32;
				int32_t _995 = _983 + _994;
				int32_t _996 = _995 + 2;
				float _997 = _f1[_996];
				float _998 = _997 * float_from_bits(1048576000 /* 0.25 */);
				float _999 = _989 + _998;
				int32_t _1000 = _444 + -1;
				int32_t _1001 = min(_1000, 8191);
				int32_t _1002 = max(_1001, 0);
				int32_t _1003 = _1002 - _426;
				int32_t _1004 = _1003 * 32;
				int32_t _1005 = _983 + _1004;
				int32_t _1006 = _1005 + 2;
				float _1007 = _f1[_1006];
				float _1008 = _1007 * float_from_bits(1048576000 /* 0.25 */);
				float _1009 = _999 + _1008;
				_f3[_982] = _1009;
				int32_t _1010 = _f3_s0_x_xi_xi * 32;
				int32_t _1011 = _f3_s0_x_xi_xii_xii * 4;
				int32_t _1012 = _1010 + _1011;
				int32_t _1013 = _169 + _1012;
				int32_t _1014 = _1013 - _f3_min_0;
				int32_t _1015 = _444 - _f3_min_1;
				int32_t _1016 = _1015 * _f3_stride_1;
				int32_t _1017 = _1014 + _1016;
				int32_t _1018 = _1017 + 3;
				int32_t _1019 = _1013 - _179;
				int32_t _1020 = _444 - _426;
				int32_t _1021 = _1020 * 32;
				int32_t _1022 = _1019 + _1021;
				int32_t _1023 = _1022 + 3;
				float _1024 = _f1[_1023];
				float _1025 = _1024 * float_from_bits(1056964608 /* 0.5 */);
				int32_t _1026 = _444 + 1;
				int32_t _1027 = min(_1026, 8191);
				int32_t _1028 = max(_1027, 0);
				int32_t _1029 = _1028 - _426;
				int32_t _1030 = _1029 * 32;
				int32_t _1031 = _1019 + _1030;
				int32_t _1032 = _1031 + 3;
				float _1033 = _f1[_1032];
				float _1034 = _1033 * float_from_bits(1048576000 /* 0.25 */);
				float _1035 = _1025 + _1034;
				int32_t _1036 = _444 + -1;
				int32_t _1037 = min(_1036, 8191);
				int32_t _1038 = max(_1037, 0);
				int32_t _1039 = _1038 - _426;
				int32_t _1040 = _1039 * 32;
				int32_t _1041 = _1019 + _1040;
				int32_t _1042 = _1041 + 3;
				float _1043 = _f1[_1042];
				float _1044 = _1043 * float_from_bits(1048576000 /* 0.25 */);
				float _1045 = _1035 + _1044;
				_f3[_1018] = _1045;
			      } // for _f3_s0_x_xi_xii_xii
			  } // for _f3_s0_y_yi_yii
			halide_free(NULL, _f0);
			halide_free(NULL, _f2);
			halide_free(NULL, _f1);
		      } // alloc _f1
		    } // alloc _f2
		  } // alloc _f0
		} // for _f3_s0_x_xi_xi
	    } // for _f3_s0_y_yi_yi
	} // for _f3_s0_x_xo_nid
      // consume f3
    } // if _108
  return 0;
}
