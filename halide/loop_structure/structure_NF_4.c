extern "C" void *halide_malloc(void *ctx, size_t);
inline float asinh_f32(float x) {return asinhf(x);}
inline float acosh_f32(float x) {return acoshf(x);}
inline float atanh_f32(float x) {return atanhf(x);}
inline double asinh_f64(double x) {return asinh(x);}
inline double acosh_f64(double x) {return acosh(x);}
inline double atanh_f64(double x) {return atanh(x);}
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
 } u;
}
template<typename T> T max(T a, T b) {if (a > b) return a; return b;}
template<typename T> T min(T a, T b) {if (a < b) return a; return b;}
template<typename T> T mod(T a, T b) {T result = a % b; if (result < 0) result += b; return result;}
template<typename T> T sdiv(T a, T b) {return (a - mod(a, b))/b;}
template<typename A, typename B> A reinterpret(B b) {A a; memcpy(&a, &b, sizeof(a)); return a;}
typedef struct buffer_t {
} buffer_t;
                           int32_t min3, int32_t extent3, int32_t stride3) {
}
extern "C" int main_compute(buffer_t *_inPar_buffer, buffer_t *_f7_buffer) {
{
} // if _f7_host_and_dev_are_null
{
} // if _inPar_host_and_dev_are_null
{
 if (!_173) {
 }
 if (!_174) {
 }
 if (!_175) {
  halide_printf(NULL, "Output buffer f7 is accessed at %d, which is before the min (%d) in dimension 0\n", _121, _f7_min_0);
 }
 if (!_178) {
 }
 if (!_182) {
  halide_printf(NULL, "Output buffer f7 is accessed at %d, which is before the min (%d) in dimension 1\n", _139, _f7_min_1);
 }
 if (!_185) {
 }
 if (!_189) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 0\n", _164, _inPar_min_0);
 }
 if (!_193) {
 }
 if (!_196) {
  halide_printf(NULL, "Input buffer inPar is accessed at %d, which is before the min (%d) in dimension 1\n", _113, _inPar_min_1);
 }
 if (!_199) {
 }
 if (!_202) {
 }
 if (!_203) {
 }
 if (!_209) {
  halide_printf(NULL, "Total allocation for buffer f7 exceeds 2^31 - 1\n");
 }
 if (!_212) {
  halide_printf(NULL, "Total allocation for buffer f7 exceeds 2^31 - 1\n");
 }
 if (!_214) {
  halide_printf(NULL, "Product of extents for buffer f7 exceeds 2^31 - 1\n");
 }
 if (!_215) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
 }
 if (!_218) {
  halide_printf(NULL, "Total allocation for buffer inPar exceeds 2^31 - 1\n");
 }
 if (!_220) {
  halide_printf(NULL, "Product of extents for buffer inPar exceeds 2^31 - 1\n");
 }
 // produce f7
 #pragma omp parallel for
 for (int _f7_s0_x_xo_nid = 0; _f7_s0_x_xo_nid < 0 + _225; _f7_s0_x_xo_nid++)
 {
  for (int _f7_s0_y_yi_yi = 0; _f7_s0_y_yi_yi < 0 + 16; _f7_s0_y_yi_yi++)
  {
   for (int _f7_s0_x_xi_xi = 0; _f7_s0_x_xi_xi < 0 + 16; _f7_s0_x_xi_xi++)
   {
    {
     {
     } // overflow test f0
     float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_516);
     {
      {
      } // overflow test f4
      float *_f4 = (float *)halide_malloc(NULL, sizeof(float)*_782);
      {
       {
       } // overflow test f1
       float *_f1 = (float *)halide_malloc(NULL, sizeof(float)*_983);
       {
        {
        } // overflow test f5
        float *_f5 = (float *)halide_malloc(NULL, sizeof(float)*_1177);
        {
         {
         } // overflow test f2
         float *_f2 = (float *)halide_malloc(NULL, sizeof(float)*_1306);
         {
          {
          } // overflow test f6
          float *_f6 = (float *)halide_malloc(NULL, sizeof(float)*_1406);
          {
           {
           } // overflow test f3
           float *_f3 = (float *)halide_malloc(NULL, sizeof(float)*_1439);
           for (int _f7_s0_y_yi_yii = 0; _f7_s0_y_yi_yii < 0 + 32; _f7_s0_y_yi_yii++)
           {
            for (int _f7_s0_x_xi_xii_xii = 0; _f7_s0_x_xi_xii_xii < 0 + 8; _f7_s0_x_xi_xii_xii++)
            {
             // produce f0
             for (int _f0_s0_y = _1618; _f0_s0_y < _1618 + _1620; _f0_s0_y++)
             {
              for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + _1623; _f0_s0_x_x++)
              {
              } // for _f0_s0_x_x
             } // for _f0_s0_y
             // produce f4
             for (int _f4_s0_y = _1568; _f4_s0_y < _1568 + _1746; _f4_s0_y++)
             {
              for (int _f4_s0_x_x = 0; _f4_s0_x_x < 0 + _1749; _f4_s0_x_x++)
              {
              } // for _f4_s0_x_x
             } // for _f4_s0_y
             // produce f1
             for (int _f1_s0_y = _1568; _f1_s0_y < _1568 + _1887; _f1_s0_y++)
             {
              for (int _f1_s0_x_x = 0; _f1_s0_x_x < 0 + _1890; _f1_s0_x_x++)
              {
              } // for _f1_s0_x_x
             } // for _f1_s0_y
             // produce f5
             for (int _f5_s0_y = _1518; _f5_s0_y < _1518 + _2013; _f5_s0_y++)
             {
              for (int _f5_s0_x_x = 0; _f5_s0_x_x < 0 + _2016; _f5_s0_x_x++)
              {
              } // for _f5_s0_x_x
             } // for _f5_s0_y
             // produce f2
             for (int _f2_s0_y = _1518; _f2_s0_y < _1518 + _2154; _f2_s0_y++)
             {
              for (int _f2_s0_x_x = 0; _f2_s0_x_x < 0 + _2157; _f2_s0_x_x++)
              {
              } // for _f2_s0_x_x
             } // for _f2_s0_y
             // produce f6
             for (int _f6_s0_y = _1466; _f6_s0_y < _1466 + _2280; _f6_s0_y++)
             {
              for (int _f6_s0_x_x = 0; _f6_s0_x_x < 0 + _2283; _f6_s0_x_x++)
              {
              } // for _f6_s0_x_x
             } // for _f6_s0_y
             // produce f3
             for (int _f3_s0_y = _1466; _f3_s0_y < _1466 + _2421; _f3_s0_y++)
             {
              for (int _f3_s0_x_x = 0; _f3_s0_x_x < 0 + 1; _f3_s0_x_x++)
              {
              } // for _f3_s0_x_x
             } // for _f3_s0_y
            } // for _f7_s0_x_xi_xii_xii
           } // for _f7_s0_y_yi_yii
          } // alloc _f3
         } // alloc _f6
        } // alloc _f2
       } // alloc _f5
      } // alloc _f1
     } // alloc _f4
    } // alloc _f0
   } // for _f7_s0_x_xi_xi
  } // for _f7_s0_y_yi_yi
 } // for _f7_s0_x_xo_nid
} // if _172
}

