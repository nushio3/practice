// produce f3
#pragma omp parallel for
for (int _f3_s0_x_xo_nid = 0; _f3_s0_x_xo_nid < 0 + _161; _f3_s0_x_xo_nid++)
  {
    for (int _f3_s0_y_yi_yi = 0; _f3_s0_y_yi_yi < 0 + 16; _f3_s0_y_yi_yi++)
      {
	for (int _f3_s0_x_xi_xi = 0; _f3_s0_x_xi_xi < 0 + 16; _f3_s0_x_xi_xi++)
	  {
	    {
	      {
	      } // overflow test f0
	      float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_308);
	      {
		{
		} // overflow test f2
		float *_f2 = (float *)halide_malloc(NULL, sizeof(float)*_408);
		{
		  {
		  } // overflow test f1
		  float *_f1 = (float *)halide_malloc(NULL, sizeof(float)*_441);
		  for (int _f3_s0_y_yi_yii = 0; _f3_s0_y_yi_yii < 0 + 32; _f3_s0_y_yi_yii++)
		    {
		      for (int _f3_s0_x_xi_xii_xii = 0; _f3_s0_x_xi_xii_xii < 0 + 8; _f3_s0_x_xi_xii_xii++)
			{
			  // produce f0
			  for (int _f0_s0_y = _520; _f0_s0_y < _520 + _522; _f0_s0_y++)
			    {
			      for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + _525; _f0_s0_x_x++)
				{
				} // for _f0_s0_x_x
			    } // for _f0_s0_y
			  // produce f2
			  for (int _f2_s0_y = _468; _f2_s0_y < _468 + _648; _f2_s0_y++)
			    {
			      for (int _f2_s0_x_x = 0; _f2_s0_x_x < 0 + _651; _f2_s0_x_x++)
				{
				} // for _f2_s0_x_x
			    } // for _f2_s0_y
			  // produce f1
			  for (int _f1_s0_y = _468; _f1_s0_y < _468 + _789; _f1_s0_y++)
			    {
			      for (int _f1_s0_x_x = 0; _f1_s0_x_x < 0 + 1; _f1_s0_x_x++)
				{
				} // for _f1_s0_x_x
			    } // for _f1_s0_y
			} // for _f3_s0_x_xi_xii_xii
		    } // for _f3_s0_y_yi_yii
		} // alloc _f1
	      } // alloc _f2
	    } // alloc _f0
	  } // for _f3_s0_x_xi_xi
      } // for _f3_s0_y_yi_yi
  } // for _f3_s0_x_xo_nid
} // if _108
}

