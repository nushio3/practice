// produce f1
#pragma omp parallel for
for (int _f1_s0_x_xo_nid = 0; _f1_s0_x_xo_nid < 0 + _129; _f1_s0_x_xo_nid++)
  {
    for (int _f1_s0_y_yi_yi = 0; _f1_s0_y_yi_yi < 0 + 16; _f1_s0_y_yi_yi++)
      {
	for (int _f1_s0_x_xi_xi = 0; _f1_s0_x_xi_xi < 0 + 16; _f1_s0_x_xi_xi++)
	  {
	    {
	      {
	      } // overflow test f0
	      float *_f0 = (float *)halide_malloc(NULL, sizeof(float)*_180);
	      for (int _f1_s0_y_yi_yii = 0; _f1_s0_y_yi_yii < 0 + 32; _f1_s0_y_yi_yii++)
		{
		  for (int _f1_s0_x_xi_xii_xii = 0; _f1_s0_x_xi_xii_xii < 0 + 8; _f1_s0_x_xi_xii_xii++)
		    {
		      // produce f0
		      for (int _f0_s0_y = _207; _f0_s0_y < _207 + _209; _f0_s0_y++)
			{
			  for (int _f0_s0_x_x = 0; _f0_s0_x_x < 0 + 1; _f0_s0_x_x++)
			    {
			    } // for _f0_s0_x_x
			} // for _f0_s0_y
		    } // for _f1_s0_x_xi_xii_xii
		} // for _f1_s0_y_yi_yii
	    } // alloc _f0
	  } // for _f1_s0_x_xi_xi
      } // for _f1_s0_y_yi_yi
  } // for _f1_s0_x_xo_nid
} // if _76
}

