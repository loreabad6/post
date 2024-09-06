# filter works as expected and returns a post_table

    Code
      out1
    Output
      # cubble:   key: gid [2], index: datetime, nested form, [sf]
      # spatial:  [0, 0.54, 0.39, 0.84], WGS 84
      # temporal: datetime [date], geometry [POLYGON [°]]
        gid       long   lat                 geom_sum ts          
      * <chr>    <dbl> <dbl>              <POINT [°]> <list>      
      1 b     -0.00254 0.840 (-0.002543201 0.8403816) <sf [5 x 2]>
      2 c      0.390   0.541    (0.3897167 0.5412363) <sf [5 x 2]>

