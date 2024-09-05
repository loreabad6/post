# as_post_table works as expected

    Code
      as_post_table(polygons)
    Output
      # cubble:   key: gid [5], index: datetime, nested form, [sf]
      # spatial:  [0, 0.18, 0.75, 0.9], WGS 84
      # temporal: datetime [date], geometry [POLYGON [°]]
        gid       long   lat                 geom_sum ts          
      * <chr>    <dbl> <dbl>              <POINT [°]> <list>      
      1 a      0.648   0.902     (0.647816 0.9018588) <sf [5 x 2]>
      2 b     -0.00254 0.840 (-0.002543201 0.8403816) <sf [5 x 2]>
      3 c      0.390   0.541    (0.3897167 0.5412363) <sf [5 x 2]>
      4 d      0.753   0.354     (0.752585 0.3538271) <sf [5 x 2]>
      5 e      0.469   0.178      (0.4690683 0.17772) <sf [5 x 2]>

---

    Code
      as_post_table(as_post_array(polygons))
    Output
      # cubble:   key: gid [5], index: datetime, nested form, [sf]
      # spatial:  [0, 0.18, 0.75, 0.9], WGS 84
      # temporal: datetime [date], geometry [POLYGON [°]]
        gid       long   lat                 geom_sum ts          
      * <chr>    <dbl> <dbl>              <POINT [°]> <list>      
      1 a      0.648   0.902     (0.647816 0.9018588) <sf [5 x 2]>
      2 b     -0.00254 0.840 (-0.002543201 0.8403816) <sf [5 x 2]>
      3 c      0.390   0.541    (0.3897167 0.5412363) <sf [5 x 2]>
      4 d      0.753   0.354     (0.752585 0.3538271) <sf [5 x 2]>
      5 e      0.469   0.178      (0.4690683 0.17772) <sf [5 x 2]>

---

    Code
      as_post_table(as_post_table(polygons))
    Output
      # cubble:   key: gid [5], index: datetime, nested form, [sf]
      # spatial:  [0, 0.18, 0.75, 0.9], WGS 84
      # temporal: datetime [date], geometry [POLYGON [°]]
        gid       long   lat                 geom_sum ts          
      * <chr>    <dbl> <dbl>              <POINT [°]> <list>      
      1 a      0.648   0.902     (0.647816 0.9018588) <sf [5 x 2]>
      2 b     -0.00254 0.840 (-0.002543201 0.8403816) <sf [5 x 2]>
      3 c      0.390   0.541    (0.3897167 0.5412363) <sf [5 x 2]>
      4 d      0.753   0.354     (0.752585 0.3538271) <sf [5 x 2]>
      5 e      0.469   0.178      (0.4690683 0.17772) <sf [5 x 2]>

