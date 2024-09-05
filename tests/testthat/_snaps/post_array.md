# as_post_array works as expected

    Code
      as_post_array(polygons)
    Output
      stars object with 2 dimensions and 1 attribute
      attribute(s):
               geometry  
       POLYGON      :25  
       epsg:4326    : 0  
       +proj=long...: 0  
      dimension(s):
               from to     offset  delta refsys point
      geom_sum    1  5         NA     NA WGS 84  TRUE
      datetime    1  5 2020-10-01 1 days   Date FALSE
                                                                 values
      geom_sum POINT (0.647816 0.9018588),...,POINT (0.4690683 0.17772)
      datetime                                                     NULL

---

    Code
      as_post_array(as_post_table(polygons))
    Output
      stars object with 2 dimensions and 1 attribute
      attribute(s):
               geometry  
       POLYGON      :25  
       epsg:4326    : 0  
       +proj=long...: 0  
      dimension(s):
               from to     offset  delta refsys point
      geom_sum    1  5         NA     NA WGS 84  TRUE
      datetime    1  5 2020-10-01 1 days   Date FALSE
                                                                 values
      geom_sum POINT (0.647816 0.9018588),...,POINT (0.4690683 0.17772)
      datetime                                                     NULL

---

    Code
      as_post_array(as_post_array(polygons))
    Output
      stars object with 2 dimensions and 1 attribute
      attribute(s):
               geometry  
       POLYGON      :25  
       epsg:4326    : 0  
       +proj=long...: 0  
      dimension(s):
               from to     offset  delta refsys point
      geom_sum    1  5         NA     NA WGS 84  TRUE
      datetime    1  5 2020-10-01 1 days   Date FALSE
                                                                 values
      geom_sum POINT (0.647816 0.9018588),...,POINT (0.4690683 0.17772)
      datetime                                                     NULL

# get_group_ids works as expected

    Code
      get_group_ids(as_post_array(polygons))
    Output
        gid
      1   a
      2   b
      3   c
      4   d
      5   e

