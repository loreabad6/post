# summarise_geometry_* functions works as expected

    Code
      summarise_geometry_bbox(polygons)
    Output
      Geometry set for 5 features 
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -0.2974337 ymin: -0.0029756 xmax: 0.9730806 ymax: 1.153558
      Geodetic CRS:  WGS 84
    Message
      POLYGON ((0.4361026 0.7456421, 0.9005707 0.7456...
      POLYGON ((-0.2974337 0.5814869, 0.3796448 0.581...
      POLYGON ((0.2145846 0.4039274, 0.5898739 0.4039...
      POLYGON ((0.5272362 0.1748722, 0.9730806 0.1748...
      POLYGON ((0.2452129 -0.0029756, 0.6988282 -0.00...

---

    Code
      summarise_geometry_union(polygons)
    Output
      Geometry set for 5 features 
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -0.2974337 ymin: -0.0029756 xmax: 0.9730806 ymax: 1.153558
      Geodetic CRS:  WGS 84
    Message
      POLYGON ((0.4971355 0.8717961, 0.5474949 0.8088...
      POLYGON ((-0.1774445 0.5995572, -0.1843257 0.58...
      POLYGON ((0.28657 0.6085207, 0.2663408 0.622264...
      POLYGON ((0.5737123 0.4613799, 0.6063966 0.4263...
      POLYGON ((0.2973931 0.2557416, 0.2813385 0.2544...

---

    Code
      summarise_geometry_centroid(polygons)
    Output
      Geometry set for 5 features 
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -0.002543201 ymin: 0.17772 xmax: 0.752585 ymax: 0.9018588
      Geodetic CRS:  WGS 84
    Message
      POINT (0.647816 0.9018588)
      POINT (-0.002543201 0.8403816)
      POINT (0.3897167 0.5412363)
      POINT (0.752585 0.3538271)
      POINT (0.4690683 0.17772)

---

    Code
      summarise_geometry_bbox(polygons, rotated = TRUE)
    Condition
      Warning in `st_minimum_rotated_rectangle.sfc()`:
      st_minimum_rotated_rectangle does not work correctly for longitude/latitude data
    Output
      Geometry set for 5 features 
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -0.4157149 ymin: -0.01302435 xmax: 0.9881167 ymax: 1.165177
      Geodetic CRS:  WGS 84
    Message
      POLYGON ((0.5137787 1.117004, 0.9005707 0.86697...
      POLYGON ((-0.008469541 1.165177, 0.3796448 0.76...
      POLYGON ((0.3321186 0.7217665, 0.6273178 0.5211...
      POLYGON ((0.6633874 0.573032, 0.9881167 0.31222...
      POLYGON ((0.6988282 0.0046238, 0.2489106 -0.013...

---

    Code
      summarise_geometry_convex_hull(polygons)
    Output
      Geometry set for 5 features 
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -0.2974337 ymin: -0.0029756 xmax: 0.9730806 ymax: 1.153558
      Geodetic CRS:  WGS 84
    Message
      POLYGON ((0.7568336 0.7456421, 0.8157179 0.7671...
      POLYGON ((-0.2341393 0.5814869, -0.1843257 0.58...
      POLYGON ((0.4606164 0.4039274, 0.5749458 0.4441...
      POLYGON ((0.8757658 0.1748722, 0.8918794 0.1923...
      POLYGON ((0.505091 -0.0029756, 0.6988282 0.0046...

