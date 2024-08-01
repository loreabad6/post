library(sf)

# Fixed seed for reproducibility
set.seed(986)

# Create a bounding box as a 0-1 square
bb = c(
  st_point(c(0,0)),
  st_point(c(0,1)),
  st_point(c(1,1)),
  st_point(c(1,0))
) |> st_cast("POLYGON")

# Find the centroid and create a circle for intersection
center = st_centroid(bb)
circle = st_buffer(center, 0.8) |>
  st_simplify()

# Sample random points inside the bounding box and union them
pts_t0 = st_sample(bb, 25) |>
  st_union()

# Compute voronoi polygons within the envelope
# intersect with buffered centroid and sample 5 polygons
pol_t0 = st_voronoi(pts_t0, envelope = bb) |>
  st_collection_extract("POLYGON") |>
  st_intersection(circle) |>
  sample(5)

# Create polygons for next timestamps using jitter and simplify
pol_t1 = st_jitter(pol_t0, factor = 0.05)
pol_t2 = st_jitter(pol_t1, factor = 0.05) |>
  st_simplify(dTolerance = 0.1)
pol_t3 = st_jitter(pol_t2, factor = 0.02)  |>
  st_buffer(-0.02)
pol_t4 = st_jitter(pol_t0, factor = 0.07)

# Set dates
date = seq(
  from = as.Date("2020-10-01"),
  to = as.Date("2020-10-05"),
  length.out = 5
)

# Combine results in one sf object and add CRS
polygons = rbind(
  st_sf(gid = letters[1:5], datetime = date[[1]], geometry = pol_t0),
  st_sf(gid = letters[1:5], datetime = date[[2]], geometry = pol_t1),
  st_sf(gid = letters[1:5], datetime = date[[3]], geometry = pol_t2),
  st_sf(gid = letters[1:5], datetime = date[[4]], geometry = pol_t3),
  st_sf(gid = letters[1:5], datetime = date[[5]], geometry = pol_t4)
) |> st_set_crs(4326)

# Save as data for the package
usethis::use_data(polygons, overwrite = TRUE)
