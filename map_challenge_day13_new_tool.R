if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse,tidycensus,dplyr,sf,tigris,lehdr,h3,mapgl)

# get jobs from LEHD ----

md_va_wac <- grab_lodes(
  state = c("md","va"),
  year = 2021,
  lodes_type = "wac",
  job_type = "JT00",
  agg_geo = "block",
  use_cache = TRUE
) %>%
  dplyr::select(GEOID = w_geocode, total_jobs = C000)

# get blocks, convert to points ----

dc_suburbs_blocks <- blocks(state = "MD", 
                            county = c("Montgomery","Prince George's"),
                            year = 2024) %>%
  dplyr::select(GEOID = GEOID20) %>%
  rbind(blocks(state = "vA",
               county = c("Alexandria","Arlington","Fairfax County","Fairfax City","Falls Church","Loudoun"),
               year = 2024) %>% 
          dplyr::select(GEOID = GEOID20))
st_point_on_surface()

# join jobs with blocks ----

dc_suburbs_wac_geo <- left_join(dc_suburbs_blocks, md_va_wac, by = "GEOID") %>% 
  st_transform(4326)

# Get h3 level 8 hexagons for the region ----

counties <- c("Alexandria","Arlington","Fairfax","Falls Church","Loudoun","Montgomery","Prince George's")

dc_suburbs_counties <- counties(c("MD","VA"), cb = TRUE) %>% 
  filter(NAME %in% counties & COUNTYFP != 121) %>% 
  st_union()

dc_suburbs_county_boundaries <- counties(c("MD","VA"), cb = TRUE) %>% 
  filter(NAME %in% counties & COUNTYFP != 121)

hexagons_suburbs <- h3::polyfill(dc_suburbs_counties, res = 8)

hex_suburbs_sf <- h3::h3_to_geo_boundary_sf(hexagons_suburbs)

hex_sf_place_suburbs <- hex_suburbs_sf %>% # joining place names to hex grid areas
  st_point_on_surface() %>% 
  st_join(places(state = c("MD","VA")) %>% dplyr::select(Place = NAME, geometry) %>% 
            st_transform(4326)) %>% 
  st_drop_geometry()

hex_sf_place_suburbs_join <- hex_suburbs_sf %>% # joining place names to hex grid areas
  left_join(hex_sf_place_suburbs, by = "h3_index")

# join the data ----

hex_total_jobs_suburbs <- st_join(hex_sf_place_suburbs_join, dc_suburbs_wac_geo %>% st_point_on_surface()) %>% 
  st_drop_geometry() %>% 
  summarize(total_jobs = sum(total_jobs, na.rm = TRUE), .by = h3_index)

hex_jobs_suburbs_sf <- left_join(hex_sf_place_suburbs_join, hex_total_jobs_suburbs, by = "h3_index")

# map in 3D ----
hex_map <- mapboxgl(style = mapbox_style("light"), 
                    customAttribution = "Data source: <a href='https://github.com/jamgreen/lehdr'>LODES / lehdr R package</a>") %>%
  fit_bounds(hex_jobs_suburbs_sf, pitch = 60, bearing = 30) %>%
  add_fill_extrusion_layer(
    id = "total-jobs",
    source = hex_jobs_suburbs_sf,
    fill_extrusion_color = interpolate(
      column = "total_jobs",
      values = c(0, 500, 5000, 15000, 37061),
      stops = c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c")
    ),
    fill_extrusion_height = interpolate(
      column = "total_jobs",
      values = c(0, 40000),
      stops = c(0, 40000)  # Adjust max height as needed
    ),
    fill_extrusion_opacity = 0.8,
    tooltip = "Place",
    hover_options = list(
      fill_extrusion_color = "yellow"
    )
  ) %>%
  add_line_layer(id = "moco",
                 source = dc_suburbs_county_boundaries,
                 line_color = "black") %>%
  add_legend(
    legend_title = "Total Jobs, 2021 LODES<br><span style='font-size: 80%; font-weight: normal;'>DC Metro Suburb Counties</span>",
    colors = c("#f7fbff", "#deebf7", "#9ecae1", "#3182bd", "#08519c"),
    values = c("0", "500", "5k", "15k", "40k")
  )
