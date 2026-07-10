# ============================================================================
# Meshedness coefficient for VA counties  (FIXED)
# ----------------------------------------------------------------------------
# What was wrong in the original and what changed:
#
# 1. THE M > 1 BUG (most important):
#    Original counted vertices and edges on DIFFERENT graphs:
#         verts <- sum(deg >= 2)   # dropped degree-1 (dead-end) NODES
#         edges <- gsize(g)        # but kept ALL edges, incl. dead-end edges
#    The meshedness formula  M = (e - v + 1) / (2v - 5)  is only bounded by 1
#    when e and v are counted on the SAME graph. Dropping nodes but not their
#    edges inflates the numerator and shrinks the denominator, so M blows past 1.
#    Even a pure tree (which must give M = 0) returned a positive M this way.
#
# 2. PLANARITY:
#    M assumes a *planar* graph (max edges = 3v-6  ->  max cycles = 2v-5).
#    Raw OSM linestrings only create nodes at their endpoints, so two roads that
#    cross mid-segment are NOT split -> the graph is non-planar and M is
#    meaningless. We fix this with to_spatial_subdivision() (node the network at
#    every shared intersection) before counting.
#
# 3. PSEUDO-NODES:
#    Meshedness is NOT invariant to degree-2 "shape" vertices that OSM scatters
#    along a road. They leave the cycle count unchanged but inflate v, dragging
#    M down. to_spatial_smooth() removes them so nodes = true intersections /
#    dead-ends only (the "primal" road graph).
#
# 4. DISCONNECTED PIECES:
#    The 2v-5 cap assumes ONE connected component. A county clip is usually
#    several disconnected fragments. We take the largest connected component so
#    the formula is valid (this is standard practice for road networks).
#
# 5. DEAD ENDS:
#    Sophia's instinct to drop degree-1 dead ends is reasonable. The correct way
#    is the graph 2-core (igraph coreness >= 2), which removes dead-end nodes
#    AND their edges together, iteratively. Toggle with prune_dead_ends below.
#
# 6. SPEED:
#    Original did st_intersection() of the FULL state road layer against every
#    county = O(counties x all_roads). We first st_filter() with the spatial
#    index to grab only candidate roads, then intersect. Much faster, same result.
# ============================================================================

library(tigris)
library(sf)
library(sfnetworks)
library(igraph)
library(osmextract)
library(dplyr)
library(tidygraph)

sf::sf_use_s2(TRUE)

# ---- toggles ---------------------------------------------------------------
prune_dead_ends <- TRUE   # TRUE -> use 2-core (drops dead-end chains), matches
                          #         Sophia's "rule out degree-1" intent, done right.
                          # FALSE -> meshedness on the full cleaned graph.

# ---- 1. Get VA road network (cached locally by osmextract) ------------------
# IMPORTANT: a stale, partial cache (e.g. only the Blacksburg/Montgomery Co.
# area) gets silently reused and will make EVERY other county clip to zero roads
# -> you'd only get a meshedness value for Montgomery. Force the full state.
va_roads <- oe_get(
  place = "Virginia",
  provider = "geofabrik",          # us/virginia = the whole state
  layer = "lines",
  force_download = TRUE,           # ignore any stale partial .pbf
  force_vectortranslate = TRUE,    # rebuild the .gpkg from the full .pbf
  query = "
    SELECT *
    FROM lines
    WHERE highway IN (
      'motorway','trunk','primary','secondary',
      'tertiary','residential','unclassified'
    )
  "
)

va_roads <- st_make_valid(st_transform(va_roads, 4326))
message("Road features loaded: ", nrow(va_roads))

va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_transform(st_crs(va_roads)) %>%
  st_make_valid()

# SANITY CHECK (no hard-coded coordinates): use the boundaries we already have.
# A partial cache (e.g. only the Blacksburg area) leaves most counties with no
# roads -- exactly the failure that gave only Montgomery. Test that directly.
n_with_roads <- sum(lengths(st_intersects(va_counties, va_roads)) > 0)
message(n_with_roads, " of ", nrow(va_counties), " counties contain roads.")
if (n_with_roads < 0.9 * nrow(va_counties)) {
  stop("Road extract looks partial (covers few counties). Delete the *virginia* ",
       "files in oe_download_directory() and rerun with force_download = TRUE.")
}

# ---- 2. Helper: meshedness for one cleaned road geometry -------------------
calc_meshedness <- function(roads_geom, prune = TRUE) {

  # keep only line geometries, force to LINESTRING
  roads_geom <- roads_geom %>%
    filter(st_geometry_type(.) %in% c("LINESTRING", "MULTILINESTRING")) %>%
    st_cast("LINESTRING", warn = FALSE)

  if (nrow(roads_geom) == 0) return(NA_real_)

  # build network, then make it planar + simple + smoothed
  net <- as_sfnetwork(roads_geom, directed = FALSE) %>%
    convert(to_spatial_subdivision, .clean = TRUE) %>%  # node at intersections -> planar
    convert(to_spatial_smooth,      .clean = TRUE)      # drop degree-2 pseudo-nodes

  g <- as.igraph(net)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

  # largest connected component so the 2v-5 cap is valid
  comp   <- components(g)
  giant  <- induced_subgraph(g, which(comp$membership == which.max(comp$csize)))

  # optional: 2-core removes dead-end nodes AND their edges (consistent!)
  if (prune) {
    core   <- coreness(giant)
    giant  <- induced_subgraph(giant, which(core >= 2))
    # after pruning, re-take the giant component (pruning can fragment it)
    if (gorder(giant) > 0) {
      comp  <- components(giant)
      giant <- induced_subgraph(giant, which(comp$membership == which.max(comp$csize)))
    }
  }

  v <- gorder(giant)   # number of nodes
  e <- gsize(giant)    # number of edges  -- SAME graph as v now

  if (v < 3) return(NA_real_)   # formula undefined for tiny graphs

  m <- (e - v + 1) / (2 * v - 5)
  # clamp tiny floating / non-planarity artifacts into [0,1]
  max(0, min(1, m))
}

# ---- 3. Loop over ALL counties (robust: one bad county can't stop the run) --
# Goal: ONE meshedness value per VA county / independent city (~133 rows).
ids   <- unique(va_counties$GEOID)
n_tot <- length(ids)
message("Computing meshedness for ", n_tot, " VA county-equivalents...")

rows <- vector("list", n_tot)

for (i in seq_along(ids)) {
  id          <- ids[i]
  county_poly <- va_counties %>% filter(GEOID == id)
  nm          <- county_poly$NAME[1]

  # Wrap the WHOLE per-county body. If clipping OR the calc throws, we record
  # NA for that county and keep going instead of aborting the entire loop.
  m <- tryCatch({
    cand <- suppressWarnings(st_filter(va_roads, county_poly,
                                       .predicate = st_intersects))
    if (nrow(cand) == 0) {
      NA_real_
    } else {
      dat <- suppressWarnings(st_intersection(cand, county_poly))
      calc_meshedness(dat, prune = prune_dead_ends)
    }
  }, error = function(err) {
    message("  FAILED ", nm, " (", id, "): ", err$message)
    NA_real_
  })

  rows[[i]] <- data.frame(GEOID = id, NAME = nm, meshedness = m)
  message(sprintf("[%d/%d] %-22s M = %s", i, n_tot, nm,
                  ifelse(is.na(m), "NA", round(m, 3))))

  # incremental save so an interruption never loses progress
  # write.csv(do.call(rbind, rows[seq_len(i)]),
  #           here::here("data","outcome","meshedness_va.csv"), row.names = FALSE)
}

results <- do.call(rbind, rows) %>% arrange(desc(meshedness))
message("\nDone. Rows: ", nrow(results),
        " | failed/NA: ", sum(is.na(results$meshedness)))
# results is a base data.frame -- `n=` is tibble-only, so wrap it to print all rows
print(tibble::as_tibble(results), n = nrow(results))

# write.csv(results, here::here("data","outcome","meshedness_va.csv"), row.names = FALSE)
