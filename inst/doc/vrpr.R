## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4.2,
  fig.align = "center"
)

## -----------------------------------------------------------------------------
library(vrpr)

## -----------------------------------------------------------------------------
set.seed(1)
clients <- tibble::tibble(
  x = round(runif(20, -50, 50)),
  y = round(runif(20, -50, 50)),
  demand = sample(5:15, 20, replace = TRUE)
)

model <- vrp_model() |>
  add_depot(x = 0, y = 0) |>
  add_clients(clients) |>
  add_vehicle_type(num_available = 5, capacity = 50)

res <- vrp_solve(model, stop = max_iterations(500), seed = 1, display = FALSE)
res

## -----------------------------------------------------------------------------
cost(res)
head(routes(res))
summary(res)

## -----------------------------------------------------------------------------
plot(res)

## -----------------------------------------------------------------------------
# vrp_solve(model, stop = max_runtime(seconds = 10)) # wall-clock budget
# vrp_solve(model, stop = max_iterations(5000))      # iteration budget
# vrp_solve(model, stop = no_improvement(1000))      # stop when stuck

## -----------------------------------------------------------------------------
tw_clients <- tibble::tibble(
  x        = c(10, 20, 30, 40, 50, 60),
  y        = 0,
  demand   = 10,
  tw_early = c(0, 30, 60, 90, 120, 150),
  tw_late  = c(50, 80, 110, 140, 170, 200),
  service  = 10
)

vrptw <- vrp_model() |>
  add_depot(0, 0, tw_early = 0, tw_late = 500) |>
  add_clients(tw_clients) |>
  add_vehicle_type(num_available = 2, capacity = 60, tw_early = 0, tw_late = 500)

res_tw <- vrp_solve(vrptw, stop = max_iterations(500), seed = 1, display = FALSE)
routes(res_tw)[, c("route_id", "client", "start_service", "wait")]

## -----------------------------------------------------------------------------
het <- vrp_model() |>
  add_depot(0, 0) |>
  add_clients(clients) |>
  add_vehicle_type(num_available = 3, capacity = 50, unit_distance_cost = 1) |>
  add_vehicle_type(num_available = 3, capacity = 50, unit_distance_cost = 5)

res_het <- vrp_solve(het, stop = max_iterations(500), seed = 1, display = FALSE)
table(routes(res_het)$vehicle_type)

## -----------------------------------------------------------------------------
mdvrp <- vrp_model() |>
  add_depot(x = -50, y = 0) |>
  add_depot(x =  50, y = 0) |>
  add_clients(tibble::tibble(
    x = c(-55, -45, -50, 55, 45, 50),
    y = c(5, -5, 10, 5, -5, 8),
    demand = 10
  )) |>
  add_vehicle_type(num_available = 3, capacity = 50, depot = 1) |>
  add_vehicle_type(num_available = 3, capacity = 50, depot = 2)

res_md <- vrp_solve(mdvrp, stop = max_iterations(500), seed = 1, display = FALSE)
routes(res_md)[, c("route_id", "depot", "client")]

## -----------------------------------------------------------------------------
sh <- tibble::tibble(
  pickup_x   = c(-20, 10, 30),  pickup_y   = c(10, -25, 20),
  delivery_x = c(25, -15, -30), delivery_y = c(-10, 30, -20),
  amount     = c(4, 6, 3)
)

pdp <- vrp_model() |>
  add_depot(0, 0) |>
  add_shipments(sh) |>
  add_vehicle_type(num_available = 2, capacity = 10)

res_pdp <- vrp_solve(pdp, stop = max_iterations(300), seed = 1, display = FALSE)
routes(res_pdp)

## -----------------------------------------------------------------------------
pc <- vrp_model() |>
  add_depot(0, 0) |>
  add_clients(tibble::tibble(
    x = c(5, -5, 0, 100, 100),
    y = c(5, -5, 8, 10, -10),
    demand = 10,
    required = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    prize = c(0, 0, 0, 5, 500)
  )) |>
  add_vehicle_type(num_available = 4, capacity = 50)

res_pc <- vrp_solve(pc, stop = max_iterations(500), seed = 1, display = FALSE)
unvisited_clients(res_pc)

## -----------------------------------------------------------------------------
path <- system.file("extdata", "sample-n6-k2.vrp", package = "vrpr")
read_vrplib(path) |>
  vrp_solve(stop = max_iterations(200), seed = 1, display = FALSE) |>
  cost()

