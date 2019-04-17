context("Neigbours")
library(sits.sampler)

test_that("shape of neighbors data.frame", {
    gn <- get_neighbors(lon = -74, lat = 4, win_size = 3, pix_res = list(x = 1, y = 1))
    expect_equal(nrow(gn), 9)
    expect_equal(ncol(gn), 5)
    expect_equal(gn$id_pix, unique(gn$id_pix))
})


