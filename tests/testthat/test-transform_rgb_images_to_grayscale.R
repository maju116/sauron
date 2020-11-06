context("transform_rgb_images_to_grayscale")

test_that("transform_rgb_images_to_grayscale transforms RGB images to grayscale", {
  test_input <- array(sample(0:255, 2*4*4*3, replace = TRUE), dim = c(2, 4, 4, 3))
  test_output <- transform_rgb_images_to_grayscale(test_input)$numpy()
  expected_output <- rowSums(test_input, dims = 3)

  expect_equal(test_output, expected_output)
})
