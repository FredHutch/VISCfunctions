context("test-collapse_group_row")

test_that("Does not allow non data.frames", {
    vect <- c(1, 2, 3, 4)
    expect_error(collapse_group_row(vect))
})


test_that("All column entries must be found in the data.frame", {
    sample_df <- data.frame(x = c(1, 1, 1, 2, 2), y = c(2, 3, 3, 2, 3), z = c(1, 
        2, 3, 4, 5))
    expect_error(collapse_group_row(vect, x, y, z, a))
})


test_that("Output columns are reordered and duplicated rows (based on columns pass) are removed", 
    {
        
        # z output order is 1:5
        sample_df <- data.frame(x = c(2, 2, 1, 1, 1), y = c(2, 3, 2, 3, 3), z = c(4, 
            5, 1, 2, 3))
        
        collapsed <- collapse_group_row(sample_df, x, y)
        
        expect_equal(collapsed$z, 1:5)
        expect_equivalent(collapsed, data.frame(x = c(1, NA, NA, 2, NA), y = c(2, 
            3, NA, 2, 3), z = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE))
    })


test_that("Handle spaces in column names of the input df", {
    
    # z output order is 1:5
    sample_df <- data.frame(`x 123` = c(2, 2, 1, 1, 1), y = c(2, 3, 2, 3, 3), z = c(4, 
        5, 1, 2, 3), check.names = FALSE)
    
    collapsed <- collapse_group_row(sample_df, `x 123`, y)
    
    expect_equal(collapsed$z, 1:5)
    expect_equivalent(collapsed, data.frame(`x 123` = c(1, NA, NA, 2, NA), y = c(2, 
        3, NA, 2, 3), z = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE, check.names = FALSE))
})
