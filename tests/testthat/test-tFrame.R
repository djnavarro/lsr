Gf <- c(105, 119, 121, 98)
Gc <- c(110, 115, 119, 103)
Gs <- c(112, 102, 108, 99)
dataset <- data.frame(Gf, Gc, Gs)
rownames(dataset) <- paste( "person", 1:4, sep="")

person1 <- c(105, 110, 112)
person2 <- c(119, 115, 102)
person3 <- c(121, 119, 108)
person4 <- c(98, 103, 99)
dataset2 <- data.frame(person1, person2, person3, person4)
rownames(dataset2) <- c("Gf", "Gc", "Gs")

test_that("tFrame example works", {
  expect_equal(tFrame(dataset), dataset2)
})
