library(rjson);

context("Abstract Syntax Tree");

test_that("Basics",{
 expect_identical(
  toJSON(r()$db("a")$table("b")$add()$query),
  "[24,[[15,[[14,[\"a\"]],\"b\"]]]]");
 expect_identical(
  toJSON(r()$db("a")$table("b")$query),
  "[15,[[14,[\"a\"]],\"b\"]]");

});

test_that("Functions",{
 Q<-r()$funcall(function(x) x,777)$query;
 expect_identical(toJSON(Q),
  "[64,[[69,[[2,[1]],[10,[1]]]],777]]")
 expect_error(r()$filter(function() r()$add(1))$query);
 expect_error(r()$filter(list(a=function(x) x))$query,
  "Functions can only exist as direct term arguments.");
})

test_that("Make array appears",{
 Q<-r()$db(c("a","b","c"))$query;
 expect_identical(toJSON(Q),"[14,[[2,[\"a\",\"b\",\"c\"]]]]");
})

test_that("Expressions",{
 Q<-r()$add(r()$add(1,2),4)$query;
 expect_identical(toJSON(Q),"[24,[[24,[1,2]],4]]");
})

test_that("Implicit var throws",{
 expect_error(r()$row("a"),"Implicit");
})

test_that("Complex list nesting maps as it should",{
 Q1<-r()$insert(list(a=list(list(a=3))))$query
 expect_identical(toJSON(Q1),"[56,[{\"a\":[2,[{\"a\":3}]]}]]")
 Q2<-r()$insert(list(a=list(list(a=list(r()$monday())))))$query
 expect_identical(toJSON(Q2),"[56,[{\"a\":[2,[{\"a\":[2,[[107,[]]]]}]]}]]")
})

test_that("Single element list is an array",{
 Q<-r()$insert(list(777))$query;
 expect_identical(toJSON(Q),"[56,[[2,[777]]]]");
})
