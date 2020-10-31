var add = function(a, b) {
  return a + b;
}

var add2 = function(a) {
  return add(a, 2);
}

add(add2(1), 3);