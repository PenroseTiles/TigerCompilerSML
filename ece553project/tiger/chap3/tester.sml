fun p x = Parse.parse ("../testcases/test"^x^".tig")

exception Nondigit
exception Neg

fun i2c 0 = #"0"
  | i2c 1 = #"1"
  | i2c 2 = #"2"
  | i2c 3 = #"3"
  | i2c 4 = #"4"
  | i2c 5 = #"5"
  | i2c 6 = #"6"
  | i2c 7 = #"7"
  | i2c 8 = #"8"
  | i2c 9 = #"9"
  | i2c _ = raise Nondigit

fun listDigits 0 = []
  | listDigits x = if (x>0) then (x mod 10)::listDigits(x div 10) else raise Neg

fun i2a n = String.implode (map i2c (rev (listDigits n)))

val ls = tl (List.tabulate(49, fn x => x))

fun f x = p(i2a(x))
