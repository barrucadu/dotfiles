(* Implementation of some 3D vector stuff so I can get used to the syntax *)
open Math;

(* ***** DEFINITIONS ***** *)

(* Vector type *)
type vect = real * real * real;

(* The zero vector *)
val zerovect = (0.0, 0.0, 0.0);

(* ***** UTILITIES ***** *)

(* Make a vector with all three components the same value *)
fun mkvect (x : real) : vect = (x, x, x);

(* Square a real value *)
fun sq x : real = x * x;

(* ***** VECTOR ARITHMATIC ***** *)

(* Negate a vector *)
fun vector_neg vect = vector_mul (vect, mkvect ~1.0)

(* Add two vectors *)
fun vector_add ((x1, y1, z1), (x2, y2, z2)) : vect = (x1 + x2, y1 + y2, z1 + z2);

(* Subtract two vectors *)
fun vector_sub (vecta, vectb) = vector_add (vecta, vector_neg (vectb));

(* Cross product of two vectors *)
fun vector_cross ((x1, y1, z1), (x2, y2, z2)) : vect = (x1 * x2, y1 * y2, z1 * z2);

(* Dot product of two vectors *)
fun vector_dot ((x1, y1, z1), (x2, y2, z2)) : real = x1 * x2 + y1 * y2 + z1 * z2;

(* Infix operators *)
infix vadd;
infix vsub;
infix vcross;
infix vdot;

fun a vadd b   = vector_add   (a, b);
fun a vsub b   = vector_sub   (a, b);
fun a vcross b = vector_cross (a, b);
fun a vdot b   = vector_dot   (a, b);

(* ***** OTHER FUNCTIONS ***** *)

(* Get the size of a vector *)
fun vector_size (x, y, z) = sqrt (sq x + sq y + sq z);

(* Get the distance between two vectors *)
fun vector_dist vects = vector_size (vector_sub vects);

(* Scale a vector *)
fun vector_scale (vect, factor) = vector_mul (vect, mkvect factor);
