-- Funkcje implementujące wyrażenia warunkowe i operacje logiczne 
if True x y = x; 
if False x y = y; 
and x y = if x y False; 
or x y = if x True y; 
not x = if x False True;

-- Listy zbudowane za konstruktorów Cons/2 i Nil/0
reverse = rev Nil where { rev a Nil = a; rev a (Cons x xs) = rev (Cons x a) xs; } append Nil ys = ys; 
append (Cons x xs) ys = Cons x (append xs ys); even Nil = True; even (Cons x Nil) = False; even (Cons x (Cons y ys)) = even ys; head (Cons x xs) = x; tail (Cons x xs) = xs;

-- Funkcje wyższych rzędów i rekursja 
app f g x = f (g x); 
curry f x y = f (Pair x y); 
uncurry g (Pair x y) = g x y;

factorial n = if (n < 0) (Nil) (fact n) where {
	fact 0 = 1;
	fact n = n * fact(n - 1);
}
