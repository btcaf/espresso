# Oryginalny opis

W gramatyce została wprowadzona poprawka do krotek - wcześniej można było używać listy wyrażeń do indeksowania - oczywiście lista była pomyłką, a indeksowanie wyrażeniami uniemożliwiało statyczne typowanie, gdyż nie można było przewidzieć typu bez ewaluacji wyrażenia. Teraz krotki są indeksowane tylko za pomocą literałów. 

Zmienione zostało także traktowanie błędów czasu wykonania - nie skutkują wywołaniem wbudowanej procedury error, a odpowiednim komunikatem o błędzie ze strony interpretera.

Aktualnie także wszystkie argumenty są przekazywane przez wartość, a krotki nie są porównywalne - potencjalnie do poprawy w ostatecznej wersji.

Aktualna tabelka cech: 1-6, 9-13, 15-16

# Zmiany

Ogólne zmiany w języku: zmienne można przekazywać zarówno przez wartość, jak i przez referencję, tuple można porównywać oraz można przypisywać z pattern matchingiem. Poprawione zostało statyczne typowanie.

Zmiany w gramatyce: listy mają zmienioną składnię (z nawiasami kwadratowymi), bowiem wcześniej powodowało to konflikt. Dodano możliwość pattern matchingu tupli (składnia m{a, b} = [1, 2]). Można przekazywać zmienne przez referencję do funkcji z użyciem znaku "&", np. f(&a).

Zmiany w kodzie: naprawa wiązania statycznego, naprawa dodawania top-level funkcji, przekazywanie przez referencję, usunięcie zbędnych błędów "out of bounds" w runtimie, pattern matching i porównywanie tupli, wyrzucenie bloku z TFun, sprawdzanie, czy parametry funkcji nie powtarzają się i nie są typu void, wykonywanie ciała pętli oraz instrukcji warunkowych jako bloku.

Z uwag z pierwszej wersji zrealizowane zostały wszystkie poza kwestią dwóch funkcji main - aktualnie main może być redefiniowane i obowiązuje wyłącznie ostatnia definicja.

Aktualna tabelka cech: 1-7, 9-13, 15-16

## Nowe przykłady

Przykłady z tuplami zostały zmodyfikowane żeby pasowały do nowej składni.

### Good

zmiana nazwy: 15-tuples.txt => 15-basic-tuples.txt

15-pattern-matching.txt

6-7-reference.txt

9-13-inner2.txt

zmodyfikowano io.txt

### Bad

tc-tuples-argument.txt

tc-tuples-compare.txt

tc-tuples-index.txt

tc-tuples-patternmatching.txt

tc-tuples-return.txt

tc-badorder.txt

tc-repeatargs.txt

tc-void1.txt

tc-void2.txt
