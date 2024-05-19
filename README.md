W gramatyce została wprowadzona poprawka do krotek - wcześniej można było używać listy wyrażeń do indeksowania - oczywiście lista była pomyłką, a indeksowanie wyrażeniami uniemożliwiało statyczne typowanie, gdyż nie można było przewidzieć typu bez ewaluacji wyrażenia. Teraz krotki są indeksowane tylko za pomocą literałów. 

Zmienione zostało także traktowanie błędów czasu wykonania - nie skutkują wywołaniem wbudowanej procedury error, a odpowiednim komunikatem o błędzie ze strony interpretera.

Aktualnie także wszystkie argumenty są przekazywane przez wartość, a krotki nie są porównywalne - potencjalnie do poprawy w ostatecznej wersji.

Aktualna tabelka cech: 1-6, 9-13, 15-16