# Interpreter

## Zrobione:
1. Co najmniej dwa typy wartości w wyrażeniach: int i bool
(to znaczy if 2+2 then _ parsuje się, ale wyrażenie ma niepoprawny typ).
2. Arytmetyka, porównania.
3. Wyrażenie warunkowe if.
4. Funkcje wieloargumentowe, rekurencja.
5. Funkcje anonimowe, częściowa aplikacja, funkcje wyższego rzędu, domknięcia.
6. Obsługa błędów wykonania, np. dzielenie przez zero (może być elegancki komunikat i zatrzymanie interpretera).
9. Statyczne wiązanie identyfikatorów przy dowolnym poziomie zagnieżdżenia definicji.
10. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem
wykonania programu). Na tym poziomie można wymagać jawnego podawania typów.
12 Dowolne zagnieżdżenie wzorców w pattern matchingu.

## Częściowo zrobione:
11. Ogólne polimorficzne i rekurencyjne typy algebraiczne.
Mile widziane wykonane w samym języku definicje typów List (składnia może być inna niż w
Haskellu, lukier syntaktyczny nie wymagany), Maybe i Either oraz ich zastosowania w
przykładowych programach.

## Do zrobienia:
Ostrzeżenie przy próbie zdefiniowania funkcji częściowej.
13. Typy polimorficzne w stylu ML (jak Caml lub Haskell bez klas) z algorytmem rekonstrukcji
typów. Nie jest konieczna (ale zabroniona też nie) składnia do deklarowania (skrótów) typów.
