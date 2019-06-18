# Interpreter

## Jak odpalić

Kompilacja interpretera za pomocą `stack build`. Dostępne są dwa tryby:

1. Interpreter może czytać ze standardowego wejścia, wtedy każda linia jest interpretowana osobno, ale stan jest zachowany między wykonaniami linii.
```stack exec interpreter```

2. Intepreter może wykonać jeden lub wiele plików, wtedy każdy plik jest interpretowany osobno, ale stan jest zachowany między wykonaniami plików.
```stack exec interpreter good/stdlib.cl good/lists.cl```

Program składa się z trzech typów instrukcji: wyrażenia, deklaracji zmiennej (w tym funkcji), definicji typu. Po każdym wyrażeniu jest wypisywany typ i wartość wyrażenia. Po deklaracji zmiennej lub funkcji jest wypisywany typ. Po definicji typu są wypisywane typy jego konstruktorów wartości.

## Zakres
Interpreter leniwego (call-by-name), silnie statycznie typowanego, funkcyjnego języka.

### Zrobione
1. Co najmniej dwa typy wartości w wyrażeniach: int i bool
(to znaczy if 2+2 then _ parsuje się, ale wyrażenie ma niepoprawny typ).
2. Arytmetyka, porównania.
3. Wyrażenie warunkowe if.
4. Funkcje wieloargumentowe, rekurencja.
5. Funkcje anonimowe, częściowa aplikacja, funkcje wyższego rzędu, domknięcia.
6. Obsługa błędów wykonania, np. dzielenie przez zero (może być elegancki komunikat i zatrzymanie interpretera).
7. Nie dotyczy
8. Nie dotyczy
9. Statyczne wiązanie identyfikatorów przy dowolnym poziomie zagnieżdżenia definicji.
10. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem
wykonania programu). Na tym poziomie można wymagać jawnego podawania typów.
11. Ogólne polimorficzne i rekurencyjne typy algebraiczne.
Mile widziane wykonane w samym języku definicje typów List (składnia może być inna niż w
Haskellu, lukier syntaktyczny nie wymagany), Maybe i Either oraz ich zastosowania w
przykładowych programach.
12. Dowolne zagnieżdżenie wzorców w pattern matchingu.
13. Typy polimorficzne w stylu ML (jak Caml lub Haskell bez klas) z algorytmem rekonstrukcji
typów. Nie jest konieczna (ale zabroniona też nie) składnia do deklarowania (skrótów) typów.

### Mogłoby być
1. Ujemne liczb
2. Lukier syntaktyczny dla list

## Źródła
1. Inferencja na podstawie http://dev.stephendiehl.com/fun/006_hindley_milner.html oraz slajdów z wykładu.