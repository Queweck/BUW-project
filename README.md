# BUW poject

## Opis projektu

WAŻNE: kod jest póki co w wersji surowej. Nie będzie zbyt czytelny

W ramach projektu chciałem zbadać jak różne czynniki wpływają na liczbę studentów w Bibliotece Uniwersyteckiej w Warszawie.

Najważniejsze czynniki podzieliłem na kategorie:
- pogodowe
- kalendarzowe
- studenckie (wydarzenia i inne)

---

## Metodologia

W analizie zastosowano:

- klasyczny model regresji liniowej (OLS)

Elementy weryfikacji poprawności modelu:
- test White’a i Breuscha–Pagana (heteroskedastyczność)
- test RESET (specyfikacja modelu)
- współczynnik VIF (współliniowość)
- estymację z macierzą wariancji odpornej (HC3)

Zmienna zależna:
- people - liczba studentów na buwie w danej godzinie

## Omówienie wyników

Najważniejsze wnioski:
- godzina ma sześcienny wpływ na liczbę studentów
- czym bliżej do sesji tym więcej studentów
- czym większe (co do wartości bezwzględnej) odchylenie temperatury od normy miesięcznej z danej godziny, tym więcej studentów
- istnieją różnice między dniami tygodnia
- generalnie studenci wolą przychodzić do BUWU w nocy niż rano
- wydarzenia tragiczne znacząco zmniejszają liczbę studentów na BUW, nawet w skali tygodniowej
- istnieją różnice semestralne
- deszcz nie ma wpływu na ilość osób w BUWie

## Wymagane pakiety R

- dplyr
- ggplot2
- lmtest
- sandwich
- car
- modelsummary
- flextable

---

## Autor

Michał Potasiński
Projekt wykonany jako hobby
