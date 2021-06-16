---
title: 'Haskell Workshop - Einführung'
author: Carsten König
date: Juni 2021
geometry: margin=2cm
output: pdf_document
papersize: a4
---

# Haskell

- **reine** (_pure_) Sprache
- **lazy** Semantik
- **Type-Classes** für Funktionenüberladung/Polymorphismus
- eine Menge _Type-Foo_

---

## Werte und Typen

- _Typen_ werden in Haskell **groß** geschrieben (`String`, `Int`, ...)
- _Werte/Funktionen/Namen_ werden **klein** geschrieben

### Strings

```haskell
let cool = "Haskell"
```

sind tatsächlich _Listen von Char_

```haskell
import Data.Char(toUpper)

map toUpper cool
```

### Unit

Ein Typ `()`, der nur einen Wert annehmen kann `() :: ()`

### Bottom

für jeden Typ `t` gilt `undefined :: t` -
allerdings wirft ein Programm
einen Fehler, wenn `undefined` ausgewertet wird.

Anderes Beispiel:

```haskell
doh :: t
doh = doh
```

## Funktionen

### einfache Definition und Typen

```haskell
plus :: Int -> Int -> Int
plus10 x y = x + y
```

### Lambdas

Werden als `\x -> ...` geschrieben

```haskell
plus10 = \x -> x + 10
```

#### Quiz

Der Typ sagt viel - was kommt für

```Haskell
f :: a -> a
```

überhaupt in Frage?

---

##### Antwort

`id`

---

### Komposition / Verkettung

```haskell
plus25 :: Int -> Int
plus25 = plus 20 . plus 5
```

### ÜBUNG Fibonnaci Funktion schreiben

Schreibe

```haskell
fib :: Integer -> Integer
```

---

#### Lösung

```haskell
fib n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = fib (n-1) + fib (n-2)
```

## Daten

```haskell
data Option a
  = None
  | Some a


withDefault :: a -> Option a -> a
withDefault a None = a
withDefault _ (Some a) = a
```

### Übung

Schreibe: `mmap :: (a -> b) -> Option a -> Option b`

#### Lösung

```haskell
mmap :: (a -> b) -> Option a -> Option b
mmap f None = None
mmap f (Some a) = Some (f a)
```

---

damit wird `Option` zu einem Funktor:

```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

mit:

```haskell
instance Functor Option where
  fmap = mmap
```

### Tupel

Patternmatching (hier _irrefutable_)

```haskell
carsten = ("Carsten", 41)

(name, alter) = carsten
```

## Listen

```haskell
l = 1 : (2 : (3 : (4 : (5 : []))))
l = [1, 2, 3, 4, 5]
l = [1..5]
```

### List Comprehensions und Funktionen

```haskell
quadratZahlen :: [Integer]
quadratZahlen = [ n*n | n <- [1..] ]

geradeQuadratZahlen :: [Integer]
geradeQuadratZahlen = [ n | n <- quadratZahlen, even n ]
```

#### Kombinatorischen Spaß

```haskell
farbenUndZahlen :: [(String,Int)]
farbenUndZahlen = [ (f,z) | f <- farben, z <- zahlen ]
```

## IO / Monaden

Verbindung mit der _richtigen Welt_ funktioniert für
_reine_ Funktionen nicht (Beispiel Uhrzeit)

Die Lösung in Haskell ist der `IO` Type

Du kannst `IO a` lesen als: eine Aktion, die
ein `a` produziert.

Solche Aktionen oder _Kommandos_ können beliebig
in Haskell definiert werden, **ausgeführt** werden
Sie aber nur, wenn sie in _GHCi_ ausgewertet werden.

Wenn Du ein Programm kompilierst, wird die spezielle
`main :: IO ()` Aktion als _Einstiegspunkt_ benutzt,
d.h. das Programm fängt damit an, dieses `main`
Kommando auszuführen, wenn Du es startest.

Kommandos, die nur Seiteneffekte enthalten sind meist
vom Typ `IO ()`

### Beispiele

- `putStrLn` und `print` schreiben Ausgaben nach **stdout**
- `getLine` liest eine Zeile (bis zum `\n`) von **stdin**

### Verknüpfen / `do`-Notation

Kommandos können in einem speziellen `do`-Block miteinander
verknüpft werden - das ist dann im Prinzip _imperative Programmierung_.
Die Kommandos werden Zeile für Zeile ausgeführt:

```haskell
main :: IO ()
main = do
  putStr "Wie ist Dein Vorname? "
  vorname <- getLine
  putStr "Und Dein Nachname? "
  nachname <- getLine
  let name = vorname ++ " " ++ nachname
  putStrLn ("Hallo " ++ name)
```

- mit `x <- berechnung` wird die `berechnung :: IO a` ausgeführt, danach
  enthält `x` das Ergebnis der Berechnung (vom Typ `a`)
- mit `let x = wert` ist _reine_ Zuweisung (wie wir sie bisher kennen) damit
  ist auch klar, warum in _GHCi_ mit `let` gearbeitet werden muss - wir befinden
  uns dort in so einer Art _interaktiven_ `IO` Umgebung.
- `return x :: IO x` ist eine Berechnung, die `x` zurückgibt (ohne Seiteneffekte)

### `do` Notation funktioniert auch für andere Monaden

(mit `>>=`)

#### Listen

```haskell
kombinationen :: [(Int,String)]
kombinationen = do
  z <- [1..3]
  s <- ["A","B","C"]
  return (z,s)
```

In GHCi:

```haskell
> kombinationen
[(1,"A"),(1,"B"),(1,"C"),(2,"A"),(2,"B"),(2,"C"),(3,"A"),(3,"B"),(3,"C")]
```

## Higher-Kinded-Types

Typen haben _Kind_ `*` (oder `Type`)

Der `Maybe Int` ebenfalls - was ist `Maybe`?
Antwort: `Maybe :: * -> *`

Ermöglicht über `Functor`, `Monad` etc. zu abstrahieren - Beispiele folgen noch.

## Higher-Ranks

Wollen folgendes machen:

```haskell
showStuff :: Show a => (a -> String) -> String
showStuff sh = sh 5 ++ " - " ++ sh False
```

Funktioniert nicht - was ist 'a'

```haskell
showStuff :: forall a. Show a => (a -> String) -> String
showStuff sh = sh 5 ++ " - " ++ sh False
```

die Funktion sagt im Prinzip: für jedes `a` bin ich eine Funktion ...

Wollen:

```haskell
showStuff :: (forall a. Show a => a -> String) -> String
showStuff sh = sh 5 ++ " - " ++ sh False
```

Dafür RankNTypes

siehe auch

- [HaskellWiki: RankNTypes](https://wiki.haskell.org/Rank-N_types)
- [GHC UserGuide - RankNTypes](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rank_polymorphism.html)
