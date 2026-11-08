{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- Можно писать лямбды в своё удовольствие.
{- HLINT ignore "Redundant lambda" -}

-- | Домашка 4. Введение в Haskell: уровень 2.
--
-- Условие каждой задачи — комментарий перед её кодом; решение пишется на месте заглушки @todo@,
-- а в задаче 2.6 — на месте заглушки 'Todo' в типе.
module Level2 where

import Level1 (pair, fstChurch, sndChurch)
import MetaUtils (todo)
import TypeCheck (Todo)


-- 2.1. Варианты из λ-исчисления
--
-- Оттранслируйте в Haskell варианты в стиле чистого λ-исчисления (по Чёрчу): термы inl,
-- inr и either. Имя either в Haskell занято стандартной функцией, ваша называется eitherChurch.

inl = \x f g -> f x
inr = \x f g -> g x
eitherChurch = \f g v -> v f g


-- 2.2. Предыдущее число
--
-- Оттранслируйте в Haskell λ-терм, используя пары из задачи 1.1:
-- pred := λn s z. snd (n (λp. pair (s (fst p)) (fst p)) (pair z z))
-- В Haskell это predChurch, а fst и snd в нём — ваши fstChurch и sndChurch.
-- Ознакомьтесь с тем, как этот код тестируется в test/SpecLevel2.hs.


predChurch = \n s z -> sndChurch (n (\p -> pair (s (fstChurch p)) (fstChurch p)) (pair z z))


-- 2.3. Простые числа
--
-- Реализуйте проверку числа на простоту.

isPrime :: Integer -> Bool
isPrime n 
  | n < 2           = False
  | n == 2          = True
  | n `mod`  2 == 0 = False
  | otherwise       = all (\d -> n `mod` d /= 0) (takeWhile (\d -> d * d <= n) [3, 5 ..])



-- 2.4. Множество как функция
--
-- Известно, что множество задаётся своей характеристической функцией:
-- то есть функцией, которая возвращает False на значении, если такого элемента
-- в множестве нет, и True — если есть.
-- Реализуйте функции для работы с множеством строк, заданным такой функцией:
--    emptySet — задаёт пустое множество: emptySet "something" == False
--    (+++) — оператор добавления элемента в множество: (set +++ "elem") "elem" == True
--      Он должен быть левоассоциативным с 5-м приоритетом.
--    (///) — оператор удаления элемента из множества.
--      Он должен быть левоассоциативным с 5-м приоритетом.
-- Пример множества {"a", "b", "c"}: emptySet +++ "a" +++ "b" +++ "c".

emptySet :: String -> Bool
emptySet = todo "2.4 emptySet"

-- TODO объявление приоритета и ассоциативности
(+++) :: (String -> Bool) -> String -> (String -> Bool)
(+++) = todo "2.4 +++"

-- TODO объявление приоритета и ассоциативности
(///) :: (String -> Bool) -> String -> (String -> Bool)
(///) = todo "2.4 ///"


-- 2.5. Функция по типу
--
-- Реализуйте функцию, применяющую функцию к первой компоненте пары.
-- Двигайтесь от сигнатуры, как на паре: поставьте на место результата дыру `_` и читайте,
-- какой тип компилятор ждёт на её месте и что лежит в контексте (Relevant bindings).

first :: (a -> a') -> (a, b) -> (a', b)
first = todo "2.5"


-- 2.6. Предскажите тип: посложнее
--
-- Правила те же, что в задаче 1.6: замените Todo наиболее общим типом выражения
-- из комментария, тело оставьте заглушкой.

-- uncurry (flip const)
typeOfUncurryFlipConst :: Todo
typeOfUncurryFlipConst = todo "2.6 uncurry (flip const)"

-- curry id
typeOfCurryId :: Todo
typeOfCurryId = todo "2.6 curry id"

-- flip (.)
typeOfFlipCompose :: Todo
typeOfFlipCompose = todo "2.6 flip (.)"
