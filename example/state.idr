module Main

import IQuery

push : StateC (STList STString) -> Event -> JS_IO Int
push s e = do
  Just input <- query "input#pushVal" >>= (\x => elemAt x 0)
  Just xs    <- get s =>> fromState
  text <- getValue input
  get s :=> toState (text :: xs)
  pure 1

shift : StateC (STList STString) -> Event -> JS_IO Int
shift s e = do
  Just x <- get s =>> access 0 =>> fromState
    | Nothing => do
                 alert "stack is empty"
                 pure 1
  alert x
  Just (_::xs) <- get s =>> fromState
  get s :=> toState xs
  pure 1

setV : Event -> JS_IO Int
setV e = do
     Just el <- !(query "input#pushVal") `elemAt` 0
        | Nothing => do
            alert "no such element 'input#pushVal'"
            pure 1
     setValue el "wohoo"
     pure 1

tryOnClick : (queryString : String) -> (listener:Event -> JS_IO Int) -> JS_IO ()
tryOnClick queryString listener= do
  Just el <- !(query queryString) `elemAt` 0
    | Nothing => alert ("no such element '" ++ queryString ++ "'")
  onClick el listener

main : JS_IO ()
main = do
  queue <- newState (STList STString) Nil
  tryOnClick "input#pushAct" (push queue)
  tryOnClick "input#shiftAct" (shift queue)
  tryOnClick "input#setVal" setV
