module Queue(Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) 
    where 

--Queue
data Queue a = Q [a]
    deriving Show 

emptyQ :: Queue a 
emptyQ = Q []

isEmptyQ :: Queue a -> Bool 
isEmptyQ (Q []) = True
isEmptyQ (Q _) = False 

enqueue :: a -> Queue a -> Queue a 
enqueue x (Q []) = (Q [x])
enqueue x (Q (as)) = (Q (as ++ [x]))

firstQ :: Queue a -> a 
firstQ (Q as) = head as 

dequeue :: Queue a -> Queue a 
dequeue (Q (a:as)) = (Q as) 