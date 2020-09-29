module exmpl.

filter _ nil nil. 
filter Pred [H | T] [H | T1] :- Pred H, filter Pred T T1.
filter Pred [_ | T] T1 :- filter Pred T T1. 

fruit "apple". 
fruit "orange". 
fruit "banana".

