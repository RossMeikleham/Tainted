# Tainted
Tainted type, and associated operations 

A Tainted type is, a value starts off clean and once an operation
which causes it to become dirty, and further operation keeps it dirty.
This is similar to the Maybe monad except once the dirty has been
reached, calculations can still be performed on the value it contains.

```
import Data.Tainted

data AS


```
