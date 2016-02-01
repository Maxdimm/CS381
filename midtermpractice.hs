module Midterm where

data A = OAO A
       | IBI B
  deriving(Show, Eq)

data B = BBC B B C
       | O
  deriving(Show, Eq)

data C = CCC C C C
       | I
  deriving(Show, Eq)

ioi :: A
ioi = IBI O

ooiii :: B
ooiii = BBC O O (CCC I I I)

iiiii :: C
iiiii = CCC I I (CCC I I I)

ooioiii :: B
ooioiii = BBC (BBC O O I) O (CCC I I I)

ooooiii :: B
ooooiii = BBC O (BBC O (BBC O O I) I) I

ooiiioi :: B
ooiiioi = BBC (BBC O O (CCC I I I)) O I
