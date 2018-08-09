{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Char.Alpha where

import Control.Applicative((<|>))
import Control.Category(id, (.))
import Control.Lens(Prism', Iso', prism', iso, from, involuted, (^.), ( # ))
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor((<$), (<$>))
import Data.List(lookup)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Maybe(Maybe(Just, Nothing), fromMaybe)
import Data.Ord(Ord)
import GHC.Generics(Generic)
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))
import Prelude(Show)

data A_a =
  A_a
  deriving (Eq, Ord, Show, Generic)

class Ca x where
  _a' ::
    Prism' x ()
  _a ::
    x
  _a =
    _a' # ()

instance Ca () where
  _a' =
    id

instance Ca A_a where
  _a' =
    prism'
      (\() -> A_a)
      (\A_a -> Just ())

instance Ca Char where
  _a' =
    prism'
      (\() -> 'a')
      (\c ->  case c of
                'a' ->
                  Just ()
                _ ->
                  Nothing)

parse_a ::
  (Ca x, CharParsing p) =>
  p x
parse_a =
  _a <$ char 'a' <?> "a"

data B_b =
  B_b
  deriving (Eq, Ord, Show, Generic)

class Cb x where
  _b' ::
    Prism' x ()
  _b ::
    x
  _b =
    _b' # ()

instance Cb () where
  _b' =
    id

instance Cb B_b where
  _b' =
    prism'
      (\() -> B_b)
      (\B_b -> Just ())

instance Cb Char where
  _b' =
    prism'
      (\() -> 'b')
      (\c ->  case c of
                'b' ->
                  Just ()
                _ ->
                  Nothing)

parse_b ::
  (Cb x, CharParsing p) =>
  p x
parse_b =
  _b <$ char 'b' <?> "b"

data C_c =
  C_c
  deriving (Eq, Ord, Show, Generic)

class Cc x where
  _c' ::
    Prism' x ()
  _c ::
    x
  _c =
    _c' # ()

instance Cc () where
  _c' =
    id

instance Cc C_c where
  _c' =
    prism'
      (\() -> C_c)
      (\C_c -> Just ())

instance Cc Char where
  _c' =
    prism'
      (\() -> 'c')
      (\c ->  case c of
                'c' ->
                  Just ()
                _ ->
                  Nothing)

parse_c ::
  (Cc x, CharParsing p) =>
  p x
parse_c =
  _c <$ char 'c' <?> "c"

data D_d =
  D_d
  deriving (Eq, Ord, Show, Generic)

class Cd x where
  _d' ::
    Prism' x ()
  _d ::
    x
  _d =
    _d' # ()

instance Cd () where
  _d' =
    id

instance Cd D_d where
  _d' =
    prism'
      (\() -> D_d)
      (\D_d -> Just ())

instance Cd Char where
  _d' =
    prism'
      (\() -> 'd')
      (\c ->  case c of
                'd' ->
                  Just ()
                _ ->
                  Nothing)

parse_d ::
  (Cd x, CharParsing p) =>
  p x
parse_d =
  _d <$ char 'd' <?> "d"

data E_e =
  E_e
  deriving (Eq, Ord, Show, Generic)

class Ce x where
  _e' ::
    Prism' x ()
  _e ::
    x
  _e =
    _e' # ()

instance Ce () where
  _e' =
    id

instance Ce E_e where
  _e' =
    prism'
      (\() -> E_e)
      (\E_e -> Just ())

instance Ce Char where
  _e' =
    prism'
      (\() -> 'e')
      (\c ->  case c of
                'e' ->
                  Just ()
                _ ->
                  Nothing)

parse_e ::
  (Ce x, CharParsing p) =>
  p x
parse_e =
  _e <$ char 'e' <?> "e"

data F_f =
  F_f
  deriving (Eq, Ord, Show, Generic)

class Cf x where
  _f' ::
    Prism' x ()
  _f ::
    x
  _f =
    _f' # ()

instance Cf () where
  _f' =
    id

instance Cf F_f where
  _f' =
    prism'
      (\() -> F_f)
      (\F_f -> Just ())

instance Cf Char where
  _f' =
    prism'
      (\() -> 'f')
      (\c ->  case c of
                'f' ->
                  Just ()
                _ ->
                  Nothing)

parse_f ::
  (Cf x, CharParsing p) =>
  p x
parse_f =
  _f <$ char 'f' <?> "f"

data G_g =
  G_g
  deriving (Eq, Ord, Show, Generic)

class Cg x where
  _g' ::
    Prism' x ()
  _g ::
    x
  _g =
    _g' # ()

instance Cg () where
  _g' =
    id

instance Cg G_g where
  _g' =
    prism'
      (\() -> G_g)
      (\G_g -> Just ())

instance Cg Char where
  _g' =
    prism'
      (\() -> 'g')
      (\c ->  case c of
                'g' ->
                  Just ()
                _ ->
                  Nothing)

parse_g ::
  (Cg x, CharParsing p) =>
  p x
parse_g =
  _g <$ char 'g' <?> "g"

data H_h =
  H_h
  deriving (Eq, Ord, Show, Generic)

class Ch x where
  _h' ::
    Prism' x ()
  _h ::
    x
  _h =
    _h' # ()

instance Ch () where
  _h' =
    id

instance Ch H_h where
  _h' =
    prism'
      (\() -> H_h)
      (\H_h -> Just ())

instance Ch Char where
  _h' =
    prism'
      (\() -> 'h')
      (\c ->  case c of
                'h' ->
                  Just ()
                _ ->
                  Nothing)

parse_h ::
  (Ch x, CharParsing p) =>
  p x
parse_h =
  _h <$ char 'h' <?> "h"

data I_i =
  I_i
  deriving (Eq, Ord, Show, Generic)

class Ci x where
  _i' ::
    Prism' x ()
  _i ::
    x
  _i =
    _i' # ()

instance Ci () where
  _i' =
    id

instance Ci I_i where
  _i' =
    prism'
      (\() -> I_i)
      (\I_i -> Just ())

instance Ci Char where
  _i' =
    prism'
      (\() -> 'i')
      (\c ->  case c of
                'i' ->
                  Just ()
                _ ->
                  Nothing)

parse_i ::
  (Ci x, CharParsing p) =>
  p x
parse_i =
  _i <$ char 'i' <?> "i"

data J_j =
  J_j
  deriving (Eq, Ord, Show, Generic)

class Cj x where
  _j' ::
    Prism' x ()
  _j ::
    x
  _j =
    _j' # ()

instance Cj () where
  _j' =
    id

instance Cj J_j where
  _j' =
    prism'
      (\() -> J_j)
      (\J_j -> Just ())

instance Cj Char where
  _j' =
    prism'
      (\() -> 'j')
      (\c ->  case c of
                'j' ->
                  Just ()
                _ ->
                  Nothing)

parse_j ::
  (Cj x, CharParsing p) =>
  p x
parse_j =
  _j <$ char 'j' <?> "j"

data K_k =
  K_k
  deriving (Eq, Ord, Show, Generic)

class Ck x where
  _k' ::
    Prism' x ()
  _k ::
    x
  _k =
    _k' # ()

instance Ck () where
  _k' =
    id

instance Ck K_k where
  _k' =
    prism'
      (\() -> K_k)
      (\K_k -> Just ())

instance Ck Char where
  _k' =
    prism'
      (\() -> 'k')
      (\c ->  case c of
                'k' ->
                  Just ()
                _ ->
                  Nothing)

parse_k ::
  (Ck x, CharParsing p) =>
  p x
parse_k =
  _k <$ char 'k' <?> "k"

data L_l =
  L_l
  deriving (Eq, Ord, Show, Generic)

class Cl x where
  _l' ::
    Prism' x ()
  _l ::
    x
  _l =
    _l' # ()

instance Cl () where
  _l' =
    id

instance Cl L_l where
  _l' =
    prism'
      (\() -> L_l)
      (\L_l -> Just ())

instance Cl Char where
  _l' =
    prism'
      (\() -> 'l')
      (\c ->  case c of
                'l' ->
                  Just ()
                _ ->
                  Nothing)

parse_l ::
  (Cl x, CharParsing p) =>
  p x
parse_l =
  _l <$ char 'l' <?> "l"

data M_m =
  M_m
  deriving (Eq, Ord, Show, Generic)

class Cm x where
  _m' ::
    Prism' x ()
  _m ::
    x
  _m =
    _m' # ()

instance Cm () where
  _m' =
    id

instance Cm M_m where
  _m' =
    prism'
      (\() -> M_m)
      (\M_m -> Just ())

instance Cm Char where
  _m' =
    prism'
      (\() -> 'm')
      (\c ->  case c of
                'm' ->
                  Just ()
                _ ->
                  Nothing)

parse_m ::
  (Cm x, CharParsing p) =>
  p x
parse_m =
  _m <$ char 'm' <?> "m"

data N_n =
  N_n
  deriving (Eq, Ord, Show, Generic)

class Cn x where
  _n' ::
    Prism' x ()
  _n ::
    x
  _n =
    _n' # ()

instance Cn () where
  _n' =
    id

instance Cn N_n where
  _n' =
    prism'
      (\() -> N_n)
      (\N_n -> Just ())

instance Cn Char where
  _n' =
    prism'
      (\() -> 'n')
      (\c ->  case c of
                'n' ->
                  Just ()
                _ ->
                  Nothing)

parse_n ::
  (Cn x, CharParsing p) =>
  p x
parse_n =
  _n <$ char 'n' <?> "n"

data O_o =
  O_o
  deriving (Eq, Ord, Show, Generic)

class Co x where
  _o' ::
    Prism' x ()
  _o ::
    x
  _o =
    _o' # ()

instance Co () where
  _o' =
    id

instance Co O_o where
  _o' =
    prism'
      (\() -> O_o)
      (\O_o -> Just ())

instance Co Char where
  _o' =
    prism'
      (\() -> 'o')
      (\c ->  case c of
                'o' ->
                  Just ()
                _ ->
                  Nothing)

parse_o ::
  (Co x, CharParsing p) =>
  p x
parse_o =
  _o <$ char 'o' <?> "o"

data P_p =
  P_p
  deriving (Eq, Ord, Show, Generic)

class Cp x where
  _p' ::
    Prism' x ()
  _p ::
    x
  _p =
    _p' # ()

instance Cp () where
  _p' =
    id

instance Cp P_p where
  _p' =
    prism'
      (\() -> P_p)
      (\P_p -> Just ())

instance Cp Char where
  _p' =
    prism'
      (\() -> 'p')
      (\c ->  case c of
                'p' ->
                  Just ()
                _ ->
                  Nothing)

parse_p ::
  (Cp x, CharParsing p) =>
  p x
parse_p =
  _p <$ char 'p' <?> "p"

data Q_q =
  Q_q
  deriving (Eq, Ord, Show, Generic)

class Cq x where
  _q' ::
    Prism' x ()
  _q ::
    x
  _q =
    _q' # ()

instance Cq () where
  _q' =
    id

instance Cq Q_q where
  _q' =
    prism'
      (\() -> Q_q)
      (\Q_q -> Just ())

instance Cq Char where
  _q' =
    prism'
      (\() -> 'q')
      (\c ->  case c of
                'q' ->
                  Just ()
                _ ->
                  Nothing)

parse_q ::
  (Cq x, CharParsing p) =>
  p x
parse_q =
  _q <$ char 'q' <?> "q"

data R_r =
  R_r
  deriving (Eq, Ord, Show, Generic)

class Cr x where
  _r' ::
    Prism' x ()
  _r ::
    x
  _r =
    _r' # ()

instance Cr () where
  _r' =
    id

instance Cr R_r where
  _r' =
    prism'
      (\() -> R_r)
      (\R_r -> Just ())

instance Cr Char where
  _r' =
    prism'
      (\() -> 'r')
      (\c ->  case c of
                'r' ->
                  Just ()
                _ ->
                  Nothing)

parse_r ::
  (Cr x, CharParsing p) =>
  p x
parse_r =
  _r <$ char 'r' <?> "r"

data S_s =
  S_s
  deriving (Eq, Ord, Show, Generic)

class Cs x where
  _s' ::
    Prism' x ()
  _s ::
    x
  _s =
    _s' # ()

instance Cs () where
  _s' =
    id

instance Cs S_s where
  _s' =
    prism'
      (\() -> S_s)
      (\S_s -> Just ())

instance Cs Char where
  _s' =
    prism'
      (\() -> 's')
      (\c ->  case c of
                's' ->
                  Just ()
                _ ->
                  Nothing)

parse_s ::
  (Cs x, CharParsing p) =>
  p x
parse_s =
  _s <$ char 's' <?> "s"

data T_t =
  T_t
  deriving (Eq, Ord, Show, Generic)

class Ct x where
  _t' ::
    Prism' x ()
  _t ::
    x
  _t =
    _t' # ()

instance Ct () where
  _t' =
    id

instance Ct T_t where
  _t' =
    prism'
      (\() -> T_t)
      (\T_t -> Just ())

instance Ct Char where
  _t' =
    prism'
      (\() -> 't')
      (\c ->  case c of
                't' ->
                  Just ()
                _ ->
                  Nothing)

parse_t ::
  (Ct x, CharParsing p) =>
  p x
parse_t =
  _t <$ char 't' <?> "t"

data U_u =
  U_u
  deriving (Eq, Ord, Show, Generic)

class Cu x where
  _u' ::
    Prism' x ()
  _u ::
    x
  _u =
    _u' # ()

instance Cu () where
  _u' =
    id

instance Cu U_u where
  _u' =
    prism'
      (\() -> U_u)
      (\U_u -> Just ())

instance Cu Char where
  _u' =
    prism'
      (\() -> 'u')
      (\c ->  case c of
                'u' ->
                  Just ()
                _ ->
                  Nothing)

parse_u ::
  (Cu x, CharParsing p) =>
  p x
parse_u =
  _u <$ char 'u' <?> "u"

data V_v =
  V_v
  deriving (Eq, Ord, Show, Generic)

class Cv x where
  _v' ::
    Prism' x ()
  _v ::
    x
  _v =
    _v' # ()

instance Cv () where
  _v' =
    id

instance Cv V_v where
  _v' =
    prism'
      (\() -> V_v)
      (\V_v -> Just ())

instance Cv Char where
  _v' =
    prism'
      (\() -> 'v')
      (\c ->  case c of
                'v' ->
                  Just ()
                _ ->
                  Nothing)

parse_v ::
  (Cv x, CharParsing p) =>
  p x
parse_v =
  _v <$ char 'v' <?> "v"

data W_w =
  W_w
  deriving (Eq, Ord, Show, Generic)

class Cw x where
  _w' ::
    Prism' x ()
  _w ::
    x
  _w =
    _w' # ()

instance Cw () where
  _w' =
    id

instance Cw W_w where
  _w' =
    prism'
      (\() -> W_w)
      (\W_w -> Just ())

instance Cw Char where
  _w' =
    prism'
      (\() -> 'w')
      (\c ->  case c of
                'w' ->
                  Just ()
                _ ->
                  Nothing)

parse_w ::
  (Cw x, CharParsing p) =>
  p x
parse_w =
  _w <$ char 'w' <?> "w"

data X_x =
  X_x
  deriving (Eq, Ord, Show, Generic)

class Cx x where
  _x' ::
    Prism' x ()
  _x ::
    x
  _x =
    _x' # ()

instance Cx () where
  _x' =
    id

instance Cx X_x where
  _x' =
    prism'
      (\() -> X_x)
      (\X_x -> Just ())

instance Cx Char where
  _x' =
    prism'
      (\() -> 'x')
      (\c ->  case c of
                'x' ->
                  Just ()
                _ ->
                  Nothing)

parse_x ::
  (Cx x, CharParsing p) =>
  p x
parse_x =
  _x <$ char 'x' <?> "x"

data Y_y =
  Y_y
  deriving (Eq, Ord, Show, Generic)

class Cy x where
  _y' ::
    Prism' x ()
  _y ::
    x
  _y =
    _y' # ()

instance Cy () where
  _y' =
    id

instance Cy Y_y where
  _y' =
    prism'
      (\() -> Y_y)
      (\Y_y -> Just ())

instance Cy Char where
  _y' =
    prism'
      (\() -> 'y')
      (\c ->  case c of
                'y' ->
                  Just ()
                _ ->
                  Nothing)

parse_y ::
  (Cy x, CharParsing p) =>
  p x
parse_y =
  _y <$ char 'y' <?> "y"

data Z_z =
  Z_z
  deriving (Eq, Ord, Show, Generic)

class Cz x where
  _z' ::
    Prism' x ()
  _z ::
    x
  _z =
    _z' # ()

instance Cz () where
  _z' =
    id

instance Cz Z_z where
  _z' =
    prism'
      (\() -> Z_z)
      (\Z_z -> Just ())

instance Cz Char where
  _z' =
    prism'
      (\() -> 'z')
      (\c ->  case c of
                'z' ->
                  Just ()
                _ ->
                  Nothing)

parse_z ::
  (Cz x, CharParsing p) =>
  p x
parse_z =
  _z <$ char 'z' <?> "z"

data A_A =
  A_A
  deriving (Eq, Ord, Show, Generic)

class CA x where
  _A' ::
    Prism' x ()
  _A ::
    x
  _A =
    _A' # ()

instance CA () where
  _A' =
    id

instance CA A_A where
  _A' =
    prism'
      (\() -> A_A)
      (\A_A -> Just ())

instance CA Char where
  _A' =
    prism'
      (\() -> 'A')
      (\c ->  case c of
                'A' ->
                  Just ()
                _ ->
                  Nothing)

parse_A ::
  (CA x, CharParsing p) =>
  p x
parse_A =
  _A <$ char 'A' <?> "A"

data B_B =
  B_B
  deriving (Eq, Ord, Show, Generic)

class CB x where
  _B' ::
    Prism' x ()
  _B ::
    x
  _B =
    _B' # ()

instance CB () where
  _B' =
    id

instance CB B_B where
  _B' =
    prism'
      (\() -> B_B)
      (\B_B -> Just ())

instance CB Char where
  _B' =
    prism'
      (\() -> 'B')
      (\c ->  case c of
                'B' ->
                  Just ()
                _ ->
                  Nothing)

parse_B ::
  (CB x, CharParsing p) =>
  p x
parse_B =
  _B <$ char 'B' <?> "B"

data C_C =
  C_C
  deriving (Eq, Ord, Show, Generic)

class CC x where
  _C' ::
    Prism' x ()
  _C ::
    x
  _C =
    _C' # ()

instance CC () where
  _C' =
    id

instance CC C_C where
  _C' =
    prism'
      (\() -> C_C)
      (\C_C -> Just ())

instance CC Char where
  _C' =
    prism'
      (\() -> 'C')
      (\c ->  case c of
                'C' ->
                  Just ()
                _ ->
                  Nothing)

parse_C ::
  (CC x, CharParsing p) =>
  p x
parse_C =
  _C <$ char 'C' <?> "C"

data D_D =
  D_D
  deriving (Eq, Ord, Show, Generic)

class CD x where
  _D' ::
    Prism' x ()
  _D ::
    x
  _D =
    _D' # ()

instance CD () where
  _D' =
    id

instance CD D_D where
  _D' =
    prism'
      (\() -> D_D)
      (\D_D -> Just ())

instance CD Char where
  _D' =
    prism'
      (\() -> 'D')
      (\c ->  case c of
                'D' ->
                  Just ()
                _ ->
                  Nothing)

parse_D ::
  (CD x, CharParsing p) =>
  p x
parse_D =
  _D <$ char 'D' <?> "D"

data E_E =
  E_E
  deriving (Eq, Ord, Show, Generic)

class CE x where
  _E' ::
    Prism' x ()
  _E ::
    x
  _E =
    _E' # ()

instance CE () where
  _E' =
    id

instance CE E_E where
  _E' =
    prism'
      (\() -> E_E)
      (\E_E -> Just ())

instance CE Char where
  _E' =
    prism'
      (\() -> 'E')
      (\c ->  case c of
                'E' ->
                  Just ()
                _ ->
                  Nothing)

parse_E ::
  (CE x, CharParsing p) =>
  p x
parse_E =
  _E <$ char 'E' <?> "E"

data F_F =
  F_F
  deriving (Eq, Ord, Show, Generic)

class CF x where
  _F' ::
    Prism' x ()
  _F ::
    x
  _F =
    _F' # ()

instance CF () where
  _F' =
    id

instance CF F_F where
  _F' =
    prism'
      (\() -> F_F)
      (\F_F -> Just ())

instance CF Char where
  _F' =
    prism'
      (\() -> 'F')
      (\c ->  case c of
                'F' ->
                  Just ()
                _ ->
                  Nothing)

parse_F ::
  (CF x, CharParsing p) =>
  p x
parse_F =
  _F <$ char 'F' <?> "F"

data G_G =
  G_G
  deriving (Eq, Ord, Show, Generic)

class CG x where
  _G' ::
    Prism' x ()
  _G ::
    x
  _G =
    _G' # ()

instance CG () where
  _G' =
    id

instance CG G_G where
  _G' =
    prism'
      (\() -> G_G)
      (\G_G -> Just ())

instance CG Char where
  _G' =
    prism'
      (\() -> 'G')
      (\c ->  case c of
                'G' ->
                  Just ()
                _ ->
                  Nothing)

parse_G ::
  (CG x, CharParsing p) =>
  p x
parse_G =
  _G <$ char 'G' <?> "G"

data H_H =
  H_H
  deriving (Eq, Ord, Show, Generic)

class CH x where
  _H' ::
    Prism' x ()
  _H ::
    x
  _H =
    _H' # ()

instance CH () where
  _H' =
    id

instance CH H_H where
  _H' =
    prism'
      (\() -> H_H)
      (\H_H -> Just ())

instance CH Char where
  _H' =
    prism'
      (\() -> 'H')
      (\c ->  case c of
                'H' ->
                  Just ()
                _ ->
                  Nothing)

parse_H ::
  (CH x, CharParsing p) =>
  p x
parse_H =
  _H <$ char 'H' <?> "H"

data I_I =
  I_I
  deriving (Eq, Ord, Show, Generic)

class CI x where
  _I' ::
    Prism' x ()
  _I ::
    x
  _I =
    _I' # ()

instance CI () where
  _I' =
    id

instance CI I_I where
  _I' =
    prism'
      (\() -> I_I)
      (\I_I -> Just ())

instance CI Char where
  _I' =
    prism'
      (\() -> 'I')
      (\c ->  case c of
                'I' ->
                  Just ()
                _ ->
                  Nothing)

parse_I ::
  (CI x, CharParsing p) =>
  p x
parse_I =
  _I <$ char 'I' <?> "I"

data J_J =
  J_J
  deriving (Eq, Ord, Show, Generic)

class CJ x where
  _J' ::
    Prism' x ()
  _J ::
    x
  _J =
    _J' # ()

instance CJ () where
  _J' =
    id

instance CJ J_J where
  _J' =
    prism'
      (\() -> J_J)
      (\J_J -> Just ())

instance CJ Char where
  _J' =
    prism'
      (\() -> 'J')
      (\c ->  case c of
                'J' ->
                  Just ()
                _ ->
                  Nothing)

parse_J ::
  (CJ x, CharParsing p) =>
  p x
parse_J =
  _J <$ char 'J' <?> "J"

data K_K =
  K_K
  deriving (Eq, Ord, Show, Generic)

class CK x where
  _K' ::
    Prism' x ()
  _K ::
    x
  _K =
    _K' # ()

instance CK () where
  _K' =
    id

instance CK K_K where
  _K' =
    prism'
      (\() -> K_K)
      (\K_K -> Just ())

instance CK Char where
  _K' =
    prism'
      (\() -> 'K')
      (\c ->  case c of
                'K' ->
                  Just ()
                _ ->
                  Nothing)

parse_K ::
  (CK x, CharParsing p) =>
  p x
parse_K =
  _K <$ char 'K' <?> "K"

data L_L =
  L_L
  deriving (Eq, Ord, Show, Generic)

class CL x where
  _L' ::
    Prism' x ()
  _L ::
    x
  _L =
    _L' # ()

instance CL () where
  _L' =
    id

instance CL L_L where
  _L' =
    prism'
      (\() -> L_L)
      (\L_L -> Just ())

instance CL Char where
  _L' =
    prism'
      (\() -> 'L')
      (\c ->  case c of
                'L' ->
                  Just ()
                _ ->
                  Nothing)

parse_L ::
  (CL x, CharParsing p) =>
  p x
parse_L =
  _L <$ char 'L' <?> "L"

data M_M =
  M_M
  deriving (Eq, Ord, Show, Generic)

class CM x where
  _M' ::
    Prism' x ()
  _M ::
    x
  _M =
    _M' # ()

instance CM () where
  _M' =
    id

instance CM M_M where
  _M' =
    prism'
      (\() -> M_M)
      (\M_M -> Just ())

instance CM Char where
  _M' =
    prism'
      (\() -> 'M')
      (\c ->  case c of
                'M' ->
                  Just ()
                _ ->
                  Nothing)

parse_M ::
  (CM x, CharParsing p) =>
  p x
parse_M =
  _M <$ char 'M' <?> "M"

data N_N =
  N_N
  deriving (Eq, Ord, Show, Generic)

class CN x where
  _N' ::
    Prism' x ()
  _N ::
    x
  _N =
    _N' # ()

instance CN () where
  _N' =
    id

instance CN N_N where
  _N' =
    prism'
      (\() -> N_N)
      (\N_N -> Just ())

instance CN Char where
  _N' =
    prism'
      (\() -> 'N')
      (\c ->  case c of
                'N' ->
                  Just ()
                _ ->
                  Nothing)

parse_N ::
  (CN x, CharParsing p) =>
  p x
parse_N =
  _N <$ char 'N' <?> "N"

data O_O =
  O_O
  deriving (Eq, Ord, Show, Generic)

class CO x where
  _O' ::
    Prism' x ()
  _O ::
    x
  _O =
    _O' # ()

instance CO () where
  _O' =
    id

instance CO O_O where
  _O' =
    prism'
      (\() -> O_O)
      (\O_O -> Just ())

instance CO Char where
  _O' =
    prism'
      (\() -> 'O')
      (\c ->  case c of
                'O' ->
                  Just ()
                _ ->
                  Nothing)

parse_O ::
  (CO x, CharParsing p) =>
  p x
parse_O =
  _O <$ char 'O' <?> "O"

data P_P =
  P_P
  deriving (Eq, Ord, Show, Generic)

class CP x where
  _P' ::
    Prism' x ()
  _P ::
    x
  _P =
    _P' # ()

instance CP () where
  _P' =
    id

instance CP P_P where
  _P' =
    prism'
      (\() -> P_P)
      (\P_P -> Just ())

instance CP Char where
  _P' =
    prism'
      (\() -> 'P')
      (\c ->  case c of
                'P' ->
                  Just ()
                _ ->
                  Nothing)

parse_P ::
  (CP x, CharParsing p) =>
  p x
parse_P =
  _P <$ char 'P' <?> "P"

data Q_Q =
  Q_Q
  deriving (Eq, Ord, Show, Generic)

class CQ x where
  _Q' ::
    Prism' x ()
  _Q ::
    x
  _Q =
    _Q' # ()

instance CQ () where
  _Q' =
    id

instance CQ Q_Q where
  _Q' =
    prism'
      (\() -> Q_Q)
      (\Q_Q -> Just ())

instance CQ Char where
  _Q' =
    prism'
      (\() -> 'Q')
      (\c ->  case c of
                'Q' ->
                  Just ()
                _ ->
                  Nothing)

parse_Q ::
  (CQ x, CharParsing p) =>
  p x
parse_Q =
  _Q <$ char 'Q' <?> "Q"

data R_R =
  R_R
  deriving (Eq, Ord, Show, Generic)

class CR x where
  _R' ::
    Prism' x ()
  _R ::
    x
  _R =
    _R' # ()

instance CR () where
  _R' =
    id

instance CR R_R where
  _R' =
    prism'
      (\() -> R_R)
      (\R_R -> Just ())

instance CR Char where
  _R' =
    prism'
      (\() -> 'R')
      (\c ->  case c of
                'R' ->
                  Just ()
                _ ->
                  Nothing)

parse_R ::
  (CR x, CharParsing p) =>
  p x
parse_R =
  _R <$ char 'R' <?> "R"

data S_S =
  S_S
  deriving (Eq, Ord, Show, Generic)

class CS x where
  _S' ::
    Prism' x ()
  _S ::
    x
  _S =
    _S' # ()

instance CS () where
  _S' =
    id

instance CS S_S where
  _S' =
    prism'
      (\() -> S_S)
      (\S_S -> Just ())

instance CS Char where
  _S' =
    prism'
      (\() -> 'S')
      (\c ->  case c of
                'S' ->
                  Just ()
                _ ->
                  Nothing)

parse_S ::
  (CS x, CharParsing p) =>
  p x
parse_S =
  _S <$ char 'S' <?> "S"

data T_T =
  T_T
  deriving (Eq, Ord, Show, Generic)

class CT x where
  _T' ::
    Prism' x ()
  _T ::
    x
  _T =
    _T' # ()

instance CT () where
  _T' =
    id

instance CT T_T where
  _T' =
    prism'
      (\() -> T_T)
      (\T_T -> Just ())

instance CT Char where
  _T' =
    prism'
      (\() -> 'T')
      (\c ->  case c of
                'T' ->
                  Just ()
                _ ->
                  Nothing)

parse_T ::
  (CT x, CharParsing p) =>
  p x
parse_T =
  _T <$ char 'T' <?> "T"

data U_U =
  U_U
  deriving (Eq, Ord, Show, Generic)

class CU x where
  _U' ::
    Prism' x ()
  _U ::
    x
  _U =
    _U' # ()

instance CU () where
  _U' =
    id

instance CU U_U where
  _U' =
    prism'
      (\() -> U_U)
      (\U_U -> Just ())

instance CU Char where
  _U' =
    prism'
      (\() -> 'U')
      (\c ->  case c of
                'U' ->
                  Just ()
                _ ->
                  Nothing)

parse_U ::
  (CU x, CharParsing p) =>
  p x
parse_U =
  _U <$ char 'U' <?> "U"

data V_V =
  V_V
  deriving (Eq, Ord, Show, Generic)

class CV x where
  _V' ::
    Prism' x ()
  _V ::
    x
  _V =
    _V' # ()

instance CV () where
  _V' =
    id

instance CV V_V where
  _V' =
    prism'
      (\() -> V_V)
      (\V_V -> Just ())

instance CV Char where
  _V' =
    prism'
      (\() -> 'V')
      (\c ->  case c of
                'V' ->
                  Just ()
                _ ->
                  Nothing)

parse_V ::
  (CV x, CharParsing p) =>
  p x
parse_V =
  _V <$ char 'V' <?> "V"

data W_W =
  W_W
  deriving (Eq, Ord, Show, Generic)

class CW x where
  _W' ::
    Prism' x ()
  _W ::
    x
  _W =
    _W' # ()

instance CW () where
  _W' =
    id

instance CW W_W where
  _W' =
    prism'
      (\() -> W_W)
      (\W_W -> Just ())

instance CW Char where
  _W' =
    prism'
      (\() -> 'W')
      (\c ->  case c of
                'W' ->
                  Just ()
                _ ->
                  Nothing)

parse_W ::
  (CW x, CharParsing p) =>
  p x
parse_W =
  _W <$ char 'W' <?> "W"

data X_X =
  X_X
  deriving (Eq, Ord, Show, Generic)

class CX x where
  _X' ::
    Prism' x ()
  _X ::
    x
  _X =
    _X' # ()

instance CX () where
  _X' =
    id

instance CX X_X where
  _X' =
    prism'
      (\() -> X_X)
      (\X_X -> Just ())

instance CX Char where
  _X' =
    prism'
      (\() -> 'X')
      (\c ->  case c of
                'X' ->
                  Just ()
                _ ->
                  Nothing)

parse_X ::
  (CX x, CharParsing p) =>
  p x
parse_X =
  _X <$ char 'X' <?> "X"

data Y_Y =
  Y_Y
  deriving (Eq, Ord, Show, Generic)

class CY x where
  _Y' ::
    Prism' x ()
  _Y ::
    x
  _Y =
    _Y' # ()

instance CY () where
  _Y' =
    id

instance CY Y_Y where
  _Y' =
    prism'
      (\() -> Y_Y)
      (\Y_Y -> Just ())

instance CY Char where
  _Y' =
    prism'
      (\() -> 'Y')
      (\c ->  case c of
                'Y' ->
                  Just ()
                _ ->
                  Nothing)

parse_Y ::
  (CY x, CharParsing p) =>
  p x
parse_Y =
  _Y <$ char 'Y' <?> "Y"

data Z_Z =
  Z_Z
  deriving (Eq, Ord, Show, Generic)

class CZ x where
  _Z' ::
    Prism' x ()
  _Z ::
    x
  _Z =
    _Z' # ()

instance CZ () where
  _Z' =
    id

instance CZ Z_Z where
  _Z' =
    prism'
      (\() -> Z_Z)
      (\Z_Z -> Just ())

instance CZ Char where
  _Z' =
    prism'
      (\() -> 'Z')
      (\c ->  case c of
                'Z' ->
                  Just ()
                _ ->
                  Nothing)

parse_Z ::
  (CZ x, CharParsing p) =>
  p x
parse_Z =
  _Z <$ char 'Z' <?> "Z"

----

type IsLower x =
  (
    Ca x
  , Cb x
  , Cc x
  , Cd x
  , Ce x
  , Cf x
  , Cg x
  , Ch x
  , Ci x
  , Cj x
  , Ck x
  , Cl x
  , Cm x
  , Cn x
  , Co x
  , Cp x
  , Cq x
  , Cr x
  , Cs x
  , Ct x
  , Cu x
  , Cv x
  , Cw x
  , Cx x
  , Cy x
  , Cz x
  )

data Lower =
  Lower_a
  | Lower_b
  | Lower_c
  | Lower_d
  | Lower_e
  | Lower_f
  | Lower_g
  | Lower_h
  | Lower_i
  | Lower_j
  | Lower_k
  | Lower_l
  | Lower_m
  | Lower_n
  | Lower_o
  | Lower_p
  | Lower_q
  | Lower_r
  | Lower_s
  | Lower_t
  | Lower_u
  | Lower_v
  | Lower_w
  | Lower_x
  | Lower_y
  | Lower_z
  deriving (Eq, Ord, Show, Generic)

instance Ca Lower where
  _a' =
    prism'
      (\() -> Lower_a)
      (\c ->  case c of
                Lower_a ->
                  Just ()
                _ ->
                  Nothing)

instance Cb Lower where
  _b' =
    prism'
      (\() -> Lower_b)
      (\c ->  case c of
                Lower_b ->
                  Just ()
                _ ->
                  Nothing)

instance Cc Lower where
  _c' =
    prism'
      (\() -> Lower_c)
      (\c ->  case c of
                Lower_c ->
                  Just ()
                _ ->
                  Nothing)

instance Cd Lower where
  _d' =
    prism'
      (\() -> Lower_d)
      (\c ->  case c of
                Lower_d ->
                  Just ()
                _ ->
                  Nothing)

instance Ce Lower where
  _e' =
    prism'
      (\() -> Lower_e)
      (\c ->  case c of
                Lower_e ->
                  Just ()
                _ ->
                  Nothing)

instance Cf Lower where
  _f' =
    prism'
      (\() -> Lower_f)
      (\c ->  case c of
                Lower_f ->
                  Just ()
                _ ->
                  Nothing)

instance Cg Lower where
  _g' =
    prism'
      (\() -> Lower_g)
      (\c ->  case c of
                Lower_g ->
                  Just ()
                _ ->
                  Nothing)

instance Ch Lower where
  _h' =
    prism'
      (\() -> Lower_h)
      (\c ->  case c of
                Lower_h ->
                  Just ()
                _ ->
                  Nothing)

instance Ci Lower where
  _i' =
    prism'
      (\() -> Lower_i)
      (\c ->  case c of
                Lower_i ->
                  Just ()
                _ ->
                  Nothing)

instance Cj Lower where
  _j' =
    prism'
      (\() -> Lower_j)
      (\c ->  case c of
                Lower_j ->
                  Just ()
                _ ->
                  Nothing)

instance Ck Lower where
  _k' =
    prism'
      (\() -> Lower_k)
      (\c ->  case c of
                Lower_k ->
                  Just ()
                _ ->
                  Nothing)

instance Cl Lower where
  _l' =
    prism'
      (\() -> Lower_l)
      (\c ->  case c of
                Lower_l ->
                  Just ()
                _ ->
                  Nothing)

instance Cm Lower where
  _m' =
    prism'
      (\() -> Lower_m)
      (\c ->  case c of
                Lower_m ->
                  Just ()
                _ ->
                  Nothing)

instance Cn Lower where
  _n' =
    prism'
      (\() -> Lower_n)
      (\c ->  case c of
                Lower_n ->
                  Just ()
                _ ->
                  Nothing)

instance Co Lower where
  _o' =
    prism'
      (\() -> Lower_o)
      (\c ->  case c of
                Lower_o ->
                  Just ()
                _ ->
                  Nothing)

instance Cp Lower where
  _p' =
    prism'
      (\() -> Lower_p)
      (\c ->  case c of
                Lower_p ->
                  Just ()
                _ ->
                  Nothing)

instance Cq Lower where
  _q' =
    prism'
      (\() -> Lower_q)
      (\c ->  case c of
                Lower_q ->
                  Just ()
                _ ->
                  Nothing)

instance Cr Lower where
  _r' =
    prism'
      (\() -> Lower_r)
      (\c ->  case c of
                Lower_r ->
                  Just ()
                _ ->
                  Nothing)

instance Cs Lower where
  _s' =
    prism'
      (\() -> Lower_s)
      (\c ->  case c of
                Lower_s ->
                  Just ()
                _ ->
                  Nothing)

instance Ct Lower where
  _t' =
    prism'
      (\() -> Lower_t)
      (\c ->  case c of
                Lower_t ->
                  Just ()
                _ ->
                  Nothing)

instance Cu Lower where
  _u' =
    prism'
      (\() -> Lower_u)
      (\c ->  case c of
                Lower_u ->
                  Just ()
                _ ->
                  Nothing)

instance Cv Lower where
  _v' =
    prism'
      (\() -> Lower_v)
      (\c ->  case c of
                Lower_v ->
                  Just ()
                _ ->
                  Nothing)

instance Cw Lower where
  _w' =
    prism'
      (\() -> Lower_w)
      (\c ->  case c of
                Lower_w ->
                  Just ()
                _ ->
                  Nothing)

instance Cx Lower where
  _x' =
    prism'
      (\() -> Lower_x)
      (\c ->  case c of
                Lower_x ->
                  Just ()
                _ ->
                  Nothing)

instance Cy Lower where
  _y' =
    prism'
      (\() -> Lower_y)
      (\c ->  case c of
                Lower_y ->
                  Just ()
                _ ->
                  Nothing)

instance Cz Lower where
  _z' =
    prism'
      (\() -> Lower_z)
      (\c ->  case c of
                Lower_z ->
                  Just ()
                _ ->
                  Nothing)

parse_lower ::
  (IsLower x, CharParsing p) =>
  p x
parse_lower =
  parse_a <|>
  parse_b <|>
  parse_c <|>
  parse_d <|>
  parse_e <|>
  parse_f <|>
  parse_g <|>
  parse_h <|>
  parse_i <|>
  parse_j <|>
  parse_k <|>
  parse_l <|>
  parse_m <|>
  parse_n <|>
  parse_o <|>
  parse_p <|>
  parse_q <|>
  parse_r <|>
  parse_s <|>
  parse_t <|>
  parse_u <|>
  parse_v <|>
  parse_w <|>
  parse_x <|>
  parse_y <|>
  parse_z

type IsUpper x =
  (
    CA x
  , CB x
  , CC x
  , CD x
  , CE x
  , CF x
  , CG x
  , CH x
  , CI x
  , CJ x
  , CK x
  , CL x
  , CM x
  , CN x
  , CO x
  , CP x
  , CQ x
  , CR x
  , CS x
  , CT x
  , CU x
  , CV x
  , CW x
  , CX x
  , CY x
  , CZ x
  )

data Upper =
  Upper_A
  | Upper_B
  | Upper_C
  | Upper_D
  | Upper_E
  | Upper_F
  | Upper_G
  | Upper_H
  | Upper_I
  | Upper_J
  | Upper_K
  | Upper_L
  | Upper_M
  | Upper_N
  | Upper_O
  | Upper_P
  | Upper_Q
  | Upper_R
  | Upper_S
  | Upper_T
  | Upper_U
  | Upper_V
  | Upper_W
  | Upper_X
  | Upper_Y
  | Upper_Z
  deriving (Eq, Ord, Show, Generic)

instance CA Upper where
  _A' =
    prism'
      (\() -> Upper_A)
      (\c ->  case c of
                Upper_A ->
                  Just ()
                _ ->
                  Nothing)

instance CB Upper where
  _B' =
    prism'
      (\() -> Upper_B)
      (\c ->  case c of
                Upper_B ->
                  Just ()
                _ ->
                  Nothing)

instance CC Upper where
  _C' =
    prism'
      (\() -> Upper_C)
      (\c ->  case c of
                Upper_C ->
                  Just ()
                _ ->
                  Nothing)

instance CD Upper where
  _D' =
    prism'
      (\() -> Upper_D)
      (\c ->  case c of
                Upper_D ->
                  Just ()
                _ ->
                  Nothing)

instance CE Upper where
  _E' =
    prism'
      (\() -> Upper_E)
      (\c ->  case c of
                Upper_E ->
                  Just ()
                _ ->
                  Nothing)

instance CF Upper where
  _F' =
    prism'
      (\() -> Upper_F)
      (\c ->  case c of
                Upper_F ->
                  Just ()
                _ ->
                  Nothing)

instance CG Upper where
  _G' =
    prism'
      (\() -> Upper_G)
      (\c ->  case c of
                Upper_G ->
                  Just ()
                _ ->
                  Nothing)

instance CH Upper where
  _H' =
    prism'
      (\() -> Upper_H)
      (\c ->  case c of
                Upper_H ->
                  Just ()
                _ ->
                  Nothing)

instance CI Upper where
  _I' =
    prism'
      (\() -> Upper_I)
      (\c ->  case c of
                Upper_I ->
                  Just ()
                _ ->
                  Nothing)

instance CJ Upper where
  _J' =
    prism'
      (\() -> Upper_J)
      (\c ->  case c of
                Upper_J ->
                  Just ()
                _ ->
                  Nothing)

instance CK Upper where
  _K' =
    prism'
      (\() -> Upper_K)
      (\c ->  case c of
                Upper_K ->
                  Just ()
                _ ->
                  Nothing)

instance CL Upper where
  _L' =
    prism'
      (\() -> Upper_L)
      (\c ->  case c of
                Upper_L ->
                  Just ()
                _ ->
                  Nothing)

instance CM Upper where
  _M' =
    prism'
      (\() -> Upper_M)
      (\c ->  case c of
                Upper_M ->
                  Just ()
                _ ->
                  Nothing)

instance CN Upper where
  _N' =
    prism'
      (\() -> Upper_N)
      (\c ->  case c of
                Upper_N ->
                  Just ()
                _ ->
                  Nothing)

instance CO Upper where
  _O' =
    prism'
      (\() -> Upper_O)
      (\c ->  case c of
                Upper_O ->
                  Just ()
                _ ->
                  Nothing)

instance CP Upper where
  _P' =
    prism'
      (\() -> Upper_P)
      (\c ->  case c of
                Upper_P ->
                  Just ()
                _ ->
                  Nothing)

instance CQ Upper where
  _Q' =
    prism'
      (\() -> Upper_Q)
      (\c ->  case c of
                Upper_Q ->
                  Just ()
                _ ->
                  Nothing)

instance CR Upper where
  _R' =
    prism'
      (\() -> Upper_R)
      (\c ->  case c of
                Upper_R ->
                  Just ()
                _ ->
                  Nothing)

instance CS Upper where
  _S' =
    prism'
      (\() -> Upper_S)
      (\c ->  case c of
                Upper_S ->
                  Just ()
                _ ->
                  Nothing)

instance CT Upper where
  _T' =
    prism'
      (\() -> Upper_T)
      (\c ->  case c of
                Upper_T ->
                  Just ()
                _ ->
                  Nothing)

instance CU Upper where
  _U' =
    prism'
      (\() -> Upper_U)
      (\c ->  case c of
                Upper_U ->
                  Just ()
                _ ->
                  Nothing)

instance CV Upper where
  _V' =
    prism'
      (\() -> Upper_V)
      (\c ->  case c of
                Upper_V ->
                  Just ()
                _ ->
                  Nothing)

instance CW Upper where
  _W' =
    prism'
      (\() -> Upper_W)
      (\c ->  case c of
                Upper_W ->
                  Just ()
                _ ->
                  Nothing)

instance CX Upper where
  _X' =
    prism'
      (\() -> Upper_X)
      (\c ->  case c of
                Upper_X ->
                  Just ()
                _ ->
                  Nothing)

instance CY Upper where
  _Y' =
    prism'
      (\() -> Upper_Y)
      (\c ->  case c of
                Upper_Y ->
                  Just ()
                _ ->
                  Nothing)

instance CZ Upper where
  _Z' =
    prism'
      (\() -> Upper_Z)
      (\c ->  case c of
                Upper_Z ->
                  Just ()
                _ ->
                  Nothing)

parse_upper ::
  (IsUpper x, CharParsing p) =>
  p x
parse_upper =
  parse_A <|>
  parse_B <|>
  parse_C <|>
  parse_D <|>
  parse_E <|>
  parse_F <|>
  parse_G <|>
  parse_H <|>
  parse_I <|>
  parse_J <|>
  parse_K <|>
  parse_L <|>
  parse_M <|>
  parse_N <|>
  parse_O <|>
  parse_P <|>
  parse_Q <|>
  parse_R <|>
  parse_S <|>
  parse_T <|>
  parse_U <|>
  parse_V <|>
  parse_W <|>
  parse_X <|>
  parse_Y <|>
  parse_Z

type IsAlpha x =
  (
    IsLower x
  , IsUpper x
  )

data Alpha =
  AlphaLower Lower
  | AlphaUpper Upper
  deriving (Eq, Ord, Show, Generic)

_AlphaLower ::
  Prism'
    Alpha
    Lower
_AlphaLower =
  prism'
    AlphaLower    
    (\c ->  case c of
              AlphaLower l ->
                Just l
              AlphaUpper _ ->
                Nothing)

_AlphaUpper ::
  Prism'
    Alpha
    Upper
_AlphaUpper =
  prism'
    AlphaUpper    
    (\c ->  case c of
              AlphaUpper l ->
                Just l
              AlphaLower _ ->
                Nothing)
instance Ca Alpha where
  _a' =
    _AlphaLower . _a'


instance Cb Alpha where
  _b' =
    _AlphaLower . _b'


instance Cc Alpha where
  _c' =
    _AlphaLower . _c'


instance Cd Alpha where
  _d' =
    _AlphaLower . _d'


instance Ce Alpha where
  _e' =
    _AlphaLower . _e'


instance Cf Alpha where
  _f' =
    _AlphaLower . _f'


instance Cg Alpha where
  _g' =
    _AlphaLower . _g'


instance Ch Alpha where
  _h' =
    _AlphaLower . _h'


instance Ci Alpha where
  _i' =
    _AlphaLower . _i'


instance Cj Alpha where
  _j' =
    _AlphaLower . _j'


instance Ck Alpha where
  _k' =
    _AlphaLower . _k'


instance Cl Alpha where
  _l' =
    _AlphaLower . _l'


instance Cm Alpha where
  _m' =
    _AlphaLower . _m'


instance Cn Alpha where
  _n' =
    _AlphaLower . _n'


instance Co Alpha where
  _o' =
    _AlphaLower . _o'


instance Cp Alpha where
  _p' =
    _AlphaLower . _p'


instance Cq Alpha where
  _q' =
    _AlphaLower . _q'


instance Cr Alpha where
  _r' =
    _AlphaLower . _r'


instance Cs Alpha where
  _s' =
    _AlphaLower . _s'


instance Ct Alpha where
  _t' =
    _AlphaLower . _t'


instance Cu Alpha where
  _u' =
    _AlphaLower . _u'


instance Cv Alpha where
  _v' =
    _AlphaLower . _v'


instance Cw Alpha where
  _w' =
    _AlphaLower . _w'


instance Cx Alpha where
  _x' =
    _AlphaLower . _x'


instance Cy Alpha where
  _y' =
    _AlphaLower . _y'


instance Cz Alpha where
  _z' =
    _AlphaLower . _z'

instance CA Alpha where
  _A' =
    _AlphaUpper . _A'


instance CB Alpha where
  _B' =
    _AlphaUpper . _B'


instance CC Alpha where
  _C' =
    _AlphaUpper . _C'


instance CD Alpha where
  _D' =
    _AlphaUpper . _D'


instance CE Alpha where
  _E' =
    _AlphaUpper . _E'


instance CF Alpha where
  _F' =
    _AlphaUpper . _F'


instance CG Alpha where
  _G' =
    _AlphaUpper . _G'


instance CH Alpha where
  _H' =
    _AlphaUpper . _H'


instance CI Alpha where
  _I' =
    _AlphaUpper . _I'


instance CJ Alpha where
  _J' =
    _AlphaUpper . _J'


instance CK Alpha where
  _K' =
    _AlphaUpper . _K'


instance CL Alpha where
  _L' =
    _AlphaUpper . _L'


instance CM Alpha where
  _M' =
    _AlphaUpper . _M'


instance CN Alpha where
  _N' =
    _AlphaUpper . _N'


instance CO Alpha where
  _O' =
    _AlphaUpper . _O'


instance CP Alpha where
  _P' =
    _AlphaUpper . _P'


instance CQ Alpha where
  _Q' =
    _AlphaUpper . _Q'


instance CR Alpha where
  _R' =
    _AlphaUpper . _R'


instance CS Alpha where
  _S' =
    _AlphaUpper . _S'


instance CT Alpha where
  _T' =
    _AlphaUpper . _T'


instance CU Alpha where
  _U' =
    _AlphaUpper . _U'


instance CV Alpha where
  _V' =
    _AlphaUpper . _V'


instance CW Alpha where
  _W' =
    _AlphaUpper . _W'


instance CX Alpha where
  _X' =
    _AlphaUpper . _X'


instance CY Alpha where
  _Y' =
    _AlphaUpper . _Y'


instance CZ Alpha where
  _Z' =
    _AlphaUpper . _Z'

parse_alpha ::
  (IsAlpha x, CharParsing p) =>
  p x
parse_alpha =
  parse_lower <|>
  parse_upper

lookup1 ::
  Eq a =>
  a
  -> NonEmpty (a, b)
  -> b
lookup1 a ((_, q) :| t) =
  fromMaybe q (lookup a t)

lower_upper ::
  Iso'
    Lower
    Upper
lower_upper =
  let r =
        (Lower_a, Upper_A) :|
        [
          (Lower_b, Upper_B)
        , (Lower_c, Upper_C)
        , (Lower_d, Upper_D)
        , (Lower_e, Upper_E)
        , (Lower_f, Upper_F)
        , (Lower_g, Upper_G)
        , (Lower_h, Upper_H)
        , (Lower_i, Upper_I)
        , (Lower_j, Upper_J)
        , (Lower_k, Upper_K)
        , (Lower_l, Upper_L)
        , (Lower_m, Upper_M)
        , (Lower_n, Upper_N)
        , (Lower_o, Upper_O)
        , (Lower_p, Upper_P)
        , (Lower_q, Upper_Q)
        , (Lower_r, Upper_R)
        , (Lower_s, Upper_S)
        , (Lower_t, Upper_T)
        , (Lower_u, Upper_U)
        , (Lower_v, Upper_V)
        , (Lower_w, Upper_W)
        , (Lower_x, Upper_X)
        , (Lower_y, Upper_Y)
        , (Lower_z, Upper_Z)
        ]
  in  iso
        (`lookup1` r)
        (`lookup1` ((\(m, n) -> (n, m)) <$> r))

upper_lower ::
  Iso'
    Upper
    Lower
upper_lower =
  from lower_upper

rotate1_lower ::
  Iso'
    Lower
    Lower
rotate1_lower =
  let r =
        (Lower_a, Lower_b) :|
        [
          (Lower_b, Lower_c)
        , (Lower_c, Lower_d)
        , (Lower_d, Lower_e)
        , (Lower_e, Lower_f)
        , (Lower_f, Lower_g)
        , (Lower_g, Lower_h)
        , (Lower_h, Lower_i)
        , (Lower_i, Lower_j)
        , (Lower_j, Lower_k)
        , (Lower_k, Lower_l)
        , (Lower_l, Lower_m)
        , (Lower_m, Lower_n)
        , (Lower_n, Lower_o)
        , (Lower_o, Lower_p)
        , (Lower_p, Lower_q)
        , (Lower_q, Lower_r)
        , (Lower_r, Lower_s)
        , (Lower_s, Lower_t)
        , (Lower_t, Lower_u)
        , (Lower_u, Lower_v)
        , (Lower_v, Lower_w)
        , (Lower_w, Lower_x)
        , (Lower_x, Lower_y)
        , (Lower_y, Lower_z)
        , (Lower_z, Lower_a)
        ]
  in  iso
        (`lookup1` r)
        (`lookup1` ((\(m, n) -> (n, m)) <$> r))

rotate1_upper ::
  Iso'
    Upper
    Upper
rotate1_upper =
  let r =
        (Upper_A, Upper_B) :|
        [
          (Upper_B, Upper_C)
        , (Upper_C, Upper_D)
        , (Upper_D, Upper_E)
        , (Upper_E, Upper_F)
        , (Upper_F, Upper_G)
        , (Upper_G, Upper_H)
        , (Upper_H, Upper_I)
        , (Upper_I, Upper_J)
        , (Upper_J, Upper_K)
        , (Upper_K, Upper_L)
        , (Upper_L, Upper_M)
        , (Upper_M, Upper_N)
        , (Upper_N, Upper_O)
        , (Upper_O, Upper_P)
        , (Upper_P, Upper_Q)
        , (Upper_Q, Upper_R)
        , (Upper_R, Upper_S)
        , (Upper_S, Upper_T)
        , (Upper_T, Upper_U)
        , (Upper_U, Upper_V)
        , (Upper_V, Upper_W)
        , (Upper_W, Upper_X)
        , (Upper_X, Upper_Y)
        , (Upper_Y, Upper_Z)
        , (Upper_Z, Upper_A)
        ]
  in  iso
        (`lookup1` r)
        (`lookup1` ((\(m, n) -> (n, m)) <$> r))

rotate1_alpha ::
  Iso'
    Alpha
    Alpha
rotate1_alpha =
  iso
    (\a ->  case a of
              AlphaLower l ->
                AlphaLower
                  (l ^. rotate1_lower)
              AlphaUpper u ->
                AlphaUpper
                  (u ^. rotate1_upper))
    (\a ->  case a of
              AlphaLower l ->
                AlphaLower
                  (rotate1_lower # l)
              AlphaUpper u ->
                AlphaUpper
                  (rotate1_upper # u))

mirror_lower ::
  Iso'
    Lower
    Lower
mirror_lower =
  let r =
        (Lower_a, Lower_z) :|
        [
          (Lower_b, Lower_y)
        , (Lower_c, Lower_x)
        , (Lower_d, Lower_w)
        , (Lower_e, Lower_v)
        , (Lower_f, Lower_u)
        , (Lower_g, Lower_t)
        , (Lower_h, Lower_s)
        , (Lower_i, Lower_r)
        , (Lower_j, Lower_q)
        , (Lower_k, Lower_p)
        , (Lower_l, Lower_o)
        , (Lower_m, Lower_n)
        , (Lower_n, Lower_m)
        , (Lower_o, Lower_l)
        , (Lower_p, Lower_k)
        , (Lower_q, Lower_j)
        , (Lower_r, Lower_i)
        , (Lower_s, Lower_h)
        , (Lower_t, Lower_g)
        , (Lower_u, Lower_f)
        , (Lower_v, Lower_e)
        , (Lower_w, Lower_d)
        , (Lower_x, Lower_c)
        , (Lower_y, Lower_b)
        , (Lower_z, Lower_a)
        ]
  in  involuted
        (`lookup1` r)

mirror_upper ::
  Iso'
    Upper
    Upper
mirror_upper =
  let r =
        (Upper_A, Upper_Z) :|
        [
          (Upper_B, Upper_Y)
        , (Upper_C, Upper_X)
        , (Upper_D, Upper_W)
        , (Upper_E, Upper_V)
        , (Upper_F, Upper_U)
        , (Upper_G, Upper_T)
        , (Upper_H, Upper_S)
        , (Upper_I, Upper_R)
        , (Upper_J, Upper_Q)
        , (Upper_K, Upper_P)
        , (Upper_L, Upper_O)
        , (Upper_M, Upper_N)
        , (Upper_N, Upper_M)
        , (Upper_O, Upper_L)
        , (Upper_P, Upper_K)
        , (Upper_Q, Upper_J)
        , (Upper_R, Upper_I)
        , (Upper_S, Upper_H)
        , (Upper_T, Upper_G)
        , (Upper_U, Upper_F)
        , (Upper_V, Upper_E)
        , (Upper_W, Upper_D)
        , (Upper_X, Upper_C)
        , (Upper_Y, Upper_B)
        , (Upper_Z, Upper_A)
        ]
  in  involuted
        (`lookup1` r)

mirror_alpha ::
  Iso'
    Alpha
    Alpha
mirror_alpha =
  iso
    (\a ->  case a of
              AlphaLower l ->
                AlphaLower
                  (l ^. mirror_lower)
              AlphaUpper u ->
                AlphaUpper
                  (u ^. mirror_upper))
    (\a ->  case a of
              AlphaLower l ->
                AlphaLower
                  (mirror_lower # l)
              AlphaUpper u ->
                AlphaUpper
                  (mirror_upper # u))
