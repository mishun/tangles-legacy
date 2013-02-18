module Math.Algebra.Polynomial
	(
	  PolynomialRing
	, Polynomial

	, fromList
	, toList
	) where

import qualified Data.List as List
import qualified Data.Map as Map

import Math.Algebra.Ring


data PolynomialRing ring element power = K ring deriving (Eq)

data Polynomial ring element power = Poly ring [(power, element)] deriving (Eq)


fromList :: (Ord power, Ring ring element) => ring -> [(power, element)] -> Polynomial ring element power
fromList r list = Poly r $! filter (\ (_, e) -> e /= z) $ Map.assocs res
	where
		z = zero r

		update new prev = case prev of
			Nothing  -> Just new
			Just !pv -> Just $! (pv <+> new)

		res = List.foldl (\ m (p, e) -> Map.alter (update e) p m) Map.empty list


toList :: (Ring ring element) => Polynomial ring element power -> [(power, element)]
toList (Poly _ list) = list


instance (Num power, Ord power, Ring ring element) => Ring (PolynomialRing ring element power) (Polynomial ring element power) where

	ring (Poly r _) = K r

	zero (K r) = fromList r []

	unity (K r) = fromList r [(0, unity r)]

	negative (Poly r list) = Poly r $ map (\ (p, e) -> (p, negative e)) list

	(<+>) (Poly r a) (Poly _ b) = fromList r (a ++ b)

	(<->) a b = a <+> (negative b)

	(<*>) (Poly r a) (Poly _ b) = fromList r [(ap + bp, ae <*> be) | (ap, ae) <- a, (bp, be) <- b]


instance (Num power, Ord power, Ring ring element, Ord element) => Ord (Polynomial ring element power) where

	compare (Poly _ la) (Poly _ lb) = compare la lb


instance (Integral power, Show power, Show element, Ring ring element) => Show (Polynomial ring element power) where

	show = List.intercalate " + " . map (\ (p, e) -> (show e) ++ "x^{" ++ (show p) ++ "}") . toList
