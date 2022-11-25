polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)
