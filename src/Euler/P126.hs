module Euler.P126 where
import qualified  Data.Map as M

c x y z n = 2*(x*y + y*z + x*z) + 4*(x+y+z+n-2)*(n-1)

gon x y z = takeWhile (<20000) . map (c x y z) $ [1..]

goz x y = concat . takeWhile (not.null) . map (gon x y) $ [y..]

goy x = concat . takeWhile (not.null) . map (goz x) $ [x..]

go = concat . takeWhile (not.null) . map goy $ [1..]

euler = filter ((==1000).snd) . M.toList . foldr (\k d -> M.insertWith (+) k 1 d) M.empty $ go


