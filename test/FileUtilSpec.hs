module FileUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           FileUtil                       ( toAbsoluteURL )


spec :: SpecWith ()
spec =
  parallel
    $          describe "toAbsoluteURL"
    $          it "handles parent-relative URLs"
    $ toAbsoluteURL "https://www.leg.state.nv.us/NRS/" "../NRS/NRS-001.html"
    `shouldBe` "https://www.leg.state.nv.us/NRS/NRS-001.html"
