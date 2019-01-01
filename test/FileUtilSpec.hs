module FileUtilSpec where

import           BasicPrelude
import           Test.Hspec

import           FileUtil                       ( toAbsoluteURL )


spec :: SpecWith ()
spec = parallel $ do

  describe "toAbsoluteURL" $ do
    it "handles parent-relative URLs"
      $ toAbsoluteURL "../NRS/NRS-001.html" "https://www.leg.state.nv.us/nrs"
      `shouldBe` "https://www.leg.state.nv.us/nrs/NRS-001.html"

