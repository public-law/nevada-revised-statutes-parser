import           BasicPrelude
import qualified Data.ByteString.Lazy          as B
import           System.IO                      ( stderr )

import           FileUtil                       ( (//) )
import qualified NRSParser                     as NRS
import           ParseReport                    ( renderParseResult )


main :: IO ()
main = do
  (nrsModel, fails) <- NRS.parseFiles $ (//) "/tmp/www.leg.state.nv.us/NRS"
  let (out, err) = renderParseResult nrsModel fails
  B.hPutStr stderr err
  B.putStr out
