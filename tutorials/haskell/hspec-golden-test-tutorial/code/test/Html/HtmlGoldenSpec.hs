module Html.HtmlGoldenSpec where

import           Test.Hspec
import           Test.Hspec.Golden
import           HTML.Html
         

spec :: Spec
spec =
    describe "renderHtml" $
    it "Renders an Html5 file " $
    defaultGolden "html" (htmlRendered somePage)
