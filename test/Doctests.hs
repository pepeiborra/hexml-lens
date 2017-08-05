import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Text/XML/Hexml/Lens.hs"]
